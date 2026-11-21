# --- Régularisation de la matrice intra-classes W (reg dans [0, 1]) ---
regularize_within_cov <- function(W, reg = 0) {
  if (!is.numeric(reg) || length(reg) != 1L || is.na(reg) || reg < 0 || reg > 1) {
    stop("`reg` must be a single numeric value in [0, 1].", call. = FALSE)
  }
  if (reg == 0) return(W)
  p <- nrow(W)
  diag_target <- diag(diag(W), nrow = p, ncol = p)
  (1 - reg) * W + reg * diag_target
}

# --- Encodage reproductible des variables explicatives (numériques + catégorielles) ---
fit_feature_encoder <- function(X) {
  if (!is.data.frame(X) && !is.matrix(X)) {
    stop("`X` must be a data.frame or a matrix.", call. = FALSE)
  }
  df <- as.data.frame(X, stringsAsFactors = FALSE)
  if (nrow(df) == 0L || ncol(df) == 0L) {
    stop("`X` must have at least one row and one column.", call. = FALSE)
  }

  col_types <- character(ncol(df))
  names(col_types) <- names(df)
  factor_levels <- list()
  encoded_cols <- character(0)

  for (nm in names(df)) {
    col <- df[[nm]]
    if (is.numeric(col) || is.integer(col)) {
      num_col <- as.numeric(col)
      if (anyNA(num_col) || any(!is.finite(num_col))) {
        stop(sprintf("Numeric predictor '%s' contains missing or non-finite values.", nm), call. = FALSE)
      }
      col_types[nm] <- "numeric"
      encoded_cols <- c(encoded_cols, nm)
    } else if (is.factor(col) || is.character(col) || is.logical(col)) {
      raw_chr <- if (is.logical(col)) ifelse(is.na(col), NA_character_, as.character(col)) else as.character(col)
      if (anyNA(raw_chr) || any(!nzchar(trimws(raw_chr)))) {
        stop(sprintf("Categorical predictor '%s' contains missing or empty values.", nm), call. = FALSE)
      }
      f <- factor(raw_chr)
      levs <- levels(f)
      if (length(levs) < 2L) {
        stop(sprintf("Categorical predictor '%s' must have at least two observed levels.", nm), call. = FALSE)
      }
      col_types[nm] <- "categorical"
      factor_levels[[nm]] <- levs
      encoded_cols <- c(encoded_cols, paste0(nm, "_", levs))
    } else {
      stop(sprintf("Unsupported column type for predictor '%s'.", nm), call. = FALSE)
    }
  }

  list(
    raw_names = names(df),
    col_types = col_types,
    factor_levels = factor_levels,
    encoded_cols = encoded_cols
  )
}

transform_features <- function(X, encoder, allow_extra_cols = TRUE) {
  if (!is.data.frame(X) && !is.matrix(X)) {
    stop("`X` must be a data.frame or a matrix.", call. = FALSE)
  }
  if (is.null(colnames(X)) || any(!nzchar(trimws(colnames(X))))) {
    stop("`X` must have non-empty column names.", call. = FALSE)
  }

  missing_cols <- setdiff(encoder$raw_names, colnames(X))
  if (length(missing_cols) > 0L) {
    stop(sprintf("Missing required predictor column(s): %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  if (!allow_extra_cols) {
    extra_cols <- setdiff(colnames(X), encoder$raw_names)
    if (length(extra_cols) > 0L) {
      stop(sprintf("Unexpected extra column(s): %s", paste(extra_cols, collapse = ", ")), call. = FALSE)
    }
  }

  df <- as.data.frame(X, stringsAsFactors = FALSE)[, encoder$raw_names, drop = FALSE]
  if (nrow(df) == 0L) {
    stop("`X` must contain at least one observation.", call. = FALSE)
  }

  out <- matrix(0, nrow = nrow(df), ncol = length(encoder$encoded_cols))
  colnames(out) <- encoder$encoded_cols

  for (nm in encoder$raw_names) {
    typ <- encoder$col_types[[nm]]
    col <- df[[nm]]
    if (typ == "numeric") {
      if (!is.numeric(col) && !is.integer(col)) {
        stop(sprintf("Predictor '%s' must be numeric.", nm), call. = FALSE)
      }
      num_col <- as.numeric(col)
      if (anyNA(num_col) || any(!is.finite(num_col))) {
        stop(sprintf("Predictor '%s' contains missing or non-finite values.", nm), call. = FALSE)
      }
      out[, nm] <- num_col
    } else if (typ == "categorical") {
      raw_chr <- if (is.logical(col)) ifelse(is.na(col), NA_character_, as.character(col)) else as.character(col)
      if (anyNA(raw_chr) || any(!nzchar(trimws(raw_chr)))) {
        stop(sprintf("Predictor '%s' contains missing or empty values.", nm), call. = FALSE)
      }
      known_levels <- encoder$factor_levels[[nm]]
      unknown_vals <- setdiff(unique(raw_chr), known_levels)
      if (length(unknown_vals) > 0L) {
        stop(
          sprintf(
            "Predictor '%s' contains unseen category level(s): %s",
            nm, paste(unknown_vals, collapse = ", ")
          ),
          call. = FALSE
        )
      }
      for (lv in known_levels) {
        out[, paste0(nm, "_", lv)] <- as.numeric(raw_chr == lv)
      }
    }
  }

  storage.mode(out) <- "double"
  out
}

# --- Sélection de variables par score F (ANOVA univariée), avec parallélisation optionnelle ---
select_features_anova <- function(X_mat, y, top_k = NULL, min_f_score = NULL, n_cores = 1L) {
  y <- factor(y)
  n <- nrow(X_mat)
  K <- nlevels(y)
  p <- ncol(X_mat)
  if (n <= K) {
    stop("Not enough observations relative to number of classes for ANOVA feature selection.", call. = FALSE)
  }
  if (!is.numeric(n_cores) || length(n_cores) != 1L || is.na(n_cores) || n_cores < 1L) {
    stop("`n_cores` must be an integer >= 1.", call. = FALSE)
  }
  n_cores <- as.integer(n_cores)

  df_between <- max(1L, K - 1L)
  df_within <- max(1L, n - K)
  class_idx <- split(seq_len(n), y)

  compute_one_f <- function(j) {
    xj <- X_mat[, j]
    grand_mean <- mean(xj)
    ss_between <- sum(vapply(class_idx, function(idx) {
      length(idx) * (mean(xj[idx]) - grand_mean)^2
    }, numeric(1)))
    ss_within <- sum(vapply(class_idx, function(idx) {
      if (length(idx) <= 1L) return(0)
      g <- xj[idx]
      sum((g - mean(g))^2)
    }, numeric(1)))

    ms_between <- ss_between / df_between
    ms_within <- ss_within / df_within

    if (ms_within > 0) ms_between / ms_within else if (ms_between > 0) Inf else 0
  }

  cols_seq <- seq_len(p)
  use_mc <- n_cores > 1L && .Platform$OS.type != "windows"
  f_vec <- if (use_mc) {
    unlist(parallel::mclapply(cols_seq, compute_one_f, mc.cores = n_cores), use.names = FALSE)
  } else {
    vapply(cols_seq, compute_one_f, numeric(1))
  }

  f_scores <- as.numeric(f_vec)
  names(f_scores) <- colnames(X_mat)

  keep_mask <- rep(TRUE, p)
  if (!is.null(min_f_score)) {
    if (!is.numeric(min_f_score) || length(min_f_score) != 1L || is.na(min_f_score) || min_f_score < 0) {
      stop("`min_f_score` must be a single non-negative number.", call. = FALSE)
    }
    keep_mask <- keep_mask & (f_scores >= min_f_score)
  }

  candidate_cols <- colnames(X_mat)[keep_mask]
  if (length(candidate_cols) == 0L) {
    stop("No variables meet the `min_f_score` criterion.", call. = FALSE)
  }

  ord <- order(f_scores[candidate_cols], decreasing = TRUE)
  k_keep <- if (is.null(top_k)) {
    length(candidate_cols)
  } else {
    if (!is.numeric(top_k) || length(top_k) != 1L || is.na(top_k) || top_k < 1L) {
      stop("`top_k` must be a single integer >= 1.", call. = FALSE)
    }
    min(as.integer(top_k), length(candidate_cols))
  }

  kept_cols <- candidate_cols[ord[seq_len(k_keep)]]
  list(
    selected_cols = kept_cols,
    f_scores = f_scores
  )
}

# --- Indicateurs d'évaluation globale et par classe ---
compute_classification_metrics <- function(y_true, y_pred, classes) {
  y_true_f <- factor(as.character(y_true), levels = classes)
  y_pred_f <- factor(as.character(y_pred), levels = classes)
  if (anyNA(y_true_f)) {
    stop("`y_true` contains missing values or unseen class labels.", call. = FALSE)
  }

  cm <- table(Observed = y_true_f, Predicted = y_pred_f)
  tp <- diag(cm)
  support <- rowSums(cm)
  pred_count <- colSums(cm)

  precision <- ifelse(pred_count > 0, tp / pred_count, NA_real_)
  recall <- ifelse(support > 0, tp / support, NA_real_)
  denom <- support + pred_count
  f1 <- ifelse(denom > 0, 2 * tp / denom, NA_real_)

  accuracy <- sum(tp) / sum(cm)
  balanced_accuracy <- mean(recall, na.rm = TRUE)
  macro_f1 <- mean(f1, na.rm = TRUE)

  list(
    confusion_matrix = cm,
    global = data.frame(
      metric = c("accuracy", "balanced_accuracy", "macro_f1"),
      value = c(accuracy, balanced_accuracy, macro_f1),
      stringsAsFactors = FALSE
    ),
    per_class = data.frame(
      class = classes,
      support = as.integer(support),
      precision = as.numeric(precision),
      recall = as.numeric(recall),
      f1 = as.numeric(f1),
      stringsAsFactors = FALSE
    )
  )
}

# --- Pertinence descriptive des variables (coefficients standardisés + norme L2 + score F) ---
compute_variable_relevance <- function(coefficients, within_cov, f_scores = NULL) {
  slopes <- coefficients[-1, , drop = FALSE]
  feat_names <- rownames(slopes)
  sd_within <- sqrt(pmax(diag(within_cov)[feat_names], 0))
  sd_within[is.na(sd_within)] <- 0

  std_slopes <- slopes * sd_within
  raw_l2 <- sqrt(rowSums(slopes^2))
  std_l2 <- sqrt(rowSums(std_slopes^2))

  out <- data.frame(
    feature = feat_names,
    std_l2_norm = as.numeric(std_l2),
    raw_l2_norm = as.numeric(raw_l2),
    within_sd = as.numeric(sd_within),
    stringsAsFactors = FALSE
  )
  if (!is.null(f_scores)) {
    out$anova_f <- as.numeric(f_scores[feat_names])
  }
  out[order(out$std_l2_norm, decreasing = TRUE), , drop = FALSE]
}
