1. Régularisation de W
# W est rétrécie vers sa diagonale : (1 - reg) * W + reg * diag(W)
regularize_within_cov <- function(W, reg = 0) {
  if (!is.numeric(reg) || length(reg) != 1L || is.na(reg) || reg < 0 || reg > 1) {
    stop("`reg` must be a single numeric value in [0, 1].", call. = FALSE)
  }
  if (reg == 0) return(W)
  (1 - reg) * W + reg * diag(diag(W), nrow = nrow(W))
}
 
 
2. Encodage des variables explicatives
# Appris sur l'apprentissage. Pour une variable catégorielle, on crée
# une indicatrice par modalité sauf la première (référence) : avec toutes les
# modalités, les indicatrices somment à 1 et W devient singulière.
fit_feature_encoder <- function(X) {
  if (!is.data.frame(X) && !is.matrix(X)) {
    stop("`X` must be a data.frame or a matrix.", call. = FALSE)
  }
  df <- as.data.frame(X, stringsAsFactors = FALSE)
  if (nrow(df) == 0L || ncol(df) == 0L) {
    stop("`X` must have at least one row and one column.", call. = FALSE)
  }
  if (any(!nzchar(trimws(names(df)))) || anyDuplicated(names(df))) {
    stop("Column names of `X` must be non-empty and unique.", call. = FALSE)
  }
 
  col_types <- stats::setNames(character(ncol(df)), names(df))
  factor_levels <- list()
  groups <- list()  # colonnes encodées de chaque variable
 
  for (nm in names(df)) {
    col <- df[[nm]]
    if (is.numeric(col)) {
      if (anyNA(col) || any(!is.finite(col))) {
        stop(sprintf("Numeric predictor '%s' contains missing or non-finite values.", nm), call. = FALSE)
      }
      col_types[nm] <- "numeric"
      groups[[nm]] <- nm
    } else if (is.factor(col) || is.character(col) || is.logical(col)) {
      chr <- as.character(col)
      if (anyNA(chr) || any(!nzchar(trimws(chr)))) {
        stop(sprintf("Categorical predictor '%s' contains missing or empty values.", nm), call. = FALSE)
      }
      levs <- if (is.factor(col)) levels(droplevels(col)) else sort(unique(chr))
      if (length(levs) < 2L) {
        stop(sprintf("Categorical predictor '%s' must have at least two observed levels.", nm), call. = FALSE)
      }
      col_types[nm] <- "categorical"
      factor_levels[[nm]] <- levs
      groups[[nm]] <- paste0(nm, "_", levs[-1L])
    } else {
      stop(sprintf("Unsupported column type for predictor '%s'.", nm), call. = FALSE)
    }
  }
 
  encoded_cols <- unlist(groups, use.names = FALSE)
  if (anyDuplicated(encoded_cols)) {
    stop("Encoded column names clash, please rename the columns.", call. = FALSE)
  }
 
  list(raw_names = names(df), col_types = col_types, factor_levels = factor_levels,
       groups = groups, encoded_cols = encoded_cols)
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
    stop(sprintf("Missing variables in 'X': %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  if (!allow_extra_cols) {
    extra_cols <- setdiff(colnames(X), encoder$raw_names)
    if (length(extra_cols) > 0L) {
      stop(sprintf("Unexpected extra column(s): %s", paste(extra_cols, collapse = ", ")), call. = FALSE)
    }
  }
 
  # on garde les noms de lignes seulement s'ils sont explicites, comme as.matrix()
  rn <- if (is.data.frame(X)) {
    if (.row_names_info(X) > 0L) row.names(X) else NULL
  } else rownames(X)
  df <- as.data.frame(X, stringsAsFactors = FALSE)[, encoder$raw_names, drop = FALSE]
  if (nrow(df) == 0L) {
    stop("`X` must contain at least one observation.", call. = FALSE)
  }
 
  out <- matrix(0, nrow = nrow(df), ncol = length(encoder$encoded_cols),
                dimnames = list(rn, encoder$encoded_cols))
 
  for (nm in encoder$raw_names) {
    col <- df[[nm]]
    if (encoder$col_types[[nm]] == "numeric") {
      if (!is.numeric(col)) {
        stop(sprintf("Predictor '%s' must be numeric (check the decimal separator).", nm), call. = FALSE)
      }
      if (anyNA(col) || any(!is.finite(col))) {
        stop(sprintf("Predictor '%s' contains missing or non-finite values.", nm), call. = FALSE)
      }
      out[, nm] <- col
    } else {
      chr <- as.character(col)
      if (anyNA(chr) || any(!nzchar(trimws(chr)))) {
        stop(sprintf("Predictor '%s' contains missing or empty values.", nm), call. = FALSE)
      }
      levs <- encoder$factor_levels[[nm]]
      unknown <- setdiff(unique(chr), levs)
      if (length(unknown) > 0L) {
        stop(sprintf("Predictor '%s' contains level(s) not seen during fit: %s",
                     nm, paste(unknown, collapse = ", ")), call. = FALSE)
      }
      for (lv in levs[-1L]) {
        out[, paste0(nm, "_", lv)] <- as.numeric(chr == lv)
      }
    }
  }
 
  storage.mode(out) <- "double"
  out
}
 
subset_encoder <- function(encoder, keep) {
  keep <- encoder$raw_names[encoder$raw_names %in% keep]
  encoder$raw_names <- keep
  encoder$col_types <- encoder$col_types[keep]
  encoder$factor_levels <- encoder$factor_levels[intersect(names(encoder$factor_levels), keep)]
  encoder$groups <- encoder$groups[keep]
  encoder$encoded_cols <- unlist(encoder$groups, use.names = FALSE)
  encoder
}
 
 
3. Parallélisation
# mclapply sous Linux/macOS, cluster PSOCK sous Windows
check_n_cores <- function(n_cores) {
  if (!is.numeric(n_cores) || length(n_cores) != 1L || is.na(n_cores) ||
      n_cores < 1 || n_cores != round(n_cores)) {
    stop("`n_cores` must be a single integer >= 1.", call. = FALSE)
  }
  as.integer(n_cores)
}
 
parallel_lapply <- function(X, FUN, n_cores = 1L,
                            use_fork = .Platform$OS.type != "windows") {
  n_cores <- min(check_n_cores(n_cores), length(X))
  if (n_cores <= 1L) return(lapply(X, FUN))
  if (use_fork) {
    res <- parallel::mclapply(X, FUN, mc.cores = n_cores)
    failed <- vapply(res, inherits, logical(1), what = "try-error")
    if (any(failed)) {
      stop("Parallel computation failed: ", as.character(res[[which(failed)[1L]]]), call. = FALSE)
    }
    return(res)
  }
  cl <- parallel::makePSOCKcluster(n_cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::parLapply(cl, X, FUN)
}
 
 
4. Lambda de Wilks et test F
# Lambda = det(W) / det(T), W et T matrices SCE intra-classes et totale.
# On met W et T à l'échelle de T pour que la tolérance ne dépende pas des unités.
scale_sscp <- function(W, Tt, n, K) {
  s <- sqrt(pmax(diag(Tt), 0))
  s[!(s > 0)] <- 1
  d <- outer(s, s)
  list(W = W / d, T = Tt / d, n = n, K = K)
}
 
sscp_from_data <- function(X, y) {
  counts <- tabulate(y, nlevels(y))
  M <- rowsum(X, y, reorder = TRUE) / counts
  Xw <- X - M[as.integer(y), , drop = FALSE]
  Xt <- sweep(X, 2L, colMeans(X))
  scale_sscp(crossprod(Xw), crossprod(Xt), nrow(X), nlevels(y))
}
 
# Même chose à partir du modèle ajusté : W = within_cov * (n - K), T = W + B
sscp_from_model <- function(means, counts, within_cov) {
  n <- sum(counts)
  K <- length(counts)
  W <- within_cov * (n - K)
  mu <- colSums(means * counts) / n
  D <- sweep(means, 2L, mu) * sqrt(counts)
  scale_sscp(W, W + crossprod(D), n, K)
}
 
log_det_pd <- function(M, tol) {
  if (!length(M)) return(0)
  R <- tryCatch(chol(M), error = function(e) NULL)
  if (is.null(R)) return(-Inf)
  d2 <- diag(R)^2
  if (min(d2) <= tol * max(d2)) return(-Inf)
  sum(log(d2))
}
 
# NA si les colonnes sont colinéaires, 0 si les classes sont parfaitement séparées
wilks_lambda <- function(S, cols, tol = 1e-8) {
  if (!length(cols)) return(1)
  lt <- log_det_pd(S$T[cols, cols, drop = FALSE], tol)
  if (!is.finite(lt)) return(NA_real_)
  exp(log_det_pd(S$W[cols, cols, drop = FALSE], tol) - lt)
}
 
# Test F d'un lambda partiel par l'approximation de Rao (exacte si p = 1 ou K = 2).
# p = colonnes testées, q_h = K - 1, q_e = n - K - colonnes déjà dans le modèle.
rao_f_test <- function(lambda, p, q_h, q_e) {
  na <- c(F = NA_real_, df1 = NA_real_, df2 = NA_real_, p_value = NA_real_)
  if (is.na(lambda) || q_e <= 0) return(na)
  lambda <- min(max(lambda, 0), 1)
  den <- p^2 + q_h^2 - 5
  t <- if (den > 0) sqrt((p^2 * q_h^2 - 4) / den) else 1
  df1 <- p * q_h
  df2 <- t * (q_e - (p - q_h + 1) / 2) - (p * q_h - 2) / 2
  if (df2 <= 0) return(na)
  lt <- lambda^(1 / t)
  f <- if (lt > 0) (1 - lt) / lt * df2 / df1 else Inf
  c(F = f, df1 = df1, df2 = df2, p_value = stats::pf(f, df1, df2, lower.tail = FALSE))
}
 
 
5. Sélection de variables (STEPDISC ascendant)
# À chaque étape on ajoute la variable dont le
# F partiel a la plus petite p-value, tant qu'elle reste sous alpha.
# Une variable catégorielle entre en bloc.
stepwise_forward <- function(X, y, groups, alpha = 0.05, max_vars = NULL,
                             n_cores = 1L, tol = 1e-8) {
  S <- sscp_from_data(X, y)
  K <- S$K
  n <- S$n
  max_vars <- if (is.null(max_vars)) length(groups) else min(max_vars, length(groups))
  selected <- character(0)
  cols <- character(0)
  lambda_cur <- 1
  steps <- list()
 
  while (length(selected) < max_vars && lambda_cur > 0) {
    candidates <- setdiff(names(groups), selected)
    q <- length(cols)
    evals <- parallel_lapply(candidates, function(g) {
      lam <- wilks_lambda(S, c(cols, groups[[g]]), tol)
      partial <- lam / lambda_cur
      c(wilks_lambda = lam, partial_lambda = partial,
        rao_f_test(partial, length(groups[[g]]), K - 1L, n - K - q))
    }, n_cores)
    tab <- do.call(rbind, evals)
    rownames(tab) <- candidates
    tab <- tab[!is.na(tab[, "p_value"]), , drop = FALSE]
    if (!nrow(tab)) break
    best <- order(tab[, "p_value"], tab[, "partial_lambda"])[1L]
    if (tab[best, "p_value"] > alpha) break
 
    g <- rownames(tab)[best]
    selected <- c(selected, g)
    cols <- c(cols, groups[[g]])
    lambda_cur <- tab[best, "wilks_lambda"]
    steps[[length(steps) + 1L]] <- data.frame(
      step = length(selected), variable = g, n_columns = length(groups[[g]]),
      as.list(tab[best, ]), check.names = FALSE, stringsAsFactors = FALSE
    )
  }
 
  steps <- if (length(steps)) do.call(rbind, steps) else data.frame(
    step = integer(0), variable = character(0), n_columns = integer(0),
    wilks_lambda = numeric(0), partial_lambda = numeric(0), F = numeric(0),
    df1 = numeric(0), df2 = numeric(0), p_value = numeric(0)
  )
  rownames(steps) <- NULL
  list(selected = selected, steps = steps)
}
 
 6. Indicateurs d'évaluation globale et par classe
compute_classification_metrics <- function(y_true, y_pred, classes) {
  if (length(y_true) != length(y_pred)) {
    stop("`y_true` and `y_pred` must have the same length.", call. = FALSE)
  }
  y_true_f <- factor(as.character(y_true), levels = classes)
  y_pred_f <- factor(as.character(y_pred), levels = classes)
  if (anyNA(y_true_f)) {
    stop("`y_true` contains missing values or class labels unknown to the model.", call. = FALSE)
  }
  if (anyNA(y_pred_f)) {
    stop("`y_pred` contains missing values or unknown class labels.", call. = FALSE)
  }
 
  cm <- table(Observed = y_true_f, Predicted = y_pred_f)
  n <- sum(cm)
  tp <- diag(cm)
  support <- rowSums(cm)
  pred_count <- colSums(cm)
 
  precision <- ifelse(pred_count > 0, tp / pred_count, NA_real_)
  recall <- ifelse(support > 0, tp / support, NA_real_)
  denom <- support + pred_count
  f1 <- ifelse(denom > 0, 2 * tp / denom, NA_real_)
 
  accuracy <- sum(tp) / n
  expected <- sum(support * pred_count) / n^2
  kappa <- if (expected < 1) (accuracy - expected) / (1 - expected) else NA_real_
 
  list(
    confusion_matrix = cm,
    global = data.frame(
      metric = c("accuracy", "error_rate", "balanced_accuracy", "macro_f1", "cohen_kappa"),
      value = c(accuracy, 1 - accuracy, mean(recall, na.rm = TRUE), mean(f1, na.rm = TRUE), kappa),
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
 
 
 7. Pertinence des variables
# F de retrait (lambda de Wilks partiel), F univarié
# et norme des coefficients standardisés. Les pentes sont centrées entre classes
# car un décalage commun à toutes les classes ne change pas les prédictions.
compute_variable_relevance <- function(means, counts, within_cov, coefficients, groups,
                                       tol = 1e-8, n_cores = 1L) {
  S <- sscp_from_model(means, counts, within_cov)
  n <- S$n
  K <- S$K
  all_cols <- unlist(groups, use.names = FALSE)
  P <- length(all_cols)
  lambda_full <- wilks_lambda(S, all_cols, tol)
 
  slopes <- coefficients[-1L, , drop = FALSE]
  slopes <- slopes - rowMeans(slopes)
  std_slopes <- slopes * sqrt(pmax(diag(within_cov), 0))[rownames(slopes)]
 
  rows <- parallel_lapply(names(groups), function(g) {
    cg <- groups[[g]]
    m <- length(cg)
    lambda_without <- wilks_lambda(S, setdiff(all_cols, cg), tol)
    partial <- lambda_full / lambda_without
    rem <- rao_f_test(partial, m, K - 1L, n - K - (P - m))
    uni <- rao_f_test(wilks_lambda(S, cg, tol), m, K - 1L, n - K)
    data.frame(
      variable = g, n_columns = m,
      wilks_lambda_without = lambda_without, partial_lambda = partial,
      F = rem[["F"]], df1 = rem[["df1"]], df2 = rem[["df2"]], p_value = rem[["p_value"]],
      univariate_F = uni[["F"]], univariate_p_value = uni[["p_value"]],
      std_coef_norm = sqrt(sum(std_slopes[cg, , drop = FALSE]^2)),
      stringsAsFactors = FALSE
    )
  }, n_cores)
 
  out <- do.call(rbind, rows)
  out <- out[order(out$p_value, out$partial_lambda, -out$std_coef_norm), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "wilks_lambda") <- lambda_full
  out
}
 
8. Validation croisée
# Plis stratifiés : on mélange chaque classe puis on distribue les observations
# à tour de rôle entre les k plis, pour garder les proportions et des plis de même taille.
make_folds <- function(y, k) {
  ord <- unlist(lapply(split(seq_along(y), y), function(i) i[sample.int(length(i))]),
                use.names = FALSE)
  folds <- integer(length(y))
  folds[ord] <- rep_len(seq_len(k), length(y))
  folds
}
 
check_cv_inputs <- function(model, X, y) {
  if (!inherits(model, "LDA")) stop("`model` must be an LDA object.", call. = FALSE)
  if (!is.data.frame(X) && !is.matrix(X)) stop("'X' must be a data frame or a matrix.", call. = FALSE)
  if (is.data.frame(y)) y <- y[[1L]]
  if (length(y) != nrow(X)) stop("'y' must have one value per row of 'X'.", call. = FALSE)
  if (anyNA(y)) stop("'y' contains missing values.", call. = FALSE)
  droplevels(as.factor(y))
}
 
check_k <- function(k, n) {
  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k < 2 || k > n || k != round(k)) {
    stop("`k` must be an integer between 2 and the number of observations.", call. = FALSE)
  }
  as.integer(k)
}
 
#' Cross-validated evaluation of an LDA model
#'
#' Stratified k-fold cross-validation: the model is fitted on k - 1 folds and
#' evaluated on the remaining one, k times. All the settings of \code{model}
#' are reused (priors, \code{reg}, forward selection...); when a selection is
#' requested, it is run again inside each fold so that the test fold never
#' influences it.
#'
#' @param model An \code{\link{LDA}} object, fitted or not, whose settings are used.
#' @param X Data frame or matrix of explanatory variables.
#' @param y Target variable, one value per row of \code{X}.
#' @param k Number of folds.
#' @param seed Optional seed for a reproducible split (calls \code{set.seed}).
#' @param n_cores Number of cores; folds are fitted in parallel when > 1.
#' @param folds Optional vector giving the fold of each row; overrides
#'   \code{k} and \code{seed}.
#' @return A list with \code{summary} (mean and standard deviation of each
#'   global indicator over the folds), \code{folds} (indicators of each fold),
#'   \code{metrics} (indicators computed on all out-of-fold predictions, same
#'   structure as \code{LDA$evaluate()}) and \code{predictions} (out-of-fold
#'   predicted class of each observation).
#' @seealso \code{\link{tune_reg}}
#' @examples
#' cv <- cross_validate(LDA$new(), iris[, 1:4], iris$Species, k = 5, seed = 2026)
#' cv$summary
#' cv$metrics$confusion_matrix
#' @export
cross_validate <- function(model, X, y, k = 10, seed = NULL, n_cores = 1L, folds = NULL) {
  y <- check_cv_inputs(model, X, y)
  n <- nrow(X)
  n_cores <- check_n_cores(n_cores)
  if (is.null(folds)) {
    k <- check_k(k, n)
    if (!is.null(seed)) set.seed(seed)
    folds <- make_folds(y, k)
  } else if (length(folds) != n || anyNA(folds)) {
    stop("`folds` must give one fold per row of 'X'.", call. = FALSE)
  }
  ids <- sort(unique(folds))
 
  results <- parallel_lapply(ids, function(f) {
    test <- which(folds == f)
    m <- model$clone(deep = TRUE)
    if (n_cores > 1L) m$n_cores <- 1L
    warns <- character(0)
    tryCatch(
      withCallingHandlers(
        m$fit(X[-test, , drop = FALSE], y[-test]),
        warning = function(w) {
          warns <<- c(warns, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) stop(sprintf("Fold %d: %s", f, conditionMessage(e)), call. = FALSE)
    )
    list(test = test, pred = as.character(m$predict(X[test, , drop = FALSE])), warnings = warns)
  }, n_cores)
 
  classes <- levels(y)
  pred <- character(n)
  for (r in results) pred[r$test] <- r$pred
  pred <- factor(pred, levels = classes)
 
  fold_scores <- do.call(rbind, lapply(seq_along(results), function(i) {
    g <- compute_classification_metrics(y[results[[i]]$test], results[[i]]$pred, classes)$global
    data.frame(fold = ids[i], t(stats::setNames(g$value, g$metric)))
  }))
  metric_names <- setdiff(names(fold_scores), "fold")
  summary <- data.frame(
    metric = metric_names,
    mean = vapply(fold_scores[metric_names], mean, numeric(1), na.rm = TRUE),
    sd = vapply(fold_scores[metric_names], stats::sd, numeric(1), na.rm = TRUE),
    row.names = NULL, stringsAsFactors = FALSE
  )
 
  warns <- unique(unlist(lapply(results, `[[`, "warnings")))
  if (length(warns)) warning(paste(warns, collapse = "\n"), call. = FALSE)
 
  list(summary = summary, folds = fold_scores,
       metrics = compute_classification_metrics(y, pred, classes), predictions = pred)
}
 
 
9. Choix de reg par validation croisée (grandes dimensions)
# Toutes les valeurs de reg sont testées sur les mêmes plis pour être comparables.
 
#' Choose the regularisation parameter by cross-validation
#'
#' Each value of \code{grid} is evaluated by stratified k-fold
#' cross-validation on the same folds. The value with the best mean score is
#' kept (the smallest one in case of a tie) and the model is refitted with it
#' on all the data. Mostly useful when there are many variables compared with
#' the number of observations.
#'
#' @inheritParams cross_validate
#' @param grid Values of \code{reg} to try, in [0, 1].
#' @param metric Global indicator to maximise.
#' @return A list with \code{results} (mean and standard deviation of
#'   \code{metric} for each value of \code{reg}), \code{metric},
#'   \code{best_reg} and \code{model} (a copy of \code{model} refitted on all
#'   the data with \code{best_reg}).
#' @seealso \code{\link{cross_validate}}
#' @examples
#' tuned <- tune_reg(LDA$new(), iris[, 1:4], iris$Species,
#'                   grid = c(0, 0.1, 0.5, 0.9), k = 5, seed = 2026)
#' tuned$results
#' tuned$best_reg
#' @export
tune_reg <- function(model, X, y, grid = c(0, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9),
                     k = 10, metric = c("accuracy", "balanced_accuracy", "macro_f1", "cohen_kappa"),
                     seed = NULL, n_cores = 1L) {
  metric <- match.arg(metric)
  yf <- check_cv_inputs(model, X, y)
  if (!is.numeric(grid) || !length(grid) || anyNA(grid) || any(grid < 0 | grid > 1)) {
    stop("`grid` must contain values in [0, 1].", call. = FALSE)
  }
  grid <- sort(unique(grid))
  k <- check_k(k, nrow(X))
  if (!is.null(seed)) set.seed(seed)
  folds <- make_folds(yf, k)
 
  scores <- lapply(grid, function(r) {
    m <- model$clone(deep = TRUE)
    m$reg <- r
    s <- suppressWarnings(cross_validate(m, X, yf, folds = folds, n_cores = n_cores))$summary
    s[s$metric == metric, c("mean", "sd")]
  })
  results <- data.frame(reg = grid, do.call(rbind, scores), row.names = NULL)
  if (all(is.na(results$mean))) stop("The metric could not be computed for any value of `reg`.", call. = FALSE)
 
  best_reg <- results$reg[which.max(results$mean)]
  final <- model$clone(deep = TRUE)
  final$reg <- best_reg
  final$fit(X, yf)
  list(results = results, metric = metric, best_reg = best_reg, model = final)
}
