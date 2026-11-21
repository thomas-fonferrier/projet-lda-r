1. Régularisation de la matrice intra-classes W 

# W_reg = (1 - reg) * W + reg * diag(W), avec reg dans [0, 1].
# Rétrécit W vers sa diagonale : W_reg est inversible dès que toutes les
# variances intra-classes sont > 0, même quand p > n - K.
regularize_within_cov <- function(W, reg = 0) {
  if (!is.numeric(reg) || length(reg) != 1L || is.na(reg) || reg < 0 || reg > 1) {
    stop("`reg` must be a single numeric value in [0, 1].", call. = FALSE)
  }
  if (reg == 0) return(W)
  p <- nrow(W)
  diag_target <- diag(diag(W), nrow = p, ncol = p)
  (1 - reg) * W + reg * diag_target
}
 
2. Encodage des variables explicatives
 
# Apprend l'encodage sur les données d'apprentissage uniquement.
# - variable numérique : gardée telle quelle ;
# - variable catégorielle (facteur, texte, logique) : une indicatrice 0/1 par
#   modalité SAUF la première, qui sert de référence. Avec toutes les
#   modalités, les indicatrices d'une variable somment à 1 et W est singulière.
# `groups` relie chaque variable d'origine à ses colonnes encodées : la
# sélection et la pertinence traitent une variable catégorielle comme un bloc.
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
  groups <- list()
 
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
      # Ordre des modalités : celui du facteur s'il existe (comme lm()),
      # sinon l'ordre alphabétique. La première modalité est la référence.
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
    stop("Encoded column names clash (e.g. a column named 'x_a' and a categorical 'x' with level 'a'). ",
         "Please rename the columns.", call. = FALSE)
  }
 
  list(
    raw_names = names(df),
    col_types = col_types,
    factor_levels = factor_levels,
    groups = groups,
    encoded_cols = encoded_cols
  )
}
 
# Applique un encodage déjà appris (apprentissage ou prédiction) et renvoie
# une matrice numérique dont les colonnes suivent encoder$encoded_cols.
# Une modalité jamais vue à l'apprentissage provoque une erreur explicite.
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
 
  # Conserver les noms de lignes explicites (comme as.matrix()).
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
 
# Restreint un encodage aux variables retenues (ordre d'origine conservé).
subset_encoder <- function(encoder, keep) {
  keep <- encoder$raw_names[encoder$raw_names %in% keep]
  encoder$raw_names <- keep
  encoder$col_types <- encoder$col_types[keep]
  encoder$factor_levels <- encoder$factor_levels[intersect(names(encoder$factor_levels), keep)]
  encoder$groups <- encoder$groups[keep]
  encoder$encoded_cols <- unlist(encoder$groups, use.names = FALSE)
  encoder
}
 
 
 6.Parallélisation 
 
check_n_cores <- function(n_cores) {
  if (!is.numeric(n_cores) || length(n_cores) != 1L || is.na(n_cores) ||
      n_cores < 1 || n_cores != round(n_cores)) {
    stop("`n_cores` must be a single integer >= 1.", call. = FALSE)
  }
  as.integer(n_cores)
}
 
# lapply(), en parallèle si n_cores > 1 : processus forkés (mclapply) sous
# Linux/macOS, cluster PSOCK sous Windows. Utile seulement quand il y a
# beaucoup de variables : pour quelques variables, le coût de lancement
# dépasse le gain.
parallel_lapply <- function(X, FUN, n_cores = 1L,
                            use_fork = .Platform$OS.type != "windows") {
  n_cores <- min(check_n_cores(n_cores), length(X))
  if (n_cores <= 1L) return(lapply(X, FUN))
  if (use_fork) {
    res <- parallel::mclapply(X, FUN, mc.cores = n_cores)
    failed <- vapply(res, inherits, logical(1), what = "try-error")
    if (any(failed)) stop("Parallel computation failed: ", as.character(res[[which(failed)[1L]]]), call. = FALSE)
    return(res)
  }
  cl <- parallel::makePSOCKcluster(n_cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::parLapply(cl, X, FUN)
}
 
 
 Outils communs : lambda de Wilks 

# Lambda de Wilks d'un ensemble de colonnes : det(W) / det(T), avec W la
# matrice SCE intra-classes et T la matrice SCE totale (T = W + B).
# Proche de 0 : classes bien séparées ; égal à 1 : aucune séparation.
 
# Met W et T à l'échelle de T (T devient une matrice de corrélation).
# Lambda ne change pas, mais la tolérance de singularité devient comparable
# d'une variable à l'autre, quelles que soient leurs unités.
scale_sscp <- function(W, Tt, n, K) {
  s <- sqrt(pmax(diag(Tt), 0))
  s[!(s > 0)] <- 1                       # variable constante : restera singulière
  d <- outer(s, s)
  list(W = W / d, T = Tt / d, n = n, K = K)
}
 
# SCE à partir des données encodées (sélection de variables).
sscp_from_data <- function(X, y) {
  counts <- tabulate(y, nlevels(y))
  M <- rowsum(X, y, reorder = TRUE) / counts
  Xw <- X - M[as.integer(y), , drop = FALSE]
  Xt <- sweep(X, 2L, colMeans(X))
  scale_sscp(crossprod(Xw), crossprod(Xt), nrow(X), nlevels(y))
}
 
# SCE à partir d'un modèle ajusté (pertinence), sans relire les données :
# W = within_cov * (n - K) et B = somme_k n_k (mu_k - mu)(mu_k - mu)'.
sscp_from_model <- function(means, counts, within_cov) {
  n <- sum(counts)
  K <- length(counts)
  W <- within_cov * (n - K)
  mu <- colSums(means * counts) / n
  D <- sweep(means, 2L, mu) * sqrt(counts)
  scale_sscp(W, W + crossprod(D), n, K)
}
 
# log(det(M)) pour M symétrique ; -Inf si M est (quasi) singulière.
log_det_pd <- function(M, tol) {
  if (!length(M)) return(0)
  R <- tryCatch(chol(M), error = function(e) NULL)
  if (is.null(R)) return(-Inf)
  d2 <- diag(R)^2
  if (min(d2) <= tol * max(d2)) return(-Inf)
  sum(log(d2))
}
 
# Lambda de Wilks des colonnes `cols` ; NA si elles sont colinéaires,
# 0 si W est singulière mais pas T (séparation parfaite des classes).
wilks_lambda <- function(S, cols, tol = 1e-8) {
  if (!length(cols)) return(1)
  lt <- log_det_pd(S$T[cols, cols, drop = FALSE], tol)
  if (!is.finite(lt)) return(NA_real_)
  exp(log_det_pd(S$W[cols, cols, drop = FALSE], tol) - lt)
}
 
# Test F d'un lambda de Wilks partiel (approximation de Rao) :
#   p   = nombre de colonnes testées (1 pour une variable numérique,
#         nombre de modalités - 1 pour une variable catégorielle),
#   q_h = K - 1, q_e = n - K - (nombre de colonnes déjà dans le modèle).
# Le test est exact quand p = 1 ou K = 2 : on retrouve alors
#   F = (q_e / (K - 1)) * (1 - lambda) / lambda.
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
 
 
3. Sélection de variables : STEPDISC ascendant 

# Part d'un modèle vide. À chaque étape, on teste l'ajout de chaque variable
# restante (une variable catégorielle entre ou sort en bloc) et on ajoute
# celle dont le F partiel a la plus petite p-value, si elle est < alpha.
# Contrairement à un classement univarié (score F de l'ANOVA), une variable
# redondante avec celles déjà retenues n'apporte presque rien et n'entre pas.
# La 1re étape correspond exactement à l'ANOVA univariée.
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
 
 
4. Indicateurs d'évaluation globale et par classe 
 
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
  # Kappa de Cohen : accord corrigé de l'accord attendu par hasard.
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
 
 
5. Pertinence des variables 

# Pour chaque variable d'origine (bloc d'indicatrices si catégorielle) :
# - F de retrait : test du lambda de Wilks partiel lambda(modèle complet) /
#   lambda(modèle sans la variable). Petite p-value : la variable apporte une
#   information discriminante que les autres n'apportent pas ;
# - F univarié : pouvoir discriminant de la variable seule (ANOVA à 1 facteur
#   pour une variable numérique) ;
# - std_coef_norm : norme des coefficients standardisés (pentes x écart-type
#   intra-classe), CENTRÉS entre classes. Ajouter le même vecteur aux pentes
#   de toutes les classes ne change aucune prédiction : sans centrage, cette
#   partie commune gonflerait la mesure.
# Les tests utilisent la matrice W non régularisée ; si W est singulière
# (p trop grand), ils valent NA et le tri se fait sur std_coef_norm.
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
 
