# Statistical building blocks of the LDA (internal, inputs already validated).

class_means <- function(X, y, counts) {
  rowsum(X, y, reorder = TRUE) / counts
}

# Pooled within-class covariance: W = Xc'Xc / (n - K), Xc centred on class means.
pooled_within_cov <- function(X, y, means) {
  Xc <- X - means[as.integer(y), , drop = FALSE]
  crossprod(Xc) / (nrow(X) - nlevels(y))
}

# Inverse via Cholesky; falls back to a Moore-Penrose pseudo-inverse (eigen)
# when W is singular or ill-conditioned.
inverse_cov <- function(W, tol) {
  R <- tryCatch(chol(W), error = function(e) NULL)
  d <- if (is.null(R)) 0 else diag(R)^2
  if (!is.null(R) && min(d) > tol * max(d)) {
    pseudo <- FALSE
    W_inv <- chol2inv(R)
  } else {
    e <- eigen(W, symmetric = TRUE)
    keep <- e$values > tol * max(e$values[1L], .Machine$double.eps)
    warning("The within-class covariance matrix is singular (collinear or constant ",
            "variables); a pseudo-inverse is used.", call. = FALSE)
    pseudo <- TRUE
    V <- e$vectors[, keep, drop = FALSE]
    W_inv <- V %*% (t(V) / e$values[keep])
  }
  dimnames(W_inv) <- dimnames(W)
  list(inverse = W_inv, pseudo = pseudo)
}

# User priors aligned on class levels and rescaled; default = class proportions.
resolve_priors <- function(priors, counts) {
  classes <- names(counts)
  if (is.null(priors)) return(counts / sum(counts))
  if (length(priors) != length(classes)) {
    stop(sprintf("'priors' must have %d values (one per class).", length(classes)), call. = FALSE)
  }
  if (!is.null(names(priors))) {
    if (!setequal(names(priors), classes)) {
      stop("Names of 'priors' must match the classes: ", paste(classes, collapse = ", "), call. = FALSE)
    }
    priors <- priors[classes]
  }
  priors <- priors / sum(priors)
  names(priors) <- classes
  priors
}

# a_k = W^-1 mu_k,  a0_k = log(pi_k) - 0.5 mu_k' W^-1 mu_k
# Returns intercept in row 1, then one row per variable; one column per class.
classification_functions <- function(means, W_inv, priors) {
  A <- W_inv %*% t(means)
  a0 <- log(priors) - 0.5 * colSums(t(means) * A)
  coefs <- rbind(a0, A)
  dimnames(coefs) <- list(c("(Intercept)", colnames(means)), rownames(means))
  coefs
}

# Row-wise softmax; the row max is subtracted first to avoid overflow.
softmax_rows <- function(S) {
  S <- exp(S - S[cbind(seq_len(nrow(S)), max.col(S, ties.method = "first"))])
  S / rowSums(S)
}
