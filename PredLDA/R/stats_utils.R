# Statistical building blocks of the LDA, independent of the R6 class.
# All functions are internal: they take a numeric matrix X and a factor y
# already validated by the class.

#' Class means
#'
#' Computed in a single pass: \code{rowsum()} sums the rows of \code{X} by class.
#' @param X Numeric matrix of explanatory variables.
#' @param y Factor of classes, one value per row of \code{X}.
#' @param counts Number of observations per class, in the order of \code{levels(y)}.
#' @return Matrix of class means (classes in rows, variables in columns).
#' @noRd
class_means <- function(X, y, counts) {
  rowsum(X, y, reorder = TRUE) / counts
}

#' Pooled within-class covariance matrix
#'
#' Each row is centred on its class mean, then
#' \eqn{W = X_c^\top X_c / (n - K)} (unbiased estimator).
#' @param X Numeric matrix of explanatory variables.
#' @param y Factor of classes.
#' @param means Matrix of class means returned by \code{class_means()}.
#' @return Symmetric \eqn{p \times p} matrix.
#' @noRd
pooled_within_cov <- function(X, y, means) {
  Xc <- X - means[as.integer(y), , drop = FALSE]
  crossprod(Xc) / (nrow(X) - nlevels(y))
}

#' Inverse of a covariance matrix
#'
#' Uses the Cholesky factor; falls back to a Moore-Penrose pseudo-inverse
#' (eigen decomposition) with a warning when the matrix is singular or
#' ill-conditioned.
#' @param W Symmetric covariance matrix.
#' @param tol Relative tolerance below which an eigenvalue is considered null.
#' @return A list with \code{inverse} (the matrix) and \code{pseudo}
#'   (\code{TRUE} when the pseudo-inverse was used).
#' @noRd
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

#' Prior class probabilities
#'
#' Priors given by the user are aligned on the class levels and rescaled;
#' otherwise they are the class proportions.
#' @param priors User priors (\code{NULL}, or a positive numeric vector,
#'   optionally named by class).
#' @param counts Named vector of class counts.
#' @return Named numeric vector summing to 1.
#' @noRd
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

#' Linear classification functions
#'
#' \eqn{a_k = W^{-1} \mu_k} and
#' \eqn{a_{0k} = \log \pi_k - \frac{1}{2} \mu_k^\top W^{-1} \mu_k}.
#' @param means Matrix of class means (classes in rows).
#' @param W_inv Inverse of the pooled within-class covariance matrix.
#' @param priors Prior class probabilities.
#' @return Matrix with the intercept in the first row, then one row per
#'   variable; one column per class.
#' @noRd
classification_functions <- function(means, W_inv, priors) {
  A <- W_inv %*% t(means)
  a0 <- log(priors) - 0.5 * colSums(t(means) * A)
  coefs <- rbind(a0, A)
  dimnames(coefs) <- list(c("(Intercept)", colnames(means)), rownames(means))
  coefs
}

#' Row-wise softmax
#'
#' Numerically stable: the row maximum is subtracted before \code{exp()} to
#' avoid overflow.
#' @param S Numeric matrix of scores.
#' @return Matrix of the same shape whose rows sum to 1.
#' @noRd
softmax_rows <- function(S) {
  S <- exp(S - S[cbind(seq_len(nrow(S)), max.col(S, ties.method = "first"))])
  S / rowSums(S)
}
