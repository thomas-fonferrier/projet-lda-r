#' PredLDA: Predictive Linear Discriminant Analysis
#'
#' Provides the R6 class \code{\link{LDA}}.
#'
#' @keywords internal
"_PACKAGE"

#' Predictive Linear Discriminant Analysis (R6 class)
#'
#' @description
#' Predictive LDA with a 'scikit-learn'-like interface: \code{LDA$new()},
#' \code{$fit(X, y)}, then \code{$predict(X)} / \code{$predict_proba(X)}.
#'
#' @details
#' Each class \eqn{k} gets a linear classification function
#' \eqn{d_k(x) = a_{0k} + a_k^\top x} with \eqn{a_k = W^{-1} \mu_k} and
#' \eqn{a_{0k} = \log \pi_k - \frac{1}{2} \mu_k^\top W^{-1} \mu_k}, where
#' \eqn{W} is the pooled within-class covariance matrix. Observations are
#' assigned to the class with the highest score. If \eqn{W} is singular, a
#' pseudo-inverse is used and a warning is issued. Variables must be numeric.
#'
#' @references
#' Hastie T., Tibshirani R., Friedman J. (2009). \emph{The Elements of
#' Statistical Learning}, 2nd ed., Springer, section 4.3.
#'
#' @seealso \code{\link[MASS]{lda}}
#'
#' @examples
#' model <- LDA$new()
#' model$fit(iris[, 1:4], iris$Species)
#' model$coefficients
#' new_obs <- iris[c(1, 51, 101), 1:4]
#' model$predict(new_obs)
#' model$predict_proba(new_obs)
#' model$summary()
#'
#' @importFrom R6 R6Class
#' @export
LDA <- R6::R6Class(
  "LDA",

  public = list(

    #' @field priors Prior class probabilities.
    priors = NULL,
    #' @field tol Tolerance used to detect a singular \eqn{W}.
    tol = NULL,
    #' @field classes Levels of the target variable.
    classes = NULL,
    #' @field class_counts Training observations per class.
    class_counts = NULL,
    #' @field feature_names Names of the explanatory variables.
    feature_names = NULL,
    #' @field n_obs Number of training observations.
    n_obs = NULL,
    #' @field means Class means (classes in rows).
    means = NULL,
    #' @field within_cov Pooled within-class covariance matrix \eqn{W}.
    within_cov = NULL,
    #' @field coefficients Classification functions (intercept first, one column per class).
    coefficients = NULL,
    #' @field is_fitted \code{TRUE} once \code{$fit()} has run.
    is_fitted = FALSE,

    #' @description Create a new, unfitted LDA model.
    #' @param priors Optional positive vector of class priors (named or in
    #'   \code{levels(y)} order), rescaled to sum to 1. Default: class proportions.
    #' @param tol Relative tolerance for null eigenvalues of \eqn{W}.
    #' @return A new \code{LDA} object.
    initialize = function(priors = NULL, tol = 1e-8) {
      if (!is.null(priors)) {
        if (!is.numeric(priors) || anyNA(priors) || any(priors <= 0)) {
          stop("'priors' must be a vector of strictly positive numbers.", call. = FALSE)
        }
      }
      if (!is.numeric(tol) || length(tol) != 1L || is.na(tol) || tol <= 0) {
        stop("'tol' must be a single strictly positive number.", call. = FALSE)
      }
      private$user_priors <- priors
      self$tol <- tol
      invisible(self)
    },

    #' @description Fit the model.
    #' @param X Data frame or matrix of numeric variables.
    #' @param y Target (factor, at least two classes), one value per row of \code{X}.
    #' @return The object, invisibly (chainable).
    fit = function(X, y) {
      X <- private$prepare_X(X, training = TRUE)
      y <- private$prepare_y(y, nrow(X))

      n <- nrow(X)
      K <- nlevels(y)
      if (n <= K) {
        stop("The number of observations must exceed the number of classes.", call. = FALSE)
      }

      counts <- tabulate(y, nbins = K)
      names(counts) <- levels(y)

      M <- class_means(X, y, counts)
      W <- pooled_within_cov(X, y, M)
      W_inv <- inverse_cov(W, self$tol)
      pri <- resolve_priors(private$user_priors, counts)
      coefs <- classification_functions(M, W_inv$inverse, pri)

      private$used_pseudo_inverse <- W_inv$pseudo
      self$classes <- levels(y)
      self$class_counts <- counts
      self$priors <- pri
      self$feature_names <- colnames(X)
      self$n_obs <- n
      self$means <- M
      self$within_cov <- W
      self$coefficients <- coefs
      self$is_fitted <- TRUE
      invisible(self)
    },

    #' @description Scores of the classification functions.
    #' @param X Data frame or matrix with the variables used in \code{$fit()}.
    #' @return Matrix of scores (observations x classes).
    decision_function = function(X) {
      private$check_fitted()
      X <- private$prepare_X(X, training = FALSE)
      S <- X %*% self$coefficients[-1L, , drop = FALSE]
      S <- sweep(S, 2L, self$coefficients[1L, ], `+`)
      dimnames(S) <- list(rownames(X), self$classes)
      S
    },

    #' @description Posterior class probabilities.
    #' @param X Data frame or matrix with the variables used in \code{$fit()}.
    #' @return Matrix of probabilities (observations x classes).
    predict_proba = function(X) {
      softmax_rows(self$decision_function(X))
    },

    #' @description Predicted classes.
    #' @param X Data frame or matrix with the variables used in \code{$fit()}.
    #' @return Factor with the training levels.
    predict = function(X) {
      S <- self$decision_function(X)
      factor(self$classes[max.col(S, ties.method = "first")], levels = self$classes)
    },

    #' @description Accuracy on \code{(X, y)}.
    #' @param X Explanatory variables.
    #' @param y True classes.
    #' @return Proportion of correct predictions.
    score = function(X, y) {
      pred <- self$predict(X)
      if (length(y) != length(pred)) {
        stop("'y' must have one value per row of 'X'.", call. = FALSE)
      }
      mean(as.character(pred) == as.character(y))
    },

    #' @description Print the model.
    #' @param ... Ignored.
    print = function(...) {
      cat("Predictive Linear Discriminant Analysis (R6 class LDA)\n")
      if (!self$is_fitted) {
        cat("Model not fitted yet: call $fit(X, y).\n")
        return(invisible(self))
      }
      cat(sprintf("  Observations : %d\n", self$n_obs))
      cat(sprintf("  Variables    : %d\n", length(self$feature_names)))
      cat(sprintf("  Classes (%d)  : %s\n", length(self$classes),
                  paste(self$classes, collapse = ", ")))
      cat("\nClassification functions:\n")
      print(round(self$coefficients, 4))
      invisible(self)
    },

    #' @description Detailed summary of the fitted model.
    #' @param digits Decimal places displayed.
    #' @return Invisibly, a list with \code{class_distribution}, \code{means}
    #'   and \code{coefficients}.
    summary = function(digits = 4) {
      private$check_fitted()
      distribution <- data.frame(
        count = as.integer(self$class_counts),
        proportion = as.numeric(self$class_counts) / self$n_obs,
        prior = as.numeric(self$priors),
        row.names = self$classes
      )

      cat("==== Predictive Linear Discriminant Analysis ====\n\n")
      cat(sprintf("Observations: %d   Variables: %d   Classes: %d\n",
                  self$n_obs, length(self$feature_names), length(self$classes)))
      if (private$used_pseudo_inverse) {
        cat("Note: W was singular; a pseudo-inverse was used.\n")
      }
      cat("\n-- Class distribution and priors --\n")
      print(round(distribution, digits))
      cat("\n-- Class means --\n")
      print(round(self$means, digits))
      cat("\n-- Classification functions --\n")
      print(round(self$coefficients, digits))
      if (length(self$classes) == 2L) {
        cat(sprintf("\n-- Score function D(x) = d_%s(x) - d_%s(x) --\n",
                    self$classes[1L], self$classes[2L]))
        cat(sprintf("(D(x) > 0 => %s, otherwise %s)\n", self$classes[1L], self$classes[2L]))
        print(round(self$coefficients[, 1L] - self$coefficients[, 2L], digits))
      }

      invisible(list(
        class_distribution = distribution,
        means = self$means,
        coefficients = self$coefficients
      ))
    }
  ),

  private = list(
    user_priors = NULL,
    used_pseudo_inverse = FALSE,

    check_fitted = function() {
      if (!self$is_fitted) {
        stop("The model is not fitted yet: call $fit(X, y) first.", call. = FALSE)
      }
    },

    # At prediction time, columns are matched by name to those seen in fit().
    prepare_X = function(X, training) {
      if (is.numeric(X) && is.null(dim(X))) {
        X <- matrix(X, nrow = 1L, dimnames = list(NULL, names(X)))
      }
      if (!is.data.frame(X) && !is.matrix(X)) {
        stop("'X' must be a data frame or a matrix.", call. = FALSE)
      }
      if (nrow(X) == 0L) stop("'X' has no rows.", call. = FALSE)

      if (training) {
        if (ncol(X) == 0L) stop("'X' has no columns.", call. = FALSE)
        if (is.null(colnames(X))) colnames(X) <- paste0("X", seq_len(ncol(X)))
        if (anyDuplicated(colnames(X))) stop("Column names of 'X' must be unique.", call. = FALSE)
      } else {
        if (is.null(colnames(X))) {
          if (ncol(X) != length(self$feature_names)) {
            stop(sprintf("'X' must have %d columns.", length(self$feature_names)), call. = FALSE)
          }
          colnames(X) <- self$feature_names
        }
        missing <- setdiff(self$feature_names, colnames(X))
        if (length(missing) > 0L) {
          stop("Missing variables in 'X': ", paste(missing, collapse = ", "), call. = FALSE)
        }
        X <- X[, self$feature_names, drop = FALSE]
      }

      is_num <- if (is.data.frame(X)) vapply(X, is.numeric, logical(1)) else rep(is.numeric(X), ncol(X))
      if (!all(is_num)) {
        stop("Non-numeric variables are not supported: ",
             paste(colnames(X)[!is_num], collapse = ", "), call. = FALSE)
      }
      X <- as.matrix(X)
      storage.mode(X) <- "double"
      if (anyNA(X)) stop("'X' contains missing values.", call. = FALSE)
      if (any(!is.finite(X))) stop("'X' contains infinite values.", call. = FALSE)
      X
    },

    prepare_y = function(y, n) {
      if (is.data.frame(y)) {
        if (ncol(y) != 1L) stop("'y' must be a single variable.", call. = FALSE)
        y <- y[[1L]]
      }
      if (length(y) != n) stop("'y' must have one value per row of 'X'.", call. = FALSE)
      if (anyNA(y)) stop("'y' contains missing values.", call. = FALSE)
      y <- droplevels(as.factor(y))
      if (nlevels(y) < 2L) stop("'y' must have at least two classes.", call. = FALSE)
      y
    }
  )
)
