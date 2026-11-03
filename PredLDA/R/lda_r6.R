#' PredLDA: Predictive Linear Discriminant Analysis
#'
#' The package provides the R6 class \code{\link{LDA}}, which fits a predictive
#' linear discriminant analysis on a data frame of explanatory variables and a
#' categorical target (factor with two or more levels).
#'
#' @keywords internal
"_PACKAGE"

#' Predictive Linear Discriminant Analysis (R6 class)
#'
#' @description
#' R6 class implementing predictive linear discriminant analysis (LDA) with an
#' interface modelled on 'scikit-learn': create the object with
#' \code{LDA$new()}, train it with \code{$fit(X, y)}, then use
#' \code{$predict(X)} and \code{$predict_proba(X)} on new data.
#'
#' @details
#' Under the LDA hypotheses (multivariate normal classes sharing a common
#' covariance matrix), the Bayes rule assigns an observation \eqn{x} to the
#' class \eqn{k} maximising the linear classification function
#' \deqn{d_k(x) = a_{0k} + a_k^\top x}{d_k(x) = a0_k + a_k' x}
#' with
#' \deqn{a_k = W^{-1} \mu_k, \qquad a_{0k} = \log \pi_k - \frac{1}{2} \mu_k^\top W^{-1} \mu_k}{a_k = W^-1 mu_k,  a0_k = log(pi_k) - 0.5 mu_k' W^-1 mu_k}
#' where \eqn{\mu_k} is the mean vector of class \eqn{k}, \eqn{\pi_k} its prior
#' probability and \eqn{W} the pooled within-class covariance matrix
#' \deqn{W = \frac{1}{n - K} \sum_{k=1}^{K} \sum_{i : y_i = k} (x_i - \mu_k)(x_i - \mu_k)^\top.}{W = 1/(n - K) sum_k sum_{i in k} (x_i - mu_k)(x_i - mu_k)'.}
#'
#' Posterior probabilities are obtained with a numerically stable softmax of
#' the scores: \eqn{P(Y = k \mid x) = \exp(d_k(x)) / \sum_j \exp(d_j(x))}.
#'
#' \strong{Implementation.} The class means are computed in a single pass with
#' \code{rowsum()}, \eqn{W} with one \code{crossprod()} of the class-centred
#' data, and \eqn{W^{-1}} through its Cholesky factor. When \eqn{W} is singular
#' or ill-conditioned (collinear or constant variables, more variables than
#' observations), a Moore-Penrose pseudo-inverse based on the eigen
#' decomposition is used instead and a warning is issued.
#'
#' The explanatory variables must currently be numeric. Columns are matched by
#' name between \code{$fit()} and \code{$predict()}, so the column order of new
#' data does not matter.
#'
#' @section Reading the results:
#' \code{$coefficients} holds one column per class: the intercept
#' (\code{"(Intercept)"}) followed by one coefficient per explanatory
#' variable. An observation is assigned to the class whose function gives the
#' highest score. With two classes, the difference of the two columns gives a
#' single score whose sign decides the class.
#'
#' @references
#' Hastie T., Tibshirani R., Friedman J. (2009). \emph{The Elements of
#' Statistical Learning}, 2nd ed., Springer, section 4.3.
#'
#' Rakotomalala R. (2020). \emph{Analyse discriminante linéaire}, course notes,
#' Université Lyon 2. \url{https://eric.univ-lyon2.fr/ricco/cours/}
#'
#' @seealso \code{\link[MASS]{lda}} for the reference implementation.
#'
#' @examples
#' model <- LDA$new()
#' model$fit(iris[, 1:4], iris$Species)
#' model
#'
#' # Classification functions (one column per class)
#' model$coefficients
#'
#' # Predictions on new data
#' new_obs <- iris[c(1, 51, 101), 1:4]
#' model$predict(new_obs)
#' model$predict_proba(new_obs)
#'
#' # Detailed output
#' model$summary()
#'
#' # User-defined priors
#' model_eq <- LDA$new(priors = c(setosa = 0.2, versicolor = 0.4, virginica = 0.4))
#' model_eq$fit(iris[, 1:4], iris$Species)$predict(new_obs)
#'
#' @importFrom R6 R6Class
#' @export
LDA <- R6::R6Class(
  "LDA",

  public = list(

    #' @field priors Prior class probabilities (named numeric vector). Set by
    #'   the user in the constructor, or estimated from the class frequencies
    #'   by \code{$fit()}.
    priors = NULL,

    #' @field tol Relative tolerance used to detect a singular within-class
    #'   covariance matrix.
    tol = NULL,

    #' @field classes Levels of the target variable (character vector).
    classes = NULL,

    #' @field class_counts Number of training observations per class.
    class_counts = NULL,

    #' @field feature_names Names of the explanatory variables used by the model.
    feature_names = NULL,

    #' @field n_obs Number of training observations.
    n_obs = NULL,

    #' @field means Matrix of class means (classes in rows, variables in columns).
    means = NULL,

    #' @field within_cov Pooled within-class covariance matrix \eqn{W}
    #'   (unbiased, divided by \eqn{n - K}).
    within_cov = NULL,

    #' @field coefficients Matrix of classification functions: the intercept
    #'   in the first row, then one row per variable; one column per class.
    coefficients = NULL,

    #' @field is_fitted \code{TRUE} once \code{$fit()} has run successfully.
    is_fitted = FALSE,

    #' @description Create a new, unfitted LDA model.
    #' @param priors Optional prior class probabilities: a positive numeric
    #'   vector with one value per class. If named, names are matched to the
    #'   class levels; otherwise values are taken in the order of
    #'   \code{levels(y)}. Values are rescaled to sum to 1. When \code{NULL}
    #'   (default), priors are the class proportions in the training data.
    #' @param tol Relative tolerance below which an eigenvalue of \eqn{W} is
    #'   considered null (default \code{1e-8}).
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

    #' @description Fit the model on training data.
    #' @param X Data frame (or matrix) of numeric explanatory variables.
    #' @param y Target variable: a factor (character or logical vectors are
    #'   converted) of length \code{nrow(X)} with at least two classes. Unused
    #'   levels are dropped.
    #' @return The object itself, invisibly, so that calls can be chained.
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

      # Statistical computations are in stats_utils.R.
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

    #' @description Compute the scores of the classification functions.
    #' @param X Data frame (or matrix) with the same variables as in
    #'   \code{$fit()}.
    #' @return Numeric matrix of scores (observations in rows, classes in
    #'   columns).
    decision_function = function(X) {
      private$check_fitted()
      X <- private$prepare_X(X, training = FALSE)
      S <- X %*% self$coefficients[-1L, , drop = FALSE]
      S <- sweep(S, 2L, self$coefficients[1L, ], `+`)
      dimnames(S) <- list(rownames(X), self$classes)
      S
    },

    #' @description Posterior class membership probabilities.
    #' @param X Data frame (or matrix) with the same variables as in
    #'   \code{$fit()}.
    #' @return Numeric matrix of probabilities (observations in rows, classes
    #'   in columns); each row sums to 1.
    predict_proba = function(X) {
      softmax_rows(self$decision_function(X))
    },

    #' @description Predict the class of each observation.
    #' @param X Data frame (or matrix) with the same variables as in
    #'   \code{$fit()}.
    #' @return Factor of predicted classes, with the same levels as the
    #'   training target.
    predict = function(X) {
      S <- self$decision_function(X)
      factor(self$classes[max.col(S, ties.method = "first")], levels = self$classes)
    },

    #' @description Proportion of correctly classified observations.
    #' @param X Data frame (or matrix) of explanatory variables.
    #' @param y True classes.
    #' @return Accuracy, a number between 0 and 1.
    score = function(X, y) {
      pred <- self$predict(X)
      if (length(y) != length(pred)) {
        stop("'y' must have one value per row of 'X'.", call. = FALSE)
      }
      mean(as.character(pred) == as.character(y))
    },

    #' @description Print a short description of the model.
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

    #' @description Print detailed information on the model: class
    #'   distribution and priors, class means and classification functions.
    #' @param digits Number of decimal places displayed (default 4).
    #' @return Invisibly, a list with elements \code{class_distribution},
    #'   \code{means} and \code{coefficients}.
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

    # Convert X to a numeric matrix. At prediction time, columns are matched
    # by name to those seen during fit().
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
