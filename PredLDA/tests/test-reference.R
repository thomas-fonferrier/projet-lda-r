# Validation of the LDA R6 class on classic R datasets.
#  1. Posterior probabilities and classes are compared with MASS::lda.
#  2. Classification functions are compared with a direct textbook
#     computation (per-class cov() and solve()), independent of the class code.
library(PredLDA)

# Classification functions computed from the textbook formulas.
reference_coefficients <- function(X, y) {
  X <- as.matrix(X)
  y <- droplevels(as.factor(y))
  n <- nrow(X); K <- nlevels(y)
  M <- t(sapply(levels(y), function(k) colMeans(X[y == k, , drop = FALSE])))
  W <- Reduce(`+`, lapply(levels(y), function(k) {
    (sum(y == k) - 1) * cov(X[y == k, , drop = FALSE])
  })) / (n - K)
  pri <- as.numeric(table(y)) / n
  A <- solve(W, t(M))
  a0 <- log(pri) - 0.5 * colSums(t(M) * A)
  rbind(a0, A)
}

datasets <- list(
  iris        = list(X = iris[, 1:4], y = iris$Species),                       # 3 balanced classes
  mtcars      = list(X = mtcars[, c("mpg", "disp", "hp", "wt", "qsec")],      # small n, 3 classes
                     y = factor(mtcars$cyl)),
  ToothGrowth = list(X = ToothGrowth[, c("len", "dose")], y = ToothGrowth$supp) # 2 classes, p = 2
)
if (requireNamespace("MASS", quietly = TRUE)) {
  datasets <- c(datasets, list(
    Pima  = list(X = MASS::Pima.tr[, 1:7], y = MASS::Pima.tr$type),             # 2 unbalanced classes
    fgl   = list(X = MASS::fgl[, 1:9], y = MASS::fgl$type),                     # 6 classes, some tiny
    crabs = list(X = MASS::crabs[, 4:8],                                        # 4 classes
                 y = interaction(MASS::crabs$sp, MASS::crabs$sex))
  ))
}

for (name in names(datasets)) {
  X <- datasets[[name]]$X
  y <- datasets[[name]]$y
  model <- LDA$new()$fit(X, y)

  # Coefficients against the textbook computation
  ref_coef <- reference_coefficients(X, y)
  if (!isTRUE(all.equal(unname(model$coefficients), unname(ref_coef), tolerance = 1e-8))) {
    stop("Coefficients differ from the reference computation on ", name)
  }

  # Posterior probabilities and classes against MASS::lda
  if (requireNamespace("MASS", quietly = TRUE)) {
    ref <- predict(MASS::lda(X, y), X)
    if (!isTRUE(all.equal(unname(model$predict_proba(X)), unname(ref$posterior), tolerance = 1e-8))) {
      stop("Posterior probabilities differ from MASS::lda on ", name)
    }
    if (!identical(as.character(model$predict(X)), as.character(ref$class))) {
      stop("Predicted classes differ from MASS::lda on ", name)
    }
  }
}

# Out-of-sample: train on Pima.tr, predict Pima.te.
if (requireNamespace("MASS", quietly = TRUE)) {
  model <- LDA$new()$fit(MASS::Pima.tr[, 1:7], MASS::Pima.tr$type)
  ref <- predict(MASS::lda(type ~ ., data = MASS::Pima.tr), MASS::Pima.te)
  stopifnot(all.equal(unname(model$predict_proba(MASS::Pima.te)), unname(ref$posterior),
                      tolerance = 1e-8))
  stopifnot(identical(as.character(model$predict(MASS::Pima.te)), as.character(ref$class)))
}

cat("Reference tests passed on:", paste(names(datasets), collapse = ", "), "\n")
