# Tests of the LDA R6 class (base R only, run by R CMD check).
library(PredLDA)

expect_error <- function(expr) {
  ok <- tryCatch({ expr; FALSE }, error = function(e) TRUE)
  if (!ok) stop("An error was expected.")
}

X <- iris[, 1:4]
y <- iris$Species

# --- fit / predict on 3 classes --------------------------------------------
m <- LDA$new()
expect_chain <- m$fit(X, y)
stopifnot(identical(expect_chain, m), m$is_fitted)
stopifnot(identical(dim(m$coefficients), c(5L, 3L)))
stopifnot(all.equal(sum(m$priors), 1))

P <- m$predict_proba(X)
stopifnot(all.equal(unname(rowSums(P)), rep(1, nrow(X))))
pred <- m$predict(X)
stopifnot(is.factor(pred), identical(levels(pred), levels(y)))
stopifnot(m$score(X, y) > 0.97)

# --- agreement with MASS::lda ----------------------------------------------
if (requireNamespace("MASS", quietly = TRUE)) {
  ref <- MASS::lda(X, y)
  ref_pred <- predict(ref, X)
  stopifnot(all.equal(unname(P), unname(ref_pred$posterior), tolerance = 1e-8))
  stopifnot(identical(as.character(pred), as.character(ref_pred$class)))

  pri <- c(setosa = 0.5, versicolor = 0.3, virginica = 0.2)
  m2 <- LDA$new(priors = pri)$fit(X, y)
  ref2 <- predict(MASS::lda(X, y, prior = unname(pri)), X)
  stopifnot(all.equal(unname(m2$predict_proba(X)), unname(ref2$posterior), tolerance = 1e-8))
}

# --- binary target and reordered / extra columns at prediction --------------
d <- iris[iris$Species != "setosa", ]
mb <- LDA$new()$fit(d[, 1:4], d$Species)    # unused level "setosa" is dropped
stopifnot(identical(mb$classes, c("versicolor", "virginica")))
stopifnot(identical(mb$predict(d[, 4:1]), mb$predict(d[, 1:4])))
stopifnot(identical(mb$predict(d), mb$predict(d[, 1:4])))   # extra column ignored
stopifnot(length(mb$predict(unlist(d[1, 1:4]))) == 1L)     # single named vector

# --- singular W: duplicated column -> warning + pseudo-inverse --------------
Xs <- cbind(X, dup = X$Sepal.Length)
ms <- LDA$new()
w <- tryCatch(ms$fit(Xs, y), warning = function(w) w)
stopifnot(inherits(w, "warning"))
suppressWarnings(ms$fit(Xs, y))
stopifnot(ms$score(Xs, y) > 0.97)

# --- input validation -------------------------------------------------------
expect_error(LDA$new(priors = c(-1, 2)))
expect_error(LDA$new()$predict(X))                       # not fitted
expect_error(LDA$new()$fit(X, rep("a", 150)))           # one class
expect_error(LDA$new()$fit(X, y[-1]))                   # length mismatch
expect_error(LDA$new()$fit(iris, y))                    # factor column in X
expect_error(LDA$new(priors = c(0.5, 0.5))$fit(X, y))   # wrong number of priors
expect_error(m$predict(X[, 1:3]))                       # missing variable
Xna <- X; Xna[1, 1] <- NA
expect_error(LDA$new()$fit(Xna, y))

cat("All LDA tests passed.\n")
