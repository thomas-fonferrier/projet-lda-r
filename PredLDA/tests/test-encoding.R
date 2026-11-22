# Tests of the categorical predictors, selection, evaluation, relevance and regularisation.
library(PredLDA)
 
expect_error <- function(expr) {
  ok <- tryCatch({ expr; FALSE }, error = function(e) TRUE)
  if (!ok) stop("An error was expected.")
}
no_warning <- function(expr) {
  withCallingHandlers(expr, warning = function(w) stop("Unexpected warning: ", conditionMessage(w)))
}
 
set.seed(2026)
n <- 150
y <- factor(rep(c("A", "B", "C"), each = n / 3))
d <- data.frame(
  x1 = rnorm(n, mean = as.integer(y)),
  x2 = rnorm(n),
  colour = factor(ifelse(runif(n) < 0.6, c("red", "green", "blue")[as.integer(y)],
                         sample(c("red", "green", "blue"), n, TRUE)),
                  levels = c("red", "green", "blue")),
  noise = rnorm(n)
)
X <- iris[, 1:4]
 
# Categorical predictors
m <- no_warning(LDA$new()$fit(d, y))
stopifnot(identical(m$feature_names, c("x1", "x2", "colour", "noise")))
stopifnot(identical(m$encoded_names, c("x1", "x2", "colour_green", "colour_blue", "noise")))
stopifnot(all.equal(unname(rowSums(m$predict_proba(d))), rep(1, n)))
 
mm <- model.matrix(~ x1 + x2 + colour + noise, d)[, -1]
stopifnot(all.equal(unname(m$coefficients), unname(LDA$new()$fit(mm, y)$coefficients)))
 
if (requireNamespace("MASS", quietly = TRUE)) {
  ref <- predict(MASS::lda(y ~ x1 + x2 + colour + noise, data = cbind(d, y = y)), d)
  stopifnot(all.equal(unname(m$predict_proba(d)), unname(ref$posterior), tolerance = 1e-8))
}
 
d_chr <- d[1:5, c("noise", "colour", "x2", "x1")]
d_chr$colour <- as.character(d_chr$colour)
d_chr$id <- 1:5
stopifnot(identical(m$predict(d_chr), m$predict(d[1:5, ])))
 
d_bad <- d[1:2, ]
d_bad$colour <- as.character(d_bad$colour)
d_bad$colour[1] <- "purple"
expect_error(m$predict(d_bad))
expect_error(LDA$new()$fit(data.frame(x = rnorm(n), k = "same"), y))
 
# Forward selection
s <- LDA$new(selection = "forward")$fit(X, iris$Species)
st <- s$selection_steps
stopifnot(st$variable[1] == "Petal.Length", nrow(st) == 4L, all(st$p_value < 0.05))
 
f_aov <- summary(aov(Petal.Length ~ Species, iris))[[1]][1, "F value"]
stopifnot(all.equal(st$F[1], f_aov))
 
v2 <- st$variable[2]
ancova <- anova(lm(iris[[v2]] ~ Petal.Length, iris), lm(iris[[v2]] ~ Petal.Length + Species, iris))
stopifnot(all.equal(st$F[2], ancova$F[2]))
 
s2 <- LDA$new(selection = "forward")$fit(d, y)
stopifnot(!"noise" %in% s2$feature_names, "colour" %in% s2$feature_names)
stopifnot(s2$selection_steps$n_columns[s2$selection_steps$variable == "colour"] == 2L)
stopifnot(identical(s2$predict(d), s2$predict(d[, s2$feature_names])))
 
step_c <- which(s2$selection_steps$variable == "colour")
before <- s2$selection_steps$variable[seq_len(step_c - 1L)]
Y <- model.matrix(~ colour, d)[, -1]
fml <- if (length(before)) as.formula(paste("Y ~", paste(before, collapse = "+"), "+ y")) else Y ~ y
mv <- summary(manova(fml, data = cbind(d, y = y)), test = "Wilks")$stats
stopifnot(all.equal(s2$selection_steps$F[step_c], mv["y", "approx F"]))
 
expect_error(LDA$new(selection = "forward", alpha = 1e-12)$fit(data.frame(z = rnorm(n)), y))
 
# Evaluation indicators
ev <- LDA$new()$fit(X, iris$Species)$evaluate(X, iris$Species)
stopifnot(identical(ev$global$metric,
                    c("accuracy", "error_rate", "balanced_accuracy", "macro_f1", "cohen_kappa")))
stopifnot(abs(ev$global$value[1] - 0.98) < 1e-12, abs(sum(ev$global$value[1:2]) - 1) < 1e-12)
stopifnot(sum(ev$confusion_matrix) == 150L)
 
met <- PredLDA:::compute_classification_metrics(c("A", "A", "B", "B"), c("A", "B", "B", "B"), c("A", "B"))
stopifnot(all.equal(met$global$value, c(0.75, 0.25, 0.75, (2 / 3 + 0.8) / 2, 0.5)))
expect_error(PredLDA:::compute_classification_metrics(c("A", "B"), c("A", "Z"), c("A", "B")))
 
# Variable relevance
rel <- LDA$new()$fit(X, iris$Species)$relevance()
stopifnot(nrow(rel) == 4L, all(rel$p_value < 0.05))
for (v in names(X)) {
  others <- setdiff(names(X), v)
  a <- anova(lm(reformulate(others, v), iris), lm(reformulate(c(others, "Species"), v), iris))
  stopifnot(all.equal(rel$F[rel$variable == v], a$F[2]))
  f1 <- summary(aov(reformulate("Species", v), iris))[[1]][1, "F value"]
  stopifnot(all.equal(rel$univariate_F[rel$variable == v], f1))
}
 
rel_d <- m$relevance()
stopifnot(setequal(rel_d$variable[3:4], c("x2", "noise")),
          rel_d$n_columns[rel_d$variable == "colour"] == 2L)
 
# Regularisation
set.seed(1)
yb <- factor(rep(c("u", "v"), each = 15))
Xb <- matrix(rnorm(30 * 40), 30, 40, dimnames = list(NULL, paste0("v", 1:40)))
Xb[yb == "v", 1:3] <- Xb[yb == "v", 1:3] + 2
 
w <- tryCatch(LDA$new()$fit(Xb, yb), warning = function(w) w)
stopifnot(inherits(w, "warning"))
mr <- no_warning(LDA$new(reg = 0.5)$fit(Xb, yb))
stopifnot(mr$score(Xb, yb) > 0.9)
stopifnot(all.equal(LDA$new(reg = 0)$fit(X, iris$Species)$coefficients,
                    LDA$new()$fit(X, iris$Species)$coefficients))
expect_error(LDA$new(reg = 2))
expect_error(LDA$new(selection = "backward"))
 
# Parallel computation
if (.Platform$OS.type != "windows") {
  p1 <- LDA$new(selection = "forward", n_cores = 1)$fit(d, y)
  p2 <- LDA$new(selection = "forward", n_cores = 2)$fit(d, y)
  stopifnot(identical(p1$selection_steps, p2$selection_steps),
            identical(p1$relevance(), p2$relevance()))
}
expect_error(LDA$new(n_cores = 0))
 
cat("All encoding tests passed.\n")
 
