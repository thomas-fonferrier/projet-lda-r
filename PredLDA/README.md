# PredLDA

Predictive linear discriminant analysis (LDA) in R, through an R6 class whose
interface follows the scikit-learn conventions: `fit()`, `predict()`,
`predict_proba()`, `score()`, `print()` and `summary()`.

## Installation

```r
# install.packages("remotes")
remotes::install_local("PredLDA")
```

## Usage

```r
library(PredLDA)

model <- LDA$new()
model$fit(iris[, 1:4], iris$Species)
model$coefficients            # one classification function per class

new_obs <- iris[c(1, 51, 101), 1:4]
model$predict(new_obs)
model$predict_proba(new_obs)
model$summary()
```

## Layout

| Path | Content |
|------|---------|
| `R/lda_r6.R` | `LDA` R6 class (input checks, public interface, printing) |
| `R/stats_utils.R` | Statistical computations: class means, pooled within-class covariance, (pseudo-)inverse, priors, classification functions, softmax |
| `tests/` | Unit tests and validation against `MASS::lda` |
| `inst/shiny/` | Shiny application |
| `report/` | LaTeX report |
