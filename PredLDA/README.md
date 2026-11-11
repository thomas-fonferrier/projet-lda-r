# PredLDA

Predictive linear discriminant analysis (LDA) in R, through an R6 class whose
interface follows the scikit-learn conventions: `fit()`, `predict()`,
`predict_proba()`, `score()`, `print()` and `summary()`.

## Installation

```r
# Installer remotes si nécessaire : install.packages("remotes")
remotes::install_local("PredLDA")
```

## Usage

```r
library(PredLDA)

model <- LDA$new()
model$fit(iris[, 1:4], iris$Species)
model$coefficients            # Une fonction de classification par classe

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

## Shiny application

Install `shiny` before launching the app. Install `readxl` if you need Excel input.

```r
# Lancer depuis la racine du dépôt
shiny::runApp("PredLDA/inst/shiny")

# Ou lancer la version incluse dans le package installé
shiny::runApp(system.file("shiny", package = "PredLDA"))
```

See the [English Shiny tutorial](inst/shiny/README.md) for a reproducible example, input formats, prediction, evaluation definitions and interface limits. Example input files are provided in [inst/shiny/examples/](inst/shiny/examples/).

The app uses numeric predictors supported by the current core. Manual predictor selection is not automatic statistical feature selection. Categorical-predictor preprocessing and variable-relevance methods remain algorithm-team integration items.
