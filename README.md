# projet-lda-r (Team G — M2 SISE)

R package **PredLDA**: predictive linear discriminant analysis with an R6,
scikit-learn-style interface, plus a Shiny demo app.

The installable package lives in the [`PredLDA/`](PredLDA/) subdirectory.

## Install from GitHub

```r
install.packages("remotes")
remotes::install_github(
  "thomas-fonferrier/projet-lda-r",
  subdir = "PredLDA",
  upgrade = "never"
)
```

## Quick start

```r
library(PredLDA)

model <- LDA$new()
model$fit(iris[, 1:4], iris$Species)
model$predict(iris[c(1, 51, 101), 1:4])
model$predict_proba(iris[c(1, 51, 101), 1:4])
model$summary()
```

## Documentation

| Resource | Location |
|----------|----------|
| Package tutorial (English, reproducible) | [PredLDA/README.md](PredLDA/README.md) |
| Function help | `?LDA` after installation |
| Vignette | `vignette("predlda-intro", package = "PredLDA")` |
| Shiny tutorial | [PredLDA/inst/shiny/README.md](PredLDA/inst/shiny/README.md) |
| French LaTeX report (sources) | [PredLDA/report/](PredLDA/report/) |

## Launch the Shiny app

```r
# After installing the package
shiny::runApp(system.file("shiny", package = "PredLDA"))

# Or from a local clone of this repository
shiny::runApp("PredLDA/inst/shiny")
```

## Team G

- Thomas Fonferrier
- Ryan Ducret
- Aya Saadi
- Xilian Jing

## Offline plan B

A source archive can be built with:

```r
devtools::build("PredLDA")  # creates PredLDA_0.1.0.tar.gz
# then: install.packages("PredLDA_0.1.0.tar.gz", repos = NULL, type = "source")
```
