# PredLDA Shiny application

This application demonstrates the public R6 interface of the PredLDA package. It provides data import, manual predictor selection, illustrative-variable selection, model fitting, structured results, evaluation, single-observation prediction, batch prediction, and CSV exports.

## Installation and launch

Install the required packages once:

```r
# Installer les dépendances de l'application
install.packages(c("shiny", "remotes"))

# Installer le package situé dans le sous-dossier PredLDA du dépôt
remotes::install_github(
  "thomas-fonferrier/projet-lda-r",
  subdir = "PredLDA",
  upgrade = "never"
)

# Facultatif : installer le lecteur de fichiers Excel
install.packages("readxl")
```

Open `app.R` in RStudio and click **Run App**. Alternatively, from the directory containing this README:

```r
# Lancer l'application depuis son dossier
shiny::runApp(".")
```

The required dependencies are `shiny` and `PredLDA`; the latter imports `R6`. `readxl` is required only for `.xlsx` import. Data are processed locally when the app runs locally. The upload limit is 50 MiB. No additional Shiny widget package is required.

## Reproducible tutorial

1. Start the app and keep **Exemple iris** selected. The example adds two illustrative columns, `Identifiant` and `Lot`, to R's built-in `iris` data.
2. Select `Species` as the target. Select `Sepal.Length`, `Sepal.Width`, `Petal.Length`, and `Petal.Width` as predictors. Select `Identifiant` and `Lot` as illustrative variables. Illustrative variables are displayed with evaluated observations and are excluded from the model.
3. Keep **Sur les données d'apprentissage** as the evaluation mode and click **Entraîner le modèle**. The **Modèle** tab displays a model fitted to 150 observations, four predictors and three classes. It shows class counts and priors, classification coefficients, class means, and the package's detailed summary.
4. Open **Évaluation**. With the tested PredLDA 0.1.0 implementation, the training accuracy on these data is 0.98. This is a check of reproducibility, not an estimate of performance on unseen data. The confusion matrix has true classes in rows and predicted classes in columns.
5. Change the evaluation mode to **Test séparé : partage stratifié 80/20** and train again. The old model is cleared when settings change. This fits on 120 observations and evaluates on 30 observations excluded from fitting, using a fixed seed of 2026. Predictions continue to use this model fitted on 120 observations; no hidden refit on the full data occurs.
6. Open **Prédictions**, select **Un individu**, and enter 5.1, 3.5, 1.4, and 0.2 for the four predictors in that order. Click **Calculer les prédictions**. The expected predicted class is `setosa`; class probabilities are displayed alongside the inputs.
7. Switch to **Un fichier**, upload `examples/iris_nouveaux.tsv`, select **Tabulation**, keep the decimal symbol as a point, and predict. All required predictor names must be present. Column order may differ; additional columns such as identifiers are preserved. The target column is not required. No new fitting occurs during prediction.
8. Download predictions, evaluation details, or coefficients using the corresponding buttons. On-screen result tables show at most 20 individual rows; CSV exports contain every row.
9. To verify file import, select **Importer un fichier** and upload `examples/iris_apprentissage.tsv` with tab separation and a decimal point. Repeat the selections above. `examples/iris_apprentissage_fr.csv` contains the same data with a semicolon delimiter and a decimal comma; choose the matching settings.

## Input contract

Text files (`.csv`, `.tsv`, `.txt`) must include a header row and use UTF-8 encoding. Choose the separator and decimal symbol explicitly. `.xlsx` input reads the first worksheet. Each column must have a nonempty, unique name. An uploaded prediction file may contain a single predictor column if the fitted model uses one predictor.

Predictors must be numeric and finite. The current app rejects missing predictor values rather than silently imputing or dropping rows. The categorical target must be nonmissing and have at least two observed classes. Numeric class codes are converted to factors. Numeric identifier columns are offered as predictors, so users must explicitly exclude identifiers and inappropriate measurements.

The illustrative variables are optional, disjoint from the target and predictors, and are never passed to `fit()`. This interface does not implement automatic statistical feature selection.

The model interface used by the app is:

```r
# Ajuster le modèle sur les variables actives uniquement
m <- PredLDA::LDA$new()
m$fit(X, y)

# Appliquer le modèle ajusté aux nouvelles observations
classes <- m$predict(nouveau_X)
probabilites <- m$predict_proba(nouveau_X)

# Accéder aux résultats du modèle
coefficients <- m$coefficients
m$summary()
```

The UI/server names and helper functions are in `app.R`. Each session owns its fitted model and prediction results. Changes to training settings clear the old model and predictions; changes to prediction inputs clear previous predictions. The table renderer does not refit the model.

## Evaluation boundaries

Accuracy is the proportion of correct classifications. Class precision is TP/(TP+FP); recall is TP/(TP+FN); F1 is 2TP/(2TP+FP+FN). Balanced accuracy is the mean class recall; macro F1 is the unweighted mean class F1. Undefined values appear as NA.

Training evaluation uses the same data for fitting and evaluation. The optional stratified split samples approximately 80% of each class for training; at least one observation per class is reserved for testing. Each class must have at least two observations, and the resulting training data must satisfy the underlying model's constraints. A single split is not cross-validation. This split assumes independent observations. Repeated measurements from one participant, clustered data and time series need a group-aware or time-aware split implemented with the team before scientific use.

Predicted probabilities are model estimates, not guarantees. Coefficient magnitude is affected by units and dependence between predictors; the app does not label it as variable importance. It does not provide variable significance tests.

## Integration in the shared repository

Copy this folder's `app.R`, `examples/`, `README.md`, and optionally `tests/` into `PredLDA/inst/shiny/`. The existing empty `global.R`, `ui.R`, and `server.R` placeholders are unnecessary when using this self-contained `app.R`; keep only one maintained application implementation.

From the repository root:

```r
# Lancer la version du dépôt
shiny::runApp("PredLDA/inst/shiny")
```

After installing the package containing this application:

```r
# Lancer la version installée avec le package
shiny::runApp(system.file("shiny", package = "PredLDA"))
```

The package integration maintainer should declare `shiny` and optionally `readxl` in `Suggests` in `DESCRIPTION`, document the launch command, include the app tutorial in the main project README, and check installation from GitHub. Files under `inst/` are included by package installation unless excluded by package configuration.

## Responsibilities still shared with the algorithm team

The inspected numerical core does not implement categorical-predictor preprocessing, an automatic feature-selection strategy, or variable-relevance statistics. The current UI makes these limits explicit and does not present manual selection as an automatic strategy. Those project requirements must be implemented or agreed upon with the algorithm team, then connected to this app. An updated preprocessing pipeline must be learned only on training data and reused unchanged at prediction time; dummy-variable columns and levels must remain compatible.

The French project report, bibliography, complete package documentation and GitHub submission remain group deliverables. This application is the member-3 deliverable for the currently available numeric model interface, not a claim that the whole group project is complete.

## Verification

With `shiny` and `PredLDA` installed, from this folder:

```r
# Exécuter les vérifications de la logique serveur
source("tests/test-server.R")
```

The included tests check model fitting, illustrative-variable exclusion, predictions, probability sums, complete exports, stratified splitting without leakage, stale-result invalidation, missing-column errors, missing-data errors, and recovery after errors. Tested with R 4.6.1, Shiny 1.14.0 and PredLDA 0.1.0. These are server-logic tests; a browser acceptance check on the user's machine is still required.

Official API references: [fileInput](https://shiny.posit.co/r/reference/shiny/latest/fileinput.html), [downloadHandler](https://shiny.posit.co/r/reference/shiny/latest/downloadhandler.html), [testServer](https://shiny.posit.co/r/reference/shiny/latest/testserver.html).
