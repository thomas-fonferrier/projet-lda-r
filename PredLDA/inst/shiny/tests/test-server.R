# Exécuter depuis le dossier de l'application : Rscript tests/test-server.R.
chemin <- if (file.exists("app.R")) "app.R" else "outputs/PredLDA-shiny/app.R"
environnement <- new.env()
sys.source(chemin, envir = environnement)

creer_import <- function(d, extension = ".tsv", separateur = "\t", decimale = ".") {
  chemin <- tempfile(fileext = extension)
  write.table(d, chemin, sep = separateur, dec = decimale, row.names = FALSE, quote = TRUE)
  data.frame(name = basename(chemin), size = file.info(chemin)$size,
             type = "text/plain", datapath = chemin)
}

# Vérifier la séparation des échantillons et la conservation de l'état aléatoire.
set.seed(12)
ancien_etat <- .Random.seed
indices <- environnement$partager_indices(iris$Species)
stopifnot(length(indices$apprentissage) == 120L, length(indices$evaluation) == 30L,
          !length(intersect(indices$apprentissage, indices$evaluation)),
          identical(ancien_etat, .Random.seed))

# Vérifier les métriques contre un exemple calculé directement.
mesures <- environnement$evaluer_classes(c("A", "A", "B", "B"), c("A", "B", "B", "B"), c("A", "B"))
stopifnot(mesures$global$Valeur[1] == 0.75, mesures$global$Valeur[2] == 0.75,
          isTRUE(all.equal(mesures$classes$F1, c(2 / 3, 0.8))))

shiny::testServer(environnement$server, {
  session$setInputs(source = "iris", separateur = "\t", decimale = ".",
    evaluation = "apprentissage", entrainer = 0, predire = 0,
    mode_prediction = "individu", nouveau_separateur = "\t", nouvelle_decimale = ".")
  session$setInputs(cible = "Species", explicatives = names(iris)[1:4],
                   illustratives = c("Identifiant", "Lot"))
  session$setInputs(entrainer = 1)
  stopifnot(is.null(erreur()), resultat()$modele$n_obs == 150L,
    identical(resultat()$modele$feature_names, names(iris)[1:4]),
    nrow(resultat()$details) == 150L,
    all(c("Identifiant", "Lot") %in% names(resultat()$details)),
    grepl("Observations: 150", output$resume, fixed = TRUE),
    abs(resultat()$mesures$global$Valeur[1] - 0.98) < 1e-12)

  # Vérifier la saisie individuelle et les probabilités normalisées.
  session$setInputs(individu_1 = 5.1, individu_2 = 3.5, individu_3 = 1.4, individu_4 = 0.2)
  session$setInputs(predire = 1)
  stopifnot(is.null(erreur_prediction()), predictions()$Classe_predite == "setosa")
  colonnes_prob <- grep("^P\\(", names(predictions()))
  stopifnot(abs(sum(predictions()[1, colonnes_prob]) - 1) < 1e-12)

  # Vérifier le fichier de prédiction, les colonnes réordonnées et les identifiants conservés.
  nouveaux <- environnement$exemple_iris()[c(1, 51, 101), c(4, 2, 1, 3, 6, 7)]
  session$setInputs(mode_prediction = "fichier", nouveau_fichier = creer_import(nouveaux))
  session$setInputs(predire = 2)
  stopifnot(is.null(erreur_prediction()),
    identical(predictions()$Classe_predite, c("setosa", "versicolor", "virginica")),
    identical(predictions()$Identifiant, nouveaux$Identifiant))
  export <- output$export_predictions
  stopifnot(file.exists(export), nrow(read.csv(export)) == 3L)
  export_eval <- output$export_evaluation
  stopifnot(nrow(read.csv(export_eval)) == 150L)

  # Vérifier qu'un fichier incomplet est rejeté sans effacer le modèle.
  session$setInputs(nouveau_fichier = creer_import(nouveaux[, -1]))
  stopifnot(is.null(predictions()))
  session$setInputs(predire = 3)
  stopifnot(!is.null(erreur_prediction()), grepl("absentes", erreur_prediction()), !is.null(resultat()))

  # Vérifier la partition indépendante et l'absence des variables illustratives dans le modèle.
  session$setInputs(evaluation = "test")
  stopifnot(is.null(resultat()), is.null(predictions()))
  session$setInputs(entrainer = 2)
  stopifnot(is.null(erreur()), resultat()$modele$n_obs == 120L,
    nrow(resultat()$details) == 30L,
    !length(intersect(resultat()$indices$apprentissage, resultat()$indices$evaluation)),
    all(abs(rowSums(resultat()$modele$predict_proba(iris[, 1:4])) - 1) < 1e-12))

  # Vérifier la sélection vide, puis un nouvel ajustement avec moins de variables.
  session$setInputs(explicatives = character(0), entrainer = 3)
  stopifnot(is.null(resultat()), grepl("au moins une", erreur()))
  session$setInputs(explicatives = names(iris)[1:3], entrainer = 4)
  stopifnot(is.null(erreur()), length(resultat()$modele$feature_names) == 3L)

  # Vérifier les données manquantes et la possibilité de récupérer après une erreur.
  d <- environnement$exemple_iris()
  d[1, 1] <- NA
  session$setInputs(source = "fichier", fichier = creer_import(d), evaluation = "apprentissage")
  session$setInputs(cible = "Species", explicatives = names(iris)[1:4], illustratives = "Lot", entrainer = 5)
  stopifnot(is.null(resultat()), grepl("manquantes", erreur()))
  session$setInputs(fichier = creer_import(environnement$exemple_iris()))
  session$setInputs(cible = "Species", explicatives = names(iris)[1:4], illustratives = "Lot", entrainer = 6)
  stopifnot(is.null(erreur()), resultat()$modele$n_obs == 150L)

  # Vérifier que le chevauchement entre variables actives et illustratives est rejeté.
  session$setInputs(illustratives = "Sepal.Length", entrainer = 7)
  stopifnot(is.null(resultat()), grepl("distinctes", erreur()))
})

# Vérifier les formats texte et le symbole décimal français.
fichier_fr <- creer_import(iris, extension = ".csv", separateur = ";", decimale = ",")
lecture <- environnement$lire_fichier(fichier_fr, ";", ",", minimum = 2)
stopifnot(isTRUE(all.equal(lecture[, 1:4], iris[, 1:4])))
cat("Tous les tests de l'application ont réussi.\n")
