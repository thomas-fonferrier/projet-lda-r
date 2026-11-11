# Application de démonstration du package PredLDA.
# Dépendances obligatoires : shiny, PredLDA. Lecture Excel facultative : readxl.
library(shiny)
library(PredLDA)
options(shiny.maxRequestSize = 50 * 1024^2)

# Fournir un exemple comprenant des variables illustratives non utilisées pour l'ajustement.
exemple_iris <- function() {
  d <- datasets::iris
  d$Identifiant <- sprintf("F%03d", seq_len(nrow(d)))
  d$Lot <- rep(c("Lot A", "Lot B"), length.out = nrow(d))
  d
}

# Lire le fichier temporaire fourni par Shiny en conservant les noms des colonnes.
lire_fichier <- function(fichier, separateur = "\t", decimale = ".", minimum = 1L) {
  extension <- tolower(tools::file_ext(fichier$name))
  if (extension == "xlsx") {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("La lecture des fichiers xlsx nécessite le package readxl.", call. = FALSE)
    }
    d <- as.data.frame(readxl::read_excel(fichier$datapath, .name_repair = "minimal"))
  } else if (extension %in% c("csv", "tsv", "txt")) {
    d <- read.table(
      fichier$datapath, header = TRUE, sep = separateur, dec = decimale,
      stringsAsFactors = FALSE, check.names = FALSE, comment.char = "",
      na.strings = c("", "NA", "N/A"), fileEncoding = "UTF-8-BOM"
    )
  } else {
    stop("Formats acceptés : csv, tsv, txt et xlsx.", call. = FALSE)
  }
  if (!nrow(d)) stop("Le fichier ne contient aucune observation.", call. = FALSE)
  if (ncol(d) < minimum) stop("Nombre de colonnes insuffisant. Vérifiez le séparateur.", call. = FALSE)
  if (anyDuplicated(names(d)) || any(!nzchar(trimws(names(d))))) {
    stop("Les noms des colonnes doivent être non vides et uniques.", call. = FALSE)
  }
  d
}

# Contrôler les entrées compatibles avec la version actuelle du modèle.
verifier_X <- function(X) {
  if (!nrow(X) || !ncol(X)) stop("Aucune observation ou variable explicative.", call. = FALSE)
  non_numeriques <- names(X)[!vapply(X, is.numeric, logical(1))]
  if (length(non_numeriques)) {
    stop("Variables non numériques : ", paste(non_numeriques, collapse = ", "), call. = FALSE)
  }
  if (anyNA(X) || any(!is.finite(as.matrix(X)))) {
    stop("Les variables explicatives contiennent des valeurs manquantes ou infinies.", call. = FALSE)
  }
  invisible(TRUE)
}

# Effectuer un partage stratifié reproductible sans modifier l'état aléatoire des autres sessions.
partager_indices <- function(y, graine = 2026L) {
  ancien_etat <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else NULL
  on.exit({
    if (is.null(ancien_etat)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", ancien_etat, envir = .GlobalEnv)
  })
  if (any(table(y) < 2L)) {
    stop("Le partage exige au moins deux observations dans chaque classe.", call. = FALSE)
  }
  set.seed(graine)
  apprentissage <- sort(unlist(lapply(split(seq_along(y), y), function(i) {
    nombre <- max(1L, min(length(i) - 1L, floor(0.8 * length(i))))
    i[sample.int(length(i), nombre)]
  }), use.names = FALSE))
  list(apprentissage = apprentissage, evaluation = setdiff(seq_along(y), apprentissage))
}

# Calculer des mesures descriptives de classification, sans tests de significativité.
evaluer_classes <- function(observe, predit, classes) {
  confusion <- table(
    Observee = factor(observe, levels = classes),
    Predite = factor(predit, levels = classes)
  )
  tp <- diag(confusion)
  effectifs <- rowSums(confusion)
  effectifs_predits <- colSums(confusion)
  precision <- ifelse(effectifs_predits > 0, tp / effectifs_predits, NA_real_)
  rappel <- ifelse(effectifs > 0, tp / effectifs, NA_real_)
  denominateur <- effectifs + effectifs_predits
  f1 <- ifelse(denominateur > 0, 2 * tp / denominateur, NA_real_)
  list(
    confusion = confusion,
    global = data.frame(
      Indicateur = c("Exactitude", "Exactitude équilibrée", "F1 macro"),
      Valeur = c(sum(tp) / sum(confusion), mean(rappel, na.rm = TRUE), mean(f1, na.rm = TRUE))
    ),
    classes = data.frame(
      Classe = classes, Effectif = as.integer(effectifs),
      Precision = as.numeric(precision), Rappel = as.numeric(rappel), F1 = as.numeric(f1)
    )
  )
}

# Appliquer le modèle déjà ajusté ; ne jamais réajuster sur les nouvelles observations.
predire_table <- function(modele, d) {
  manquantes <- setdiff(modele$feature_names, names(d))
  if (length(manquantes)) {
    stop("Variables requises absentes : ", paste(manquantes, collapse = ", "), call. = FALSE)
  }
  X <- d[, modele$feature_names, drop = FALSE]
  verifier_X(X)
  probabilites <- modele$predict_proba(X)
  resultat <- data.frame(
    Observation = seq_len(nrow(d)),
    Classe_predite = as.character(modele$predict(X)),
    Probabilite_max = apply(probabilites, 1L, max),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  probabilites <- as.data.frame(probabilites, check.names = FALSE)
  names(probabilites) <- paste0("P(", names(probabilites), ")")
  resultat <- cbind(resultat, probabilites, d)
  names(resultat) <- make.unique(names(resultat))
  resultat
}

# Définir une zone de tableau défilante pour les fichiers comportant de nombreuses colonnes.
table_defilante <- function(id) div(style = "overflow-x:auto;", tableOutput(id))
separateurs <- c("Tabulation" = "\t", "Point-virgule" = ";", "Virgule" = ",")

ui <- fluidPage(
  tags$head(tags$style(HTML(
    ".container-fluid {max-width:1500px;} .shiny-output-error-validation {color:#9a3412;} .well {background:#f7f9fc;} .nav-tabs {margin-bottom:18px;}"
  ))),
  titlePanel("PredLDA — Analyse discriminante linéaire"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 radioButtons("source", "Données", c("Exemple iris" = "iris", "Importer un fichier" = "fichier"), selected = "iris"),
                 conditionalPanel("input.source === 'fichier'",
                                  fileInput("fichier", "Fichier d'apprentissage", accept = c(".csv", ".tsv", ".txt", ".xlsx"), buttonLabel = "Parcourir…", placeholder = "Aucun fichier"),
                                  selectInput("separateur", "Séparateur", separateurs, selected = "\t"),
                                  selectInput("decimale", "Symbole décimal", c("Point" = ".", "Virgule" = ","), selected = ".")
                 ),
                 selectInput("cible", "Variable cible", choices = character(0)),
                 selectInput("explicatives", "Variables explicatives numériques", choices = character(0), multiple = TRUE),
                 selectInput("illustratives", "Variables illustratives", choices = character(0), multiple = TRUE),
                 helpText("Les variables illustratives décrivent les observations et ne participent pas à l'ajustement."),
                 selectInput("evaluation", "Évaluation", c("Sur les données d'apprentissage" = "apprentissage", "Test séparé : partage stratifié 80/20" = "test"), selected = "apprentissage"),
                 helpText("Le partage 80/20 suppose des observations indépendantes. Il ne convient pas aux mesures répétées ou aux séries temporelles sans adaptation."),
                 actionButton("entrainer", "Entraîner le modèle", class = "btn-primary"),
                 hr(), textOutput("statut")
    ),
    mainPanel(width = 9,
              tabsetPanel(id = "onglet",
                          tabPanel("Données", value = "donnees",
                                   h3("Aperçu des données"), textOutput("dimensions"), table_defilante("apercu"),
                                   h3("Variables illustratives"), table_defilante("illustration")
                          ),
                          tabPanel("Modèle", value = "modele",
                                   h3("Ajustement"), textOutput("contexte"), textOutput("avertissements"),
                                   h4("Classes du modèle"), table_defilante("distribution"),
                                   h4("Coefficients des fonctions de classification"),
                                   p("Une colonne par classe, avec l'interception en première ligne. Ces coefficients ne constituent pas un classement d'importance des variables."),
                                   table_defilante("coefficients"), downloadButton("export_coefficients", "Télécharger les coefficients"),
                                   h4("Moyennes par classe"), table_defilante("moyennes"),
                                   h4("Résumé détaillé du package"), verbatimTextOutput("resume")
                          ),
                          tabPanel("Évaluation", value = "evaluation",
                                   h3("Résultats de classification"), textOutput("contexte_evaluation"),
                                   table_defilante("indicateurs"), h4("Résultats par classe"), table_defilante("par_classe"),
                                   p("Les mesures vont de 0 à 1. NA signifie qu'une mesure n'est pas définie. Les définitions figurent dans l'onglet Aide."),
                                   h4("Matrice de confusion : lignes observées, colonnes prédites"), table_defilante("confusion"),
                                   h4("Observations évaluées, avec leurs variables illustratives"), table_defilante("details_evaluation"),
                                   downloadButton("export_evaluation", "Télécharger toutes les observations évaluées")
                          ),
                          tabPanel("Prédictions", value = "predictions",
                                   h3("Prédire de nouvelles observations"),
                                   p("Entraînez d'abord le modèle. La variable cible n'est pas nécessaire pour la prédiction."),
                                   radioButtons("mode_prediction", "Mode de saisie", c("Un individu" = "individu", "Un fichier" = "fichier"), inline = TRUE),
                                   conditionalPanel("input.mode_prediction === 'individu'", uiOutput("champs_individu")),
                                   conditionalPanel("input.mode_prediction === 'fichier'",
                                                    fileInput("nouveau_fichier", "Nouvelles observations", accept = c(".csv", ".tsv", ".txt", ".xlsx"), buttonLabel = "Parcourir…", placeholder = "Aucun fichier"),
                                                    selectInput("nouveau_separateur", "Séparateur du nouveau fichier", separateurs, selected = "\t"),
                                                    selectInput("nouvelle_decimale", "Symbole décimal du nouveau fichier", c("Point" = ".", "Virgule" = ","), selected = ".")
                                   ),
                                   actionButton("predire", "Calculer les prédictions", class = "btn-primary"),
                                   p(), textOutput("erreur_prediction"), table_defilante("predictions"),
                                   downloadButton("export_predictions", "Télécharger toutes les prédictions"),
                                   p("L'écran présente les 20 premières lignes ; le téléchargement contient toutes les lignes.")
                          ),
                          tabPanel("Aide", value = "aide",
                                   h3("Utilisation"),
                                   p("Choisissez l'exemple iris ou importez un fichier avec une ligne d'en-tête. Vérifiez le séparateur, le symbole décimal et l'aperçu. Pour xlsx, la première feuille est lue ; le package readxl est nécessaire."),
                                   p("Sélectionnez une cible catégorielle comportant au moins deux classes. Les variables explicatives doivent être numériques, sans valeurs manquantes ou infinies. Les variables illustratives peuvent être textuelles ou numériques et restent exclues du modèle."),
                                   p("Cliquez sur Entraîner le modèle. Les changements de données, de variables ou de mode d'évaluation effacent les résultats pour éviter de présenter un ancien modèle."),
                                   p("Dans Prédictions, saisissez un individu ou importez un fichier possédant les mêmes noms de variables explicatives. L'ordre des colonnes peut différer. Les colonnes supplémentaires sont conservées dans le téléchargement."),
                                   h3("Évaluation et interprétation"),
                                   p("Exactitude : proportion de classifications correctes. Précision d'une classe : proportion d'observations correctes parmi celles prédites dans cette classe. Rappel : proportion d'observations retrouvées parmi celles appartenant réellement à cette classe. F1 : 2TP / (2TP + FP + FN). L'exactitude équilibrée est la moyenne des rappels ; le F1 macro est la moyenne des F1 des classes."),
                                   p("L'évaluation sur l'apprentissage décrit les données utilisées pour ajuster le modèle et surestime souvent les performances sur de nouvelles données. Le partage stratifié utilise environ 80 % de chaque classe pour apprendre et le reste pour évaluer, avec une graine fixe de 2026. Il n'est adapté qu'aux observations indépendantes. Le modèle de prédiction reste celui ajusté sur la partie apprentissage."),
                                   p("Les probabilités sont celles du modèle et ne garantissent pas qu'une prédiction est correcte. Les coefficients dépendent notamment des unités et des corrélations ; leur amplitude seule n'est pas une mesure d'importance."),
                                   h3("Périmètre"),
                                   p("Cette application utilise les méthodes publiques de PredLDA : fit, predict, predict_proba et summary. La sélection manuelle des variables dans l'interface ne constitue pas une procédure automatique de sélection. Le traitement des variables explicatives catégorielles et les indicateurs de pertinence restent à intégrer avec les fonctions correspondantes du package.")
                          )
              )
    )
  )
)

server <- function(input, output, session) {
  # Isoler l'état de chaque utilisateur.
  resultat <- reactiveVal(NULL)
  erreur <- reactiveVal(NULL)
  predictions <- reactiveVal(NULL)
  erreur_prediction <- reactiveVal(NULL)
  
  donnees <- reactive({
    req(input$source)
    if (input$source == "iris") return(exemple_iris())
    req(input$fichier, input$separateur, input$decimale)
    tryCatch(
      lire_fichier(input$fichier, input$separateur, input$decimale, minimum = 2L),
      error = function(e) validate(need(FALSE, conditionMessage(e)))
    )
  })
  
  observeEvent(donnees(), {
    noms <- names(donnees())
    cible <- if ("Species" %in% noms) "Species" else tail(noms, 1L)
    updateSelectInput(session, "cible", choices = noms, selected = cible)
  })
  
  observeEvent(list(donnees(), input$cible), {
    req(input$cible)
    d <- donnees()
    choix <- setdiff(names(d)[vapply(d, is.numeric, logical(1))], input$cible)
    updateSelectInput(session, "explicatives", choices = choix, selected = choix)
  })
  
  observeEvent(list(donnees(), input$cible, input$explicatives), {
    choix <- setdiff(names(donnees()), c(input$cible, input$explicatives))
    updateSelectInput(session, "illustratives", choices = choix, selected = intersect(input$illustratives, choix))
  })
  
  # Invalider les résultats avant tout nouvel ajustement si les réglages changent.
  observe({
    input$source; input$fichier; input$separateur; input$decimale
    input$cible; input$explicatives; input$illustratives; input$evaluation
    resultat(NULL); erreur(NULL); predictions(NULL); erreur_prediction(NULL)
  }, priority = 20)
  
  observeEvent(input$entrainer, {
    resultat(NULL); erreur(NULL); predictions(NULL); erreur_prediction(NULL)
    req(donnees())
    tryCatch({
      d <- donnees()
      cible <- input$cible
      actives <- input$explicatives
      illustratives <- input$illustratives
      if (length(cible) != 1L || !cible %in% names(d)) stop("Sélectionnez une variable cible.")
      if (!length(actives)) stop("Sélectionnez au moins une variable explicative.")
      if (!all(c(actives, illustratives) %in% names(d))) stop("Vérifiez les noms des variables.")
      if (cible %in% c(actives, illustratives) || length(intersect(actives, illustratives))) {
        stop("La cible, les variables explicatives et les variables illustratives doivent être distinctes.")
      }
      X <- d[, actives, drop = FALSE]
      verifier_X(X)
      if (anyNA(d[[cible]]) || any(!nzchar(trimws(as.character(d[[cible]]))))) {
        stop("La variable cible contient des valeurs manquantes ou vides.")
      }
      y <- droplevels(factor(d[[cible]]))
      if (nlevels(y) < 2L) stop("La variable cible doit comporter au moins deux classes.")
      indices <- if (identical(input$evaluation, "test")) partager_indices(y) else {
        list(apprentissage = seq_len(nrow(d)), evaluation = seq_len(nrow(d)))
      }
      avertissements <- character(0)
      m <- LDA$new()
      withCallingHandlers(
        m$fit(X[indices$apprentissage, , drop = FALSE], y[indices$apprentissage]),
        warning = function(w) {
          avertissements <<- c(avertissements, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      ie <- indices$evaluation
      sortie <- predire_table(m, d[ie, c(actives, illustratives), drop = FALSE])
      sortie$Observation <- ie
      sortie$Classe_observee <- as.character(y[ie])
      mesures <- evaluer_classes(y[ie], sortie$Classe_predite, m$classes)
      resultat(list(
        modele = m, cible = cible, actives = actives, illustratives = illustratives,
        indices = indices, evaluation = input$evaluation, mesures = mesures,
        details = sortie, avertissements = unique(avertissements),
        valeurs_defaut = vapply(X[indices$apprentissage, , drop = FALSE], median, numeric(1))
      ))
      updateTabsetPanel(session, "onglet", selected = "modele")
    }, error = function(e) erreur(paste("Échec de l'entraînement :", conditionMessage(e))))
  })
  
  # Propager les erreurs d'ajustement aux zones de résultats sans interrompre la session.
  modele_courant <- reactive({
    validate(need(is.null(erreur()), erreur()))
    req(resultat())
    resultat()
  })
  
  output$apercu <- renderTable(head(donnees(), 10), digits = 3)
  output$dimensions <- renderText(sprintf("%d observations ; %d colonnes. Aperçu des 10 premières lignes.", nrow(donnees()), ncol(donnees())))
  output$illustration <- renderTable({
    req(length(input$illustratives) > 0)
    head(donnees()[, c(input$cible, input$illustratives), drop = FALSE], 20)
  })
  output$statut <- renderText({
    if (!is.null(erreur())) return(erreur())
    if (!is.null(resultat())) return("Modèle entraîné. Consultez Modèle, Évaluation ou Prédictions.")
    "Choisissez les données et les variables, puis cliquez sur Entraîner le modèle."
  })
  output$contexte <- renderText({
    r <- modele_courant()
    sprintf("Cible : %s. Apprentissage : %d observations, %d variables, %d classes. Variables illustratives : %s.",
            r$cible, r$modele$n_obs, length(r$actives), length(r$modele$classes),
            if (length(r$illustratives)) paste(r$illustratives, collapse = ", ") else "aucune")
  })
  output$avertissements <- renderText(paste(modele_courant()$avertissements, collapse = " "))
  output$distribution <- renderTable({
    m <- modele_courant()$modele
    data.frame(Classe = m$classes, Effectif = as.integer(m$class_counts), Prior = as.numeric(m$priors))
  }, digits = 4)
  tableau_coefficients <- reactive({
    b <- modele_courant()$modele$coefficients
    data.frame(Variable = rownames(b), as.data.frame(b, check.names = FALSE), row.names = NULL, check.names = FALSE)
  })
  output$coefficients <- renderTable(tableau_coefficients(), digits = 4)
  output$moyennes <- renderTable({
    m <- modele_courant()$modele$means
    data.frame(Classe = rownames(m), as.data.frame(m, check.names = FALSE), row.names = NULL, check.names = FALSE)
  }, digits = 4)
  output$resume <- renderPrint(modele_courant()$modele$summary())
  output$contexte_evaluation <- renderText({
    r <- modele_courant()
    if (identical(r$evaluation, "test")) {
      sprintf("Évaluation sur %d observations de test exclues de l'ajustement ; apprentissage sur %d observations. Un seul partage stratifié, graine 2026.", length(r$indices$evaluation), length(r$indices$apprentissage))
    } else "Évaluation sur les données d'apprentissage : ces résultats ne mesurent pas les performances sur de nouvelles données."
  })
  output$indicateurs <- renderTable(modele_courant()$mesures$global, digits = 4)
  output$par_classe <- renderTable(modele_courant()$mesures$classes, digits = 4, na = "NA")
  output$confusion <- renderTable({
    matrice <- modele_courant()$mesures$confusion
    data.frame(Classe_observee = rownames(matrice), as.data.frame.matrix(matrice), row.names = NULL, check.names = FALSE)
  })
  output$details_evaluation <- renderTable(head(modele_courant()$details, 20), digits = 4)
  
  # Construire les champs d'un individu à partir des variables du modèle ajusté.
  output$champs_individu <- renderUI({
    r <- modele_courant()
    tagList(lapply(seq_along(r$actives), function(i) {
      numericInput(paste0("individu_", i), r$actives[i], value = unname(r$valeurs_defaut[i]))
    }))
  })
  
  # Effacer les anciennes prédictions lorsqu'une entrée de prédiction change.
  observe({
    input$mode_prediction; input$nouveau_fichier; input$nouveau_separateur; input$nouvelle_decimale
    r <- resultat()
    if (!is.null(r)) lapply(seq_along(r$actives), function(i) input[[paste0("individu_", i)]])
    predictions(NULL); erreur_prediction(NULL)
  }, priority = 10)
  
  observeEvent(input$predire, {
    predictions(NULL); erreur_prediction(NULL)
    if (is.null(resultat())) {
      erreur_prediction("Entraînez d'abord le modèle.")
      return()
    }
    tryCatch({
      r <- resultat()
      if (identical(input$mode_prediction, "fichier")) {
        if (is.null(input$nouveau_fichier)) stop("Importez un fichier contenant les nouvelles observations.")
        d <- lire_fichier(input$nouveau_fichier, input$nouveau_separateur, input$nouvelle_decimale)
      } else {
        valeurs <- lapply(seq_along(r$actives), function(i) input[[paste0("individu_", i)]])
        if (any(lengths(valeurs) != 1L)) stop("Renseignez chaque variable explicative.")
        d <- as.data.frame(setNames(valeurs, r$actives), check.names = FALSE)
      }
      predictions(predire_table(r$modele, d))
    }, error = function(e) erreur_prediction(paste("Prédiction impossible :", conditionMessage(e))))
  })
  output$erreur_prediction <- renderText(erreur_prediction())
  output$predictions <- renderTable({ req(predictions()); head(predictions(), 20) }, digits = 4)
  
  # Exporter toutes les lignes, même lorsque l'aperçu affiché est limité.
  output$export_predictions <- downloadHandler(
    filename = function() "predictions_PredLDA.csv",
    content = function(file) { req(predictions()); write.csv(predictions(), file, row.names = FALSE, fileEncoding = "UTF-8") }
  )
  output$export_evaluation <- downloadHandler(
    filename = function() "evaluation_PredLDA.csv",
    content = function(file) write.csv(modele_courant()$details, file, row.names = FALSE, fileEncoding = "UTF-8")
  )
  output$export_coefficients <- downloadHandler(
    filename = function() "coefficients_PredLDA.csv",
    content = function(file) write.csv(tableau_coefficients(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
}

shinyApp(ui = ui, server = server)
