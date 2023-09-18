help_scripts <- function(output, r = shiny::reactiveValues(), id = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r$help_scripts_open_panel,
      br(),
      strong(i18n$t("scripts")), br(), br(),
      shiny.fluent::Link(i18n$t("whats_a_script"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("local_and_remote_scripts"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("choose_dataset_scripts"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_3', Math.random()); }"))), br(), br(),
      strong(i18n$t("create_and_test_a_script")), br(), br(),
      shiny.fluent::Link(i18n$t("scripts_management"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_4', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("edit_script_code"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_5', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("script_options"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_6', Math.random()); }"))), br(), br(),
      strong(i18n$t("import_and_export_scripts")), br(), br(),
      shiny.fluent::Link(i18n$t("import_and_export_scripts"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_7', Math.random()); }"))), br(), br(),
      isLightDismiss = r$help_scripts_open_panel_light_dismiss,
      isBlocking = r$help_scripts_open_panel_light_dismiss,
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({
    
    shiny.fluent::Modal(
      isOpen = r$help_scripts_open_modal, dragOptions = TRUE, isModeless = FALSE, topOffsetFixed = TRUE,
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
      div(
        style = "width: 1000px; padding: 15px 10px 0px 15px;",
        shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
          div(style = list(display = "flex"),
            shiny.fluent::Text(r$help_scripts_modal_title, variant = "large"),
            div(style = list(flexGrow = 1)),
            shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
          ),
          r$help_scripts_modal_text
        )
      )
    )
  })
  
  load_help_page <- function(r){
    r$help_scripts_open_modal <- TRUE
    r$help_scripts_open_panel_light_dismiss <- FALSE
  }
  
  # Code divs
  
  code_1 <- paste0(
    "d$measurement <-\n",
    "  d$measurement %>%\n",
    "  dplyr::mutate(value_as_number = dplyr::case_when(\n",
    "    measurement_concept_id == 3036277 & value_as_number > 280 ~ NA_real_,\n",
    "    TRUE ~ value_as_number\n",
    "  )\n",
    ")"
  )
  div_code_1 <- div(
    span("d$measurement <-"), br(),
    span("d$measurement %>%", style = "margin-left:20px;"), br(),
    span("dplyr::mutate(value_as_number = dplyr::case_when(", style = "margin-left:20px;"), br(),
    span("measurement_concept_id == 3453 & value_as_number > 280 ~ NA_real_,", style = "margin-left:40px;"), br(),
    span("TRUE ~ value_as_number", style = "margin-left:40px;"), br(),
    span(")", style = "margin-left:20px;"), br(),
    span(")"),
    shiny.fluent::IconButton.shinyInput(ns("copy_code_1"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  
  # code_2 <- list()
  # div_code_2 <- list()
  # code_2$en <- paste0(
  #   
  # )
  # div_code_2$en <- div(
  #   
  # )
  # code_2$fr <- paste0(
  #   "# Création d'un concept \"Diurèse\" dans la terminologie MIMIC-IV\n\n",
  #   "new_concept <- get_vocabulary_concept(output = output, m = m, i18n = i18n, ns = ns, vocabulary_id = \"MIMIC-IV\", concept_name = \"Diuresis\", method = \"concept_name\")\n\n",
  #   "if (nrow(new_concept) == 0){\n",
  #   "add_vocabulary_concept(output = output, m = m, i18n = i18n, ns = ns, vocabulary_id = \"MIMIC-IV\", concept_name = \"Diuresis\", domain_id = \"Measurement\", ",
  #     "concept_class_id = \"Clinical Observation\", concept_code = \"Diuresis\")\n\n",
  #   "new_concept <- get_vocabulary_concept(output = output, m = m, i18n = i18n, ns = ns, vocabulary_id = \"MIMIC-IV\", concept_name = \"Diuresis\", method = \"concept_name\")\n",
  #   "}\n\n",
  #   "# Fusion des différents concepts concernant la diurèse au sein de notre nouvel item \"Diurèse\"\n",
  #   "new_concept_values <-\n",
  #   "  d$measurement %>%\n",
  #   "  dplyr::filter(measurement_concept_id %in% c(3036603, 3014315)) %>%\n",
  #   "  dplyr::mutate(\n",
  #   "    measurement_id = NA_integer_,\n",
  #   "    person_id = person_id,\n",
  #   "    measurement_concept_id = new_concept$concept_id,\n",
  #   "    measurement_concept_name = new_concept$concept_name,\n",
  #   "    measurement_date = measurement_date,\n",
  #   "    measurement_datetime = measurement_datetime,\n",
  #   "    measurement_time = measurement_time,\n",
  #   "    measurement_type_concept_id = measurement_type_concept_id,\n",
  #   "    measurement_type_concept_name, measurement_type_concept_name,\n",
  #   "    operator_concept_id = operator_concept_id,\n",
  #   "    value_as_number = sum(value_as_number),\n",
  #   "    value_as_concept_id = value_as_concept_id,\n",
  #   "    value_as_concept_name = value_as_concept_name,\n",
  #   "    unit_concept_id = unit_concept_id,\n",
  #   "    unit_concept_code = unit_concept_code,\n",
  #   "    range_low = range_low,\n",
  #   "    range_high = range_high,\n",
  #   "    provider_id = provider_id,\n",
  #   "    visit_occurrence_id = visit_occurrence_id,\n",
  #   "    visit_detail_id = visit_detail_id,\n",
  #   "    measurement_source_value = measurement_source_value,\n",
  #   "    measurement_source_concept_id = measurement_source_concept_id,\n",
  #   "    unit_source_value = unit_source_value,\n",
  #   "    value_source_value = value_source_value\n",
  #   "  )\n\n",
  #   "if (nrow(new_concept_values) > 0) new_concept_values <- new_concept_values %>% dplyr::mutate(measurement_id = max(d$measurement$measurement_id) + 1:dplyr::n())\n\n",
  #   "d$measurement <- d$measurement %>% dplyr::bind_rows(new_concept_values)"
  # )
  # div_code_2$fr <- div(
  #   span("# Création d'un concept \"Diurèse\" dans la terminologie MIMIC-IV"), br(), br(),
  #   span("new_concept <- get_vocabulary_concept(output = output, m = m, i18n = i18n, ns = ns,"), br(),
  #   span("vocabulary_id = \"MIMIC-IV\", concept_name = \"Diuresis\", method = \"concept_name\")", style = "margin-left:20px;"), br(), br(),
  #   span("if (nrow(new_concept) == 0){"), br(),
  #   span("add_vocabulary_concept(output = output, m = m, i18n = i18n, ns = ns, vocabulary_id = \"MIMIC-IV\", concept_name = \"Diuresis\", domain_id = \"Measurement\", ", style = "margin-left:20px;"), br(),
  #   span("concept_class_id = \"Clinical Observation\", concept_code = \"Diuresis\")", style = "margin-left:40px;"), br(), br(),
  #   span("new_concept <- get_vocabulary_concept(output = output, m = m, i18n = i18n, ns = ns, "), br(),
  #   span("vocabulary_id = \"MIMIC-IV\", concept_name = \"Diuresis\", method = \"concept_name\")", style = "margin-left:20px;"), br(),
  #   span("}"), br(), br(),
  #   span("# Fusion des différents concepts concernant la diurèse au sein de notre nouvel item \"Diurèse\""), br(),
  #   span("new_concept_values <-"), br(),
  #   span("d$measurement %>%", style = "margin-left:20px;"), br(),
  #   span("dplyr::filter(measurement_concept_id %in% c(3036603, 3014315)) %>%", style = "margin-left:20px;"), br(),
  #   span("dplyr::mutate(", style = "margin-left:20px;"), br(),
  #   span("measurement_id = NA_integer_,", style = "margin-left:40px;"), br(),
  #   span("person_id = person_id,", style = "margin-left:40px;"), br(),
  #   span("measurement_concept_id = new_concept$concept_id,", style = "margin-left:40px;"), br(),
  #   span("measurement_concept_name = new_concept$concept_name,", style = "margin-left:40px;"), br(),
  #   span("measurement_date = measurement_date,", style = "margin-left:40px;"), br(),
  #   span("measurement_datetime = measurement_datetime,", style = "margin-left:40px;"), br(),
  #   span("measurement_time = measurement_time,", style = "margin-left:40px;"), br(),
  #   span("measurement_type_concept_id = measurement_type_concept_id,", style = "margin-left:40px;"), br(),
  #   span("measurement_type_concept_name, measurement_type_concept_name,", style = "margin-left:40px;"), br(),
  #   span("operator_concept_id = operator_concept_id,", style = "margin-left:40px;"), br(),
  #   span("value_as_number = sum(value_as_number),", style = "margin-left:40px;"), br(),
  #   span("value_as_concept_id = value_as_concept_id,", style = "margin-left:40px;"), br(),
  #   span("value_as_concept_name = value_as_concept_name,", style = "margin-left:40px;"), br(),
  #   span("unit_concept_id = unit_concept_id,", style = "margin-left:40px;"), br(),
  #   span("unit_concept_code = unit_concept_code,", style = "margin-left:40px;"), br(),
  #   span("range_low = range_low,", style = "margin-left:40px;"), br(),
  #   span("range_high = range_high,", style = "margin-left:40px;"), br(),
  #   span("provider_id = provider_id,", style = "margin-left:40px;"), br(),
  #   span("visit_occurrence_id = visit_occurrence_id,", style = "margin-left:40px;"), br(),
  #   span("visit_detail_id = visit_detail_id,", style = "margin-left:40px;"), br(),
  #   span("measurement_source_value = measurement_source_value,", style = "margin-left:40px;"), br(),
  #   span("measurement_source_concept_id = measurement_source_concept_id,", style = "margin-left:40px;"), br(),
  #   span("unit_source_value = unit_source_value,", style = "margin-left:40px;"), br(),
  #   span("value_source_value = value_source_value", style = "margin-left:40px;"), br(),
  #   span(")", style = "margin-left:20px;"), br(), br(),
  #   span("if (nrow(new_concept_values) > 0) new_concept_values <- new_concept_values %>% dplyr::mutate(measurement_id = max(d$measurement$measurement_id) + 1:dplyr::n())"), br(), br(),
  #   span("d$measurement <- d$measurement %>% dplyr::bind_rows(new_concept_values)"),
  #   shiny.fluent::IconButton.shinyInput(ns("copy_code_2"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
  #   style = r$code_style
  # )
  
  code_3 <- list()
  div_code_3 <- list()
  code_3$fr <- paste0(
    "## Script de data cleaning pour le poids et la taille\n\n",
    "Ce script permet d'exclure les données aberrantes pour le poids et la taille, avec les règles suivantes.\n\n",
    "**Pour le poids** :\n",
    "- Exclusion des valeurs inférieures à __ kg\n",
    "- Exclusion des valeurs supérieures à __ kg\n\n",
    "**Pour la taille** :\n",
    "- Exclusion des valeurs inférieures à __ cm\n",
    "- Exclusion des valeurs supérieures à __ cm\n\n",
    "Les poids sont transformés en taille si leur valeur est inférieure à 2.5 et l'âge du patient supérieur à 1 an (inversion des champs poids et taille)."
  )
  div_code_3$fr <- div(
    span("## Script de data cleaning pour le poids et la taille"), br(), br(),
    span("Ce script permet d'exclure les données aberrantes pour le poids et la taille, avec les règles suivantes."), br(), br(),
    span("**Pour le poids** :"), br(),
    span("- Exclusion des valeurs inférieures à __ kg"), br(),
    span("- Exclusion des valeurs supérieures à __ kg"), br(), br(),
    span("**Pour la taille** :"), br(),
    span("- Exclusion des valeurs inférieures à __ cm"), br(),
    span("- Exclusion des valeurs supérieures à __ cm"), br(), br(),
    span("Les poids sont transformés en taille si leur valeur est inférieure à 2.5 et l'âge du patient supérieur à 1 an (inversion des champs poids et taille)."),
    shiny.fluent::IconButton.shinyInput(ns("copy_code_3"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  code_3$en <- paste0(
    "## Data Cleaning Script for weight and height\n\n",
    "This script is used to exclude outlier data for weight and height, with the following rules.\n\n",
    "For weight :\n",
    "- Exclusion of values less than __ kg\n",
    "- Exclusion of values greater than __ kg\n\n",
    "For height :\n",
    "- Exclusion of values less than __ cm\n",
    "- Exclusion of values greater than __ cm\n\n",
    "Weights are converted to heights if their value is less than 2.5 and the patient's age is greater than 1 year (inversion of weight and height fields).")
  div_code_3$en <- div(
    span("## Data Cleaning Script for Weight and Height"), br(), br(),
    span("This script is used to exclude outlier data for weight and height, with the following rules."), br(), br(),
    span("For weight :"), br(),
    span("- Exclusion of values less than __ kg"), br(),
    span("- Exclusion of values greater than __ kg"), br(), br(),
    span("For height :"), br(),
    span("- Exclusion of values less than __ cm"), br(),
    span("- Exclusion of values greater than __ cm"), br(), br(),
    span("Weights are converted to heights if their value is less than 2.5 and the patient's age is greater than 1 year (inversion of weight and height fields)."),
    shiny.fluent::IconButton.shinyInput(ns("copy_code_3"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  
  # What's a plugin ?
  
  observeEvent(r$help_scripts_page_1, {
    
    load_help_page(r)
    
    r$help_scripts_modal_title <- i18n$t("whats_a_plugin")
    
    if (language == "fr"){
      r$help_scripts_modal_text <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Les scripts sont des morceaux de code ", strong("R"), " qui s'", strong("éxécutent au chargement d'un set de données"), "."),
        p("Ils permetent :"),
        tags$ul(
          tags$li("De réaliser du data cleaning"),
          tags$li("D'ajouter ou de fusionner des concepts")
        ),
        br()
      )
    }
    
    if (language == "en"){
      r$help_scripts_modal_text <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Scripts are pieces of ", strong("R"), " code that ", strong("execute upon loading a data set"), "."),
        p("They allow:"),
        tags$ul(
          tags$li("To perform data cleaning"),
          tags$li("To add or merge concepts")
        ),
        br()
      )
    }
  })
  
  # Remote & local scripts
  
  observeEvent(r$help_scripts_page_2, {
    
    load_help_page(r)
    
    r$help_scripts_modal_title <- i18n$t("local_and_remote_scripts")
    
    if (language == "fr"){
      r$help_scripts_modal_text <- div(
        p("Dans l'onglet ", tags$em("Tous les scripts"), ", vous avez accès aux scripts locaux et à des scripts sur git distant."),
        tags$h3(tags$i(class = "fa fa-house", style = "color: steelblue;"), " ", strong("Scripts locaux")),
        p("Ici sont répertoriés l'ensemble des scripts ", strong("disponibles sur votre instance"), " de l'application."),
        p("Ces scripts sont soit des scripts que vous avez ", strong("créés localement"), ", soit des scripts ", strong("téléchargés"), " depuis un git distant."),
        p("Pour obtenir une ", strong("description du scripts"), ", cliquez sur la ligne du tableau concernant ce script."),
        tags$h3(tags$i(class = "fa fa-cloud", style = "color: steelblue;"), " ", strong("Scripts sur git distant")),
        p("Vous pouvez accéder à une liste de scripts disponibles sur un git distant (fichiers stockés sur un site type github.com, framagit.org...)."),
        p("Cliquez sur l'icône ", actionButton("download_button_help", "", icon = icon("plus")), " pour ", strong("télécharger"), " un script ou sur l'icône ",
          actionButton("update_button_help", "", icon = icon("refresh")), " pour ", strong("mettre à jour"), " le script"),
        br()
      )
    }
    
    if (language == "en"){
      r$help_scripts_modal_text <- div(
        p("In the ", tags$em("All scripts"), " tab, you have access to local scripts and scripts on a remote git."),
        tags$h3(tags$i(class = "fa fa-house", style = "color: steelblue;"), " ", strong("Local Scripts")),
        p("Here are listed all the scripts ", strong("available on your instance"), " of the application."),
        p("These scripts are either scripts that you have ", strong("created locally"), ", or scripts ", strong("downloaded"), " from a remote git."),
        p("To get a ", strong("description of the script"), ", click on the line in the table concerning this script."),
        tags$h3(tags$i(class = "fa fa-cloud", style = "color: steelblue;"), " ", strong("Scripts on Remote Git")),
        p("You can access a list of scripts available on a remote git (files stored on sites like github.com, framagit.org...)."),
        p("Click on the icon ", actionButton("download_button_help", "", icon = icon("plus")), " to ", strong("download"), " a script or on the icon ",
          actionButton("update_button_help", "", icon = icon("refresh")), " to ", strong("update"), " the script"),
        br()
      )
    }
  })
  
  # Choose dataset scripts
  
  observeEvent(r$help_scripts_page_3, {
    
    load_help_page(r)
    
    r$help_scripts_modal_title <- i18n$t("choose_dataset_scripts")
    
    if (language == "fr"){
      r$help_scripts_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ", strong("Configurer le set de données")),
        p("La colonne de droite référencie les scripts ", strong("disponibles"), " pour cet set de données, non utilisés."),
        p("La colonne de gauche référencie les scripts ", strong("utilisés"), " pour ce set de données."),
        p("Lorsque vous chargez un set de données, tous les scripts de la colonne ", tags$em("Scripts choisis"), " seront ", strong("éxécutés au lancement du set de données"), "."),
        p("Cliquez sur un script et glissez-le dans la colonne correspondante."),
        tags$h3(tags$i(class = "fa fa-sd-card", style = "color: steelblue;"), " ", strong("Mémoire cache")),
        p("L'éxécution de certains scripts peut prendre du temps au chargement d'un set de données."),
        p("Activer la mémoire cache permet de ", strong("sauvegarder"), " au format CSV les ", strong("sets de données après éxécution des scripts"), 
          ", permettant un gain de temps au chargement du set de données."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_scripts_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ", strong("Configure the dataset")),
        p("The right column lists the ", strong("available"), " scripts for this dataset that are not used."),
        p("The left column lists the ", strong("used"), " scripts for this dataset."),
        p("When you load a dataset, all the scripts in the ", tags$em("Selected scripts"), " column will be ", strong("executed at the launch of the dataset"), "."),
        p("Click on a script and drag it to the corresponding column."),
        tags$h3(tags$i(class = "fa fa-sd-card", style = "color: steelblue;"), " ", strong("2) Cache memory")),
        p("The execution of some scripts can take time when loading a dataset."),
        p("Enabling cache memory allows you to ", strong("save"), " the datasets in CSV format after the ", strong("scripts have been executed"),
          ", which saves time when loading the dataset."),
        br()
      )
    }
  })
  
  # Scripts management
  
  observeEvent(r$help_scripts_page_4, {
    
    load_help_page(r)
    
    r$help_scripts_modal_title <- i18n$t("scripts_management")
    
    if (language == "fr"){
      r$help_scripts_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ", strong("Créer un script")),
        p("Pour créer un script, allez dans l'onglet ", tags$em("Gestion des scripts"), "."), 
        p("Choisissez un nom, faites-en sorte qu'il ne soit pas déjà utilisé, puis cliquez sur ", tags$em("Ajouter"), "."),
        tags$h3(tags$i(class = "fa fa-pen-to-square", style = "color: steelblue;"), " ", strong("Changer le nom d'un script")),
        p("Pour changer le nom d'une étude, double-cliquez sur le nom, changez-le, faites-en sorte qu'il ne soit pas déjà utilisé, puis cliquez sur ", tags$em("Sauvegarder"), "."),
        tags$h3(tags$i(class = "fa fa-trash", style = "color: steelblue;"), " ", strong("Supprimer un ou des scripts")),
        p("Pour supprimer une ou plusieurs scripts, sélectionnez-les en cliquant dessus dans le tableau puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
        p("Vous pouvez également supprimer un script en cliquant sur l'icône  ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), " ."),
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ", strong("Editer le code ou les options d'un script")),
        p("Cliquez sur"),
        p(shiny::actionButton("edit_plugin_code_button_help", "", icon = icon("file-code")), "  pour ", strong("éditer le code"), " du script,"),
        p(shiny::actionButton("edit_plugin_options_button_help", "", icon = icon("cog")), "  pour ", strong("éditer les options"), " du script"),
        br()
      )
    }
    
    if (language == "en"){
      r$help_scripts_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ", strong("Create a script")),
        p("To create a script, go to the ", tags$em("Scripts Management"), " tab."),
        p("Choose a name, make sure it is not already used, then click on ", tags$em("Add"), "."),
        tags$h3(tags$i(class = "fa fa-pen-to-square", style = "color: steelblue;"), " ", strong("Change the name of a script")),
        p("To change the name of a script, double-click on the name, change it, make sure it is not already used, then click on ", tags$em("Save"), "."),
        tags$h3(tags$i(class = "fa fa-trash", style = "color: steelblue;"), " ", strong("Delete one or more scripts")),
        p("To delete one or more scripts, select them by clicking on them in the table, then click on ", tags$em("Delete selection"), "."),
        p("You can also delete a script by clicking on the  ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), "  icon."),
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ", strong("Edit the code or options of a script")),
        p("Click on"),
        p(shiny::actionButton("edit_plugin_code_button_help", "", icon = icon("file-code")), "  to ", strong("edit the code"), " of the script,"),
        p(shiny::actionButton("edit_plugin_options_button_help", "", icon = icon("cog")), "  to ", strong("edit the options"), " of the script"),
        br()
      )
    }
  })
  
  # Edit script code
  
  observeEvent(r$help_scripts_page_5, {
    
    load_help_page(r)
    
    r$help_scripts_modal_title <- i18n$t("edit_script_code")
    
    if (language == "fr"){
      r$help_scripts_modal_text <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Dans cette rubrique, vous pouvez ", strong("écrire le code"), " d'un script et le ", strong("tester"), "."),
        p("L'intérêt d'un script est de faire du ", strong("data cleaning"), " sur un set, ou de ", strong("créer de nouveaux concepts"), " de terminologie."),
        p("Par exemple :"),
        tags$ul(
          tags$li("Script permettant ", strong("d'exclure les données aberrantes de poids et de taille"), ", par exemple lorsque les deux valeurs sont inversées."),
          tags$li("Script permettant de ", strong("créer un concept \"Diurèse\""), " faisant la somme des diurèses de sonde urinaire, de néphrostomie, de miction etc")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Voici un exemple de code pour ", strong("l'exclusion de valeurs aberrantes"), " de taille, en cm :"),
        div_code_1,
        # p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
        #   "Exemple de code pour la ", strong("création d'un concept"), " de diurèse :"),
        # div_code_2$fr,
        br()
      )
    }
    
    if (language == "en"){
      r$help_scripts_modal_text <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "In this section, you can ", strong("write code"), " for a script and ", strong("test it"), "."),
        p("The purpose of a script is to perform ", strong("data cleaning"), " on a dataset or to ", strong("create new concepts"), " in a vocabulary."),
        p("For example:"),
        tags$ul(
          tags$li("Script allowing to ", strong("exclude aberrant weight and height data"), ", for example when the two values are reversed."),
          tags$li("Script allowing to ", strong("create a \"Diuresis\" concept"), " by adding up the diuresis from urinary catheter, nephrostomy, micturition, etc.")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Here is an example of code for ", strong("excluding aberrant height values"), " in cm:"),
        div_code_1,
        # p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
        #   "Example of code for ", strong("creating a new concept"), " of diuresis:"),
        # div_code_2$en,
        br()
      )
    }
  })
  
  # Script options
  
  observeEvent(r$help_scripts_page_6, {
    
    load_help_page(r)
    
    r$help_scripts_modal_title <- i18n$t("script_options")
    
    if (language == "fr"){
      r$help_scripts_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-user", style = "color: steelblue;"), " ", strong("Auteur & version")),
        p("Le nom de l'auteur et la version du script seront visibles depuis l'onglet ", tags$em("Tous les scripts"), "."),
        p("Pensez à ", strong("modifier la version du script"), " lorsque des modifications sont réalisées dans le code."),
        tags$h3(tags$i(class = "fa fa-grip-lines", style = "color: steelblue;"), " ", strong("Nom et catégorie")),
        p("Nom et catégorie s'affichant dans l'onglet ", tags$em("Tous les scripts"), ", selon la langue choisie au démarrage de l'application."),
        tags$h3(tags$i(class = "fa fa-file-lines", style = "color: steelblue;"), " ", strong("Description")),
        p("La description s'affiche dans l'onglet ", tags$em("Tous les scripts"), ", lorsque l'on clique sur une ligne du tableau."),
        p("Le code ici est du ", strong("Markdown"), ", dont voici un exemple."),
        div_code_3$fr,
        p("Cliquez sur ", tags$em("Exécuter"), " pour visualiser le rendu de la description."),
        p("Utilisez les ", strong("raccourcis"), " :"),
        tags$ul(
          tags$li("CMD/CTRL + SHIFT + ENTER : exécute l'ensemble du code"),
          tags$li("CMD/CTRL + ENTER : exécute le code sélectionné"),
          tags$li("CMD/CTRL + S : sauvegarde le code")
        ),
        br()
      )
    }
    
    if (language == "en"){
      r$help_scripts_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-user", style = "color: steelblue;"), " ", strong("Author & version")),
        p("The author's name and script version will be visible from the ", tags$em("All scripts"), " tab."),
        p("Remember to ", strong("update the script version"), " whenever changes are made to the code."),
        tags$h3(tags$i(class = "fa fa-grip-lines", style = "color: steelblue;"), " ", strong("Name and category")),
        p("Name and category displayed in the ", tags$em("All scripts"), " tab, according to the language chosen at the start of the application."),
        tags$h3(tags$i(class = "fa fa-file-lines", style = "color: steelblue;"), " ", strong("Description")),
        p("The description is displayed in the ", tags$em("All scripts"), " tab, when you click on a line in the datatable."),
        p("The code here is in ", strong("Markdown"), ", here is an example."),
        div_code_3$fr,
        p("Click on ", tags$em("Run code"), " to view the description render."),
        p("Use the ", strong("shortcuts"), " :"),
        tags$ul(
          tags$li("CMD/CTRL + SHIFT + ENTER : runs all of the code"),
          tags$li("CMD/CTRL + ENTER : runs the selected code"),
          tags$li("CMD/CTRL + S : saves the code")
        ),
        br()
      )
    }
  })
  
  # Import and export scripts
  
  observeEvent(r$help_scripts_page_7, {
    
    load_help_page(r)
    
    r$help_scripts_modal_title <- i18n$t("script_options")
    
    if (language == "fr"){
      r$help_scripts_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-download", style = "color: steelblue;"), " ",  strong("Importer un script")),
        p("Vous pouvez importer un ou plusieurs scripts ", strong("à partir d'un fichier ZIP"), ", crée depuis l'onglet ", tags$em("Exporter un script"), "."),
        p("Si le script existe déjà, il ne sera remplacé que si l'option ", tags$em("Remplacer les scripts déjà existants"), " est cochée."),
        tags$h3(tags$i(class = "fa fa-upload", style = "color: steelblue;"), " ", strong("Exporter un script")),
        p("Vous pouvez exporter un ou plusieurs scripts, ils seront téléchargés dans un fichier ZIP, que vous pouvez ", strong("partager"), " avec d'autres utilisateurs utilisant LinkR."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_scripts_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-download", style = "color: steelblue;"), " ", strong("Import a script")),
        p("You can import one or several scripts ", strong("from a ZIP file"), ", created from the ", tags$em("Export a script"), " tab."),
        p("If the script already exists, it will only be replaced if the option ", tags$em("Replace existing scripts"), " is checked."),
        tags$h3(tags$i(class = "fa fa-upload", style = "color: steelblue;"), " ", strong("Export a script")),
        p("You can export one or several scripts, they will be downloaded in a ZIP file, which you can ", strong("share"), " with other LinkR users."),
        br()
      )
    }
  })
  
  # Copy code divs
  
  observeEvent(r$help_scripts_copy_code_1, clipr::write_clip(code_1))
  # observeEvent(r$help_scripts_copy_code_2, clipr::write_clip(code_2[[language]]))
  observeEvent(r$help_scripts_copy_code_3, clipr::write_clip(code_3[[language]]))
}
