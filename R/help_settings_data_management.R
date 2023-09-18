help_settings_data_management <- function(output, r = shiny::reactiveValues(), id = character(), prefix = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r[[paste0("help_settings_data_management_", prefix, "_open_panel")]],
      br(),
      strong(i18n$t("data_sources")), br(), br(),
      shiny.fluent::Link(i18n$t("data_sources_datatable_card"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      strong(i18n$t("datasets")), br(), br(),
      shiny.fluent::Link(i18n$t("datasets_datatable_card"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("edit_dataset_code"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_3', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("dataset_options"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_4', Math.random()); }"))), br(), br(),
      strong(i18n$t("vocabulary")), br(), br(),
      shiny.fluent::Link(i18n$t("vocabularies_management"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_5', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("edit_vocabulary_code"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_6', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("vocabularies_tables"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_7', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("import_vocabulary"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_8', Math.random()); }"))), br(), br(),
      strong(i18n$t("data_model")), br(), br(),
      shiny.fluent::Link(i18n$t("data_model"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_9', Math.random()); }"))), br(), br(),
      isLightDismiss = r[[paste0("help_settings_data_management_", prefix, "_open_panel_light_dismiss")]],
      isBlocking = r[[paste0("help_settings_data_management_", prefix, "_open_panel_light_dismiss")]],
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({
    
    shiny.fluent::Modal(
      isOpen = r[[paste0("help_settings_data_management_", prefix, "_open_modal")]], dragOptions = TRUE, isModeless = TRUE, topOffsetFixed = TRUE,
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
      div(
        style = "width: 1000px; padding: 15px 10px 0px 15px;",
        shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
          div(style = list(display = "flex"),
            shiny.fluent::Text(r[[paste0("help_settings_data_management_", prefix, "_modal_title")]], variant = "large"),
            div(style = list(flexGrow = 1)),
            shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
          ),
          r[[paste0("help_settings_data_management_", prefix, "_modal_text")]]
        )
      )
    )
  })
  
  load_help_page <- function(r){
    r[[paste0("help_settings_data_management_", prefix, "_open_modal")]] <- TRUE
    r[[paste0("help_settings_data_management_", prefix, "_open_panel_light_dismiss")]] <- FALSE
  }
  
  # Code divs
  
  code_1 <- paste0(
    "person <- function(){\n",
    "  tibble::tibble(\n",
    "    person_id = c(1L, 2L),\n",
    "    gender_concept_id = c(8507L, 8507L),\n",
    "    year_of_birth = c(1990L, 1992L),\n",
    "    month_of_birth = c(NA_integer_, NA_integer_),\n",
    "    day_of_birth = c(NA_integer_, NA_integer_),\n",
    "    birth_datetime = c(as.POSIXct(NA_integer_), as.POSIXct(NA_integer_)),\n",
    "    death_datetime = c(as.POSIXct(NA_integer_), as.POSIXct(NA_integer_)),\n",
    "    race_concept_id = c(NA_integer_, NA_integer_),\n",
    "    ethnicity_concept_id = c(NA_integer_, NA_integer_),\n",
    "    location_id = c(NA_integer_, NA_integer_),\n",
    "    provider_id = c(NA_integer_, NA_integer_),\n",
    "    care_site_id = c(NA_integer_, NA_integer_),\n",
    "    person_source_value = c(\"27783\", \"18939\"),\n",
    "    gender_source_value = c(\"M\", \"M\"),\n",
    "    gender_source_concept_id = c(NA_integer_, NA_integer_),\n",
    "    race_source_value = c(NA_character_, NA_character_),\n",
    "    race_source_concept_id = c(NA_integer_, NA_integer_),\n",
    "    ethnicity_source_value = c(NA_character_, NA_character_),\n",
    "    ethnicity_source_concept_id = c(NA_integer_, NA_integer_)\n",
    " )\n",
    "}\n",
    "\n",
    "import_dataset(output = output, ns = ns, i18n = i18n, r = r, d = d, dataset_id = %dataset_id%, data = person(),\n",
    "  type = \"person\", omop_version = \"6.0\", save_as_csv = TRUE, rewrite = FALSE)"
  )
  div_code_1 <- div(
    "person <- function(){", br(),
    span("tibble::tibble(", style = "padding-left:20px;"), br(),
    span("person_id = c(1L, 2L),", style = "padding-left:40px;"), br(),
    span("gender_concept_id = c(8507L, 8507L),", style = "padding-left:40px;"), br(),
    span("year_of_birth = c(1990L, 1992L),", style = "padding-left:40px;"), br(),
    span("month_of_birth = c(NA_integer_, NA_integer_),", style = "padding-left:40px;"), br(),
    span("day_of_birth = c(NA_integer_, NA_integer_),", style = "padding-left:40px;"), br(),
    span("birth_datetime = c(as.POSIXct(NA_integer_), as.POSIXct(NA_integer_)),", style = "padding-left:40px;"), br(),
    span("death_datetime = c(as.POSIXct(NA_integer_), as.POSIXct(NA_integer_)),", style = "padding-left:40px;"), br(),
    span("race_concept_id = c(NA_integer_, NA_integer_),", style = "padding-left:40px;"), br(),
    span("ethnicity_concept_id = c(NA_integer_, NA_integer_),", style = "padding-left:40px;"), br(),
    span("location_id = c(NA_integer_, NA_integer_),", style = "padding-left:40px;"), br(),
    span("provider_id = c(NA_integer_, NA_integer_),", style = "padding-left:40px;"), br(),
    span("care_site_id = c(NA_integer_, NA_integer_),", style = "padding-left:40px;"), br(),
    span("person_source_value = c(\"27783\", \"18939\"),", style = "padding-left:40px;"), br(),
    span("gender_source_value = c(\"M\", \"M\"),", style = "padding-left:40px;"), br(),
    span("gender_source_concept_id = c(NA_integer_, NA_integer_),", style = "padding-left:40px;"), br(),
    span("race_source_value = c(NA_character_, NA_character_),", style = "padding-left:40px;"), br(),
    span("race_source_concept_id = c(NA_integer_, NA_integer_),", style = "padding-left:40px;"), br(),
    span("ethnicity_source_value = c(NA_character_, NA_character_),", style = "padding-left:40px;"), br(),
    span("ethnicity_source_concept_id = c(NA_integer_, NA_integer_)", style = "padding-left:40px;"), br(),
    span(")", style = "padding-left:20px;"), br(),
    "}", br(),
    "import_dataset(output = output, ns = ns, i18n = i18n, r = r, d = d, dataset_id = %dataset_id%, data = person(),", br(),
    span("type = \"person\", omop_version = \"6.0\", save_as_csv = TRUE, rewrite = FALSE)", style = "padding-left:20px;"),
    shiny.fluent::IconButton.shinyInput(ns("copy_code_1"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  
  code_2 <- paste0(
    "folder <- \"https://www.physionet.org/files/mimic-iv-demo-omop/0.9/1_omop_data_csv/\"\n\n",
    "col_types <- list()\n",
    "col_types$concept <- \"iccccccDDc\"\n",
    "col_types$concept_relationship <- \"iicDDc\"\n\n",
    "for (table_name in c(\"concept\", \"concept_relationship\")){\n\n",
    "  cat(paste0(toupper(table_name), \"\\n\\n\"))\n\n",
    "  data <-\n",
    "    vroom::vroom(paste0(folder, \"2b_\", table_name, \".csv\"), col_types = col_types[[table_name]], progress = FALSE) %>%\n",
    "    dplyr::rename(valid_start_date = valid_start_DATE, valid_end_date = valid_end_DATE)\n\n",
    "  import_vocabulary_table(output = output, ns = ns, i18n = i18n, r = r, m = m, table_name = table_name, data = data, vocabulary_id = \"%vocabulary_id%\") %>% print()\n",
    "  cat(\"\\n\\n\")\n", 
    "}"
  )
  div_code_2 <- div(
    "folder <- \"https://www.physionet.org/files/mimic-iv-demo-omop/0.9/1_omop_data_csv/\"", br(), br(),
    "col_types <- list()", br(),
    "col_types$concept <- \"iccccccDDc\"", br(),
    "col_types$concept_relationship <- \"iicDDc\"", br(), br(),
    "for (table_name in c(\"concept\", \"concept_relationship\")){", br(), br(),
    span("cat(paste0(toupper(table_name), \"\\n\\n\"))", style = "padding-left:20px;"), br(), br(),
    span("data <-", style = "padding-left:20px;"), br(),
    span("vroom::vroom(paste0(folder, \"2b_\", table_name, \".csv\"), col_types = col_types[[table_name]], progress = FALSE) %>%", style = "padding-left:40px;"), br(),
    span("dplyr::rename(valid_start_date = valid_start_DATE, valid_end_date = valid_end_DATE)", style = "padding-left:40px;"), br(), br(),
    span("import_vocabulary_table(output = output, ns = ns, i18n = i18n, r = r, m = m, table_name = table_name, data = data, vocabulary_id = \"%vocabulary_id%\") %>% print()", style = "padding-left:20px;"), br(),
    span("cat(\"\\n\\n\")", style = "padding-left:20px;"), br(),
    "}",
    shiny.fluent::IconButton.shinyInput(ns("copy_code_2"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  
  # Data sources management
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_1")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("data_sources_datatable_card")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ", strong("Ajouter une source de données")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Pour créer une source de données, entrez un nom et cliquez sur ", tags$em("Ajouter"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Une source de données comprend ", strong("plusieurs sets de données"), "."),
        p("Par exemple, vous créez une source de données nommée ", tags$em("MIMIC-IV"), ", qui comprendra les sets de données créés à partir de la base de données MIMIC-IV."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Différents sets de données seront créés à partir de cette source, par exemple : "),
        tags$ul(
          tags$li("un set ", tags$em("Infections nosocomiales"), " qui comprendra les patients ayant eu une infection nosocomiale au cours de leur séjour hospitalier"),
          tags$li("un set ", tags$em("Hépatites"), " qui comprendra les patients ayant été diagnostiqué d'une hépatite")
        ),
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ", strong("Gérer les sources de données")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Vous pouvez modifier le nom des sources de données en double-cliquant sur la ligne et la colonne correspondants dans le tableau."),
        p("Une fois les informations modifiées, cliquez sur ", tags$em("Sauvegarder"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Pour supprimer une ou plusieurs sources de données, sélectionnez-les en cliquant dessus dans le tableau puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
        p("Vous pouvez également supprimer une source de données en cliquant sur l'icône  ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), " ."),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ", strong("Add a data source")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "To create a data source, enter a name and click on ", tags$em("Add"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "A data source comprises ", strong("several datasets"), "."),
        p("For instance, you create a data source named ", tags$em("MIMIC-IV"), ", which will comprise datasets created from the MIMIC-IV database."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Different datasets will be created from this source, for example: "),
        tags$ul(
          tags$li("a set ", tags$em("Hospital-Acquired Infections"), " that will comprise patients who had a hospital-acquired infection during their hospital stay"),
          tags$li("a set ", tags$em("Hepatitis"), " that will comprise patients who have been diagnosed with hepatitis")
        ),
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ", strong("Manage data sources")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "You can modify the name of the data sources by double-clicking on the corresponding row and column in the table."),
        p("Once the information is modified, click on ", tags$em("Save"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "To delete one or more data sources, select them by clicking on them in the table and then click on ", tags$em("Delete selection"), "."),
        p("You can also delete a data source by clicking on the icon  ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), " ."),
        br()
      )
    }
  })
  
  # Datasets management
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_2")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("datasets_datatable_card")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ", strong("Ajouter un set de données")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Pour créer un set de données, entrez un nom, choisissez de ", strong("quelle source de données"), " va dépendre le set puis cliquez sur ", tags$em("Ajouter"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Un set de données comprend ", strong("plusieurs études"), "."),
        p("Par exemple, vous créez un set de données nommé ", tags$em("Infections nosocomiales"), " à partir de la source de données ", tags$em("MIMIC-IV"), 
          ", qui comprendra tous les patients ayant développé une infection nosocomiale au cours d'une période définie."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Plusieurs études seront créées à partir de ce set, par exemple "),
        tags$ul(
          tags$li("une étude ", tags$em("Epidémiologie des infections nosocomiales")),
          tags$li("une étude ", tags$em("Incidence des infections nosocomiales à BMR"))
        ),
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ", strong("Gérer les sets de données")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Vous pouvez modifier le nom des sets de données en double-cliquant sur la ligne et la colonne correspondants dans le tableau."),
        p("Une fois les informations modifiées, cliquez sur ", tags$em("Sauvegarder"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Pour supprimer un ou plusieurs sets de données, sélectionnez-les en cliquant dessus dans le tableau puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
        p("Vous pouvez également supprimer un set de données en cliquant sur l'icône  ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), " ."),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ", strong("Add a data set")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "To create a data set, enter a name, choose from which ", strong("data source"), " the set will depend and then click on ", tags$em("Add"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "A data set includes ", strong("several studies"), "."),
        p("For example, you create a data set named ", tags$em("Nosocomial Infections"), " from the data source ", tags$em("MIMIC-IV"), 
          ", which will include all patients who developed a nosocomial infection over a defined period."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Several studies will be created from this set, for example "),
        tags$ul(
          tags$li("a study ", tags$em("Epidemiology of Nosocomial Infections")),
          tags$li("a study ", tags$em("Incidence of MDR Nosocomial Infections"))
        ),
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ", strong("Manage data sets")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "You can modify the name of the data sets by double-clicking on the corresponding row and column in the table."),
        p("Once the information is modified, click on ", tags$em("Save"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "To delete one or more data sets, select them by clicking on them in the table and then click on ", tags$em("Delete Selection"), "."),
        p("You can also delete a data set by clicking on the icon ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), "."),
        br()
      )
    }
  })
  
  # Edit dataset code
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_3")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("edit_dataset_code")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Dans cette rubrique, vous pouvez ", strong("écrire le code"), " qui permettra d'obtenir les données du set de données."),
        p(tags$i(class = "fa fa-circle-info", style = "color: steelblue;"), " ", 
          "Pour vous aider, référez-vous à la section ", tags$em("Modèle de données"), ", qui détaille le ", strong("modèle de données"), " utilisé par l'application."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Pour charger un set de données, deux étapes sont nécessaires :"),
        tags$ul(
          tags$li(strong("Créez une fonction"), " qui chargera les données une fois éxécutée, variable par variable (", tags$em("person, measurement"), "...)"),
          tags$li(strong("Importez les données"), " avec la fonction ", 
            strong(tags$a(href = "https://borisdelange.github.io/LinkR/reference/import_dataset.html", "import_dataset", target = "_blank")))
        ),
        p(tags$i(class = "fa fa-code", style = "color: steelblue;"), " ", 
          "La fonction ", tags$em(tags$a(href = "https://borisdelange.github.io/LinkR/reference/import_dataset.html", "import_dataset", target = "_blank")), 
          " comprend les arguments suivants :"),
        tags$ul(
          tags$li(tags$em("output, ns, i18n, r, d"), " : qui sont les variables permettant le fonctionnement de l'application"),
          tags$li(tags$em("dataset_id"), " : où vous indiquez ", strong("l'ID du dataset"), " actuel, via la balise ", tags$em("%dataset_id%")),
          tags$li(tags$em("data"), " : où vous indiquez la ", strong("fonction qui chargera les données"), " pour une variable (exemple : ", tags$em("person"), ")"),
          tags$li(tags$em("type"), " : où vous indiquez la ", strong("variable que vous souhaitez importer"), " (", tags$em("person, measurement"), "...)"),
          tags$li(tags$em("save_as_csv"), " : indiquant si vous souhaitez ", strong("sauvegarder l'import"), " dans un fichier CSV (logical)"),
          tags$li(tags$em("rewrite"), " : indiquant si vous souhaitez écraser l'ancien fichier CSV pour le remplacer par le nouveau (logical)"),
          tags$li(tags$em("quiet"), " : indique si des messages sont affichés lors de l'éxécution du code (logical)")
        ),
        p("L'argument ", tags$em("rewrite"), " n'est utile que lorsque vous travaillez sur le code permettant d'importer le dataset. ",
          "En effet, inutile d'enregistrer une copie au format CSV de votre import s'il est écrasé et recréé à chaque fois que vous chargez le set de données."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Le code que vous créez ici ", strong("s'éxécutera à chaque fois"), " que quelqu'un chargera le set de données,",
          " d'où le fait d'utiliser l'argument ", tags$em("save_as_csv"), " qui permettra d'économiser des ressources."),
        p("Voici un exemple de code :"),
        div_code_1,
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Cliquez sur ", tags$em("Sauvegarder"), " pour sauvegarder le code, sur ", tags$em("Exécuter"), " pour tester le code."),
        p("Un ", strong("tableau"), " en bas de la page vous indiquera le nombre de lignes chargées par variable."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Utilisez les ", strong("raccourcis"), " :",
          tags$ul(
            tags$li("CMD/CTRL + SHIFT + ENTER : exécute l'ensemble du code"),
            tags$li("CMD/CTRL + ENTER : exécute le code sélectionné"),
            tags$li("CMD/CTRL + S : sauvegarde le code")
          )  
        ),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "In this section, you can ", strong("write the code"), " that will allow you to obtain the data from the data set."),
        p(tags$i(class = "fa fa-circle-info", style = "color: steelblue;"), " ", 
          "For help, refer to the ", tags$em("Data Model"), " section, which details the ", strong("data model"), " used by the application."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "To load a data set, two steps are necessary:"),
        tags$ul(
          tags$li(strong("Create a function"), " that will load the data once executed, variable by variable (", tags$em("person, measurement"), "...)."),
          tags$li(strong("Import the data"), " with the function ",
            tags$em(tags$a(href = "https://borisdelange.github.io/LinkR/reference/import_dataset.html", "import_dataset", target = "_blank")))
        ),
        p(tags$i(class = "fa fa-code", style = "color: steelblue;"), " ", 
          "The function ", strong(tags$a(href = "https://borisdelange.github.io/LinkR/reference/import_dataset.html", "import_dataset", target = "_blank")), 
          " includes the following arguments:"),
        tags$ul(
          tags$li(tags$em("output, ns, i18n, r, d"), ": which are the variables allowing the application to function"),
          tags$li(tags$em("dataset_id"), ": where you indicate the current ", strong("dataset ID"), ", via the tag ", tags$em("%dataset_id%")),
          tags$li(tags$em("data"), ": where you indicate the ", strong("function that will load the data"), " for a variable (example: ", tags$em("person"), ")"),
          tags$li(tags$em("type"), ": where you indicate the ", strong("variable you wish to import"), " (", tags$em("person, measurement"), "...)."),
          tags$li(tags$em("save_as_csv"), ": indicating whether you want to ", strong("save the import"), " in a CSV file (logical)"),
          tags$li(tags$em("rewrite"), ": indicating whether you want to overwrite the old CSV file to replace it with the new one (logical)"),
          tags$li(tags$em("quiet"), ": indicates whether messages are displayed when the code is executed (logical)")
        ),
        p("The argument ", tags$em("rewrite"), " is only useful when you are working on the code to import the dataset. ",
          "Indeed, there's no point in saving a CSV copy of your import if it's overwritten and recreated every time you load the data set."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "The code you create here ", strong("will run each time"), " someone loads the data set,",
          " hence the use of the argument ", tags$em("save_as_csv"), " which will save resources."),
        p("Here is an example of code:"),
        div_code_1,
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Click on ", tags$em("Save"), " to save the code, on ", tags$em("Execute"), " to test the code."),
        p("A ", strong("table"), " at the bottom of the page will indicate the number of lines loaded per variable."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Use the ", strong("shortcuts"), ":",
          tags$ul(
            tags$li("CMD/CTRL + SHIFT + ENTER: executes the entire code"),
            tags$li("CMD/CTRL + ENTER: executes the selected code"),
            tags$li("CMD/CTRL + S: saves the code")
          )
        ),
        br()
      )
    }
  })
  
  # Dataset options
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_4")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("dataset_options")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Accès aux données agrégées uniquement")),
        p("Vous pouvez choisir de ne donner accès qu'aux ", strong("Données agrégées"), " d'un set de données."),
        p("Ainsi, les utilisateurs n'auront ", strong("pas accès aux données individuelles"), " de cet set, depuis la page ", tags$em("Données"), "."),
        tags$h3(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Version d'OMOP")),
        p("Sélectionnez la version d'OMOP dont est issu le set de données."),
        p("Voir la ", tags$a(href = "https://ohdsi.github.io/CommonDataModel/cdm60.html", "documentation OMOP", target = "_blank"),
          " pour plus d'informations."),
        tags$h3(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Accès aux données")),
        p("Choisissez qui a accès à ce set de données."),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Access to aggregated data only")),
        p("You can choose to provide access to the ", strong("Aggregated Data"), " of a dataset only."),
        p("Thus, users will ", strong("not have access to individual data"), " from this dataset, from the ", tags$em("Data"), " page."),
        tags$h3(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("OMOP Version")),
        p("Select the version of OMOP from which the dataset is derived."),
        p("See the ", tags$a(href = "https://ohdsi.github.io/CommonDataModel/cdm60.html", "OMOP documentation", target = "_blank"),
          " for more information."),
        tags$h3(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Data Access")),
        p("Choose who has access to this dataset."),
        br()
      )
      
    }
  })
  
  # Vocabularies management
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_5")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("vocabularies_management")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ", strong("Ajouter une terminologie")),
        p("Pour créer une terminologie, entrez un nom, choisissez depuis ", strong("quelles sources de données"), " ces terminologies serront accessibles puis cliquez sur ", tags$em("Ajouter"), "."),
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ", strong("Gérer les terminologies")),
        p("Vous pouvez modifier le nom des terminologies en double-cliquant sur la ligne et la colonne correspondants dans le tableau."),
        p("Vous pouvez changer les ", strong("sources de données associées à la terminologie"), " en cliquant sur les menus déroulants dans la colonne ", tags$em("Sources de données"), "."),
        p("Une fois les informations modifiées, cliquez sur ", tags$em("Sauvegarder"), "."),
        p("Pour supprimer une ou plusieurs terminologies, sélectionnez-les en cliquant dessus dans le tableau puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
        p("Vous pouvez également supprimer une terminologie en cliquant sur l'icône  ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), " ."),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ", strong("Add a vocabulary")),
        p("To create a vocabulary, enter a name, choose from which ", strong("data sources"), " these vocabularies will be accessible and then click on ", tags$em("Add"), "."),
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ", strong("Manage vocabularies")),
        p("You can modify the name of vocabularies by double-clicking on the corresponding row and column in the table."),
        p("You can change the ", strong("data sources associated with the vocabulary"), " by clicking on the dropdown menus in the ", tags$em("Data sources"), " column."),
        p("Once the information is modified, click on ", tags$em("Save"), "."),
        p("To delete one or more vocabularies, select them by clicking on them in the table and then click on ", tags$em("Delete selection"), "."),
        p("You can also delete a vocabulary by clicking on the icon  ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), " ."),
        br()
      )
    }
  })
  
  # Edit vocabulary code
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_6")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("edit_vocabulary_code")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Dans cette rubrique, vous pouvez ", strong("écrire le code"), " qui permettra de charger les données d'une terminologie."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Pour charger une terminologie, ", strong("importez les données"), " avec la fonction ", 
          strong(tags$a(href = "https://borisdelange.github.io/LinkR/reference/import_vocabulary_table.html", "import_vocabulary_table", target = "_blank"))),
        p(tags$i(class = "fa fa-code", style = "color: steelblue;"), " ", 
          "La fonction comprend les arguments suivants :"),
        tags$ul(
          tags$li(tags$em("output, ns, i18n, r, m"), " : qui sont les variables permettant le fonctionnement de l'application"),
          tags$li(tags$em("table_name"), " : où vous indiquez le ", strong("nom de table"), " que vous souhaitez importer (concept, concept_relationship...)"),
          tags$li(tags$em("data"), " : où vous indiquez la ", strong("variable contenant les données")),
          tags$li(tags$em("vocabulary_id"), " : où vous indiquez ", strong("l'ID de la terminologie"), " actuelle, via la balise ", tags$em("%vocabulary_id%"))
        ),
        p("Voici un exemple de code :"),
        div_code_2,
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Cliquez sur ", tags$em("Sauvegarder"), " pour sauvegarder le code, sur ", tags$em("Exécuter"), " pour tester le code."),
        p("Un ", strong("message"), " vous indiquera le nombre de lignes chargées par table"),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Utilisez les ", strong("raccourcis"), " :",
          tags$ul(
            tags$li("CMD/CTRL + SHIFT + ENTER : exécute l'ensemble du code"),
            tags$li("CMD/CTRL + ENTER : exécute le code sélectionné"),
            tags$li("CMD/CTRL + S : sauvegarde le code")
          )  
        ),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "In this section, you can ", strong("write the code"), " that will allow loading the data of a vocabulary."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "To load a vocabulary, ", strong("import the data"), " with the function ", 
          strong(tags$a(href = "https://borisdelange.github.io/LinkR/reference/import_vocabulary_table.html", "import_vocabulary_table", target = "_blank"))),
        p(tags$i(class = "fa fa-code", style = "color: steelblue;"), " ", 
          "The function includes the following arguments:"),
        tags$ul(
          tags$li(tags$em("output, ns, i18n, r, m"), " : which are the variables allowing the application to operate"),
          tags$li(tags$em("table_name"), " : where you indicate the ", strong("table name"), " you wish to import (concept, concept_relationship...)"),
          tags$li(tags$em("data"), " : where you indicate the ", strong("variable containing the data")),
          tags$li(tags$em("vocabulary_id"), " : where you indicate the ", strong("ID of the current vocabulary"), " via the tag ", tags$em("%vocabulary_id%"))
        ),
        p("Here is a code example:"),
        div_code_2,
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Click on ", tags$em("Save"), " to save the code, on ", tags$em("Run"), " to test the code."),
        p("A ", strong("message"), " will indicate the number of lines loaded per table"),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Use the ", strong("shortcuts"), ":",
          tags$ul(
            tags$li("CMD/CTRL + SHIFT + ENTER : executes the entire code"),
            tags$li("CMD/CTRL + ENTER : executes the selected code"),
            tags$li("CMD/CTRL + S : saves the code")
          )  
        ),
        br()
      )
    }
  })
  
  # Vocabularies tables
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_7")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("vocabularies_tables")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-database", style = "color: steelblue;"), " ", strong("Tables de terminologie")),
        p("Vous pouvez accéder aux différentes ", strong("tables de terminologie OMOP"), " chargées dans l'application."),
        p("Retrouvez le détail de ces tables ",
          tags$a(href = "https://ohdsi.github.io/CommonDataModel/cdm60.html#Vocabulary_Tables", "sur ce lien", target = "_blank"), "."),
        p("Lorsque vous êtes dans la table ", tags$em("CONCEPT"), ", vous pouvez afficher les concepts alignés au concept sélectionné en cochant ", tags$em("Afficher les concepts alignés"), "."),
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ", strong("Modifier des données")),
        p("Vous pouvez modifier les valeurs de certaines colonnes en double-cliquant dessus dans le tableau puis en cliquant sur ", tags$em("Sauvegarder"), "."),
        tags$h3(tags$i(class = "fa fa-trash", style = "color: steelblue;"), " ", strong("Supprimer des données")),
        p("Pour supprimer des données, sélectionnez-les en cliquant dessus dans le tableau puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-database", style = "color: steelblue;"), " ", strong("Vocabulary tables")),
        p("You can access the different ", strong("OMOP vocabulary tables"), " loaded in the application."),
        p("Find the details of these tables ",
          tags$a(href = "https://ohdsi.github.io/CommonDataModel/cdm60.html#Vocabulary_Tables", "on this link", target = "_blank"), "."),
        p("When you are in the ", tags$em("CONCEPT"), " table, you can display the concepts aligned with the selected concept by checking ", tags$em("Show mapped concepts"), "."),
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ", strong("Modify data")),
        p("You can modify the values of certain columns by double-clicking on them in the table and then clicking on ", tags$em("Save"), "."),
        tags$h3(tags$i(class = "fa fa-trash", style = "color: steelblue;"), " ", strong("Delete data")),
        p("To delete data, select them by clicking on them in the table and then click on ", tags$em("Delete selection"), "."),
        br()
      )
    }
  })
  
  # Import a vocabulary
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_8")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("vocabularies_tables")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        p("Vous pouvez importer une terminologie à partir : "),
        tags$ul(
          tags$li("d'un ", strong("fichier ZIP"), " contenant les fichiers CSV d'une terminologie"),
          tags$li("des ", strong("fichiers CSV"), " d'une terminologie (CONCEPT.csv, CONCEPT_RELATIONSHIP.csv etc)")
        ),
        p("Si les concepts sont déjà présents dans la base de données, il ne seront pas remplacés."),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        p("You can import a vocabulary from: "),
        tags$ul(
          tags$li("a ", strong("ZIP file"), " containing the CSV files of a vocabulary"),
          tags$li("the ", strong("CSV files"), " of a vocabulary (CONCEPT.csv, CONCEPT_RELATIONSHIP.csv etc)")
        ),
        p("If the concepts are already present in the database, they will not be replaced."),
        br()
      )
    }
  })
  
  # Data model
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_9")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("data_model")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Le modèle de données utilisé par l'application est le ", strong("modèle standard OMOP"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Vous pouvez accéder aux ",
          tags$a(href = "https://ohdsi.github.io/CommonDataModel/cdm60.html", "détails du modèle de données ici", target = "_blank"), ".")
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "The data model used by the application is the ", strong("OMOP standard model"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "You can access the ",
          tags$a(href = "https://ohdsi.github.io/CommonDataModel/cdm60.html", "details of the data model here", target = "_blank"), ".")
      )
    }
  })
  
  # Copy code divs
  
  observeEvent(r$help_settings_data_management_copy_code_1, clipr::write_clip(code_1))
  observeEvent(r$help_settings_data_management_copy_code_2, clipr::write_clip(code_2))
}
