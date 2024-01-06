help_settings_data_management <- function(output, r = shiny::reactiveValues(), id = character(), prefix = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r[[paste0("help_settings_data_management_", prefix, "_open_panel")]],
      br(),
      strong(i18n$t("data")), br(), br(),
      shiny.fluent::Link(i18n$t("app_data_structure"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("data_model"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_9', Math.random()); }"))), br(), br(),
      strong(i18n$t("datasets")), br(), br(),
      shiny.fluent::Link(i18n$t("all_datasets_card"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_10', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("datasets_datatable_card"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("edit_dataset_code"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_3', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("dataset_options"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_4', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("import_export_datasets"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_11', Math.random()); }"))), br(), br(),
      strong(i18n$t("vocabularies")), br(), br(),
      shiny.fluent::Link(i18n$t("vocabularies_management"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_5', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("edit_vocabulary_code"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_6', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("vocabularies_tables"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_7', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("import_vocabulary"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_8', Math.random()); }"))), br(), br(),
      isLightDismiss = r[[paste0("help_settings_data_management_", prefix, "_open_panel_light_dismiss")]],
      isBlocking = r[[paste0("help_settings_data_management_", prefix, "_open_panel_light_dismiss")]],
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({
    
    shiny.fluent::Modal(
      isOpen = r[[paste0("help_settings_data_management_", prefix, "_open_modal")]], dragOptions = TRUE, isModeless = FALSE, topOffsetFixed = TRUE,
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
  
  code <- list()
  div_code <- list()
  
  code$code_1 <- paste0(
    "person <- function(){\n",
    "  tibble::tibble(\n",
    "    person_id = 1:100,\n",
    "    gender_concept_id = sample(c(8507L, 8532L), 100, replace = TRUE),\n",
    "    year_of_birth = sample(1920:2010, 100, replace = TRUE),\n",
    "    month_of_birth = sample(1:12, 100, replace = TRUE),\n",
    "    day_of_birth = sample(1:28, 100, replace = TRUE),\n",
    "    race_concept_id = NA_integer_,\n",
    "    ethnicity_concept_id = NA_integer_,\n",
    "    location_id = sample(1:10, 100, replace = TRUE),\n",
    "    provider_id = sample(1:10, 100, replace = TRUE),\n",
    "    care_site_id = sample(1:10, 100, replace = TRUE),\n",
    "    person_source_value = paste(\"Source\", 1:100),\n",
    "    gender_source_value = NA_character_,\n",
    "    gender_source_concept_id = NA_integer_,\n",
    "    race_source_value = NA_character_,\n",
    "    race_source_concept_id = NA_integer_,\n",
    "    ethnicity_source_value = NA_character_,\n",
    "    ethnicity_source_concept_id = NA_integer_\n",
    "  ) %>%\n",
    "  dplyr::mutate(\n",
    "      birth_datetime = lubridate::ymd_hms(paste0(paste(year_of_birth, month_of_birth, day_of_birth, sep = \"-\"), \" 00:00:00\")),\n",
    "      death_datetime = dplyr::case_when(runif(100) < 2/3 ~ as.POSIXct(NA), TRUE ~ birth_datetime + lubridate::years(sample(30:80, 100, replace = TRUE))),\n",
    "      .after = \"day_of_birth\"\n",
    "  )\n",
    "}\n",
    "\n",
    "import_dataset(\n",
    "  data = person(), omop_table = \"person\", omop_version = \"6.0\", read_with = \"none\", save_as = \"none\", rewrite = FALSE,\n",
    "  output = output, ns = ns, i18n = i18n, r = r, d = d, dataset_id = 5, \n",
    ")\n",
    "\n",
    "cat('\\n')\n",
    "d$person %>% nrow() # n = 100"
  )
  
  code$code_2 <- paste0(
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
  
  for (i in 1:2){
    div_code[[paste0("code_", i)]] <- div(
      class = "help_code",
      tags$pre(
        tags$code(
          class = "r code_highlight", 
          code[[paste0("code_", i)]]
        ),
        style = "border:none; background-color:inherit;"
      ),
      shiny.fluent::IconButton.shinyInput(ns(paste0("copy_code_", i)), iconProps = list(iconName = "Copy"), class = "help_copy_button")
    )
  }
  
  # --- --- -
  # Data ----
  # --- --- -
  
  # --- --- --- --- -- -
  ## Data structure ----
  # --- --- --- --- -- -
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_1")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("app_data_structure")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        tags$img(src = "https://raw.githubusercontent.com/BorisDelange/LinkR-content/main/home/fr/get_started/data_structure.svg", alt = "Data structure", width = "500", 
          style = "display:block; margin-left:auto; margin-right:auto;"), br(),
          "Dans LinkR, Les données sont ", tags$strong("structurées de la façon suivante"), " :", br(),
          tags$ul(
            tags$li(tags$strong("Set de données"), " : un set de données correspond à un ", tags$strong("ensemble de patients"), " avec leurs données, ",
              "cet ensemble correspond souvent aux ", tags$strong("critères d'inclusion"), " d'une étude (ex : tous les patients s'étant présentés aux urgences dans un hôpital sur une année)."),
            tags$li(tags$strong("Etudes"), " : il est possible de créer plusieurs études différentes depuis un même set de données. Une étude comprend toutes les données du set."),
            tags$li(tags$strong("Subsetes"), " : une étude peut être divisées en plusieurs subsets, correspondant à des sous-ensembles de patients (ex : subset des patients inclus pour cette étude, subset des patients exclus).")
          ),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
          tags$img(src = "https://raw.githubusercontent.com/BorisDelange/LinkR-content/main/home/fr/get_started/data_structure.svg", alt = "Data structure", width = "500", 
            style = "display:block; margin-left:auto; margin-right:auto;"), br(),
          "In LinkR, data are ", tags$strong("structured as follows"), ":", br(),
          tags$ul(
            tags$li(tags$strong("Dataset"), ": a dataset corresponds to a ", tags$strong("group of patients"), " with their data, ",
              "this group often corresponds to the ", tags$strong("inclusion criteria"), " of a study (e.g.: all patients presenting at the emergency department in a hospital over one year)."),
            tags$li(tags$strong("Studies"), ": it is possible to create several different studies from the same data set. A study includes all the data from the set."),
            tags$li(tags$strong("Subsets"), ": a study can be divided into multiple subsets, corresponding to subgroups of patients (e.g.: subset of patients included in this study, subset of patients excluded).")
          ),
          br()
      )
    }
  })
  
  # --- --- --- -- -
  ## Data model ----
  # --- --- --- -- -
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_9")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("data_model")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        tags$img(src = "https://raw.githubusercontent.com/BorisDelange/LinkR-content/main/home/fr/get_started/ohdsi_logo.png", alt = "Data structure", width = "350", 
          style = "display:block; margin-left:auto; margin-right:auto;"), br(),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Le modèle de données utilisé par l'application est le ", strong("modèle standard OMOP"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Il s'agit d'un ", tags$strong("modèle de données international"), ", standard, largement utilisé par la communauté scientifique médicale mondiale."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Ceci a l'avantage de rendre votre travail réalisé sur LinkR ", strong("interopérable"), " : votre travail pourra facilement être ",
          strong("réutilisé"), " par d'autres équipes, contribuant ainsi à l'", strong("open science"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Vous pouvez accéder aux ",
          tags$a(href = "https://ohdsi.github.io/CommonDataModel/cdm60.html", "détails du modèle de données ici", target = "_blank"), "."),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        tags$img(src = "https://raw.githubusercontent.com/BorisDelange/LinkR-content/main/home/fr/get_started/ohdsi_logo.png", alt = "OHDSI Logo", width = "350", 
          style = "display:block; margin-left:auto; margin-right:auto;"), br(),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "The data model used by the application is the ", strong("OMOP standard model"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "This is an ", tags$strong("international standard data model"), ", widely used by the global medical scientific community."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "This has the advantage of making your work on LinkR ", strong("interoperable"), ": your work can easily be ",
          strong("reused"), " by other teams, thus contributing to ", strong("open science"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "You can access the ",
          tags$a(href = "https://ohdsi.github.io/CommonDataModel/cdm60.html", "data model details here", target = "_blank"), "."),
        br()
      )
    }
  })
  
  # --- --- --- -
  # Datasets ----
  # --- --- --- -
  
  # --- --- --- --- --
  ## All datasets ----
  # --- --- --- --- --
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_10")]], {

    load_help_page(r)

    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("all_datasets")

    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-location-dot", style = "color: steelblue;"), " ", strong("Sets de données locaux")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Vous voyez ici ", strong("tous les sets de données"), " disponibles dans ", strong("votre instance"), " de LinkR."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Vous pouvez afficher la ", strong("description"), " d'un set de données en ", strong("cliquant"), " sur une ligne du tableau."),
        tags$h3(tags$i(class = "fa fa-globe", style = "color: steelblue;"), " ", strong("Sets de données distants")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Vous voyez ici ", strong("tous les sets de données"), " disponibles dans ", strong("vos dépôts git distants"), " enregistrés."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Les dépôts git distants sont des ", strong("dossiers partagés"), " hébergés sur des sites tels que ", tags$em("framagit.org"),
          " , ", tags$em("gitlab.com"), " et ", tags$em("github.com"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Vous pouvez ", strong("gérer vos dépôts git"), " dans la rubrique ", tags$em("Dépôts git distants"),
          " dans les ", strong("paramètres"), " de l'application."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "De la même façon que pour les sets de données locaux, vous pouvez accéder à la ", strong("description"),
          " des sets en ", strong("cliquant"), " sur une ligne du tableau."),
        br()
      )
    }

    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-location-dot", style = "color: steelblue;"), " ", strong("Local Datasets")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Here you see ", strong("all the datasets"), " available in ", strong("your instance"), " of LinkR."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "You can view a ", strong("description"), " of a dataset by ", strong("clicking"), " on a row in the table."),
        tags$h3(tags$i(class = "fa fa-globe", style = "color: steelblue;"), " ", strong("Remote Datasets")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Here you see ", strong("all the datasets"), " available in ", strong("your remote git repositories"), " registered."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Remote git repositories are ", strong("shared folders"), " hosted on sites such as ", tags$em("framagit.org"),
          ", ", tags$em("gitlab.com"), " and ", tags$em("github.com"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "You can ", strong("manage your git repositories"), " in the ", tags$em("Remote Git Repositories"),
          " section in the ", strong("settings"), " of the application."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "In the same way as for local datasets, you can access the ", strong("description"),
          " of the datasets by ", strong("clicking"), " on a row in the table."),
        br()
      )
    }
  })
  
  # --- --- --- --- --- --- -
  ## Datasets management ----
  # --- --- --- --- --- --- -
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_2")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("datasets_datatable_card")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ", strong("Ajouter un set de données")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Pour créer un set de données, entrez un nom puis cliquez sur ", tags$em("Ajouter"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Un set de données comprend ", strong("plusieurs études"), "."),
        p("Par exemple, vous ", strong("créez un set de données"), " nommé ", tags$em("Infections nosocomiales"), " à partir de la source de données ", tags$em("MIMIC-IV"), 
          ", qui comprendra tous les patients ayant développé une infection nosocomiale au cours d'une période définie."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          strong("Plusieurs études"), " seront créées à partir de ce set, par exemple :"),
        tags$ul(
          tags$li("une étude ", tags$em("Epidémiologie des infections nosocomiales")),
          tags$li("une étude ", tags$em("Incidence des infections nosocomiales à BMR"))
        ),
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ", strong("Gérer les sets de données")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Pour ", strong("modifier"), " les informations relatives à un set de données, cliquez sur le bouton ", 
          div(shiny::actionButton("delete_button_help", "", icon = icon("cog")), style = "display:inline-block"), " ."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Pour ", strong("supprimer"), " un ou plusieurs sets de données, sélectionnez-les en cliquant dessus dans le tableau puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
        p("Vous pouvez également supprimer un set de données en cliquant sur l'icône  ", 
          div(shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), style = "display:inline-block"), " ."),
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
  
  # --- --- --- --- --- ---
  ## Edit dataset code ----
  # --- --- --- --- --- ---
  
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
            strong(tags$a(href = "https://interhop.frama.io/linkr/linkr/reference/import_dataset.html", "import_dataset", target = "_blank")))
        ),
        p(tags$i(class = "fa fa-code", style = "color: steelblue;"), " ", 
          "La fonction ", tags$em(tags$a(href = "https://interhop.frama.io/linkr/linkr/reference/import_dataset.html", "import_dataset", target = "_blank")), 
          " comprend les arguments suivants :"),
        tags$ul(
          tags$li(tags$em("output, ns, i18n, r, d"), " : qui sont les variables permettant le fonctionnement de l'application"),
          tags$li(tags$em("dataset_id"), " : où vous indiquez ", strong("l'ID du dataset"), " actuel, via la balise ", tags$em("%dataset_id%")),
          tags$li(tags$em("data"), " : où vous indiquez la ", strong("fonction qui chargera les données"), " pour une variable (exemple : ", tags$em("person()"), ")"),
          tags$li(tags$em("omop_table"), " : où vous indiquez la ", strong("variable que vous souhaitez importer"), " (", tags$em("person, measurement"), "...)"),
          tags$li(tags$em("omop_version"), " : où vous indiquez la ", strong("version utilisées"), " du modèle de données OMOP"),
          tags$li(tags$em("read_with"), " : indiquez avec quelle ", strong("librairie R"), " vous voulez ", strong("lire les données"), " importées"),
          tags$li(tags$em("save_as"), " : indiquez sous quel ", strong("format"), " vous voulez enregistrer les données après les avoir importées"),
          tags$li(tags$em("rewrite"), " : indiquez si vous souhaitez écraser l'ancien fichier de données pour le remplacer par le nouveau"),
          tags$li(tags$em("allow_numeric_instead_integer"), " : indiquez si vous autorisez que les colonnes au format numérique puissent être laissées telles quelles plutôt que converties au format integer"),
          tags$li(tags$em("allow_dttm_instead_date"), " : indiquez si vous autorisez que les colonnes au format datetime puissent être laissées telles quelles plutôt que converties au format date")
        ),
        p("L'argument ", tags$em("rewrite"), " n'est utile que lorsque vous travaillez sur le code permettant d'importer le dataset. ",
          "En effet, inutile d'enregistrer une copie de vos données importées si ces données sont supprimées puis réécrites à chaque fois que vous chargez le set de données."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Le code que vous créez ici ", strong("s'éxécutera à chaque fois"), " que quelqu'un chargera le set de données,",
          " d'où le fait d'utiliser l'argument ", tags$em("save_as_csv"), " qui permettra d'économiser des ressources."),
        p("Voici un exemple de code :"),
        div_code$code_1,
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Cliquez sur ", tags$em("Sauvegarder"), " pour sauvegarder le code, sur ", tags$em("Exécuter"), " pour tester le code."),
        p("Un ", strong("tableau"), " en bas de la page vous indiquera le nombre de lignes chargées par variable."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Utilisez les ", strong("raccourcis"), " :",
          tags$ul(
            tags$li("CMD/CTRL + SHIFT + ENTER : exécute l'ensemble du code"),
            tags$li("CMD/CTRL + ENTER : exécute le code sélectionné"),
            tags$li("CMD/CTRL + SHIFT + C : commente le code sélectionné"),
            tags$li("CMD/CTRL + S : sauvegarde le code"),
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
            tags$em(tags$a(href = "https://interhop.frama.io/linkr/linkr/reference/import_dataset.html", "import_dataset", target = "_blank")))
        ),
        p(tags$i(class = "fa fa-code", style = "color: steelblue;"), " ", 
          "The function ", strong(tags$a(href = "https://interhop.frama.io/linkr/linkr/reference/import_dataset.html", "import_dataset", target = "_blank")), 
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
        div_code$code_1,
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
  
  # --- --- --- --- --- -
  ## Dataset options ----
  # --- --- --- --- --- -
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_4")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("dataset_options")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Version du set de données")),
        p("Dès lors que vous ", strong("modifiez"), " les options ou le code d'un set de données, ", strong("mettez à jour"), " la version,",
          " ceci permettra aux utilisateurs de ", strong("mettre à jour"), " leur copie de votre set de données, s'ils l'ont téléchargée depuis votre dépôt git."),
        tags$h3(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Auteur(s)")),
        p("Indiquez ", strong("qui a contribué"), " à la création de ce set (un ou plusieurs auteurs, séparés par des virgules)."),
        tags$h3(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Nom, catégorie et description")),
        p("En changeant la ", strong("langue"), " dans le menu déroulant, les champs ", tags$em("Nom"), ", ", tags$em("Catégorie") , " et ", tags$em("Description"),
          " vont changer."),
        p("Vous pouvez ainsi ", strong("modifier ces informations"), " en ", strong("plusieurs langues"), "."),
        p("Cela permet aux utilisateurs utlisant une langue différente sur leur instance de l'application d'avoir accès à ces données ", strong("dans leur langue.")),
        tags$h3(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Accès aux données agrégées uniquement")),
        p("Vous pouvez choisir de ne donner accès qu'aux ", strong("Données agrégées"), " d'un set de données."),
        p("Ainsi, les utilisateurs n'auront ", strong("pas accès aux données individuelles"), " de cet set, depuis la page ", tags$em("Données"), "."),
        tags$h3(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Version d'OMOP")),
        p("Sélectionnez la version d'OMOP dont est issu le set de données."),
        p("Voir la ", tags$a(href = "https://ohdsi.github.io/CommonDataModel/cdm60.html", "documentation OMOP", target = "_blank"),
          " pour plus d'informations."),
        tags$h3(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Accès aux données")),
        p("Choisissez qui a accès à ce set de données au sein de votre instance de l'application."),
        tags$h3(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Gestion des fichiers")),
        p("Vous pouvez ", strong("importer des fichiers"), ", et choisir de les supprimer si besoin."),
        p("Il peut être utile par exemple d'importer des fichiers de connexion, afin de récupérer des identifiants de connexion depuis le code du set de données."),
        p("L'emplacement du set de données sera récupéré avec la balise ", tags$em("%dataset_folder%"), " depuis le code du set de données."),
        p("Par exemple : ", tags$em("%dataset_folder%/connection.yml"), "."),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Dataset Version")),
        p("As soon as you ", strong("modify"), " the options or code of a dataset, ", strong("update"), " the version.",
          " This will allow users to ", strong("update"), " their copy of your dataset if they have downloaded it from your git repository."),
        tags$h3(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Author(s)")),
        p("Indicate ", strong("who contributed"), " to the creation of this dataset (one or multiple authors, separated by commas)."),
        tags$h3(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Name, Category and Description")),
        p("By changing the ", strong("language"), " in the dropdown menu, the fields ", tags$em("Name"), ", ", tags$em("Category"), " and ", tags$em("Description"),
          " will change."),
        p("You can thus ", strong("modify these details"), " in ", strong("several languages"), "."),
        p("This allows users who use a different language on their application instance to access these data ", strong("in their language.")),
        tags$h3(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Access to Aggregated Data Only")),
        p("You can choose to give access only to the ", strong("Aggregated Data"), " of a dataset."),
        p("Thus, users will ", strong("not have access to the individual data"), " of this dataset from the ", tags$em("Data"), " page."),
        tags$h3(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("OMOP Version")),
        p("Select the OMOP version from which the dataset originates."),
        p("See the ", tags$a(href = "https://ohdsi.github.io/CommonDataModel/cdm60.html", "OMOP documentation", target = "_blank"),
          " for more information."),
        tags$h3(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Data Access")),
        p("Choose who has access to this dataset within your application instance."),
        tags$h3(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("File Management")),
        p("You can ", strong("import files"), ", and choose to delete them if necessary."),
        p("It can be useful, for example, to import connection files in order to retrieve login credentials from the dataset's code."),
        p("The dataset's location will be retrieved with the tag ", tags$em("%dataset_folder%"), " from the dataset's code."),
        p("For example: ", tags$em("%dataset_folder%/connection.yml"), "."),
        br()
      )
      
    }
  })
  
  # --- --- --- --- --- --- --- --
  ## Import / export datasets ----
  # --- --- --- --- --- --- --- --
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_11")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("import_export_datasets")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-download", style = "color: steelblue;"), " ",  strong("Importer un set de données")),
        p("Vous pouvez importer un ou plusieurs set(s) de données ", strong("à partir d'un fichier ZIP"), ", crée depuis l'onglet ", tags$em("Exporter des sets"), "."),
        p("Si le set ded onnées existe déjà, il ne sera remplacé que si l'option ", tags$em("Remplacer les sets de données déjà existants"), " est cochée."),
        tags$h3(tags$i(class = "fa fa-upload", style = "color: steelblue;"), " ", strong("Exporter un set de données")),
        p("Vous pouvez exporter un ou plusieurs set(s) de données, ils seront téléchargés dans un fichier ZIP, que vous pouvez ", strong("partager"), " avec d'autres utilisateurs utilisant LinkR."),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-download", style = "color: steelblue;"), " ", strong("Import a plugin")),
        p("You can import one or several plugins ", strong("from a ZIP file"), ", created from the ", tags$em("Export a plugin"), " tab."),
        p("If the plugin already exists, it will only be replaced if the option ", tags$em("Replace existing plugins"), " is checked."),
        tags$h3(tags$i(class = "fa fa-upload", style = "color: steelblue;"), " ", strong("Export a plugin")),
        p("You can export one or several plugins, they will be downloaded in a ZIP file, which you can ", strong("share"), " with other LinkR users."),
        br()
      )
    }
  })
  
  # --- --- --- --- -
  # Vocabularies ----
  # --- --- --- --- -
  
  # --- --- --- --- --- --- --- -
  ## Vocabularies management ----
  # --- --- --- --- --- --- --- -
  
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
  
  # --- --- --- --- --- --- --
  ## Edit vocabulary code ----
  # --- --- --- --- --- --- --
  
  observeEvent(r[[paste0("help_settings_data_management_", prefix, "_page_6")]], {
    
    load_help_page(r)
    
    r[[paste0("help_settings_data_management_", prefix, "_modal_title")]] <- i18n$t("edit_vocabulary_code")
    
    if (language == "fr"){
      r[[paste0("help_settings_data_management_", prefix, "_modal_text")]] <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Dans cette rubrique, vous pouvez ", strong("écrire le code"), " qui permettra de charger les données d'une terminologie."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Pour charger une terminologie, ", strong("importez les données"), " avec la fonction ", 
          strong(tags$a(href = "https://interhop.frama.io/linkr/linkr/reference/import_vocabulary_table.html", "import_vocabulary_table", target = "_blank"))),
        p(tags$i(class = "fa fa-code", style = "color: steelblue;"), " ", 
          "La fonction comprend les arguments suivants :"),
        tags$ul(
          tags$li(tags$em("output, ns, i18n, r, m"), " : qui sont les variables permettant le fonctionnement de l'application"),
          tags$li(tags$em("table_name"), " : où vous indiquez le ", strong("nom de table"), " que vous souhaitez importer (concept, concept_relationship...)"),
          tags$li(tags$em("data"), " : où vous indiquez la ", strong("variable contenant les données")),
          tags$li(tags$em("vocabulary_id"), " : où vous indiquez ", strong("l'ID de la terminologie"), " actuelle, via la balise ", tags$em("%vocabulary_id%"))
        ),
        p("Voici un exemple de code :"),
        div_code$code_2,
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
          strong(tags$a(href = "https://interhop.frama.io/linkr/linkr/reference/import_vocabulary_table.html", "import_vocabulary_table", target = "_blank"))),
        p(tags$i(class = "fa fa-code", style = "color: steelblue;"), " ", 
          "The function includes the following arguments:"),
        tags$ul(
          tags$li(tags$em("output, ns, i18n, r, m"), " : which are the variables allowing the application to operate"),
          tags$li(tags$em("table_name"), " : where you indicate the ", strong("table name"), " you wish to import (concept, concept_relationship...)"),
          tags$li(tags$em("data"), " : where you indicate the ", strong("variable containing the data")),
          tags$li(tags$em("vocabulary_id"), " : where you indicate the ", strong("ID of the current vocabulary"), " via the tag ", tags$em("%vocabulary_id%"))
        ),
        p("Here is a code example:"),
        div_code$code_2,
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
  
  # --- --- --- --- --- --- -
  ## Vocabularies tables ----
  # --- --- --- --- --- --- -
  
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
  
  # --- --- --- --- --- --- -
  ## Import a vocabulary ----
  # --- --- --- --- --- --- -
  
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
  
  # Copy code divs
  
  observeEvent(r$help_settings_data_management_copy_code_1, clipr::write_clip(code$code_1))
  observeEvent(r$help_settings_data_management_copy_code_2, clipr::write_clip(code$code_2))
}
