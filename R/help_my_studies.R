help_my_studies <- function(output, r = shiny::reactiveValues(), id = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r$help_my_studies_open_panel,
      br(),
      shiny.fluent::Link(i18n$t("studies_management"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("study_options"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      isLightDismiss = r$help_my_studies_open_panel_light_dismiss,
      isBlocking = r$help_my_studies_open_panel_light_dismiss,
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({
    
    shiny.fluent::Modal(
      isOpen = r$help_my_studies_open_modal, dragOptions = TRUE, isModeless = TRUE, topOffsetFixed = TRUE,
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
      div(
        style = "width: 1000px; padding: 15px 10px 0px 15px;",
        shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
          div(style = list(display = "flex"),
            shiny.fluent::Text(r$help_my_studies_modal_title, variant = "large"),
            div(style = list(flexGrow = 1)),
            shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
          ),
          r$help_my_studies_modal_text
        )
      )
    )
  })
  
  load_help_page <- function(r){
    r$help_my_studies_open_modal <- TRUE
    r$help_my_studies_open_panel_light_dismiss <- FALSE
  }
  
  # Code divs
  
  code_1 <- list()
  div_code_1 <- list()
  code_1$fr <- paste0(
    "## Description\n\n",
    "Cette étude vise à développer un modèle de prédiction de la mortalité.\n\n",
    "## Critères d'inclusion\n\n",
    "Tout patient ayant une durée de séjour supérieure à 24 heures."
  )
  div_code_1$fr <- div(
    span("## Description"), br(), br(),
    span("Cette étude vise à développer un modèle de prédiction de la mortalité."), br(), br(),
    span("## Critères d'inclusion"), br(), br(),
    span("Tout patient ayant une durée de séjour supérieure à 24 heures."),
    shiny.fluent::IconButton.shinyInput(ns("copy_code_4"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  code_1$en <- paste0(
    "## Description\n\n",
    "This study aims to develop a mortality prediction model.\n\n",
    "## Inclusion Criteria\n\n",
    "Any patient with a length of stay exceeding 24 hours."
  )
  div_code_1$en <- div(
    span("## Description"), br(), br(),
    span("This study aims to develop a mortality prediction model."), br(), br(),
    span("## Inclusion Criteria"), br(), br(),
    span("Any patient with a length of stay exceeding 24 hours."),
    shiny.fluent::IconButton.shinyInput(ns("copy_code_4"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  
  # Studies management
  
  observeEvent(r$help_my_studies_page_1, {
    
    load_help_page(r)
    
    r$help_my_studies_modal_title <- i18n$t("studies_management")
    
    if (language == "fr"){
      r$help_my_studies_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ", strong("Créer une étude")),
        p("Pour créer une étude, allez dans l'onglet ", tags$em("Gestion des études"), "."), 
        p("Choisissez un nom, faites-en sorte qu'il ne soit pas déjà utilisé, puis cliquez sur ", tags$em("Ajouter"), "."),
        tags$h3(tags$i(class = "fa fa-pen-to-square", style = "color: steelblue;"), " ", strong("Changer le nom d'une étude")),
        p("Pour changer le nom d'une étude, double-cliquez sur le nom, changez-le, faites-en sorte qu'il ne soit pas déjà utilisé, puis cliquez sur ", tags$em("Sauvegarder"), "."),
        tags$h3(tags$i(class = "fa fa-trash", style = "color: steelblue;"), " ", strong("Supprimer une ou des études")),
        p("Pour supprimer une ou plusieurs études, sélectionnez-les en cliquant dessus dans le tableau puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
        p("Vous pouvez également supprimer une étude en cliquant sur l'icône  ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), " ."),
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ", strong("Editer les options d'une étude")),
        p("Cliquez sur  ", shiny::actionButton("study_options_button_help", "", icon = icon("cog")), "  pour ", strong("éditer les options"), " de l'étude."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_my_studies_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ", strong("Create a study")),
        p("To create a study, go to the ", tags$em("Study Management"), " tab."),
        p("Choose a name, make sure it's not already in use, then click on ", tags$em("Add"), "."),
        tags$h3(tags$i(class = "fa fa-pen-to-square", style = "color: steelblue;"), " ", strong("Rename a study")),
        p("To rename a study, double-click on the name, change it, make sure it's not already in use, then click on ", tags$em("Save"), "."),
        tags$h3(tags$i(class = "fa fa-trash", style = "color: steelblue;"), " ", strong("Delete one or more studies")),
        p("To delete one or more studies, select them by clicking on them in the table, then click on ", tags$em("Delete selection"), "."),
        p("You can also delete a study by clicking on the  ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), "  icon."),
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ", strong("Edit study options")),
        p("Click on  ", shiny::actionButton("study_options_button_help", "", icon = icon("cog")), "  to ", strong("edit the options"), " of the study."),
        br()
      )
    }
  })
  
  # Study options
  
  observeEvent(r$help_my_studies_page_2, {
    
    load_help_page(r)
    
    r$help_my_studies_modal_title <- i18n$t("study_options")
    
    if (language == "fr"){
      r$help_my_studies_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-user", style = "color: steelblue;"), " ", strong("Auteur & version")),
        p("Le nom de l'auteur et la version de l'étude seront visibles depuis l'onglet ", tags$em("Toutes les études"), "."),
        p("Pensez à ", strong("modifier la version de l'étude"), " lorsque des modifications sont réalisées, afin de partager celle-ci."),
        tags$h3(tags$i(class = "fa fa-pen-to-square", style = "color: steelblue;"), " ", strong("Nom et catégorie")),
        p("Nom et catégorie s'affichant dans l'onglet ", tags$em("Toutes les études"), ", selon la langue choisie au démarrage de l'application."),
        tags$h3(tags$i(class = "fa fa-lock", style = "color: steelblue;"), " ", strong("Accès")),
        p("Choisissez ici qui peut avoir accès à cette étude."),
        tags$h3(tags$i(class = "fa fa-file-lines", style = "color: steelblue;"), " ", strong("Description")),
        p("La description s'affiche dans l'onglet ", tags$em("Toutes les études"), ", lorsque l'on clique sur une ligne du tableau."),
        p("Le code ici est du ", strong("Markdown"), ", dont voici un exemple."),
        div_code_1$fr,
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
      r$help_my_studies_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-user", style = "color: steelblue;"), " ", strong("Author & version")),
        p("The name of the author and the version of the study will be visible from the tab ", tags$em("All Studies"), "."),
        p("Remember to ", strong("modify the study version"), " when changes are made, in order to share it."),
        tags$h3(tags$i(class = "fa fa-pen-to-square", style = "color: steelblue;"), " ", strong("Name and Category")),
        p("Name and category are displayed in the tab ", tags$em("All Studies"), ", according to the language chosen at the start of the application."),
        tags$h3(tags$i(class = "fa fa-lock", style = "color: steelblue;"), " ", strong("Access")),
        p("Choose here who can have access to this study."),
        tags$h3(tags$i(class = "fa fa-file-lines", style = "color: steelblue;"), " ", strong("Description")),
        p("The description is displayed in the tab ", tags$em("All Studies"), ", when one clicks on a line of the table."),
        p("The code here is ", strong("Markdown"), ", here's an example."),
        div_code_1$en,
        p("Click on ", tags$em("Execute"), " to visualize the rendering of the description."),
        p("Use the ", strong("shortcuts"), ":"),
        tags$ul(
          tags$li("CMD/CTRL + SHIFT + ENTER : execute the whole code"),
          tags$li("CMD/CTRL + ENTER : execute the selected code"),
          tags$li("CMD/CTRL + S : save the code")
        ),
        br()
      )
    }
  })
  
  # Copy code divs
  
  observeEvent(r$help_my_studies_copy_code_1, clipr::write_clip(code_1[[language]]))
}