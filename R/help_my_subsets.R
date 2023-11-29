help_my_subsets <- function(output, r = shiny::reactiveValues(), id = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r$help_my_subsets_open_panel,
      br(),
      shiny.fluent::Link(i18n$t("subsets_management"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("edit_subset_code"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("subset_persons"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_3', Math.random()); }"))), br(), br(),
      isLightDismiss = r$help_my_subsets_open_panel_light_dismiss,
      isBlocking = r$help_my_subsets_open_panel_light_dismiss,
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({
    
    shiny.fluent::Modal(
      isOpen = r$help_my_subsets_open_modal, dragOptions = TRUE, isModeless = FALSE, topOffsetFixed = TRUE,
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
      div(
        style = "width: 1000px; padding: 15px 10px 0px 15px;",
        shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
          div(style = list(display = "flex"),
            shiny.fluent::Text(r$help_my_subsets_modal_title, variant = "large"),
            div(style = list(flexGrow = 1)),
            shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
          ),
          r$help_my_subsets_modal_text
        )
      )
    )
  })
  
  load_help_page <- function(r){
    r$help_my_subsets_open_modal <- TRUE
    r$help_my_subsets_open_panel_light_dismiss <- FALSE
  }
  
  # Code divs
  
  code_1 <- list()
  div_code_1 <- list()
  
  code_1$fr <- paste0(
    "# Sélection des patients depuis la variable d$person\n",
    "persons <- d$person %>% dplyr::select(person_id)\n\n",
    "# Ajout des patients au subset sélectionné dans le menu déroulant\n",
    "add_persons_to_subset(output = output, m = m, persons = persons, subset_id = %subset_id%, i18n = i18n, ns = ns)\n\n",
    "# Supprimer des patients du subset\n",
    "remove_persons_from_subset(output = output, m = m, persons = persons, subset_id = %subset_id%, i18n = i18n, ns = ns)"
  )
  div_code_1$fr <- div(
    span("# Sélection des patients depuis la variable d$person"), br(),
    span("persons <- d$person %>% dplyr::select(person_id)"), br(), br(),
    span("# Ajout des patients au subset sélectionné dans le menu déroulant"), br(),
    span("add_persons_to_subset(output = output, m = m, persons = persons, subset_id = %subset_id%, i18n = i18n, ns = ns)"), br(), br(),
    span("# Supprimer des patients du subset"), br(),
    span("remove_persons_from_subset(output = output, m = m, persons = persons, subset_id = %subset_id%, i18n = i18n, ns = ns)"),
    shiny.fluent::IconButton.shinyInput(ns("copy_code_1"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  
  code_1$en <- paste0(
    "# Select patients from the d$person variable\n",
    "persons <- d$person %>% dplyr::select(person_id)\n\n",
    "# Add patients to the subset selected in the dropdown menu\n",
    "add_persons_to_subset(output = output, m = m, persons = persons, subset_id = %subset_id%, i18n = i18n, ns = ns)\n\n",
    "# Remove patients from the subset\n",
    "remove_persons_from_subset(output = output, m = m, persons = persons, subset_id = %subset_id%, i18n = i18n, ns = ns)"
  )
  div_code_1$en <- div(
    span("# Select patients from the d$person variable"), br(),
    span("persons <- d$person %>% dplyr::select(person_id)"), br(), br(),
    span("# Add patients to the subset selected in the dropdown menu"), br(),
    span("add_persons_to_subset(output = output, m = m, persons = persons, subset_id = %subset_id%, i18n = i18n, ns = ns)"), br(), br(),
    span("# Remove patients from the subset"), br(),
    span("remove_persons_from_subset(output = output, m = m, persons = persons, subset_id = %subset_id%, i18n = i18n, ns = ns)"),
    shiny.fluent::IconButton.shinyInput(ns("copy_code_1"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  
  # Subsets management
  
  observeEvent(r$help_my_subsets_page_1, {
    
    load_help_page(r)
    
    r$help_my_subsets_modal_title <- i18n$t("subsets_management")
    
    if (language == "fr"){
      r$help_my_subsets_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ", strong("Créer un subset")),
        p("Pour créer un subset, allez dans l'onglet ", tags$em("Gestion des subsets"), "."), 
        p("Choisissez un nom, faites-en sorte qu'il ne soit pas déjà utilisé, puis cliquez sur ", tags$em("Ajouter"), "."),
        tags$h3(tags$i(class = "fa fa-pen-to-square", style = "color: steelblue;"), " ", strong("Changer le nom d'un subset")),
        p("Pour changer le nom d'un subset, double-cliquez sur le nom, changez-le, faites-en sorte qu'il ne soit pas déjà utilisé, puis cliquez sur ", tags$em("Sauvegarder"), "."),
        tags$h3(tags$i(class = "fa fa-trash", style = "color: steelblue;"), " ", strong("Supprimer un ou des subsets")),
        p("Pour supprimer un ou plusieurs subsets, sélectionnez-les en cliquant dessus dans le tableau puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
        p("Vous pouvez également supprimer un subset en cliquant sur l'icône  ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), " ."),
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ", strong("Editer le code d'un subset")),
        p("Cliquez sur :"),
        p(shiny::actionButton("edit_code_button_help", "", icon = icon("file-code")), "  pour ", strong("éditer le code"), " du subset,"),
        p(shiny::actionButton("subset_persons_button_help", "", icon = icon("table")), "  pour ", strong("gérer les patients"), " du subset."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_my_subsets_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ", strong("Create a subset")),
        p("To create a subset, go to the ", tags$em("Subset management"), " tab."),
        p("Choose a name that is not already used, then click on ", tags$em("Add"), "."),
        tags$h3(tags$i(class = "fa fa-pen-to-square", style = "color: steelblue;"), " ", strong("Rename a subset")),
        p("To rename a subset, double-click on its name, change it to a new name that is not already used, then click on ", tags$em("Save"), "."),
        tags$h3(tags$i(class = "fa fa-trash", style = "color: steelblue;"), " ", strong("Delete one or more subsets")),
        p("To delete one or more subsets, select them in the table by clicking on them, then click on ", tags$em("Delete selection"), "."),
        p("You can also delete a subset by clicking on the  ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), "  icon."),
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ", strong("Edit the code of a subset")),
        p("Click on"),
        p(shiny::actionButton("edit_code_button_help", "", icon = icon("file-code")), "  to ", strong("edit the code"), " of the subset,"),
        p(shiny::actionButton("subset_persons_button_help", "", icon = icon("table")), "  to ", strong("manage the patients"), " of the subset."),
        br()
      )
    }
  })
  
  # Edit subset code
  
  observeEvent(r$help_my_subsets_page_2, {
    
    load_help_page(r)
    
    r$help_my_subsets_modal_title <- i18n$t("edit_subset_code")
    
    if (language == "fr"){
      r$help_my_subsets_modal_text <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Vous pouvez ici ajouter ou supprimer des patients d'un subset en utilisant les fonctions :"),
        tags$ul(
          tags$li(strong(tags$a(href = "https://interhop.frama.io/linkr/linkr/reference/add_persons_to_subset.html", "add_persons_to_subset", target = "_blank")),
            " pour ajouter des patients à un subset"),
          tags$li(strong(tags$a(href = "https://interhop.frama.io/linkr/linkr/reference/remove_persons_from_subset.html", "remove_persons_from_subset", target = "_blank")),
            " pour retirer des patients d'un subset")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Ceci permet de sélectionner les patients de ", strong("façon plus ciblée"), " qu'avec le tableau listant tous les patients dans l'onglet ", tags$em("Patients du subset"), "."),
        p("Voici un exemple de code :"),
        div_code_1$fr,
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Utilisez la balise ", strong("%subset_id%"), ", qui sera remplacée par l'ID du subset sélectionné dans le menu déroulant."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_my_subsets_modal_text <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Here you can add or remove patients from a subset using the following functions:"),
        tags$ul(
          tags$li(strong(tags$a(href = "https://interhop.frama.io/linkr/linkr/reference/add_persons_to_subset.html", "add_persons_to_subset", target = "_blank")), 
            " to add patients to a subset"),
          tags$li(strong(tags$a(href = "https://interhop.frama.io/linkr/linkr/reference/remove_persons_from_subset.html", "remove_persons_from_subset", target = "_blank")), " to remove patients from a subset")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "This allows you to select patients in a ", strong("more targeted way"), " than with the table listing all patients in the ", tags$em("Subset patients"), " tab."),
        p("Here is an example of code:"),
        div_code_1$en,
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Use the tag ", strong("%subset_id%"), ", which will be replaced by the ID of the selected subset in the dropdown menu."),
        br()
      )
    }
  })
  
  # Subset patients
  
  observeEvent(r$help_my_subsets_page_3, {
    
    load_help_page(r)
    
    r$help_my_subsets_modal_title <- i18n$t("subset_persons")
    
    if (language == "fr"){
      r$help_my_subsets_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ", strong("Ajouter des patients au subset")),
        p("Pour ajouter des patients au subset sélectionné dans le menu déroulant, ", strong("sélectionnez les patients dans le tableau du bas"),
          " puis cliquez sur ", tags$em("Ajouter"), "."),
        tags$h3(tags$i(class = "fa fa-trash", style = "color: steelblue;"), " ", strong("Supprimer des patients du subset")),
        p("De la même façon, ", strong("sélectionnez les patients dans le tableau du haut"), 
          " puis cliquez sur ", tags$em("Supprimer la sélection"), " pour retirer les patients du subset."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_my_subsets_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ", strong("Add patients to subset")),
        p("To add patients to the selected subset in the dropdown menu, ", strong("select the patients in the bottom table"),
          " and then click on ", tags$em("Add"), "."),
        tags$h3(tags$i(class = "fa fa-trash", style = "color: steelblue;"), " ", strong("Remove patients from subset")),
        p("Similarly, ", strong("select the patients in the top table"),
          " and then click on ", tags$em("Delete selection"), " to remove the patients from the subset."),
        br()
      )
    }
  })
  
  # Copy code divs
  
  observeEvent(r$help_my_subsets_copy_code_1, clipr::write_clip(code_1[[language]]))
}
