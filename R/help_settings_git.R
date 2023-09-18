help_settings_git <- function(output, r = shiny::reactiveValues(), id = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r$help_settings_git_open_panel,
      br(),
      shiny.fluent::Link(i18n$t("remote_git_repos"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      isLightDismiss = r$help_settings_git_open_panel_light_dismiss,
      isBlocking = r$help_settings_git_open_panel_light_dismiss,
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({
    
    shiny.fluent::Modal(
      isOpen = r$help_settings_git_open_modal, dragOptions = TRUE, isModeless = FALSE, topOffsetFixed = TRUE,
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
      div(
        style = "width: 1000px; padding: 15px 10px 0px 15px;",
        shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
          div(style = list(display = "flex"),
            shiny.fluent::Text(r$help_settings_git_modal_title, variant = "large"),
            div(style = list(flexGrow = 1)),
            shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
          ),
          r$help_settings_git_modal_text
        )
      )
    )
  })
  
  load_help_page <- function(r){
    r$help_settings_git_open_modal <- TRUE
    r$help_settings_git_open_panel_light_dismiss <- FALSE
  }
  
  # Git repos
  
  observeEvent(r$help_settings_git_page_1, {
    
    load_help_page(r)
    
    r$help_settings_git_modal_title <- i18n$t("remote_git_repos")
    
    if (language == "fr"){
      r$help_settings_git_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-question", style = "color: steelblue;"), " ", strong("A quoi servent les dépôts git ?")),
        p("Les ", strong("dépôts git"), " sont des dossiers hébergés par des sites tels que ", tags$em("gitlab.com"), ", ", tags$em("github.com"), " et ", tags$em("framagit.org"),
          " permettant le partage de code."),
        p("Vous pouvez ", strong("rendre accessibles"), " les codes de vos ", strong("études"), ", ", strong("scripts"), " et ", strong("plugins"),
          " en les hébergeant sur un dépôt git."),
        p("Il vous suffira alors de ", strong("partager l'URL"), " de votre dépôt git pour partager le code."),
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ", strong("Ajouter un dépôt git")),
        p("Pour ajouter un dépôt git, allez dans l'onglet ", tags$em("Ajouter un dépôt git"), " puis :"),
        tags$ul(
          tags$li("Choisissez un ", strong("nom"), " qui n'est pas encore utilisé dans la catégorie choisie"),
          tags$li("Choisissez la ", strong("catégorie"), " de votre dépôt git : dossier de partage d'études, de plugins ou de scripts"),
          tags$li("Renseignez l'", strong("adresse URL"), " de votre dépôt git")
        ),
        p("Attention à donner l'adresse URL de ", strong("type Raw"), "."),
        p("Pour cela, allez sur votre dépôt git, cliquez sur un fichier, cliquez sur Raw."),
        p("Copiez l'adresse URL, par exemple : ", tags$em("https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/plugins")),
        tags$h3(tags$i(class = "fa fa-trash", style = "color: steelblue;"), " ", strong("Supprimer un ou des dépôts git")),
        p("Pour supprimer un ou plusieurs dépôt git, sélectionnez-les en cliquant dessus dans le tableau puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
        p("Vous pouvez également supprimer un dépôt git en cliquant sur l'icône  ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), " ."),
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ", strong("Editer un dépôt git")),
        p("Vous pouvez modifier l'", strong("adresse URL"), " et le ", strong("nom"), " d'un dépôt git en double-cliquant sur la case concernée dans le tableau."),
        p("Enregistrez les modifications en cliquant sur ", tags$em("Sauvegarder"), "."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_settings_git_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-question", style = "color: steelblue;"), " ", strong("What is the purpose of git repositories?")),
        p("The ", strong("git repositories"), " are folders hosted by sites such as ", tags$em("gitlab.com"), ", ", tags$em("github.com"), " and ", tags$em("framagit.org"),
          " allowing code sharing."),
        p("You can ", strong("make accessible"), " the codes of your ", strong("studies"), ", ", strong("scripts"), " and ", strong("plugins"),
          " by hosting them on a git repository."),
        p("You will then simply need to ", strong("share the URL"), " of your git repository to share the code."),
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ", strong("Add a git repository")),
        p("To add a git repository, go to the tab ", tags$em("Add a git repository"), " then:"),
        tags$ul(
          tags$li("Choose a ", strong("name"), " that is not yet used in the chosen category"),
          tags$li("Choose the ", strong("category"), " of your git repository: sharing folder for studies, plugins or scripts"),
          tags$li("Fill in the ", strong("URL address"), " of your git repository")
        ),
        p("Be careful to give the URL address of ", strong("Raw type"), "."),
        p("To do this, go to your git repository, click on a file, click on Raw."),
        p("Copy the URL address, for example: ", tags$em("https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/plugins")),
        tags$h3(tags$i(class = "fa fa-trash", style = "color: steelblue;"), " ", strong("Delete one or more git repositories")),
        p("To delete one or more git repositories, select them by clicking on them in the table and then click on ", tags$em("Delete selection"), "."),
        p("You can also delete a git repository by clicking on the icon  ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), " ."),
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ", strong("Edit a git repository")),
        p("You can modify the ", strong("URL address"), " and the ", strong("name"), " of a git repository by double-clicking on the relevant box in the table."),
        p("Save the changes by clicking on ", tags$em("Save"), "."),
        br()
      )
    }
  })
}
