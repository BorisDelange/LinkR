help_settings_log <- function(output, r = shiny::reactiveValues(), id = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r$help_settings_log_open_panel,
      br(),
      shiny.fluent::Link(i18n$t("access_log"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      isLightDismiss = r$help_settings_log_open_panel_light_dismiss,
      isBlocking = r$help_settings_log_open_panel_light_dismiss,
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({
    
    shiny.fluent::Modal(
      isOpen = r$help_settings_log_open_modal, dragOptions = TRUE, isModeless = FALSE, topOffsetFixed = TRUE,
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
      div(
        style = "width: 1000px; padding: 15px 10px 0px 15px;",
        shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
          div(style = list(display = "flex"),
            shiny.fluent::Text(r$help_settings_log_modal_title, variant = "large"),
            div(style = list(flexGrow = 1)),
            shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
          ),
          r$help_settings_log_modal_text
        )
      )
    )
  })
  
  load_help_page <- function(r){
    r$help_settings_log_open_modal <- TRUE
    r$help_settings_log_open_panel_light_dismiss <- FALSE
  }
  
  # Access log
  
  observeEvent(r$help_settings_log_page_1, {
    
    load_help_page(r)
    
    r$help_settings_log_modal_title <- i18n$t("access_log")
    
    if (language == "fr"){
      r$help_settings_log_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-user", style = "color: steelblue;"), " ", strong("Accéder à son log")),
        p("Accéder à votre log en cliquant sur ", tags$em("Accéder uniquement à son log"), ", puis sur ", tags$em("Recharger le log"), "."),
        p("Lorsque vous cliquez sur une ligne, le ", strong("détail du log s'affiche"), " en bas de la page."),
        p("Le log est utile notamment ", strong("en cas de bug de l'application"), ", afin d'identifier plus précisément le problème."),
        tags$h3(tags$i(class = "fa fa-users", style = "color: steelblue;"), " ", strong("Accéder au log des utilisateurs")),
        p("Si vous avez les droits nécessaires pour, accédez au log des autres utilisateurs en cliquant sur ", tags$em("Choisir les utilisateurs"), 
          ", en sélectionnant les utilisateurs puis cliquez sur ", tags$em("Recharger le log"), "."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_settings_log_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-user", style = "color: steelblue;"), " ", strong("Access your log")),
        p("Access your log by clicking on ", tags$em("Access to current account log only"), ", then on ", tags$em("Reload log"), "."),
        p("When you click on a line, the ", strong("log detail appears"), " at the bottom of the page."),
        p("The log is useful especially in case of ", strong("application bug"), " to more precisely identify the bug."),
        tags$h3(tags$i(class = "fa fa-users", style = "color: steelblue;"), " ", strong("Access users log")),
        p("If you have the necessary rights, access other users' logs by clicking on ", tags$em("Select users"), 
          ", select the users then click on ", tags$em("Reload log"), "."),
        br()
      )
    }
  })
}
