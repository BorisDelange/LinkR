help_my_subsets <- function(output, r = shiny::reactiveValues(), id = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r$help_my_subsets_open_panel,
      br(),
      strong("Category 1"), br(), br(),
      shiny.fluent::Link("Link 1", onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      shiny.fluent::Link("Link 2", onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      strong("Category 2"), br(), br(),
      shiny.fluent::Link("Link 3", onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_3', Math.random()); }"))), br(), br(),
      isLightDismiss = r$help_my_subsets_open_panel_light_dismiss,
      isBlocking = r$help_my_subsets_open_panel_light_dismiss,
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({
    
    shiny.fluent::Modal(
      isOpen = r$help_my_subsets_open_modal, dragOptions = TRUE, isModeless = TRUE, topOffsetFixed = TRUE,
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
  
  observeEvent(r$help_my_subsets_page_1, {
    
    load_help_page(r)
    
    r$help_my_subsets_modal_title <- "Help page 1"
    r$help_my_subsets_modal_text <- div(
      p("Paragraph 1")
    )
  })
  
  observeEvent(r$help_my_subsets_page_2, {
    
    load_help_page(r)
    
    r$help_my_subsets_modal_title <- "Help page 2"
    r$help_my_subsets_modal_text <- div(
      p("Paragraph 2")
    )
  })
}