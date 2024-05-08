#' @noRd 
mod_explore_ui <- function(id = character(), language = "en", languages = tibble::tibble(), i18n = character()){
  ns <- NS(id)
  
  uiOutput(ns("explore"))
}

#' @noRd 
mod_explore_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), language = "en", i18n = character(), debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    r$reload_explore <- now()
    
    observeEvent(r$reload_explore, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer r$reload_explore"))
      
      output$explore <- renderUI("")
      shinyjs::show("explore")
    })
  })
}