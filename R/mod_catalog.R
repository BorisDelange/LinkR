#' @noRd 
mod_catalog_ui <- function(id = character(), language = "en", languages = tibble::tibble(), i18n = character()){
  ns <- NS(id)
  
  uiOutput(ns("catalog"))
}

#' @noRd 
mod_catalog_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), language = "en", i18n = character(), debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    r$reload_catalog <- now()
    
    observeEvent(r$reload_catalog, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer r$reload_catalog"))
      
      output$catalog <- renderUI("")
      shinyjs::show("catalog")
    })
  })
}