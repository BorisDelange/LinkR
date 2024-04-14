#' @noRd 
mod_project_console_ui <- function(id = character(), language = "en", languages = tibble::tibble(), i18n = character()){
  ns <- NS(id)
  
  div()
}

#' @noRd 
mod_project_console_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), language = "en", i18n = character(), debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
  })
}