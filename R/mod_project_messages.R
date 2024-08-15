#' @noRd
mod_project_messages_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  
  tagList()
}

#' @noRd 
mod_project_messages_server <- function(id, r, d, m, language, i18n, debug, user_accesses){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (debug) cat(paste0("\n", now(), " - mod_messages - start"))
    
  })
}