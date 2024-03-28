#' page_footer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_page_footer_ui <- function(i18n = character()){
  div(class = "footer", 
    tags$a(icon("gitlab"), "", href = "https://framagit.org/interhop/linkr/linkr", target = "_blank"),
    "Version 0.2.0.9086"
  )
}
 
