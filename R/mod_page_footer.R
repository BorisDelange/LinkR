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
    shiny.fluent::Stack(
      horizontal = TRUE,
      horizontalAlign = 'space-between',
      tokens = list(childrenGap = 20),
      tags$a(icon("gitlab"), "", href = "https://framagit.org/interhop/linkr/linkr", target="_blank"),
      shiny.fluent::Text(variant = "medium", nowrap = FALSE, ""),
      shiny.fluent::Text(variant = "medium", nowrap = FALSE, "Version 0.2.0.9030")
    )
  )
}
 
