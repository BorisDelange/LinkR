#' @noRd
mod_page_footer_ui <- function(i18n){
  div(
    class = "footer",
    div(
      tags$a(icon("gitlab"), "", href = "https://framagit.org/interhop/linkr/linkr", target = "_blank"),
      "v0.3.0.9016",
      class = "footer_content"
    )
  )
}
 
