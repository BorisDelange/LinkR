#' @noRd
mod_page_footer_ui <- function(i18n = character()){
  div(
    class = "footer",
    div(
      tags$a(icon("gitlab"), "", href = "https://framagit.org/interhop/linkr/linkr", target = "_blank"),
      "v0.2.0.9086",
      class = "footer_content"
    )
  )
}
 
