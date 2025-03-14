#' @noRd
mod_page_footer_ui <- function(i18n, language){
  div(
    class = "footer",
    div(
      tags$a(icon("git-alt"), "", href = "https://framagit.org/interhop/linkr/linkr", style = "font-size: 18px;", target = "_blank"),
      div(
        tags$a("LinkR", href = paste0("https://linkr.interhop.org/", language, "/"), target = "_blank"), " | ",
        tags$a("InterHop", href = "https://interhop.org/", target = "_blank"), " | 2019 - 2024"
      ),
      div("v0.3.1.9003"),
      class = "footer_content"
    )
  )
}
 
