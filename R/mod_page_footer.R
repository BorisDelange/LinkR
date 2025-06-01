#' @noRd
mod_page_footer_ui <- function(id){
  
  pages_variables_list <- get("pages_variables_list", envir = parent.frame())
  for (obj_name in pages_variables_list) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  div(
    class = "footer",
    div(
      tags$a(icon("git-alt"), "", href = "https://framagit.org/interhop/linkr/linkr", style = "font-size: 18px;", target = "_blank"),
      div(
        tags$a("LinkR", href = paste0("https://linkr.interhop.org/", language, "/"), target = "_blank"), " | ",
        tags$a("InterHop", href = "https://interhop.org/", target = "_blank")
      ),
      div("v0.3.1.9009"),
      class = "footer_content"
    )
  )
}
 
