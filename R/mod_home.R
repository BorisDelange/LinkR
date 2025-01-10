#' @noRd
mod_home_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  
  if (id == "home") page <- "home" else page <- substr(id, 6, nchar(id))
  
  div(
    class = "main",
    div(
      tags$h1(i18n$t("projects"), style = "font-size: 14px; margin-bottom: 0;"),
      uiOutput(ns("projects")),
      style = "margin-bottom: 25px;"
    ),
    htmlTemplate(system.file("html_pages", paste0(language, "_home.html"), package = "linkr"))
  )
}

#' @noRd 
mod_home_server <- function(id, r, d, m, language, i18n, debug, user_accesses){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (debug) cat(paste0("\n", now(), " - mod_home - start"))
      
    if ("projects" %in% user_accesses){
      
      reload_elements_var(page_id = "home_projects", con = r$db, r = r, m = m, long_var_filtered = "filtered_projects_long", user_accesses)
      elements_ui <- create_elements_ui(page_id = "home_projects", elements = r$filtered_projects_long, r = r, language = language, i18n = i18n)
      output$projects <- renderUI(elements_ui)
    }
    
    observeEvent(input$reload_elements_list, {
      if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - observer input$reload_elements_list"))
      
    })
  })
}
