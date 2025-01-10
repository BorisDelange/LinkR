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
    div(
      tags$h1(i18n$t("datasets"), style = "font-size: 14px; margin-bottom: 0;"),
      uiOutput(ns("datasets")),
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
    
    sapply(c("projects", "datasets"), function(page){
      
      single_id <- switch(
        page, 
        "data_cleaning" = "data_cleaning", 
        "datasets" = "dataset",
        "projects" = "project", 
        "plugins" = "plugin", 
        "subsets" = "subset", 
        "vocabularies" = "vocabulary"
      )
      
      if (page %in% user_accesses){
        
        reload_elements_var(page_id = "home", id = page, con = r$db, r = r, m = m, long_var_filtered = paste0("filtered_", page, "_long"), user_accesses)
        elements_ui <- create_elements_ui(page_id = "home", id = page, elements = r[[paste0("filtered_", page, "_long")]], r = r, language = language, i18n = i18n)
        output[[page]] <- renderUI(elements_ui)
      }
      
      observeEvent(input[[paste0("selected_", single_id, "_trigger")]], {
        if (debug) cat(paste0("\n", now(), " - mod_home - observer input$selected_", single_id, "_trigger"))
        
        shiny.router::change_page(page)
        shinyjs::runjs(paste0("Shiny.setInputValue('", page, "-selected_element', ", input[[paste0("selected_", single_id)]], ");"))
        shinyjs::delay(500, shinyjs::runjs(paste0("Shiny.setInputValue('", page, "-selected_element_trigger', Math.random());")))
      })
    })
  })
}
