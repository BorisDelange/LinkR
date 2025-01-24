#' @noRd
mod_home_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  
  if (id == "home") page <- "home" else page <- substr(id, 6, nchar(id))
  
  div(
    class = "main",
    div(
      class = "home-container",
      div(
        class = "home-section",
        tags$h1(tags$a(href = shiny.router::route_link("projects"), i18n$t("projects")), class = "home-section-title"),
        uiOutput(ns("projects"))
      ),
      div(
        class = "home-section",
        tags$h1(tags$a(href = shiny.router::route_link("datasets"), i18n$t("datasets")), class = "home-section-title"),
        uiOutput(ns("datasets"))
      ),
      div(
        class = "home-section",
        tags$h1(tags$a(href = shiny.router::route_link("plugins"), i18n$t("plugins")), class = "home-section-title"),
        uiOutput(ns("plugins"))
      ),
      htmlTemplate(system.file("html_pages", paste0(language, "_home.html"), package = "linkr"))
    )
  )
}

#' @noRd 
mod_home_server <- function(id, r, d, m, language, i18n, debug, user_accesses){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (debug) cat(paste0("\n", now(), " - mod_home - start"))
    
    sapply(c("projects", "datasets", "plugins"), function(page){
      
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
        
        observeEvent(r[[paste0("reload_home_", page)]], {
          reload_elements_var(page_id = "home", id = page, con = r$db, r = r, m = m, long_var_filtered = paste0("filtered_", page, "_long"), user_accesses)
          elements_ui <- create_elements_ui(page_id = "home", id = page, elements = r[[paste0("filtered_", page, "_long")]], r = r, language = language, i18n = i18n)
          output[[page]] <- renderUI(elements_ui)
        })
      }
      
      r[[paste0("reload_home_", page)]] <-
      
      observeEvent(input[[paste0("selected_", single_id, "_trigger")]], {
        if (debug) cat(paste0("\n", now(), " - mod_home - observer input$selected_", single_id, "_trigger"))
        
        shiny.router::change_page(page)
        shinyjs::runjs(paste0("Shiny.setInputValue('", page, "-selected_element', ", input[[paste0("selected_", single_id)]], ");"))
        shinyjs::delay(500, shinyjs::runjs(paste0("Shiny.setInputValue('", page, "-selected_element_trigger', Math.random());")))
      })
    })
    
    shinyjs::delay(10, shinyjs::runjs("
      document.querySelectorAll('.home-section').forEach(section => {
        updateNavButtons(section);
      });
    "))
  })
}
