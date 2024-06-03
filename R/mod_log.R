#' @noRd 
mod_log_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  
  div(
    class = "main",
    style = "padding: 10px;",
    uiOutput(ns("log"))
  )
}
    
#' @noRd 
mod_log_server <- function(id, r, d, m, language, i18n, debug){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    log_file <- paste0(r$app_folder, "/log/", r$user_id, ".txt")
    
    # Create log file if doesn't exist
    if (!file.exists(log_file)) file.create(log_file)
    
    observeEvent(shiny.router::get_page(), {
      req(shiny.router::get_page() == "log")
      if (debug) cat(paste0("\n", now(), " - mod_log - observer shiny.router::get_page()"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-refresh_log', Math.random());"))
    })
    
    observeEvent(input$refresh_log, {
      if (debug) cat(paste0("\n", now(), " - mod_log - observer input$refresh_log"))
      
      log <-
        readLines(log_file, warn = FALSE) %>% 
        gsub("^(.*?\\- )(.*?)( \\- .*)$", "\\1<span style='color: #015bb5; font-weight: 600;'>\\2</span>\\3", .) %>%
        gsub("Error", "<span style='color: #f44336; font-weight: 600;'></span>", .) %>%
        rev() %>% 
        paste(collapse = "<br />")
        
      output$log <- renderUI(div(HTML(log), style = "font-weight: 500; font-family: monospace;"))
    })
    
    observeEvent(input$reset_log, {
      if (debug) cat(paste0("\n", now(), " - mod_log - observer input$reset_log"))
      
      file.remove(log_file)
      file.create(log_file)
      sink(log_file, append = TRUE)
      
      output$log <- renderUI("")
    })
  })
}
