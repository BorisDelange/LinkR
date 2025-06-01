#' @noRd 
mod_log_ui <- function(id){
  
  pages_variables_list <- get("pages_variables_list", envir = parent.frame())
  for (obj_name in pages_variables_list) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  ns <- NS(id)
  
  div(
    id = ns("main"),
    class = "main",
    style = "padding: 10px;",
    div(
      id = ns("user_log_forbidden_access"),
      shiny.fluent::MessageBar(i18n$t("unauthorized_access_area"), messageBarType = 5),
      style = "display: inline-block; margin: 5px;"
    ),
    shinyjs::hidden(
      div(
        id = ns("user_log_div"),
        uiOutput(ns("log"))
      )
    )
  )
}
    
#' @noRd 
mod_log_server <- function(id){
  
  pages_variables_list <- get("pages_variables_list", envir = parent.frame())
  for (obj_name in pages_variables_list) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Current user accesses ----
    
    if ("log_user_log" %in% user_accesses){
      shinyjs::hide("user_log_forbidden_access")
      sapply(c("user_log_div", "user_log_buttons"), shinyjs::show)
    }
    
    # User log ----
    
    log_file <- paste0(r$app_folder, "/log/", r$user_id, ".txt")
    
    # Create log file if doesn't exist
    if (!file.exists(log_file)) file.create(log_file)
    
    observe_event(shiny.router::get_page(), if (shiny.router::get_page() == "log") shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-refresh_log', Math.random());")))
    
    observe_event(input$refresh_log, {
      
      if ("log_user_log" %not_in% user_accesses) return()
      
      log <-
        readLines(log_file, warn = FALSE) %>% 
        gsub("^(.*?\\- )(.*?)( \\- .*)$", "\\1<span style='color: #015bb5; font-weight: 600;'>\\2</span>\\3", .) %>%
        gsub("Error", "<span style='color: #f44336; font-weight: 600;'></span>", .) %>%
        # rev() %>% 
        paste(collapse = "<br />")
        
      output$log <- renderUI(div(HTML(log), style = "font-weight: 500; font-family: monospace;"))
      
      # Scroll to the bottom of the page
      shinyjs::delay(100,
        shinyjs::runjs("
          const element = document.getElementById('log-main');
          element.scrollTop = element.scrollHeight;")
      )
    })
    
    observe_event(input$reset_log, {
      
      if ("log_user_log" %not_in% user_accesses) return()
      
      file.remove(log_file)
      file.create(log_file)
      sink(log_file, append = TRUE)
      
      output$log <- renderUI("")
    })
  })
}
