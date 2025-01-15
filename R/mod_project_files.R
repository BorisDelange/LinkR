#' @noRd 
mod_project_files_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  
  div()
}

#' @noRd 
mod_project_files_server <- function(id, r, d, m, language, i18n, debug, user_accesses){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Reload project files ----
    
    observeEvent(m$selected_project, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer m$selected_project"))
      
      req(!is.na(m$selected_project))
      
      project_folder <- file.path(r$app_folder, "projects_files", m$selected_project)
      
      if (!dir.exists(project_folder)) dir.create(project_folder, recursive = TRUE)
      else {
        file_names <- list.files(project_folder, full.names = FALSE)
        
        r$project_files_list <-
          tibble::tibble(id = 1:length(file_names), filename = file_names) %>%
          dplyr::arrange(filename) %>%
          dplyr::mutate(short_filename = ifelse(nchar(filename) >= 23, paste0(substr(filename, 1, 20), "..."), filename))
      }
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_files_browser', Math.random());"))
    })
    
    ## Reload files browser ----
    
    observeEvent(input$reload_files_browser, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$reload_files_browser"))
      
      files_ui <- load_files_browser_ui(id, r$project_files_list, "")

      output$files_browser <- renderUI(files_ui)
    })
  })
}