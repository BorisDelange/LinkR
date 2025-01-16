#' @noRd 
mod_project_files_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  
  div(
    class = "main",
    
    div(
      shinyjs::hidden(
        div(
          id = ns("forbidden_access"),
          shiny.fluent::MessageBar(i18n$t("unauthorized_access_area"), messageBarType = 5),
          style = "display: inline-block; margin-top: 15px;"
        )
      ),
      div(id = ns("tabs_div"), uiOutput(ns("tabs_ui"))),
      div(id = ns("editors_div"), style = "height: calc(100% - 45px);"),
      style = "height: 100%;"
    ),
    
    # Delete a file modal ----
    
    shinyjs::hidden(
      div(
        id = ns("delete_file_modal"),
        div(
          tags$h1(i18n$t("delete_file_title")), tags$p(i18n$t("delete_file_text")),
          div(
            shiny.fluent::DefaultButton.shinyInput(ns("close_file_deletion_modal"), i18n$t("dont_delete")),
            div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_file_deletion"), i18n$t("delete")), class = "delete_button"),
            class = "delete_modal_buttons"
          ),
          class = "delete_modal_content"
        ),
        class = "delete_modal"
      )
    )
  )
}

#' @noRd 
mod_project_files_server <- function(id, r, d, m, language, i18n, debug, user_accesses, user_settings){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Initiate vars ----
    
    r$project_files_list <- tibble::tibble(id = integer(), project_id = integer(), filename = character())
    r$project_tabs <- tibble::tibble(id = integer(), project_id = integer(), filename = character(), position = integer())
    r$project_deleted_files_id <- c()
    r$project_editors <- tibble::tibble(id = integer(), project_id = integer(), filename = character())
    
    code_hotkeys <- list(
      save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
      run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
      run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
      comment = list(win = "CTRL-SHIFT-C", mac = "CTRL-SHIFT-C|CMD-SHIFT-C")
    )
    
    # Load project files ----
    
    observeEvent(m$selected_project, {
      if (debug) cat(paste0("\n", now(), " - mod_project_files - observer m$selected_project"))
      
      # req("projects_edit_code" %in% user_accesses)
      req(!is.na(m$selected_project))
      
      # Unique ID
      selected_project_unique_id <- r$projects_long %>% dplyr::filter(id == m$selected_project, name == "unique_id") %>% dplyr::pull(value)
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-selected_project_unique_id', '", selected_project_unique_id, "');"))
      
      project_folder <- file.path(r$app_folder, "projects_files", selected_project_unique_id)
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-selected_project_folder', '", project_folder, "');"))
      
      if (!dir.exists(project_folder)) dir.create(project_folder, recursive = TRUE)
      else {
        file_names <- list.files(project_folder, full.names = FALSE)
        project_files <- r$project_files_list %>% dplyr::filter(project_id == m$selected_project) %>% dplyr::pull(filename)
        file_names <- setdiff(file_names, project_files)
        
        if (length(file_names) > 0){
          
          if (nrow(r$project_files_list) > 0) max_id <- max(r$project_files_list$id)
          else max_id <- 0
          
          r$project_files_list <-
            r$project_files_list %>%
            dplyr::bind_rows(tibble::tibble(id = max_id + 1:length(file_names), project_id = m$selected_project, filename = file_names)) %>%
            dplyr::arrange(filename)
        }
      }
      
      # Reload files browser + tabs
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_files_browser', Math.random());"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_files_tab', '", now(format = "%Y-%m-%d %H:%M:%OS3"), "');"))
    })
    
    # Reload files browser ----
    
    observeEvent(input$reload_files_browser, {
      if (debug) cat(paste0("\n", now(), " - mod_project_files - observer input$reload_files_browser"))
      
      project_files_list <- r$project_files_list %>% dplyr::filter(project_id == m$selected_project)
      files_ui <- load_files_browser_ui(id = id, input_prefix = "", files_list = project_files_list)

      output$files_browser <- renderUI(files_ui)
    })
    
    # Reload tabs ----
    
    observeEvent(input$reload_files_tab, {
      if (debug) cat(paste0("\n", now(), " - mod_project_files - observer input$reload_files_tab"))
      
      # req("plugins_edit_code" %in% user_accesses)
      
      tabs_container <- reload_files_browser_tabs(
        id = id, input_prefix = "", r = r, r_prefix = "project",
        element_id = m$selected_project, file_id = input$selected_file
      )
      
      output$tabs_ui <- renderUI(tabs_container)
      
      shinyjs::delay(500, shinyjs::runjs(sprintf("initSortableTabs('%s')", ns("tabs"))))
    })
    
    observeEvent(input$tab_positions, {
      if (debug) cat(paste0("\n", now(), " - mod_project_files - observer input$tab_positions"))
      
      files_browser_edit_tab_positions(positions = input$tab_positions, r = r, r_prefix = "project", element_id = m$selected_project)
    })
    
    # Change file tab ----
    
    observeEvent(input$selected_tab, {
      if (debug) cat(paste0("\n", now(), " - mod_project_files - observer input$selected_tab"))
      
      files_browser_change_tab(id = id, input_prefix = "", r = r, r_prefix = "project", file_id = input$selected_tab)
    })
    
    # Open a file ----
    
    observeEvent(input$selected_file, {
      if (debug) cat(paste0("\n", now(), " - mod_project_files - observer input$selected_file"))
      
      # req("plugins_edit_code" %in% user_accesses)
      
      files_browser_open_file(
        id = id, input_prefix = "", r = r, r_prefix = "project", folder = input$selected_project_folder,
        element_id = m$selected_project, file_id = input$selected_file, code_hotkeys = code_hotkeys, user_settings = user_settings
      )
    })
    
    # Close a file ----
    
    observeEvent(input$close_selected_tab_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_project_files - observer input$close_selected_tab_trigger"))
      
      files_browser_close_file(
        id = id, input_prefix = "", r = r, r_prefix = "project",
        element_id = m$selected_project, file_id = input$close_selected_tab
      )
    })
    
    # Create a file ----
    
    observeEvent(input$add_file, {
      if (debug) cat(paste0("\n", now(), " - mod_project_files - observer input$add_file"))
      
      # req("projects_edit_code" %in% user_accesses)
      
      files_browser_create_file(
        id = id, input_prefix = "", r = r, r_prefix = "project", folder = input$selected_project_folder,
        element_id = m$selected_project, code_hotkeys = code_hotkeys, user_settings = user_settings
      )
    })
    
    # Editor hotkeys ----
    observeEvent(input$add_code_editor_hotkeys, {
      if (debug) cat(paste0("\n", now(), " - mod_project_files - observer input$add_code_editor_hotkeys"))
      
      file_id <- input$add_code_editor_hotkeys
      editor_id <- paste0("editor_", file_id)
      
      observeEvent(input[[paste0(editor_id, "_run_all")]], {
        if (debug) cat(paste0("\n", now(), " - mod_project_files - observer input$editor..run_all"))
        r$run_project_code <- now()
      })
      
      observeEvent(input[[paste0(editor_id, "_run_selection")]], {
        if (debug) cat(paste0("\n", now(), " - mod_project_files - observer input$editor..run_selection"))
        r$run_project_code <- now()
      })
      
      observeEvent(input[[paste0(editor_id, "_comment")]], {
        if (debug) cat(paste0("\n", now(), " - mod_project_files - observer input$editor..comment"))
        
        toggle_comments(id = id, input_id = editor_id, code = input[[editor_id]], selection = input[[paste0(editor_id, "_comment")]]$range, session = session)
      })
      
      observeEvent(input[[paste0(editor_id, "_save")]], {
        if (debug) cat(paste0("\n", now(), " - mod_project_files - observer input$edit_code_editor..save"))
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_file_code', Math.random());"))
      })
    })
    
    # Save updates ----
    
    observeEvent(input$save_file_code, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$save_file_code"))
      
      # req("plugins_edit_code" %in% user_accesses)
      
      file <- r$project_files_list %>% dplyr::filter(id == input$selected_file)
      
      files_browser_save_file(
        id = id, i18n = i18n, output = output, input_prefix = "", r = r, r_prefix = "project",
        folder = input$selected_project_folder, element_id = m$selected_project, file_id = file$id,
        new_code = input[[paste0("editor_", file$id)]]
      )
    })
    
    # Rename a file ----
    
    # Show textfield
    
    observeEvent(input$edit_filename_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_project_files - observer input$edit_filename_trigger"))
      
      file_id <- input$edit_filename
      file_name <- r$project_files_list %>% dplyr::filter(id == file_id) %>% dplyr::pull(filename)
      shiny.fluent::updateTextField.shinyInput(session, paste0("edit_filename_textfield_", file_id), value = file_name)
      
      shinyjs::delay(
        50, 
        sapply(c(
          paste0("edit_filename_textfield_div_", file_id),
          paste0("save_filename_button_div_", file_id),
          paste0("cancel_rename_button_div_", file_id)),
          shinyjs::show
        )
      )
      sapply(c(
        paste0("filename_div_", file_id),
        paste0("delete_file_button_div_", file_id),
        paste0("edit_filename_button_div_", file_id)),
        shinyjs::hide
      )
    })
    
    # Cancel rename
    
    observeEvent(input$cancel_rename_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_project_files - observer input$cancel_rename_trigger"))
      
      file_id <- input$edit_filename
      
      shinyjs::delay(
        50, 
        sapply(c(
          paste0("edit_filename_textfield_div_", file_id),
          paste0("save_filename_button_div_", file_id),
          paste0("cancel_rename_button_div_", file_id)),
          shinyjs::hide
        )
      )
      sapply(c(
        paste0("filename_div_", file_id),
        paste0("delete_file_button_div_", file_id),
        paste0("edit_filename_button_div_", file_id)),
        shinyjs::show
      )
    })
    
    # Save new name
    
    observeEvent(input$save_filename_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$save_filename_trigger"))
      
      file_id <- input$save_filename
      textfield_id <- paste0("edit_filename_textfield_", file_id)
      new_name <- input[[textfield_id]]
      
      files_browser_edit_filename(
        id = id, i18n = i18n, output = output, input_prefix = "", r = r, r_prefix = "project",
        folder = input$selected_project_folder, element_id = m$selected_project, file_id = file_id,
        new_name = new_name
      )
    })
    
    # Delete a file ----
    
    observeEvent(input$delete_file_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_project_files - observer input$delete_file_trigger"))
      shinyjs::show("delete_file_modal")
    })
    
    observeEvent(input$close_file_deletion_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_project_files - observer input$close_file_deletion_modal"))
      shinyjs::hide("delete_file_modal")
    })
    
    observeEvent(input$confirm_file_deletion, {
      if (debug) cat(paste0("\n", now(), " - mod_project_files - observer input$confirm_file_deletion"))
      
      # req("plugins_edit_code" %in% user_accesses)
      
      files_browser_delete_file(
        id = id, i18n = i18n, output = output, input_prefix = "", r = r, r_prefix = "project",
        folder = input$selected_project_folder, element_id = m$selected_project, file_id = input$delete_file
      )
    })
    
    # Run code ----
    
    
  })
}