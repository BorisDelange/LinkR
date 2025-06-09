# Server - Settings files management

settings_files_ui_style <- paste0(
    "display: inline-block; color: white; max-width: 250px; border-radius: 8px; padding: 1px 5px; align-items: center;",
    "height: 18px; font-weight: 600; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; cursor: pointer; margin: 2px 5px;"
)

output$settings_files_ui_%widget_id% <- renderUI(div(i18np$t("no_settings_file_selected"), style = paste0(settings_files_ui_style, "background-color: #606060ab;")))

## Show / hide settings file
observe_event(input$show_settings_file_%widget_id%, {
    if (input$show_settings_file_%widget_id%) shinyjs::show("settings_files_ui_%widget_id%")
    else shinyjs::hide("settings_files_ui_%widget_id%")
})

## Show / hide settings files div
observe_event(input$show_settings_files_tab_%widget_id%, {
    sapply(c(paste0(tabs, "_div_%widget_id%"), "figure_settings_code_div_%widget_id%"), shinyjs::hide)
    shinyjs::show("settings_files_div_%widget_id%")
})

## Show add settings file modal
observe_event(input$create_settings_file_%widget_id%, shinyjs::show("add_settings_file_modal_%widget_id%"))

## Close add settings file modal
observe_event(input$close_add_settings_file_modal_%widget_id%, shinyjs::hide("add_settings_file_modal_%widget_id%"))

## Confirm creation of settings file
observe_event(input$add_settings_file_%widget_id%, {
   
    file_name <- input$settings_file_name_%widget_id%
    
    # Check if name if empty
    empty_name <- TRUE
    if (length(file_name) > 0) if (!is.na(file_name) & file_name != "") empty_name <- FALSE
    if (empty_name) shiny.fluent::updateTextField.shinyInput(session, "settings_file_name_%widget_id%", errorMessage = i18np$t("provide_valid_name"))
    else {
    
        shiny.fluent::updateTextField.shinyInput(session, "settings_file_name_%widget_id%", errorMessage = NULL)
        
        # Check if name is already used
        sql <- glue::glue_sql("SELECT value FROM widgets_options WHERE widget_id = %widget_id% AND category = 'settings_files' AND name = 'file_name'", .con = m$db)
        files_names <- DBI::dbGetQuery(m$db, sql) %>% dplyr::pull()
        name_already_used <- remove_special_chars(file_name) %in% remove_special_chars(files_names)
        
        if (name_already_used) shiny.fluent::updateTextField.shinyInput(session, "settings_file_name_%widget_id%", errorMessage = i18np$t("name_already_used"))
        else {
            
            new_id <- get_last_row(m$db, "widgets_options") + 1
            
            # Add settings file in database
            new_data <- tibble::tibble(
                id = new_id, widget_id = %widget_id%, person_id = NA_integer_, link_id = NA_integer_,
                category = "settings_files", name = "file_name", value = file_name, value_num = NA_real_, creator_id = m$user_id, datetime = now(), deleted = FALSE
            )
            DBI::dbAppendTable(m$db, "widgets_options", new_data)
            
            # Update filenames var
            m$settings_filenames_%widget_id% <- m$settings_filenames_%widget_id% %>% dplyr::bind_rows(tibble::tibble(id = new_id, name = file_name))
            
            # Reset fields
            shiny.fluent::updateTextField.shinyInput(session, "settings_file_name_%widget_id%", value = "")
            
            # Update dropdown
            dropdown_options <- convert_tibble_to_list(m$settings_filenames_%widget_id%, key_col = "id", text_col = "name")
            shiny.fluent::updateDropdown.shinyInput(session, "settings_file_%widget_id%", options = dropdown_options, value = new_id)
            
            # Reset ace editor code
            shinyAce::updateAceEditor(session, "code_%widget_id%", value = "")
            
            # Close modal
            shinyjs::hide("add_settings_file_modal_%widget_id%")
            
            # Notify user
            show_message_bar("new_settings_file_added", "success")
        }
    }
})

## A settings file is selected
observe_event(input$settings_file_%widget_id%, {
    
    # Show delete button
    shinyjs::show("delete_settings_file_div_%widget_id%")

    # Get file name
    file_id <- input$settings_file_%widget_id%
    filename <- m$settings_filenames_%widget_id% %>% dplyr::filter(id == file_id) %>% dplyr::pull(name)
    
    output$settings_files_ui_%widget_id% <- renderUI(div(filename, style = paste0(settings_files_ui_style, "background-color: #1d94ce;")))
    
    # Save that this file is selected
    sql_send_statement(m$db, glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND category = 'general_settings' AND name = 'selected_file_id'", .con = m$db))
    new_data <- tibble::tibble(
        id = get_last_row(m$db, "widgets_options") + 1, widget_id = %widget_id%, person_id = NA_integer_, link_id = file_id,
        category = "general_settings", name = "selected_file_id", value = NA_character_, value_num = NA_real_, creator_id = m$user_id, datetime = now(), deleted = FALSE
    )
    DBI::dbAppendTable(m$db, "widgets_options", new_data)
    
    # Load saved settings
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-load_figure_settings_%widget_id%', Math.random());"))
})

## Open delete a settings file modal
observe_event(input$delete_settings_file_%widget_id%, {
    if (length(input$settings_file_%widget_id%) == 0) return()
    shinyjs::show("delete_settings_file_modal_%widget_id%")
})

## Close delete a settings file modal
observe_event(input$close_file_deletion_modal_%widget_id%, shinyjs::hide("delete_settings_file_modal_%widget_id%"))

## Confirm settings file deletion
observe_event(input$confirm_file_deletion_%widget_id%, {
    
    file_id <- input$settings_file_%widget_id%
    
    # Delete row in db
    sql_send_statement(m$db, glue::glue_sql("DELETE FROM widgets_options WHERE id = {file_id}", .con = m$db))
    
    # Delete selected file in widgets_options
    sql_send_statement(m$db, glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND category = 'general_settings' AND name = 'selected_file_id'", .con = m$db))
    
    # Update filenames var
    m$settings_filenames_%widget_id% <- m$settings_filenames_%widget_id% %>% dplyr::filter(id != file_id)
    
    # Update dropdown
    dropdown_options <- convert_tibble_to_list(m$settings_filenames_%widget_id%, key_col = "id", text_col = "name")
    shiny.fluent::updateDropdown.shinyInput(session, "settings_file_%widget_id%", options = dropdown_options, value = NULL)
    
    # Close modal
    shinyjs::hide("delete_settings_file_modal_%widget_id%")
    
    # Update selected settings file UI
    output$settings_files_ui_%widget_id% <- renderUI(div(i18np$t("no_settings_file_selected"), style = paste0(settings_files_ui_style, "background-color: #606060ab;")))
    
    # Hide delete button
    shinyjs::hide("delete_settings_file_div_%widget_id%")
    
    # Notify user
    show_message_bar("settings_file_delete", "warning")
})
