# Server - Figure settings

# Load figure settings and code

observe_event(input$load_figure_settings_%widget_id%, {
   
    # Update figure settings UI
    
    link_id <- input$settings_file_%widget_id%
    sql <- glue::glue_sql("SELECT name, value, value_num FROM widgets_options WHERE widget_id = %widget_id% AND category = 'figure_settings' AND link_id = {link_id}", .con = m$db)
    figure_settings <- DBI::dbGetQuery(m$db, sql)
    
    code <- ""
    
    if (nrow(figure_settings) > 0){
        sapply(figure_settings$name, function(name){
        
            value <- figure_settings %>% dplyr::filter(name == !!name) %>% dplyr::pull(value)
            value_num <- figure_settings %>% dplyr::filter(name == !!name) %>% dplyr::pull(value_num)
            
            if (name == "data_source") shiny.fluent::updateDropdown.shinyInput(session, paste0(name, "_%widget_id%"), value = value)
            else if (name == "concepts"){
                value <- as.numeric(unlist(strsplit(value, ", ")))
                shiny.fluent::updateDropdown.shinyInput(session, paste0(name, "_%widget_id%"), value = value)
            }
            else if (name == "synchronize_timelines"){
                value <- as.logical(value_num)
                shiny.fluent::updateToggle.shinyInput(session, paste0(name, "_%widget_id%"), value = value)
            }
            else if (name == "code"){
                code <- value
                m$code_%widget_id% <- value
                shinyAce::updateAceEditor(session, "code_%widget_id%", value = code)
            }
        })
    }
    
    # Run code if toggle is activated
    if (length(input$run_code_at_settings_file_load_%widget_id%) > 0) if (input$run_code_at_settings_file_load_%widget_id%) shinyjs::delay(500, {
        # m$code_%widget_id% <- code
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))  
    })
})

# Save current settings

observe_event(input$save_params_and_code_%widget_id%, {
    
    # If no settings file is selected, go to settings files management page
    if (length(input$settings_file_%widget_id%) == 0) shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_settings_files_tab_%widget_id%', Math.random());"))
    
    if (length(input$settings_file_%widget_id%) > 0){
        
        link_id <- input$settings_file_%widget_id%
    
        # Delete old settings
        sql_send_statement(m$db, glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND category = 'figure_settings' AND link_id = {link_id}", .con = m$db))
        
        # Add new settings in db
        new_data <- tibble::tribble(
            ~name, ~value, ~value_num,
            "data_source", input$data_source_%widget_id%, NA_real_,
            "concepts", input$concepts_%widget_id% %>% toString(), NA_real_,
            "synchronize_timelines", NA_character_, as.integer(input$synchronize_timelines_%widget_id%),
            "code", input$code_%widget_id%, NA_real_
        )
        
        new_data <-
            new_data %>%
            dplyr::transmute(
                id = get_last_row(m$db, "widgets_options") + 1:nrow(new_data), widget_id = %widget_id%, person_id = NA_integer_, link_id = link_id,
                category = "figure_settings", name, value, value_num, creator_id = m$user_id, datetime = now(), deleted = FALSE
            )
        
        DBI::dbAppendTable(m$db, "widgets_options", new_data)
        
        # Notify user
        show_message_bar("modif_saved", "success")
    }
})

# Synchronize timelines

# Create a reactiveVal with timeline datetimes
if (length(m$datetimes_timeline_%tab_id%) == 0){
    m$datetimes_timeline_%tab_id% <- reactiveVal()
    m$debounced_datetimes_timeline_%tab_id% <- reactive(m$datetimes_timeline_%tab_id%()) %>% debounce(500)
}

observe_event(input$synchronize_timelines_%widget_id%, {
    
    # Add a padding to align widget's timeline with other widgets
    if (input$synchronize_timelines_%widget_id%) {
        shinyjs::runjs(sprintf(
            "document.getElementById('%s').style.paddingLeft = '80px'; var event = new Event('resize'); window.dispatchEvent(event);",
            ns("dygraph_div_%widget_id%")
        ))
    } else {
        shinyjs::runjs(sprintf(
            "document.getElementById('%s').style.paddingLeft = '0px'; var event = new Event('resize'); window.dispatchEvent(event);",
            ns("dygraph_div_%widget_id%")
        ))
    }
})

observe_event(input$dygraph_%widget_id%_date_window, {
    
    if (!input$synchronize_timelines_%widget_id%) return()
        
    datetime_values <- as.POSIXct(input$dygraph_%widget_id%_date_window, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    m$datetimes_timeline_%tab_id%(datetime_values)
})

observe_event(m$debounced_datetimes_timeline_%tab_id%(), {
    
    if (!input$synchronize_timelines_%widget_id% || length(m$debounced_datetimes_timeline_%tab_id%()) == 0 || length(m$datetimes_%widget_id%) == 0) return()
    
    if (
        abs(as.numeric(m$debounced_datetimes_timeline_%tab_id%()[[1]]) - as.numeric(m$datetimes_%widget_id%[[1]])) > 5 |
        abs(as.numeric(m$debounced_datetimes_timeline_%tab_id%()[[2]]) - as.numeric (m$datetimes_%widget_id%[[2]])) > 5){
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
}, ignoreInit = TRUE)
