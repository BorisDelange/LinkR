# Server - Figure settings

# Load figure settings

observe_event(input$load_figure_settings_%widget_id%, {
    
    # Update figure settings UI
    
    link_id <- input$settings_file_%widget_id%
    sql <- glue::glue_sql("SELECT name, value, value_num FROM widgets_options WHERE widget_id = %widget_id% AND category = 'figure_settings' AND link_id = {link_id}", .con = m$db)
    figure_settings <- DBI::dbGetQuery(m$db, sql)
    
    if (nrow(figure_settings) > 0){
        sapply(figure_settings$name, function(name){
        
            value <- figure_settings %>% dplyr::filter(name == !!name) %>% dplyr::pull(value)
            value_num <- figure_settings %>% dplyr::filter(name == !!name) %>% dplyr::pull(value_num)
            
            if (name %in% c("data_source", "concepts_choice", "aggregate_fct")) shiny.fluent::updateDropdown.shinyInput(session, paste0(name, "_%widget_id%"), value = value)
            else if (name %in% c("concepts", "concept_classes")){
                value <- unlist(strsplit(value, ", "))
                if (name == "concepts") value <- as.numeric(value)
                shiny.fluent::updateDropdown.shinyInput(session, paste0(name, "_%widget_id%"), value = value)
            }
            else if (name == "num_cols") shiny.fluent::updateSpinButton.shinyInput(session, paste0(name, "_%widget_id%"), value = value_num)
            else if (name == "synchronize_timelines"){
                value <- as.logical(value_num)
                shiny.fluent::updateToggle.shinyInput(session, paste0(name, "_%widget_id%"), value = value)
            }
        })
    }
    
    # Run code if toggle is activated
    if (length(input$run_code_at_settings_file_load_%widget_id%) > 0){
        if (input$run_code_at_settings_file_load_%widget_id%){
           shinyjs::delay(500, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-display_figure_%widget_id%', Math.random());")))
       }
    }
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
            "concepts_choice", input$concepts_choice_%widget_id%, NA_real_,
            "concept_classes", input$concept_classes_%widget_id% %>% toString(), NA_real_,
            "concepts", input$concepts_%widget_id% %>% toString(), NA_real_,
            "num_cols", NA_character_, input$num_cols_%widget_id%,
            "aggregate_fct", input$aggregate_fct_%widget_id%, NA_real_,
            "synchronize_timelines", NA_character_, as.integer(input$synchronize_timelines_%widget_id%)
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

# Show / hide concepts_div and concept_classes_div

observe_event(input$concepts_choice_%widget_id%, {
    
    if (input$concepts_choice_%widget_id% == "selected_concepts"){
        shinyjs::hide("concept_classes_div_%widget_id%")
        shinyjs::show("concepts_div_%widget_id%")
    }
    else if (input$concepts_choice_%widget_id% == "selected_concept_classes"){
        
        # Update concept class IDs
        
        concept_class_ids <- tibble::tibble(concept_class_id = character())
        if (length(d$dataset_concept) > 0) if (nrow(d$dataset_concept) > 0) concept_class_ids <- d$dataset_concept %>% dplyr::filter(domain_id == "Measurement") %>% dplyr::distinct(concept_class_id)
        shiny.fluent::updateDropdown.shinyInput(session, "concept_classes_%widget_id%", options = convert_tibble_to_list(concept_class_ids, key_col = "concept_class_id", text_col = "concept_class_id"))
    
        shinyjs::hide("concepts_div_%widget_id%")
        shinyjs::show("concept_classes_div_%widget_id%")
    }
    else sapply(c("concepts_div_%widget_id%", "concept_classes_div_%widget_id%"), shinyjs::hide)
})

# Synchronize timelines

# Create a reactiveVal with timeline datetimes
if (length(m$datetimes_timeline_%tab_id%) == 0){
    m$datetimes_timeline_%tab_id% <- reactiveVal()
    m$debounced_datetimes_timeline_%tab_id% <- reactive(m$datetimes_timeline_%tab_id%()) %>% debounce(500)
}

m$debounced_datetime_slider_%widget_id% <- reactive(input$datetime_slider_%widget_id%) %>% debounce(500)

observe_event(m$debounced_datetime_slider_%widget_id%(), {
    
    if(length(m$debounced_datetime_slider_%widget_id%()) == 0 || length(m$datetimes_%widget_id%) == 0 || length(m$data_datetimes_range_%widget_id%) == 0) return()
    
    if (
        m$debounced_datetime_slider_%widget_id%()[[1]] >= m$data_datetimes_range_%widget_id%[[1]] &
        m$debounced_datetime_slider_%widget_id%()[[2]] <= m$data_datetimes_range_%widget_id%[[2]] 
    ){
        m$datetimes_timeline_%tab_id%(m$debounced_datetime_slider_%widget_id%() %>% as.POSIXct())
        
        if (
            isFALSE(input$synchronize_timelines_%widget_id%) &
            (abs(as.numeric(m$debounced_datetime_slider_%widget_id%()[[1]]) - as.numeric(m$datetimes_%widget_id%[[1]])) > 5 |
            abs(as.numeric(m$debounced_datetime_slider_%widget_id%()[[2]]) - as.numeric (m$datetimes_%widget_id%[[2]])) > 5)){
            shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
        }
    }
})

observe_event(m$debounced_datetimes_timeline_%tab_id%(), {
    
    if (!input$synchronize_timelines_%widget_id% || length(m$debounced_datetimes_timeline_%tab_id%()) == 0 || length(m$datetimes_%widget_id%) == 0) return()
    
    if (
        abs(as.numeric(m$debounced_datetimes_timeline_%tab_id%()[[1]]) - as.numeric(m$datetimes_%widget_id%[[1]])) > 5 |
        abs(as.numeric(m$debounced_datetimes_timeline_%tab_id%()[[2]]) - as.numeric (m$datetimes_%widget_id%[[2]])) > 5){
            shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
}, ignoreInit = TRUE)
