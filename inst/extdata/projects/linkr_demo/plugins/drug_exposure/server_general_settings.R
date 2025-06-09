# Server - General settings

# Settings / editor side-by-side with figure

observe_event(input$figure_and_settings_side_by_side_%widget_id%, {
   
    if (input$figure_and_settings_side_by_side_%widget_id%){
        shinyjs::runjs(paste0("
            $('#", id, "-figure_div_%widget_id%').css('flex-basis', '50%');
            $('#", id, "-figure_settings_div_%widget_id%').css('flex-basis', '50%');
            $('#", id, "-code_div_%widget_id%').css('flex-basis', '50%');
            
            if (!window.resizingInitialized_%widget_id%) {
                var container = document.getElementById('", id, "-figure_settings_code_div_%widget_id%');
                
                var isResizing = false;
                var lastDownX = 0;
                
                var leftPanel = container.querySelector('.left-panel');
                var figureSettingsPanel = document.getElementById('", id, "-figure_settings_div_%widget_id%');
                var codePanel = document.getElementById('", id, "-code_div_%widget_id%');
                var resizer = container.querySelector('.resizer');
                
                function triggerResizeEvent() {
                    var event = new Event('resize');
                    window.dispatchEvent(event);
                }
                
                resizer.addEventListener('mousedown', function(e) {
                    isResizing = true;
                    lastDownX = e.clientX;
            
                    document.addEventListener('mousemove', resizePanels);
                    document.addEventListener('mouseup', stopResizing);
                });
                
                function resizePanels(e) {
                    if (!isResizing) return;
                    
                    var offsetLeftPanel = leftPanel.offsetWidth;
                    var offsetFigureSettingsPanel = figureSettingsPanel.offsetWidth;
                    var offsetCodePanel = codePanel.offsetWidth;
                    var deltaX = e.clientX - lastDownX;
                    
                    leftPanel.style.flexBasis = (offsetLeftPanel + deltaX) + 'px';
                    
                    figureSettingsPanel.style.flexBasis = (offsetFigureSettingsPanel - deltaX) + 'px';
                    codePanel.style.flexBasis = (offsetCodePanel - deltaX) + 'px';
            
                    lastDownX = e.clientX;
                    triggerResizeEvent();
                }
                
                function stopResizing(e) {
                    isResizing = false;
                    document.removeEventListener('mousemove', resizePanels);
                    document.removeEventListener('mouseup', stopResizing);
                    triggerResizeEvent();
                }
            }
            
            window.resizingInitialized_%widget_id% = true;
        "))
        shinyjs::hide("figure_button_div_%widget_id%")
        shinyjs::show("resizer_%widget_id%")
    }
    else {
        shinyjs::runjs(paste0("
            $('#", id, "-figure_div_%widget_id%').css('flex-basis', '100%');
            $('#", id, "-figure_settings_div_%widget_id%').css('flex-basis', '100%');
            $('#", id, "-code_div_%widget_id%').css('flex-basis', '100%');
        "))
        
        shinyjs::show("figure_button_div_%widget_id%");
        shinyjs::hide("resizer_%widget_id%");
    }
})

# Save general settings in db

observe_event(input$save_general_settings_%widget_id%, {
    
    # Delete old rows
    sql_send_statement(m$db, glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND category = 'general_settings' AND name != 'selected_file_id'", .con = m$db))
    
    file_id <- input$settings_file_%widget_id%
    new_data <- tibble::tibble(name = character(), value = character(), value_num = integer())
    
    general_settings_vec <- c("show_settings_file", "figure_and_settings_side_by_side", "run_code_on_data_update", "run_code_at_settings_file_load")
    
    sapply(general_settings_vec, function(name){
        toggle_value <- 0L
        input_name <- paste0(name, "_%widget_id%")
        if (length(input[[input_name]]) > 0) if (input[[input_name]]) toggle_value <- 1L
        new_data <<- new_data %>% dplyr::bind_rows(tibble::tibble(name = name, value = NA_character_, value_num = toggle_value))
    })
    
    new_data <-
        new_data %>%
        dplyr::transmute(
            id = get_last_row(m$db, "widgets_options") + 1:(length(general_settings_vec)), widget_id = %widget_id%, person_id = NA_integer_, link_id = NA_integer_,
            category = "general_settings", name, value, value_num, creator_id = m$user_id, datetime = now(), deleted = FALSE
        )
    
    DBI::dbAppendTable(m$db, "widgets_options", new_data)
})
