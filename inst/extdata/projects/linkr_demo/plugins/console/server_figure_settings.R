# Server - Figure settings

# Load figure settings

observe_event(input$load_figure_settings_%widget_id%, {
    
    # Update figure settings UI
    
    link_id <- input$settings_file_%widget_id%
    sql <- glue::glue_sql("SELECT name, value FROM widgets_options WHERE widget_id = %widget_id% AND category = 'figure_settings' AND link_id = {link_id}", .con = m$db)
    figure_settings <- DBI::dbGetQuery(m$db, sql)
    
    if (nrow(figure_settings) > 0){
        sapply(figure_settings$name, function(name){
        
            value <- figure_settings %>% dplyr::filter(name == !!name) %>% dplyr::pull(value)
            
            if (name %in% c("prog_language", "output")) shiny.fluent::updateDropdown.shinyInput(session, paste0(name, "_%widget_id%"), value = value)
            if (name == "code"){
                m$code_%widget_id% <- value
                shinyAce::updateAceEditor(session, "code_%widget_id%", value = value)
            }
        })
    }
    
    # Run code if toggle is activated
    if (length(input$run_code_at_settings_file_load_%widget_id%) > 0){
        if (input$run_code_at_settings_file_load_%widget_id%){
            shinyjs::delay(500, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());")))
        }
    }
})

# Save current settings

observe_event(input$save_params_and_code_%widget_id%, {
    
    # If no settings file is selected, go to settings files management page
    if (length(input$settings_file_%widget_id%) == 0){
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_settings_files_tab_%widget_id%', Math.random());"))
        return()
    }
        
    link_id <- input$settings_file_%widget_id%

    # Delete old settings
    sql_send_statement(m$db, glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = %widget_id% AND category = 'figure_settings' AND link_id = {link_id}", .con = m$db))
    
    # Add new settings in db
    new_data <- tibble::tribble(
        ~name, ~value, ~value_num,
        "prog_language", input$prog_language_%widget_id%, NA_real_,
        "output", input$output_%widget_id%, NA_real_,
        "code", input$code_%widget_id%, NA_real_
    ) %>%
    dplyr::transmute(
        id = get_last_row(m$db, "widgets_options") + 1:3, widget_id = %widget_id%, person_id = NA_integer_, link_id = link_id,
        category = "figure_settings", name, value, value_num, creator_id = m$user_id, datetime = now(), deleted = FALSE
    )
    
    DBI::dbAppendTable(m$db, "widgets_options", new_data)
    
    # Notify user
    show_message_bar("modif_saved", "success")
})

# Change programming language

output_dropdown_options <- list()
output_dropdown_options$r <- list(
    list(key = "console", text = i18np$t("console")),
    list(key = "ui", text = i18np$t("ui_html")),
    list(key = "figure", text = i18np$t("figure")),
    list(key = "table", text = i18np$t("table")),
    list(key = "datatable", text = i18np$t("datatable")),
    list(key = "dygraphs", text = i18np$t("dygraphs")),
    list(key = "plotly", text = i18np$t("plotly")),
    list(key = "rmarkdown", text = i18np$t("rmarkdown"))
)
output_dropdown_options$python <- list(
    list(key = "console", text = i18n$t("console")),
    list(key = "matplotlib", text = i18n$t("matplotlib"))
)

observe_event(input$prog_language_%widget_id%, {
    
    # Update ace editor
    shinyAce::updateAceEditor(session, "code_%widget_id%", mode = input$prog_language_%widget_id%)
    
    # Update output dropdown
    shiny.fluent::updateDropdown.shinyInput(session, "output_%widget_id%", options = output_dropdown_options[[input$prog_language_%widget_id%]], value = "console")
})

observe_event(input$output_%widget_id%, {
    
    # Update ace editor
    if (input$output_%widget_id% == "rmarkdown") mode <- "markdown"
    else mode <- input$prog_language_%widget_id%
    shinyAce::updateAceEditor(session, "code_%widget_id%", mode = mode)
})
