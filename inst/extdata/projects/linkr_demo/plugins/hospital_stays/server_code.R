# Server - Code

# Init code var
m$code_%widget_id% <- ""

# Prevent a bug with scroll into ace editor
shinyjs::delay(300, shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);"))

# Comment code
observe_event(input$code_%widget_id%_comment, {
    toggle_comments(
        id = id, input_id = "code_%widget_id%", code = input$code_%widget_id%,
        selection = input$code_%widget_id%_comment$range, session = session
    )
})

# Run all code with shortcut
observe_event(input$code_%widget_id%_run_all, {
    if ("projects_widgets_console" %in% user_accesses){
        m$code_%widget_id% <- input$code_%widget_id%
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
})

# Run code when button is clicked
observe_event(input$display_figure_%widget_id%, {
    
    # If current selected tab is figure settings when run code button is clicked, generate code from these settings
    if (length(input$current_tab_%widget_id%) == 0) current_tab <- "figure_settings"
    else current_tab <- input$current_tab_%widget_id%
    
    if (current_tab == "figure_settings"){
        
        # Code to generate code from figure settings
        
        # ...
        code <- ""
        
        # Update ace editor with generated code
        shinyAce::updateAceEditor(session, "code_%widget_id%", value = code)
        
        m$code_%widget_id% <- code
        
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
    
    # Check if user has access
    else if ("projects_widgets_console" %in% user_accesses){
        m$code_%widget_id% <- input$code_%widget_id%
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
})

# Run code at patient update
observe_event(m$selected_person, if (isTRUE(input$run_code_on_data_update_%widget_id%)) shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());")))

# Run code
observe_event(input$run_code_%widget_id%, {
    
    if (is.na(m$selected_person)){
        output$error_message_%widget_id% <- renderUI(div(shiny.fluent::MessageBar(i18np$t("select_patient"), messageBarType = 5), style = "display: inline-block;"))
        shinyjs::hide("stays_plot_%widget_id%")
        shinyjs::show("error_message_div_%widget_id%")
    }
    else {

        sql <- glue::glue_sql("SELECT * FROM visit_detail WHERE person_id = {m$selected_person}", .con = d$con)
        data <- DBI::dbGetQuery(d$con, sql)
        
        if (nrow(data) == 0){
                
            output$error_message_%widget_id% <- renderUI(div(shiny.fluent::MessageBar(i18np$t("no_data_to_display"), messageBarType = 5), style = "display: inline-block;"))
            
            shinyjs::hide("stays_plot_%widget_id%")
            shinyjs::show("error_message_div_%widget_id%")
        } else {
            
            shinyjs::hide("error_message_div_%widget_id%")
            shinyjs::show("stays_plot_%widget_id%")
            
            data <-
                data %>%
                dplyr::select(visit_detail_start_datetime, visit_detail_end_datetime, care_site_id) %>%
                dplyr::left_join(
                    d$care_site %>% dplyr::select(care_site_id, care_site_name) %>% dplyr::collect(),
                    by = "care_site_id"
                ) %>%
                dplyr::arrange(visit_detail_start_datetime) %>%
                dplyr::filter(!is.na(care_site_name)) %>%
                dplyr::mutate(
                    service_order = as.numeric(forcats::fct_rev(forcats::fct_inorder(care_site_name)))
                )
            
            unique_levels <- data$service_order
            unique_labels <- data$care_site_name
            
            if (language == "fr") datetime_format <- "%d-%m-%Y %H:%M"
            else datetime_format <- "%Y-%m-%d %H:%M"
            
            plotly_visit_detail <- plotly::plot_ly(data = data, source = "visit_detail_plot_%widget_id%") %>%
                plotly::add_segments(
                    x = ~visit_detail_start_datetime,
                    xend = ~visit_detail_end_datetime,
                    y = ~service_order,
                    yend = ~service_order,
                    line = list(color = "steelblue", width = 10),
                    text = ~paste0(
                        i18np$t("hospital_unit"), " : ", care_site_name, "<br>",
                        i18np$t("datetime_start"), " : ", format(visit_detail_start_datetime, datetime_format), "<br>",
                        i18np$t("datetime_end"), " : ", format(visit_detail_end_datetime, datetime_format)
                    ),
                    hoverinfo = "text"
                ) %>%
                plotly::layout(
                    xaxis = list(
                        type = "date",
                        tickmode = "auto",
                        title = "",
                        nticks = 10,
                        tickfont = list(size = 10),
                        tickformat = datetime_format
                    ),
                    yaxis = list(
                        tickvals = unique_levels,
                        ticktext = unique_labels,
                        title = "",
                        automargin = TRUE
                    ),
                    hoverlabel = list(align = "left")
                ) %>%
                plotly::config(displayModeBar = FALSE)
        
            output$stays_plot_%widget_id% <- plotly::renderPlotly(plotly_visit_detail)
        }
    }

    # Go to figure tab
    if (!input$figure_and_settings_side_by_side_%widget_id%) shinyjs::click("figure_button_%widget_id%")
})

# Save code with shortcut
observe_event(input$code_%widget_id%_save, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_params_and_code_%widget_id%', Math.random());")))
