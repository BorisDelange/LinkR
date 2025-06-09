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
observe_event(m$selected_person, {

    if (!(isTRUE(input$run_code_on_data_update_%widget_id%) && length(input$data_source_%widget_id%) > 0 && input$data_source_%widget_id% == "person")) return()
        
    # Reset synchronized datetimes
    m$debounced_datetimes_timeline_%tab_id% <- reactiveVal()
    m$datetimes_timeline_%tab_id% <- reactiveVal()
    m$debounced_datetimes_timeline_%tab_id% <- reactiveVal()
    m$debounced_datetimes_timeline_%tab_id% <- reactive(m$datetimes_timeline_%tab_id%()) %>% debounce(500)
    
    # Run code
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# Run code at visit_detail update
observe_event(m$selected_visit_detail, {

    if (!(isTRUE(input$run_code_on_data_update_%widget_id%) && length(input$data_source_%widget_id%) > 0 && input$data_source_%widget_id% == "visit_detail")) return()
        
    # Reset synchronized datetimes
    m$debounced_datetimes_timeline_%tab_id% <- reactiveVal()
    m$datetimes_timeline_%tab_id% <- reactiveVal()
    m$debounced_datetimes_timeline_%tab_id% <- reactiveVal()
    m$debounced_datetimes_timeline_%tab_id% <- reactive(m$datetimes_timeline_%tab_id%()) %>% debounce(500)
    
    # Run code
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
})

# Run code
observe_event(input$run_code_%widget_id%, {
    
    data_source <- "visit_detail"
    if (length(input$data_source_%widget_id%) > 0) data_source <- input$data_source_%widget_id%
    
    if ((data_source == "person" & is.na(m$selected_person)) | (data_source == "visit_detail" & is.na(m$selected_visit_detail))){
        
        if (data_source == "person" & is.na(m$selected_person)) output$error_message_%widget_id% <- renderUI(
            div(shiny.fluent::MessageBar(i18np$t("select_patient"), messageBarType = 5), style = "display: inline-block;")
        )
        else if (data_source == "visit_detail" & is.na(m$selected_visit_detail)) output$error_message_%widget_id% <- renderUI(
            div(shiny.fluent::MessageBar(i18np$t("select_stay"), messageBarType = 5), style = "display: inline-block;")
        )
        
        sapply(c("datatable_%widget_id%", "datetime_slider_div_%widget_id%"), shinyjs::hide)
        shinyjs::show("error_message_div_%widget_id%")
        
    } else {
    
        if (data_source == "person") sql <- glue::glue_sql("
            SELECT
                person_id,
                visit_detail_id,
                CASE WHEN measurement_concept_id > 0 THEN measurement_concept_id ELSE measurement_source_concept_id END AS measurement_concept_id,
                measurement_datetime,
                value_as_number
            FROM measurement
            WHERE person_id = {m$selected_person}", .con = d$con)
        else if (data_source == "visit_detail") sql <- glue::glue_sql("
            SELECT
                person_id,
                visit_detail_id,
                CASE WHEN measurement_concept_id > 0 THEN measurement_concept_id ELSE measurement_source_concept_id END AS measurement_concept_id,
                measurement_datetime,
                value_as_number
            FROM measurement
            WHERE visit_detail_id = {m$selected_visit_detail}", .con = d$con)
        data <- DBI::dbGetQuery(d$con, sql)
        
        if (length(input$concepts_choice_%widget_id%) > 0){
            if (input$concepts_choice_%widget_id% == "selected_concept_classes"){
                if (nrow(data) > 0) data <- 
                    data %>%
                    dplyr::collect() %>%
                    dplyr::inner_join(d$dataset_concept %>% dplyr::select(measurement_concept_id = concept_id, concept_class_id), by = "measurement_concept_id") %>%
                    dplyr::filter(concept_class_id %in% input$concept_classes_%widget_id%) %>%
                    dplyr::select(-concept_class_id)
            }
            else if (input$concepts_choice_%widget_id% == "selected_concepts"){
                if (nrow(data) > 0) data <- data %>% dplyr::filter(measurement_concept_id %in% input$concepts_%widget_id%)
            }
        }
        
        if (nrow(data) == 0){
            
            output$error_message_%widget_id% <- renderUI(div(shiny.fluent::MessageBar(i18np$t("no_data_to_display"), messageBarType = 5), style = "display: inline-block;"))
            
            sapply(c("datatable_%widget_id%", "datetime_slider_div_%widget_id%"), shinyjs::hide)
            shinyjs::show("error_message_div_%widget_id%")
        } else {
            
            shinyjs::hide("error_message_div_%widget_id%")
            sapply(c("datatable_%widget_id%", "datetime_slider_div_%widget_id%"), shinyjs::show)
            
            if (length(input$num_cols_%widget_id%) > 0) num_cols <- input$num_cols_%widget_id%
            else num_cols <- 8
            
            if (length(input$aggregate_fct_%widget_id%) > 0) aggregate_fct <- input$aggregate_fct_%widget_id%
            else aggregate_fct <- 8
            
            
            if (data_source == "person") {
            sql <- glue::glue_sql("
                    SELECT 
                        MIN(visit_start_datetime) AS min_visit_start_datetime,
                        MAX(visit_end_datetime) AS max_visit_end_datetime
                    FROM visit_occurrence
                    WHERE person_id = {m$selected_person}
                ", .con = d$con)
                data_datetimes_range <- DBI::dbGetQuery(d$con, sql)
            }
            else if (data_source == "visit_detail") {
                sql <- glue::glue_sql("
                    SELECT
                        MIN(visit_detail_start_datetime) AS min_visit_start_datetime,
                        MAX(visit_detail_end_datetime) AS max_visit_end_datetime
                    FROM visit_detail
                    WHERE visit_detail_id = {m$selected_visit_detail}
                ", .con = d$con)
                data_datetimes_range <- DBI::dbGetQuery(d$con, sql)
            }
            
            data_datetimes_range <- c(data_datetimes_range$min_visit_start_datetime, data_datetimes_range$max_visit_end_datetime)
            m$data_datetimes_range_%widget_id% <- data_datetimes_range
            
            datetimes <- data_datetimes_range
            
            if (isTRUE(input$synchronize_timelines_%widget_id%)){
                if(!is.null(m$debounced_datetimes_timeline_%tab_id%)) if (length(m$debounced_datetimes_timeline_%tab_id%()) > 0) datetimes <- m$debounced_datetimes_timeline_%tab_id%()
            }
            else {
                if (!is.null(m$debounced_datetime_slider_%widget_id%)){
                    if (length(m$debounced_datetime_slider_%widget_id%()) > 0) {
                        if (m$debounced_datetime_slider_%widget_id%()[[1]] >= data_datetimes_range[[1]] & m$debounced_datetime_slider_%widget_id%()[[2]] <= data_datetimes_range[[2]]){
                            datetimes <- m$debounced_datetime_slider_%widget_id%()
                        }
                    }
                }
            }
            
            if (length(datetimes) > 0) m$datetimes_%widget_id% <- datetimes
            
            updateSliderInput(
                session, "datetime_slider_%widget_id%", min = data_datetimes_range[[1]], max = data_datetimes_range[[2]],
                value = datetimes,
                timeFormat = ifelse(language == "fr", "%d-%m-%Y %H:%M", "%Y-%m-%d %H:%M"), step = 3600000
            )
            
            data <-
                data %>%
                dplyr::collect() %>%
                dplyr::left_join(
                    d$dataset_concept %>% 
                    dplyr::select(measurement_concept_id = concept_id, measurement_concept_name = concept_name),
                    by = "measurement_concept_id"
                ) %>%
                # Replace NA concept_name by concept_id
                dplyr::mutate(measurement_concept_name = dplyr::if_else(is.na(measurement_concept_name), as.character(measurement_concept_id), measurement_concept_name)) %>%
                dplyr::select(measurement_concept_name, measurement_datetime, value_as_number) %>%
                dplyr::arrange(measurement_concept_name, measurement_datetime)
            
            interval_duration <- as.numeric(difftime(datetimes[[2]], datetimes[[1]], units = "secs")) / num_cols
            
            if (language == "fr") date_format <- "%d-%m-%Y"
            else date_format <- "%Y-%m-%d"
            
            intervals <- tibble::tibble(
                interval = 0:(num_cols - 1),
                interval_start = datetimes[[1]] + interval * interval_duration,
                interval_end = interval_start + interval_duration
            ) %>%
            dplyr::mutate(
                interval_label = paste(
                    format(interval_start, date_format),
                    " <span style='color:#0084D8'>", format(interval_start, "%H:%M"), "</span>",
                    " ", tolower(i18np$t("to")), " ",
                    format(interval_end, date_format),
                    " <span style='color:#0084D8'>", format(interval_end, "%H:%M"), "</span>"
                )
            )
            
            data <-
                data %>%
                dplyr::mutate(
                    interval = floor(as.numeric(difftime(measurement_datetime, datetimes[[1]], units = "secs")) / interval_duration)
                ) %>%
                dplyr::group_by(measurement_concept_name, interval) %>%
                dplyr::summarize(
                    agg_value = round(do.call(aggregate_fct, list(value_as_number, na.rm = TRUE)), 1),
                    n = sum(!is.na(value_as_number)),
                    .groups = "drop"
                ) %>%
                dplyr::mutate(agg_value = paste0(agg_value, " (n = ", n, ")")) %>%
                dplyr::right_join(intervals, by = "interval") %>%
                dplyr::select(measurement_concept_name, interval_label, agg_value) %>%
                tidyr::pivot_wider(
                    names_from = interval_label,
                    values_from = agg_value
                ) %>%
                dplyr::arrange(measurement_concept_name) %>%
                dplyr::rename(!!i18np$t("concept") := measurement_concept_name) %>%
                dplyr::filter(!is.na(!!rlang::sym(i18np$t("concept"))) & !!rlang::sym(i18np$t("concept")) != "")
                
            # Reorder date columns (works also with french date format)
            date_cols <- names(data)[-1]
            sorted_date_cols <- date_cols[order(as.Date(
                gsub(" <.*$", "", date_cols),
                format = date_format
            ))]
            
            data <-
                data %>%
                dplyr::relocate(all_of(sorted_date_cols), .after = 1)
            
            output$datatable_%widget_id% <- DT::renderDT({
                row_count <- nrow(data)
            
                DT::datatable(
                    data,
                    rownames = FALSE,
                    options = list(
                        dom = if (row_count > 10) "<'datatable_length'l><'top't><'bottom'p>" else "<'top't>",
                        pageLength = if (row_count > 10) 25 else 10,
                        paging = row_count > 10,
                        compact = TRUE, 
                        hover = TRUE
                    ),
                    escape = FALSE,
                    
                    # CSS for datatable
                    callback = htmlwidgets::JS(
                      "table.on('draw.dt', function() {",
                      "  $('.dataTable tbody tr td').css({",
                      "    'height': '12px',",
                      "    'padding': '2px 5px'",
                      "  });",
                      "  $('.dataTable thead tr td div .form-control').css('font-size', '12px');",
                      "  $('.dataTable thead tr td').css('padding', '5px');",
                      "  $('.dataTable tbody tr td:first-child').css({",
                      "    'min-width': '100px',",
                      "    'max-width': '100px',",
                      "    'width': '100px',",
                      "    'white-space': 'nowrap'",
                      "  });",
                      "  $('.dataTable thead tr th:first-child').css({",
                      "    'min-width': '100px',",
                      "    'max-width': '100px',",
                      "    'width': '100px',",
                      "    'white-space': 'nowrap'",
                      "  });",
                      "",
                      "  $('.dataTable tbody td').each(function() {",
                      "    var cellText = $(this).text();",
                      "    $(this).attr('title', cellText);",
                      "  });",
                      "});"
                    )
                )
            })
        }
    }
    
    # Go to figure tab
    if (!input$figure_and_settings_side_by_side_%widget_id%) shinyjs::click("figure_button_%widget_id%")
})

# Save code with shortcut
observe_event(input$code_%widget_id%_save, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_params_and_code_%widget_id%', Math.random());")))

# Adjust CSS of datetime sliderInput

# shinyjs::delay(100, shinyjs::runjs(paste0("
#     function adjustSliderLabels_%widget_id%() {
#         const toLabel = document.querySelector('#", id, "-datetime_slider_div_%widget_id% .irs-to');
#         const singleLabel = document.querySelector('#", id, "-datetime_slider_div_%widget_id% .irs-single');
#     
#         if (toLabel) {
#             const toLeft = parseFloat(toLabel.style.left);
#             if (toLeft > 72) {
#                 toLabel.style.left = '72%';
#             }
#         }
#     
#         if (singleLabel) {
#             const singleLeft = parseFloat(singleLabel.style.left);
#             if (singleLeft < 0) {
#                 singleLabel.style.left = '0%';
#             } else if (singleLeft > 32) {
#                 singleLabel.style.left = '32%';
#             }
#         }
#     }
#     
#     window.observeSliderChanges_%widget_id% = function() {
#         const toLabel = document.querySelector(('#", id, "-datetime_slider_div_%widget_id% .irs-to');
#         const singleLabel = document.querySelector(('#", id, "-datetime_slider_div_%widget_id% .irs-single');
#         
#         if (toLabel) {
#             const observer = new MutationObserver(adjustSliderLabels);
#             
#             observer.observe(fromLabel, { attributes: true, attributeFilter: ['style'] });
#             observer.observe(toLabel, { attributes: true, attributeFilter: ['style'] });
#             
#             if (singleLabel) {
#                 observer.observe(singleLabel, { attributes: true, attributeFilter: ['style'] });
#             }
#             
#             adjustSliderLabels_%widget_id%();
#         }
#     }
#     
#     observeSliderChanges_%widget_id%();
# ")))
