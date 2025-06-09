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
        
        data_source <- "visit_detail"
        if (length(input$data_source_%widget_id%) > 0) data_source <- input$data_source_%widget_id%
        
        code <- paste0(
            "concepts <- tibble::tribble(\\n",
            "    ~concept_id, ~concept_name, ~domain_id, ~vocabulary_id"
        )
        
        concepts <- selected_concepts %>% dplyr::filter(concept_id %in% input$concepts_%widget_id%)
        
        if (nrow(concepts) > 0){
            for (i in 1:nrow(concepts)){
                    row <- concepts[i, ]
                    code <- paste0(
                        code,
                        ",\\n",
                        "    ", row$concept_id, ", '", row$concept_name, "', '", row$domain_id, "', '", row$vocabulary_id, "'"
                    )
                }
        }
        
        code <- paste0(code, "\\n", ")")
        
        code <- paste0(
            code, "\\n\\n",
            "features <- list()\\n",
            "features_names <- c()\\n",
            "raw_data <- tibble::tibble()\\n",
            "data_datetimes_range <- c()\\n",
            "combined_features <- c()"
        )
        
        code <- paste0(
            code,
            "\\n\\n",
            "sql <- glue::glue_sql('\\n",
            "    SELECT \\n",
            "        measurement_concept_id AS concept_id,\\n",
            "        measurement_source_concept_id AS source_concept_id,\\n",
            "        measurement_datetime AS datetime,\\n",
            "        value_as_number\\n",
            "    FROM measurement \\n",
            "    WHERE ", data_source, "_id = {m$selected_", data_source, "} \\n",
            "    AND (measurement_concept_id IN ({concepts$concept_id*}) OR measurement_source_concept_id IN ({concepts$concept_id*}))\\n",
            "    UNION\\n",
            "    SELECT \\n",
            "        observation_concept_id AS concept_id,\\n",
            "        observation_source_concept_id AS source_concept_id,\\n",
            "        observation_datetime AS datetime, value_as_number\\n",
            "    FROM observation \\n",
            "    WHERE ", data_source, "_id = {m$selected_", data_source, "} \\n",
            "    AND (observation_concept_id IN ({concepts$concept_id*}) OR observation_source_concept_id IN ({concepts$concept_id*}))\\n",
            "', .con = d$con)\\n\\n",
            "raw_data <- DBI::dbGetQuery(d$con, sql) %>% tibble::as_tibble()"
        )
        
        if (data_source == "person") {
            
            code <- paste0(
                code,
                "\\n\\n",
                "if (!is.na(m$selected_person)){\\n",
                "    sql <- glue::glue_sql('\\n",
                "        SELECT \\n",
                "            MIN(visit_start_datetime) AS min_visit_start_datetime, \\n",
                "            MAX(visit_end_datetime) AS max_visit_end_datetime \\n",
                "        FROM visit_occurrence \\n",
                "        WHERE person_id = {m$selected_person} \\n",
                "    ', .con = d$con)\\n\\n",
                "    data_datetimes_range <- DBI::dbGetQuery(d$con, sql)\\n",
                "}"
            )
        }
        else if (data_source == "visit_detail") {
            
            code <- paste0(
                code,
                "\\n\\n",
                "if (!is.na(m$selected_visit_detail)){\\n",
                "    sql <- glue::glue_sql('\\n",
                "        SELECT \\n",
                "            MIN(visit_detail_start_datetime) AS min_visit_start_datetime, \\n",
                "            MAX(visit_detail_end_datetime) AS max_visit_end_datetime \\n",
                "        FROM visit_detail \\n",
                "        WHERE visit_detail_id = {m$selected_visit_detail} \\n",
                "    ', .con = d$con)\\n\\n",
                "    data_datetimes_range <- DBI::dbGetQuery(d$con, sql)\\n",
                "}"
            )
        }
        
        code <- paste0(
            code,
            "\\n\\n",
            "if (length(data_datetimes_range) > 0){\\n",
            "    data_datetimes_range <- c(data_datetimes_range$min_visit_start_datetime, data_datetimes_range$max_visit_end_datetime)\\n",
            "    m$data_datetimes_range_%widget_id% <- data_datetimes_range\\n",
            "}\\n\\n",
            "datetimes <- data_datetimes_range"
        )
        
        if (isTRUE(input$synchronize_timelines_%widget_id%)) code <- paste0(
            code,
            "\\n",
            "if(!is.null(m$debounced_datetimes_timeline_%tab_id%)) if (length(m$debounced_datetimes_timeline_%tab_id%()) > 0) datetimes <- m$debounced_datetimes_timeline_%tab_id%()"
        )
        
        code <- paste0(
            code,
            "\\n\\nif (length(datetimes) > 0) m$datetimes_%widget_id% <- datetimes"
        )
        
        code <- paste0(
            code,
            "\\n\\n",
            "for (concept_id in concepts$concept_id) {\\n",
            "    concept <- concepts %>% dplyr::filter(concept_id == !!concept_id)\\n\\n",
            "    if (nrow(concept) > 0){\\n",
            "        if (concept$domain_id %in% c('Measurement', 'Observation')) {\\n",
            "            data <- raw_data\\n\\n",
            "            if (nrow(data) > 0) {\\n",
            "                data <- data %>%\\n",
            "                    dplyr::filter(concept_id == !!concept_id | source_concept_id == !!concept_id) %>%\\n",
            "                    dplyr::select(datetime, value_as_number)\\n\\n",
            "                if (nrow(data) > 0) {\\n",
            "                    fake_data <- tibble::tibble(\\n",
            "                        datetime = c(data_datetimes_range[[1]] - lubridate::seconds(1), data_datetimes_range[[2]] + lubridate::seconds(1)),\\n",
            "                        value_as_number = c(NA, NA)\\n",
            "                    )\\n\\n",
            "                    data <- dplyr::bind_rows(fake_data, data)\\n",
            "                    data <- data %>% dplyr::arrange(datetime)\\n\\n",
            "                    features[[paste0('concept_', concept_id)]] <- xts::xts(data$value_as_number, data$datetime)\\n",
            "                    features_names <- c(features_names, concept$concept_name)\\n",
            "                }\\n",
            "            }\\n",
            "        }\\n",
            "    }\\n",
            "}\\n\\n",
            "if (length(features) > 0) combined_features <- do.call(merge, features)\\n",
            "if (length(features_names) > 0) colnames(combined_features) <- features_names"
        )
        
        code <- paste0(code, "\\n\\n", "if (length(combined_features) > 0){\\n    ")
        
        if (isTRUE(input$synchronize_timelines_%widget_id%)) code <- paste0(code, "fig <- \\n", "        dygraphs::dygraph(combined_features, group = 'tab_%tab_id%') %>%\\n")
        else code <- paste0(code, "fig <- \\n", "        dygraphs::dygraph(combined_features) %>%\\n")
            
        code <- paste0(
            code,
            "        dygraphs::dyOptions(drawPoints = TRUE, pointSize = 2, useDataTimezone = TRUE) %>%\\n",
            "        dygraphs::dyRangeSelector(dateWindow = c(\\n",
            "            format(datetimes[[1]], '%Y-%m-%d %H:%M:%S'),\\n",
            "            format(datetimes[[2]], '%Y-%m-%d %H:%M:%S')\\n",
            "        )) %>%\\n",
            "        dygraphs::dyAxis('y', valueRange = c(0, NA))\\n",
            "}\\n\\n",
            "fig"
        )
        
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
    if (!isTRUE(input$run_code_on_data_update_%widget_id%) || length(input$data_source_%widget_id%) == 0 || input$data_source_%widget_id% != "person") return()
        
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
    
    if (!isTRUE(input$run_code_on_data_update_%widget_id%) || length(input$data_source_%widget_id%) == 0 || input$data_source_%widget_id% != "visit_detail") return()
        
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
    
    fig <- character()
    
    # To test code without plugin aceEditor, comment Part 1 and uncomment Part 2
    
    ## Part 1 - run code from aceEditor
    
    eval(parse(text = m$code_%widget_id%))
    
    ## Part 2 - run code directly without aceEditor (dev mode)
    
    # data_source <- "visit_detail"
    # if (length(input$data_source_%widget_id%) > 0) data_source <- input$data_source_%widget_id%
    
    # concepts <- selected_concepts %>% dplyr::filter(concept_id %in% input$concepts_%widget_id%)
    
    # features <- list()
    # features_names <- c()
    # raw_data <- tibble::tibble()
    # data_datetimes_range <- c()
    # combined_features <- c()
    
    # sql <- glue::glue_sql("
        # SELECT measurement_concept_id AS concept_id, measurement_source_concept_id AS source_concept_id, measurement_datetime AS datetime, value_as_number
        # FROM measurement 
        # WHERE {DBI::SQL(paste0(data_source, '_id'))} = {m[[paste0('selected_', data_source)]]} AND (measurement_concept_id IN ({concepts$concept_id*}) OR measurement_source_concept_id IN ({concepts$concept_id*}))
        # UNION
        # SELECT observation_concept_id AS concept_id, observation_source_concept_id AS source_concept_id, observation_datetime AS datetime, value_as_number
        # FROM observation 
        # WHERE {DBI::SQL(paste0(data_source, '_id'))} = {m[[paste0('selected_', data_source)]]} AND (observation_concept_id IN ({concepts$concept_id*}) OR observation_source_concept_id IN ({concepts$concept_id*}))
        # ", .con = d$con)
    # raw_data <- DBI::dbGetQuery(d$con, sql) %>% tibble::as_tibble()
    
    # if (data_source == "person") {
        
        # if (!is.na(m$selected_person)){
            # sql <- glue::glue_sql("
                # SELECT MIN(visit_start_datetime) AS min_visit_start_datetime, MAX(visit_end_datetime) AS max_visit_end_datetime
                # FROM visit_occurrence
                # WHERE person_id = {m$selected_person}
            # ", .con = d$con)
            # data_datetimes_range <- DBI::dbGetQuery(d$con, sql)
        # }
    # }
    # else if (data_source == "visit_detail") {
    
        # if (!is.na(m$selected_visit_detail)){
            # sql <- glue::glue_sql("
                # SELECT MIN(visit_detail_start_datetime) AS min_visit_start_datetime, MAX(visit_detail_end_datetime) AS max_visit_end_datetime
                # FROM visit_detail
                # WHERE visit_detail_id = {m$selected_visit_detail}
            # ", .con = d$con)
            # data_datetimes_range <- DBI::dbGetQuery(d$con, sql)
        # }
    # }
    
    # if (length(data_datetimes_range) > 0){
        # data_datetimes_range <- c(data_datetimes_range$min_visit_start_datetime, data_datetimes_range$max_visit_end_datetime)
        # m$data_datetimes_range_%widget_id% <- data_datetimes_range
    # }
    
    # datetimes <- data_datetimes_range
    
    # if (isTRUE(input$synchronize_timelines_%widget_id%)){
        # if(!is.null(m$debounced_datetimes_timeline_%tab_id%)) if (length(m$debounced_datetimes_timeline_%tab_id%()) > 0) datetimes <- m$debounced_datetimes_timeline_%tab_id%()
    # }
    
    # if (length(datetimes) > 0) m$datetimes_%widget_id% <- datetimes
    
    # for (concept_id in concepts$concept_id){
    
        # concept <- concepts %>% dplyr::filter(concept_id == !!concept_id)
    
        # if (concept$domain_id %in% c("Measurement", "Observation")){
        
            # data <- raw_data
        
            # if (nrow(data) > 0){
                # data <-
                    # data %>%
                    # dplyr::filter(concept_id == !!concept_id | source_concept_id == !!concept_id) %>%
                    # dplyr::select(datetime, value_as_number)
                
                # if (nrow(data) > 0){
                    # fake_data <- tibble::tibble(
                        # datetime = c(data_datetimes_range[[1]] - lubridate::seconds(1), data_datetimes_range[[2]] + lubridate::seconds(1)),
                        # value_as_number = c(NA, NA)
                    # )
                    
                    # data <- dplyr::bind_rows(fake_data, data)
                    # data <- data %>% dplyr::arrange(datetime)
                
                    # features[[paste0("concept_", concept_id)]] <- xts::xts(data$value_as_number, data$datetime)
                    # features_names <- c(features_names, concept$concept_name)
                # }
            # }
        # }
    # }
    
    # if (length(features) > 0) combined_features <- do.call(merge, features)
    # if (length(features_names) > 0) colnames(combined_features) <- features_names
    
    # if (length(combined_features) > 0){
        # if (isTRUE(input$synchronize_timelines_%widget_id%)) fig <- dygraphs::dygraph(combined_features, group = "tab_%tab_id%")
        # else fig <- dygraphs::dygraph(combined_features)
        
        # fig <-
            # fig %>%
            # dygraphs::dyOptions(drawPoints = TRUE, pointSize = 2, useDataTimezone = TRUE) %>%
            # dygraphs::dyRangeSelector(dateWindow = c(
                # format(datetimes[[1]], "%Y-%m-%d %H:%M:%S"),
                # format(datetimes[[2]], "%Y-%m-%d %H:%M:%S")
            # )) %>%
            # dygraphs::dyAxis("y", valueRange = c(0, NA))
    # }
    
    ## End of part 2
    
    if (length(fig) == 0){
        output$error_message_%widget_id% <- renderUI(div(shiny.fluent::MessageBar(i18np$t("no_data_to_display"), messageBarType = 5), style = "display: inline-block;"))
    
        shinyjs::show("error_message_div_%widget_id%")
        shinyjs::hide("dygraph_div_%widget_id%")
    }
    
    if (length(fig) > 0){
        output$dygraph_%widget_id% <- dygraphs::renderDygraph(fig)
        
        shinyjs::hide("error_message_div_%widget_id%")
        shinyjs::show("dygraph_div_%widget_id%")
    }
    
    # Go to figure tab
    if (length(input$figure_and_settings_side_by_side_%widget_id%) > 0) if (!input$figure_and_settings_side_by_side_%widget_id%) shinyjs::click("figure_button_%widget_id%")
})

# Save code with shortcut
observe_event(input$code_%widget_id%_save, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_params_and_code_%widget_id%', Math.random());")))
