#' @noRd 
mod_concepts_ui <- function(id, language, languages, i18n, dropdowns){
  ns <- NS(id)
  
  div(
    class = "main",
    div(
      div(
        div(
          div(
            div(shiny.fluent::Dropdown.shinyInput(ns("vocabulary"), label = i18n$t("vocabulary")), style = "width: 200px;"),
            div(
              shiny.fluent::Dropdown.shinyInput(ns("primary_concepts_dt_cols"), multiSelect = TRUE, label = i18n$t("columns"),
                options = convert_tibble_to_list(dropdowns$concept_with_counts %>% dplyr::mutate_at("text", i18n$t), key_col = "key", text_col = "text"), value = c(1, 3, 4, 11, 12)
              ), 
              style = "width: 200px;"
            ),
            style = "display: flex; gap: 5px;"
          ),
          br(),
          DT::DTOutput(ns("primary_concepts_dt")),
          class = "widget", style = "height: 100%; overflow: auto; padding: 10px 15px;"
        ),
        class = "concepts_left"
      ),
      div(
        div(
          uiOutput(ns("primary_concept_info")),
          class = "widget", style = "height: 50%; display: flex; overflow: auto; padding: 15px 10px 5px 10px;"
        ),
        div(
          plotOutput(ns("primary_concept_plot"), height = "100%"),
          class = "widget", style = "height: 50%; padding: 10px;"
        ),
        class = "concepts_right"
      ),
      class = "concepts_container",
    ),
    
    # Settings modal ----
    shinyjs::hidden(
      div(
        id = ns("settings_modal"),
        div(
          div(
            tags$h1(i18n$t("settings")),
            shiny.fluent::IconButton.shinyInput(ns("close_settings_modal"), iconProps = list(iconName = "ChromeClose")),
            class = "concepts_settings_modal_head small_close_button"
          ),
          div(
            class = "concepts_settings_modal_body",
          ),
          class = "concepts_settings_modal_content"
        ),
        class = "concepts_settings_modal"
      )
    ),
    
    # Reload dataset concepts count modal ----
    shinyjs::hidden(
      div(
        id = ns("reload_concepts_count_modal"),
        div(
          tags$h1(i18n$t("reload_concepts_count_title")), tags$p(i18n$t("reload_concepts_count_text")),
          div(
            shiny.fluent::DefaultButton.shinyInput(ns("close_reload_concepts_count_modal"), i18n$t("dont_reload")),
            div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_reload_concepts_count"), i18n$t("reload"))),
            class = "reload_concepts_count_modal_buttons"
          ),
          class = "reload_concepts_count_modal_content"
        ),
        class = "reload_concepts_count_modal"
      )
    )
  )
}

#' @noRd 
mod_concepts_server <- function(id, r, d, m, language, i18n, debug, user_accesses){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (debug) cat(paste0("\n", now(), " - mod_concepts - start"))
    
    # Current user accesses ----
    
    if ("concepts_reload_dataset_concepts" %in% user_accesses) shinyjs::show("reload_concepts_count_button")
    
    # Reload vocabulary dropdown ----
    
    observeEvent(r$dataset_vocabularies, {
      if (debug) cat(paste0("\n", now(), " - mod_concepts - ", id, " - observer r$dataset_vocabularies"))

      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_vocabulary_dropdown', Math.random());"))
    })
    
    observeEvent(input$reload_vocabulary_dropdown, {
      if (debug) cat(paste0("\n", now(), " - mod_concepts - ", id, " - observer input$reload_vocabulary_dropdown"))
      
      if (nrow(r$dataset_vocabularies) > 0){
        # Reload vocabulary dropdown
        dropdown_options <-
          r$dataset_vocabularies %>% 
          dplyr::select(vocabulary_id) %>%
          dplyr::mutate(text = vocabulary_id) %>%
          dplyr::bind_rows(tibble::tibble(vocabulary_id = "all_vocabularies", text = i18n$t("all_vocabularies"))) %>%
          dplyr::arrange(vocabulary_id != "all_vocabularies", vocabulary_id) %>%
          convert_tibble_to_list(key_col = "vocabulary_id", text_col = "text")
        
        shiny.fluent::updateDropdown.shinyInput(session, "vocabulary", options = dropdown_options, value = NULL)
      }
      else shiny.fluent::updateDropdown.shinyInput(session, "vocabulary", options = list(), value = NULL)
        
      # Reload concepts datatable
      shinyjs::delay(100, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_concepts_dt', Math.random());")))
      
      # Reset UI
      output$primary_concept_info <- renderUI("")
      shinyjs::hide("primary_concept_plot")
    })
    
    # Reload concepts datatable ----
    
    # Reload datatable when vocabulary is updated
    observeEvent(input$vocabulary, {
      if (debug) cat(paste0("\n", now(), " - mod_concepts - ", id, " - observer input$vocabulary"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_concepts_dt', Math.random());"))
    })
    
    # Reload datatable
    observeEvent(input$reload_concepts_dt, {
      if (debug) cat(paste0("\n", now(), " - mod_concepts - ", id, " - observer input$reload_concepts_dt"))
      
      data <- tibble::tibble(
        concept_id = character(), concept_name = character(), concept_display_name = character(), domain_id = character(), vocabulary_id = character(),
        concept_class_id = character(), standard_concept = character(), concept_code = character(), valid_start_date = character(),
        valid_end_date = character(), invalid_reason = character(), count_persons_rows = integer(), count_concepts_rows = integer()
      )
      
      if (length(input$vocabulary) > 0){
        if (nrow(d$dataset_concept) > 0){
          if (input$vocabulary == "all_vocabularies") data <- d$dataset_concept
          else data <- d$dataset_concept %>% dplyr::filter(vocabulary_id == input$vocabulary)
          data <- 
            data %>% 
            dplyr::select(-add_concept_input) %>%
            dplyr::mutate_at("concept_id", as.character)
        }
      }
      
      r$concepts_dt_data <- data %>% dplyr::arrange(dplyr::desc(count_concepts_rows))
      
      sortable_cols <- c(
        "concept_id", "concept_name", "concept_display_name", "domain_id", "vocabulary_id", 
        "concept_class_id", "standard_concept", "concept_code", "valid_start_date", "valid_end_date", 
        "invalid_reason", "count_persons_rows", "count_concepts_rows"
      )
      searchable_cols <- c(
        "concept_id", "concept_name", "concept_display_name", "domain_id", 
        "vocabulary_id", "concept_class_id", "standard_concept", "concept_code", "invalid_reason"
      )
      factorize_cols <- c("domain_id", "vocabulary_id", "concept_class_id", "standard_concept", "invalid_reason")
      editable_cols <- "concept_display_name"
      column_widths <- c("concept_name" = "200px", "concept_display_name" = "200px")
      
      if (length(input$primary_concepts_dt_cols) > 0) hidden_cols <- r$dropdowns$concept_with_counts %>% dplyr::filter(key %not_in% input$primary_concepts_dt_cols) %>% dplyr::pull(text)
      else hidden_cols <- c("concept_id", "concept_display_name", "concept_class_id", "standard_concept", "concept_code", "valid_start_date", "valid_end_date", "invalid_reason")
      
      col_names <- unname(sapply(colnames(data), i18n$t))
      
      render_datatable(
        output = output, ns = ns, i18n = i18n, data = r$concepts_dt_data, editable_cols = editable_cols, hidden_cols = hidden_cols, column_widths = column_widths,
        output_name = "primary_concepts_dt", sortable_cols = sortable_cols, page_length = 25,
        col_names = col_names, searchable_cols = searchable_cols, factorize_cols = factorize_cols, filter = TRUE
      )
      
      r$concepts_primary_concepts_dt_proxy <- DT::dataTableProxy("primary_concepts_dt", deferUntilFlush = FALSE)
    })
    
    # Show / hide cols
    observeEvent(input$primary_concepts_dt_cols, {
      if (debug) cat(paste0("\n", now(), " - mod_concepts - observer input$primary_concepts_dt_cols"))
      
      req(r$concepts_primary_concepts_dt_proxy)
      
      r$concepts_primary_concepts_dt_proxy %>%
        DT::showCols(0:12) %>%
        DT::hideCols(setdiff(0:12, input$primary_concepts_dt_cols))
    })
    
    # A concept is selected ----
    observeEvent(input$primary_concepts_dt_rows_selected, {
      if (debug) cat(paste0("\n", now(), " - mod_concepts - observer input$primary_concepts_dt_rows_selected"))
      
      concept <- r$concepts_dt_data[input$primary_concepts_dt_rows_selected, ]
      
      # Show concept informations
      
      concept_ui <- tagList()
      
      for (col in colnames(concept)){
        concept_ui <- tagList(
          concept_ui,
          tags$tr(tags$td(tags$span(tags$strong(i18n$t(col)), style = "display: block; margin-right: 10px;")), tags$td(concept[[col]])),
        )
      }
      
      concept_ui <- tags$table(concept_ui)
      
      output$primary_concept_info <- renderUI(concept_ui)
      
      # Show concept distribution
      
      if (concept$domain_id == "Measurement"){
        concept_id <- as.integer(concept$concept_id)
        
        data <-
          d$measurement %>%
          dplyr::select(measurement_concept_id, measurement_source_concept_id, value_as_number) %>%
          dplyr::filter(measurement_concept_id == concept_id | measurement_source_concept_id == concept_id, !is.na(value_as_number)) %>%
          dplyr::collect()
        
        # binwidth <- (max(data$value_as_number) - min(data$value_as_number)) / (1 + log2(nrow(data)))
        
        plot <- 
          data %>%
          ggplot2::ggplot(ggplot2::aes(x = value_as_number)) +
          ggplot2::geom_histogram(color = "white", fill = "#0084D8", bins = 50) +
          ggplot2::theme_minimal() +
          ggplot2::labs(x = i18n$t("value"), y = i18n$t("occurrences"))
        
        output$primary_concept_plot <- renderPlot(plot)
        shinyjs::show("primary_concept_plot")
      }
      
      else shinyjs::hide("primary_concept_plot")
    })
    
    # Reload dataset concepts count ----
    
    observeEvent(input$reload_concepts_count, {
      if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$reload_concepts_count"))
      shinyjs::show("reload_concepts_count_modal")
    })

    observeEvent(input$close_reload_concepts_count_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$close_reload_concepts_count_modal"))
      shinyjs::hide("reload_concepts_count_modal")
    })

    observeEvent(input$confirm_reload_concepts_count, {
      if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$confirm_reload_concepts_count"))
      
      # Remove concept files
      # Also remove duckDB tables
      dataset_folder <- paste0(r$app_folder, "/datasets_files/", r$selected_dataset)
      
      for (table in c("concept", "concept_ancestor", "concept_relationship", "concept_synonym", "drug_strength", "vocabulary", "concept_class", "relationship", "domain", "dataset_concept", "dataset_drug_strength")){
        if (r$dataset_data_source == "disk") DBI::dbExecute(d$con, paste0("DROP VIEW IF EXISTS ", table)) 
        file_path <- file.path(dataset_folder, paste0(table, ".parquet"))
        if (file.exists(file_path)) unlink(file_path)
      }

      # Reload concepts count
      tryCatch(load_dataset_concepts(), error = function(e){
        cat(paste0("\n", now(), " - mod_concepts - error loading dataset concepts - error = ", toString(e)))
        show_message_bar(id, output, "error_loading_dataset_concepts", "severeWarning", i18n = i18n, ns = ns)
      })
      
      # Reset fields
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_vocabulary_dropdown', Math.random());"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_concepts_dt', Math.random())"))

      shinyjs::hide("reload_concepts_count_modal")
    })
  })
}
