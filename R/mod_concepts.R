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
          DT::DTOutput(ns("primary_concepts_dt")),
          class = "widget", style = "height: 50%; overflow: auto; padding: 10px 15px;"
        ),
        div(
          class = "widget", style = "height: 50%;"
        ),
        class = "concepts_left"
      ),
      div(
        div(
          div(
            uiOutput(ns("primary_concept_info")),
            style = "width: 50%;"
          ),
          div(
            plotOutput(ns("primary_concept_plot")),
            style = "width: 50%;"
          ),
          class = "widget", style = "height: 50%; display: flex; padding: 15px;"
        ),
        div(
          class = "widget", style = "height: 50%;"
        ),
        class = "concepts_right"
      ),
      class = "concepts_container",
    )
  )
}

#' @noRd 
mod_concepts_server <- function(id, r, d, m, language, i18n, debug){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (debug) cat(paste0("\n", now(), " - mod_concepts - start"))
    
    # Reload vocabulary dropdown ----
    observeEvent(r$dataset_vocabularies, {
      if (debug) cat(paste0("\n", now(), " - mod_concepts - ", id, " - observer r$dataset_vocabularies"))
      
      # Reload vocabulary dropdown
      dropdown_options <- r$dataset_vocabularies %>% 
        dplyr::select(vocabulary_id) %>%
        dplyr::mutate(text = vocabulary_id) %>%
        dplyr::bind_rows(tibble::tibble(vocabulary_id = "all_vocabularies", text = i18n$t("all_vocabularies"))) %>%
        dplyr::arrange(vocabulary_id != "all_vocabularies", vocabulary_id) %>%
        convert_tibble_to_list(key_col = "vocabulary_id", text_col = "text")
      
      shiny.fluent::updateDropdown.shinyInput(session, "vocabulary", options = dropdown_options, value = NULL)
      
      # Reload concepts datatable
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_concepts_dt', Math.random());"))
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
        if (nrow(d$concept) > 0){
          if (input$vocabulary == "all_vocabularies") data <- d$concept
          else data <- d$concept %>% dplyr::filter(vocabulary_id == input$vocabulary)
          data <- 
            data %>% 
            dplyr::select(-id, -add_concept_input) %>%
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
        output_name = "primary_concepts_dt", datatable_dom = "<'top't><'bottom'p>", sortable_cols = sortable_cols,
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
          dplyr::select(measurement_concept_id, value_as_number) %>%
          dplyr::filter(measurement_concept_id == concept_id) %>%
          dplyr::collect()
        
        plot <- 
          data %>%
          ggplot2::ggplot(ggplot2::aes(x = value_as_number)) +
          ggplot2::geom_histogram(color = "white", fill = "#0084D8", bins = 50) +
          ggplot2::theme_minimal() +
          ggplot2::labs(x = i18n$t("value"), y = i18n$t("occurrences"))
      }
      
      else plot <- ggplot2::ggplot() + ggplot2::theme_void()
      
      output$primary_concept_plot <- renderPlot(plot)
    })
  })
}