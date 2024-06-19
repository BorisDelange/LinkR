#' @noRd 
mod_vocabularies_ui <- function(id, language, languages, i18n, code_hotkeys){
  ns <- NS(id)
  
  pivot_item_js <- paste0("
    Shiny.setInputValue('", id, "-current_tab', this.id);
    Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());"
  )
  
  div(class = "main",
      
      # Load widget UI ----
      
      mod_widgets_ui(id, language, languages, i18n),
      
      # Vocabulary details ----
      
      shinyjs::hidden(
        div(
          id = ns("one_element"),
          div(
            uiOutput(ns("breadcrumb")),
            div(
              id = ns("vocabulary_pivot"),
              tags$button(id = ns("summary"), i18n$t("summary"), class = "pivot_item selected_pivot_item", onclick = pivot_item_js),
              tags$button(id = ns("concepts"), i18n$t("concepts"), class = "pivot_item", onclick = pivot_item_js),
              tags$button(id = ns("edit_code"), i18n$t("code"), class = "pivot_item", onclick = pivot_item_js),
              tags$button(id = ns("share"), i18n$t("share"), class = "pivot_item", onclick = pivot_item_js),
              class = "pivot"
            ),
            style = "display:flex; justify-content:space-between;"
          ),
          
          ## Summary ----
          div(
            id = ns("summary_div"),
            div(
              div(
                h1(i18n$t("informations")),
                uiOutput(ns("vocabulary_summary")),
                div(
                  div(shiny.fluent::PrimaryButton.shinyInput(ns("delete_element"), i18n$t("delete")), class = "delete_button"),
                  class = "create_element_modal_buttons"
                ),
                class = "widget", style = "height: 50%;"
              ),
              div(
                # h1(i18n$t("vocabulary_tables")),
                # tags$ul(
                #   tags$li(
                #     shiny.fluent::Link(
                #       href = "", paste0(i18n$t("concept_table"), " (concept)"), target = "_blank",
                #       onClick = htmlwidgets::JS(paste0(
                #         "item => {",
                #           "Shiny.setInputValue('", id, "-show_vocabulary_table', Math.random());",
                #           "Shiny.setInputValue('", id, "-show_vocabulary_table_type', 'concept');",
                #         "}"
                #     )))
                #   ),
                #   tags$li(
                #     shiny.fluent::Link(
                #       href = "", paste0(i18n$t("concept_relationship_table"), " (concept_relationship)"), target = "_blank",
                #       onClick = htmlwidgets::JS(paste0(
                #         "item => {",
                #         "Shiny.setInputValue('", id, "-show_vocabulary_table', Math.random());",
                #         "Shiny.setInputValue('", id, "-show_vocabulary_table_type', 'concept_relationship');",
                #         "}"
                #       )))
                #   )
                # ),
                class = "widget", style = "height: 50%;"
              ),
              class = "vocabularies_summary_left"
            ),
            div(
              div(
                h1(i18n$t("description")),
                class = "widget", style = "height: calc(100% - 25px); padding-top: 1px;"
              ),
              class = "vocabularies_summary_right"
            ),
            class = "vocabularies_summary_container"
          ),
          
          ## Concepts ----
          shinyjs::hidden(
            div(
              id = ns("concepts_div"),
              div(
                div(
                  div(
                    div(
                      shiny.fluent::Dropdown.shinyInput(
                        ns("vocabulary_table"),
                        label = i18n$t("table"),
                        options = list(
                          list(key = "concept", text = paste0(i18n$t("concept_table"), " (concept)")),
                          list(key = "concept_relationship", text = paste0(i18n$t("concept_relationship_table"), " (concept_relationship)")),
                          list(key = "concept_synonym", text = paste0(i18n$t("concept_synonym_table"), " (concept_synonym)")),
                          list(key = "concept_ancestor", text = paste0(i18n$t("concept_ancestor_table"), " (concept_ancestor)")),
                          list(key = "drug_strength", text = paste0(i18n$t("drug_strength_table"), " (drug_strength)"))
                        )
                      ),
                      style = "width: 300px;"
                    ),
                    div(
                      shiny.fluent::Dropdown.shinyInput(ns("primary_concepts_dt_cols"), multiSelect = TRUE, label = i18n$t("columns"),
                        options = list(
                          list(key = 0, text = i18n$t("concept_id")),
                          list(key = 1, text = i18n$t("concept_name")),
                          list(key = 2, text = i18n$t("domain_id")),
                          list(key = 3, text = i18n$t("concept_class_id")),
                          list(key = 4, text = i18n$t("standard_concept")),
                          list(key = 5, text = i18n$t("concept_code")),
                          list(key = 6, text = i18n$t("valid_start_date")),
                          list(key = 7, text = i18n$t("valid_end_date")),
                          list(key = 8, text = i18n$t("invalid_reason"))
                        ),
                        value = c(0, 1, 2, 3)
                      ),
                      style = "width: 200px;"
                    ),
                    style = "display: flex; gap: 5px;"
                  ),
                  DT::DTOutput(ns("primary_concepts_dt")),
                  class = "widget", style = "height: 50%; padding: 10px 15px; overflow: auto;"
                ),
                div(
                  DT::DTOutput(ns("secondary_concepts_dt")),
                  class = "widget", style = "height: 50%;"
                ),
                class = "vocabularies_concepts"
              ),
              div(
                div(
                  uiOutput(ns("primary_concepts_details_ui")),
                  class = "widget", style = "height: 50%;"
                ),
                div(
                  uiOutput(ns("secondary_concepts_details_ui")),
                  class = "widget", style = "height: 50%;"
                ),
                class = "vocabularies_concepts"
              ),
              class = "vocabularies_concepts_container"
            )
          ),
          
          ## Edit code ----
          shinyjs::hidden(
            div(
              id = ns("edit_code_div"),
              div(
                shinyAce::aceEditor(
                  ns("vocabulary_code"), value = "", mode = "r",
                  code_hotkeys = list("r", code_hotkeys),
                  autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, fontSize = 11, showPrintMargin = FALSE
                ),
                class = "element_ace_editor"
              ),
              div(
                verbatimTextOutput(ns("code_result")),
                class = "element_code_result"
              ),
              style = "height: 100%; display: flex;"
            )
          ),
          
          ## Share ----
          shinyjs::hidden(
            div(
              id = ns("share_div"),
              style = "height: 100%;"
            )
          ),
          
          style = "height: 100%; display: flex; flex-direction: column;"
        )
      )
  )
}

#' @noRd 
mod_vocabularies_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), language = "en", i18n = character(), debug = FALSE){
  # |-------------------------------- -----
  
  if (debug) cat(paste0("\n", now(), " - mod_vocabularies - ", id, " - start"))
  
  # Load widgets ----
  
  all_divs <- c("summary", "concepts", "edit_code", "share")
  mod_widgets_server(id, r, d, m, language, i18n, all_divs, debug)
  
  # Vocabularies module ----
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # |-------------------------------- -----
    
    # --- --- --- --- --- ---
    # Vocabulary summary ----
    # --- --- --- --- --- ---
    
    # |-------------------------------- -----
    
    # --- --- --- --- --- --
    # Vocabulary tables ----
    # --- --- --- --- --- --
    
    ## Load vocabulary tables ----
    
    observeEvent(input$load_vocabulary_tables, {
      if (debug) cat(paste0("\n", now(), " - mod_vocabularies - ", id, " - observer input$load_vocabulary_tables"))
      
      vocabulary_id <- r$vocabularies_wide %>% dplyr::filter(id == input$selected_element) %>% dplyr::pull(vocabulary_id)
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-selected_vocabulary_id', '", vocabulary_id, "');"))
      
      for (table in c("concept", "concept_relationship", "concept_synonym", "concept_ancestor", "drug_strength")) m[[table]] <- tibble::tibble()
    })
    
    ## Concepts main table ----
    
    observeEvent(input$vocabulary_table, {
      if (debug) cat(paste0("\n", now(), " - mod_vocabularies - ", id, " - observer input$vocabulary_table"))
      
      table <- input$vocabulary_table
      vocabulary_id <- input$selected_vocabulary_id
      
      if (nrow(m$concept) == 0) m$concept <- DBI::dbGetQuery(m$db, glue::glue_sql("SELECT * FROM concept WHERE vocabulary_id = {vocabulary_id}", .con = m$db))
      
      if (table == "concept"){
        
        data <- m$concept %>% dplyr::select(-id, -vocabulary_id) %>% dplyr::mutate_at("concept_id", as.character) %>% dplyr::arrange(concept_id)
        
        sortable_cols <- c("concept_id", "concept_name", "domain_id", "concept_class_id", "standard_concept", "valid_start_date", "valid_end_date", "invalid_reason")
        searchable_cols <- c("concept_id", "concept_name", "domain_id", "concept_class_id", "standard_concept", "concept_code", "invalid_reason")
        factorize_cols <- c("domain_id", "concept_class_id", "standard_concept", "invalid_reason")
        hidden_cols <- c("standard_concept", "concept_code", "valid_start_date", "valid_end_date", "invalid_reason")
        editable_cols <- "concept_name"
      }
      
      else if (table == "concept_relationship"){
        
        # It's faster to load all concept_relationship table in memory to perform the inner join (at least with 15M rows)
        
        if (nrow(m$concept_relationship) == 0) m$concept_relationship <- DBI::dbGetQuery(m$db, glue::glue_sql(paste0(
          "SELECT cr.concept_id_1, c2.concept_name AS concept_name_1, cr.relationship_id, cr.concept_id_2, c2.concept_name AS concept_name_2, ",
          "cr.valid_start_date, cr.valid_end_date, cr.invalid_reason ",
          "FROM concept_relationship cr ",
          "INNER JOIN concept c1 ON cr.concept_id_1 = c1.concept_id ",
          "LEFT JOIN concept c2 ON cr.concept_id_2 = c2.concept_id ",
          "WHERE c1.vocabulary_id = {vocabulary_id}"), .con = m$db))
        
        data <- m$concept_relationship
        
        sortable_cols <- c("concept_id_1", "concept_id_2", "relationship_id", "valid_start_date", "valid_end_date", "invalid_reason")
        searchable_cols <- c("concept_id_1", "concept_id_2", "relationship_id", "invalid_reason")
        factorize_cols <- c("relationship_id", "invalid_reason")
        hidden_cols <- c("valid_start_date", "valid_end_date", "invalid_reason")
        editable_cols <- ""
      }
      
      else if (table == "concept_synonym"){
        if (nrow(m$concept_synonym) == 0) m$concept_synonym <- DBI::dbGetQuery(m$db, glue::glue_sql(paste0(
          "SELECT cs.concept_id, c1.concept_name, cs.concept_synonym_name, cs.language_concept_id, c2.concept_name AS language_concept_name ",
          "FROM concept_synonym cs ",
          "INNER JOIN concept c1 ON cs.concept_id = c1.concept_id ",
          "LEFT JOIN concept c2 ON cs.language_concept_id = c2.concept_id ",
          "WHERE c1.vocabulary_id = {vocabulary_id}"), .con = m$db))
        
        data <- m$concept_synonym
        
        sortable_cols <- c("concept_id", "concept_name", "concept_synonym_name", "language_concept_id", "language_concept_name")
        searchable_cols <- c("concept_id", "concept_name", "concept_synonym_name", "language_concept_id", "language_concept_name")
        factorize_cols <- c("language_concept_id", "language_concept_name")
        hidden_cols <- ""
        editable_cols <- "concept_synonym_name"
      }
      
      else if (table == "concept_ancestor"){
        if (nrow(m$concept_ancestor) == 0) m$concept_ancestor <- DBI::dbGetQuery(m$db, glue::glue_sql(paste0(
          "SELECT ca.ancestor_concept_id, c1.concept_name AS ancestor_concept_name, ca.descendant_concept_id, c2.concept_name AS descendant_concept_name, ",
          "ca.min_levels_of_separation, ca.max_levels_of_separation ",
          "FROM concept_ancestor ca ",
          "INNER JOIN concept c1 ON ca.ancestor_concept_id = c1.concept_id ",
          "LEFT JOIN concept c2 ON ca.descendant_concept_id = c2.concept_id ",
          "WHERE c1.vocabulary_id = {vocabulary_id}"), .con = m$db))
        
        data <- m$concept_ancestor
        
        sortable_cols <- c("ancestor_concept_id", "ancestor_concept_name", "descendant_concept_id", "descendant_concept_name")
        searchable_cols <-  c("ancestor_concept_id", "ancestor_concept_name", "descendant_concept_id", "descendant_concept_name")
        factorize_cols <- ""
        hidden_cols <- ""
        editable_cols <- ""
      }
      
      else if (table == "drug_strength"){
        
      }
      
      col_names <- unname(sapply(colnames(data), i18n$t))
      
      render_datatable(
        output = output, ns = ns, i18n = i18n, data = data, hidden_cols = hidden_cols, editable_cols = editable_cols,
        output_name = "primary_concepts_dt", datatable_dom = "<'top't><'bottom'p>", sortable_cols = sortable_cols,
        col_names = col_names, searchable_cols = searchable_cols, factorize_cols = factorize_cols, filter = TRUE
      )
      
      r$primary_concepts_dt_proxy <- DT::dataTableProxy("primary_concepts_dt", deferUntilFlush = FALSE)
    })
    
    observeEvent(input$primary_concepts_dt_cols, {
      if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$primary_concepts_dt_cols"))
      
      req(r$primary_concepts_dt_proxy)
      
      r$primary_concepts_dt_proxy %>%
        DT::showCols(0:8) %>%
        DT::hideCols(setdiff(0:8, input$primary_concepts_dt_cols))
    })
    
    
    ## Concepts secondary table ----
    
    
    
    # |-------------------------------- -----
    
    # --- --- --- --- --- --- -
    # Edit vocabulary code ----
    # --- --- --- --- --- --- -
    
  })
}