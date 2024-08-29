#' @noRd 
mod_vocabularies_ui <- function(id, language, languages, i18n, code_hotkeys, dropdowns){
  ns <- NS(id)
  
  pivot_item_js <- paste0("
    Shiny.setInputValue('", id, "-current_tab', this.id);
    Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());"
  )
  
  div(
    class = "main",
      
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
              class = "widget", style = "height: 50%;"
            ),
            class = "vocabularies_summary_left"
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
                      options = dropdowns$concept,
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
        
        # ## Edit code ----
        # shinyjs::hidden(
        #   div(
        #     id = ns("edit_code_div"),
        #     div(
        #       shinyAce::aceEditor(
        #         ns("vocabulary_code"), value = "", mode = "r",
        #         code_hotkeys = list("r", code_hotkeys),
        #         autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, fontSize = 11, showPrintMargin = FALSE
        #       ),
        #       class = "element_ace_editor"
        #     ),
        #     div(
        #       verbatimTextOutput(ns("code_result")),
        #       class = "element_code_result"
        #     ),
        #     style = "height: 100%; display: flex;"
        #   )
        # ),
        # 
        # ## Share ----
        # shinyjs::hidden(
        #   div(
        #     id = ns("share_div"),
        #     style = "height: 100%;"
        #   )
        # ),
        
        style = "height: 100%; display: flex; flex-direction: column;"
      )
    ),
    
    # Create a vocabulary modal ----
    
    shinyjs::hidden(
      div(
        id = ns("create_element_modal"),
        div(
          div(
            tags$h1(i18n$t("create_vocabulary")),
            shiny.fluent::IconButton.shinyInput(ns("close_create_element_modal"), iconProps = list(iconName = "ChromeClose")),
            class = "create_element_modal_head small_close_button"
          ),
          div(
            div(shiny.fluent::TextField.shinyInput(ns("element_creation_name"), label = i18n$t("name")), style = "width: 200px;"),
            div(
              shiny.fluent::PrimaryButton.shinyInput(ns("add_element"), i18n$t("add")),
              class = "create_element_modal_buttons"
            ),
          ),
          class = "create_vocabulary_modal_content"
        ),
        class = "create_element_modal"
      )
    ),
    
    # Import concepts modal ----
    
    shinyjs::hidden(
      div(
        id = ns("import_concepts_modal"),
        div(
          div(
            tags$h1(i18n$t("import_concepts_title")),
            shiny.fluent::IconButton.shinyInput(ns("close_import_concepts_modal_1"), iconProps = list(iconName = "ChromeClose")),
            class = "import_concepts_modal_head small_close_button"
          ),
          div(
            div(
              shiny.fluent::DefaultButton.shinyInput(ns("select_files"), i18n$t("choose_files")),
              div(fileInput(ns("select_files_input"), label = "", multiple = TRUE, accept = c(".zip", ".csv")), style = "display:none;"),
              uiOutput(ns("selected_files"), style = "margin-top: 10px;"),
              div(
                shinyjs::hidden(div(id = ns("import_files_div"), shiny.fluent::PrimaryButton.shinyInput(ns("import_files"), i18n$t("import")))),
                div(id = ns("close_import_concepts_modal_2_div"), shiny.fluent::DefaultButton.shinyInput(ns("close_import_concepts_modal_2"), i18n$t("close"))),
                class = "import_concepts_modal_buttons"
              ),
              style = "width: 50%; overflow: auto;"
            ),
            div(
              DT::DTOutput(ns("inserted_concepts_dt")),
              style = "width: 50%; overflow: auto;"
            ),
            style = "display: flex; gap: 10px;"
          ),
          class = "import_concepts_modal_content"
        ),
        class = "import_concepts_modal"
      )
    )
  )
}

#' @noRd 
mod_vocabularies_server <- function(id, r, d, m, language, i18n, debug, user_accesses){
  # |-------------------------------- -----
  
  if (debug) cat(paste0("\n", now(), " - mod_vocabularies - ", id, " - start"))
  
  # Initiate vars ----
  authorized_concept_files <- c("concept", "domain", "concept_class", "concept_relationship", "relationship", "concept_synonym", "concept_ancestor", "drug_strength", "vocabulary")
  
  # Load widgets ----
  
  all_divs <- c("summary", "concepts")
  mod_widgets_server(id, r, d, m, language, i18n, all_divs, debug, user_accesses)
  
  # Vocabularies module ----
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Current user accesses ----
    
    if ("vocabularies_management" %in% user_accesses) sapply(c("create_element_button", "edit_summary_div", "delete_element_div"), shinyjs::show)
    if ("vocabularies_import" %in% user_accesses) sapply(c("import_concepts_div_1", "import_concepts_div_2"), shinyjs::show)
    
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
        if (nrow(m$drug_strength) == 0) m$drug_strength <- DBI::dbGetQuery(m$db, glue::glue_sql(paste0(
          "SELECT ds.drug_concept_id, c1.concept_name AS drug_concept_name, ds.ingredient_concept_id, c2.concept_name AS ingredient_concept_name, ",
          "ds.amount_value, ds.amount_unit_concept_id, c3.concept_name AS amount_unit_concept_name, ds.numerator_value, ",
          "ds.numerator_unit_concept_id, c4.concept_name AS numerator_unit_concept_name, ds.denominator_value, ",
          "ds.denominator_unit_concept_id, c5.concept_name AS denominator_concept_name, ds.box_size, ",
          "ds.valid_start_date, ds.valid_end_date, ds.invalid_reason ",
          "FROM drug_strength ds ",
          "INNER JOIN concept c1 ON ds.drug_concept_id = c1.concept_id ",
          "LEFT JOIN concept c2 ON ds.ingredient_concept_id = c2.concept_id ",
          "LEFT JOIN concept c3 ON ds.amount_unit_concept_id = c3.concept_id ",
          "LEFT JOIN concept c4 ON ds.numerator_unit_concept_id = c4.concept_id ",
          "LEFT JOIN concept c5 ON ds.denominator_unit_concept_id = c5.concept_id ",
          "WHERE c1.vocabulary_id = {vocabulary_id}"), .con = m$db))
        
        data <- m$drug_strength
        
        # sortable_cols <- c("drug_concept_id", "ingredient_concept_id", "descendant_concept_id", "descendant_concept_name")
        # searchable_cols <-  c("ancestor_concept_id", "ancestor_concept_name", "descendant_concept_id", "descendant_concept_name")
        sortable_cols <- ""
        searchable_cols <- ""
        factorize_cols <- ""
        hidden_cols <- ""
        editable_cols <- ""
      }
      
      col_names <- unname(sapply(colnames(data), i18n$t))
      
      render_datatable(
        output = output, ns = ns, i18n = i18n, data = data, hidden_cols = hidden_cols, editable_cols = editable_cols,
        output_name = "primary_concepts_dt", datatable_dom = "<'top't><'bottom'p>", sortable_cols = sortable_cols,
        col_names = col_names, searchable_cols = searchable_cols, factorize_cols = factorize_cols, filter = TRUE,
        column_widths = c("table_name" = "100px", "n_rows" = "100px")
      )
      
      r$vocabularies_primary_concepts_dt_proxy <- DT::dataTableProxy("primary_concepts_dt", deferUntilFlush = FALSE)
    })
    
    observeEvent(input$primary_concepts_dt_cols, {
      if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$primary_concepts_dt_cols"))
      
      req(r$vocabularies_primary_concepts_dt_proxy)
      
      r$vocabularies_primary_concepts_dt_proxy %>%
        DT::showCols(0:8) %>%
        DT::hideCols(setdiff(0:8, input$primary_concepts_dt_cols))
    })
    
    
    ## Concepts secondary table ----
    
    ## Import concepts ----
    
    # Show / hide import concepts modal
    
    observeEvent(input$import_concepts_1, {
      if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$import_concepts_1"))
      shinyjs::show("import_concepts_modal")
    })
    observeEvent(input$import_concepts_2, {
      if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$import_concepts_2"))
      shinyjs::show("import_concepts_modal")
    })
    
    observeEvent(input$close_import_concepts_modal_1, {
      if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$close_import_concepts_modal"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-close_import_concepts_modal', Math.random());"))
    })
    
    observeEvent(input$close_import_concepts_modal_2, {
      if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$close_import_concepts_modal_2"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-close_import_concepts_modal', Math.random());"))
    })
    
    observeEvent(input$close_import_concepts_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$close_import_concepts_modal"))
      
      req("vocabularies_import" %in% user_accesses)
      
      # Reset fields
      render_datatable(
        output = output, ns = ns, i18n = i18n, data = tibble::tibble(table_name = character(), n_rows = character(), message = character()),
        output_name = "inserted_concepts_dt", datatable_dom = "<'top't><'bottom'p>", col_names = c(i18n$t("table_name"), i18n$t("rows_inserted"), i18n$t("message")),
        column_widths = c("table_name" = "100px", "n_rows" = "100px")
      )
      shinyjs::hide("inserted_concepts_dt")
      
      output$selected_files <- renderUI(div())
      
      shinyjs::hide("import_concepts_modal")
    })
    
    # Show file browser input
    
    observeEvent(input$select_files, {
      if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$select_files"))
      shinyjs::click("select_files_input")
    })
    
    # Select files
    
    observeEvent(input$select_files_input, {
      if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$select_files_input"))
      
      req("vocabularies_import" %in% user_accesses)
      
      req(length(input$select_files_input) > 0)
      
      # Change button, from close to import
      shinyjs::hide("close_import_concepts_modal_2_div")
      shinyjs::show("import_files_div")
      
      # Create files to import UI
      
      files_ui <- tagList()
      
      for (filename in input$select_files_input$name){
        file_ext <- sub(".*\\.", "", tolower(filename))
        
        concept_style <- "selected_file"
        
        if (file_ext %in% c("csv", "zip")){
          concept_style <- paste0(concept_style, " ", file_ext, "_file")
        
          files_ui <- tagList(
            files_ui,
            div(
              create_hover_card(ui = div(filename, class = concept_style), text = filename),
              style = "display: flex; margin: 2px 0;"
            )
          )
        }
      }
      
      output$selected_files <- renderUI(files_ui)
      
      # Reset datatable
      render_datatable(
        output = output, ns = ns, i18n = i18n, data = tibble::tibble(table_name = character(), n_rows = integer(), message = character()),
        output_name = "inserted_concepts_dt", datatable_dom = "<'top't><'bottom'p>",
        sortable_cols = c("table_name", "n_rows", "message"), col_names = c(i18n$t("table_name"), i18n$t("rows_inserted"), i18n$t("message")),
        column_widths = c("table_name" = "100px", "n_rows" = "100px")
      )
    })
    
    # Import files
    
    # CSV col types
    col_types <- list(
      concept = "iccccccccc",
      vocabulary = "cccci",
      domain = "cci",
      concept_class = "cci",
      concept_relationship = "iicccc",
      relationship = "ccccci",
      concept_synonym = "ici",
      concept_ancestor = "iiii",
      drug_strength = "iinininiiccc"
    )
    
    observeEvent(input$import_files, {
      if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$import_files"))
      
      req("vocabularies_import" %in% user_accesses)
      
      req(length(input$select_files_input) > 0)
      
      # Change button, from import to close
      shinyjs::hide("import_files_div")
      sapply(c("close_import_concepts_modal_2_div", "inserted_concepts_dt"), shinyjs::show)
      
      # Add new vocabularies ?
      add_vocabulary <- input$import_new_vocabularies
      
      # Reset count rows var
      inserted_data <- tibble::tibble(table_name = character(), n_rows = character(), message = character())
      
      files <- input$select_files_input %>% tibble::as_tibble()
      
      for (i in 1:nrow(files)){
        
        row <- files[i, ]
        file_ext <- sub(".*\\.", "", tolower(row$name))
        
        if (file_ext == "csv"){
          
          table_name <- tolower(substr(row$name, 1, nchar(row$name) - 4))
          
          if (table_name %in% authorized_concept_files){
            
            # Insert new data
            new_inserted_data <- load_concepts_from_csv_file(r, m, table_name, row$datapath, col_types[[table_name]])
            inserted_data <- inserted_data %>% dplyr::bind_rows(new_inserted_data)
          }
        }
        
        else if (file_ext == "zip"){
          
          # Extract zip file
          temp_dir <- paste0(r$app_folder, "/temp_files/", r$user_id, "/vocabularies/", now() %>% stringr::str_replace_all(":| |-", ""), paste0(sample(c(0:9, letters[1:6]), 24, TRUE), collapse = ''))
          zip::unzip(row$datapath, exdir = temp_dir)
          zip_files <- zip::zip_list(row$datapath)
          
          for(filename in zip_files$filename){
            
            if (grepl(".csv$", filename)){
              
              table_name <- tolower(substr(filename, 1, nchar(filename) - 4))
              
              if (table_name %in% authorized_concept_files){
                
                file_path <- paste0(temp_dir, "/", filename)
                
                # Insert new data
                new_inserted_data <- load_concepts_from_csv_file(r, m, table_name, file_path, col_types[[table_name]])
                inserted_data <- inserted_data %>% dplyr::bind_rows(new_inserted_data)
              }
            }
          }
        }
      }
      
      # Show inserted data
      render_datatable(
        output = output, ns = ns, i18n = i18n, data = inserted_data, output_name = "inserted_concepts_dt", datatable_dom = "<'top't><'bottom'p>",
        sortable_cols = c("table_name", "n_rows", "message"), col_names = c(i18n$t("table_name"), i18n$t("rows_inserted"), i18n$t("message")),
        column_widths = c("table_name" = "100px", "n_rows" = "100px")
      )
      
      # Reload widgets
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_elements_var', Math.random());"))
    })
    
    # |-------------------------------- -----
    
    # --- --- --- --- --- --- -
    # Edit vocabulary code ----
    # --- --- --- --- --- --- -
    
    # |-------------------------------- -----
    
    # --- --- --- --- --- -
    # Module functions ----
    # --- --- --- --- --- -
    
    load_concepts_from_csv_file <- function(r, m, table_name, file_path, col_types, add_vocabulary){
      
      # Load CSV file
      data <- vroom::vroom(file_path, col_types = col_types, progress = FALSE)
      
      if ("valid_start_date" %in% names(data)) data <- data %>% dplyr::mutate_at(c("valid_start_date", "valid_end_date"), lubridate::ymd)
      
      res <- import_vocabulary_table(r = r, m = m, table_name = table_name, data = data)
      return(tibble::tibble(table_name = table_name, n_rows = res[1], message = res[2]))
    }
  })
}
