#' patient_and_aggregated_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_data_ui <- function(id = character(), language = "en", languages = tibble::tibble(), i18n = character()){
  ns <- NS(id)
  result <- ""
  language <- "EN"
  
  # category depending on page id
  if (id == "patient_level_data"){
    category <- "patient_lvl"
    page_name <- "patient_level_data"
  } 
  if (id == "aggregated_data"){
    category <- "aggregated"
    page_name <- "aggregated_data"
  }
  
  # --- --- --- --- -- -
  # Add a tab modal ----
  # --- --- --- --- -- -
  
  add_tab_modal <- shinyjs::hidden(
    div(
      id = ns("add_tab_modal"),
      div(
        div(
          tags$h1(i18n$t("add_a_tab")),
          shiny.fluent::IconButton.shinyInput(ns("close_add_tab_modal"), iconProps = list(iconName = "ChromeClose")),
          class = "create_element_modal_head small_close_button"
        ),
        div(
          shiny.fluent::ChoiceGroup.shinyInput(
            ns("add_tab_type"), value = "same_level", 
            options = list(
              list(key = "same_level", text = i18n$t("same_level_current_tab")),
              list(key = "level_under", text = i18n$t("level_under"))
            ),
            className = "inline_choicegroup"
          ),
          make_textfield(i18n, ns, id = "tab_name", label = "name", width = "200px"),
          class = "create_element_modal_body"
        ),
        div(
          shiny.fluent::PrimaryButton.shinyInput(ns("add_tab_button"), i18n$t("add")),
          class = "create_element_modal_buttons"
        ),
        class = "create_tab_modal_content"
      ),
      class = "create_element_modal"
    )
  )
  
  # --- --- --- --- --- ---
  # Add a widget modal ----
  # --- --- --- --- --- ---
  
  add_widget_modal <- shinyjs::hidden(
    div(
      id = ns("add_widget_modal"),
      div(
        div(
          tags$h1(i18n$t("add_a_widget")),
          shiny.fluent::IconButton.shinyInput(ns("close_add_widget_modal"), iconProps = list(iconName = "ChromeClose")),
          class = "create_element_modal_head small_close_button"
        ),
        div(
          make_textfield(i18n, ns, id = "widget_creation_name", label = "name", width = "200px"),
          make_combobox(i18n, ns, id = "widget_creation_plugin", label = "plugin", allowFreeform = FALSE, multiSelect = FALSE, width = "200px"),
          div(
            make_combobox(i18n, ns, id = "widget_creation_vocabulary", label = "vocabulary", allowFreeform = FALSE, multiSelect = FALSE, width = "200px"),
            make_dropdown(i18n, ns, id = "widget_creation_vocabulary_concepts_table_cols", label = "columns", multiSelect = TRUE,
              options = list(
                list(key = 0, text = i18n$t("concept_id")),
                list(key = 1, text = i18n$t("concept_name")),
                list(key = 2, text = i18n$t("concept_display_name")),
                list(key = 3, text = i18n$t("domain_id")),
                list(key = 4, text = i18n$t("concept_class_id")),
                list(key = 5, text = i18n$t("standard_concept")),
                list(key = 6, text = i18n$t("concept_code")),
                list(key = 7, text = i18n$t("num_patients")),
                list(key = 8, text = i18n$t("num_rows")),
                list(key = 9, text = i18n$t("action"))
              ),
              value = c(0, 1, 2, 7, 8, 9) ,
              width = "200px"
            ),
            style = "display: flex; gap: 10px;"
          ),
          div(id = ns("widget_creation_vocabulary_selected_concepts_title"), class = "input_title", i18n$t("vocabulary_selected_concepts")),
          div(
            shiny.fluent::Dropdown.shinyInput(ns("widget_creation_vocabulary_selected_concepts"), value = NULL, options = list(), multiSelect = TRUE,
            onChanged = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-widget_creation_vocabulary_selected_concepts_trigger', Math.random())"))),
            style = "width:410px;"
          ),
          div(DT::DTOutput(ns("widget_creation_vocabulary_concepts")), class = "vocabulary_table"),
          class = "create_element_modal_body"
        ),
        div(
          shiny.fluent::PrimaryButton.shinyInput(ns("widget_creation_save"), i18n$t("add")),
          class = "create_element_modal_buttons"
        ),
        class = "create_widget_modal_content"
      ),
      class = "create_element_modal"
    )
  )
  
  # --- --- --- --- --- --- --
  # Delete a widget modal ----
  # --- --- --- --- --- --- --
  
  delete_wigdet_modal <- shinyjs::hidden(
    div(
      id = ns("delete_widget_modal"),
      div(
        tags$h1(i18n$t("delete_widget_title")), tags$p(i18n$t("delete_widget_text")),
        div(
          shiny.fluent::DefaultButton.shinyInput(ns("close_widget_deletion_modal"), i18n$t("dont_delete")),
          shiny.fluent::PrimaryButton.shinyInput(ns("confirm_widget_deletion"), i18n$t("delete")),
          class = "delete_modal_buttons"
        ),
        class = "delete_modal_content"
      ),
      class = "delete_modal"
    )
  )
  
  # --- --- --- --- --- -
  # Tab edition card ----
  # --- --- --- --- --- -
  
  # tab_edition_card <- make_card(
  #   title = i18n$t("edit_tab"),
  #   content = div(
  #     actionButton(ns(paste0(category, "_close_edit_tab")), "", icon = icon("xmark"), style = "position:absolute; top:10px; right:10px;"),
  #     make_textfield(ns = ns, label = "name", id = "edit_tab_name", width = "300px", i18n = i18n), br(),
  #     shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
  #       shiny.fluent::PrimaryButton.shinyInput(ns("edit_tab_save"), i18n$t("save")),
  #       shiny.fluent::DefaultButton.shinyInput(ns("remove_tab"), i18n$t("delete_tab"))
  #     ), 
  #     br()
  #   )
  # )
  
  div(
    class = "main",
    add_tab_modal,
    add_widget_modal,
    delete_wigdet_modal,
    shinyjs::hidden(uiOutput(ns("study_menu"))),
    div(id = ns("study_cards"), style = "overflow: hidden;"),
    shinyjs::hidden(
      div(
        id = ns(paste0(category, "_no_tabs_to_display")),
        make_card(shiny.fluent::MessageBar(i18n$t("no_tabs_to_display_click_add_tab"), messageBarType = 5))
      )
    )
  )
}

#' patient_and_aggregated_data Server Functions
#'
#' @noRd 

mod_data_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), language = "en", i18n = character(), debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # |-------------------------------- -----
    
    if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - start"))
    
    # --- --- --- --- --
    # Initiate vars ----
    # --- --- --- --- --
    
    if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - initiate vars"))
    
    # category depending on page id
    if (id == "patient_level_data"){
      category <- "patient_lvl"
      page_name <- "patient_level_data"
    } 
    if (id == "aggregated_data"){
      category <- "aggregated"
      page_name <- "aggregated_data"
    }
    
    # Initiate var for already loaded studies, so that a UI element is not loaded twice
    r[[paste0(category, "_loaded_studies")]] <- integer()
    
    # Initiate var for list of cards
    r[[paste0(category, "_cards")]] <- character()
    
    # --- --- --- --- --- --- --
    # A project is selected ----
    # --- --- --- --- --- --- --
    
    visit_detail_tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement", "observation", "note", "note_nlp", "fact_relationship", "payer_plan_period", "cost")
    person_tables <- c(visit_detail_tables, "specimen", "death", "drug_era", "dose_era", "condition_era")
    subset_tables <- c(person_tables, "person", "observation_period", "visit_occurrence", "visit_detail")
    main_tables <- c(subset_tables, "location", "care_site", "provider")
    
    observeEvent(m$selected_study, {
      
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer m$selected_study"))
      
      # Reset data variables
      sapply(subset_tables, function(table) d$data_subset[[table]] <- tibble::tibble())
      sapply(person_tables, function(table) d$data_person[[table]] <- tibble::tibble())
      sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())
      
      req(!is.na(m$selected_study))
      
      # Subsets depending on the selected study
      update_r(r = r, m = m, table = "subsets")

      # Reset selected_subset, selected_person & selected_visit_detail
      m$selected_subset <- NA_integer_
      m$selected_person <- NA_integer_
      m$selected_visit_detail <- NA_integer_

      # Select patients belonging to subsets of this study
      update_r(r = r, m = m, table = "subsets_persons")
      
      shinyjs::show("study_menu")
      
      # Reset selected key
      r[[paste0(category, "_selected_tab")]] <- NA_integer_
      
      # Reset displayed tabs
      r[[paste0(category, "_opened_cards")]] <- ""
      
      # Load tabs
      r[[paste0(category, "_reload_tabs")]] <- paste0("ui_first_load_", now())
      
      # Run observers
      r[[paste0(category, "_load_server")]] <- now()
      
      r[[paste0(category, "_load_ui_stage")]] <- "first_time"
      
      # Load tabs variables for this study
      update_r(r = r, m = m, table = paste0(category, "_tabs_groups"))
      update_r(r = r, m = m, table = paste0(category, "_tabs"))
      update_r(r = r, m = m, table = paste0(category, "_widgets"))
      update_r(r = r, m = m, table = paste0(category, "_widgets_concepts"))
      
      r[[paste0(category, "_reload_plugins_dropdown")]] <- now()
      
      if (category == "patient_lvl"){
        # Load data
        sql <- glue::glue_sql("SELECT dataset_id FROM studies WHERE id = {m$selected_study}", .con = r$db)
        r$selected_dataset <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(dataset_id)
        
        # Load concepts
        r$load_dataset_all_concepts <- now()
      }
    })
    
    # --- --- --- --
    # Load data ----
    # --- --- --- --
    
    # Load only once between patient_lvl and aggregated modules
    if (category == "patient_lvl"){
      
      ## Dataset ----
      observeEvent(r$selected_dataset, {
        if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " observer r$selected_dataset"))
        
        # Get OMOP version for this dataset
        omop_version <- r$options %>% dplyr::filter(category == "dataset" & link_id == r$selected_dataset & name == "omop_version") %>% dplyr::pull(value)
        m$omop_version <- omop_version

        # Try to load dataset
        tryCatch({

          captured_output <- capture.output(run_dataset_code(output, r = r, d = d, m = m, dataset_id = r$selected_dataset, i18n = i18n))

          # If an error occured
          if (grepl(paste0("\\*\\*", i18n$t("error"), "\\*\\*"), toString(captured_output))){
            show_message_bar(output, "fail_load_dataset", "severeWarning", i18n = i18n, ns = ns)
            report_bug(r = r, output = output, error_message = "fail_load_dataset",
              error_name = paste0(id, " - run server code"), category = "Error", error_report = toString(captured_output), i18n = i18n)
          }
          else {
            show_message_bar(output, "import_dataset_success", "success", i18n = i18n, ns = ns)
            # r$load_scripts <- now()
          }
        },
        error = function(e){
          show_message_bar(output, "fail_load_dataset", "severeWarning", i18n = i18n, ns = ns)
          report_bug(r = r, output = output, error_message = "fail_load_dataset",
            error_name = paste0(id, " - run server code"), category = "Error", error_report = toString(e), i18n = i18n)
        })
      })
      
      ## Subset ----
      observeEvent(m$selected_subset, {
        if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " observer m$selected_subset"))
        
        req(!is.na(m$selected_subset))
        
        for(table in subset_tables){
          if (nrow(m$subset_persons) > 0){
            if (d[[table]] %>% dplyr::count() %>% dplyr::pull() > 0){
              person_ids <- m$subset_persons$person_id
              d$data_subset[[table]] <- d[[table]] %>% dplyr::filter(person_id %in% person_ids)
            }
            else d$data_subset[[table]] <- tibble::tibble()
          }
          else d$data_subset[[table]] <- tibble::tibble()
        }
      })
      
      ## Patient ----
      observeEvent(m$selected_person, {
        if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " observer m$selected_person"))
        
        req(!is.na(m$selected_person))
        
        selected_person <- m$selected_person
        
        for(table in person_tables){
          if (d$data_subset[[table]] %>% dplyr::count() %>% dplyr::pull() > 0){
            d$data_person[[table]] <- d$data_subset[[table]] %>% dplyr::filter(person_id == selected_person)
          } 
        }
      })
      
      ## Stay ----
      observeEvent(m$selected_visit_detail, {
        if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " observer m$selected_visit_detail"))
        
        req(!is.na(m$selected_visit_detail))
        
        selected_visit_detail <- m$selected_visit_detail
        
        for(table in visit_detail_tables){
          if (d$data_person[[table]] %>% dplyr::count() %>% dplyr::pull() > 0){
            d$data_visit_detail[[table]] <- d$data_person[[table]] %>% dplyr::filter(visit_detail_id == selected_visit_detail)
          }
        }
      })
    }
    
    # --- --- --- --- --- --
    # Sidenav dropdowns ----
    # --- --- --- --- --- --
    
      ## Subset ----
      
      observeEvent(m$subsets, {
        if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " observer m$subsets"))
        
        # Update subset dropdown
        if (nrow(m$subsets) == 0) shiny.fluent::updateComboBox.shinyInput(session, "subset", options = list(), value = NULL, errorMessage = i18n$t("no_subset_available"))
        if (nrow(m$subsets) > 0) shiny.fluent::updateComboBox.shinyInput(session, "subset", options = convert_tibble_to_list(m$subsets, key_col = "id", text_col = "name"), value = NULL)
        
        # Reset other dropdowns & uiOutput
        if (category == "patient_lvl"){
          sapply(c("person", "visit_detail"), function(name) {
            shiny.fluent::updateComboBox.shinyInput(session, name, options = list(), value = NULL)
            shinyjs::hide(paste0(name, "_div"))
          })
          output$person_info <- renderUI("")
          shinyjs::hide("person_info_div")
        }
      })
    
      observeEvent(input$subset, {
        if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " observer input$subset"))
        
        req(input$subset$key)
        
        # Prevent multiple changes of m$selected_subset
        # We have to keep multiple observers, cause we use input variable
        if (is.na(m$selected_subset)) m$selected_subset <- input$subset$key
        if (!is.na(m$selected_subset) & m$selected_subset != input$subset$key) m$selected_subset <- input$subset$key
        
        # Reset data var
        if (category == "patient_lvl"){
          sapply(person_tables, function(table) d$data_person[[table]] <- tibble::tibble())
          sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())
        }

        # Select patients who belong to this subset
        update_r(r = r, m = m, table = "subset_persons")

        # If this subset contains no patient, maybe the code has not been run yet
        if (nrow(m$subset_persons) == 0){
          subset_code <- r$code %>% dplyr::filter(category == "subset" & link_id == m$selected_subset) %>% dplyr::pull(code) %>%
            stringr::str_replace_all("%dataset_id%", as.character(r$selected_dataset)) %>%
            stringr::str_replace_all("%subset_id%", as.character(m$selected_subset)) %>%
            stringr::str_replace_all("\r", "\n") %>%
            stringr::str_replace_all("''", "'")

          tryCatch(eval(parse(text = subset_code)),
            error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_execute_subset_code",
              error_name = paste0("sidenav - execute_subset_code  - id = ", m$selected_subset), category = "Error", error_report = toString(e), i18n = i18n, ns = ns)
          )

          update_r(r = r, m = m, table = "subset_persons")
        }

        # Reset selected_person & selected_visit_detail
        m$selected_person <- NA_integer_
        m$selected_visit_detail <- NA_integer_
      })
    
      # observeEvent(m$selected_subset, {
      #   if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " observer m$selected_subset"))
      #   
      #   shiny.fluent::updateComboBox.shinyInput(session, "subset", options = convert_tibble_to_list(m$subsets, key_col = "id", text_col = "name"), value = list(key = m$selected_subset))
      # })
    
      ## Patient ----
      
      if (category == "patient_lvl"){
        
        # Update patients dropdown
        observeEvent(m$subset_persons, {
          if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " observer m$subset_persons"))
          
          persons <- tibble::tibble()
          
          if (nrow(m$subset_persons) > 0 & d$person %>% dplyr::count() %>% dplyr::pull() > 0){
            person_ids <- m$subset_persons$person_id
            
            persons <- d$person %>% dplyr::filter(person_id %in% person_ids) %>% dplyr::collect() %>%
              dplyr::left_join(d$dataset_all_concepts %>% dplyr::filter(is.na(relationship_id)) %>% dplyr::select(gender_concept_id = concept_id_1, gender_concept_name = concept_name_1), by = "gender_concept_id")
          }
          
          if (nrow(persons) == 0){
            # Set selected_person to NA, not to display a chart when no person is selected
            m$selected_person <- NA_integer_
            shiny.fluent::updateComboBox.shinyInput(session, "person", options = list(), value = NULL, errorMessage = i18n$t("no_person_in_subset")) 
          }
          
          if (nrow(persons) > 0){
            # Order persons by person_id
            persons <- persons %>% dplyr::arrange(person_id)
            
            # Update persons dropdown
            shiny.fluent::updateComboBox.shinyInput(session, "person", 
              options = convert_tibble_to_list(data = persons %>% dplyr::slice_head(n = 100) %>% dplyr::mutate(name_display = paste0(person_id, " - ", gender_concept_name)),
                key_col = "person_id", text_col = "name_display"), value = NULL)
          }
          
          # Reset other dropdowns & uiOutput
          shiny.fluent::updateComboBox.shinyInput(session, "visit_detail", options = list(), value = NULL)
          output$person_info <- renderUI("")
          sapply(c("visit_detail", "person_info"), function(name) shinyjs::hide(paste0(name, "_div")))
          shinyjs::show("person_div")
        })
      
        # When a patient is searched
        observeEvent(input$person_trigger, {
          
          if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " observer input$person_trigger"))
          
          input_value <- input$person_trigger
          if (nchar(input_value) >= 2) {
            filtered_person <- d$person
            if ("tbl_lazy" %in% class(d$person)) filtered_person <- filtered_person %>%
                dplyr::filter(dplyr::sql(paste0("CAST(person_id AS TEXT) LIKE '%", input_value, "%'")))
            else filtered_person <- filtered_person %>%
                dplyr::filter(stringr::str_detect(person_id, stringr::regex(input_value, ignore_case = TRUE)))
            filtered_person <- filtered_person %>%
              dplyr::collect() %>%
              dplyr::slice_head(n = 100) %>%
              dplyr::left_join(d$dataset_all_concepts %>% dplyr::filter(is.na(relationship_id)) %>% dplyr::select(gender_concept_id = concept_id_1, gender_concept_name = concept_name_1), by = "gender_concept_id") %>%
              dplyr::mutate(name_display = paste0(person_id, " - ", gender_concept_name))
            
            shiny.fluent::updateComboBox.shinyInput(session, "person", options = convert_tibble_to_list(filtered_person, key_col = "person_id", text_col = "name_display"))
          } else {
            shiny.fluent::updateComboBox.shinyInput(session, "person", 
              options = convert_tibble_to_list(data = d$person %>% 
                dplyr::arrange(person_id) %>%
                dplyr::collect() %>%
                dplyr::slice_head(n = 100) %>% 
                dplyr::left_join(d$dataset_all_concepts %>% dplyr::filter(is.na(relationship_id)) %>% dplyr::select(gender_concept_id = concept_id_1, gender_concept_name = concept_name_1), by = "gender_concept_id") %>%
                dplyr::mutate(name_display = paste0(person_id, " - ", gender_concept_name)),
              key_col = "person_id", text_col = "name_display"), value = NULL)
          }
        })
        
        # When a patient is selected
        observeEvent(input$person, {
          if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " observer input$person"))
          
          # Check if the entry exists
          
          req(input$person$text)
          
          if (length(input$person$key) == 0){
            person_text <- input$person$text
            person <- d$person %>%
              dplyr::left_join(d$dataset_all_concepts %>% dplyr::filter(is.na(relationship_id)) %>% dplyr::select(gender_concept_id = concept_id_1, gender_concept_name = concept_name_1), by = "gender_concept_id") %>%
              dplyr::mutate(name_display = paste0(person_id, " - ", gender_concept_name)) %>%
              dplyr::filter(name_display == person_text) %>% dplyr::collect()
          }
          if (length(input$person$key) > 0){
            person_key <- input$person$key
            person <- d$person %>% dplyr::filter(person_id == person_key) %>% dplyr::collect() %>%
              dplyr::left_join(d$dataset_all_concepts %>% dplyr::filter(is.na(relationship_id)) %>% dplyr::select(gender_concept_id = concept_id_1, gender_concept_name = concept_name_1), by = "gender_concept_id") %>%
              dplyr::mutate(name_display = paste0(person_id, " - ", gender_concept_name))
          }
          req(nrow(person) == 1)
          
          person_id <- person$person_id
          m$selected_person <- person_id
          
          # Reset variables
          sapply(person_tables, function(table) d$data_person[[table]] <- tibble::tibble())
          sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())

          # Reset selected_visit_detail
          m$selected_visit_detail <- NA_integer_
          
          no_stay_available <- FALSE
          if (d$visit_detail %>% dplyr::count() %>% dplyr::pull() == 0) no_stay_available <- TRUE
          if (d$visit_detail %>% dplyr::count() %>% dplyr::pull() > 0) if (d$visit_detail %>% dplyr::filter(person_id == !!person_id) %>% dplyr::count() %>% dplyr::pull() == 0) no_stay_available <- TRUE
          
          if (no_stay_available) shiny.fluent::updateComboBox.shinyInput(session, "visit_detail", options = list(), value = NULL, errorMessage = i18n$t("no_stay_available"))
          
          if (!no_stay_available){
            
            visit_detail <- d$visit_detail %>% dplyr::filter(person_id == !!person_id, is.na(visit_detail_parent_id)) %>% dplyr::collect() %>% 
              dplyr::left_join(d$dataset_all_concepts %>% dplyr::filter(is.na(relationship_id)) %>% dplyr::select(visit_detail_concept_id = concept_id_1, visit_detail_concept_name = concept_name_1), by = "visit_detail_concept_id") %>%
              dplyr::arrange(visit_detail_start_datetime)
            
            if ("visit_detail_concept_name" %in% colnames(visit_detail)){
              if (tolower(language) == "fr") visit_details <- convert_tibble_to_list(data = visit_detail %>% dplyr::mutate(name_display = paste0(visit_detail_concept_name, " - ",
                format(as.POSIXct(visit_detail_start_datetime), format = "%d-%m-%Y"), " ", tolower(i18n$t("to")), " ",  format(as.POSIXct(visit_detail_end_datetime), format = "%d-%m-%Y"))),
                 key_col = "visit_detail_id", text_col = "name_display")
              else visit_details <- convert_tibble_to_list(data = visit_detail %>% dplyr::mutate(name_display = paste0(visit_detail_concept_name, " - ",
                format(as.POSIXct(visit_detail_start_datetime), format = "%Y-%m-%d"), " ", tolower(i18n$t("to")), " ",  format(as.POSIXct(visit_detail_end_datetime), format = "%Y-%m-%d"))),
                key_col = "visit_detail_id", text_col = "name_display")
            }
            else {
              if (tolower(language) == "fr") visit_details <- convert_tibble_to_list(data = visit_detail %>% dplyr::mutate(name_display = paste0(visit_detail_concept_id, " - ",
                format(as.POSIXct(visit_detail_start_datetime), format = "%d-%m-%Y"), " ", tolower(i18n$t("to")), " ",  format(as.POSIXct(visit_detail_end_datetime), format = "%d-%m-%Y"))),
                key_col = "visit_detail_id", text_col = "name_display")
              else visit_details <- convert_tibble_to_list(data = visit_detail %>% dplyr::mutate(name_display = paste0(visit_detail_concept_id, " - ",
                format(as.POSIXct(visit_detail_start_datetime), format = "%Y-%m-%d"), " ", tolower(i18n$t("to")), " ",  format(as.POSIXct(visit_detail_end_datetime), format = "%Y-%m-%d"))),
                key_col = "visit_detail_id", text_col = "name_display")
            }
            
            # Load visit_details of the person & update dropdown
            shiny.fluent::updateComboBox.shinyInput(session, "visit_detail", options = visit_details, value = NULL)
          }
          
          # Update person informations on sidenav
          style <- "display:inline-block; width:80px; font-weight:bold;"
          output$person_info <- renderUI({
            selected_person <- m$selected_person
            tagList(
              span(i18n$t("person_id"), style = style), person$person_id, br(),
              span(i18n$t("gender"), style = style), person$gender_concept_name
            )
          })
          
          sapply(c("visit_detail", "person_info"), function(name) shinyjs::show(paste0(name, "_div")))
          
          # Reset variables
          sapply(person_tables, function(table) d$data_person[[table]] <- tibble::tibble())
          sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())
          
          if (length(m$selected_person) > 0){
            selected_person <- m$selected_person
            for(table in person_tables) if (d$data_subset[[table]] %>% dplyr::count() %>% dplyr::pull() > 0) d$data_person[[table]] <- 
                d$data_subset[[table]] %>% dplyr::filter(person_id == selected_person)
          }
          
          # Reset selected_visit_detail
          m$selected_visit_detail <- NA_integer_
        })
      }
      
      ## Stay ----
      
      if (category == "patient_lvl"){
        observeEvent(input$visit_detail, {
          if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " observer input$visit_detail"))
          
          m$selected_visit_detail <- input$visit_detail$key
          
          # Update person informations on sidenav
          
          style <- "display:inline-block; width:80px; font-weight:bold;"
          
          person_id <- m$selected_person
          person <- d$person %>% dplyr::filter(person_id == !!person_id) %>% dplyr::collect() %>%
            dplyr::left_join(d$dataset_all_concepts %>% dplyr::filter(is.na(relationship_id)) %>% dplyr::select(gender_concept_id = concept_id_1, gender_concept_name = concept_name_1), by = "gender_concept_id")
          
          visit_detail_id <- input$visit_detail$key
          visit_detail <- d$visit_detail %>% dplyr::filter(visit_detail_id == !!visit_detail_id) %>% dplyr::collect() %>%
            dplyr::left_join(d$dataset_all_concepts %>% dplyr::filter(is.na(relationship_id)) %>% dplyr::select(visit_detail_concept_id = concept_id_1, visit_detail_concept_name = concept_name_1), by = "visit_detail_concept_id")
          
          if (!is.na(person$birth_datetime)) age <- lubridate::interval(person$birth_datetime, visit_detail$visit_detail_start_datetime) / lubridate::years(1)
          else if (is.na(person$birth_datetime) & !is.na(person$year_of_birth)) age <- as.numeric(format(visit_detail$visit_detail_start_datetime, "%Y")) - person$year_of_birth
          else age <- NA_integer_
          
          age_div <- tagList(round(age, 0), " ", i18n$t("years"))
          if (!is.na(age) & age <= 2) age_div <- tagList(round(age * 12, 0), " ", i18n$t("months"))
          
          if ("visit_detail_concept_name" %in% names(visit_detail)) visit_detail_concept_name <- visit_detail %>% dplyr::pull(visit_detail_concept_name)
          else visit_detail_concept_name <- visit_detail %>% dplyr::pull(visit_detail_concept_id)
          
          output$person_info <- renderUI({
            tagList(
              span(i18n$t("person_id"), style = style), m$selected_person, br(),
              span(i18n$t("gender"), style = style), person$gender_concept_name, br(), br(),
              span(i18n$t("visit_detail_id"), style = style), m$selected_visit_detail, br(),
              span(i18n$t("age"), style = style), age_div, br(),
              span(i18n$t("hosp_unit"), style = style), visit_detail_concept_name, br(),
              span(i18n$t("from"), style = style), visit_detail$visit_detail_start_datetime %>% format_datetime(language), br(),
              span(i18n$t("to"), style = style), visit_detail$visit_detail_end_datetime %>% format_datetime(language))
          })
        })
        
        sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())
        
        if (length(m$selected_visit_detail) > 0){
          selected_visit_detail <- m$selected_visit_detail
          for(table in visit_detail_tables) if (d$data_person[[table]] %>% dplyr::count() %>% dplyr::pull() > 0) d$data_visit_detail[[table]] <- 
            d$data_person[[table]] %>% dplyr::filter(visit_detail_id == selected_visit_detail)
        }
      }
      
      # --- --- --- --- -- -
      # Other dropdowns ----
      # --- --- --- --- -- -
      
      # Reload plugins dropdown
      
      observeEvent(r$plugins, {
        if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer r$plugins"))
        r[[paste0(category, "_reload_plugins_dropdown")]] <- now()
      })
      
      observeEvent(r[[paste0(category, "_reload_plugins_dropdown")]], {
        if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer r$..reload_plugins_dropdown"))
        
        tab_type_id <- switch(category, "patient_lvl" = 1, "aggregated" = 2)
        
        plugins <- r$plugins %>% dplyr::filter(tab_type_id == !!tab_type_id)
        
        options <- convert_tibble_to_list(data = plugins %>% dplyr::arrange(name), key_col = "id", text_col = "name", i18n = i18n)
        shiny.fluent::updateComboBox.shinyInput(session, "widget_creation_plugin", options = options, value = NULL)
      })
      
      # |-------------------------------- -----
      
      # --- --- -- -
      # Load UI ----
      # --- --- -- -
      
      # --- --- --- --- --
      ## Prepare vars ----
      # --- --- --- --- --
      
      observeEvent(r[[paste0(category, "_reload_tabs")]], {
        
        if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer r$..reload_tabs"))
        
        # Tabs without parent are set to level 1
        tabs <- r[[paste0(category, "_tabs")]] %>% dplyr::mutate(level = dplyr::case_when(is.na(parent_tab_id) ~ 1L, TRUE ~ NA_integer_))
        
        # Prevent infinite loop, max loops = 7
        i <- 1
        
        # Creating levels for all tabs
        while(nrow(tabs %>% dplyr::filter(is.na(level))) > 0 & i <= 7){
          tabs <-
            tabs %>%
            dplyr::left_join(tabs %>% dplyr::filter(!is.na(level)) %>% dplyr::transmute(parent_tab_id = id, parent_level = level), by = "parent_tab_id") %>%
            dplyr::mutate(level = dplyr::case_when(!is.na(parent_level) ~ parent_level + 1L, TRUE ~ level)) %>%
            dplyr::select(-parent_level)
          i <- i + 1
        }
        
        # Exclude tabs without level
        tabs <- tabs %>% dplyr::filter(!is.na(level))
        
        # Order by display order
        tabs <- tabs %>% dplyr::arrange(level, display_order)
        
        # Calculate first tab displayed in the menu
        if(nrow(tabs) > 0 & "level" %in% names(tabs) & !is.na(m$selected_study)){
          
          # First tab displayed
          first_tab_displayed <- tabs %>% dplyr::filter(level == 1) %>% dplyr::slice(1)
          if (max(tabs$level) >= 2){
            for(current_level in 2:max(tabs$level)){
              children <- tabs %>% dplyr::filter(level == current_level, parent_tab_id == first_tab_displayed$id) %>% dplyr::slice(1)
              if (nrow(children) > 0) first_tab_displayed <- children
            }
          }
          
          r[[paste0(category, "_first_tab_displayed")]] <- first_tab_displayed
        }
        
        r[[paste0(category, "_menu_tabs")]] <- 
          tabs %>%
          dplyr::group_by(level, parent_tab_id) %>%
          dplyr::summarize(
            id = id, tab_group_id = tab_group_id, tab_sub_group = dplyr::cur_group_id(), parent_tab_id = parent_tab_id,
            name = name, display_order = display_order, level = level
          ) %>%
          dplyr::ungroup()
        
        # Reload menu
        r[[paste0(category, "_reload_menu")]] <- now()
        
        # Load cards
        if (grepl("ui_first_load", r[[paste0(category, "_reload_tabs")]])) r[[paste0(category, "_load_ui_widgets")]] <- now()
      })
      
      # --- --- --- ---
      ## Load menu ----
      # --- --- --- ---
      
      observeEvent(r[[paste0(category, "_reload_menu")]], {
        if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - r$..reload_menu"))
        
        tab_group_id <- r[[paste0(category, "_menu_tabs")]] %>% dplyr::slice(1) %>% dplyr::pull(tab_group_id)
        
        # Create an ID per level / sub_group
        all_tabs <- r[[paste0(category, "_menu_tabs")]]
        
        if (nrow(all_tabs) > 0){
          
          shinyjs::hide(paste0(category, "_no_tabs_to_display"))
          
          study_first_tab_id <- all_tabs %>% dplyr::filter(level == 1) %>% dplyr::slice(1) %>% dplyr::pull(id)
          
          # Load breadcrumb & pivots, one per level / subgroup
          
          breadcrumbs <- tagList()
          pivots <- tagList()
          
          i <- 1L
          
          for (tab_sub_group in unique(all_tabs$tab_sub_group)){
            
            tabs <- all_tabs %>% dplyr::filter(tab_sub_group == !!tab_sub_group)
            tabs_ui <- tagList()
            
            for (j in 1:nrow(tabs)){
              tab <- tabs[j, ]
              tabs_ui <- tagList(tabs_ui, shiny.fluent::PivotItem(id = tab$id, itemKey = tab$id, headerText = tab$name))
              
              if (i == 1 & j == 1) if (is.na(r[[paste0(category, "_selected_tab")]])) r[[paste0(category, "_selected_tab")]] <- tab$id
            }
            
            pivot_id <- paste0(category, "_study_pivot_", tab_group_id, "_", tab_sub_group)
            pivot_sub_div_id <- paste0(category, "_study_pivot_sub_div_", tab_group_id, "_", tab_sub_group)
            
            pivot <- 
              div(
                id = ns(pivot_id),
                shiny.fluent::Pivot(
                  id = ns(pivot_sub_div_id),
                  onLinkClick = htmlwidgets::JS(paste0("item => {",
                                                       "Shiny.setInputValue('", id, "-study_current_tab', item.props.id);",
                                                       "Shiny.setInputValue('", id, "-study_current_tab_trigger', Math.random());",
                                                       "}"
                  )),
                  tabs_ui,
                  defaultSelectedKey = r[[paste0(category, "_selected_tab")]]
                )
              )
            
            if (is.na(r[[paste0(category, "_selected_tab")]]) & i > 1) pivot <- shinyjs::hidden(pivot)
            if (!is.na(r[[paste0(category, "_selected_tab")]]) & r[[paste0(category, "_selected_tab")]] %not_in% tabs$id) pivot <- shinyjs::hidden(pivot)
            pivot_tabs <- paste0(category, "_pivot_tabs_", tab_group_id, "_", tab_sub_group)
            
            pivots <- tagList(pivots, pivot)
            
            tab_sub_group_first_tab <- all_tabs %>% dplyr::filter(tab_sub_group == !!tab_sub_group) %>% dplyr::arrange(display_order) %>% dplyr::slice(1)
            nb_levels <- tab_sub_group_first_tab %>% dplyr::slice(1) %>% dplyr::pull(level)
            
            tabs_tree <- tibble::tibble()
            
            if (nb_levels >= 2){
              
              is_current_item <- FALSE
              
              for (level in nb_levels:2){
                
                if (level == nb_levels) parent_tab <- all_tabs %>% dplyr::filter(level == !!level - 1, id == tab_sub_group_first_tab$parent_tab_id)
                else parent_tab <- all_tabs %>% dplyr::filter(level == !!level - 1, id == parent_tab$parent_tab_id)
                
                if (level == nb_levels) tabs_tree <- parent_tab
                else tabs_tree <- tabs_tree %>% dplyr::bind_rows(parent_tab)
              }
            }
            
            if (nrow(tabs_tree) > 0) tabs_tree <- tabs_tree %>% dplyr::arrange(level)
            
            if (nb_levels == 1) is_current_item <- TRUE else is_current_item <- FALSE
            
            first_list_element <- list(key = "main", text = shiny.fluent::FontIcon(iconName = "Home"), href = paste0("#!/", page_name), isCurrentItem = FALSE,
                                       onClick = htmlwidgets::JS(paste0("item => {",
                                                                        "Shiny.setInputValue('", id, "-study_go_to_tab', ", study_first_tab_id, ");",
                                                                        "Shiny.setInputValue('", id, "-study_go_to_tab_trigger', Math.random());",
                                                                        "}"
                                       )))
            
            breadcrumb_list <- list(first_list_element)
            
            if (nb_levels >= 2){
              
              for (j in 1:nrow(tabs_tree)){
                
                row <- tabs_tree[j, ]
                
                if (row$level == nb_levels - 1) breadcrumb_list <- rlist::list.append(breadcrumb_list, list(key = "main", text = row$name, isCurrentItem = TRUE))
                else {
                  breadcrumb_list <- rlist::list.append(breadcrumb_list, list(
                    key = "main", text = row$name,
                    onClick = htmlwidgets::JS(paste0(
                      "item => {",
                      "Shiny.setInputValue('", id, "-study_go_to_tab', ", row$id, ");",
                      "Shiny.setInputValue('", id, "-study_go_to_tab_trigger', Math.random());",
                      "}"
                    ))
                  ))
                }
              }
            }
            
            breadcrumb <- div(
              id = ns(paste0(category, "_study_breadcrumb_", tab_group_id, "_", tab_sub_group)),
              shiny.fluent::Breadcrumb(items = breadcrumb_list, maxDisplayedItems = 5),
              style = "margin-left: 10px;"
            )
            
            if (is.na(r[[paste0(category, "_selected_tab")]]) & i > 1) breadcrumb <- shinyjs::hidden(breadcrumb)
            if (!is.na(r[[paste0(category, "_selected_tab")]]) & r[[paste0(category, "_selected_tab")]] %not_in% tabs$id)  breadcrumb <- shinyjs::hidden(breadcrumb)
            
            breadcrumbs <- tagList(breadcrumbs, breadcrumb)
            
            i <- 2L
          }
        }
        
        if (nrow(all_tabs) == 0){
          
          breadcrumbs <- div(
            id = ns(paste0(category, "_study_breadcrumb_", tab_group_id, "_0")),
            shiny.fluent::Breadcrumb(items = list(list(key = "main", text = shiny.fluent::FontIcon(iconName = "Home"), href = paste0("#!/", page_name), isCurrentItem = FALSE,
                                                       onClick = htmlwidgets::JS(paste0("item => {",
                                                                                        "Shiny.setInputValue('", id, "-study_go_to_tab', 0);",
                                                                                        "Shiny.setInputValue('", id, "-study_go_to_tab_trigger', Math.random());",
                                                                                        "}"
                                                       )))), maxDisplayedItems = 3)
          )
          
          pivots <- ""
          
          shinyjs::show(paste0(category, "_no_tabs_to_display"))
        }
        
        output$study_menu <- renderUI(div(
          div(breadcrumbs), div(pivots),
          style = "display:flex; justify-content:space-between; margin:5px 13px 0px 0px;"
        ))
      })
      
      # --- --- --- --- --
      ## Load widgets ----
      # --- --- --- --- --
      
      observeEvent(r[[paste0(category, "_load_ui_widgets")]], {
        if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer r$..load_ui_widgets"))
        
        # Don't reload study UI if already loaded
        req(m$selected_study %not_in% r[[paste0(category, "_loaded_studies")]])
        
        distinct_tabs <- r[[paste0(category, "_menu_tabs")]]$id
        
        code_ui <- tagList()
        
        all_groups <- NA_integer_
        
        # Loop over distinct tabs, for this study
        
        selected_tab <- r[[paste0(category, "_selected_tab")]]
        
        ### Loop over tabs ----
        sapply(distinct_tabs, function(tab_id){
          
          widgets_ui <- tagList()
          
          widgets <- r[[paste0(category, "_widgets")]] %>% dplyr::filter(tab_id == !!tab_id) %>% dplyr::rename(widget_id = id) %>% dplyr::arrange(display_order)
          
          if (nrow(widgets) > 0){
            
            # Load widgets concepts
            widgets_concepts <- r[[paste0(category, "_widgets_concepts")]] %>% dplyr::inner_join(widgets %>% dplyr::select(widget_id), by = "widget_id")
            
            # Get widget ID
            distinct_widgets <- unique(widgets$widget_id)
            
            ### Loop over widgets ----
            
            # Use sapply instead of for loop, cause with for loop, widget_id doesn't change
            sapply(distinct_widgets, function(widget_id){
              
              # Load over selected concepts
              selected_concepts <- 
                widgets_concepts %>% 
                dplyr::filter(widget_id == !!widget_id) %>%
                dplyr::select(concept_id, concept_name, concept_display_name, domain_id, mapped_to_concept_id, merge_mapped_concepts)
              
              # Load UI code for this widget
              plugin_id <- widgets %>% dplyr::filter(widget_id == !!widget_id) %>% dplyr::slice(1) %>% dplyr::pull(plugin_id)
              
              # Check if plugin has been deleted
              check_deleted_plugin <- nrow(DBI::dbGetQuery(r$db, paste0("SELECT * FROM plugins WHERE id = ", plugin_id))) == 0
              if (check_deleted_plugin){
                code_ui_card <- paste0("div(shiny.fluent::MessageBar('", i18n$t("plugin_deleted"), "', messageBarType = 5), style = 'margin-top:10px;')")
                settings_widget_button <- ""
              }
              else {
                
                # Get plugin unique_id
                sql <- glue::glue_sql("SELECT value FROM options WHERE category = 'plugin' AND name = 'unique_id' AND link_id = {plugin_id}", .con = r$db)
                plugin_unique_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
                
                # Get plugin folder
                plugin_folder <- paste0(r$app_folder, "/plugins/", category, "/", plugin_unique_id)
                
                code_ui_card <- r$code %>% dplyr::filter(link_id == plugin_id, category == "plugin_ui") %>% dplyr::pull(code)
                # settings_widget_button <- actionButton(ns(paste0(category, "_widget_settings_", widget_id)), "", icon = icon("cog"))
                
                # Create translations files
                tryCatch({
                  # Create plugin folder in translations folder if doesn't exist
                  new_dir <- paste0(r$app_folder, "/translations/", plugin_unique_id)
                  if (!dir.exists(new_dir)) dir.create(new_dir)
                  
                  # Get translations file
                  translations <- readLines(paste0(plugin_folder, "/translations.csv"), warn = FALSE)
                  
                  # Create a csv with all languages
                  data <- read.csv(text = translations, header = TRUE, stringsAsFactors = FALSE)
                  
                  # Create one csv by language
                  for(lang in names(data)[-1]){
                    # Create a new dataframe with base & current language cols
                    data_lang <- data[, c("base", lang)]
                    filename <- paste0(new_dir, "/translation_", lang, ".csv")
                    write.csv(data_lang, filename, row.names = FALSE)
                  }
                }, error = function(e) cat(paste0("\n", now(), " - mod_data - ", id, " - error creating translations file - plugin_id = ", plugin_id)))
                
                tryCatch({
                  i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = new_dir))
                  i18np$set_translation_language(language)},
                  error = function(e) cat(paste0("\n", now(), " - mod_data - ", id, " - error creating translator - plugin_id = ", plugin_id)))
              }
              
              # Get name of widget
              widget_name <- widgets %>% dplyr::filter(widget_id == !!widget_id) %>% dplyr::slice(1) %>% dplyr::pull(name)
              
              # Get UI code from db. Try to run plugin UI code
              
              sql <- glue::glue_sql("SELECT id FROM options WHERE link_id = {plugin_id} AND name = 'filename' AND value = 'ui.R'", .con = r$db)
              code_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
              
              sql <- glue::glue_sql("SELECT code FROM code WHERE category = 'plugin' AND link_id = {code_id}", .con = r$db)
              ui_code <- 
                DBI::dbGetQuery(r$db, sql) %>% 
                dplyr::pull() %>%
                stringr::str_replace_all("%study_id%", as.character(m$selected_study)) %>%
                stringr::str_replace_all("%tab_id%", as.character(tab_id)) %>%
                stringr::str_replace_all("%widget_id%", as.character(widget_id)) %>%
                stringr::str_replace_all("%req%", "req(m[[session_code]] == session_num)\nreq(m$selected_study == %study_id%)") %>%
                stringr::str_replace_all("\r", "\n") %>%
                stringr::str_replace_all("''", "'")
              
              # Widget card
              
              ui_code <- tryCatch(eval(parse(text = ui_code)), error = function(e) cat(paste0("\n", now(), " - mod_data - ", id, " - error loading UI code - widget_id = ", widget_id)))
              
              ui_output <- div(
                ui_code,
                shinyjs::hidden(
                  div(
                    id = ns(paste0(category, "_widget_settings_remove_buttons_", widget_id)),
                    div(
                      div(
                        shiny.fluent::IconButton.shinyInput(ns(paste0(category, "_widget_settings_", widget_id)), iconProps = list(iconName = "Settings")),
                        class = "small_icon_button"
                      ),
                      div(
                        shiny.fluent::IconButton.shinyInput(ns(paste0(category, "_remove_widget_", widget_id)), iconProps = list(iconName = "Delete")),
                        class = "small_icon_button"
                      ),
                      style = "display: flex; gap: 2px;"
                    ),
                    class = "widget_buttons"
                  )
                ),
                class = "widget"
              )
              
              # Insert into a gridster widget
              ui_output <- tags$li(
                id = ns(paste0(category, "_widget_", widget_id)),
                `data-row` = 1, `data-col` = 1, `data-sizex` = 6, `data-sizey` = 2,
                ui_output,
                class = "gridster_widget"
              )
              
              widgets_ui <<- tagList(widgets_ui, ui_output)
            })
          }
          
          # Add gridster div
          gridster_id <- paste0(category, "_gridster_", tab_id)
          gridster_div <- div(id = ns(gridster_id), class = "gridster", tags$ul(widgets_ui))
          
          hide_div <- TRUE
          if (!is.na(selected_tab)) if (tab_id == selected_tab) hide_div <- FALSE
          if (hide_div) gridster_div <- shinyjs::hidden(gridster_div)
          
          insertUI(selector = paste0("#", ns("study_cards")), where = "beforeEnd", ui = gridster_div)
          r[[paste0(category, "_cards")]] <- c(r[[paste0(category, "_cards")]], gridster_id)
          
          shinyjs::delay(100, shinyjs::runjs(paste0("
            $(document).ready(function() {
              window.", gridster_id, " = $('#", ns(gridster_id), " ul').gridster({
                namespace: '#", ns(gridster_id), "',
                widget_margins: [15, 15],
                widget_base_dimensions: [100, 100],
                resize: {
                  enabled: true
                }
              }).data('gridster');
              ", gridster_id, ".disable().disable_resize();
            });"
          )))
        })
        
        # Indicate that this study has been loaded, so that UI elements aren't loaded twice
        r[[paste0(category, "_loaded_studies")]] <- c(r[[paste0(category, "_loaded_studies")]], m$selected_study)
      })
      
      # --- --- --- -- -
      # Load server ----
      # --- --- --- -- -
      
      observeEvent(r[[paste0(category, "_load_server")]], {
        
        if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer r$..load_server"))
        
        req(!is.na(m$selected_study))
        
        # Get tabs elements, arrange them by display_order
        
        tab_group <- r$studies %>% dplyr::filter(id == m$selected_study) %>% dplyr::pull(paste0(category, "_tab_group_id"))
        tabs <- r[[paste0(category, "_tabs")]] %>% dplyr::filter(tab_group_id == tab_group) %>% dplyr::select(tab_id = id)
        widgets <- r[[paste0(category, "_widgets")]] %>% dplyr::inner_join(tabs, by = "tab_id") %>% dplyr::rename(widget_id = id)
        widgets_concepts <- r[[paste0(category, "_widgets_concepts")]] %>% dplyr::inner_join(widgets %>% dplyr::select(widget_id), by = "widget_id")
        
        # --- --- --- --- --- --- --- ---
        ## Run server code for cards ----
        # --- --- --- --- --- --- --- ---
        
        if (nrow(widgets) > 0){
          
          # Get widget widget_id
          distinct_widgets <- unique(widgets$widget_id)
          
          # Loop over distinct cards
          sapply(distinct_widgets, function(widget_id){
            
            # Run plugin server code
            # Only if this code has not been already loaded
            trace_code <- paste0(category, "_", widget_id, "_", m$selected_study)
            # if (trace_code %in% r$server_tabs_groups_loaded) print(trace_code)
            if (trace_code %not_in% r$server_tabs_groups_loaded){
              
              # Add the trace_code to loaded plugins list
              r$server_tabs_groups_loaded <- c(r$server_tabs_groups_loaded, trace_code)
              
              # selected_concepts <- widgets_concepts %>% dplyr::filter(widget_id == !!widget_id) %>%
              #   dplyr::select(concept_id, concept_name, concept_display_name, domain_id,
              #     mapped_to_concept_id, merge_mapped_concepts)
              selected_concepts <- tibble::tibble(
                concept_id = integer(), concept_name = character(), concept_display_name = character(), domain_id = character(),
                mapped_to_concept_id = integer(), merge_mapped_concepts = logical())
              
              # Get plugin code
              
              ids <- widgets %>% dplyr::filter(widget_id == !!widget_id) %>% dplyr::slice(1) %>% dplyr::select(plugin_id, tab_id)
              
              # Check if plugin has been deleted
              check_deleted_plugin <- nrow(DBI::dbGetQuery(r$db, paste0("SELECT * FROM plugins WHERE id = ", ids$plugin_id))) == 0
              if (!check_deleted_plugin){
                
                sql <- glue::glue_sql("SELECT id FROM options WHERE link_id = {ids$plugin_id} AND name = 'filename' AND value = 'server.R'", .con = r$db)
                code_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
                
                sql <- glue::glue_sql("SELECT code FROM code WHERE category = 'plugin' AND link_id = {code_id}", .con = r$db)
                server_code <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
                
                server_code <- 
                  server_code %>%
                  stringr::str_replace_all("%tab_id%", as.character(ids$tab_id)) %>%
                  stringr::str_replace_all("%widget_id%", as.character(widget_id)) %>%
                  stringr::str_replace_all("%req%", "req(m[[session_code]] == session_num)\nreq(m$selected_study == %study_id%)") %>%
                  stringr::str_replace_all("%study_id%", as.character(m$selected_study)) %>%
                  stringr::str_replace_all("\r", "\n") %>%
                  stringr::str_replace_all("''", "'")
                
              }
              else server_code <- ""
              
              # Create translations file & var
              # if (!check_deleted_plugin){
              #   
              #   plugin_translations <- r$code %>%
              #     dplyr::filter(link_id == ids$plugin_id, category == "plugin_translations") %>%
              #     dplyr::pull(code)
              #   
              #   if (plugin_translations != ""){
              #     
              #     tryCatch({
              #       # Get plugin unique_id
              #       plugin_unique_id <- r$options %>% dplyr::filter(category == "plugin", name == "unique_id", link_id == !!ids$plugin_id) %>% dplyr::pull(value)
              #       
              #       # Create plugin folder in translations folder if doesn't exist
              #       new_dir <- paste0(r$app_folder, "/translations/", plugin_unique_id)
              #       if (!dir.exists(new_dir)) dir.create(new_dir)
              #       
              #       # Create a csv with all languages
              #       data <- read.csv(text = plugin_translations, header = TRUE, stringsAsFactors = FALSE)
              #       
              #       # Create one csv by language
              #       for(lang in names(data)[-1]){
              #         file_name <- paste0(new_dir, "/translation_", lang, ".csv")
              #         
              #         if (!file.exists(file_name)){
              #           # Create a new dataframe with base & current language cols
              #           data_lang <- data[, c("base", lang)]
              #           
              #           # Create csv
              #           write.csv(data_lang, file_name, row.names = FALSE)
              #         }
              #       }
              #     },
              #       error = function(e) report_bug(r = r, output = output, error_message = "error_creating_translations_file",
              #         error_name = paste0(id, " - create translations files - plugin_id ", ids$plugin_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
              #     
              #     tryCatch({
              #       i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = new_dir))
              #       i18np$set_translation_language(language)},
              #       error = function(e) report_bug(r = r, output = output, error_message = "error_creating_new_translator",
              #         error_name = paste0(id, " - create i18np translator - plugin_id ", ids$plugin_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
              #   }
              # }
              
              # Create a session number, to inactivate older observers
              # Reset all older observers for this widget_id
              
              session_code <- paste0(category, "_widget_", widget_id)
              if (length(m[[session_code]]) == 0) session_num <- 1L
              if (length(m[[session_code]]) > 0) session_num <- m[[session_code]] + 1
              m[[session_code]] <- session_num
              
              # NB : req(m[[session_code]] == session_num) must be put at the beginning of each observeEvent in plugins code
              
              # Variables to hide
              new_env_vars <- list("r" = NA)
              
              # Variables to keep
              variables_to_keep <- c("d", "m", "session_code", "session_num", "i18n", "selected_concepts", "debug")
              if (exists("i18np")) variables_to_keep <- c(variables_to_keep, "i18np")
              
              for (var in variables_to_keep) new_env_vars[[var]] <- eval(parse(text = var))
              
              new_env <- rlang::new_environment(data = new_env_vars, parent = pryr::where("r"))
              tryCatch(eval(parse(text = server_code), envir = new_env),
                error = function(e) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code",
                error_name = paste0(id, " - run_study_server_code - run_plugin_code - plugin_id ", ids$plugin_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
              
              # Observer for widget deletion
              observeEvent(input[[paste0(category, "_remove_widget_", widget_id)]], {
                if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$..remove_widget.."))
                r[[paste0(category, "_selected_widget")]] <- widget_id
                shinyjs::show("delete_widget_modal")
              })
              
              # Observer for widget settings
              observeEvent(input[[paste0(category, "_widget_settings_", widget_id)]], {
                if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$..widget_settings.."))
                r[[paste0(category, "_widget_settings_trigger")]] <- now()
                r[[paste0(category, "_widget_settings")]] <- widget_id
              })
            }
          })
        }
      })
    
    # |-------------------------------- -----
    
    # --- --- --- --- --- --
    # A tab is selected ####
    # --- --- --- --- --- ---
    
    observeEvent(r[[paste0(category, "_selected_tab")]], {
      
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer r$..selected_tab"))
      
      # req(!grepl("show_tab", r[[paste0(category, "_selected_tab")]]))
      
      # Hide all cards
      sapply(r[[paste0(category, "_cards")]], shinyjs::hide)
      
      # Show gridster card of this tab
      shinyjs::show(paste0(category, "_gridster_", r[[paste0(category, "_selected_tab")]]))
    })
    
    # Tab selected from the menu
    observeEvent(input$study_current_tab_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$study_current_tab_trigger"))
      
      if (!grepl("add_tab", input$study_current_tab)) r[[paste0(category, "_selected_tab")]] <- input$study_current_tab
      
      current_tab <- r[[paste0(category, "_menu_tabs")]] %>% dplyr::filter(id == input$study_current_tab)
      children_tabs <- r[[paste0(category, "_menu_tabs")]] %>% dplyr::filter(parent_tab_id == input$study_current_tab)
      
      # If current tab has children, load children
      if (nrow(children_tabs) > 0){
        first_child <- children_tabs %>% dplyr::arrange(display_order) %>% dplyr::slice(1)
        r[[paste0(category, "_selected_tab")]] <- first_child$id
        
        for (name in c("pivot", "breadcrumb")){
          shinyjs::hide(paste0(category, "_study_", name, "_", current_tab$tab_group_id, "_", current_tab$tab_sub_group)) 
          shinyjs::show(paste0(category, "_study_", name, "_", first_child$tab_group_id, "_", first_child$tab_sub_group))
        }
      }
    })
    
    # Tab selected from breadcrumb
    observeEvent(input$study_go_to_tab_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$study_go_to_tab"))
      
      r[[paste0(category, "_selected_tab")]] <- input$study_go_to_tab
      r[[paste0(category, "_reload_menu")]] <- now()
    })
    
    # --- --- --- --
    # Add a tab ----
    # --- --- --- --
    
    # Open modal
    observeEvent(input$add_tab, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$add_tab"))
      shinyjs::show("add_tab_modal")
    })
    
    # Close modal
    observeEvent(input$close_add_tab_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$close_add_tab_modal"))
      shinyjs::hide("add_tab_modal")
    })
    
    # Add a tab
    observeEvent(input$add_tab_button, {
      
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$add_tab_button"))
      
      req(length(input$tab_name) > 0)
      tab_name <- input$tab_name
      
      selected_tab <- r[[paste0(category, "_selected_tab")]]
      tabs <- r[[paste0(category, "_tabs")]] %>% dplyr::filter(id == selected_tab)
      
      # Get tab_group_id
      sql <- glue::glue_sql("SELECT {`paste0(category, '_tab_group_id')`} FROM studies WHERE id = {m$selected_study}", .con = r$db)
      tab_group_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
      
      # If it is the first tab to be created
      if (nrow(tabs) == 0){
        parent_tab_id <- NA_integer_
        display_order <- 1
      }
      
      # If there are already existing tabs
      else {
        
        # If tab is at the same level of current tab, get common parent_tab_id
        # Calculate display order
        
        if (input$add_tab_type == "same_level") parent_tab_id <- tabs %>% dplyr::pull(parent_tab_id)
        else if (input$add_tab_type == "level_under") parent_tab_id <- tabs %>% dplyr::pull(id)
        
        # Calculate display order
        if (!is.na(parent_tab_id)) sql <- glue::glue_sql("SELECT COALESCE(MAX(display_order), 0) FROM tabs WHERE tab_group_id = {tab_group_id} AND parent_tab_id = {parent_tab_id}", .con = r$db)
        else sql <- glue::glue_sql("SELECT COALESCE(MAX(display_order), 0) FROM tabs WHERE tab_group_id = {tab_group_id} AND parent_tab_id IS NULL", .con = r$db)
        
        display_order <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull() + 1
        
        # Can't add a tab at the level under if there are tabs elements attached to current tab
        if (input$add_tab_type == "level_under"){
          selected_tab <- r[[paste0(category, "_selected_tab")]]
          widgets <- r[[paste0(category, "_widgets")]] %>% dplyr::filter(tab_id == selected_tab, !deleted) %>% dplyr::rename(widget_id = id)
          if (nrow(widgets) > 0) show_message_bar(output, message = "add_tab_has_widgets", i18n = i18n, ns = ns)
          req(nrow(widgets) == 0)
        }
      }
      
      # Check if name is not empty
      if (is.na(tab_name)) shiny.fluent::updateTextField.shinyInput(session, "tab_name", errorMessage = i18n$t("provide_valid_name"))
      req(!is.na(tab_name))
      
      # Check if name is not already used
      if (is.na(parent_tab_id)) sql <- glue::glue_sql("SELECT name FROM tabs WHERE deleted IS FALSE AND category = {category} AND tab_group_id = {tab_group_id} AND parent_tab_id IS NULL AND name = {tab_name}", .con = r$db)
      else sql <- glue::glue_sql("SELECT name FROM tabs WHERE deleted IS FALSE AND category = {category} AND tab_group_id = {tab_group_id} AND parent_tab_id = {parent_tab_id} AND name = {tab_name}", .con = r$db)
      name_already_used <- nrow(DBI::dbGetQuery(r$db, sql) > 0)
      
      if (name_already_used) shiny.fluent::updateTextField.shinyInput(session, "tab_name", errorMessage = i18n$t("name_already_used"))
      req(!name_already_used)
      
      new_id <- get_last_row(r$db, "tabs") + 1
      
      # Add data in database
      new_data <- tibble::tibble(id = new_id, category = category, name = tab_name, description = NA_character_, 
        tab_group_id = tab_group_id, parent_tab_id = parent_tab_id, display_order = display_order, creator_id = r$user_id, datetime = now(), deleted = FALSE)
      DBI::dbAppendTable(r$db, "tabs", new_data)
      
      # Update r var
      r[[paste0(category, "_tabs")]] <- r[[paste0(category, "_tabs")]] %>% dplyr::bind_rows(new_data)
      
      # Notify user
      show_message_bar(output, paste0(category, "_tab_added"), "success", i18n = i18n, ns = ns)
      
      # Reset fields
      
      shiny.fluent::updateTextField.shinyInput(session, "tab_name", value = "")
      shiny.fluent::updateChoiceGroup.shinyInput(session, "add_tab_type", value = "same_level")
      
      # Reload UI, with new tab opened
      
      r[[paste0(category, "_selected_tab")]] <- new_id
      r[[paste0(category, "_reload_tabs")]] <- now()
      
      # Reload UI menu and set to added tab
      r[[paste0(category, "_reload_menu")]] <- now()
      
      # Hide currently opened cards
      sapply(r[[paste0(category, "_opened_cards")]], shinyjs::hide)
      
      # Hide add tab model
      shinyjs::hide("add_tab_modal")
    })
    
    # --- --- --- - -
    # Edit a tab ----
    # --- --- --- - -
    
    # observeEvent(input$edit_tab, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$edit_tab"))
    #   
    #   sapply(r[[paste0(category, "_opened_cards")]], shinyjs::hide)
    #   shinyjs::hide(paste0(category, "_add_widget"))
    #   shinyjs::hide(paste0(category, "_widget_settings"))
    #   shinyjs::show(paste0(category, "_edit_tab"))
    #   
    #   # Update textfield with current tab name
    #   selected_tab <- r[[paste0(category, "_selected_tab")]]
    #   shiny.fluent::updateTextField.shinyInput(session, "edit_tab_name", 
    #                                            value = r[[paste0(category, "_tabs")]] %>% dplyr::filter(id == selected_tab) %>% dplyr::pull(name))
    # })
    # 
    # # Save updates
    # observeEvent(input$edit_tab_save, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$edit_tab_save"))
    #   
    #   # Check if the name is not empty
    #   if (input$edit_tab_name == "") shiny.fluent::updateTextField.shinyInput(session, "edit_tab_name", errorMessage = i18n$t("provide_valid_name"))
    #   
    #   req(input$edit_tab_name != "")
    #   
    #   # Check if the name is already used
    #   selected_tab <- r[[paste0(category, "_selected_tab")]]
    #   tab <- r[[paste0(category, "_tabs")]] %>% dplyr::filter(id == selected_tab)
    #   
    #   if (!is.na(tab$parent_tab_id)) same_level_tabs <- r[[paste0(category, "_tabs")]] %>% dplyr::filter(id != tab$id, parent_tab_id == tab$parent_tab_id)
    #   if (is.na(tab$parent_tab_id)) same_level_tabs <- r[[paste0(category, "_tabs")]] %>% dplyr::filter(id != tab$id, is.na(parent_tab_id))
    #   
    #   if (input$edit_tab_name %in% same_level_tabs$name) show_message_bar(output, "name_already_used", "severeWarning", i18n = i18n, ns = ns)
    #   
    #   req(input$edit_tab_name %not_in% same_level_tabs$name)
    #   
    #   # Update database
    #   
    #   sql <- glue::glue_sql("UPDATE tabs SET name = {input$edit_tab_name} WHERE id = {r[[paste0(category, '_selected_tab')]]}", .con = r$db)
    #   query <- DBI::dbSendStatement(r$db, sql)
    #   DBI::dbClearResult(query)
    #   
    #   # Update r vars
    #   r[[paste0(category, "_tabs")]] <- r[[paste0(category, "_tabs")]] %>% dplyr::mutate(name = dplyr::case_when(
    #     id == selected_tab ~ input$edit_tab_name, TRUE ~ name))
    #   
    #   r[[paste0(category, "_menu_tabs")]] <- r[[paste0(category, "_menu_tabs")]] %>% dplyr::mutate(name = dplyr::case_when(
    #     id == selected_tab ~ input$edit_tab_name, TRUE ~ name))
    #   
    #   # Notify user
    #   show_message_bar(output, message = "modif_saved", type = "success", i18n = i18n, ns = ns)
    #   
    #   # Show opened cards before opening Add widget div
    #   sapply(r[[paste0(category, "_opened_cards")]], shinyjs::show)
    #   
    #   # Hide Edit widget div
    #   shinyjs::hide(paste0(category, "_edit_tab"))
    #   
    #   # Reload output
    #   r[[paste0(category, "_reload_menu")]] <- now()
    # })
    # 
    # # Close edition div
    # observeEvent(input[[paste0(category, "_close_edit_tab")]], {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$..close_edit_tab"))
    #   
    #   # Show opened cards before opening Add widget div
    #   sapply(r[[paste0(category, "_opened_cards")]], shinyjs::show)
    #   
    #   # Hide Edit widget div
    #   shinyjs::hide(paste0(category, "_edit_tab"))
    # })
    
    # --- --- --- -- --
    # Delete a tab ----
    # --- --- --- -- --
    
    # # Code to make Remove tab button work
    # observeEvent(input$remove_tab, {
    #   if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$remove_tab"))
    #   r[[tab_delete_variable]] <- TRUE
    # })
    # 
    # tab_delete_prefix <- paste0(category, "_tab")
    # tab_dialog_title <- paste0(category, "_tabs_delete")
    # tab_dialog_subtext <- paste0(category, "_tabs_delete_subtext")
    # tab_react_variable <- "tab_delete_confirm"
    # tab_table <- paste0(category, "_tabs")
    # tab_id_var_sql <- "id"
    # tab_id_var_r <- paste0(category, "_selected_tab")
    # tab_delete_message <- paste0(category, "_tab_deleted")
    # tab_reload_variable <- paste0(category, "_load_ui")
    # tab_information_variable <- paste0(category, "_tab_deleted")
    # tab_delete_variable <- paste0(tab_delete_prefix, "_open_dialog")
    # 
    # delete_element(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
    #                delete_prefix = tab_delete_prefix, dialog_title = tab_dialog_title, dialog_subtext = tab_dialog_subtext,
    #                react_variable = tab_react_variable, table = tab_table, id_var_sql = tab_id_var_sql, id_var_r = tab_id_var_r,
    #                delete_message = tab_delete_message, translation = TRUE, reload_variable = tab_reload_variable,
    #                information_variable = tab_information_variable)
    # 
    # # When a tab is deleted, change current tab variable
    # # Reload toggles if necessary
    # # Delete sub-tabs either
    # 
    # observeEvent(r[[tab_information_variable]], {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer r$..tab_deleted"))
    #   
    #   table <- paste0(category, "_tabs")
    #   deleted_tab_id <- r[[paste0(category, "_tab_deleted")]]
    #   sql <- glue::glue_sql("SELECT * FROM tabs WHERE id = {deleted_tab_id}", .con = r$db)
    #   tab_deleted <- DBI::dbGetQuery(r$db, sql)
    #   
    #   # If we are at level one, take first tab of level one
    #   if (is.na(tab_deleted$parent_tab_id)){
    #     show_tab_id <- r[[table]] %>%
    #       dplyr::filter(tab_group_id == tab_deleted$tab_group_id & is.na(parent_tab_id) & id != tab_deleted$id) %>%
    #       dplyr::arrange(display_order) %>%
    #       dplyr::slice(1) %>%
    #       dplyr::pull(id)
    #   }
    #   
    #   # Else, take first tab of the same level
    #   if (!is.na(tab_deleted$parent_tab_id)){
    #     show_tab <- r[[table]] %>%
    #       dplyr::filter(parent_tab_id == tab_deleted$parent_tab_id & id != tab_deleted$id)
    #     
    #     # If not any tab in this level, take lower level
    #     if (nrow(show_tab) == 0) show_tab <- r[[table]] %>%
    #         dplyr::filter(id == tab_deleted$parent_tab_id)
    #     
    #     show_tab_id <- show_tab %>%
    #       dplyr::arrange(display_order) %>%
    #       dplyr::slice(1) %>%
    #       dplyr::pull(id)
    #   }
    #   
    #   r[[paste0(category, "_selected_tab")]] <- show_tab_id
    #   sapply(r[[paste0(category, "_opened_cards")]], shinyjs::hide)
    #   shinyjs::show(paste0(category, "_toggles_", show_tab_id))
    #   
    #   # Reload UI menu
    #   r[[paste0(category, "_reload_tabs")]] <- now()
    #   
    #   # Check if parent tab still have children and reload toggles div if not
    #   sql <- glue::glue_sql("SELECT parent_tab_id FROM tabs WHERE id = {deleted_tab_id}", .con = r$db)
    #   parent_tab_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(parent_tab_id)
    #   if (!is.na(parent_tab_id)){
    #     
    #     has_children <- r[[paste0(category, "_tabs")]] %>% dplyr::filter(parent_tab_id == !!parent_tab_id) %>% nrow()
    #     if(has_children == 0){
    #       
    #       shinyjs::hide(paste0(category, "_toggles_", parent_tab_id))
    #       
    #       # parent_toggles_div <- div(
    #       #   make_card("",
    #       #     shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
    #       #       shiny.fluent::ActionButton.shinyInput(ns(paste0(category, "_add_widget_", parent_tab_id)), i18n$t("new_widget"), iconProps = list(iconName = "Add"),
    #       #         onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", category, "_add_widget_trigger', Math.random())"))),
    #       #       shiny.fluent::ActionButton.shinyInput(ns(paste0(category, "_edit_tab_", parent_tab_id)), i18n$t("edit_tab"), iconProps = list(iconName = "Edit"),
    #       #         onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-edit_tab_trigger', Math.random())"))),
    #       #       div(style = "width:20px;")
    #       #     )
    #       #   )
    #       # )
    #       
    #       # insertUI(selector = paste0("#", ns("study_cards")), where = "beforeEnd", ui = uiOutput(ns(paste0(category, "_toggles_", parent_tab_id))))
    #       # output[[paste0(category, "_toggles_", parent_tab_id)]] <- renderUI(parent_toggles_div)
    #     }
    #   }
    #   
    #   # Hide edit_tab div
    #   shinyjs::hide(paste0(category, "_edit_tab"))
    # })
    
    # --- --- --- --- -
    # Add a widget ----
    # --- --- --- --- -
    
    # Open modal
    observeEvent(input$add_widget, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$add_widget"))
      shinyjs::show("add_widget_modal")
    })
    
    # Close modal
    observeEvent(input$close_add_widget_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$close_add_widget_modal"))
      shinyjs::hide("add_widget_modal")
    })
    
    ## Concepts datatable ----
    
    # Load vocabularies attached to this dataset
    observeEvent(r$dataset_vocabularies, {
      
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer r$dataset_vocabularies"))
      
      if (nrow(r$dataset_vocabularies) == 0) vocabulary_options = list()
      if (nrow(r$dataset_vocabularies) > 0) vocabulary_options <- convert_tibble_to_list(data = r$dataset_vocabularies, 
        key_col = "vocabulary_id", text_col = "vocabulary_id", i18n = i18n)
      
      for(name in c("widget_creation_vocabulary", "widget_settings_vocabulary")) shiny.fluent::updateComboBox.shinyInput(
        session, name, options = vocabulary_options, value = NULL)
    })
    
    # Reload vocabulary concepts
    
    observeEvent(input$widget_creation_vocabulary, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$widget_creation_vocabulary"))
      r[[paste0(category, "_reload_widget_vocabulary_concepts")]] <- now()
      r[[paste0(category, "_reload_widget_vocabulary_concepts_type")]] <- "widget_creation"
    })
    
    observeEvent(input$widget_settings_vocabulary, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$widget_settings_vocabulary"))
      r[[paste0(category, "_reload_widget_vocabulary_concepts")]] <- now()
      r[[paste0(category, "_reload_widget_vocabulary_concepts_type")]] <- "widget_settings"
    })
    
    observeEvent(r[[paste0(category, "_reload_widget_vocabulary_concepts")]], {
      
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " observer r$..reload_widget_vocabulary_concepts"))
      
      req(length(d$dataset_all_concepts) > 0, nrow(d$dataset_all_concepts) > 0)
      
      type <- r[[paste0(category, "_reload_widget_vocabulary_concepts_type")]]
      
      vocabulary_id <- input[[paste0(type, "_vocabulary")]]$key
      req(length(vocabulary_id) > 0)
      
      widget_vocabulary_concepts <- d$dataset_all_concepts %>%
        dplyr::filter(vocabulary_id_1 == vocabulary_id) %>%
        dplyr::select(concept_id = concept_id_1, concept_name = concept_name_1, concept_display_name = concept_display_name_1,
          relationship_id, domain_id, concept_class_id, standard_concept, concept_code,
          count_persons_rows, count_concepts_rows, add_concept_input)
      
      widget_vocabulary_concepts <- widget_vocabulary_concepts %>% dplyr::filter(is.na(relationship_id))
      
      widget_vocabulary_concepts <-
        widget_vocabulary_concepts %>%
        dplyr::select(-relationship_id) %>%
        dplyr::mutate_at(c("add_concept_input"), stringr::str_replace_all, "%ns%", id) %>%
        dplyr::mutate_at("add_concept_input", stringr::str_replace_all, "%input_prefix%", paste0(type, "_add_concept")) %>%
        dplyr::mutate_at("add_concept_input", stringr::str_replace_all, "%input_prefix_2%", paste0(type, "_")) %>%
        dplyr::mutate_at("concept_id", as.character) %>%
        dplyr::mutate(add_concept_input = stringr::str_replace_all(add_concept_input, "%concept_id_1%", concept_id))
      
      r[[paste0(category, "_", type, "_vocabulary_concepts")]] <- widget_vocabulary_concepts
      
      if (length(r[[paste0("widget_", type, "_vocabulary_concepts_proxy")]]) == 0){
        editable_cols <- c("concept_display_name")
        searchable_cols <- c("concept_id", "concept_name", "concept_display_name")
        column_widths <- c("concept_id" = "80px", "action" = "80px")
        sortable_cols <- c("concept_id", "concept_name", "concept_display_name", "count_persons_rows", "count_concepts_rows")
        centered_cols <- c("concept_id", "count_persons_rows", "count_concepts_rows", "add_concept_input")
        col_names <- get_col_names("plugins_vocabulary_concepts_with_counts", i18n)
        hidden_cols <- c("domain_id", "concept_class_id", "standard_concept", "concept_code")
        column_widths <- c("concept_id" = "120px", "count_persons_rows" = "80px", "count_concepts_rows" = "80px", "add_concept_input" = "80px")
        
        # Render datatable
        render_datatable(output = output, ns = ns, i18n = i18n, data = widget_vocabulary_concepts,
          output_name = paste0(type, "_vocabulary_concepts"), col_names =  col_names,
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, hidden_col = hidden_cols)
        
        # Create a proxy for datatatable
        r[[paste0(category, "_", type, "_vocabulary_concepts_proxy")]] <- DT::dataTableProxy(paste0(type, "_vocabulary_concepts"), deferUntilFlush = FALSE)
        
        # if (input[[paste0(type, "_hide_concepts_datatables")]]) shinyjs::show(paste0(type, "_blank_space")) else shinyjs::hide(paste0(type, "_blank_space"))
      }
      else DT::replaceData(r[[paste0(category, "_", type, "_vocabulary_concepts_proxy")]], r[[paste0(category, "_", type, "_vocabulary_concepts")]], resetPaging = FALSE, rownames = FALSE)
    })
    
    # Update which cols are hidden
    
    observeEvent(input$widget_creation_vocabulary_concepts_table_cols, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$widget_creation_vocabulary_concepts_table_cols"))
      
      req(length(r[[paste0(category, "_widget_creation_vocabulary_concepts_proxy")]]) > 0)
      
      r[[paste0(category, "_widget_creation_vocabulary_concepts_proxy")]] %>%
        DT::showCols(0:9) %>%
        DT::hideCols(setdiff(0:9, input$widget_creation_vocabulary_concepts_table_cols))
    })
    
    observeEvent(input$widget_settings_vocabulary_concepts_table_cols, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$widget_settings_vocabulary_concepts_table_cols"))
      
      req(length(r[[paste0(category, "_widget_settings_vocabulary_concepts_proxy")]]) > 0)
      
      r[[paste0(category, "_widget_settings_vocabulary_concepts_proxy")]] %>%
        DT::showCols(0:9) %>%
        DT::hideCols(setdiff(0:9, input$widget_settings_vocabulary_concepts_table_cols))
    })
    
    observeEvent(input$widget_creation_vocabulary_mapped_concepts_table_cols, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$widget_creation_vocabulary_mapped_concepts_table_cols"))
      
      req(length(r[[paste0(category, "_widget_creation_vocabulary_mapped_concepts_proxy")]]) > 0)
      
      r[[paste0(category, "_widget_creation_vocabulary_mapped_concepts_proxy")]] %>%
        DT::showCols(0:8) %>%
        DT::hideCols(setdiff(0:8, input$widget_creation_vocabulary_mapped_concepts_table_cols))
    })
    
    observeEvent(input$widget_settings_vocabulary_mapped_concepts_table_cols, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$widget_settings_vocabulary_mapped_concepts_table_cols"))
      
      req(length(r[[paste0(category, "_widget_settings_vocabulary_mapped_concepts_proxy")]]) > 0)
      
      r[[paste0(category, "_widget_settings_vocabulary_mapped_concepts_proxy")]] %>%
        DT::showCols(0:8) %>%
        DT::hideCols(setdiff(0:8, input$widget_settings_vocabulary_mapped_concepts_table_cols))
    })
    
    # Updates in datatable
    
    observeEvent(input$widget_creation_vocabulary_concepts_cell_edit, {
      
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$widget_creation_vocabulary_concepts_cell_edit"))
      
      edit_info <- input$widget_creation_vocabulary_concepts_cell_edit
      r[[paste0(category, "_widget_creation_vocabulary_concepts")]] <- DT::editData(r[[paste0(category, "_widget_creation_vocabulary_concepts")]], edit_info, rownames = FALSE)
    })
    
    observeEvent(input$widget_creation_vocabulary_mapped_concepts_cell_edit, {
      
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$widget_creation_vocabulary_mapped_concepts_cell_edit"))
      
      edit_info <- input$widget_creation_vocabulary_mapped_concepts_cell_edit
      r[[paste0(category, "_widget_creation_vocabulary_mapped_concepts")]] <- DT::editData(r[[paste0(category, "_widget_creation_vocabulary_mapped_concepts")]], edit_info, rownames = FALSE)
    })
    
    observeEvent(input$widget_settings_vocabulary_concepts_cell_edit, {
      
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$widget_settings_vocabulary_concepts_cell_edit"))
      
      edit_info <- input$widget_settings_vocabulary_concepts_cell_edit
      r[[paste0(category, "_widget_settings_vocabulary_concepts")]] <- DT::editData(r[[paste0(category, "_widget_settings_vocabulary_concepts")]], edit_info, rownames = FALSE)
    })
    
    observeEvent(input$widget_settings_vocabulary_mapped_concepts_cell_edit, {
      
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$widget_settings_vocabulary_mapped_concepts_cell_edit"))
      
      edit_info <- input$widget_settings_vocabulary_mapped_concepts_cell_edit
      r[[paste0(category, "_widget_settings_vocabulary_mapped_concepts")]] <- DT::editData(r[[paste0(category, "_widget_settings_vocabulary_mapped_concepts")]], edit_info, rownames = FALSE)
    })
    
    # When add button is clicked
    
    observeEvent(input$widget_creation_concept_selected, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$widget_creation_concept_selected"))
      r[[paste0(category, "_widget_concept_selected")]] <- now()
      r[[paste0(category, "_widget_concept_selected_type")]] <- "widget_creation"
    })
    
    observeEvent(input$widget_settings_concept_selected, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$widget_settings_concept_selected"))
      r[[paste0(category, "_widget_concept_selected")]] <- now()
      r[[paste0(category, "_widget_concept_selected_type")]] <- "widget_settings"
    })
    
    observeEvent(r[[paste0(category, "_widget_concept_selected")]], {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer r$..widget_concept_selected"))
      
      type <- r[[paste0(category, "_widget_concept_selected_type")]]
      
      # Initiate r variable if doesn't exist
      if (length(r[[paste0(category, "_", type, "_vocabulary_selected_concepts")]]) == 0) r[[paste0(category, "_", type, "_vocabulary_selected_concepts")]] <- tibble::tibble(
        concept_id = integer(), concept_name = character(), concept_display_name = character(), domain_id = character(),
        mapped_to_concept_id = integer(), merge_mapped_concepts = logical())
      
      if (grepl("mapped", input[[paste0(type, "_concept_selected")]])) sub_type <- "mapped_concept"
      else sub_type <- "concept"
      
      # Get ID of selected concept
      link_id <- as.integer(substr(input[[paste0(type, "_concept_selected")]], nchar(paste0(id, "-", type, "_add_", sub_type, "_")) + 1, nchar(input[[paste0(type, "_concept_selected")]])))
      if (sub_type == "concept") concept_id <- link_id
      if (sub_type == "mapped_concept") concept_id <- r[[paste0(category, "_", type, "_vocabulary_mapped_concepts")]] %>% 
        dplyr::filter(id == link_id) %>% dplyr::pull(concept_id_2)
      
      # If this concept is not already selected, add it to the vocabulary_selected_concepts dropdown
      
      if (concept_id %not_in% r[[paste0(category, "_", type, "_vocabulary_selected_concepts")]]$concept_id){
        
        if (sub_type == "concept") new_data <- r[[paste0(category, "_", type, "_vocabulary_concepts")]] %>%
            dplyr::mutate_at("concept_id", as.integer) %>%
            dplyr::filter(concept_id == link_id) %>%
            dplyr::transmute(concept_id, concept_name, concept_display_name, domain_id, mapped_to_concept_id = NA_integer_, merge_mapped_concepts = FALSE)
        
        if (sub_type == "mapped_concept"){
          
          selected_concept <- r[[paste0(category, "_", type, "_vocabulary_mapped_concepts")]] %>%
            dplyr::mutate_at(c("concept_id_1", "concept_id_2"), as.integer) %>%
            dplyr::filter(id == link_id)
          
          new_data <- r[[paste0(category, "_", type, "_vocabulary_concepts")]] %>%
            dplyr::mutate_at("concept_id", as.integer) %>%
            dplyr::filter(concept_id == selected_concept$concept_id_1) %>%
            dplyr::transmute(concept_id, concept_name, concept_display_name, domain_id, 
                             mapped_to_concept_id = NA_integer_, merge_mapped_concepts = input[[paste0(type, "_merge_mapped_concepts")]]) %>%
            dplyr::bind_rows(
              selected_concept %>%
                dplyr::transmute(concept_id = concept_id_2, concept_name = concept_name_2, concept_display_name = concept_display_name_2, domain_id,
                                 mapped_to_concept_id = concept_id_1, merge_mapped_concepts = input[[paste0(type, "_merge_mapped_concepts")]])
            ) %>%
            dplyr::bind_rows(
              r[[paste0(category, "_", type, "_vocabulary_selected_concepts")]] %>% 
                dplyr::filter(mapped_to_concept_id == selected_concept$concept_id_1) %>%
                dplyr::mutate(merge_mapped_concepts = input[[paste0(type, "_merge_mapped_concepts")]])
            )
          
          # Add also original concept, which concepts are mapped from
          r[[paste0(category, "_", type, "_vocabulary_selected_concepts")]] <- r[[paste0(category, "_", type, "_vocabulary_selected_concepts")]] %>% 
            dplyr::filter(concept_id != selected_concept$concept_id_1, (is.na(mapped_to_concept_id) | mapped_to_concept_id != selected_concept$concept_id_1))
        }
        
        r[[paste0(category, "_", type, "_vocabulary_selected_concepts")]] <- new_data %>% dplyr::bind_rows(r[[paste0(category, "_", type, "_vocabulary_selected_concepts")]])
      }
      
      # Update dropdown of selected concepts
      
      r[[paste0(category, "_widget_vocabulary_update_selected_concepts_dropdown")]] <- now()
      r[[paste0(category, "_widget_vocabulary_update_selected_concepts_dropdown_type")]] <- type
      
    })
    
    # When reset button is clicked
    observeEvent(input$widget_creation_reset_vocabulary_concepts, {
      
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$reset_vocabulary_concepts"))
      
      r[[paste0(category, "_widget_creation_vocabulary_selected_concepts")]] <- r[[paste0(category, "_widget_creation_vocabulary_selected_concepts")]] %>% dplyr::slice(0)
      r[[paste0(category, "_widget_vocabulary_selected_concepts_trigger")]] <- now()
      r[[paste0(category, "_widget_vocabulary_selected_concepts_trigger_type")]] <- "widget_creation"
    })
    
    observeEvent(input$widget_settings_reset_vocabulary_concepts, {
      
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$reset_vocabulary_concepts"))
      
      r[[paste0(category, "_widget_settings_vocabulary_selected_concepts")]] <- r[[paste0(category, "_widget_settings_vocabulary_selected_concepts")]] %>% dplyr::slice(0)
      r[[paste0(category, "_widget_vocabulary_selected_concepts_trigger")]] <- now()
      r[[paste0(category, "_widget_vocabulary_selected_concepts_trigger_type")]] <- "widget_settings"
    })
    
    # When dropdown is modified
    
    observeEvent(input$widget_creation_vocabulary_selected_concepts_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$widget_creation_vocabulary_selected_concepts_trigger"))
      r[[paste0(category, "_widget_vocabulary_selected_concepts_trigger")]] <- now()
      r[[paste0(category, "_widget_vocabulary_selected_concepts_trigger_type")]] <- "widget_creation"
    })
    observeEvent(input$widget_settings_vocabulary_selected_concepts_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$widget_settings_vocabulary_selected_concepts_trigger"))
      r[[paste0(category, "_widget_vocabulary_selected_concepts_trigger")]] <- now()
      r[[paste0(category, "_widget_vocabulary_selected_concepts_trigger_type")]] <- "widget_settings"
    })
    
    observeEvent(r[[paste0(category, "_widget_vocabulary_selected_concepts_trigger")]], {
      
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer r$..widget_vocabulary_selected_concepts_trigger"))
      
      type <- r[[paste0(category, "_widget_vocabulary_selected_concepts_trigger_type")]]
      
      if (length(input[[paste0(type, "_vocabulary_selected_concepts")]]) == 0) r[[paste0(category, "_", type, "_vocabulary_selected_concepts")]] <- r[[paste0(category, "_", type, "_vocabulary_selected_concepts")]] %>% dplyr::slice(0)
      if (length(input[[paste0(type, "_vocabulary_selected_concepts")]]) > 0) {
        r[[paste0(category, "_", type, "_vocabulary_selected_concepts")]] <- r[[paste0(category, "_", type, "_vocabulary_selected_concepts")]] %>%
          dplyr::filter(concept_id %in% input[[paste0(type, "_vocabulary_selected_concepts")]])
        
        # Delete also mapped concepts
        r[[paste0(category, "_", type, "_vocabulary_selected_concepts")]] <- r[[paste0(category, "_", type, "_vocabulary_selected_concepts")]] %>%
          dplyr::filter(is.na(mapped_to_concept_id) | mapped_to_concept_id %in% r[[paste0(category, "_", type, "_vocabulary_selected_concepts")]]$concept_id)
      }
      
      r[[paste0(category, "_widget_vocabulary_update_selected_concepts_dropdown")]] <- now()
      r[[paste0(category, "_widget_vocabulary_update_selected_concepts_dropdown_type")]] <- type
    })
    
    # Update dropdown of selected concepts
    
    observeEvent(r[[paste0(category, "_widget_vocabulary_update_selected_concepts_dropdown")]], {
      
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer r$..widget_vocabulary_update_selected_concepts_dropdown"))
      
      type <- r[[paste0(category, "_widget_vocabulary_update_selected_concepts_dropdown_type")]]
      
      options <- convert_tibble_to_list(
        r[[paste0(category, "_", type, "_vocabulary_selected_concepts")]] %>%
          dplyr::mutate(concept_name = ifelse(!is.na(mapped_to_concept_id), paste0("--- ", concept_name), concept_name)), 
        key_col = "concept_id", text_col = "concept_name", i18n = i18n)
      value <- r[[paste0(category, "_", type, "_vocabulary_selected_concepts")]] %>% dplyr::pull(concept_id)
      shiny.fluent::updateDropdown.shinyInput(session, paste0(type, "_vocabulary_selected_concepts"),
                                              options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
    })
    
    ## Widget add button clicked ----
    
    observeEvent(input$widget_creation_save, {
      
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$widget_creation_save"))
      
      new_data <- list()
      
      selected_tab <- r[[paste0(category, "_selected_tab")]]
      new_data$name <- coalesce2(type = "char", x = input$widget_creation_name)
      new_data$tab_group <- r[[paste0(category, "_tabs")]] %>% dplyr::filter(id == selected_tab) %>% dplyr::pull(tab_group_id)
      new_data$tab_new_element <- r[[paste0(category, "_selected_tab")]]
      new_data$plugin <- input$widget_creation_plugin$key
      
      # Check if name is not empty
      if (is.na(new_data$name)) shiny.fluent::updateTextField.shinyInput(session, "widget_creation_name", errorMessage = i18n$t("provide_valid_name"))
      else shiny.fluent::updateTextField.shinyInput(session, "widget_creation_name", errorMessage = NULL)
      req(!is.na(new_data$name))
      
      # Check if values required to be unique are unique
      
      table <- paste0(category, "_widgets")
      
      sql <- glue::glue_sql("SELECT DISTINCT(name) FROM widgets WHERE deleted IS FALSE AND tab_id = {new_data$tab_new_element}", .con = r$db)
      distinct_values <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
      if (new_data$name %in% distinct_values) show_message_bar(output, "name_already_used", "severeWarning", i18n = i18n, ns = ns)
      req(new_data$name %not_in% distinct_values)
      
      # Check if dropdowns are not empty (if all are required)
      dropdowns_check <- TRUE
      
      required_dropdowns <- c("plugin")
      
      for (dropdown in required_dropdowns){
        if (is.null(new_data[[dropdown]])) dropdowns_check <- FALSE
        else if (is.na(new_data[[dropdown]])) dropdowns_check <- FALSE
      }
      
      if (!dropdowns_check) show_message_bar(output, "dropdown_empty", "severeWarning", i18n = i18n, ns = ns)
      req(dropdowns_check)
      
      # Get last_row nb
      # last_row_widgets <- get_last_row(r$db, paste0(category, "_widgets"))
      widget_id <- get_last_row(r$db, "widgets") + 1
      sql <- glue::glue_sql("SELECT COALESCE(MAX(display_order), 0) FROM widgets WHERE tab_id = {new_data$tab_new_element}", .con = r$db)
      last_display_order <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull() %>% as.integer()
      
      new_data <- tibble::tribble(
        ~id, ~name, ~category, ~tab_id, ~plugin_id, ~display_order, ~creator_id, ~datetime, ~deleted,
        widget_id, as.character(new_data$name), category, as.integer(new_data$tab_new_element),
        as.integer(new_data$plugin), last_display_order + 1, r$user_id, now(), FALSE)
      
      DBI::dbAppendTable(r$db, "widgets", new_data)
      add_log_entry(r = r, category = paste0(table, " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_data))
      r[[paste0(category, "_widgets")]] <- r[[paste0(category, "_widgets")]] %>% dplyr::bind_rows(new_data)
      
      last_row_widgets_concepts <- get_last_row(m$db, "widgets_concepts")
      
      has_vocabulary_concepts <- TRUE
      selected_concepts <- tibble::tibble(
        concept_id = integer(), concept_name = character(), concept_display_name = character(), domain_id = character(),
        mapped_to_concept_id = integer(), merge_mapped_concepts = logical())
      
      if (length(r[[paste0(category, "_widget_creation_vocabulary_selected_concepts")]]) == 0) has_vocabulary_concepts <- FALSE
      if (length(r[[paste0(category, "_widget_creation_vocabulary_selected_concepts")]]) > 0) if (nrow(r[[paste0(category, "_widget_creation_vocabulary_selected_concepts")]]) == 0) has_vocabulary_concepts <- FALSE
      
      if (has_vocabulary_concepts){
        
        new_data <-
          r[[paste0(category, "_widget_creation_vocabulary_selected_concepts")]] %>%
          dplyr::transmute(
            id = 1:dplyr::n() + last_row_widgets_concepts + 1, widget_id = !!widget_id,
            concept_id, concept_name, concept_display_name, domain_id, mapped_to_concept_id, merge_mapped_concepts, 
            creator_id = r$user_id, datetime = now(), deleted = FALSE
          )
        
        DBI::dbAppendTable(m$db, "widgets_concepts", new_data)
        add_log_entry(r = r, category = paste0(table, " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_data))
        r[[paste0(category, "_widgets_concepts")]] <- r[[paste0(category, "_widgets_concepts")]] %>% dplyr::bind_rows(new_data)
        
        # Vocabulary concepts for server code
        selected_concepts <- r[[paste0(category, "_widget_creation_vocabulary_selected_concepts")]]
        
        # Reset r$..widget_vocabulary_selected_concepts & dropdown
        r[[paste0(category, "_widget_creation_vocabulary_selected_concepts")]] <- tibble::tibble(
          concept_id = integer(), concept_name = character(), concept_display_name = character(), domain_id = character(),
          mapped_to_concept_id = integer(), merge_mapped_concepts = logical())
        
        shiny.fluent::updateDropdown.shinyInput(session, "widget_creation_vocabulary_selected_concepts", options = list(), multiSelect = TRUE, multiSelectDelimiter = " || ")
      }
      
      show_message_bar(output, message = paste0(get_singular(paste0(category, "_widgets")), "_added"), type = "success", i18n = i18n, ns = ns)
      
      # Reset name textfield & dropdowns
      shiny.fluent::updateTextField.shinyInput(session, "widget_creation_name", value = "")
      
      # # Load translations file
      # 
      # plugin_id <- input$widget_creation_plugin$key
      # 
      # plugin_translations <- r$code %>% dplyr::filter(link_id == plugin_id, category == "plugin_translations") %>% dplyr::pull(code)
      # 
      # if (plugin_translations != ""){
      #   
      #   tryCatch({
      #     # Get plugin unique_id
      #     plugin_unique_id <- r$options %>% dplyr::filter(category == "plugin", name == "unique_id", link_id == plugin_id) %>% dplyr::pull(value)
      #     
      #     # Create plugin folder in translations folder if doesn't exist
      #     new_dir <- paste0(r$app_folder, "/translations/", plugin_unique_id)
      #     if (!dir.exists(new_dir)) dir.create(new_dir)
      #     
      #     # Create a csv with all languages
      #     data <- read.csv(text = plugin_translations, header = TRUE, stringsAsFactors = FALSE)
      #     
      #     # Create one csv by language
      #     for(lang in names(data)[-1]){
      #       file_name <- paste0(new_dir, "/translation_", lang, ".csv")
      #       
      #       if (!file.exists(file_name)){
      #         # Create a new dataframe with base & current language cols
      #         data_lang <- data[, c("base", lang)]
      #         
      #         # Create csv
      #         write.csv(data_lang, file_name, row.names = FALSE)
      #       }
      #     }
      #   },
      #   error = function(e) report_bug(r = r, output = output, error_message = "error_creating_translations_file",
      #     error_name = paste0(id, " - create translations files - plugin_id ", plugin_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
      #   
      #   tryCatch({
      #     i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = new_dir))
      #     i18np$set_translation_language(language)},
      #     error = function(e) report_bug(r = r, output = output, error_message = "error_creating_new_translator",
      #       error_name = paste0(id, " - create i18np translator - plugin_id ", plugin_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
      # }
      # 
      # # Run server code
      # 
      # trace_code <- paste0(category, "_", widget_id, "_", m$selected_study)
      # # if (trace_code %in% r$server_tabs_groups_loaded) print(trace_code)
      # if (trace_code %not_in% r$server_tabs_groups_loaded){
      #   
      #   # Add the trace_code to loaded plugins list
      #   r$server_tabs_groups_loaded <- c(r$server_tabs_groups_loaded, trace_code)
      #   
      #   # Get plugin code
      #   
      #   # Check if plugin has been deleted
      #   check_deleted_plugin <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM plugins WHERE id = ", input$widget_creation_plugin$key)) %>% dplyr::pull(deleted)
      #   if (!check_deleted_plugin){
      #     
      #     # NB : req(m[[session_code]] == session_num) & req(m$selected_study == %study_id%) must be put at the beginning of each observeEvent in plugins code
      #     
      #     code_server_card <- r$code %>%
      #       dplyr::filter(link_id == input$widget_creation_plugin$key, category == "plugin_server") %>%
      #       dplyr::pull(code) %>%
      #       stringr::str_replace_all("%tab_id%", as.character(r[[paste0(category, "_selected_tab")]])) %>%
      #       stringr::str_replace_all("%widget_id%", as.character(widget_id)) %>%
      #       stringr::str_replace_all("%req%", "req(m[[session_code]] == session_num)\nreq(m$selected_study == %study_id%)") %>%
      #       stringr::str_replace_all("%study_id%", as.character(m$selected_study)) %>%
      #       stringr::str_replace_all("\r", "\n") %>%
      #       stringr::str_replace_all("''", "'")
      #     
      #     # If it is an aggregated plugin, change %study_id% with current selected study
      #     if (length(m$selected_study) > 0) code_server_card <- code_server_card %>% stringr::str_replace_all("%study_id%", as.character(m$selected_study))
      #   }
      #   else code_server_card <- ""
      #   
      #   # Create a session number, to inactivate older observers
      #   # Reset all older observers for this widget_id
      #   
      #   session_code <- paste0(category, "_widget_", widget_id)
      #   if (length(m[[session_code]]) == 0) session_num <- 1L
      #   if (length(m[[session_code]]) > 0) session_num <- m[[session_code]] + 1
      #   m[[session_code]] <- session_num
      #   
      #   # Variables to hide
      #   new_env_vars <- list("r" = NA)
      #   
      #   # Variables to keep
      #   variables_to_keep <- c("d", "m", "session_code", "session_num", "i18n", "selected_concepts", "debug")
      #   if (exists("i18np")) variables_to_keep <- c(variables_to_keep, "i18np")
      #   
      #   for (var in variables_to_keep) new_env_vars[[var]] <- eval(parse(text = var))
      #   new_env <- rlang::new_environment(data = new_env_vars, parent = pryr::where("r"))
      #   
      #   tryCatch(eval(parse(text = code_server_card), envir = new_env),
      #     error = function(e) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code",
      #       error_name = paste0(id, " - add_new_widget - run_plugin_code - plugin_id ", input$widget_creation_plugin$key), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
      #   
      #   # Code for toggle reactivity
      #   toggle <- paste0(category, "_widget_", widget_id)
      #   
      #   observeEvent(input[[paste0(toggle, "_toggle")]], {
      #     if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle)
      #     else shinyjs::hide(toggle)
      #   })
      #   
      #   # Code for removing widget
      #   
      #   observeEvent(input[[paste0(category, "_remove_widget_", widget_id)]], {
      #     r[[paste0(category, "_selected_widget")]] <- widget_id
      #     shinyjs::show("delete_widget_modal")
      #   })
      #   
      #   # Code for widget settings
      #   
      #   observeEvent(input[[paste0(category, "_widget_settings_", widget_id)]], {
      #     r[[paste0(category, "_widget_settings_trigger")]] <- now()
      #     r[[paste0(category, "_widget_settings")]] <- widget_id
      #   })
      #   
      # }
      
      # Prepare widget UI code
      tab_id <- r[[paste0(category, "_selected_tab")]]
      
      code_ui_card <- isolate(r$code) %>% dplyr::filter(link_id == input$widget_creation_plugin$key, category == "plugin_ui") %>% dplyr::pull(code)
      element_code <- div()
      widget_name <- r[[paste0(category, "_widgets")]] %>% dplyr::filter(widget_id == !!widget_id) %>% dplyr::slice(1) %>% dplyr::pull(name)
      
      code_ui_card <- code_ui_card %>%
        stringr::str_replace_all("%tab_id%", as.character(tab_id)) %>%
        stringr::str_replace_all("%widget_id%", as.character(widget_id)) %>%
        stringr::str_replace_all("%req%", "req(m[[session_code]] == session_num)\nreq(m$selected_study == %study_id%)") %>%
        stringr::str_replace_all("\r", "\n") %>%
        stringr::str_replace_all("''", "'") %>%
        stringr::str_replace_all("%study_id%", as.character(isolate(m$selected_study)))
      
      code_ui <- tryCatch(eval(parse(text = code_ui_card)), error = function(e){
        report_bug(r = r, output = output, error_message = "error_run_plugin_code_ui_card",
          error_name = paste0(id, " - run ui code - ", widget_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns)
        p(toString(e))
      }, warning = function(w){
        report_bug(r = r, output = output, error_message = "error_run_plugin_code_ui_card",
          error_name = paste0(id, " - run ui code - ", widget_id), category = "Error", error_report = toString(w), i18n = i18n, ns = ns)
        p(toString(w))
      })
      
      element_code <- div(
        make_shiny_ace_card("",
          div(
            div(id = ns(paste0(category, "_widget_plugin_ui_", widget_id)), code_ui),
            div(
              id = ns(paste0(category, "_widget_settings_remove_buttons_", widget_id)),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 2),
                uiOutput(ns(paste0("additional_buttons_", widget_id))),
                actionButton(ns(paste0(category, "_widget_settings_", widget_id)), "", icon = icon("cog")),
                actionButton(ns(paste0(category, "_remove_widget_", widget_id)), "", icon = icon("trash-alt"))
              ),
              style = "position:absolute; top:8px; right: 10px;"
            )
          ),
          style = "position:relative;"
        )
      )
      
      widgets <- r[[paste0(category, "_widgets")]] %>% dplyr::filter(tab_id == !!tab_id) %>% dplyr::rename(widget_id = id) %>% dplyr::arrange(display_order)
      
      # Get widget ids
      distinct_widgets <- unique(widgets$widget_id)
      
      # Reset opened cards
      r[[paste0(category, "_opened_cards")]] <- ""
      
      # Loop over distinct cards (widgets), for this tab
      
      # Use sapply instead of for loop, cause with for loop, widget_id doesn't change
      sapply(distinct_widgets, function(widget_id){
        
        # Get name of widget
        widget_name <- widgets %>% dplyr::filter(widget_id == !!widget_id) %>% dplyr::slice(1) %>% dplyr::pull(name)
        
        # Add to the list of opened cards
        r[[paste0(category, "_opened_cards")]] <- c(r[[paste0(category, "_opened_cards")]], paste0(category, "_widget_", widget_id))
      })
      
      r[[paste0(category, "_opened_cards")]] <- c(r[[paste0(category, "_opened_cards")]], paste0(category, "_toggles_", tab_id))
      
      # Show opened cards
      sapply(r[[paste0(category, "_opened_cards")]], shinyjs::show)
      
      # Add tab toggles UI
      # insertUI(selector = paste0("#", ns("study_cards")), where = "afterBegin", ui = uiOutput(ns(paste0(category, "_toggles_", tab_id))))
      # output[[paste0(category, "_toggles_", tab_id)]] <- renderUI(toggles_div)
      
      # Add widget UI
      # insertUI(selector = paste0("#", ns("study_cards")), where = "beforeEnd", ui = uiOutput(ns(paste0(category, "_widget_", widget_id))))
      # output[[paste0(category, "_widget_", widget_id)]] <- renderUI(element_code)
      
      # Reload UI menu
      r[[paste0(category, "_reload_tabs")]] <- now()
      # r[[paste0(category, "_reload_menu")]] <- now()
    })
    
    # --- --- --- --- -- -
    # Widget settings ----
    # --- --- --- --- -- -
    
    # observeEvent(r[[paste0(category, "_widget_settings_trigger")]], {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer r$..widget_settings_trigger"))
    #   
    #   sapply(r[[paste0(category, "_opened_cards")]], shinyjs::hide)
    #   shinyjs::show(paste0(category, "_widget_settings"))
    #   r[[paste0(category, "_widget_card_selected_type")]] <- "widget_settings"
    #   
    #   widget_id <- r[[paste0(category, "_widget_settings")]]
    #   widget_infos <- r[[paste0(category, "_widgets")]] %>% dplyr::filter(id == widget_id)
    #   req(nrow(widget_infos) > 0)
    #   
    #   # Update name & plugin textfields
    #   
    #   widget_plugin_infos <- r$plugins %>% dplyr::filter(id == widget_infos$plugin_id)
    #   
    #   shiny.fluent::updateTextField.shinyInput(session = session, "widget_settings_name", value = widget_infos$name)
    #   shiny.fluent::updateTextField.shinyInput(session = session, "widget_settings_plugin", value = widget_plugin_infos$name)
    #   
    #   # Get selected_concepts for this widget
    #   
    #   if (nrow(r[[paste0(category, "_widgets_concepts")]] %>%
    #            dplyr::filter(widget_id == r[[paste0(category, "_widget_settings")]])) > 0){
    #     
    #     r[[paste0(category, "_widget_settings_vocabulary_selected_concepts")]] <- r[[paste0(category, "_widgets_concepts")]] %>%
    #       dplyr::filter(widget_id == r[[paste0(category, "_widget_settings")]]) %>%
    #       dplyr::select(concept_id, concept_name, concept_display_name, domain_id, mapped_to_concept_id, merge_mapped_concepts)
    #     
    #     r[[paste0(category, "_widget_vocabulary_update_selected_concepts_dropdown")]] <- now()
    #     r[[paste0(category, "_widget_vocabulary_update_selected_concepts_dropdown_type")]] <- "widget_settings"
    #   }
    # })
    # 
    # # Close button clicked
    # observeEvent(input[[paste0(category, "_close_widget_settings")]], {
    #   if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$..close_widget_settings"))
    #   shinyjs::hide(paste0(category, "_widget_settings"))
    #   sapply(r[[paste0(category, "_opened_cards")]], shinyjs::show)
    # })
    # 
    # # Save updates
    # observeEvent(input$widget_settings_save, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$..widget_settings_save"))
    #   
    #   new_data <- list()
    #   
    #   new_data$name <- coalesce2(type = "char", x = input$widget_settings_name)
    #   
    #   widget_id <- r[[paste0(category, "_widget_settings")]]
    #   ids <- r[[paste0(category, "_widgets")]] %>% dplyr::filter(id == widget_id) %>% dplyr::slice(1) %>% dplyr::select(plugin_id, tab_id)
    #   
    #   # Check if name is not empty
    #   if (is.na(new_data$name)) shiny.fluent::updateTextField.shinyInput(session, "widget_settings_name", errorMessage = i18n$t("provide_valid_name"))
    #   else shiny.fluent::updateTextField.shinyInput(session, "widget_settings_name", errorMessage = NULL)
    #   req(!is.na(new_data$name))
    #   
    #   # Check if values required to be unique are unique
    #   
    #   sql <- glue::glue_sql("SELECT DISTINCT(name) FROM widgets WHERE deleted IS FALSE AND tab_id = {ids$tab_id} AND id != {widget_id}", .con = r$db)
    #   distinct_values <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
    #   if (new_data$name %in% distinct_values) show_message_bar(output,  "name_already_used", "severeWarning", i18n = i18n, ns = ns)
    #   req(new_data$name %not_in% distinct_values)
    #   
    #   # Update name in database & r var
    #   r[[paste0(category, "_widgets")]] <- r[[paste0(category, "_widgets")]] %>%
    #     dplyr::mutate(name = dplyr::case_when(
    #       id == widget_id ~ new_data$name,
    #       TRUE ~ name
    #     ))
    #   sql <- glue::glue_sql("UPDATE widgets SET name = {new_data$name} WHERE id = {widget_id}", .con = r$db)
    #   query <- DBI::dbSendStatement(r$db, sql)
    #   DBI::dbClearResult(query)
    #   
    #   # Get last_row nb
    #   last_row_widgets_concepts <- get_last_row(m$db, "widgets_concepts")
    #   
    #   has_vocabulary_concepts <- TRUE
    #   selected_concepts <- tibble::tibble(concept_id = integer(), concept_name = character(), concept_display_name = character(), domain_id = character(),
    #                                       mapped_to_concept_id = integer(), merge_mapped_concepts = logical())
    #   
    #   if (length(r[[paste0(category, "_widget_settings_vocabulary_selected_concepts")]]) == 0) has_vocabulary_concepts <- FALSE
    #   if (length(r[[paste0(category, "_widget_settings_vocabulary_selected_concepts")]]) > 0) if (nrow(r[[paste0(category, "_widget_settings_vocabulary_selected_concepts")]]) == 0) has_vocabulary_concepts <- FALSE
    #   
    #   if (has_vocabulary_concepts){
    #     
    #     new_data <-
    #       r[[paste0(category, "_widget_settings_vocabulary_selected_concepts")]] %>%
    #       dplyr::transmute(
    #         id = 1:dplyr::n() + last_row_widgets_concepts + 1, widget_id = !!widget_id,
    #         concept_id, concept_name, concept_display_name, domain_id, mapped_to_concept_id, merge_mapped_concepts, 
    #         creator_id = r$user_id, datetime = now(), deleted = FALSE
    #       )
    #     
    #     # Remove old data
    #     sql <- glue::glue_sql("UPDATE widgets_concepts SET deleted = TRUE WHERE widget_id = {widget_id}", .con = m$db)
    #     query <- DBI::dbSendStatement(m$db, sql)
    #     DBI::dbClearResult(query)
    #     r[[paste0(category, "_widgets_concepts")]] <- r[[paste0(category, "_widgets_concepts")]] %>% dplyr::filter(widget_id != !!widget_id)
    #     
    #     # Add new data
    #     DBI::dbAppendTable(m$db, "widgets_concepts", new_data)
    #     r[[paste0(category, "_widgets_concepts")]] <- r[[paste0(category, "_widgets_concepts")]] %>% dplyr::bind_rows(new_data)
    #     
    #     selected_concepts <- r[[paste0(category, "_widget_settings_vocabulary_selected_concepts")]]
    #   }
    #   
    #   show_message_bar(output, message = "modif_saved", type = "success", i18n = i18n, ns = ns)
    #   
    #   # Load translations file
    #   
    #   plugin_translations <- r$code %>% dplyr::filter(link_id == ids$plugin_id, category == "plugin_translations") %>% dplyr::pull(code)
    #   
    #   if (plugin_translations != ""){
    #     
    #     tryCatch({
    #       # Get plugin unique_id
    #       plugin_unique_id <- r$options %>% dplyr::filter(category == "plugin", name == "unique_id", link_id == ids$plugin_id) %>% dplyr::pull(value)
    #       
    #       # Create plugin folder in translations folder if doesn't exist
    #       new_dir <- paste0(r$app_folder, "/translations/", plugin_unique_id)
    #       if (!dir.exists(new_dir)) dir.create(new_dir)
    #       
    #       # Create a csv with all languages
    #       data <- read.csv(text = plugin_translations, header = TRUE, stringsAsFactors = FALSE)
    #       
    #       # Create one csv by language
    #       for(lang in names(data)[-1]){
    #         file_name <- paste0(new_dir, "/translation_", lang, ".csv")
    #         
    #         if (!file.exists(file_name)){
    #           # Create a new dataframe with base & current language cols
    #           data_lang <- data[, c("base", lang)]
    #           
    #           # Create csv
    #           write.csv(data_lang, file_name, row.names = FALSE)
    #         }
    #       }
    #     },
    #     error = function(e) report_bug(r = r, output = output, error_message = "error_creating_translations_file",
    #                                    error_name = paste0(id, " - create translations files - plugin_id ", ids$plugin_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
    #     
    #     tryCatch({
    #       i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = new_dir))
    #       i18np$set_translation_language(language)},
    #       error = function(e) report_bug(r = r, output = output, error_message = "error_creating_new_translator",
    #                                      error_name = paste0(id, " - create i18np translator - plugin_id ", ids$plugin_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
    #   }
    #   
    #   # Run server code
    #   
    #   code_server_card <- r$code %>%
    #     dplyr::filter(link_id == ids$plugin_id, category == "plugin_server") %>%
    #     dplyr::pull(code) %>%
    #     stringr::str_replace_all("%tab_id%", as.character(ids$tab_id)) %>%
    #     stringr::str_replace_all("%widget_id%", as.character(widget_id)) %>%
    #     stringr::str_replace_all("%req%", "req(m[[session_code]] == session_num)\nreq(m$selected_study == %study_id%)") %>%
    #     stringr::str_replace_all("%study_id%", as.character(m$selected_study)) %>%
    #     stringr::str_replace_all("\r", "\n") %>%
    #     stringr::str_replace_all("''", "'")
    #   
    #   # If it is an aggregated plugin, change %study_id% with current selected study
    #   if (length(m$selected_study) > 0) code_server_card <- code_server_card %>% stringr::str_replace_all("%study_id%", as.character(m$selected_study))
    #   
    #   session_code <- paste0(category, "_widget_", widget_id)
    #   if (length(m[[session_code]]) == 0) session_num <- 1L
    #   if (length(m[[session_code]]) > 0) session_num <- m[[session_code]] + 1
    #   m[[session_code]] <- session_num
    #   
    #   # Variables to hide
    #   new_env_vars <- list("r" = NA)
    #   
    #   # Variables to keep
    #   variables_to_keep <- c("d", "m", "session_code", "session_num", "i18n", "selected_concepts", "debug")
    #   if (exists("i18np")) variables_to_keep <- c(variables_to_keep, "i18np")
    #   
    #   for (var in variables_to_keep) new_env_vars[[var]] <- eval(parse(text = var))
    #   new_env <- rlang::new_environment(data = new_env_vars, parent = pryr::where("r"))
    #   
    #   tryCatch(eval(parse(text = code_server_card), envir = new_env),
    #            error = function(e) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code",
    #                                           error_name = paste0(id, " - save_widget_settings - run_plugin_code - plugin_id ", ids$plugin_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
    #   
    #   # Update toggles
    #   
    #   widgets <- r[[paste0(category, "_widgets")]] %>% dplyr::filter(tab_id == ids$tab_id) %>% dplyr::rename(widget_id = id) %>% dplyr::arrange(display_order)
    #   
    #   # Get widget widget_id
    #   distinct_widgets <- unique(widgets$widget_id)
    #   
    #   toggles <- tagList()
    #   
    #   # Loop over distinct cards (tabs elements), for this tab
    #   # Use sapply instead of for loop, cause with for loop, widget_id doesn't change
    #   sapply(distinct_widgets, function(widget_id){
    #     
    #     # Get name of widget
    #     widget_name <- widgets %>% dplyr::filter(widget_id == !!widget_id) %>% dplyr::slice(1) %>% dplyr::pull(name)
    #     
    #     toggles <<- tagList(toggles,
    #                         shiny.fluent::Toggle.shinyInput(ns(paste0(category, "_widget_", widget_id, "_toggle")), value = TRUE, style = "margin-top:10px;"),
    #                         div(class = "toggle_title", widget_name, style = "padding-top:10px;"))
    #     
    #     # Add to the list of opened cards
    #     r[[paste0(category, "_opened_cards")]] <- c(r[[paste0(category, "_opened_cards")]], paste0(category, "_widget_", widget_id))
    #   })
    #   
    #   # toggles_div <- div(
    #   #   make_card("",
    #   #     shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
    #   #       shiny.fluent::ActionButton.shinyInput(ns(paste0(category, "_add_widget_", ids$tab_id)), i18n$t("new_widget"), iconProps = list(iconName = "Add"),
    #   #         onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", category, "_add_widget_trigger', Math.random())"))),
    #   #       shiny.fluent::ActionButton.shinyInput(ns(paste0(category, "_edit_tab_", ids$tab_id)), i18n$t("edit_tab"), iconProps = list(iconName = "Edit"),
    #   #         onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-edit_tab_trigger', Math.random())"))),
    #   #       div(style = "width:20px;"),
    #   #       toggles
    #   #     )
    #   #   )
    #   # )
    #   
    #   # output[[paste0(category, "_toggles_", ids$tab_id)]] <- renderUI(toggles_div)
    #   
    #   # Hide settings card and show opened cards
    #   shinyjs::hide(paste0(category, "_widget_settings"))
    #   sapply(r[[paste0(category, "_opened_cards")]], shinyjs::show)
    # })
    
    # --- --- --- --- -- -
    # Delete a widget ----
    # --- --- --- --- -- -
    
    observeEvent(input$close_widget_deletion_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$close_file_deletion_modal"))
      shinyjs::hide("delete_widget_modal")
    })
    
    observeEvent(input$confirm_widget_deletion, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$confirm_widget_deletion"))
      
      widget_id <- r[[paste0(category, "_selected_widget")]]
      
      # Delete from database
      sql <- glue::glue_sql("DELETE FROM widgets WHERE id = {widget_id}", .con = m$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      # Update r var
      r[[paste0(category, "_widgets")]] <- r[[paste0(category, "_widgets")]] %>% dplyr::filter(id != widget_id)
      
      # Remove widget from gridstacks
      # ...
      
      # Close modal
      shinyjs::hide("delete_widget_modal")
      
      # Notify user
      show_message_bar(output,  "widget_deleted", "warning", i18n = i18n, ns = ns)
    })
    
    # --- --- --- --
    # Edit page ----
    # --- --- --- --
    
    observeEvent(input$edit_page_on, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$edit_page_on"))
      
      # Enable gridster edition
      sapply(r[[paste0(category, "_cards")]], function(gridster_id) shinyjs::runjs(paste0(gridster_id, ".enable().enable_resize();")))
      shinyjs::addClass(selector = ".gridster", class = "editable_gridster")
      
      # Show edit and delete widget buttons
      sapply(r[[paste0(category, "_widgets")]]$id, function(widget_id) shinyjs::show(paste0(category, "_widget_settings_remove_buttons_", widget_id)))
      
      # Show quit edit page button
      shinyjs::hide("edit_page_on_div")
      shinyjs::delay(100, shinyjs::show("edit_page_off_div"))
    })
    
    observeEvent(input$edit_page_off, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$edit_page_off"))
      
      # Disable gridster edition
      sapply(r[[paste0(category, "_cards")]], function(gridster_id) shinyjs::runjs(paste0(gridster_id, ".disable().disable_resize();")))
      shinyjs::removeClass(selector = ".gridster", class = "editable_gridster")
      
      # Hide edit and delete widget buttons
      sapply(r[[paste0(category, "_widgets")]]$id, function(widget_id) shinyjs::hide(paste0(category, "_widget_settings_remove_buttons_", widget_id)))
      
      # Show edit page button
      shinyjs::hide("edit_page_off_div")
      shinyjs::delay(50, shinyjs::show("edit_page_on_div"))
    })
  })
}
