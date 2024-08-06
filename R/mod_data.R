#' @noRd
mod_data_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  result <- ""
  language <- "EN"
  
  # Load status modal ----
  
  load_status_modal <- shinyjs::hidden(
    div(
      id = ns("load_status_modal"),
      div(
        h1("Loading project"),
        uiOutput(ns("load_status")),
        class = "load_status_modal_content"
      ),
      class = "load_status_modal"
    )
  )
  
  # Add a tab modal ----
  
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
          shinyjs::hidden(
            shiny.fluent::ChoiceGroup.shinyInput(
              ns("add_tab_type"), value = "same_level", 
              options = list(
                list(key = "same_level", text = i18n$t("same_level_current_tab")),
                list(key = "level_under", text = i18n$t("level_under"))
              ),
              className = "inline_choicegroup"
            )
          ),
          div(shiny.fluent::TextField.shinyInput(ns("tab_name"), label = i18n$t("name")), style = "width: 200px;"),
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
  
  # Edit a tab modal ----
  
  edit_tab_modal <- shinyjs::hidden(
    div(
      id = ns("edit_tab_modal"),
      div(
        div(
          tags$h1(i18n$t("edit_a_tab")),
          shiny.fluent::IconButton.shinyInput(ns("close_edit_tab_modal"), iconProps = list(iconName = "ChromeClose")),
          class = "create_element_modal_head small_close_button"
        ),
        div(
          div(shiny.fluent::TextField.shinyInput(ns("edit_tab_name"), label = i18n$t("name")), style = "width: 200px;"),
          class = "create_element_modal_body"
        ),
        div(
          div(shiny.fluent::PrimaryButton.shinyInput(ns("delete_tab_button"), i18n$t("delete")), class = "delete_button"),
          shiny.fluent::PrimaryButton.shinyInput(ns("save_tab_button"), i18n$t("save")),
          class = "create_element_modal_buttons"
        ),
        class = "create_tab_modal_content"
      ),
      class = "create_element_modal"
    )
  )
  
  # Delete a tab modal ----
  
  delete_tab_modal <- shinyjs::hidden(
    div(
      id = ns("delete_tab_modal"),
      div(
        tags$h1(i18n$t("delete_tab_title")), tags$p(i18n$t("delete_tab_text")),
        div(
          shiny.fluent::DefaultButton.shinyInput(ns("close_tab_deletion_modal"), i18n$t("dont_delete")),
          div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_tab_deletion"), i18n$t("delete")), class = "delete_button"),
          class = "delete_modal_buttons"
        ),
        class = "delete_modal_content"
      ),
      class = "delete_modal"
    )
  )
  
  # Add a widget modal ----
  
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
          div(
            div(shiny.fluent::TextField.shinyInput(ns("widget_creation_name"), label = i18n$t("name")), style = "width: 320px; height: 70px; margin-left: 10px;"),
            div(
              uiOutput(ns("selected_plugin"), style = "height: 100%;"),
              onclick = paste0("Shiny.setInputValue('", id, "-open_select_a_plugin_modal', Math.random());")
            )
          ),
          div(
            class = "selected_concepts_widget",
            uiOutput(ns("selected_concepts"), class = "selected_concepts_ui"),
            onclick = paste0("Shiny.setInputValue('", id, "-open_select_concepts_modal', Math.random());"),
            style = "display: inherit; margin: 10px 0; overflow: auto;"
          ),
          class = "create_element_modal_body",
          style = "display: flex; gap: 10px; padding-right: 10px; height: calc(100% - 70px);"
        ),
        div(
          shiny.fluent::PrimaryButton.shinyInput(ns("widget_creation_save"), i18n$t("add")),
          style = "display: flex; justify-content: flex-end; margin-right: 10px;"
        ),
        class = "create_widget_modal_content"
      ),
      class = "create_element_modal"
    )
  )
  
  # Delete a widget modal ----
  
  delete_wigdet_modal <- shinyjs::hidden(
    div(
      id = ns("delete_widget_modal"),
      div(
        tags$h1(i18n$t("delete_widget_title")), tags$p(i18n$t("delete_widget_text")),
        div(
          shiny.fluent::DefaultButton.shinyInput(ns("close_widget_deletion_modal"), i18n$t("dont_delete")),
          div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_widget_deletion"), i18n$t("delete")), class = "delete_button"),
          class = "delete_modal_buttons"
        ),
        class = "delete_modal_content"
      ),
      class = "delete_modal"
    )
  )
  
  # Select a plugin modal ----
  
  select_a_plugin_modal <- shinyjs::hidden(
    div(
      id = ns("select_a_plugin_modal"),
      div(
        div(
          tags$h1(i18n$t("select_a_plugin")),
          shiny.fluent::IconButton.shinyInput(ns("close_select_a_plugin_modal"), iconProps = list(iconName = "ChromeClose")),
          class = "select_a_plugin_modal_head small_close_button"
        ),
        div(shiny.fluent::SearchBox.shinyInput(ns("search_plugin")), style = "width:320px; margin:10px 0 0 10px;"),
        uiOutput(ns("plugins_widgets")),
        class = "select_a_plugin_modal_content"
      ),
      class = "select_a_plugin_modal"
    )
  )
  
  # Select concepts modal ----
  
  select_concepts_modal <- mod_select_concepts_ui(id, language, languages, i18n)
  
  # Study divs ----
  
  study_divs <- tagList()
  for (category in c("patient_lvl", "aggregated")) study_divs <- tagList(
    study_divs, 
    shinyjs::hidden(uiOutput(ns(paste0(category, "_study_menu")))),
    shinyjs::hidden(div(
      id = ns(paste0(category, "_no_tabs_to_display")), shiny.fluent::MessageBar(i18n$t("no_tabs_to_display_click_add_tab"), messageBarType = 5),
      style = "display: inline-block; margin-top: 10px;"
    )),
  )
  
  div(
    class = "main", style = "margin-left: 10px;",
    add_tab_modal,
    edit_tab_modal,
    delete_tab_modal,
    add_widget_modal,
    delete_wigdet_modal,
    select_a_plugin_modal,
    select_concepts_modal,
    load_status_modal,
    study_divs,
    shinyjs::hidden(shiny.fluent::PrimaryButton.shinyInput(ns("react_activation"))),
    div(id = ns("study_widgets"))
  )
}

#' @noRd 
mod_data_server <- function(id, r, d, m, language, i18n, debug){
  
  # |-------------------------------- -----
  
  if (debug) cat(paste0("\n", now(), " - mod_data - start"))
  
  # Load concepts backend ----
  
  mod_select_concepts_server(id, r, d, m, language, i18n, debug)
  
  # |-------------------------------- -----
  
  # Data module ----
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # |-------------------------------- -----
    
    # --- --- --- --- --
    # Initiate vars ----
    # --- --- --- --- --
    
    if (debug) cat(paste0("\n", now(), " - mod_data - initiate vars"))
    
    categories <- c("patient_lvl", "aggregated")
    
    # Initiate var for already loaded studies, so that a UI element is not loaded twice
    r$data_loaded_studies <- integer()
    
    # Initiate var for list of cards
    r$data_grids <- tibble::tibble(study_id = integer(), grids = character())
    
    # Tabs server code already loaded
    r$widgets_server_code_loaded <- character()
    
    # Project loading status
    # r$project_load_status_displayed <- FALSE
    
    r$patient_lvl_no_tabs_to_display <- TRUE
    r$aggregated_no_tabs_to_display <- TRUE
    
    default_selected_plugin_ui <- div(
      div(i18n$t("select_a_plugin"), class = "default_content_widget"),
      class = "plugin_widget"
    )
    update_selected_concepts_css <- paste0(
      "$('#", id, "-selected_concepts').css('height', '100%');",
      "$('#", id, "-selected_concepts').css('justify-content', 'center');",
      "$('#", id, "-selected_concepts').css('align-items', 'left;');"
    )
    default_selected_concepts_ui <- div(i18n$t("select_concepts"), class = "default_content_widget")
    
    # session max widget_id (prevent a UI bug when we create a widget with the same id of a widget we just deleted)
    r$session_max_widget <- get_last_row(r$db, "widgets")
    
    # Widget selected concepts
    r[[paste0(id, "_selected_concepts")]] <- tibble::tibble(
      concept_id = integer(), concept_name = character(), domain_id = character(), vocabulary_id = character(),
      mapped_to_concept_id = integer(), merge_mapped_concepts = logical()
    )
    
    # --- --- --- --- --- -
    # Change data page ----
    # --- --- --- --- --- -
    
    observeEvent(r$data_page, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer r$data_page"))
      
      displayed_category <- r$data_page
      hidden_category <- categories[categories != displayed_category]
      
      # Show / hide sidenav dropdowns
      if (displayed_category == "patient_lvl") sapply(c("person_dropdown_div", "visit_detail_dropdown_div"), shinyjs::show)
      else sapply(c("person_dropdown_div", "visit_detail_dropdown_div"), shinyjs::hide)
      
      # Show / hide study menu
      shinyjs::hide(paste0(hidden_category, "_study_menu"))
      shinyjs::delay(100, shinyjs::show(paste0(displayed_category, "_study_menu")))
      
      # Show / hide study widgets
      sapply(r$data_grids, shinyjs::hide)
      shinyjs::show(paste0("gridstack_", r[[paste0(displayed_category, "_selected_tab")]]))
      
      # Show / hide "no tabs to display" message
      sapply(categories, function(category) shinyjs::hide(paste0(category, "_no_tabs_to_display")))
      if (r[[paste0(r$data_page, "_no_tabs_to_display")]] & length(m$selected_study) > 0) shinyjs::show(paste0(r$data_page, "_no_tabs_to_display"))
    })
    
    # --- --- --- --- --- --- --
    # A project is selected ----
    # --- --- --- --- --- --- --
    
    visit_detail_tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement", "observation", "note", "note_nlp", "payer_plan_period", "cost")
    person_tables <- c(visit_detail_tables, "specimen", "death", "drug_era", "dose_era", "condition_era")
    subset_tables <- c(person_tables, "person", "observation_period", "visit_occurrence", "visit_detail")
    main_tables <- c(subset_tables, "location", "care_site", "provider")
    
    observeEvent(r$load_project_data_trigger, {
      
      if (debug) cat(paste0("\n", now(), " - mod_data - observer r$load_project_data_trigger"))
      
      # Hide all grids
      sapply(r$data_grids, shinyjs::hide)
      
      # Show selected project div
      shinyjs::show("selected_project_div")
      
      # Show loading status
      # r$project_load_status_displayed <- TRUE
      # r$project_load_status <- list()
      # shinyjs::show("load_status_modal")

      # Display project loading status
      # if (r$project_load_status_displayed) r$project_load_status$init_vars_starttime <- now("%Y-%m-%d %H:%M:%OS3")

      shinyjs::delay(100, {
        
        # Reset data variables
        sapply(subset_tables, function(table) d$data_subset[[table]] <- tibble::tibble())
        sapply(person_tables, function(table) d$data_person[[table]] <- tibble::tibble())
        sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())

        req(!is.na(m$selected_study))

        # Subsets depending on the selected study
        sql <- glue::glue_sql("SELECT * FROM subsets WHERE study_id = {m$selected_study}", .con = m$db)
        m$subsets <- DBI::dbGetQuery(m$db, sql)

        # Reset selected_subset, selected_person & selected_visit_detail
        m$selected_subset <- NA_integer_
        m$selected_person <- NA_integer_
        m$selected_visit_detail <- NA_integer_

        # Select patients belonging to subsets of this study
        sql <- glue::glue_sql("SELECT * FROM subset_persons WHERE subset_id IN ({m$subsets$id*})", .con = m$db)
        m$subsets_persons <- DBI::dbGetQuery(m$db, sql)

        shinyjs::show("study_menu")
        
        # Reset selected plugin and selected concepts
        shinyjs::runjs(update_selected_concepts_css)
        output$selected_plugin <- renderUI(default_selected_plugin_ui)
        output$selected_concepts <- renderUI(default_selected_concepts_ui)

        # Reset selected key
        r$patient_lvl_selected_tab <- NA_integer_
        r$aggregated_selected_tab <- NA_integer_
        
        r$data_load_ui_stage <- "first_time"

        # Load tabs & widgets variables for this study
        
        tab_group_ids <- c(
          r$projects_wide %>% dplyr::filter(id == m$selected_study) %>% dplyr::pull(patient_lvl_tab_group_id),
          r$projects_wide %>% dplyr::filter(id == m$selected_study) %>% dplyr::pull(aggregated_tab_group_id)
        )
        
        r$data_tabs_groups <- tibble::tibble(category = c("patient_lvl", "aggregated"), id = tab_group_ids)
        
        sql <- glue::glue_sql("SELECT * FROM tabs WHERE tab_group_id IN ({tab_group_ids*})", .con = r$db)
        r$data_tabs <- DBI::dbGetQuery(r$db, sql)
        
        sql <- glue::glue_sql("SELECT * FROM widgets WHERE tab_id IN ({r$data_tabs$id*})", .con = r$db)
        r$data_widgets <- DBI::dbGetQuery(r$db, sql)
        
        sql <- glue::glue_sql("SELECT * FROM widgets_concepts WHERE widget_id IN ({r$data_widgets$id*})", .con = m$db)
        r$data_widgets_concepts <- DBI::dbGetQuery(m$db, sql)
        
        # Load tabs
        r$data_reload_tabs <- paste0("ui_first_load_", now())

        # Load data
        sql <- glue::glue_sql("SELECT dataset_id FROM studies WHERE id = {m$selected_study}", .con = r$db)
        r$selected_dataset <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(dataset_id)
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-load_dataset', Math.random());"))

        # Load concepts
        load_dataset_concepts(r, d, m)
      })

      # Display project loading status
      # if (r$project_load_status_displayed) r$project_load_status$init_vars_endtime <- now("%Y-%m-%d %H:%M:%OS3")
    })
    
    # Show loading status ----
    observeEvent(r$project_load_status, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer r$project_load_status"))
      
      category <- r$data_page
      
      statuses_ui <- tagList()

      for (status in c("init_vars", "dataset", "menu", "widgets_ui", "widgets_server", "concepts")){

        if (length(r$project_load_status[[paste0(status, "_endtime")]]) > 0) status_ui <-
          paste0(round(as.numeric(as.POSIXct(r$project_load_status[[paste0(status, "_endtime")]]) - as.POSIXct(r$project_load_status[[paste0(status, "_starttime")]])), 3), " s")
        else status_ui <- "..."

        statuses_ui <-
          tagList(
            statuses_ui, br(),
            "- ", strong(paste0("Loading ", status)), " - ",
            status_ui
          )
      }

      output$load_status <- renderUI(div(statuses_ui))

      # if (length(r$project_load_status$dataset_endtime) > 0) shinyjs::delay(500, shinyjs::hide("load_status_modal"))
      shinyjs::delay(500, shinyjs::hide("load_status_modal"))

      # if (r$data_ui_loaded & r$data_server_loaded) shinyjs::hide("load_status_modal")
    })
    
    # --- --- --- --
    # Load data ----
    # --- --- --- --
    
    ## Dataset ----
    observeEvent(input$load_dataset, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer r$load_dataset"))
      
      # Display project loading status
      # if (r$project_load_status_displayed) r$project_load_status$dataset_starttime <- now("%Y-%m-%d %H:%M:%OS3")
      
      shinyjs::delay(100, {
        dataset_id <- r$selected_dataset
        load_dataset(r, m, d, dataset_id, main_tables)
      })
      
      # Display project loading status
      # if (r$project_load_status_displayed) r$project_load_status$dataset_endtime <- now("%Y-%m-%d %H:%M:%OS3")
    })
    
    ## Subset ----
    observeEvent(m$selected_subset, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer m$selected_subset"))
      
      req(!is.na(m$selected_subset))
      
      for(table in subset_tables){
        
        # No person_id col in note_nlp table
        if (table != "note_nlp"){
          if (nrow(m$subset_persons) > 0){
            if (d[[table]] %>% dplyr::count() %>% dplyr::pull() > 0){
              person_ids <- m$subset_persons$person_id
              d$data_subset[[table]] <- d[[table]] %>% dplyr::filter(person_id %in% person_ids)
            }
            else d$data_subset[[table]] <- tibble::tibble()
          }
          else d$data_subset[[table]] <- tibble::tibble()
        }
      }
    })
    
    ## Patient ----
    observeEvent(m$selected_person, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer m$selected_person"))
      
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
      if (debug) cat(paste0("\n", now(), " - mod_data - observer m$selected_visit_detail"))
      
      req(!is.na(m$selected_visit_detail))
      
      selected_visit_detail <- m$selected_visit_detail
      
      for(table in visit_detail_tables){
        if (d$data_person[[table]] %>% dplyr::count() %>% dplyr::pull() > 0){
          d$data_visit_detail[[table]] <- d$data_person[[table]] %>% dplyr::filter(visit_detail_id == selected_visit_detail)
        }
      }
    })
    
    # --- --- --- --- --- --
    # Sidenav dropdowns ----
    # --- --- --- --- --- --
    
      ## Subset ----
      
      observeEvent(m$subsets, {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer m$subsets"))
        
        # Update subset dropdown
        if (nrow(m$subsets) == 0) shiny.fluent::updateComboBox.shinyInput(session, "subset", options = list(), value = NULL, errorMessage = i18n$t("no_subset_available"))
        if (nrow(m$subsets) > 0) shiny.fluent::updateComboBox.shinyInput(session, "subset", options = convert_tibble_to_list(m$subsets, key_col = "id", text_col = "name"), value = NULL)
        
        # Reset other dropdowns & uiOutput
        sapply(c("person", "visit_detail"), function(name) {
          shiny.fluent::updateComboBox.shinyInput(session, name, options = list(), value = NULL)
          shinyjs::hide(paste0(name, "_div"))
        })
        output$person_info <- renderUI("")
        shinyjs::hide("person_info_div")
      })
    
      observeEvent(input$subset, {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer input$subset"))
        
        req(input$subset$key)
        
        # Prevent multiple changes of m$selected_subset
        # We have to keep multiple observers, cause we use input variable
        if (is.na(m$selected_subset)) m$selected_subset <- input$subset$key
        if (!is.na(m$selected_subset) & m$selected_subset != input$subset$key) m$selected_subset <- input$subset$key
        
        # Reset data var
        if (r$data_page == "patient_lvl"){
          sapply(person_tables, function(table) d$data_person[[table]] <- tibble::tibble())
          sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())
        }

        # Select patients who belong to this subset
        m$subset_persons <- DBI::dbGetQuery(m$db, glue::glue_sql("SELECT * FROM subset_persons WHERE subset_id = {m$selected_subset}", .con = m$db))
        
        # If this subset contains no patient, maybe the code has not been run yet
        if (nrow(m$subset_persons) == 0){
          sql <- glue::glue_sql("SELECT code FROM code WHERE category = 'subset' AND link_id = {m$selected_subset}", .con = m$db)
          
          subset_code <- 
            DBI::dbGetQuery(m$db, sql) %>% 
            dplyr::pull() %>% 
            stringr::str_replace_all("\r", "\n") %>%
            stringr::str_replace_all("%dataset_id%", as.character(r$selected_dataset)) %>%
            stringr::str_replace_all("%subset_id%", as.character(m$selected_subset))
          
          tryCatch(eval(parse(text = subset_code)),
            error = function(e) if (nchar(e[1]) > 0) cat(paste0("\n", now(), " - mod_data - error executing subset code - subset_id = ", m$selected_subset)))

          m$subset_persons <- DBI::dbGetQuery(m$db, glue::glue_sql("SELECT * FROM subset_persons WHERE subset_id = {m$selected_subset}", .con = m$db))
        }

        # Reset selected_person & selected_visit_detail
        m$selected_person <- NA_integer_
        m$selected_visit_detail <- NA_integer_
      })
    
      ## Patient ----
      
      # Update patients dropdown
      observeEvent(m$subset_persons, {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer m$subset_persons"))
        
        persons <- tibble::tibble()
        
        if (nrow(m$subset_persons) > 0 & d$person %>% dplyr::count() %>% dplyr::pull() > 0){
          person_ids <- m$subset_persons$person_id
          
          persons <-
            d$person %>% 
            dplyr::mutate_at("person_id", as.integer) %>%
            dplyr::filter(person_id %in% person_ids) %>% 
            dplyr::collect() %>%
            dplyr::left_join(d$concept %>% dplyr::select(gender_concept_id = concept_id, gender_concept_name = concept_name), by = "gender_concept_id")
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
        # sapply(c("visit_detail", "person_info"), function(name) shinyjs::hide(paste0(name, "_div")))
        # shinyjs::show("person_div")
      })
    
      # When a patient is searched
      observeEvent(input$person_trigger, {
        
        if (debug) cat(paste0("\n", now(), " - mod_data - observer input$person_trigger"))
        
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
            dplyr::left_join(d$concept %>% dplyr::select(gender_concept_id = concept_id, gender_concept_name = concept_name), by = "gender_concept_id") %>%
            dplyr::mutate(name_display = paste0(person_id, " - ", gender_concept_name))
          
          shiny.fluent::updateComboBox.shinyInput(session, "person", options = convert_tibble_to_list(filtered_person, key_col = "person_id", text_col = "name_display"))
        } else {
          shiny.fluent::updateComboBox.shinyInput(session, "person", 
            options = convert_tibble_to_list(data = d$person %>% 
              dplyr::arrange(person_id) %>%
              dplyr::collect() %>%
              dplyr::slice_head(n = 100) %>% 
              dplyr::left_join(d$concept %>% dplyr::select(gender_concept_id = concept_id, gender_concept_name = concept_name), by = "gender_concept_id") %>%
              dplyr::mutate(name_display = paste0(person_id, " - ", gender_concept_name)),
            key_col = "person_id", text_col = "name_display"), value = NULL)
        }
      })
      
      # When a patient is selected
      observeEvent(input$person, {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer input$person"))
        
        # Check if the entry exists
        
        req(input$person$text)
        
        if (length(input$person$key) == 0){
          person_text <- input$person$text
          person <- d$person %>%
            dplyr::left_join(d$concept %>% dplyr::select(gender_concept_id = concept_id, gender_concept_name = concept_name), by = "gender_concept_id") %>%
            dplyr::mutate(name_display = paste0(person_id, " - ", gender_concept_name)) %>%
            dplyr::filter(name_display == person_text) %>% dplyr::collect()
        }
        if (length(input$person$key) > 0){
          person_key <- input$person$key
          person <- 
            d$person %>%
            dplyr::mutate_at("person_id", as.integer) %>%
            dplyr::filter(person_id == person_key) %>% dplyr::collect() %>%
            dplyr::left_join(d$concept %>% dplyr::select(gender_concept_id = concept_id, gender_concept_name = concept_name), by = "gender_concept_id") %>%
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
          
          visit_detail <- d$visit_detail %>% dplyr::mutate_at("person_id", as.integer) %>% dplyr::filter(person_id == !!person_id)
          
          if ("parent_visit_detail_id" %in% colnames(visit_detail)) visit_detail <- visit_detail %>% dplyr::filter(is.na(parent_visit_detail_id))
          else if ("visit_detail_parent_id" %in% colnames(visit_detail)) visit_detail <- visit_detail %>% dplyr::filter(is.na(visit_detail_parent_id))
          
          visit_detail <- visit_detail %>% 
            dplyr::collect() %>% 
            dplyr::left_join(
              d$concept %>% dplyr::select(visit_detail_concept_id = concept_id, visit_detail_concept_name = concept_name),
              by = "visit_detail_concept_id"
            ) %>%
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
      
      ## Stay ----
      
      observeEvent(input$visit_detail, {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer input$visit_detail"))
        
        m$selected_visit_detail <- input$visit_detail$key
        
        # Update person informations on sidenav
        
        style <- "display:inline-block; width:80px; font-weight:bold;"
        
        person_id <- m$selected_person
        person <- 
          d$person %>%
          dplyr::mutate_at("person_id", as.integer) %>%
          dplyr::filter(person_id == !!person_id) %>% 
          dplyr::collect() %>%
          dplyr::left_join(d$concept %>% dplyr::select(gender_concept_id = concept_id, gender_concept_name = concept_name), by = "gender_concept_id")
        
        visit_detail_id <- input$visit_detail$key
        visit_detail <- 
          d$visit_detail %>% 
          dplyr::mutate_at("visit_detail_id", as.integer) %>%
          dplyr::filter(visit_detail_id == !!visit_detail_id) %>% 
          dplyr::collect() %>%
          dplyr::left_join(d$concept %>% dplyr::select(visit_detail_concept_id = concept_id, visit_detail_concept_name = concept_name), by = "visit_detail_concept_id")
        
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
        
        sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())
        
        if (length(m$selected_visit_detail) > 0){
          selected_visit_detail <- m$selected_visit_detail
          for(table in visit_detail_tables) if (d$data_person[[table]] %>% dplyr::count() %>% dplyr::pull() > 0) d$data_visit_detail[[table]] <- 
              d$data_person[[table]] %>% dplyr::filter(visit_detail_id == selected_visit_detail)
        }
      })
      
      # |-------------------------------- -----
      
      # --- --- -- -
      # Load UI ----
      # --- --- -- -
      
      # --- --- --- --- --
      ## Prepare vars ----
      # --- --- --- --- --
      
      observeEvent(r$data_reload_tabs, {
        
        if (debug) cat(paste0("\n", now(), " - mod_data - observer r$..reload_tabs"))
        
        category <- r$data_page

        # Tabs without parent are set to level 1
        tabs <- r$data_tabs %>% dplyr::mutate(level = dplyr::case_when(is.na(parent_tab_id) ~ 1L, TRUE ~ NA_integer_))

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

          r$data_first_tab_displayed <- first_tab_displayed
        }

        r$data_menu_tabs <-
          tabs %>%
          dplyr::group_by(level, parent_tab_id) %>%
          dplyr::summarize(
            id = id, category = category, tab_group_id = tab_group_id, tab_sub_group = dplyr::cur_group_id(), parent_tab_id = parent_tab_id,
            name = name, display_order = display_order, level = level
          ) %>%
          dplyr::ungroup()

        # Reload menu
        r$data_reload_menu <- now()
        
        # Load widgets
        if (grepl("ui_first_load", r$data_reload_tabs)) r$data_load_ui_widgets <- now()
      })
      
      # --- --- --- ---
      ## Load menu ----
      # --- --- --- ---
      
      observeEvent(r$data_reload_menu, {
        if (debug) cat(paste0("\n", now(), " - mod_data - r$data_reload_menu"))
        
        # if (r$project_load_status_displayed) r$project_load_status$menu_starttime <- now("%Y-%m-%d %H:%M:%OS3")
        
        sapply(categories, function(category){
          
          tab_group_id <- r$data_tabs_groups %>% dplyr::filter(category == !!category) %>% dplyr::pull(id)
          
          # Create an ID per level / sub_group
          all_tabs <- r$data_menu_tabs %>% dplyr::filter(tab_group_id == !!tab_group_id)
          
          if (nrow(all_tabs) > 0){
            
            r[[paste0(category, "_no_tabs_to_display")]] <- FALSE
            shinyjs::hide(paste0(category, "_no_tabs_to_display"))
            
            study_first_tab_id <- all_tabs %>% dplyr::filter(level == 1) %>% dplyr::slice(1) %>% dplyr::pull(id)
            
            # Load breadcrumb & pivots, one per level / subgroup
            
            breadcrumbs <- tagList()
            pivots <- tagList()
            
            i <- 1L
            
            if (r$data_edit_page_activated) style <- "display: block;" else style <- "display: none;"
            
            for (tab_sub_group in unique(all_tabs$tab_sub_group)){
              
              tabs <- all_tabs %>% dplyr::filter(tab_sub_group == !!tab_sub_group)
              tabs_ui <- tagList()
              
              for (j in 1:nrow(tabs)){
                tab <- tabs[j, ]
                
                pivot_header <- div(
                  tab$name,
                  div(
                    id = ns(paste0("edit_tab_", tab$id, "_container")),
                    shiny.fluent::IconButton.shinyInput(ns(paste0("edit_tab_", tab$id)), iconProps = list(iconName = "Edit"), onClick = htmlwidgets::JS(paste0(
                      "item => { ",
                        "event.stopPropagation();",
                        "Shiny.setInputValue('", id, "-edit_tab_id', ", tab$id, ", {priority: 'event'});",
                        "Shiny.setInputValue('", id, "-edit_tab_trigger', Math.random(), {priority: 'event'});",
                      "}"
                    ))),
                    class = "edit_tab_button",
                    style = style
                  ),
                  style = "display: flex;"
                )
                
                if (i == 1 & j == 1) if (is.na(r[[paste0(category, "_selected_tab")]])) r[[paste0(category, "_selected_tab")]] <- tab$id
                
                if (tab$id == r[[paste0(category, "_selected_tab")]]) class <- "pivot_item selected_pivot_item"
                else class <- "pivot_item"
                
                tabs_ui <- tagList(
                  tabs_ui, 
                  tags$button(
                    id = ns(paste0("tab_", tab$id)), pivot_header, class = class,
                    onclick = paste0("
                      Shiny.setInputValue('", id, "-", category, "_study_current_tab', this.id);
                      Shiny.setInputValue('", id, "-", category, "_study_current_tab_trigger', Math.random());"
                    )
                  )
                )
              }
              
              pivot_id <- paste0("study_pivot_", tab_group_id, "_", tab_sub_group)
              pivot_sub_div_id <- paste0("study_pivot_sub_div_", tab_group_id, "_", tab_sub_group)
              
              pivot <- 
                div(
                  id = ns(pivot_id),
                  div(
                    id = ns(pivot_sub_div_id),
                    tabs_ui,
                    class = "pivot"
                  ),
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
              
              first_list_element <- list(key = "main", text = shiny.fluent::FontIcon(iconName = "Home"), href = paste0("#!/data?type=", category), isCurrentItem = FALSE,
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
              if (!is.na(r[[paste0(category, "_selected_tab")]]) & r[[paste0(category, "_selected_tab")]] %not_in% tabs$id) breadcrumb <- shinyjs::hidden(breadcrumb)
              
              breadcrumbs <- tagList(breadcrumbs, breadcrumb)
              
              i <- 2L
            }
            
            study_menu_ui <- tagList(div(breadcrumbs), div(pivots))
          }
          
          if (nrow(all_tabs) == 0){
            
            study_menu_ui <- div(
              id = ns(paste0(category, "_study_breadcrumb_", tab_group_id, "_0")),
              shiny.fluent::Breadcrumb(items = list(list(key = "main", text = shiny.fluent::FontIcon(iconName = "Home"), href = paste0("#!/data?type=", category), isCurrentItem = FALSE,
                onClick = htmlwidgets::JS(paste0("item => {",
                  "Shiny.setInputValue('", id, "-study_go_to_tab', 0);",
                  "Shiny.setInputValue('", id, "-study_go_to_tab_trigger', Math.random());",
                  "}"
                )))), maxDisplayedItems = 3)
            )
            
            r[[paste0(category, "_no_tabs_to_display")]] <- TRUE
            if (r$data_page == category) shinyjs::show(paste0(category, "_no_tabs_to_display"))
          }
          
          output[[paste0(category, "_study_menu")]] <- renderUI({
            div(
              study_menu_ui,
              style = "display:flex; justify-content:space-between; margin:5px 13px 0px 0px;"
            )
          })
          
          displayed_category <- r$data_page
          hidden_category <- categories[categories != displayed_category]
          
          shinyjs::show(paste0(displayed_category, "_study_menu"))
          shinyjs::hide(paste0(hidden_category, "_study_menu"))
        })
        
        # if (r$project_load_status_displayed) r$project_load_status$menu_endtime <- now("%Y-%m-%d %H:%M:%OS3")
      })
      
      # --- --- --- --- --
      ## Load widgets ----
      # --- --- --- --- --
      
      observeEvent(r$data_load_ui_widgets, {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer r$data_load_ui_widgets"))
        
        # Don't reload study UI if already loaded
        req(m$selected_study %not_in% r$data_loaded_studies)
        
        sapply(categories, function(category){
          
          distinct_tabs <- r$data_menu_tabs %>% dplyr::filter(category == !!category) %>% dplyr::pull(id)
          
          code_ui <- tagList()
          
          all_groups <- NA_integer_
          
          # Loop over distinct tabs, for this study
          # Load front-end & back-end
          sapply(distinct_tabs, function(tab_id){
            load_tab_plugins(tab_id)
            load_tab_ui(category, tab_id, action = "load_tabs")
            load_tab_server(tab_id)
          })
          
          # displayed_category <- r$data_page
          # hidden_category <- categories[categories != displayed_category]
        })
        
        # Indicate that this study has been loaded, so that UI elements aren't loaded twice
        r$data_loaded_studies <- c(r$data_loaded_studies, m$selected_study)
      })
      
    # |-------------------------------- -----
    
    # --- --- --- --- --- --
    # A tab is selected ####
    # --- --- --- --- --- ---
    
    sapply(categories, function(category){
      
      # A tab is selected
      observeEvent(r[[paste0(category, "_selected_tab")]], {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer r$..selected_tab"))
        
        # Hide all grids
        sapply(r$data_grids, shinyjs::hide)
        
        # Display grid of this tab
        shinyjs::show(paste0("gridstack_", r[[paste0(category, "_selected_tab")]]))
        
        # Reload responsive
        gridstack_id <- paste0("gridstack_", r[[paste0(category, "_selected_tab")]])
      })
    
    # Tab selected from the menu
      observeEvent(input[[paste0(category, "_study_current_tab_trigger")]], {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer input$", category, "_study_current_tab_trigger"))
        
        last_selected_tab <- r[[paste0(category, "_selected_tab")]]
        selected_tab <- gsub("data-tab_", "", input[[paste0(category, "_study_current_tab")]], fixed = FALSE)
        r[[paste0(category, "_selected_tab")]] <- selected_tab
        
        # Remove selected_pivot_item class from last item and set it to current item
        shinyjs::removeClass(class = "selected_pivot_item", selector = paste0("#", id, "-tab_", last_selected_tab))
        shinyjs::addClass(class = "selected_pivot_item", selector = paste0("#", id, "-tab_", selected_tab))
        
        current_tab <- r$data_menu_tabs %>% dplyr::filter(id == selected_tab)
        children_tabs <- r$data_menu_tabs %>% dplyr::filter(parent_tab_id == selected_tab)
        
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
    })
    
    # Tab selected from breadcrumb
    observeEvent(input$study_go_to_tab_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$study_go_to_tab"))
      
      r[[paste0(r$data_page, "_selected_tab")]] <- input$study_go_to_tab
      r$data_reload_menu <- now()
    })
    
    # --- --- --- --
    # Add a tab ----
    # --- --- --- --
    
    # Open modal
    observeEvent(input$add_tab, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$add_tab"))
      shinyjs::show("add_tab_modal")
    })
    
    # Close modal
    observeEvent(input$close_add_tab_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$close_add_tab_modal"))
      shinyjs::hide("add_tab_modal")
    })
    
    # Add a tab
    observeEvent(input$add_tab_button, {
      
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$add_tab_button"))
      
      req(length(input$tab_name) > 0)
      tab_name <- input$tab_name
      
      category <- r$data_page
      
      # Check if name is not empty
      if (is.na(tab_name) | tab_name == "") shiny.fluent::updateTextField.shinyInput(session, "tab_name", errorMessage = i18n$t("provide_valid_name"))
      req(!is.na(tab_name) & tab_name != "")
      
      selected_tab <- r[[paste0(category, "_selected_tab")]]
      tabs <- r$data_tabs %>% dplyr::filter(id == selected_tab)
      
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
          # selected_tab <- r[[paste0(category, "_selected_tab")]]
          widgets <- r$data_widgets %>% dplyr::filter(tab_id == selected_tab, !deleted) %>% dplyr::rename(widget_id = id)
          if (nrow(widgets) > 0) show_message_bar(output, message = "add_tab_has_widgets", i18n = i18n, ns = ns)
          req(nrow(widgets) == 0)
        }
      }
      
      # Check if name is not already used
      if (is.na(parent_tab_id)) sql <- glue::glue_sql("SELECT name FROM tabs WHERE category = {category} AND tab_group_id = {tab_group_id} AND parent_tab_id IS NULL AND LOWER(name) = {tolower(tab_name)}", .con = r$db)
      else sql <- glue::glue_sql("SELECT name FROM tabs WHERE category = {category} AND tab_group_id = {tab_group_id} AND parent_tab_id = {parent_tab_id} AND LOWER(name) = {tolower(tab_name)}", .con = r$db)
      name_already_used <- nrow(DBI::dbGetQuery(r$db, sql) > 0)
      
      if (name_already_used) shiny.fluent::updateTextField.shinyInput(session, "tab_name", errorMessage = i18n$t("name_already_used"))
      req(!name_already_used)
      
      new_id <- get_last_row(r$db, "tabs") + 1
      
      # Add data in database
      new_data <- tibble::tibble(id = new_id, category = category, name = tab_name, description = NA_character_, 
        tab_group_id = tab_group_id, parent_tab_id = parent_tab_id, display_order = display_order, creator_id = r$user_id, datetime = now(), deleted = FALSE)
      DBI::dbAppendTable(r$db, "tabs", new_data)
      
      # Update r var
      r$data_tabs <- r$data_tabs %>% dplyr::bind_rows(new_data)
      
      # Add gridstack instance
      load_tab_ui(category, new_id, action = "add_tab")
      
      # Notify user
      show_message_bar(output, "tab_added", "success", i18n = i18n, ns = ns)
      
      # Reload update_datetime
      sql_update_datetime(r, m)
      
      # Reset fields
      shiny.fluent::updateTextField.shinyInput(session, "tab_name", value = "")
      shiny.fluent::updateChoiceGroup.shinyInput(session, "add_tab_type", value = "same_level")
      
      # Reload UI, with new tab opened
      r[[paste0(category, "_selected_tab")]] <- new_id
      r$data_reload_tabs <- now()
      
      # Reload UI menu and set to added tab
      r$data_reload_menu <- now()
      
      # Hide currently opened grids
      sapply(r$data_grids, shinyjs::hide)
      
      # Hide "no tabs" message
      shinyjs::hide(paste0(category, "_no_tabs_to_display"))
      
      # Hide add tab model
      shinyjs::hide("add_tab_modal")
    })
    
    # --- --- --- - -
    # Edit a tab ----
    # --- --- --- - -
    
    # Open modal
    observeEvent(input$edit_tab_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$edit_tab_trigger"))
      shinyjs::show("edit_tab_modal")
      
      # Get tab ID
      tab_id <- input$edit_tab_id
      
      # Get tab name and update textfield
      tab <- r$data_tabs %>% dplyr::filter(id == tab_id)
      shiny.fluent::updateTextField.shinyInput(session, "edit_tab_name", value = tab$name)
    })
    
    # Close modal
    observeEvent(input$close_edit_tab_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$close_edit_tab_modal"))
      shinyjs::hide("edit_tab_modal")
    })
    
    # Save updates
    observeEvent(input$save_tab_button, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$save_tab_button"))
      
      # Get tab ID
      tab_id <- input$edit_tab_id
      
      # Check if name is not empty
      tab_name <- input$edit_tab_name
      if (is.na(tab_name) | tab_name == "") shiny.fluent::updateTextField.shinyInput(session, "edit_tab_name", errorMessage = i18n$t("provide_valid_name"))
      req(!is.na(tab_name) & tab_name != "")
      
      # Check if name is not already used
      name_already_used <- nrow(r$data_tabs %>% dplyr::filter(category == r$data_page, tolower(name) == tolower(tab_name)))
      
      if (name_already_used) shiny.fluent::updateTextField.shinyInput(session, "edit_tab_name", errorMessage = i18n$t("name_already_used"))
      req(!name_already_used)
      
      # Save updates in db
      sql <- glue::glue_sql("UPDATE tabs SET name = {tab_name} WHERE id = {tab_id}", .con = r$db)
      sql_send_statement(r$db, sql)
      
      # Update r var
      r$data_tabs <- r$data_tabs %>% dplyr::mutate(name = dplyr::case_when(
        id == tab_id ~ tab_name,
        TRUE ~ name
      ))
      
      # Reload study menu
      r$data_reload_tabs <- now()
      
      # Notify user
      show_message_bar(output, message = "modif_saved", type = "success", i18n = i18n, ns = ns)
      
      # Reload update_datetime
      sql_update_datetime(r, m)
      
      # Close modal
      shinyjs::hide("edit_tab_modal")
    })
    
    # --- --- --- --- -
    # Delete a tab ----
    # --- --- --- --- -
    
    observeEvent(input$delete_tab_button, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$delete_tab_button"))
      shinyjs::show("delete_tab_modal")
    })
    
    observeEvent(input$close_tab_deletion_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$close_tab_deletion_modal"))
      shinyjs::hide("delete_tab_modal")
    })
    
    observeEvent(input$confirm_tab_deletion, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$confirm_tab_deletion"))
      
      category <- r$data_page
      
      # Get tab ID
      tab_id <- input$edit_tab_id
      tab <- r$data_menu_tabs %>% dplyr::filter(id == tab_id)
      
      # Delete tab from db
      sql <- glue::glue_sql("DELETE FROM tabs WHERE id = {tab_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      # Delete widgets_options and widgets_concepts
      sql <- glue::glue_sql("SELECT id FROM widgets WHERE tab_id = {tab_id}", .con = r$db)
      widgets_ids <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
      
      sql <- glue::glue_sql("DELETE FROM widgets_options WHERE widget_id IN ({widgets_ids*})", .con = m$db)
      sql_send_statement(m$db, sql)
      sql <- glue::glue_sql("DELETE FROM widgets_concepts WHERE widget_id IN ({widgets_ids*})", .con = m$db)
      sql_send_statement(m$db, sql)
      
      # Delete widgets from db
      sql <- glue::glue_sql("DELETE FROM widgets WHERE tab_id = {tab_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      # Update session max widget_id
      r$session_max_widget <- max(r$session_max_widget, r$data_widgets %>% dplyr::filter(tab_id == !!tab_id) %>% dplyr::pull(id))
      
      # Update r vars
      r$data_tabs <- r$data_tabs %>% dplyr::filter(id != tab_id)
      r$data_widgets <- r$data_widgets %>% dplyr::filter(tab_id != !!tab_id)
      
      # Change selected tab to first tab
      # Choose a tab from the same level, or level up if there's not any tab at the same level
      
      first_tab <- r$data_menu_tabs %>% dplyr::filter(category == r$data_page, level == tab$level)
      
      if (nrow(first_tab) == 0){
        first_tab <- r$data_menu_tabs %>% dplyr::filter(category == r$data_page, level == tab$level - 1)
        if (nrow(first_tab) == 0){
          first_tab <- 0
        }
      }
      
      r[[paste0(category, "_selected_tab")]] <- first_tab %>% dplyr::slice(1) %>% dplyr::pull(id)
      
      # Reload study menu
      r$data_reload_tabs <- now()
      
      # Notify user
      show_message_bar(output, message = "tab_deleted", type = "warning", i18n = i18n, ns = ns)
      
      # Reload update_datetime
      sql_update_datetime(r, m)
      
      # Close modals
      sapply(c("edit_tab_modal", "delete_tab_modal"), shinyjs::hide)
    })
    
    # --- --- --- --- -
    # Add a widget ----
    # --- --- --- --- -
    
    # Open modal
    observeEvent(input$add_widget, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$add_widget"))
      shinyjs::show("add_widget_modal")
      
      # Reload plugins var
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_plugins_var', Math.random());"))
    })
    
    # Close modal
    observeEvent(input$close_add_widget_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$close_add_widget_modal"))
      shinyjs::hide("add_widget_modal")
    })
    
    ## Selected plugin ----
    
    observeEvent(input$open_select_a_plugin_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$open_select_a_plugin_modal"))
      shinyjs::show("select_a_plugin_modal")
    })
    
    observeEvent(input$close_select_a_plugin_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$close_select_a_plugin_modal"))
      shinyjs::hide("select_a_plugin_modal")
    })
    
    observeEvent(input$reload_plugins_var, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$reload_plugins_var"))
      
      reload_elements_var(page_id = id, con = r$db, r = r, m = m, long_var_filtered = "filtered_data_plugins_long")
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_plugins_list', Math.random());"))
    })
    
    observeEvent(input$reload_plugins_list, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$reload_plugins_list"))
      
      elements_ui <- create_elements_ui(page_id = id, elements = r$filtered_data_plugins_long, r = r, language = language, i18n = i18n)
      
      output$plugins_widgets <- renderUI(elements_ui)
    })
    
    # Search a plugin
    
    observeEvent(input$search_plugin, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$search_plugin"))
      
      if (input$search_plugin == "") r$filtered_data_plugins_long <- r$plugins_long
      else {
        
        # Filter on name or description
        
        filtered_ids <- r$plugins_long %>% 
          dplyr::filter(
            (name == paste0("name_", language) & grepl(tolower(input$search_plugin), tolower(value))) |
            (name == paste0("short_description_", language) & grepl(tolower(input$search_plugin), tolower(value)))
          ) %>%
          dplyr::pull(id)
        
        r$filtered_data_plugins_long <- r$plugins_long %>% dplyr::filter(id %in% filtered_ids)
      }
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_plugins_list', Math.random());"))
    })
    
    # A plugin is selected
    
    observeEvent(input$selected_element_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$selected_element_trigger"))
      
      row <- r$filtered_data_plugins_long %>% dplyr::filter(id == input$selected_element)
      plugin_type <- row %>% dplyr::slice(1) %>% dplyr::pull(tab_type_id)
      plugin_name <- row %>% dplyr::filter(name == paste0("name_", language)) %>% dplyr::pull(value)
      short_description <- row %>% dplyr::filter(name == paste0("short_description_", language)) %>% dplyr::pull(value)
      
      users_ui <- create_authors_ui(row %>% dplyr::filter(name == "author") %>% dplyr::pull(value))
      plugin_buttons <- ""
      element_ui <- create_element_ui(id, "plugin", plugin_name, users_ui, plugin_buttons, "", short_description)
      
      output$selected_plugin <- renderUI(element_ui)
      
      shinyjs::delay(50, shinyjs::hide("select_a_plugin_modal"))
    })
    
    ## Confirm creation of widget ----
    
    observeEvent(input$widget_creation_save, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$widget_creation_save"))
      
      category <- r$data_page
      
      ### Add widget in db ----
      
      req(length(input$widget_creation_name) > 0)
      widget_name <- input$widget_creation_name
      
      # Check if name is not empty
      if (is.na(widget_name) | widget_name == "") shiny.fluent::updateTextField.shinyInput(session, "widget_creation_name", errorMessage = i18n$t("provide_valid_name"))
      req(!is.na(widget_name) & widget_name != "")
      
      # Check if a plugin is selected
      if (length(input$selected_element) == 0) output$selected_plugin <- renderUI(
        div(
          div(i18n$t("select_a_plugin"), class = "default_content_widget", style = "font-weight: 600; color: #B83137"),
          class = "plugin_widget"
        )
      )
      req(length(input$selected_element) > 0)
      
      plugin_id <- input$selected_element
      selected_tab <- r[[paste0(category, "_selected_tab")]]
      tab_id <- r[[paste0(category, "_selected_tab")]]
      
      sql <- glue::glue_sql("SELECT value FROM options WHERE category = 'plugin' AND name = 'unique_id' AND link_id = {plugin_id}", .con = r$db)
      plugin_unique_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
      
      plugin_folder <- paste0(r$app_folder, "/plugins/", category, "/", plugin_unique_id)
      
      # Check if name is not already used
      sql <- glue::glue_sql("SELECT name FROM widgets WHERE tab_id = {tab_id} AND LOWER(name) = {tolower(widget_name)}", .con = r$db)
      name_already_used <- nrow(DBI::dbGetQuery(r$db, sql) > 0)
      
      if (name_already_used) shiny.fluent::updateTextField.shinyInput(session, "widget_creation_name", errorMessage = i18n$t("name_already_used"))
      req(!name_already_used)
      
      # Add widget in db
      widget_id <- get_last_row(r$db, "widgets") + 1
      if (widget_id <= r$session_max_widget) widget_id <- r$session_max_widget + 1
      
      new_data <- tibble::tibble(
        id = widget_id, name = widget_name, category = category, tab_id = as.integer(tab_id), plugin_id = plugin_id, display_order = NA_integer_,
        creator_id = r$user_id, datetime = now(), deleted = FALSE)
        
      DBI::dbAppendTable(r$db, "widgets", new_data)
      r$data_widgets <- r$data_widgets %>% dplyr::bind_rows(new_data)

      # Add widget concepts in db
      last_row_widgets_concepts <- get_last_row(m$db, "widgets_concepts")

      # Vocabulary concepts for server code
      selected_concepts <- r[[paste0(id, "_selected_concepts")]]
      
      if (nrow(r[[paste0(id, "_selected_concepts")]]) > 0){

        new_data <-
          r[[paste0(id, "_selected_concepts")]] %>%
          dplyr::transmute(
            id = 1:dplyr::n() + last_row_widgets_concepts + 1, widget_id = !!widget_id,
            concept_id, concept_name, concept_display_name = "", domain_id, mapped_to_concept_id, merge_mapped_concepts,
            creator_id = r$user_id, datetime = now(), deleted = FALSE
          )

        DBI::dbAppendTable(m$db, "widgets_concepts", new_data)
        r$data_widgets_concepts <- r$data_widgets_concepts %>% dplyr::bind_rows(new_data)
        
        # Reset selected concepts
        r[[paste0(id, "_selected_concepts")]] <- tibble::tibble(
          concept_id = integer(), concept_name = character(), domain_id = character(), vocabulary_id = character(),
          mapped_to_concept_id = integer(), merge_mapped_concepts = logical()
        )
      }
      
      # Notify user
      show_message_bar(output, message = "widget_added", type = "success", i18n = i18n, ns = ns)
      
      # Reload update_datetime
      sql_update_datetime(r, m)
      
      # Reset fields
      shiny.fluent::updateTextField.shinyInput(session, "widget_creation_name", value = "")
      
      shiny.fluent::updateSearchBox.shinyInput(session, "search_plugin", value = "")
      
      # Reset selected plugin and selected concepts
      shinyjs::runjs(update_selected_concepts_css)
      output$selected_concepts <- renderUI(default_selected_concepts_ui)
      
      output$selected_plugin <- renderUI(default_selected_plugin_ui)
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-selected_element', null);"))
      
      output$selected_concepts <- renderUI(default_selected_concepts_ui)
      output$selected_concepts_list <- renderUI("")
      
      ## Load front-end & back-end ----
      
      load_tab_plugins(tab_id, widget_id, "add")
      load_tab_ui(category, tab_id, widget_id, action = "add_widget")
      load_tab_server(tab_id, widget_id, "add")
      
      # Close modal
      shinyjs::hide("add_widget_modal")
      
      # Reload menu (issue : it changed selected tab)
      r$data_reload_menu <- now()
    })
    
    # --- --- --- --- --
    # Edit a widget ----
    # --- --- --- --- --
    
    # ...
    
    # --- --- --- --- -- -
    # Delete a widget ----
    # --- --- --- --- -- -
    
    observeEvent(input$close_widget_deletion_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$close_file_deletion_modal"))
      shinyjs::hide("delete_widget_modal")
    })
    
    observeEvent(input$confirm_widget_deletion, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$confirm_widget_deletion"))
      
      category <- r$data_page
      
      tab_id <- r[[paste0(category, "_selected_tab")]]
      widget_id <- r$data_selected_widget
      
      # Update session max widget_id
      r$session_max_widget <- max(r$session_max_widget, widget_id)
      
      # Delete from database
      sql <- glue::glue_sql("DELETE FROM widgets WHERE id = {widget_id}", .con = r$db)
      sql_send_statement(r$db, sql)
      
      # Delete widgets_options and widgets_concepts
      sql <- glue::glue_sql("DELETE FROM widgets_options WHERE widget_id = {widget_id}", .con = m$db)
      sql_send_statement(m$db, sql)
      sql <- glue::glue_sql("DELETE FROM widgets_concepts WHERE widget_id = {widget_id}", .con = m$db)
      sql_send_statement(m$db, sql)
      
      # Update r var
      r$data_widgets <- r$data_widgets %>% dplyr::filter(id != widget_id)
      
      # Remove widget from gridstacks
      shinyjs::runjs(paste0("
        var grid = window.gridStackInstances['", tab_id, "'];
        var current_widget = grid.el.querySelector('#", ns(paste0("data_gridstack_item_", widget_id)), "');
        if (current_widget) grid.removeWidget(current_widget);"))
      
      gridstack_id <- paste0("gridstack_", tab_id)
      shinyjs::runjs(paste0(gridstack_id, ".remove_widget($('#", ns(paste0("widget_", widget_id)), "'))"))
      
      # Close modal
      shinyjs::hide("delete_widget_modal")
      
      # Notify user
      show_message_bar(output,  "widget_deleted", "warning", i18n = i18n, ns = ns)
      
      # Reload update_datetime
      sql_update_datetime(r, m)
    })
    
    # --- --- --- --
    # Edit page ----
    # --- --- --- --
    
    r$data_edit_page_activated <- FALSE
    
    observeEvent(input$edit_page_on, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$edit_page_on"))
      
      # Enable gridstack edition
      sapply(gsub("gridstack_", "", r$data_grids, fixed = FALSE), function(tab_id) shinyjs::runjs(paste0("
        const grid = window.gridStackInstances['", tab_id, "'];
        grid.setStatic(false);
      ")))
      
      # Show edit tab buttons
      distinct_tabs <- r$data_menu_tabs %>% dplyr::pull(id)
      sapply(distinct_tabs, function(tab_id) shinyjs::show(paste0("edit_tab_", tab_id, "_container")))
      
      # Show edit and delete widget buttons
      sapply(r$data_widgets$id, function(widget_id) shinyjs::show(paste0("data_widget_settings_buttons_", widget_id)))
      
      # Show quit edit page button
      shinyjs::hide("edit_page_on_div")
      shinyjs::show("edit_page_off_div")
      
      # Hide resize button when sidenav is displayed or not
      r$data_edit_page_activated <- TRUE
    })
    
    observeEvent(input$edit_page_off, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$edit_page_off"))
      
      # Save each widget position
      data_widgets <- r$data_widgets %>% dplyr::filter(tab_id == r[[paste0(r$data_page, "_selected_tab")]])
      
      if (nrow(data_widgets) > 0){
        for (i in 1:nrow(data_widgets)){
          
          data_widget <- data_widgets[i, ]
          
          shinyjs::runjs(paste0(
            "var widget = document.getElementById('", id, "-data_gridstack_item_", data_widget$id, "');",
            "if (widget) {",
            "  var widgetPosition = {",
            "    id: ", data_widget$id, ",",
            "    w: widget.getAttribute('gs-w'),",
            "    h: widget.getAttribute('gs-h'),",
            "    x: widget.getAttribute('gs-x'),",
            "    y: widget.getAttribute('gs-y')",
            "  }",
            "};",
            "Shiny.setInputValue('", id, "-widget_position', widgetPosition);",
            "Shiny.setInputValue('", id, "-widget_position_trigger', Math.random());"
          ))
        }
      }
      
      # Disable gridstack edition
      sapply(gsub("gridstack_", "", r$data_grids, fixed = FALSE), function(tab_id) shinyjs::runjs(paste0("
        const grid = window.gridStackInstances['", tab_id, "'];
        grid.setStatic(true);
      ")))
      
      # Show edit tab buttons
      distinct_tabs <- r$data_menu_tabs %>% dplyr::pull(id)
      sapply(distinct_tabs, function(tab_id) shinyjs::hide(paste0("edit_tab_", tab_id, "_container")))
      
      # Hide edit and delete widget buttons
      sapply(r$data_widgets$id, function(widget_id) shinyjs::hide(paste0("data_widget_settings_buttons_", widget_id)))
      
      # Show edit page button
      shinyjs::hide("edit_page_off_div")
      shinyjs::delay(50, shinyjs::show("edit_page_on_div"))
      
      # Hide resize button when sidenav is displayed or not
      r$data_edit_page_activated <- FALSE
      
      # Prevent a bug with scroll into ace editor
      shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
      
      # Reload update_datetime
      sql_update_datetime(r, m)
    })
    
    # Save each widget position
    observeEvent(input$widget_position_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$widget_position_trigger"))
      
      widget <- input$widget_position
      
      widget_position <- ""
      for (pos in c("w", "h", "x", "y")){
        if (length(widget[[pos]]) == 0) widget_pos <- 1
        else widget_pos <- widget[[pos]]
        widget_position <- paste0(widget_position, pos, "=", widget_pos, ";")
      }
      
      # Check if this data is already registred
      sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id = {widget$id} AND category = 'widget_position'", .con = m$db)
      
      if (nrow(DBI::dbGetQuery(m$db, sql)) > 0){
        sql <- glue::glue_sql("UPDATE widgets_options SET value = {widget_position} WHERE widget_id = {widget$id} AND category = 'widget_position'", .con = m$db)
        sql_send_statement(m$db, sql)
      }
      else {
        new_data <- tibble::tibble(
          id =  get_last_row(m$db, "widgets_options") + 1, widget_id = widget$id, person_id = NA_integer_, link_id = NA_integer_,
          category = "widget_position", name = NA_character_, value = widget_position, value_num = NA_integer_,
          creator_id = r$user_id, datetime = now(), deleted = FALSE
        )
        DBI::dbAppendTable(m$db, "widgets_options", new_data)
      }
    })
    
    # |-------------------------------- -----
    
    # --- --- --- --- --- -
    # Module functions ----
    # --- --- --- --- --- -
    
    create_translations_files <- function(plugin_id, plugin_translations_dir, plugin_folder){
      
      if (!dir.exists(plugin_translations_dir)) dir.create(plugin_translations_dir)
      if (!dir.exists(plugin_folder)) dir.create(plugin_folder)
      
      # Create translations file if doesn't exist, from database
      translations_file <- paste0(plugin_folder, "/translations.csv")
      
      if (!file.exists(translations_file)){
        
        sql <- glue::glue_sql("SELECT id FROM options WHERE category = 'plugin_code' AND link_id = {plugin_id} AND name = 'filename' AND value = 'translations.csv'", .con = r$db)
        options_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
        
        sql <- glue::glue_sql("SELECT code FROM code WHERE category = 'plugin' AND link_id = {options_id}", .con = r$db)
        translations_code <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
        
        writeLines(translations_code, translations_file)
      }
      
      # Get translations file
      data <- vroom::vroom(translations_file, col_types = "ccc", progress = FALSE)
      
      # Create one csv by language
      for(lang in names(data)[-1]){
        # Create a new dataframe with base & current language cols
        data_lang <- data[, c("base", lang)]
        filename <- paste0(plugin_translations_dir, "/translation_", lang, ".csv")
        write.csv(data_lang, filename, row.names = FALSE)
      }
    }
    
    load_tab_plugins <- function(tab_id, widget_id = NA_integer_, action = "reload"){
      
      if (action == "reload") widgets <- r$data_widgets %>% dplyr::filter(tab_id == !!tab_id)
      else if (action == "add") widgets <- r$data_widgets %>% dplyr::filter(id == !!widget_id)
      
      if (nrow(widgets) > 0){
        
        # Load all plugins
        sql <- glue::glue_sql("SELECT DISTINCT(plugin_id) FROM widgets WHERE id IN ({widgets$id*})", .con = r$db)
        plugin_ids <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
        
        # For each plugin, create plugins files if don't exist
        if (length(plugin_ids) > 0) for (plugin_id in plugin_ids) create_plugin_files(id = id, r = r, plugin_id = plugin_id)
      }
    }
    
    load_tab_ui <- function(category, tab_id, widget_id = NA_integer_, action){
      
      # if (r$project_load_status_displayed) r$project_load_status$widgets_ui_starttime <- now("%Y-%m-%d %H:%M:%OS3")
      
      selected_tab <- r[[paste0(category, "_selected_tab")]]

      widgets_ui <- tagList()

      if (action %in% c("add_tab", "load_tabs")) widgets <- r$data_widgets %>% dplyr::filter(tab_id == !!tab_id)
      else if (action %in% "add_widget") widgets <- r$data_widgets %>% dplyr::filter(id == !!widget_id)
      widgets <- widgets %>% dplyr::rename(widget_id = id)

      gridstack_id <- paste0("gridstack_", tab_id)

      # Create a tab gridstack instance ?
      if (action %in% c("load_tabs", "add_tab")){

        # Add gridstack div
        gridstack_div <- div(id = ns(gridstack_id), class = "grid-stack")

        # Hide this gridstack instance ? Not if this is current tab of if this is a new stab
        hide_div <- TRUE
        if (action == "add_tab") hide_div <- FALSE
        else if (!is.na(selected_tab) & category == r$data_page) if (tab_id == selected_tab) hide_div <- FALSE
        
        if (hide_div) gridstack_div <- shinyjs::hidden(gridstack_div)

        insertUI(selector = paste0("#", ns("study_widgets")), where = "beforeEnd", ui = gridstack_div)

        create_gridstack_instance(id, tab_id)

        r$data_grids <- c(r$data_grids, gridstack_id)

        r$data_ui_loaded <- TRUE
      }

      if (nrow(widgets) > 0){

        # Load widgets concepts
        widgets_concepts <- r$data_widgets_concepts %>% dplyr::inner_join(widgets %>% dplyr::select(widget_id), by = "widget_id")

        # Get widgets ids
        widgets_ids <- unique(widgets$widget_id)

        # Use sapply instead of for loop, cause with for loop, widget_id doesn't change
        sapply(widgets_ids, function(widget_id){

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
            ui_code <- div(shiny.fluent::MessageBar(i18n$t("plugin_deleted"), messageBarType = 5), style = "display: inline-block; margin-top:10px;")
            settings_widget_button <- ""
          }
          else {

            # Get plugin unique_id
            sql <- glue::glue_sql("SELECT value FROM options WHERE category = 'plugin' AND name = 'unique_id' AND link_id = {plugin_id}", .con = r$db)
            plugin_unique_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()

            # Get plugin folder
            plugin_folder <- paste0(r$app_folder, "/plugins/", plugin_unique_id)

            # Create translations files and var
            plugin_translations_dir <- paste0(r$app_folder, "/translations/", plugin_unique_id)
            create_translations_files(plugin_id, plugin_translations_dir, plugin_folder)

            tryCatch({
              i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = plugin_translations_dir))
              i18np$set_translation_language(language)},
              error = function(e) cat(paste0("\n", now(), " - mod_data - error creating translator - plugin_id = ", plugin_id)))

            # Get name of widget
            widget_name <- widgets %>% dplyr::filter(widget_id == !!widget_id) %>% dplyr::pull(name)

            # Get UI code from db. Try to run plugin UI code

            sql <- glue::glue_sql("SELECT id FROM options WHERE link_id = {plugin_id} AND name = 'filename' AND value = 'ui.R'", .con = r$db)
            code_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()

            patient_id <- NA_integer_
            if (length(m$selected_person) > 0) patient_id <- m$selected_person

            # Widget card

            ui_code <- tryCatch({
                sql <- glue::glue_sql("SELECT code FROM code WHERE category = 'plugin' AND link_id = {code_id}", .con = r$db)
                ui_code <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull() %>% process_widget_code(tab_id, widget_id, m$selected_study, patient_id, plugin_folder)
                eval(parse(text = ui_code))
              },
              error = function(e){
                r$widget_ui_last_error <- e
                show_message_bar(output,  "error_run_plugin_ui_code", "severeWarning", i18n = i18n, ns = ns)
                cat(paste0("\n", now(), " - mod_data - error loading UI code - widget_id = ", widget_id, " - ", toString(e)))
              }
            )
          }
          
          # Get widget position
          sql <- glue::glue_sql("SELECT value FROM widgets_options WHERE widget_id = {widget_id} AND category = 'widget_position'", .con = m$db)
          widget_position <- DBI::dbGetQuery(m$db, sql) %>% dplyr::pull(value)
          matches <- stringr::str_match(widget_position, "w=(\\d+);h=(\\d+);x=(\\d+);y=(\\d+)")
          widget_pos <- list(w = as.integer(matches[2]), h = as.integer(matches[3]), x = as.integer(matches[4]), y = as.integer(matches[5]))
          
          ui_output <- create_widget(id, widget_id, ui_code, w = widget_pos$w, h = widget_pos$h, x = widget_pos$x, y = widget_pos$y)
          
          add_widget_to_gridstack(id, tab_id, ui_output, widget_id)
          output[[paste0("ui_", widget_id)]] <- renderUI(ui_code)
          output[[paste0("edit_buttons_", widget_id)]] <- renderUI(get_widget_edit_buttons(id, widget_id, show_edit_buttons = r$data_edit_page_activated))
        })
      }
      
      # if (r$project_load_status_displayed) r$project_load_status$widgets_ui_endtime <- now("%Y-%m-%d %H:%M:%OS3")
    }
    
    load_tab_server <- function(tab_id, widget_id = NA_integer_, action = "reload"){
      
      # if (r$project_load_status_displayed) r$project_load_status$widgets_server_starttime <- now("%Y-%m-%d %H:%M:%OS3")
      
      shinyjs::delay(100, {
        # Get tabs and widgets
        
        if (action == "reload") widgets <- r$data_widgets %>% dplyr::filter(tab_id == !!tab_id)
        else if (action == "add") widgets <- r$data_widgets %>% dplyr::filter(id == !!widget_id)
        widgets <- widgets %>% dplyr::rename(widget_id = id)
        
        req(nrow(widgets) > 0)
        
        widgets_concepts <- r$data_widgets_concepts %>% dplyr::inner_join(widgets %>% dplyr::select(widget_id), by = "widget_id")
          
        widgets_ids <- unique(widgets$widget_id)
        
        # Loop over widgets
        sapply(widgets_ids, function(widget_id){
          
          # Run plugin server code
          # Only if this code has not been already loaded
          trace_code <- paste0(widget_id, "_", m$selected_study)
          
          if (trace_code %not_in% r$widgets_server_code_loaded){
            
            # Add the trace_code to loaded plugins list
            r$widgets_server_code_loaded <- c(r$widgets_server_code_loaded, trace_code)
            
            selected_concepts <- 
              widgets_concepts %>% 
              dplyr::filter(widget_id == !!widget_id) %>%
              dplyr::select(concept_id, concept_name, concept_display_name, domain_id, mapped_to_concept_id, merge_mapped_concepts)
            
            # Get plugin code
            
            ids <- widgets %>% dplyr::filter(widget_id == !!widget_id) %>% dplyr::slice(1) %>% dplyr::select(plugin_id, tab_id)
            plugin_id <- ids$plugin_id
            tab_id <- ids$tab_id
            
            # Check if plugin has been deleted
            check_deleted_plugin <- nrow(DBI::dbGetQuery(r$db, paste0("SELECT * FROM plugins WHERE id = ", plugin_id))) == 0
            if (!check_deleted_plugin){
              
              sql <- glue::glue_sql("SELECT id FROM options WHERE link_id = {plugin_id} AND name = 'filename' AND value = 'server.R'", .con = r$db)
              code_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
              
              sql <- glue::glue_sql("SELECT code FROM code WHERE category = 'plugin' AND link_id = {code_id}", .con = r$db)
              server_code <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
              
              patient_id <- NA_integer_
              if (length(m$selected_person) > 0) patient_id <- m$selected_person
              
              sql <- glue::glue_sql("SELECT value FROM options WHERE category = 'plugin' AND name = 'unique_id' AND link_id = {plugin_id}", .con = r$db)
              plugin_unique_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
              
              tab_type_id <- r$plugins_wide %>% dplyr::filter(id == plugin_id) %>% dplyr::pull(tab_type_id)
              if (tab_type_id == 1) category <- "patient_lvl" else category <- "aggregated"
              
              plugin_folder <- paste0(r$app_folder, "/plugins/", plugin_unique_id)
              
              server_code <- process_widget_code(server_code, tab_id, widget_id, m$selected_study, patient_id, plugin_folder)
              
            }
            else server_code <- ""
            
            # Create translations var (translations files are created in UI)
            if (!check_deleted_plugin){
              
              # Get plugin unique_id
              sql <- glue::glue_sql("SELECT value FROM options WHERE category = 'plugin' AND name = 'unique_id' AND link_id = {plugin_id}", .con = r$db)
              plugin_unique_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
              
              translations_dir <- paste0(r$app_folder, "/translations/", plugin_unique_id)
              
              tryCatch({
                i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = translations_dir))
                i18np$set_translation_language(language)},
                error = function(e) cat(paste0("\n", now(), " - mod_data - error creating translator - plugin_id = ", plugin_id)))
            }
            
            # Create a session number, to inactivate older observers
            # Reset all older observers for this widget_id
            
            session_code <- paste0("widget_", widget_id)
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
            tryCatch({
                eval(parse(text = server_code), envir = new_env)
                r$project_load_status$widgets_server_endtime <- now("%Y-%m-%d %H:%M:%OS3")
              },
              error = function(e){
                r$widget_server_last_error <- e
                show_message_bar(output,  "error_run_plugin_server_code", "severeWarning", i18n = i18n, ns = ns)
                cat(paste0("\n", now(), " - mod_data - error loading server code - widget_id = ", widget_id, " - ", toString(e)))
              })
            
            # Observer for widget deletion
            observeEvent(input[[paste0("data_widget_remove_", widget_id)]], {
              if (debug) cat(paste0("\n", now(), " - mod_data - observer input$..remove_widget.."))
              r$data_selected_widget <- widget_id
              shinyjs::show("delete_widget_modal")
            })
            
            # Observer for widget settings
            observeEvent(input[[paste0("data_widget_settings_", widget_id)]], {
              if (debug) cat(paste0("\n", now(), " - mod_data - observer input$..widget_settings.."))
              r$data_widget_settings_trigger <- now()
              r$data_widget_settings <- widget_id
            })
          }
        })
        
        r$data_server_loaded <- TRUE
      })
      
      # if (r$project_load_status_displayed) r$project_load_status$widgets_server_endtime <- now("%Y-%m-%d %H:%M:%OS3")
    }
    
    sql_update_datetime <- function(r, m){
      sql <- glue::glue_sql("UPDATE studies SET update_datetime = {now()} WHERE id = {m$selected_study}", .con = r$db)
      sql_send_statement(r$db, sql)
    }
  })
}
