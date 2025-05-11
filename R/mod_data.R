#' @noRd
mod_data_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  result <- ""
  language <- "EN"
  
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
            div(shiny.fluent::TextField.shinyInput(ns("widget_creation_name"), label = i18n$t("name")), style = "width: 280px; height: 80px;"),
            div(
              uiOutput(ns("add_widget_selected_plugin"), style = "margin-top: 20px;"),
              onclick = paste0("Shiny.setInputValue('", id, "-open_select_a_plugin_modal', Math.random());")
            )
          ),
          div(
            class = "selected_concepts_widget",
            uiOutput(ns("add_widget_selected_concepts"), class = "selected_concepts_ui"),
            onclick = paste0("Shiny.setInputValue('", id, "-open_select_concepts_modal', Math.random());"),
            style = "display: inherit; margin-top: 20px; overflow: auto;"
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
  
  # Edit a widget modal ----
  
  edit_widget_modal <- shinyjs::hidden(
    div(
      id = ns("edit_widget_modal"),
      div(
        div(
          tags$h1(i18n$t("edit_a_widget")),
          shiny.fluent::IconButton.shinyInput(ns("close_edit_widget_modal"), iconProps = list(iconName = "ChromeClose")),
          class = "create_element_modal_head small_close_button"
        ),
        div(
          div(
            div(shiny.fluent::TextField.shinyInput(ns("widget_edition_name"), label = i18n$t("name")), style = "width: 280px; height: 80px;"),
            div(
              uiOutput(ns("edit_widget_selected_plugin"), style = "margin-top: 20px;"),
              onclick = paste0("Shiny.setInputValue('", id, "-open_select_a_plugin_modal', Math.random());")
            )
          ),
          div(
            class = "selected_concepts_widget",
            uiOutput(ns("edit_widget_selected_concepts"), class = "selected_concepts_ui"),
            onclick = paste0("Shiny.setInputValue('", id, "-open_select_concepts_modal', Math.random());"),
            style = "display: inherit; margin-top: 20px; overflow: auto;"
          ),
          class = "create_element_modal_body",
          style = "display: flex; gap: 10px; padding-right: 10px; height: calc(100% - 70px);"
        ),
        div(
          shiny.fluent::PrimaryButton.shinyInput(ns("widget_edition_save"), i18n$t("save")),
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
        div(shiny.fluent::SearchBox.shinyInput(ns("search_plugin")), style = "width:280px;"),
        div(uiOutput(ns("plugins_widgets")), style = "margin-top: 15px;"),
        class = "select_a_plugin_modal_content"
      ),
      class = "select_a_plugin_modal"
    )
  )
  
  # Plugin description modal ----
  
  plugin_description_modal <- shinyjs::hidden(
    div(
      id = ns("plugin_description_modal"),
      div(
        div(
          uiOutput(ns("plugin_description_title")),
          shiny.fluent::IconButton.shinyInput(ns("close_plugin_description_modal"), iconProps = list(iconName = "ChromeClose")),
          class = "plugin_description_modal_head small_close_button"
        ),
        uiOutput(ns("plugin_description")),
        class = "plugin_description_modal_content"
      ),
      class = "plugin_description_modal"
    )
  )
  
  # Select concepts modal ----
  
  select_concepts_modal <- mod_select_concepts_ui(id, language, languages, i18n)
  
  # Study divs ----
  
  study_divs <- tagList()
  for (category in c("patient_lvl", "aggregated")) study_divs <- div(
    study_divs, 
    shinyjs::hidden(uiOutput(ns(paste0(category, "_study_menu")))),
    shinyjs::hidden(div(
      id = ns(paste0(category, "_no_tabs_to_display")), shiny.fluent::MessageBar(i18n$t("no_tabs_to_display_click_add_tab"), messageBarType = 5),
      style = "display: inline-block; margin: 5px 8px;"
    ))
  )
  study_divs <- div(study_divs, style = "min-height: 30px; margin:5px 13px 0px 0px;")
  
  div(
    class = "main",
    add_tab_modal,
    edit_tab_modal,
    delete_tab_modal,
    add_widget_modal,
    edit_widget_modal,
    delete_wigdet_modal,
    select_a_plugin_modal,
    plugin_description_modal,
    select_concepts_modal,
    study_divs,
    shinyjs::hidden(shiny.fluent::PrimaryButton.shinyInput(ns("react_activation"))),
    div(id = ns("study_widgets"))
  )
}

#' @noRd 
mod_data_server <- function(id, r, d, m, language, i18n, debug, user_accesses){
  
  # |-------------------------------- -----
  
  if (debug) cat(paste0("\n", now(), " - mod_data - start"))
  
  # Load concepts backend ----
  
  mod_select_concepts_server(id, r, d, m, language, i18n, debug)
  
  # |-------------------------------- -----
  
  # Data module ----
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    # Current user accesses ----
    
    if ("projects_content_management" %in% user_accesses) shinyjs::show("project_content_management")
    
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
    
    r$patient_lvl_no_tabs_to_display <- TRUE
    r$aggregated_no_tabs_to_display <- TRUE
    
    default_selected_plugin_ui <- div(
      class = "element_widget plugin_widget",
      div(
        class = "element_widget_icon plugin_widget_icon",
        tags$i(class = "fas fa-puzzle-piece")
      ),
      div(
        class = "element_widget_content",
        tags$span(i18n$t("select_a_plugin"), style = "color: #a3a5a6;font-size: 18px;")
      )
    )
    
    update_selected_concepts_css <- list()
    for (action in c("add", "edit")) update_selected_concepts_css[[action]] <- paste0(
      "$('#", id, "-", action, "_widget_selected_concepts').css('height', '100%');",
      "$('#", id, "-", action, "_widget_selected_concepts').css('justify-content', 'center');",
      "$('#", id, "-", action, "_widget_selected_concepts').css('align-items', 'left;');"
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
      divs <- c("person_dropdown_div", "visit_detail_dropdown_div", "person_info_div", "patient_switching_buttons_div")
      if (displayed_category == "patient_lvl") sapply(divs, shinyjs::show)
      else sapply(divs, shinyjs::hide)
      
      divs <- c("subset_date_div", "subset_info_div")
      if (displayed_category == "aggregated") sapply(divs, shinyjs::show)
      else sapply(divs, shinyjs::hide)
      
      # Show / hide study menu
      shinyjs::hide(paste0(hidden_category, "_study_menu"))
      shinyjs::delay(100, shinyjs::show(paste0(displayed_category, "_study_menu")))
      
      # Show / hide study widgets
      sapply(r$data_grids, shinyjs::hide)
      shinyjs::show(paste0("gridstack_", r[[paste0(displayed_category, "_selected_tab")]]))
      
      # Show / hide "no tabs to display" message
      sapply(categories, function(category) shinyjs::hide(paste0(category, "_no_tabs_to_display")))
      if (r[[paste0(r$data_page, "_no_tabs_to_display")]] & length(m$selected_study) > 0) shinyjs::show(paste0(r$data_page, "_no_tabs_to_display"))
      
      # Reload window size (correct bug with some plugins display)
      shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
    })
    
    # --- --- --- --- --- --- --
    # A project is selected ----
    # --- --- --- --- --- --- --
    
    visit_detail_tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement", "observation", "note")
    person_tables <- c(visit_detail_tables, "specimen", "death", "drug_era", "dose_era", "condition_era", "observation_period", "visit_occurrence", "visit_detail", "payer_plan_period")
    subset_tables <- c(person_tables, "person")
    main_tables <- c(subset_tables, "location", "care_site", "provider")
    
    observeEvent(r$load_project_trigger, {
      
      if (debug) cat(paste0("\n", now(), " - mod_data - observer r$load_project_trigger"))
      
      # Hide all grids
      sapply(r$data_grids, shinyjs::hide)
      
      # Show selected project div
      shinyjs::show("selected_project_div")
      
      # Load subsets
      # Already loaded in load_dataset fct
      # reload_elements_var(page_id = "subsets", id = "subsets", con = m$db, r = r, m = m, long_var_filtered = "filtered_subsets_long", user_accesses)

      shinyjs::delay(100, {
        
        # Reset data variables
        sapply(subset_tables, function(table) d$data_subset[[table]] <- tibble::tibble())
        sapply(person_tables, function(table) d$data_person[[table]] <- tibble::tibble())
        sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())

        req(!is.na(m$selected_study))

        # Reset selected_subset, selected_person & selected_visit_detail
        m$selected_subset <- NA_integer_
        m$selected_person <- NA_integer_
        m$selected_visit_detail <- NA_integer_

        shinyjs::show("study_menu")
        shinyjs::hide("patient_switching_buttons")
        
        # Reset selected plugin and selected concepts
        shinyjs::runjs(update_selected_concepts_css$add)
        shinyjs::runjs(update_selected_concepts_css$edit)
        output$add_widget_selected_plugin <- renderUI(default_selected_plugin_ui)
        output$edit_widget_selected_plugin <- renderUI(default_selected_plugin_ui)
        output$add_widget_selected_concepts <- renderUI(default_selected_concepts_ui)
        output$edit_widget_selected_concepts <- renderUI(default_selected_concepts_ui)

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
        r$data_widgets <- DBI::dbGetQuery(r$db, sql) %>% dplyr::mutate(plugin_id = dplyr::if_else(is.na(plugin_id), 0, plugin_id))
        
        sql <- glue::glue_sql("SELECT * FROM widgets_concepts WHERE widget_id IN ({r$data_widgets$id*})", .con = m$db)
        r$data_widgets_concepts <- DBI::dbGetQuery(m$db, sql)
        
        # Load tabs
        r$data_reload_tabs <- paste0("ui_first_load_", now())

        # Load data & concepts
        sql <- glue::glue_sql("SELECT dataset_id FROM studies WHERE id = {m$selected_study}", .con = r$db)
        r$selected_dataset <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(dataset_id)
        
        # Reset d list if no selected dataset
        # Reset dropdowns and hive divs
        if (is.na(r$selected_dataset)){
          
          sapply(main_tables, function(table) d[[table]] <- tibble::tibble())
          
          updateSelectizeInput(
            session, "subset", choices = NULL, server = TRUE,
            options = list(
              placeholder = i18n$t("no_subset_available"),
              onInitialize = I("function() { this.setValue(''); }")
            )
          )
          
          updateSelectizeInput(session, "person", choices = NULL, server = TRUE)
          updateSelectizeInput(session, "visit_detail", choices = NULL, server = TRUE)
          
          output$person_info <- renderUI("")
          output$subset_info <- renderUI("")
          
          shinyjs::hide("patient_switching_buttons")
          m$selected_subset <- NA_integer_
          m$selected_person <- NA_integer_
          m$selected_visit_detail <- NA_integer_
          
          m$subsets <- tibble::tibble()
          m$subset_persons <- tibble::tibble()
          r$dataset_vocabularies <- tibble::tibble()
        }
          
        else {
          ## Load data
          shinyjs::runjs(paste0("Shiny.setInputValue('projects-load_dataset', Math.random());"))
        }
      })
    })
    
    # --- --- --- --
    # Load data ----
    # --- --- --- --
    
    ## Subset ----
    observeEvent(m$selected_subset, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer m$selected_subset"))
      
      req(!is.na(m$selected_subset))
      
      if (nrow(m$subset_persons) == 0) for(table in c("person", "visit_occurrence", "visit_detail")) d$data_subset[[table]] <- tibble::tibble()
      else {
        
        person_ids <- m$subset_persons %>% dplyr::distinct(person_id) %>% dplyr::pull()
        person_ids_only <- m$subset_persons %>% dplyr::filter(is.na(visit_occurrence_id) & is.na(visit_detail_id)) %>% dplyr::distinct(person_id) %>% dplyr::pull()
        visit_occurrence_ids <- m$subset_persons %>% dplyr::filter(!is.na(visit_occurrence_id)) %>% dplyr::distinct(visit_occurrence_id) %>% dplyr::pull()
        visit_occurrence_ids_only <- m$subset_persons %>% dplyr::filter(is.na(visit_detail_id)) %>% dplyr::distinct(visit_occurrence_id) %>% dplyr::pull()
        visit_detail_ids <- m$subset_persons %>% dplyr::filter(!is.na(visit_detail_id)) %>% dplyr::distinct(visit_detail_id) %>% dplyr::pull()
        
        for (table in c("person", "visit_occurrence", "visit_detail")){
          if ("person_id" %in% colnames(d[[table]])){
            if (table == "person") d$data_subset$person <- d$person %>% dplyr::filter(person_id %in% person_ids)
            else if (table == "visit_occurrence") d$data_subset$visit_occurrence <- d$visit_occurrence %>% 
              dplyr::filter(visit_occurrence_id %in% visit_occurrence_ids | person_id %in% person_ids_only)
            else if (table == "visit_detail") d$data_subset$visit_detail <- d$visit_detail %>% 
              dplyr::filter(visit_detail_id %in% visit_detail_ids | visit_occurrence_id %in% visit_occurrence_ids_only | person_id %in% person_ids_only)
          }
          else d$data_subset[[table]] <- tibble::tibble()
          d$data_subset_source[[table]] <- d$data_subset[[table]]
        }
        
        tables <- c(
          "condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement", "observation", "note", "payer_plan_period",
          "specimen", "death", "drug_era", "dose_era", "condition_era", "observation_period"
        )
        
        for(table in tables){
          
          # Filter data on person_id, visit_occurrence_id and visit_detail_id

          if ("person_id" %in% colnames(d[[table]])){
            if (table %in% c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement", "observation", "note")){
              d$data_subset[[table]] <- d[[table]] %>% dplyr::inner_join(d$data_subset$visit_detail %>% dplyr::distinct(visit_detail_id), by = "visit_detail_id")
            }
            else if (table %in% c("specimen", "death", "drug_era", "dose_era", "condition_era", "observation_period", "payer_plan_period")){
              d$data_subset[[table]] <- d[[table]] %>% dplyr::inner_join(d$data_subset$person %>% dplyr::distinct(person_id), by = "person_id")
            }
          }
          else d$data_subset[[table]] <- tibble::tibble()
          
          d$data_subset_source[[table]] <- d$data_subset[[table]]
        }
      }
      
      # Reload subset informations UI
      r$subset_updated_data <- Sys.time()
      
      # Update date sliderInput and DatePicker
      if ("visit_occurrence_id" %in% colnames(d$data_subset$visit_occurrence)){
        dates_range <-
          d$data_subset$visit_occurrence %>%
          dplyr::summarize(
            min_date = min(visit_start_datetime, na.rm = TRUE),
            max_date = max(visit_end_datetime, na.rm = TRUE)
          ) %>%
          dplyr::collect() %>%
          dplyr::mutate_at(c("min_date", "max_date"), as.Date)

        subset_dates_range(c(dates_range$min_date, dates_range$max_date))
        subset_dates(c(dates_range$min_date, dates_range$max_date))
      }
    })
    
    ## Patient ----
    observeEvent(m$selected_person, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer m$selected_person"))
      
      req(!is.na(m$selected_person))
      
      selected_person <- m$selected_person
      
      # for(table in person_tables){
      #   if ("person_id" %in% colnames(d$data_subset[[table]])){
      #     d$data_person[[table]] <- d$data_subset[[table]] %>% dplyr::filter(person_id == selected_person)
      #   }
      #   else d$data_person[[table]] <- tibble::tibble()
      # }
    })
    
    ## Stay ----
    observeEvent(m$selected_visit_detail, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer m$selected_visit_detail"))
      
      req(!is.na(m$selected_visit_detail))
      
      selected_visit_detail <- m$selected_visit_detail
      
      # for(table in visit_detail_tables){
      #   if ("visit_detail_id" %in% colnames(d$data_person[[table]])){
      #     d$data_visit_detail[[table]] <- d$data_person[[table]] %>% dplyr::filter(visit_detail_id == selected_visit_detail)
      #   }
      #   else d$data_visit_detail[[table]] <- tibble::tibble()
      # }
    })
    
    # --- --- --- --- --- --
    # Sidenav dropdowns ----
    # --- --- --- --- --- --
    
      ## Subset ----
      
      observeEvent(m$subsets, {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer m$subsets"))
        
        # Update subset dropdown
        if (nrow(m$subsets) == 0) updateSelectizeInput(
            session, "subset", choices = NULL, server = TRUE,
            options = list(
              placeholder = i18n$t("no_subset_available"),
              onInitialize = I("function() { this.setValue(''); }")
            )
          )
         
        if (nrow(m$subsets) > 0){
          
          # Load a specific subset_id if noticed in loading_options
          if (length(r$loading_options$subset_id) > 0){
            
            subset_id <- r$loading_options$subset_id
            
            if (subset_id %in% m$subsets$id) selected <- subset_id
            else {
              selected <- FALSE
              cat(paste0("\n", now(), " - mod_data - ", subset_id, " is not a valid subset ID"))
            }
            
            r$loading_options$subset_id <- NULL
          }
          else selected <- FALSE
          
          choices <- setNames(m$subsets$id, m$subsets$name)
          updateSelectizeInput(
            session, "subset", choices = choices, server = TRUE, selected = selected,
            options = list(
              placeholder = "",
              onInitialize = I("function() { this.setValue(''); }")
            )
          )
        }
        
        # Reset other dropdowns & uiOutput
        sapply(c("person", "visit_detail"), function(name){
          # shinyjs::hide(paste0(name, "_div"))
          updateSelectizeInput(
            session, name, choices = NULL, server = TRUE,
            options = list(
              placeholder = "",
              onInitialize = I("function() { this.setValue(''); }")
            )
          )
        })
        
        output$person_info <- renderUI("")
        shinyjs::hide("person_info_div")
      })
    
      observeEvent(input$subset, {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer input$subset"))
        
        req(input$subset)
        
        selected_subset <- as.numeric(input$subset)
        
        # Prevent multiple changes of m$selected_subset
        # We have to keep multiple observers, cause we use input variable
        if (is.na(m$selected_subset)) m$selected_subset <- selected_subset
        if (!is.na(m$selected_subset) & m$selected_subset != selected_subset) m$selected_subset <- selected_subset

        # Reset data var
        if (r$data_page == "patient_lvl"){
          sapply(person_tables, function(table) d$data_person[[table]] <- tibble::tibble())
          sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())
        }

        # Select patients who belong to this subset
        m$subset_persons <- DBI::dbGetQuery(m$db, glue::glue_sql("SELECT * FROM subset_persons WHERE subset_id = {selected_subset}", .con = m$db))
        
        # If this subset contains no patient, maybe the code has not been run yet
        if (nrow(m$subset_persons) == 0){
          
          sql <- glue::glue_sql("SELECT value FROM options WHERE category = 'subset' AND name = 'unique_id' AND link_id = {selected_subset}", .con = m$db)
          subset_unique_id <- DBI::dbGetQuery(m$db, sql) %>% dplyr::pull()
          
          file_path <- file.path(r$app_folder, "subsets", subset_unique_id, "main.R")
          subset_code <- readLines(file_path, warn = FALSE) %>% paste(collapse = "\n")

          subset_code <- subset_code %>% stringr::str_replace_all("\r", "\n")

          tryCatch(eval(parse(text = subset_code)),
            error = function(e) if (nchar(e[1]) > 0) cat(paste0("\n", now(), " - mod_data - error executing subset code - subset_id = ", selected_subset)))

          m$subset_persons <- DBI::dbGetQuery(m$db, glue::glue_sql("SELECT * FROM subset_persons WHERE subset_id = {selected_subset}", .con = m$db))
        }

        # Reset selected_person & selected_visit_detail
        m$selected_person <- NA_integer_
        m$selected_visit_detail <- NA_integer_
      })
      
      ## Filter subset dates ----
      
      # Synchronize sliderInput and DatePicker
      
      subset_dates_range <- reactiveVal(c(as.Date("1970-01-01"), Sys.Date()))
      subset_dates <- reactiveVal(c(as.Date("1970-01-01"), Sys.Date()))
      debounced_subset_dates <- reactive(subset_dates()) %>% debounce(100)
      
      observeEvent(input$subset_date_slider, {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer input$subset_date_slider"))
        
        subset_dates(input$subset_date_slider)
      })
      
      observeEvent(input$subset_start_date, {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer input$subset_start_date"))
        
        if (as.Date(input$subset_start_date) != subset_dates()[1]) subset_dates(c(as.Date(input$subset_start_date), subset_dates()[2]))
      })
      
      observeEvent(input$subset_end_date, {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer input$subset_end_date"))
        
        if (as.Date(input$subset_end_date) != subset_dates()[2]) subset_dates(c(subset_dates()[1], as.Date(input$subset_end_date)))
      })
      
      observeEvent(debounced_subset_dates(), {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer subset_dates()"))
        
        req(input$subset_date_slider, debounced_subset_dates(), subset_dates_range())
        
        start_date <- debounced_subset_dates()[1]
        end_date <- debounced_subset_dates()[2]
        min_date <- subset_dates_range()[1]
        max_date <- subset_dates_range()[2]
        min_date_js <- htmlwidgets::JS(sprintf("new Date('%s')", as.character(min_date)))
        max_date_js <- htmlwidgets::JS(sprintf("new Date('%s')", as.character(max_date)))
        
        if (input$subset_date_slider[1] != start_date | input$subset_date_slider[2] != end_date){
          if (start_date >= min_date & end_date <= max_date){
            updateSliderInput(
              session, "subset_date_slider",
              min = min_date, max = max_date,
              value = as.Date(debounced_subset_dates()),
              timeFormat = ifelse(language == "fr", "%d-%m-%Y", "%Y-%m-%d")
            )
          }
        }
        
        shiny.fluent::updateDatePicker.shinyInput(session, "subset_start_date", value = start_date, minDate = min_date_js, maxDate = max_date_js)
        shiny.fluent::updateDatePicker.shinyInput(session, "subset_end_date", value = end_date, minDate = min_date_js, maxDate = max_date_js)
        
        shinyjs::delay(10, shinyjs::runjs("observeSubsetSliderChanges();"))
      })
      
      # Adjust position of subset_date_slider
      
      shinyjs::runjs("
        function adjustSliderLabels() {
          const fromLabel = document.querySelector('#data-subset_date_div .irs-from');
          const toLabel = document.querySelector('#data-subset_date_div .irs-to');
          const singleLabel = document.querySelector('#data-subset_date_div .irs-single');
          
          if (fromLabel) {
            const fromLeft = parseFloat(fromLabel.style.left);
            if (fromLeft < 0) {
              fromLabel.style.left = '0%';
            }
          }
        
          if (toLabel) {
            const toLeft = parseFloat(toLabel.style.left);
            if (toLeft > 72) {
              toLabel.style.left = '72%';
            }
          }
        
          if (singleLabel) {
            const singleLeft = parseFloat(singleLabel.style.left);
            if (singleLeft < 0) {
              singleLabel.style.left = '0%';
            } else if (singleLeft > 32) {
              singleLabel.style.left = '32%';
            }
          }
        }
        
        window.observeSubsetSliderChanges = function() {
          const fromLabel = document.querySelector('#data-subset_date_div .irs-from');
          const toLabel = document.querySelector('#data-subset_date_div .irs-to');
          const singleLabel = document.querySelector('#data-subset_date_div .irs-single');
        
          if (fromLabel && toLabel) {
            const observer = new MutationObserver(adjustSliderLabels);
        
            observer.observe(fromLabel, { attributes: true, attributeFilter: ['style'] });
            observer.observe(toLabel, { attributes: true, attributeFilter: ['style'] });
        
            if (singleLabel) {
              observer.observe(singleLabel, { attributes: true, attributeFilter: ['style'] });
            }
          
            adjustSliderLabels();
          }
        }
        
        observeSubsetSliderChanges();
      ")
      
      # Filter d$data_subset with selected date range
      
      observeEvent(input$apply_subset_date_filters, {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer input$apply_subset_date_filters"))
        
        req(!is.na(m$selected_subset))
        
        start_date <- subset_dates()[1]
        end_date <- subset_dates()[2]
        
        if (nrow(m$subset_persons) == 0) for(table in subset_tables) d$data_subset[[table]] <- tibble::tibble()
        
        else {
          
          if ("visit_occurrence_id" %in% colnames(d$data_subset_source$visit_occurrence)) d$data_subset$visit_occurrence <-
              d$data_subset_source$visit_occurrence %>% dplyr::filter(visit_start_date >= start_date & visit_start_date <= end_date)
          if ("visit_detail_id" %in% colnames(d$data_subset_source$visit_detail)) d$data_subset$visit_detail <-
              d$data_subset_source$visit_detail %>% dplyr::inner_join(d$data_subset$visit_occurrence %>% dplyr::distinct(visit_occurrence_id), by = "visit_occurrence_id")
          if ("person_id" %in% colnames(d$data_subset_source$person)) d$data_subset$person <-
              d$data_subset_source$person %>% dplyr::inner_join(d$data_subset$visit_occurrence %>% dplyr::distinct(person_id), by = "person_id")
          
          tables <- c(
            "condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement", "observation", "note", "payer_plan_period",
            "specimen", "death", "drug_era", "dose_era", "condition_era", "observation_period"
          )
          
          for(table in tables){
            
            # Filter data on person_id, visit_occurrence_id and visit_detail_id
            
            if ("person_id" %in% colnames(d$data_subset_source[[table]])){
              if (table %in% c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement", "observation", "note")){
                d$data_subset[[table]] <- d$data_subset_source[[table]] %>% dplyr::inner_join(d$data_subset$visit_detail %>% dplyr::distinct(visit_detail_id), by = "visit_detail_id")
              }
              else if (table %in% c("specimen", "death", "drug_era", "dose_era", "condition_era", "observation_period", "payer_plan_period")){
                d$data_subset[[table]] <- d$data_subset_source[[table]] %>% dplyr::inner_join(d$data_subset$person %>% dplyr::distinct(person_id), by = "person_id")
              }
            }
            else d$data_subset[[table]] <- tibble::tibble()
          }
        }
        
        # Reload subset informations UI
        r$subset_updated_data <- Sys.time()
      })
      
      ## Subset informations
      
      observeEvent(r$subset_updated_data, {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer r$subset_updated_data"))
        
        style <- "display:inline-block; width:100px; font-weight:bold;"
        output$subset_info <- renderUI(
          tagList(
            span(i18n$t("start_date"), style = style), format_datetime(isolate(subset_dates()[1]), language = language, type = "date"), br(),
            span(i18n$t("end_date"), style = style), format_datetime(isolate(subset_dates()[2]), language = language, type = "date"), br(), br(),
            span(i18n$t("patients"), style = style), d$data_subset$person %>% dplyr::count() %>% dplyr::pull(), br(),
            span(i18n$t("stays"), style = style), d$data_subset$visit_occurrence %>% dplyr::count() %>% dplyr::pull()
          )
        )
      })
    
      ## Patient ----
      
      # Update patients dropdown
      observeEvent(m$subset_persons, {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer m$subset_persons"))
        
        # Hide patients switching buttons
        shinyjs::hide("patient_switching_buttons")
        
        persons <- tibble::tibble()
        
        if (nrow(m$subset_persons) > 0 & "person_id" %in% colnames(d$person)){
          person_ids <- m$subset_persons$person_id
          
          persons <-
            d$data_subset$person %>%
            dplyr::collect() %>%
            dplyr::arrange(person_id) %>%
            dplyr::mutate(n = dplyr::row_number()) %>%
            dplyr::left_join(d$dataset_concept %>% dplyr::select(gender_concept_id = concept_id, gender_concept_name = concept_name), by = "gender_concept_id")
          
          r$subset_merged_patients <- persons
       
          num_patients <- nrow(r$subset_merged_patients)
          
          if (num_patients == 0){
            # Set selected_person to NA, not to display a chart when no person is selected
            m$selected_person <- NA_integer_
            updateSelectizeInput(
              session, "person", choices = NULL, server = TRUE, 
              options = list(
                placeholder = i18n$t("no_person_in_subset"),
                onInitialize = I("function() { this.setValue(''); }")
            ))
            
            output$person_switch_nums <- renderUI("")
          }
          
          if (num_patients > 0){
            
            # Load a specific person_id if noticed in loading_options
            if (length(r$loading_options$person_id) > 0){
              
              person_id <- r$loading_options$person_id
              
              if (person_id %in% persons$person_id) selected <- person_id
              else {
                selected <- FALSE
                cat(paste0("\n", now(), " - mod_data - ", person_id, " is not a valid patient ID"))
              }
              
              r$loading_options$person_id <- NULL
            }
            else selected <- FALSE
            
            # Update persons dropdown
            choices <- setNames(persons$person_id, paste(persons$person_id, "-", persons$gender_concept_name))
            updateSelectizeInput(
              session, "person", choices = choices, server = TRUE, selected = selected,
              options = list(
                placeholder = "",
                onInitialize = I("function() { this.setValue(''); }")
              )
            )
            
            output$person_switch_nums <- renderUI(div("1 / ", num_patients))
            r$num_selected_patient <- 1
          }
        }
        
        # Reset other dropdowns & uiOutput
        updateSelectizeInput(
          session, "visit_detail", choices = NULL, server = TRUE,
          options = list(
            placeholder = "",
            onInitialize = I("function() { this.setValue(''); }")
          )
        )
        output$person_info <- renderUI("")
      })
      
      # When a patient is selected
      observeEvent(input$person, {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer input$person"))
        
        req(input$person)
        
        person_id <- as.numeric(input$person)
        m$selected_person <- person_id
        
        # Update switch buttons
        shinyjs::show("patient_switching_buttons")
        num_patient <- r$subset_merged_patients %>% dplyr::filter(person_id == !!person_id) %>% dplyr::pull(n)
        r$num_selected_patient <- num_patient
        num_patients <- max(r$subset_merged_patients$n)
        output$person_switch_nums <- renderUI(div(num_patient, " / ", num_patients))
        
        # Reset variables
        sapply(person_tables, function(table) d$data_person[[table]] <- tibble::tibble())
        sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())

        # Reset selected_visit_detail
        m$selected_visit_detail <- NA_integer_
        
        no_stay_available <- FALSE
        if ("visit_detail_id" %not_in% colnames(d$visit_detail)) no_stay_available <- TRUE
        if ("visit_detail_id" %in% colnames(d$visit_detail)) if (d$visit_detail %>% dplyr::filter(person_id == !!person_id) %>% dplyr::count() %>% dplyr::pull() == 0) no_stay_available <- TRUE
        
        if (no_stay_available) updateSelectizeInput(
          session, "visit_detail", choices = NULL, server = TRUE,
          options = list(
            placeholder = i18n$t("no_stay_available"),
            onInitialize = I("function() { this.setValue(''); }")
          )
        )
        
        if (!no_stay_available){
          
          visit_detail <- d$visit_detail %>% dplyr::mutate_at("person_id", as.integer) %>% dplyr::filter(person_id == !!person_id)
          
          if ("parent_visit_detail_id" %in% colnames(visit_detail)) visit_detail <- visit_detail %>% dplyr::filter(is.na(parent_visit_detail_id))
          else if ("visit_detail_parent_id" %in% colnames(visit_detail)) visit_detail <- visit_detail %>% dplyr::filter(is.na(visit_detail_parent_id))
          
          visit_detail <-
            visit_detail %>% 
            dplyr::collect() %>% 
            dplyr::left_join(
              d$dataset_concept %>% dplyr::select(visit_detail_concept_id = concept_id, visit_detail_concept_name = concept_name),
              by = "visit_detail_concept_id"
            ) %>%
            dplyr::arrange(visit_detail_start_datetime)
          
          if ("visit_detail_concept_name" %in% colnames(visit_detail)){
            
            if (tolower(language) == "fr") visit_details <- 
                visit_detail %>% 
                dplyr::mutate(
                  name_display = paste0(
                    format(as.POSIXct(visit_detail_start_datetime), format = "%d-%m-%Y"), " ", 
                    tolower(i18n$t("to")), " ",  format(as.POSIXct(visit_detail_end_datetime), format = "%d-%m-%Y"),
                    " - ", visit_detail_concept_name
                  )
                )
            
            else visit_details <- 
                visit_detail %>% 
                dplyr::mutate(
                  name_display = paste0(
                    format(as.POSIXct(visit_detail_start_datetime), format = "%Y-%m-%d"), " ",
                    tolower(i18n$t("to")), " ",  format(as.POSIXct(visit_detail_end_datetime), format = "%Y-%m-%d"),
                    " - ", visit_detail_concept_name
                  )
                )
          }
          else {
            
            if (tolower(language) == "fr") visit_details <-
                visit_detail %>% dplyr::mutate(
                  name_display = paste0(
                    format(as.POSIXct(visit_detail_start_datetime), format = "%d-%m-%Y"), " ",
                    tolower(i18n$t("to")), " ",  format(as.POSIXct(visit_detail_end_datetime), format = "%d-%m-%Y"),
                    " - ", visit_detail_concept_id
                  )
                )
            
            
            else visit_details <- 
                visit_detail %>% 
                dplyr::mutate(
                  name_display = paste0(
                    format(as.POSIXct(visit_detail_start_datetime), format = "%Y-%m-%d"), " ",
                    tolower(i18n$t("to")), " ",  format(as.POSIXct(visit_detail_end_datetime), format = "%Y-%m-%d"),
                    " - ", visit_detail_concept_id
                  )
                )
          }
          
          # Load visit_details of the person & update dropdown
          choices <- setNames(visit_details$visit_detail_id, visit_details$name_display)
          updateSelectizeInput(
            session, "visit_detail", choices = choices, server = TRUE, selected = FALSE,
            options = list(
              placeholder = "",
              onInitialize = I("function() { this.setValue(''); }")
            )
          )
        }
        
        # Update person informations on sidenav
        style <- "display:inline-block; width:80px; font-weight:bold;"
        
        person <- 
          d$person %>%
          dplyr::mutate_at("person_id", as.integer) %>%
          dplyr::filter(person_id == !!person_id) %>% 
          dplyr::collect() %>%
          dplyr::left_join(d$dataset_concept %>% dplyr::select(gender_concept_id = concept_id, gender_concept_name = concept_name), by = "gender_concept_id")
        
        output$person_info <- renderUI(
          tagList(
            span(i18n$t("person_id"), style = style), person$person_id, br(),
            span(i18n$t("gender"), style = style), person$gender_concept_name
          )
        )
        
        sapply(c("visit_detail", "person_info"), function(name) shinyjs::show(paste0(name, "_div")))
        
        # Reset variables
        sapply(person_tables, function(table) d$data_person[[table]] <- tibble::tibble())
        sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())
        
        # Reset selected_visit_detail
        m$selected_visit_detail <- NA_integer_
      })
      
      ## Stay ----
      
      observeEvent(input$visit_detail, {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer input$visit_detail"))
        
        req(input$visit_detail)
        
        selected_visit_detail <- as.numeric(input$visit_detail)
        m$selected_visit_detail <- selected_visit_detail
        
        # Update person informations on sidenav
        
        style <- "display:inline-block; width:80px; font-weight:bold;"
        
        person_id <- m$selected_person
        
        person <- 
          d$person %>%
          # dplyr::mutate_at("person_id", as.integer) %>%
          dplyr::filter(person_id == !!person_id) %>% 
          dplyr::collect() %>%
          dplyr::left_join(d$dataset_concept %>% dplyr::select(gender_concept_id = concept_id, gender_concept_name = concept_name), by = "gender_concept_id")
        
        visit_detail_id <- selected_visit_detail
        visit_detail <- 
          d$visit_detail %>% 
          # dplyr::mutate_at("visit_detail_id", as.integer) %>%
          dplyr::filter(visit_detail_id == !!visit_detail_id) %>% 
          dplyr::collect() %>%
          dplyr::left_join(d$dataset_concept %>% dplyr::select(visit_detail_concept_id = concept_id, visit_detail_concept_name = concept_name), by = "visit_detail_concept_id")
        
        if (!is.na(person$birth_datetime)) age <- lubridate::interval(person$birth_datetime, visit_detail$visit_detail_start_datetime) / lubridate::years(1)
        else if (is.na(person$birth_datetime) & !is.na(person$year_of_birth)) age <- as.numeric(format(visit_detail$visit_detail_start_datetime, "%Y")) - person$year_of_birth
        else age <- NA_integer_
        
        age_div <- tagList(round(age, 0), " ", i18n$t("years"))
        if (!is.na(age) & age <= 2) age_div <- tagList(round(age * 12, 0), " ", i18n$t("months"))
        
        if ("visit_detail_concept_name" %in% names(visit_detail)) visit_detail_concept_name <- visit_detail %>% dplyr::pull(visit_detail_concept_name)
        else visit_detail_concept_name <- visit_detail %>% dplyr::pull(visit_detail_concept_id)
        
        max_length <- 26
        if (is.na(visit_detail_concept_name)) visit_detail_concept_name <- i18n$t("concept_not_loaded")
        if (nchar(visit_detail_concept_name) > max_length){
          visit_detail_concept_name_short <- paste0(substr(visit_detail_concept_name, 1, max_length - 3), "...")
          visit_detail_div <- div(create_hover_card(ui = tags$span(visit_detail_concept_name_short), text = visit_detail_concept_name), style = "display: inline-block;")
        }
        else visit_detail_div <- tags$span(visit_detail_concept_name)
        
        los <- as.integer(difftime(visit_detail$visit_detail_end_datetime, visit_detail$visit_detail_start_datetime, units = "days"))
        days_trad <- ifelse(los == 1, i18n$t("day"), i18n$t("days"))
        
        output$person_info <- renderUI({
          tagList(
            span(i18n$t("person_id"), style = style), tags$span(m$selected_person), br(),
            span(i18n$t("gender"), style = style), tags$span(person$gender_concept_name), br(), br(),
            span(i18n$t("visit_detail_id"), style = style), tags$span(selected_visit_detail), br(),
            span(i18n$t("age"), style = style), tags$span(age_div), br(),
            span(i18n$t("hosp_unit"), style = style), visit_detail_div, br(),
            span(i18n$t("from"), style = style), tags$span(visit_detail$visit_detail_start_datetime %>% format_datetime(language, sec = FALSE)), br(),
            span(i18n$t("to"), style = style), tags$span(visit_detail$visit_detail_end_datetime %>% format_datetime(language, sec = FALSE)), br(),
            span(i18n$t("duration"), style = style), tags$span(los, " ", tolower(days_trad))
          )
        })
      })
      
      ## Patients switching ----
      
      observeEvent(input$next_patient, {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer input$next_patient"))
        
        num_selected_patient <- r$num_selected_patient + 1
        if (num_selected_patient > max(r$subset_merged_patients$n)) num_selected_patient <- r$num_selected_patient
        r$num_selected_patient <- num_selected_patient
        
        person_id <- r$subset_merged_patients %>% dplyr::filter(n == num_selected_patient) %>% dplyr::pull(person_id)
        
        updateSelectizeInput(session, "person", selected = person_id)
      })
      
      observeEvent(input$previous_patient, {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer input$previous_patient"))
        
        num_selected_patient <- r$num_selected_patient - 1
        if (num_selected_patient < min(r$subset_merged_patients$n)) num_selected_patient <- r$num_selected_patient
        r$num_selected_patient <- num_selected_patient
        
        person_id <- r$subset_merged_patients %>% dplyr::filter(n == num_selected_patient) %>% dplyr::pull(person_id)
        
        updateSelectizeInput(session, "person", selected = person_id)
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
          dplyr::ungroup() %>%
          dplyr::arrange(category, display_order)

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
        
        sapply(categories, function(category){
          
          tab_group_id <- r$data_tabs_groups %>% dplyr::filter(category == !!category) %>% dplyr::pull(id)
          
          # Create an ID per level / sub_group
          all_tabs <- r$data_menu_tabs %>% dplyr::filter(tab_group_id == !!tab_group_id) %>% dplyr::arrange(display_order)
          
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
              
              pivot <- 
                div(
                  id = ns(pivot_id),
                  prepare_sortable_pivot_tabs(ns, category, tab_group_id, tab_sub_group, tabs_ui)
                )
              
              if (is.na(r[[paste0(category, "_selected_tab")]]) & i > 1) pivot <- shinyjs::hidden(pivot)
              if (!is.na(r[[paste0(category, "_selected_tab")]]) & r[[paste0(category, "_selected_tab")]] %not_in% tabs$id) pivot <- shinyjs::hidden(pivot)
              pivot_tabs <- paste0(category, "_pivot_tabs_", tab_group_id, "_", tab_sub_group)
              
              sortable_id <- paste0("study_pivot_sub_div_", tab_group_id, "_", tab_sub_group)
              pivots <- tagList(
                pivots, 
                pivot,
                tags$script(HTML(sprintf("$(document).ready(function() { setTimeout(function() { initSortableTabs('%s'); }, 500); });", ns(sortable_id))))
              )
              
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
              
              # first_list_element <- list(key = "main", text = shiny.fluent::FontIcon(iconName = "Home"), href = paste0("#!/data?type=", category), isCurrentItem = FALSE,
              #   onClick = htmlwidgets::JS(paste0("item => {",
              #     "Shiny.setInputValue('", id, "-study_go_to_tab', ", study_first_tab_id, ");",
              #     "Shiny.setInputValue('", id, "-study_go_to_tab_trigger', Math.random());",
              #     "}"
              #   )))
              # 
              # breadcrumb_list <- list(first_list_element)
              # 
              # if (nb_levels >= 2){
              #   
              #   for (j in 1:nrow(tabs_tree)){
              #     
              #     row <- tabs_tree[j, ]
              #     
              #     if (row$level == nb_levels - 1) breadcrumb_list <- rlist::list.append(breadcrumb_list, list(key = "main", text = row$name, isCurrentItem = TRUE))
              #     else {
              #       breadcrumb_list <- rlist::list.append(breadcrumb_list, list(
              #         key = "main", text = row$name,
              #         onClick = htmlwidgets::JS(paste0(
              #           "item => {",
              #           "Shiny.setInputValue('", id, "-study_go_to_tab', ", row$id, ");",
              #           "Shiny.setInputValue('", id, "-study_go_to_tab_trigger', Math.random());",
              #           "}"
              #         ))
              #       ))
              #     }
              #   }
              # }
              # 
              # breadcrumb <- div(
              #   id = ns(paste0(category, "_study_breadcrumb_", tab_group_id, "_", tab_sub_group)),
              #   shiny.fluent::Breadcrumb(items = breadcrumb_list, maxDisplayedItems = 5),
              #   style = "margin-left: 10px;"
              # )
              # 
              # if (is.na(r[[paste0(category, "_selected_tab")]]) & i > 1) breadcrumb <- shinyjs::hidden(breadcrumb)
              # if (!is.na(r[[paste0(category, "_selected_tab")]]) & r[[paste0(category, "_selected_tab")]] %not_in% tabs$id) breadcrumb <- shinyjs::hidden(breadcrumb)
              
              # breadcrumbs <- tagList(breadcrumbs, breadcrumb)
              
              i <- 2L
            }
            
            # study_menu_ui <- tagList(div(breadcrumbs), div(pivots))
            study_menu_ui <- tagList(div(), div(pivots))
          }
          
          if (nrow(all_tabs) == 0){
            
            study_menu_ui <- div()
            
            # study_menu_ui <- div(
            #   id = ns(paste0(category, "_study_breadcrumb_", tab_group_id, "_0")),
            #   shiny.fluent::Breadcrumb(items = list(list(key = "main", text = shiny.fluent::FontIcon(iconName = "Home"), href = paste0("#!/data?type=", category), isCurrentItem = FALSE,
            #     onClick = htmlwidgets::JS(paste0("item => {",
            #       "Shiny.setInputValue('", id, "-study_go_to_tab', 0);",
            #       "Shiny.setInputValue('", id, "-study_go_to_tab_trigger', Math.random());",
            #       "}"
            #     )))), maxDisplayedItems = 3)
            # )
            # 
            r[[paste0(category, "_no_tabs_to_display")]] <- TRUE
            if (r$data_page == category) shinyjs::show(paste0(category, "_no_tabs_to_display"))
          }
          
          output[[paste0(category, "_study_menu")]] <- renderUI({
            div(
              study_menu_ui,
              style = "display:flex; justify-content:space-between;"
            )
          })
          
          displayed_category <- r$data_page
          hidden_category <- categories[categories != displayed_category]
          
          shinyjs::show(paste0(displayed_category, "_study_menu"))
          shinyjs::hide(paste0(hidden_category, "_study_menu"))
        })
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
            load_tab_server(tab_id, action = "load_tabs")
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
        
        # Reload window size (correct bug with some plugins display)
        shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
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
      
      # A tab is moved
      observeEvent(input[[paste0(category, "_tab_positions")]], {
        if (debug) cat(paste0("\n", now(), " - mod_data - observer input$", category, "_tab_positions"))
        
        positions <- input[[paste0(category, "_tab_positions")]]
        
        ids <- positions[c(TRUE, FALSE)]
        new_positions <- positions[c(FALSE, TRUE)]
        
        for (i in 1:length(ids)) {
          tab_id <- ids[i]
          new_position <- new_positions[i]
          
          r$data_menu_tabs <- r$data_menu_tabs %>% dplyr::mutate(display_order = ifelse(id == tab_id, new_position, display_order))
          r$data_tabs <- r$data_tabs %>% dplyr::mutate(display_order = ifelse(id == tab_id, new_position, display_order))
          
          # Update database with the new position
          sql <- glue::glue_sql("UPDATE tabs SET display_order = {new_position} WHERE id = {tab_id}", .con = r$db)
          DBI::dbExecute(r$db, sql)
        }
        r$data_menu_tabs <- r$data_menu_tabs %>% dplyr::arrange(category, display_order)
        r$data_tabs <- r$data_tabs %>% dplyr::arrange(category, display_order)
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
          if (nrow(widgets) > 0) show_message_bar(id, output, message = "add_tab_has_widgets", i18n = i18n, ns = ns)
          req(nrow(widgets) == 0)
        }
      }
      
      # Check if name is not already used
      if (is.na(parent_tab_id)) sql <- glue::glue_sql("SELECT name FROM tabs WHERE category = {category} AND tab_group_id = {tab_group_id} AND parent_tab_id IS NULL", .con = r$db)
      else sql <- glue::glue_sql("SELECT name FROM tabs WHERE category = {category} AND tab_group_id = {tab_group_id} AND parent_tab_id = {parent_tab_id}", .con = r$db)
      tabs_names <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
      name_already_used <- remove_special_chars(tab_name) %in% remove_special_chars(tabs_names)
      
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
      show_message_bar(id, output, "tab_added", "success", i18n = i18n, ns = ns)
      
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
      show_message_bar(id, output, message = "modif_saved", type = "success", i18n = i18n, ns = ns)
      
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
      
      # Remove all widgets from gridstack
      data_widgets <- r$data_widgets %>% dplyr::filter(tab_id == !!tab_id)
      for (widget_id in data_widgets$id){
        shinyjs::runjs(paste0("
          var grid = window.gridStackInstances['", tab_id, "'];
          var current_widget = grid.el.querySelector('#", ns(paste0("data_gridstack_item_", widget_id)), "');
          if (current_widget) grid.removeWidget(current_widget);"))
      }
      
      # Delete tab
      shinyjs::hide(paste0("gridstack_", tab_id))
      
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
      
      first_tab <- r$data_menu_tabs %>% dplyr::filter(category == r$data_page, level == tab$level, id != tab_id)
      
      if (nrow(first_tab) == 0){
        selected_tab <- r$data_menu_tabs %>% dplyr::filter(category == r$data_page, level == tab$level - 1)
        if (nrow(selected_tab) == 0){
          selected_tab <- 0
        }
      }
      else selected_tab <- first_tab %>% dplyr::slice(1) %>% dplyr::pull(id)
      
      r[[paste0(category, "_selected_tab")]] <- selected_tab
      
      # Reload study menu
      r$data_reload_tabs <- now()
      
      # Notify user
      show_message_bar(id, output, message = "tab_deleted", type = "warning", i18n = i18n, ns = ns)
      
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
      
      if (is.na(r[[paste0(r$data_page, "_selected_tab")]])) show_message_bar(id, output, message = "create_a_tab_before_adding_a_widget", type = "warning", i18n = i18n, ns = ns)
      
      req(!is.na(r[[paste0(r$data_page, "_selected_tab")]]))
      
      if (r[[paste0(r$data_page, "_selected_tab")]] %in% r$data_tabs_full_screen$tab_id) show_message_bar(id, output, message = "cant_add_widget_in_full_screen_mode", type = "warning", i18n = i18n, ns = ns)
      req(r[[paste0(r$data_page, "_selected_tab")]] %not_in% r$data_tabs_full_screen$tab_id)
      
      shinyjs::show("add_widget_modal")
      
      # Reload concepts datatable
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_vocabulary_datatable', Math.random())"))
      
      # Set opened widget modal to creation
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-opened_widget_modal', 'add');"))
      
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
      shinyjs::hide(paste0(input$opened_widget_modal, "_widget_modal"))
    })
    
    observeEvent(input$close_select_a_plugin_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$close_select_a_plugin_modal"))
      shinyjs::hide("select_a_plugin_modal")
      shinyjs::show(paste0(input$opened_widget_modal, "_widget_modal"))
    })
    
    observeEvent(input$reload_plugins_var, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$reload_plugins_var"))
      
      reload_elements_var(page_id = id, id = "plugins", con = r$db, r = r, m = m, long_var_filtered = "filtered_data_plugins_long", user_accesses)
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_plugins_list', Math.random());"))
    })
    
    observeEvent(input$reload_plugins_list, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$reload_plugins_list"))
      
      elements_ui <- create_elements_ui(page_id = id, id = "plugins", elements = r$filtered_data_plugins_long, r = r, language = language, i18n = i18n)
      
      output$plugins_widgets <- renderUI(elements_ui)
    })
    
    # Display a plugin description
    
    observeEvent(input$show_plugin_description_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$show_plugin_description_trigger"))
      
      shinyjs::hide("select_a_plugin_modal")
      shinyjs::delay(50, shinyjs::show("plugin_description_modal"))
      
      description_code <-
        r$plugins_long %>%
        dplyr::filter(id == input$show_plugin_description, name == paste0("description_", language)) %>%
        dplyr::pull(value)
      
      description_title <-
        r$plugins_wide %>%
        dplyr::filter(id == input$show_plugin_description) %>%
        dplyr::pull(name)
      
      if (description_code == "" | is.na(description_code)) output$plugin_description <- renderUI(div(shiny.fluent::MessageBar(i18n$t("no_description_available"), messageBarType = 5) ,style = "display: inline-block;"))
      else {
        output_file <- create_rmarkdown_file(r, description_code, interpret_code = FALSE)
        output$plugin_description <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(output_file))))
      }
      output$plugin_description_title <- renderUI(tags$h1(description_title))
    })
    
    observeEvent(input$close_plugin_description_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$close_plugin_description_modal"))
      
      shinyjs::hide("plugin_description_modal")
      shinyjs::delay(50, shinyjs::show("select_a_plugin_modal"))
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
      if (length(short_description) == 0) short_description <- ""
      
      users_ui <- create_authors_ui(row %>% dplyr::filter(name == "author") %>% dplyr::pull(value))
      plugin_buttons <- ""
      element_ui <- create_element_ui(ns, id, input$selected_element, "plugin", plugin_name, users_ui, plugin_buttons, "", short_description, FALSE)
      
      output[[paste0(input$opened_widget_modal, "_widget_selected_plugin")]] <- renderUI(element_ui)
      
      shinyjs::show(paste0(input$opened_widget_modal, "_widget_modal"))
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
      if (length(input$selected_element) == 0) output$add_widget_selected_plugin <- renderUI(
        div(
          class = "element_widget plugin_widget",
          div(
            class = "element_widget_icon plugin_widget_icon",
            tags$i(class = "fas fa-puzzle-piece")
          ),
          div(
            class = "element_widget_content",
            tags$h1(i18n$t("select_a_plugin"), style = "font-weight: 600; color: #B83137")
          )
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
      sql <- glue::glue_sql("SELECT name FROM widgets WHERE tab_id = {tab_id}", .con = r$db)
      widgets_names <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
      name_already_used <- remove_special_chars(widget_name) %in% remove_special_chars(widgets_names)
      
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
        
        # Reset add concept buttons
        sapply(r[[paste0(id, "_selected_concepts")]]$concept_id, function(concept_id) shinyjs::runjs(paste0("$('#", id, "-add_concept_", concept_id, " i').removeClass('fa-minus').addClass('fa-plus');")))
        
        # Reset selected concepts
        r[[paste0(id, "_selected_concepts")]] <- tibble::tibble(
          concept_id = integer(), concept_name = character(), domain_id = character(), vocabulary_id = character(),
          mapped_to_concept_id = integer(), merge_mapped_concepts = logical()
        )
      }
      
      # Notify user
      show_message_bar(id, output, message = "widget_added", type = "success", i18n = i18n, ns = ns)
      
      # Reload update_datetime
      sql_update_datetime(r, m)
      
      # Reset fields
      shiny.fluent::updateTextField.shinyInput(session, "widget_creation_name", value = "")
      
      shiny.fluent::updateSearchBox.shinyInput(session, "search_plugin", value = "")
      
      # Reset selected plugin and selected concepts
      shinyjs::runjs(update_selected_concepts_css$add)
      output$add_widget_selected_concepts <- renderUI(default_selected_concepts_ui)
      
      output$add_widget_selected_plugin <- renderUI(default_selected_plugin_ui)
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-selected_element', null);"))
      
      output$add_widget_selected_concepts <- renderUI(default_selected_concepts_ui)
      output$selected_concepts_list <- renderUI("")
      
      ## Load front-end & back-end ----
      
      load_tab_plugins(tab_id, widget_id, "add")
      load_tab_ui(category, tab_id, widget_id, action = "add_widget")
      load_tab_server(tab_id, widget_id, "add_widget")
      
      # Close modal
      shinyjs::hide("add_widget_modal")
      
      # Reload menu (issue : it changed selected tab)
      r$data_reload_menu <- now()
    })
    
    # --- --- --- --- --
    # Edit a widget ----
    # --- --- --- --- --
    
    observeEvent(input$edit_widget_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$edit_widget_trigger"))
      
      # Set opened widget modal to creation
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-opened_widget_modal', 'edit');"))
      
      # Get widget informations and update fields
      widget <- r$data_widgets %>% dplyr::filter(id == input$edit_widget_id)
      
      ## Widget name
      
      shiny.fluent::updateTextField.shinyInput(session, "widget_edition_name", value = widget$name)
      
      ## Selected plugin
      
      row <- r$filtered_data_plugins_long %>% dplyr::filter(id == widget$plugin_id)
      
      if (nrow(row) > 0){
        plugin_type <- row %>% dplyr::slice(1) %>% dplyr::pull(tab_type_id)
        plugin_name <- row %>% dplyr::filter(name == paste0("name_", language)) %>% dplyr::pull(value)
        short_description <- row %>% dplyr::filter(name == paste0("short_description_", language)) %>% dplyr::pull(value)
        if (length(short_description) == 0) short_description <- ""
        
        users_ui <- create_authors_ui(row %>% dplyr::filter(name == "author") %>% dplyr::pull(value))
        plugin_buttons <- ""
        element_ui <- create_element_ui(ns, id, input$selected_element, "plugin", plugin_name, users_ui, plugin_buttons, "", short_description, FALSE)
        
        output$edit_widget_selected_plugin <- renderUI(element_ui)
        
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-selected_element', ", widget$plugin_id, ")"))
      }
      else output$edit_widget_selected_plugin <- renderUI(default_selected_plugin_ui)
      
      ## Selected concepts
      
      r$data_selected_concepts <- tibble::tibble()
      if (length(d$dataset_concept) > 0) r$data_selected_concepts <-
        d$dataset_concept %>%
        dplyr::filter(concept_id %in% r$data_widgets_concepts$concept_id) %>%
        dplyr::transmute(concept_id, concept_name, domain_id, vocabulary_id, mapped_to_concept_id = NA_integer_, merge_mapped_concepts = FALSE)
      
      selected_concepts_ui <- tagList()
      selected_concepts_list_ui <- tagList()
      
      if (nrow(r$data_selected_concepts) > 0){
        for (i in 1:nrow(r$data_selected_concepts)){
          row <- r$data_selected_concepts[i, ]
          
          if (row$domain_id %in% c("Drug", "Condition", "Procedure", "Measurement", "Observation")) concept_style <- paste0(tolower(row$domain_id), "_domain_id")
          else concept_style <- "default_domain_id"
          
          concept_style <- paste0("selected_concept ", concept_style)
          concept_name <- paste0(row$vocabulary_id, " - ", row$concept_name)
          
          selected_concepts_list_ui <- tagList(
            selected_concepts_list_ui,
            div(
              div(
                shiny.fluent::IconButton.shinyInput(ns(paste0("remove_concept_", row$concept_id)), iconProps = list(iconName = "Cancel"), style = "height: 20px; margin: 0; font-size: 10px;"),
                onclick = paste0(
                  "Shiny.setInputValue('", id, "-remove_concept_trigger', Math.random());",
                  "Shiny.setInputValue('", id, "-remove_concept', ", row$concept_id, ");"
                ),
                class = "small_icon_button"
              ),
              create_hover_card(ui = div(concept_name, class = concept_style), text = concept_name),
              style = "display: flex; margin: 2px 0;"
            )
          )
          
          selected_concepts_ui <- tagList(
            selected_concepts_ui, 
            create_hover_card(ui = div(concept_name, class = concept_style, style = "display: inline-block;"), text = concept_name)
          )
        }
        
        shinyjs::runjs(paste0(
          "$('#", id, "-edit_widget_selected_concepts').css('height', 'auto');",
          "$('#", id, "-edit_widget_selected_concepts').css('justify-content', 'left');",
          "$('#", id, "-edit_widget_selected_concepts').css('align-items', 'flex-wrap;');"
        ))
      }
      else {
        shinyjs::runjs(update_selected_concepts_css$edit)
        selected_concepts_ui <- div(i18n$t("select_concepts"), class = "default_content_widget")
      }
      
      output$edit_widget_selected_concepts <- renderUI(selected_concepts_ui)
      output$selected_concepts_list <- renderUI(selected_concepts_list_ui)
      
      # Reload vocabulary datatable
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_vocabulary_datatable', Math.random())"))
      
      shinyjs::show("edit_widget_modal")
    })
    
    # Close modal
    observeEvent(input$close_edit_widget_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$close_edit_widget_modal"))
      shinyjs::hide("edit_widget_modal")
      
      # Reload selected concepts
      output$selected_concepts_list <- renderUI("")
      output$add_widget_selected_concepts <- renderUI(renderUI(default_selected_concepts_ui))
      
      r$data_selected_concepts <- tibble::tibble(
        concept_id = integer(), concept_name = character(), domain_id = character(), vocabulary_id = character(),
        mapped_to_concept_id = integer(), merge_mapped_concepts = logical()
      )
    })
    
    # Save updates
    observeEvent(input$widget_edition_save, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$widget_edition_save"))
      
      widget_id <- input$edit_widget_id
      tab_id <- r$data_widgets %>% dplyr::filter(id == widget_id) %>% dplyr::pull(tab_id)
      
      # Make sure name and plugins fields aren't empty
      
      req(length(input$widget_edition_name) > 0)
      widget_name <- input$widget_edition_name
      
      # Check if name is not empty
      if (is.na(widget_name) | widget_name == "") shiny.fluent::updateTextField.shinyInput(session, "widget_edition_name", errorMessage = i18n$t("provide_valid_name"))
      req(!is.na(widget_name) & widget_name != "")
      
      # Check if name is not already used
      
      widgets_names <- r$data_widgets %>% dplyr::filter(tab_id == !!tab_id & id != widget_id) %>% dplyr::pull(name)
      name_already_used <- remove_special_chars(widget_name) %in% remove_special_chars(widgets_names)
      if (name_already_used) shiny.fluent::updateTextField.shinyInput(session, "widget_edition_name", errorMessage = i18n$t("name_already_used"))
      req(!name_already_used)
      
      # Check if a plugin is selected
      plugin_id <- input$selected_element
      old_plugin_id <- r$data_widgets %>% dplyr::filter(id == widget_id) %>% dplyr::pull(plugin_id)
      
      if (length(plugin_id) == 0) output$edit_widget_selected_plugin <- renderUI(
        div(
          class = "element_widget plugin_widget",
          div(
            class = "element_widget_icon plugin_widget_icon",
            tags$i(class = "fas fa-puzzle-piece")
          ),
          div(
            class = "element_widget_content",
            tags$h1(i18n$t("select_a_plugin"), style = "font-weight: 600; color: #B83137")
          )
        )
      )
      req(length(plugin_id) > 0)
      
      # Update data in app DB
      
      sql <- glue::glue_sql("UPDATE widgets SET name = {input$widget_edition_name}, plugin_id = {plugin_id} WHERE id = {widget_id}", .con = r$db)
      DBI::dbExecute(r$db, sql)
      
      sql <- glue::glue_sql("DELETE FROM widgets_concepts WHERE widget_id = {widget_id}", .con = m$db)
      DBI::dbExecute(m$db, sql)
      
      if (nrow(r$data_selected_concepts) > 0){
        last_row_widgets_concepts <- get_last_row(m$db, "widgets_concepts")
        new_selected_concepts <-
          r$data_selected_concepts %>%
          dplyr::transmute(
            id = 1:dplyr::n() + last_row_widgets_concepts + 1, widget_id = widget_id,
            concept_id, concept_name, concept_display_name = "", domain_id, mapped_to_concept_id, merge_mapped_concepts,
            creator_id = r$user_id, datetime = now(), deleted = FALSE
          )
        
        DBI::dbAppendTable(m$db, "widgets_concepts", new_selected_concepts)
      }
      
      # Update r vars
      
      new_plugin_id <- plugin_id
      r$data_widgets <- 
        r$data_widgets %>% 
        dplyr::mutate(
          name = ifelse(id == widget_id, input$widget_edition_name, name),
          plugin_id = ifelse(id == widget_id, new_plugin_id, plugin_id)
        )
      
      r$data_widgets_concepts <- r$data_widgets_concepts %>% dplyr::filter(widget_id != !!widget_id)
      if (nrow(r$data_selected_concepts) > 0) r$data_widgets_concepts <- r$data_widgets_concepts %>% dplyr::bind_rows(new_selected_concepts)
      
      # Reset selected concepts
      r$data_selected_concepts <- tibble::tibble(
        concept_id = integer(), concept_name = character(), domain_id = character(), vocabulary_id = character(),
        mapped_to_concept_id = integer(), merge_mapped_concepts = logical()
      )
      
      # Run new UI and server code
      
      if (plugin_id != old_plugin_id){
        load_tab_ui(category, tab_id, widget_id, action = "reload_widget")
        load_tab_server(tab_id, widget_id, "reload_widget")
      }
      
      # Reload selected concepts
      output$selected_concepts_list <- renderUI("")
      output$add_widget_selected_concepts <- renderUI(renderUI(default_selected_concepts_ui))
      
      r$data_selected_concepts <- tibble::tibble(
        concept_id = integer(), concept_name = character(), domain_id = character(), vocabulary_id = character(),
        mapped_to_concept_id = integer(), merge_mapped_concepts = logical()
      )
        
      show_message_bar(id, output, message = "modif_saved", type = "success", i18n = i18n, ns = ns)
      shinyjs::hide("edit_widget_modal")
    })
    
    # --- --- --- --- --- --- --
    # Widget in full screen ----
    # --- --- --- --- --- --- --
    
    r$data_tabs_full_screen <- tibble::tibble(widget_id = integer(), tab_id = integer(), x = integer(), y = integer(), w = integer(), h = integer())
    
    observeEvent(input$widget_full_screen_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$widget_full_screen_trigger"))
      
      widget_id <- input$widget_full_screen_id
      tab_id <- r$data_widgets %>% dplyr::filter(id == widget_id) %>% dplyr::pull(tab_id)
      other_widgets <- r$data_widgets %>% dplyr::filter(tab_id == !!tab_id & id != widget_id) %>% dplyr::pull(id)
      all_widgets <- r$data_widgets %>% dplyr::filter(tab_id == !!tab_id) %>% dplyr::pull(id)
      
      if (tab_id %in% r$data_tabs_full_screen$tab_id){
        
        shiny.fluent::updateIconButton.shinyInput(session, paste0(id, "_widget_full_screen_", widget_id), iconProps = list(iconName = "FullScreen"))
        
        # Reset with last coordinates
        
        sapply(all_widgets, function(widget_id){
          coords <- r$data_tabs_full_screen %>% dplyr::filter(widget_id == !!widget_id)
          
          shinyjs::runjs(paste0(
            "let grid = GridStack.init();",
            "let widget = document.getElementById('", id, "-data_gridstack_item_", widget_id, "');",
            "grid.update(widget, { x: ", coords$x, ", y: ", coords$y, ", w: ", coords$w, ", h: ", coords$h, " });"
          ))
        })
        
        # Show all other widgets
        
        sapply(other_widgets, function(widget_id) shinyjs::delay(100, shinyjs::show(paste0("data_gridstack_item_", widget_id))))
        
        # Enable gridstack edition for this tab
        
        shinyjs::runjs(paste0("
          const grid = window.gridStackInstances['", tab_id, "'];
          grid.setStatic(false);  
        "))
        
        # Add and remove an invisible widget to restart gridstack resizing
        shinyjs::runjs(paste0(
          "const grid = window.gridStackInstances['", tab_id, "'];",
          "if (grid) {",
          "  let tempWidget = document.createElement('div');",
          "  tempWidget.className = 'grid-stack-item';",
          "  tempWidget.style.display = 'none';",
          "  grid.addWidget(tempWidget, { x: 0, y: 0, w: 1, h: 1 });",
          "  grid.removeWidget(tempWidget);",
          "}"
        ))
        
        r$data_tabs_full_screen <- r$data_tabs_full_screen %>% dplyr::filter(tab_id != !!tab_id)
      }
      else {
        
        shiny.fluent::updateIconButton.shinyInput(session, paste0(id, "_widget_full_screen_", widget_id), iconProps = list(iconName = "BackToWindow"))
        
        # Get and save current coordinates
        
        sapply(all_widgets, function(widget_id){
          shinyjs::runjs(paste0(
            "var widget = document.getElementById('", id, "-data_gridstack_item_", widget_id, "');",
            "if (widget) {",
            "  var widgetPosition = {",
            "    id: ", widget_id, ",",
            "    tab_id: ", tab_id, ",",
            "    w: widget.getAttribute('gs-w'),",
            "    h: widget.getAttribute('gs-h'),",
            "    x: widget.getAttribute('gs-x'),",
            "    y: widget.getAttribute('gs-y')",
            "  }",
            "};",
            "Shiny.setInputValue('", id, "-full_screen_widget_position', widgetPosition);",
            "Shiny.setInputValue('", id, "-full_screen_widget_position_trigger', Math.random());"
          ))
        })
        
        # Hide all others widgets
        
        sapply(other_widgets, function(widget_id) shinyjs::hide(paste0("data_gridstack_item_", widget_id)))
        
        # Resize current widget to full screen
        
        # Adding a temporary invisible widget to the grid before resizing the target widget.
        # This helps to "wake up" the gridstack instance, ensuring that layout recalculations
        # are triggered correctly for the target widget. The temporary widget is immediately 
        # removed after resizing to avoid any visible impact on the grid layout.
        
        shinyjs::runjs(paste0(
          "const grid = window.gridStackInstances['", tab_id, "'];",
          "if (grid) {",
          "  let tempWidget = document.createElement('div');",
          "  tempWidget.className = 'grid-stack-item';",
          "  tempWidget.style.display = 'none';",
          "  grid.addWidget(tempWidget, { x: 0, y: 0, w: 1, h: 1 });",
          "  let widget = document.getElementById('", id, "-data_gridstack_item_", widget_id, "');",
          "  grid.update(widget, { x: 0, y: 0, w: 12, h: 40 });",
          "  grid.removeWidget(tempWidget);",
          "}"
        ))
        
        # Disable gridstack edition for this tab
        
        shinyjs::runjs(paste0("
          const grid = window.gridStackInstances['", tab_id, "'];
          grid.setStatic(true);  
        "))
      }
      
      # Reload window size (correct bug with Ace editor display)
      shinyjs::delay(300, shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);"))
    })
    
    observeEvent(input$full_screen_widget_position_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$full_screen_widget_position_trigger"))
      
      widget <- input$full_screen_widget_position
      
      r$data_tabs_full_screen <- r$data_tabs_full_screen %>% dplyr::bind_rows(
        tibble::tibble(
          widget_id = as.integer(widget$id),
          tab_id = as.integer(widget$tab_id),
          x = as.integer(widget$x),
          y = as.integer(widget$y),
          w = as.integer(widget$w),
          h = as.integer(widget$h)
        )
      )
    })
    
    # --- --- --- --- -- -
    # Delete a widget ----
    # --- --- --- --- -- -
    
    observeEvent(input$remove_widget_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$remove_widget_trigger"))
      shinyjs::show("delete_widget_modal")
    })
    
    observeEvent(input$close_widget_deletion_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$close_file_deletion_modal"))
      shinyjs::hide("delete_widget_modal")
    })
    
    observeEvent(input$confirm_widget_deletion, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$confirm_widget_deletion"))
      
      category <- r$data_page
      
      tab_id <- r[[paste0(category, "_selected_tab")]]
      widget_id <- input$remove_widget_id
      
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
      show_message_bar(id, output,  "widget_deleted", "warning", i18n = i18n, ns = ns)
      
      # Reload update_datetime
      sql_update_datetime(r, m)
    })
    
    # --- --- --- --
    # Edit page ----
    # --- --- --- --
    
    r$data_edit_page_activated <- FALSE
    
    observeEvent(input$edit_page, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$edit_page"))
      
      if (r$data_edit_page_activated) r$data_edit_page_activated <- FALSE
      else r$data_edit_page_activated <- TRUE
    })
    
    observeEvent(r$data_edit_page_activated, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer r$data_edit_page_activated"))
      
      if (r$data_edit_page_activated){
        
        # Enable gridstack edition if we are not in full screen mode
        sapply(gsub("gridstack_", "", r$data_grids, fixed = FALSE), function(tab_id){
          if (length(tab_id) > 0){
            if (tab_id %not_in% r$data_tabs_full_screen$tab_id){
              shinyjs::runjs(paste0("
                const grid = window.gridStackInstances['", tab_id, "'];
                grid.setStatic(false);
              "))
            }
          }
        })
        
        # Show edit tab buttons
        distinct_tabs <- r$data_menu_tabs %>% dplyr::pull(id)
        sapply(distinct_tabs, function(tab_id) shinyjs::show(paste0("edit_tab_", tab_id, "_container")))
        
        # Show edit and delete widget buttons
        sapply(r$data_widgets$id, function(widget_id) shinyjs::show(paste0("data_widget_settings_buttons_", widget_id)))
        
        # Update edit page button
        shiny.fluent::updateDefaultButton.shinyInput(session, "edit_page", text = i18n$t("validate_updates"), iconProps = list(iconName = "Accept"))
      }
      else {
        
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
        sapply(gsub("gridstack_", "", r$data_grids, fixed = FALSE), function(tab_id){
          if (length(tab_id) > 0){
            shinyjs::runjs(paste0("
              const grid = window.gridStackInstances['", tab_id, "'];
              grid.setStatic(true);
            "))
          }
        })
        
        # Show edit tab buttons
        distinct_tabs <- r$data_menu_tabs %>% dplyr::pull(id)
        sapply(distinct_tabs, function(tab_id) shinyjs::hide(paste0("edit_tab_", tab_id, "_container")))
        
        # Hide edit and delete widget buttons
        sapply(r$data_widgets$id, function(widget_id) shinyjs::hide(paste0("data_widget_settings_buttons_", widget_id)))
        
        # Update edit page button
        shiny.fluent::updateDefaultButton.shinyInput(session, "edit_page", text = i18n$t("edit_page"), iconProps = list(iconName = "Edit"))
        
        # Hide resize button when sidenav is displayed or not
        r$data_edit_page_activated <- FALSE
        
        # Prevent a bug with scroll into ace editor
        shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
        
        # Reload update_datetime
        sql_update_datetime(r, m)
      }
    }, ignoreInit = TRUE)
    
    # Save each widget position
    observeEvent(input$widget_position_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$widget_position_trigger"))
      
      widget <- input$widget_position
      
      # Don't save if we are in full screen mode
      tab_id <- r$data_widgets %>% dplyr::filter(id == widget$id) %>% dplyr::pull(tab_id)
      req(tab_id %not_in% r$data_tabs_full_screen$tab_id)
      
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
    
    # --- --- --- --- --- --- --- -- -
    # Show / hide widgets buttons ----
    # --- --- --- --- --- --- --- -- -
    
    r$data_show_widgets_buttons <- TRUE
    
    observeEvent(input$show_hide_widgets_buttons, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$show_hide_widgets_buttons"))
      
      # Hide all data widgets buttons
      if (r$data_show_widgets_buttons){
        r$data_show_widgets_buttons <- FALSE
        shiny.fluent::updateDefaultButton.shinyInput(session, "show_hide_widgets_buttons", text = i18n$t("show_widgets_buttons"), iconProps = list(iconName = "RedEye"))
        shinyjs::runjs("$('.data_widget_top_icons').hide()");
        shinyjs::runjs("$('.data_widget_settings_code_panel').css('height', '100%');")
        shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
      }
      
      # Show all data widgets buttons
      else {
        r$data_show_widgets_buttons <- TRUE
        shiny.fluent::updateDefaultButton.shinyInput(session, "show_hide_widgets_buttons", text = i18n$t("hide_widgets_buttons"), iconProps = list(iconName = "Hide"))
        shinyjs::runjs("$('.data_widget_top_icons').show()");
        shinyjs::runjs("$('.data_widget_settings_code_panel').css('height', 'calc(100% - 34px)');")
        shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
      }
    })
    
    # |-------------------------------- -----
    
    # --- --- --- --- --- -
    # Module functions ----
    # --- --- --- --- --- -
    
    create_translations_files <- function(plugin_unique_id, plugin_translations_dir, plugin_folder){
      
      if (!dir.exists(plugin_translations_dir)) dir.create(plugin_translations_dir)
      if (!dir.exists(plugin_folder)) dir.create(plugin_folder)
      
      # Create translations file if doesn't exist, from database
      translations_file <- paste0(plugin_folder, "/translations.csv")
      
      if (!file.exists(translations_file)){
        
        file_path <- file.path(r$app_folder, "plugins", plugin_unique_id, "translations.csv")
        translations_code <- readLines(file_path, warn = FALSE) %>% paste(collapse = "\n")
        
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
      }
    }
    
    load_tab_ui <- function(category, tab_id, widget_id = NA_integer_, action){

      widgets_ui <- tagList()

      if (action %in% c("add_tab", "load_tabs")) widgets <- r$data_widgets %>% dplyr::filter(tab_id == !!tab_id)
      else if (action %in% c("add_widget", "reload_widget")) widgets <- r$data_widgets %>% dplyr::filter(id == !!widget_id)
      widgets <- widgets %>% dplyr::rename(widget_id = id)

      # Create a tab gridstack instance ?
      if (action %in% c("load_tabs", "add_tab")){

        selected_tab <- r[[paste0(category, "_selected_tab")]]
        
        gridstack_id <- paste0("gridstack_", tab_id)
        
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
            ui_code <- div(shiny.fluent::MessageBar(i18n$t("plugin_deleted"), messageBarType = 5), style = "display: inline-block; margin: 5px 8px;")
            settings_widget_button <- ""
          }
          else {

            # Get plugin unique_id
            plugin_unique_id <- r$plugins_long %>% dplyr::filter(id == plugin_id, name == "unique_id") %>% dplyr::pull(value)

            # Get plugin folder
            plugin_folder <- paste0(r$app_folder, "/plugins/", plugin_unique_id)

            # Create translations files and var
            plugin_translations_dir <- paste0(r$app_folder, "/translations/", plugin_unique_id)
            create_translations_files(plugin_unique_id, plugin_translations_dir, plugin_folder)

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
              
                file_path <- file.path(r$app_folder, "plugins", plugin_unique_id, "ui.R")
                ui_code <-
                  readLines(file_path, warn = FALSE) %>% paste(collapse = "\n") %>%
                  process_widget_code(tab_id, widget_id, m$selected_study, patient_id, plugin_folder)
                
                eval(parse(text = ui_code))
              },
              error = function(e){
                r$widget_ui_last_error <- e
                show_message_bar(id, output,  "error_run_plugin_ui_code", "severeWarning", i18n = i18n, ns = ns)
                cat(paste0("\n", now(), " - mod_data - error loading UI code - widget_id = ", widget_id, " - ", toString(e)))
              }
            )
          }
          
          # Get widget position
          if (action %in% c("add_tab", "load_tabs", "add_widget")){
            sql <- glue::glue_sql("SELECT value FROM widgets_options WHERE widget_id = {widget_id} AND category = 'widget_position'", .con = m$db)
            widget_position <- DBI::dbGetQuery(m$db, sql) %>% dplyr::pull(value)
            matches <- stringr::str_match(widget_position, "w=(\\d+);h=(\\d+);x=(\\d+);y=(\\d+)")
            widget_pos <- list(w = as.integer(matches[2]), h = as.integer(matches[3]), x = as.integer(matches[4]), y = as.integer(matches[5]))
            
            ui_output <- create_widget(id, widget_id, ui_code, w = widget_pos$w, h = widget_pos$h, x = widget_pos$x, y = widget_pos$y)
            
            add_widget_to_gridstack(id, tab_id, ui_output, widget_id)
          }
          
          output[[paste0("ui_", widget_id)]] <- renderUI(ui_code)
          
          if (action %in% c("add_tab", "load_tabs", "add_widget")) output[[paste0("edit_buttons_", widget_id)]] <- renderUI(get_widget_edit_buttons(id, widget_id, show_edit_buttons = r$data_edit_page_activated))
        })
      }
    }
    
    load_tab_server <- function(tab_id, widget_id = NA_integer_, action = NA_character_){
      
      shinyjs::delay(100, {
        # Get tabs and widgets
        
        if (action == "load_tabs") widgets <- r$data_widgets %>% dplyr::filter(tab_id == !!tab_id)
        else if (action %in% c("add_widget", "reload_widget")) widgets <- r$data_widgets %>% dplyr::filter(id == !!widget_id)
        widgets <- widgets %>% dplyr::rename(widget_id = id)
        
        req(nrow(widgets) > 0)
        
        widgets_concepts <- r$data_widgets_concepts %>% dplyr::inner_join(widgets %>% dplyr::select(widget_id), by = "widget_id")
          
        widgets_ids <- unique(widgets$widget_id)
        
        # Loop over widgets
        sapply(widgets_ids, function(widget_id){
          
          # Run plugin server code
          # Only if this code has not been already loaded
          trace_code <- paste0(widget_id, "_", m$selected_study)
          
          if (trace_code %not_in% r$widgets_server_code_loaded | action == "reload_widget"){
            
            # Add the trace_code to loaded plugins list
            r$widgets_server_code_loaded <- c(r$widgets_server_code_loaded, trace_code)
            
            selected_concepts <- 
              widgets_concepts %>% 
              dplyr::filter(widget_id == !!widget_id) %>%
              dplyr::select(concept_id, concept_name, concept_display_name, domain_id, mapped_to_concept_id, merge_mapped_concepts)
            
            # Get plugin code
            
            ids <- widgets %>% dplyr::filter(widget_id == !!widget_id) %>% dplyr::slice(1) %>% dplyr::select(plugin_id, tab_id)
            
            plugin_id <- ids$plugin_id
            plugin_unique_id <- r$plugins_long %>% dplyr::filter(id == plugin_id, name == "unique_id") %>% dplyr::pull(value)
            
            tab_id <- ids$tab_id
            
            # Check if plugin has been deleted
            check_deleted_plugin <- nrow(DBI::dbGetQuery(r$db, paste0("SELECT * FROM plugins WHERE id = ", plugin_id))) == 0
            if (!check_deleted_plugin){
              
              file_path <- file.path(r$app_folder, "plugins", plugin_unique_id, "server.R")
              server_code <- readLines(file_path, warn = FALSE) %>% paste(collapse = "\n")
              
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
            tryCatch(eval(parse(text = server_code), envir = new_env),
              error = function(e){
                r$widget_server_last_error <- e
                show_message_bar(id, output,  "error_run_plugin_server_code", "severeWarning", i18n = i18n, ns = ns)
                cat(paste0("\n", now(), " - mod_data - error loading server code - widget_id = ", widget_id, " - ", toString(e)))
              })
          }
        })
        
        r$data_server_loaded <- TRUE
      })
    }
    
    sql_update_datetime <- function(r, m){
      sql <- glue::glue_sql("UPDATE studies SET update_datetime = {now()} WHERE id = {m$selected_study}", .con = r$db)
      sql_send_statement(r$db, sql)
    }
  })
}
