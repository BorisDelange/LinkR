#' patient_and_aggregated_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_data_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  result <- ""
  language <- "EN"
  
  # Prefix depending on page id
  if (id == "patient_level_data"){
    prefix <- "patient_lvl"
    page_name <- "patient_level_data"
  } 
  if (id == "aggregated_data"){
    prefix <- "aggregated"
    page_name <- "aggregated_data"
  }
  
  # --- --- --- --- --- --
  # Tab creation card ----
  # --- --- --- --- --- --
  
  tab_creation_options <- list(
    list(key = "same_level", text = i18n$t("same_level_current_tab")),
    list(key = "level_under", text = i18n$t("level_under"))
  )
  
  tab_creation_card <- make_card(
    title = i18n$t("add_tab"),
    content = div(
      actionButton(ns(paste0(prefix, "_close_add_tab")), "", icon = icon("times"), style = "position:absolute; top:10px; right:10px;"),
      shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
        make_textfield(ns = ns, label = "name", id = "tab_name", width = "300px", i18n = i18n),
        div(shiny.fluent::ChoiceGroup.shinyInput(ns("add_tab_type"), value = "same_level", 
          options = tab_creation_options, className = "inline_choicegroup"), style = "padding-top:35px;")
      ), br(),
      shiny.fluent::PrimaryButton.shinyInput(ns("add_tab_button"), i18n$t("add")), br()
    )
  )
  
  # --- --- --- --- --- -
  # Tab edition card ----
  # --- --- --- --- --- -
  
  tab_edition_card <- make_card(
    title = i18n$t("edit_tab"),
    content = div(
      actionButton(ns(paste0(prefix, "_close_edit_tab")), "", icon = icon("times"), style = "position:absolute; top:10px; right:10px;"),
      make_textfield(ns = ns, label = "name", id = "edit_tab_name", width = "300px", i18n = i18n), br(),
      shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
        shiny.fluent::PrimaryButton.shinyInput(ns("edit_tab_save"), i18n$t("save")),
        shiny.fluent::DefaultButton.shinyInput(ns("remove_tab"), i18n$t("delete_tab"))
      ), 
      br()
    )
  )
  
  # --- --- --- --- --- --- --- --- --- -
  # Widget creation & settings cards ----
  # --- --- --- --- --- --- --- --- --- -
  
  for (type in c("widget_creation", "widget_settings")){
    
    if (type == "widget_creation") plugin_div <- make_combobox(i18n = i18n, ns = ns, label = "plugin", id = paste0(type, "_plugin"), allowFreeform = FALSE, multiSelect = FALSE, width = "300px")
    if (type == "widget_settings") plugin_div <- make_textfield(i18n = i18n, ns = ns, label = "plugin", id = paste0(type, "_plugin"), disabled = TRUE, width = "300px")
    
    widget_card <- make_card(
      title = i18n$t(type),
      content = div(
        actionButton(ns(paste0(prefix, "_close_", type)), "", icon = icon("times"), style = "position:absolute; top:10px; right:10px;"),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
          make_textfield(i18n = i18n, ns = ns, label = "name", id = paste0(type, "_name"), width = "300px"),
          plugin_div
        ),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
          make_combobox(i18n = i18n, ns = ns, label = "vocabulary", id = paste0(type, "_vocabulary"), allowFreeform = FALSE, multiSelect = FALSE, width = "300px"),
          make_dropdown(i18n = i18n, ns = ns, label = "columns_concepts", id = paste0(type, "_vocabulary_concepts_table_cols"), width = "300px", multiSelect = TRUE,
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
            value = c(0, 1, 2, 7, 8, 9)
          ),
          make_dropdown(i18n = i18n, ns = ns, label = "columns_mapped_concepts", id = paste0(type, "_vocabulary_mapped_concepts_table_cols"), width = "300px", multiSelect = TRUE,
            options = list(
              list(key = 1, text = i18n$t("concept_id")),
              list(key = 2, text = i18n$t("relationship_id")),
              list(key = 3, text = i18n$t("mapped_concept_id")),
              list(key = 4, text = i18n$t("concept_name_2")),
              list(key = 5, text = i18n$t("concept_display_name_2")),
              list(key = 6, text = i18n$t("domain_id")),
              list(key = 7, text = i18n$t("num_patients")),
              list(key = 8, text = i18n$t("num_rows")),
              list(key = 9, text = i18n$t("action"))
            ),
            value = c(2, 3, 4, 5, 7, 8, 9)
          )
        ),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 0),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              div(shiny.fluent::Toggle.shinyInput(ns(paste0(type, "_show_mapped_concepts")), value = FALSE), style = "margin-top:30px; margin-bottom:5px;"),
              div(i18n$t("show_mapped_concepts"), style = "font-weight:bold; margin-top:30px;; margin-bottom:5px;")
            ),
            style = "width:330px;"
          ),
          shinyjs::hidden(
            div(
              id = ns("widget_creation_show_mapped_concepts_div"),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                div(shiny.fluent::Toggle.shinyInput(ns(paste0(type, "_merge_mapped_concepts")), value = TRUE), style = "margin-top:30px;; margin-bottom:5px; margin-left:-10px;"),
                div(i18n$t("merge_mapped_concepts"), style = "font-weight:bold; margin-top:30px;; margin-bottom:5px;")
              ),
              style = "width:330px;"
            )
          ),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              div(shiny.fluent::Toggle.shinyInput(ns(paste0(type, "_hide_concepts_datatables")), value = FALSE), style = "margin-top:30px;; margin-bottom:5px; margin-left:-10px;"),
              div(i18n$t("hide_concepts_datatables"), style = "font-weight:bold; margin-top:30px;; margin-bottom:5px;")
            )
          )
        ),
        shiny.fluent::Stack(
          horizontal = TRUE, tokens = list(childrenGap = 20),
          div(
            div(id = ns(paste0(type, "_vocabulary_selected_concepts_title")), class = "input_title", i18n$t("vocabulary_selected_concepts")),
            div(shiny.fluent::Dropdown.shinyInput(ns(paste0(type, "_vocabulary_selected_concepts")), value = NULL, options = list(), multiSelect = TRUE,
              onChanged = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", type, "_vocabulary_selected_concepts_trigger', Math.random())"))), style = "width:620px;")
          ),
          div(shiny.fluent::DefaultButton.shinyInput(ns(paste0(type, "_reset_vocabulary_concepts")), i18n$t("reset")), style = "margin-top:39px;")
        ),
        div(DT::DTOutput(ns(paste0(type, "_vocabulary_concepts"))), class = "vocabulary_table"),
        div(DT::DTOutput(ns(paste0(type, "_vocabulary_mapped_concepts"))), class = "vocabulary_table"),
        div(id = ns(paste0(type, "_blank_space")), br()),
        div(shiny.fluent::PrimaryButton.shinyInput(ns(paste0(type, "_save")), i18n$t(paste0(type, "_save"))))
      )
    )
    
    if (type == "widget_creation") widget_creation_card <- widget_card
    if (type == "widget_settings") widget_settings_card <- widget_card
  }
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    shiny.fluent::reactOutput(ns("tab_delete_confirm")), shiny.fluent::reactOutput(ns("widget_delete_confirm")),
    div(id = ns("initial_breadcrumb"),
      shiny.fluent::Breadcrumb(items = list(
        list(key = "main", text = i18n$t(paste0(prefix, "_data")), href = paste0("#!/", page_name), isCurrentItem = TRUE)),
        maxDisplayedItems = 3)
    ),
    div(
      id = ns("choose_a_study_card"),
      make_card("", div(shiny.fluent::MessageBar(i18n$t("choose_study_and_dataset_left_side"), messageBarType = 5), style = "margin-top:10px;"))
    ),
    shinyjs::hidden(uiOutput(ns("study_menu"))),
    div(id = ns("study_cards")),
    shinyjs::hidden(
      div(
        id = ns(paste0(prefix, "_add_widget")),
        widget_creation_card,
        style = "position:relative;"
      )
    ),
    shinyjs::hidden(
      div(
        id = ns(paste0(prefix, "_widget_settings")),
        widget_settings_card,
        style = "position:relative;"
      )
    ),
    shinyjs::hidden(
      div(
        id = ns(paste0(prefix, "_add_tab")),
        tab_creation_card,
        style = "position:relative;"
      )
    ), 
    shinyjs::hidden(
      div(
        id = ns(paste0(prefix, "_edit_tab")),
        tab_edition_card,
        style = "position:relative;"
      )
    ),
    shinyjs::hidden(
      div(
        id = ns(paste0(prefix, "_no_tabs_to_display")),
        make_card(shiny.fluent::MessageBar(i18n$t("no_tabs_to_display_click_add_tab"), messageBarType = 5))
      )
    ),
    br()
  )
}

#' patient_and_aggregated_data Server Functions
#'
#' @noRd 

mod_data_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), 
  language = "en", i18n = character(), perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (perf_monitoring) monitor_perf(r = r, action = "start")
    if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - start"))
    
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    # --- --- --- --- --- --- -
    # Summary ----
    # --- --- --- --- --- --- -
    
    # - INITIATE VARS
    # - HELP ON THIS PAGE
    # - LOAD DATA
    # - INITIATE UI
    # - LOAD UI
    # --- LOAD UI TABS
    # --- RENDER TABS MENU
    # --- RENDER WIDGETS
    # - LOAD SERVER
    # - OTHER SERVER REACTIVITY
    # --- SORTABLE / CHANGE PIVOTITEMS ORDER
    # --- SHOW / HIDE DIV WHEN PIVOT ITEM SELECTED
    # --- ADD A TAB
    # --- DELETE A TAB
    # --- ADD A WIDGET
    # --- DELETE A WIDGET
    
    # --- --- --- --- --- -
    # Show message bar ----
    # --- --- --- --- --- -
    
    # This allows to show message in multiple pages at the same time (eg when loading a dataset in Studies page, render message bar in Subsets page)
    
    observeEvent(r$show_message_bar, show_message_bar(output, r$show_message_bar$message, r$show_message_bar$type, i18n = i18n, ns = ns))
    if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - show_message_bars"))
    
    # --- --- --- --- --
    # Initiate vars ----
    # --- --- --- --- --
    
    if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - initiate vars"))
    
    # Prefix depending on page id
    if (id == "patient_level_data"){
      prefix <- "patient_lvl"
      page_name <- "patient_level_data"
    } 
    if (id == "aggregated_data"){
      prefix <- "aggregated"
      page_name <- "aggregated_data"
    }
    
    # Initiale variables for help panel & help modal
    
    r[[paste0("help_data_", prefix, "_open_panel")]] <- FALSE
    r[[paste0("help_data_", prefix, "_open_panel_light_dismiss")]] <- TRUE
    r[[paste0("help_data_", prefix, "_open_modal")]] <- FALSE
    r[[paste0("help_data_", prefix, "_modal_text")]] <- div()
    r[[paste0("help_data_", prefix, "_modal_title")]] <- ""
    
    # Initiate var for already loaded studies, so that a UI element is not loaded twice
    r[[paste0(prefix, "_loaded_studies")]] <- integer()
    
    # Initiate var for list of cards
    r[[paste0(prefix, "_cards")]] <- character()
    
    # Load page from header
    
    observeEvent(shiny.router::get_page(), {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer shiny_router::change_page"))
      
      if (prefix == "aggregated" & shiny.router::get_page() == "data" & r$data_page == "patient_level_data") shiny.router::change_page("patient_level_data")
      else if (prefix == "patient_lvl" & shiny.router::get_page() == "data" & r$data_page == "aggregated_data") shiny.router::change_page("aggregated_data")
      
      # Close help pages when page changes
      # r[[paste0("help_data_", prefix, "_open_panel")]] <- FALSE
      # r[[paste0("help_data_", prefix, "_open_modal")]] <- FALSE

      # Refresh reactivity
      shinyjs::hide("study_cards")
      shinyjs::show("study_cards")
      
      # Refresh sortable
      shinyjs::delay(1000, r[[paste0(prefix, "_initialize_sortable")]] <- Sys.time())
    })
    
    if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - initiate vars"))
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    observeEvent(input$help, if (id == shiny.router::get_page()) r[[paste0("help_data_", prefix, "_open_panel")]] <- TRUE)
    observeEvent(input$hide_panel, r[[paste0("help_data_", prefix, "_open_panel")]] <- FALSE)
    
    r[[paste0("help_data_", prefix, "_open_panel_light_dismiss")]] <- TRUE
    observeEvent(input$show_modal, r[[paste0("help_data_", prefix, "_open_modal")]] <- TRUE)
    observeEvent(input$hide_modal, {
      r[[paste0("help_data_", prefix, "_open_modal")]] <- FALSE
      r[[paste0("help_data_", prefix, "_open_panel_light_dismiss")]] <- TRUE
    })
    
    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_data_", prefix, "_page_", i)]] <- Sys.time())
    })
    
    help_data(output = output, r = r, id = id, prefix = prefix, language = language, i18n = i18n, ns = ns)
    
    # --- --- --- --
    # Load data ----
    # --- --- --- --
    
    if (prefix == "patient_lvl"){
      
      observeEvent(m$selected_person, {
        
        if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer m$selected_person"))
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        
        # Reset variables
        
        visit_detail_tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
          "observation", "note", "note_nlp", "fact_relationship", "payer_plan_period", "cost")
        person_tables <- c(visit_detail_tables, "specimen", "death", "drug_era", "dose_era", "condition_era")
        
        sapply(person_tables, function(table) d$data_person[[table]] <- tibble::tibble())
        sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())
        
        if (length(m$selected_person) > 0){
          selected_person <- m$selected_person
          for(table in person_tables) if (d$data_subset[[table]] %>% dplyr::count() %>% dplyr::pull() > 0) d$data_person[[table]] <- 
          d$data_subset[[table]] %>% dplyr::filter(person_id == selected_person)
        }
        
        # Reset selected_visit_detail
        m$selected_visit_detail <- NA_integer_
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer m$selected_person"))
      })
      
      observeEvent(m$selected_visit_detail, {
        
        if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer m$selected_visit_detail"))
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        
        req(d$data_person)
        
        visit_detail_tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
          "observation", "note", "note_nlp", "fact_relationship", "payer_plan_period", "cost")
        sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())
        
        if (length(m$selected_visit_detail) > 0){
          selected_visit_detail <- m$selected_visit_detail
          for(table in visit_detail_tables) if (d$data_person[[table]] %>% dplyr::count() %>% dplyr::pull() > 0) d$data_visit_detail[[table]] <- 
              d$data_person[[table]] %>% dplyr::filter(visit_detail_id == selected_visit_detail)
        }
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer m$selected_visit_detail"))
      })
    }
    
    if (prefix == "aggregated"){
      
      observeEvent(m$subset_persons, {
        
        if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer m$subset_persons"))
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        
        # Reset variables
        
        subset_tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
          "observation", "death", "note", "note_nlp", "specimen", "fact_relationship", "payer_plan_period", "cost", 
          "drug_era", "dose_era", "condition_era", "person", "observation_period", "visit_occurrence", "visit_detail")
        
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
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer m$subset_persons"))
      })
    }
    
    # --- --- --- -- -
    # Initiate UI ----
    # --- --- --- -- -
    
    # When a study is selected
    
    observeEvent(m$selected_study, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer m$selected_study"))
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      
      # Hide UI from previous loaded study
      # Don't use removeUI, cause when you switch study, it is deleted and cannot be reinserted
      
      sapply(r[[paste0(prefix, "_cards")]], shinyjs::hide)
      
      req(!is.na(m$selected_study))
      
      # removeUI(selector = paste0("#", ns(r[[paste0(prefix, "_cards")]])), multiple = TRUE)
      
      # Initiiate vector of study cards
      # r[[paste0(prefix, "_cards")]] <- character()
      
      # Hide "choose a study" card
      shinyjs::hide("choose_a_study_card")
      shinyjs::hide("initial_breadcrumb")
      shinyjs::show("study_menu")
      
      # Reset selected key
      r[[paste0(prefix, "_selected_tab")]] <- NA_integer_
      
      # Reset shown tabs
      r[[paste0(prefix, "_opened_cards")]] <- ""
      
      # Hide Add widget card & Add tab
      shinyjs::hide("add_widget")
      shinyjs::hide("add_tab")
      
      r[[paste0(prefix, "_load_display_tabs")]] <- paste0("first_load_ui_", Sys.time())
      
      # Run observers
      r[[paste0(prefix, "_load_server")]] <- Sys.time()
      
      r[[paste0(prefix, "_load_ui_stage")]] <- "first_time"
      
      # Load tabs variables for this study
      update_r(r = r, m = m, table = paste0(prefix, "_tabs_groups"))
      update_r(r = r, m = m, table = paste0(prefix, "_tabs"))
      update_r(r = r, m = m, table = paste0(prefix, "_widgets"))
      update_r(r = r, m = m, table = paste0(prefix, "_widgets_concepts"))
      
      # Reload create widget fields
      
      ## Reload vocabulary datatable & selected_concepts
      if (length(r[[paste0(prefix, "_widget_creation_vocabulary_concepts")]]) > 0) r[[paste0(prefix, "_widget_creation_vocabulary_concepts")]] <- r[[paste0(prefix, "_widget_creation_vocabulary_concepts")]] %>% dplyr::slice(0)
      
      if (length(r[[paste0(prefix, "_widget_creation_vocabulary_selected_concepts")]]) > 0) r[[paste0(prefix, "_widget_creation_vocabulary_selected_concepts")]] <-
        r[[paste0(prefix, "_widget_creation_vocabulary_selected_concepts")]] %>% dplyr::slice(0)
      shiny.fluent::updateDropdown.shinyInput(session, "widget_creation_vocabulary_selected_concepts", options = list(), value = NULL)
      
      ## Reload other fields
      shiny.fluent::updateTextField.shinyInput(session, "widget_creation_name", value = "")
      shiny.fluent::updateDropdown.shinyInput(session, "widget_creation_vocabulary_mapping", 
        options = list(list(key = 1, text = i18n$t("equivalent_to")), list(key = 2, text = i18n$t("included_in")), list(key = 3, text = i18n$t("include"))),
        value = NULL)
      
      r[[paste0(prefix, "_reload_plugins_dropdown")]] <- Sys.time()
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer m$selected_study"))
    })
    
    # Reload plugins dropdown
    
    observeEvent(r$plugins, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer r$plugins"))
      r[[paste0(prefix, "_reload_plugins_dropdown")]] <- Sys.time()
    })
    
    observeEvent(r[[paste0(prefix, "_reload_plugins_dropdown")]], {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer r$..reload_plugins_dropdown"))
      
      tab_type_id <- switch(prefix, "patient_lvl" = 1, "aggregated" = 2)
      
      plugins <- r$plugins %>% dplyr::filter(tab_type_id == !!tab_type_id)
      
      options <- convert_tibble_to_list(data = plugins %>% dplyr::arrange(name), key_col = "id", text_col = "name", i18n = i18n)
      shiny.fluent::updateComboBox.shinyInput(session, "widget_creation_plugin", options = options, value = NULL)
    })
    
    # Load study display tabs
    
    observeEvent(r[[paste0(prefix, "_load_display_tabs")]], {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer r$..load_display_tabs"))
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      
      # Load study informations
      # For one study, you choose ONE patient_lvl or aggregated data tab family
      study_infos <- r$studies %>% dplyr::filter(id == m$selected_study)
      # study_infos <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM studies WHERE id = ", m$selected_study))
      
      # Check if users has access only to aggregated data
      r$options %>% dplyr::filter(category == "dataset" & link_id == r$selected_dataset & name == "show_only_aggregated_data") %>%
        dplyr::pull(value_num) -> r[[paste0(prefix, "_show_only_aggregated_data")]]
      
      # Load tabs belonging to this tab family
      # display_tabs <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM ", prefix, "_tabs WHERE tab_group_id = ",
      #   study_infos[[paste0(prefix, "_tab_group_id")]], " AND deleted IS FALSE"))
      display_tabs <- r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(tab_group_id == study_infos[[paste0(prefix, "_tab_group_id")]]) 
      
      # Tabs without parent are set to level 1
      display_tabs <- display_tabs %>% 
        dplyr::mutate(level = dplyr::case_when(is.na(parent_tab_id) ~ 1L, TRUE ~ NA_integer_))
      
      # Prevent infinite loop, max loops = 7
      i <- 1
      
      # Creating levels for distinct tabs
      while(nrow(display_tabs %>% dplyr::filter(is.na(level))) > 0 & i <= 7){
        display_tabs <-
          display_tabs %>%
          dplyr::left_join(display_tabs %>%
              dplyr::filter(!is.na(level)) %>%
              dplyr::transmute(parent_tab_id = id, parent_level = level), by = "parent_tab_id") %>%
          dplyr::mutate(level = dplyr::case_when(!is.na(parent_level) ~ parent_level + 1L, TRUE ~ level)) %>%
          dplyr::select(-parent_level)
        i <- i + 1
      }
      
      # Exclude tabs without level
      display_tabs <- display_tabs %>% dplyr::filter(!is.na(level))
      
      # Order by display order
      display_tabs <- display_tabs %>% dplyr::arrange(level, display_order)
      
      # Calculate first tab shown in the menu
      if(nrow(display_tabs) > 0 & "level" %in% names(display_tabs) & !is.na(m$selected_study)){
        
        # First tab shown
        first_tab_shown <- display_tabs %>% dplyr::filter(level == 1) %>% dplyr::slice(1)
        if (max(display_tabs$level) >= 2){
          for(current_level in 2:max(display_tabs$level)){
            children <- display_tabs %>% dplyr::filter(level == current_level, parent_tab_id == first_tab_shown$id) %>% dplyr::slice(1)
            if (nrow(children) > 0) first_tab_shown <- children
          }
        }
        
        r[[paste0(prefix, "_first_tab_shown")]] <- first_tab_shown
      }
      
      r[[paste0(prefix, "_display_tabs")]] <- display_tabs %>%
        dplyr::group_by(level, parent_tab_id) %>%
        dplyr::summarize(id = id, tab_group_id = tab_group_id, tab_sub_group = dplyr::cur_group_id(), parent_tab_id = parent_tab_id,
          name = name, display_order = display_order, level = level) %>%
        dplyr::ungroup()
      
      # Reload menu
      r[[paste0(prefix, "_load_ui_menu")]] <- Sys.time()
      
      # Load UI cards
      if (grepl("first_load_ui", r[[paste0(prefix, "_load_display_tabs")]])) r[[paste0(prefix, "_load_ui_cards")]] <- Sys.time()
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer r$..load_display_tabs"))
    })
    
    # --- --- -- -
    # Load UI ----
    # --- --- -- -
    
    # --- --- --- --- --- --
    ## Render tabs menu ----
    # --- --- --- --- --- --
    
    # Render menu
    
    r[[paste0(prefix, "initialize_sortable_vars")]] <- character()
    
    observeEvent(r[[paste0(prefix, "_load_ui_menu")]], {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - r$..load_ui_menu"))
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      
      tab_group_id <- r[[paste0(prefix, "_display_tabs")]] %>% dplyr::slice(1) %>% dplyr::pull(tab_group_id)
      
      # Create an ID per level / sub_group
      display_tabs <- r[[paste0(prefix, "_display_tabs")]]
      
      r[[paste0(prefix, "_initialize_sortable_ids")]] <- character()
      
      if (nrow(display_tabs) > 0){
        
        shinyjs::hide(paste0(prefix, "_no_tabs_to_display"))
        
        study_first_tab_id <- display_tabs %>% dplyr::filter(level == 1) %>% dplyr::slice(1) %>% dplyr::pull(id)
        
        # Load Breadcrumb(s) & Pivot(s), one per level / subgroup
        
        breadcrumbs <- tagList()
        pivots <- tagList()
        # sortable_code <- tagList()
        
        i <- 1L
        
        for (tab_sub_group in unique(display_tabs$tab_sub_group)){
          
          tabs <- display_tabs %>% dplyr::filter(tab_sub_group == !!tab_sub_group)
          tabs_ui <- tagList()
          
          for (j in 1:nrow(tabs)){
            tab <- tabs[j, ]
            tabs_ui <- tagList(tabs_ui, 
              shiny.fluent::PivotItem(id = tab$id, itemKey = tab$id, headerText = tab$name)
            )
            
            if (i == 1 & j == 1) if (is.na(r[[paste0(prefix, "_selected_tab")]])) r[[paste0(prefix, "_selected_tab")]] <- tab$id
          }
          
          pivot_id <- paste0(prefix, "_study_pivot_", tab_group_id, "_", tab_sub_group)
          pivot_sub_div_id <- paste0(prefix, "_study_pivot_sub_div_", tab_group_id, "_", tab_sub_group)
          
          r[[paste0(prefix, "_initialize_sortable_ids")]] <- c(r[[paste0(prefix, "_initialize_sortable_ids")]], pivot_sub_div_id)
          
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
                defaultSelectedKey = r[[paste0(prefix, "_selected_tab")]]
              )
            )
          
          if (is.na(r[[paste0(prefix, "_selected_tab")]]) & i > 1) pivot <- shinyjs::hidden(pivot)
          if (!is.na(r[[paste0(prefix, "_selected_tab")]]) & r[[paste0(prefix, "_selected_tab")]] %not_in% tabs$id) pivot <- shinyjs::hidden(pivot)
          pivot_tabs <- paste0(prefix, "_pivot_tabs_", tab_group_id, "_", tab_sub_group)
          
          pivots <- tagList(pivots, pivot,
            # A script to use sortable with PivotItems
            tags$script(HTML(paste0(
              'Shiny.addCustomMessageHandler("initialize_sortable_', pivot_sub_div_id, '", function(message) {',
              # 'Shiny.addCustomMessageHandler("initialize_sortable", function(message) {',
              '  var pivotElement = $("#', id, '-', pivot_sub_div_id, '").children("div:first");',
              '  pivotElement.attr("id", "', pivot_tabs, '");',
              '  if (window.Sortable && pivotElement.length > 0) {',
              '    new Sortable(pivotElement[0], {',
              '      onUpdate: function(evt) {',
              '        var order = [];',
              '        $(evt.from).children().each(function() {', 
              '          order.push($(this).text());',
              '        });',
              '        Shiny.setInputValue("', id, '-study_pivot_order", order);',
              '      }',
              '    });',
              '  }',
              '});'
            ))),
            sortable::sortable_js(pivot_tabs)#, options = sortable::sortable_options(onUpdate =
              # htmlwidgets::JS(paste0("function(evt) { Shiny.setInputValue('", id, "-study_pivot_order', evt.from.innerText);}"))))
          )
          
          tab_sub_group_first_tab <- display_tabs %>% dplyr::filter(tab_sub_group == !!tab_sub_group) %>% dplyr::arrange(display_order) %>% dplyr::slice(1)
          nb_levels <- tab_sub_group_first_tab %>% dplyr::slice(1) %>% dplyr::pull(level)
          
          tabs_tree <- tibble::tibble()
          
          if (nb_levels >= 2){
            
            is_current_item <- FALSE
            
            for (level in nb_levels:2){
              
              if (level == nb_levels) parent_tab <- display_tabs %>% dplyr::filter(level == !!level - 1, id == tab_sub_group_first_tab$parent_tab_id)
              else parent_tab <- display_tabs %>% dplyr::filter(level == !!level - 1, id == parent_tab$parent_tab_id)
              
              if (level == nb_levels) tabs_tree <- parent_tab
              else tabs_tree <- tabs_tree %>% dplyr::bind_rows(parent_tab)
            }
          }
          
          if (nrow(tabs_tree) > 0) tabs_tree <- tabs_tree %>% dplyr::arrange(level)
          
          if (nb_levels == 1) is_current_item <- TRUE else is_current_item <- FALSE
          
          first_list_element <- list(key = "main", text = i18n$t(page_name), href = paste0("#!/", page_name), isCurrentItem = FALSE,
            onClick = htmlwidgets::JS(paste0("item => {",
              "Shiny.setInputValue('", id, "-study_go_to_tab', ", study_first_tab_id, ");",
              "Shiny.setInputValue('", id, "-study_go_to_tab_trigger', Math.random());",
              "}"
            )))
          
          breadcrumb_list <- list(first_list_element)
          
          if (nb_levels >= 2){
            
            for (j in 1:nrow(tabs_tree)){
              
              row <- tabs_tree[j, ]
              
              if (row$level == nb_levels - 1) is_current_item <- TRUE
              else is_current_item <- FALSE
              
              breadcrumb_list <- rlist::list.append(breadcrumb_list, list(
                key = "main", text = row$name, href = paste0("#!/", page_name), isCurrentItem = is_current_item,
                onClick = htmlwidgets::JS(paste0("item => {",
                  "Shiny.setInputValue('", id, "-study_go_to_tab', ", row$id, ");",
                  "Shiny.setInputValue('", id, "-study_go_to_tab_trigger', Math.random());",
                  "}"
                ))
              ))
            }
          }
          
          breadcrumb <- div(
            id = ns(paste0(prefix, "_study_breadcrumb_", tab_group_id, "_", tab_sub_group)),
            shiny.fluent::Breadcrumb(items = breadcrumb_list, maxDisplayedItems = 3)
          )
          
          if (is.na(r[[paste0(prefix, "_selected_tab")]]) & i > 1) breadcrumb <- shinyjs::hidden(breadcrumb)
          if (!is.na(r[[paste0(prefix, "_selected_tab")]]) & r[[paste0(prefix, "_selected_tab")]] %not_in% tabs$id)  breadcrumb <- shinyjs::hidden(breadcrumb)
          
          breadcrumbs <- tagList(breadcrumbs, breadcrumb)
          
          i <- 2L
        }
      }
      
      if (nrow(display_tabs) == 0){
        
        breadcrumbs <- div(
          id = ns(paste0(prefix, "_study_breadcrumb_", tab_group_id, "_0")),
          shiny.fluent::Breadcrumb(items = list(list(key = "main", text = i18n$t(page_name), href = paste0("#!/", page_name), isCurrentItem = FALSE,
            onClick = htmlwidgets::JS(paste0("item => {",
              "Shiny.setInputValue('", id, "-study_go_to_tab', 0);",
              "Shiny.setInputValue('", id, "-study_go_to_tab_trigger', Math.random());",
              "}"
            )))), maxDisplayedItems = 3)
        )
        
        pivots <- shiny.fluent::Pivot(
          id = ns(paste0(prefix, "_study_pivot_", tab_group_id, "_0")),
          onLinkClick = htmlwidgets::JS(paste0("item => {",
            "Shiny.setInputValue('", id, "-study_current_tab', item.props.id);",
            "Shiny.setInputValue('", id, "-study_current_tab_trigger', Math.random());",
            "}"
          )),
          defaultSelectedKey = r[[paste0(prefix, "_selected_tab")]]
        )
        
        shinyjs::show(paste0(prefix, "_no_tabs_to_display"))
      }
      
      output$study_menu <- renderUI(tagList(
        breadcrumbs,
        div(
          pivots,
          shiny.fluent::Pivot(
            shiny.fluent::PivotItem(id = paste0(prefix, "_add_tab"), headerText = span(i18n$t("add_tab"), style = "padding-left:5px;"), itemIcon = "Add"),
            onLinkClick = htmlwidgets::JS(paste0("item => {",
              "Shiny.setInputValue('", id, "-study_add_new_tab', Math.random());",
              "}"
            )),
            selectedKey = NULL
          ),
          style = "display:flex;"
        )
      ))
      
      r[[paste0(prefix, "_hide_ui")]] <- Sys.time()
      
      shinyjs::delay(2000, r[[paste0(prefix, "_initialize_sortable")]] <- Sys.time())
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - r$..display_tabs"))
    })
    
    observeEvent(r[[paste0(prefix, "_initialize_sortable")]], {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer r$..initialize_sortable"))
      invalidateLater(1000)
      # session$sendCustomMessage(type = paste0(prefix, "_initialize_sortable"), message = paste0("init_", prefix))
      # session$sendCustomMessage(type = "initialize_sortable", message = paste0("init_", prefix))
      
      for (pivot_sub_div_id in r[[paste0(prefix, "_initialize_sortable_ids")]]) session$sendCustomMessage(type = paste0("initialize_sortable_", pivot_sub_div_id), message = "init")
    })
    
    # shinyjs::hidden doesn't work, so delay shinyjs::hide to hide pivots
    # observeEvent(r[[paste0(prefix, "_hide_ui")]], {
    #   if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer r$..hide_ui"))
    #   
    #   display_tabs <- r[[paste0(prefix, "_display_tabs")]]
    #   selected_tab <- r[[paste0(prefix, "_selected_tab")]]
    #   
    #   tab_group_id <- display_tabs %>% dplyr::slice(1) %>% dplyr::pull(tab_group_id)
    #   
    #   if (!is.na(selected_tab)) selected_tab_sub_group <- display_tabs %>% dplyr::filter(id == selected_tab) %>% dplyr::slice(1) %>% dplyr::pull(tab_sub_group)
    #   else selected_tab_sub_group <- 1
    #   
    #   sapply(c("pivot", "breadcrumb"), function(name){
    #     shinyjs::delay(100,
    #       for (tab_sub_group in unique(display_tabs$tab_sub_group)){
    #         if (tab_sub_group != selected_tab_sub_group) shinyjs::hide(paste0(prefix, "_study_", name, "_", tab_group_id, "_", tab_sub_group))
    #       }
    #     )
    #   })
    # })
    
    observeEvent(input$study_current_tab_trigger, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$study_current_tab_trigger"))
      
      if (!grepl("add_tab", input$study_current_tab)) r[[paste0(prefix, "_selected_tab")]] <- input$study_current_tab
      
      current_tab <- r[[paste0(prefix, "_display_tabs")]] %>% dplyr::filter(id == input$study_current_tab)
      children_tabs <- r[[paste0(prefix, "_display_tabs")]] %>% dplyr::filter(parent_tab_id == input$study_current_tab)
      
      # If current tab has children, load children
      if (nrow(children_tabs) > 0){
        first_child <- children_tabs %>% dplyr::arrange(display_order) %>% dplyr::slice(1)
        r[[paste0(prefix, "_selected_tab")]] <- first_child$id
        
        for (name in c("pivot", "breadcrumb")){
          shinyjs::hide(paste0(prefix, "_study_", name, "_", current_tab$tab_group_id, "_", current_tab$tab_sub_group)) 
          shinyjs::show(paste0(prefix, "_study_", name, "_", first_child$tab_group_id, "_", first_child$tab_sub_group))
        }
      }
    })
    
    observeEvent(input$study_go_to_tab_trigger, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$study_go_to_tab"))
      
      r[[paste0(prefix, "_selected_tab")]] <- input$study_go_to_tab
      r[[paste0(prefix, "_load_ui_menu")]] <- Sys.time()
    })
    
    # If r$studies changes, hide study_menu
    observeEvent(r$studies, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer r$studies"))
      
      shinyjs::show("initial_breadcrumb")
      shinyjs::show("choose_a_study_card")
      shinyjs::hide("study_menu")
      sapply(r[[paste0(prefix, "_cards")]], shinyjs::hide)
    })
    
    # --- --- --- --- -- -
    ## Render widgets ----
    # --- --- --- --- -- -
    
    observeEvent(r[[paste0(prefix, "_load_ui_cards")]], {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer r$..load_ui_cards"))
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      
      # Don't reload study UI if already loaded
      req(m$selected_study %not_in% r[[paste0(prefix, "_loaded_studies")]])
      
      distinct_tabs <- r[[paste0(prefix, "_display_tabs")]] %>% dplyr::pull(id)
      
      code_ui <- tagList("")
      
      all_groups <- NA_integer_
      
      # Loop over distinct tabs, for this study
      
      selected_tab <- r[[paste0(prefix, "_selected_tab")]]
      
      if (grepl("show_tab", selected_tab)) selected_tab <- as.integer(substr(selected_tab, nchar("show_tab_") + 1, 100))
      
      sapply(distinct_tabs, function(tab_id){
        
        toggles <- tagList()
        
        widgets <- r[[paste0(prefix, "_widgets")]] %>% dplyr::filter(tab_id == !!tab_id) %>% 
          dplyr::rename(widget_id = id) %>% dplyr::arrange(display_order)
        
        if (nrow(widgets) > 0){
          
          # Load widgets concepts
          widgets_concepts <- r[[paste0(prefix, "_widgets_concepts")]] %>% dplyr::inner_join(widgets %>% dplyr::select(widget_id), by = "widget_id")
          
          # Get widget ID
          distinct_widgets <- unique(widgets$widget_id)
          
          # Loop over distinct cards (tabs elements), for this tab
          
          # Use sapply instead of for loop, cause with for loop, widget_id doesn't change
          sapply(distinct_widgets, function(widget_id){
            
            # Load selected concepts
            selected_concepts <- widgets_concepts %>% dplyr::filter(widget_id == !!widget_id) %>%
              dplyr::select(concept_id, concept_name, concept_display_name, domain_id, mapped_to_concept_id, merge_mapped_concepts)
            
            # Load UI code for this widget
            plugin_id <- widgets %>% dplyr::filter(widget_id == !!widget_id) %>% dplyr::slice(1) %>% dplyr::pull(plugin_id)
            # if (length(plugin_id) != 0) code_ui_card <- r$code %>% dplyr::filter(link_id == plugin_id, category == "plugin_ui") %>% dplyr::pull(code)
            
            # Check if plugin has been deleted
            # check_deleted_plugin <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM plugins WHERE id = ", plugin_id)) %>% dplyr::pull(deleted)
            check_deleted_plugin <- r$plugins %>% dplyr::filter(id == plugin_id)
            if (nrow(check_deleted_plugin) == 0){
              code_ui_card <- paste0("div(shiny.fluent::MessageBar('", i18n$t("plugin_deleted"), "', messageBarType = 5), style = 'margin-top:10px;')")
              settings_widget_button <- ""
            }
            if (nrow(check_deleted_plugin) > 0){
              
              code_ui_card <- r$code %>% dplyr::filter(link_id == plugin_id, category == "plugin_ui") %>% dplyr::pull(code)
              settings_widget_button <- actionButton(ns(paste0(prefix, "_widget_settings_", widget_id)), "", icon = icon("cog"))
              
              # Create translations file & var
              
              plugin_translations <- r$code %>% dplyr::filter(link_id == plugin_id, category == "plugin_translations") %>% dplyr::pull(code)
              
              if (plugin_translations != ""){
                
                tryCatch({
                  # Get plugin unique_id
                  plugin_unique_id <- r$options %>% dplyr::filter(category == "plugin", name == "unique_id", link_id == plugin_id) %>% dplyr::pull(value)
                  
                  # Create plugin folder in translations folder if doesn't exist
                  new_dir <- paste0(r$app_folder, "/translations/", plugin_unique_id)
                  if (!dir.exists(new_dir)) dir.create(new_dir)
                  
                  # Create a csv with all languages
                  data <- read.csv(text = plugin_translations, header = TRUE, stringsAsFactors = FALSE)
                  
                  # Create one csv by language
                  for(lang in names(data)[-1]){
                    file_name <- paste0(new_dir, "/translation_", lang, ".csv")
                    
                    if (!file.exists(file_name)){
                      # Create a new dataframe with base & current language cols
                      data_lang <- data[, c("base", lang)]
                      
                      # Create csv
                      write.csv(data_lang, file_name, row.names = FALSE)
                    }
                  }
                },
                  error = function(e) report_bug(r = r, output = output, error_message = "error_creating_translations_file",
                    error_name = paste0(id, " - create translations files - plugin_id ", plugin_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
                
                tryCatch({
                  i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = new_dir))
                  i18np$set_translation_language(language)},
                  error = function(e) report_bug(r = r, output = output, error_message = "error_creating_new_translator",
                    error_name = paste0(id, " - create i18np translator - plugin_id ", plugin_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
              }
            }
            
            # Get name of widget
            widget_name <- widgets %>% dplyr::filter(widget_id == !!widget_id) %>% dplyr::slice(1) %>% dplyr::pull(name)
            
            # Append a toggle to our cards list
            r[[paste0(prefix, "_cards")]] <- c(r[[paste0(prefix, "_cards")]], paste0(prefix, "_widget_", widget_id))
            
            toggles <<- tagList(toggles,
              shiny.fluent::Toggle.shinyInput(ns(paste0(prefix, "_widget_", widget_id, "_toggle")), value = TRUE, style = "margin-top:10px;"),
              div(class = "toggle_title", widget_name, style = "padding-top:10px;"))
            
            # Try to run plugin UI code
            # ID of UI element is in the following format : "group_[ID]"
            # tryCatch({
            code_ui_card <- code_ui_card %>%
              stringr::str_replace_all("%tab_id%", as.character(tab_id)) %>%
              stringr::str_replace_all("%widget_id%", as.character(widget_id)) %>%
              stringr::str_replace_all("%req%", "req(m[[session_code]] == session_num)\nreq(m$selected_study == %study_id%)") %>%
              stringr::str_replace_all("\r", "\n") %>%
              stringr::str_replace_all("''", "'")
            
            if (length(m$selected_study) > 0) code_ui_card <- code_ui_card %>% stringr::str_replace_all("%study_id%", as.character(m$selected_study))
            
            # Widget card
            
            code_ui <- tryCatch(eval(parse(text = code_ui_card)), error = function(e){
              report_bug(r = r, output = output, error_message = "error_run_plugin_ui_code",
                error_name = paste0(id, " - run ui code - ", widget_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns)
              p(toString(e))
            }, warning = function(w){
              report_bug(r = r, output = output, error_message = "error_run_plugin_ui_code",
                error_name = paste0(id, " - run ui code - ", widget_id), category = "Error", error_report = toString(w), i18n = i18n, ns = ns)
              p(toString(w))
            })
            
            element_code <- div(
              make_shiny_ace_card("",
                div(
                  div(id = ns(paste0(prefix, "_widget_plugin_ui_", widget_id)), code_ui),
                  div(
                    id = ns(paste0(prefix, "_widget_settings_remove_buttons_", widget_id)),
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 2),
                      settings_widget_button,
                      actionButton(ns(paste0(prefix, "_remove_widget_", widget_id)), "", icon = icon("trash-alt"))
                    ),
                    style = "position:absolute; top:8px; right: 10px;"
                  )
                ),
                style = "position:relative;"
              )
            )
            
            ui_output <- uiOutput(ns(paste0(prefix, "_widget_", widget_id)))
            hide_div <- TRUE
            if (!is.na(selected_tab)) if (tab_id == selected_tab) hide_div <- FALSE
            if (hide_div) ui_output <- shinyjs::hidden(ui_output)
            
            insertUI(selector = paste0("#", ns("study_cards")), where = "beforeEnd", ui = ui_output)
            output[[paste0(prefix, "_widget_", widget_id)]] <- renderUI(element_code)
          })
        }
        
        # Put all div together
        
        r[[paste0(prefix, "_cards")]] <- c(r[[paste0(prefix, "_cards")]], paste0(prefix, "_toggles_", tab_id))
        
        # Does this tab have sub-tabs ?
        if (r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(parent_tab_id == tab_id) %>% nrow() > 0) toggles_div <- div(
          make_card("",
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_edit_tab_", tab_id)), i18n$t("edit_tab"), iconProps = list(iconName = "Edit"),
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-edit_tab_trigger', Math.random())"))),
              div(shiny.fluent::MessageBar(i18n$t("tab_contains_sub_tabs"), messageBarType = 5), style = "margin-top:4px;")
            )
          )
        )
        
        else toggles_div <- div(
          make_card("",
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_widget_", tab_id)), i18n$t("new_widget"), iconProps = list(iconName = "Add"),
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_add_widget_trigger', Math.random())"))),
              shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_edit_tab_", tab_id)), i18n$t("edit_tab"), iconProps = list(iconName = "Edit"),
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-edit_tab_trigger', Math.random())"))),
              div(style = "width:20px;"),
              toggles
            )
          )
        )
        
        ui_output <- uiOutput(ns(paste0(prefix, "_toggles_", tab_id)))
        hide_div <- TRUE
        if (!is.na(selected_tab)) if (tab_id == selected_tab) hide_div <- FALSE
        if (hide_div) ui_output <- shinyjs::hidden(ui_output)
        
        insertUI(selector = paste0("#", ns("study_cards")), where = "afterBegin", ui = ui_output)
        output[[paste0(prefix, "_toggles_", tab_id)]] <- renderUI(toggles_div)
        
      })
      
      # Indicate that this study has been loaded, so that UI elements aren't loaded twice
      r[[paste0(prefix, "_loaded_studies")]] <- c(r[[paste0(prefix, "_loaded_studies")]], m$selected_study)
      
      # Reload UI menu (problem for displaying cards : blanks if we do not do that)
      # shinyjs::delay(100, r[[paste0(prefix, "_load_ui_menu")]] <- Sys.time())
      # r[[paste0(prefix, "_load_ui_menu")]] <- Sys.time()
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer r$..load_ui_cards"))
    })
    
    # --- --- --- --- --- ---
    # Close creation div ----
    # --- --- --- --- --- ---
    
    observeEvent(input[[paste0(prefix, "_close_widget_creation")]], {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$..close_add_widget"))
      
      # Show opened cards before opening Add widget div
      sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)
      
      # Hide Add widget div
      shinyjs::hide(paste0(prefix, "_add_widget"))
    })
    
    # --- --- --- -- -
    # Load server ----
    # --- --- --- -- -
    
    observeEvent(r[[paste0(prefix, "_load_server")]], {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer r$..load_server"))
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      
      req(!is.na(m$selected_study))
      
      # Get tabs elements, arrange them by display_order
      
      tab_group <- r$studies %>% dplyr::filter(id == m$selected_study) %>% dplyr::pull(paste0(prefix, "_tab_group_id"))
      tabs <- r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(tab_group_id == tab_group) %>% dplyr::select(tab_id = id)
      widgets <- r[[paste0(prefix, "_widgets")]] %>% dplyr::inner_join(tabs, by = "tab_id") %>% dplyr::rename(widget_id = id)
      widgets_concepts <- r[[paste0(prefix, "_widgets_concepts")]] %>% dplyr::inner_join(widgets %>% dplyr::select(widget_id), by = "widget_id")
      
      # --- --- --- --- --- --- --- ---
      ## Run server code for cards ----
      # --- --- --- --- --- --- --- ---
      
      if (nrow(widgets) > 0){
        
        # Get widget widget_id
        distinct_widgets <- unique(widgets$widget_id)
        
        toggles <- c()
        
        # Loop over distinct cards
        sapply(distinct_widgets, function(widget_id){
          
          # Run plugin server code
          # Only if this code has not been already loaded
          trace_code <- paste0(prefix, "_", widget_id, "_", m$selected_study)
          # if (trace_code %in% r$server_tabs_groups_loaded) print(trace_code)
          if (trace_code %not_in% r$server_tabs_groups_loaded){
            
            # Add the trace_code to loaded plugins list
            r$server_tabs_groups_loaded <- c(r$server_tabs_groups_loaded, trace_code)
            
            # Server code for toggles reactivity
            toggle <- paste0(prefix, "_widget_", widget_id)
            observeEvent(input[[paste0(toggle, "_toggle")]], {
              req(r[[paste0(prefix, "_selected_tab")]] == widgets %>% dplyr::filter(widget_id == !!widget_id) %>% dplyr::distinct(tab_id) %>% dplyr::pull())
              if (input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle) else shinyjs::hide(toggle)
            })
            
            selected_concepts <- widgets_concepts %>% dplyr::filter(widget_id == !!widget_id) %>%
              dplyr::select(concept_id, concept_name, concept_display_name, domain_id,
                mapped_to_concept_id, merge_mapped_concepts)
            
            # Get plugin code
            
            ids <- widgets %>% dplyr::filter(widget_id == !!widget_id) %>% dplyr::slice(1) %>% dplyr::select(plugin_id, tab_id)
            
            # Check if plugin has been deleted
            check_deleted_plugin <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM plugins WHERE id = ", ids$plugin_id)) %>% dplyr::pull(deleted)
            if (!check_deleted_plugin){
              
              code_server_card <- r$code %>%
                dplyr::filter(link_id == ids$plugin_id, category == "plugin_server") %>%
                dplyr::pull(code) %>%
                stringr::str_replace_all("%tab_id%", as.character(ids$tab_id)) %>%
                stringr::str_replace_all("%widget_id%", as.character(widget_id)) %>%
                stringr::str_replace_all("%req%", "req(m[[session_code]] == session_num)\nreq(m$selected_study == %study_id%)") %>%
                stringr::str_replace_all("%study_id%", as.character(m$selected_study)) %>%
                stringr::str_replace_all("\r", "\n") %>%
                stringr::str_replace_all("''", "'")
              
              # If it is an aggregated plugin, change %study_id% with current selected study
              if (length(m$selected_study) > 0) code_server_card <- code_server_card %>% stringr::str_replace_all("%study_id%", as.character(m$selected_study))
            }
            else code_server_card <- ""
            
            # Create translations file & var
            if (!check_deleted_plugin){
              
              plugin_translations <- r$code %>%
                dplyr::filter(link_id == ids$plugin_id, category == "plugin_translations") %>%
                dplyr::pull(code)
              
              if (plugin_translations != ""){
                
                tryCatch({
                  # Get plugin unique_id
                  plugin_unique_id <- r$options %>% dplyr::filter(category == "plugin", name == "unique_id", link_id == !!ids$plugin_id) %>% dplyr::pull(value)
                  
                  # Create plugin folder in translations folder if doesn't exist
                  new_dir <- paste0(r$app_folder, "/translations/", plugin_unique_id)
                  if (!dir.exists(new_dir)) dir.create(new_dir)
                  
                  # Create a csv with all languages
                  data <- read.csv(text = plugin_translations, header = TRUE, stringsAsFactors = FALSE)
                  
                  # Create one csv by language
                  for(lang in names(data)[-1]){
                    file_name <- paste0(new_dir, "/translation_", lang, ".csv")
                    
                    if (!file.exists(file_name)){
                      # Create a new dataframe with base & current language cols
                      data_lang <- data[, c("base", lang)]
                      
                      # Create csv
                      write.csv(data_lang, file_name, row.names = FALSE)
                    }
                  }
                },
                  error = function(e) report_bug(r = r, output = output, error_message = "error_creating_translations_file",
                    error_name = paste0(id, " - create translations files - plugin_id ", ids$plugin_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
                
                tryCatch({
                  i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = new_dir))
                  i18np$set_translation_language(language)},
                  error = function(e) report_bug(r = r, output = output, error_message = "error_creating_new_translator",
                    error_name = paste0(id, " - create i18np translator - plugin_id ", ids$plugin_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
              }
            }
            
            # Create a session number, to inactivate older observers
            # Reset all older observers for this widget_id
            
            session_code <- paste0(prefix, "_widget_", widget_id)
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
            tryCatch(eval(parse(text = code_server_card), envir = new_env),
              error = function(e) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code",
                error_name = paste0(id, " - run_study_server_code - run_plugin_code - plugin_id ", ids$plugin_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
            
            # --- --- --- --- --- ---
            #### Delete a widget ----
            # --- --- --- --- --- ---
            
            observeEvent(input[[paste0(prefix, "_remove_widget_", widget_id)]], {
              r[[paste0(prefix, "_selected_widget")]] <- widget_id
              r[[widget_delete_variable]] <- TRUE
            })
            
            # --- --- --- --- --- ---
            #### Widget settings ----
            # --- --- --- --- --- ---
            
            observeEvent(input[[paste0(prefix, "_widget_settings_", widget_id)]], {
              r[[paste0(prefix, "_widget_settings_trigger")]] <- Sys.time()
              r[[paste0(prefix, "_widget_settings")]] <- widget_id
            })
          }
        })
      }
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer r$..load_server"))
    })
    
    # --- --- --- --- --- --- -- -
    # Other server reactivity ----
    # --- --- --- --- --- --- -- -
    
    # --- --- --- --- -- -
    ## Widget settings ----
    # --- --- --- --- -- -
    
    observeEvent(r[[paste0(prefix, "_widget_settings_trigger")]], {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer r$..widget_settings_trigger"))
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      
      sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
      shinyjs::show(paste0(prefix, "_widget_settings"))
      r[[paste0(prefix, "_widget_card_selected_type")]] <- "widget_settings"
      
      widget_infos <- r[[paste0(prefix, "_widgets")]] %>% dplyr::filter(id == r[[paste0(prefix, "_widget_settings")]])
      req(nrow(widget_infos) > 0)
      
      # Update name & plugin textfields
      
      widget_plugin_infos <- r$plugins %>% dplyr::filter(id == widget_infos$plugin_id)
      
      shiny.fluent::updateTextField.shinyInput(session = session, "widget_settings_name", value = widget_infos$name)
      shiny.fluent::updateTextField.shinyInput(session = session, "widget_settings_plugin", value = widget_plugin_infos$name)
      
      # Get selected_concepts for this widget
      
      if (nrow(r[[paste0(prefix, "_widgets_concepts")]] %>%
          dplyr::filter(widget_id == r[[paste0(prefix, "_widget_settings")]])) > 0){
        
        r[[paste0(prefix, "_widget_settings_vocabulary_selected_concepts")]] <- r[[paste0(prefix, "_widgets_concepts")]] %>%
          dplyr::filter(widget_id == r[[paste0(prefix, "_widget_settings")]]) %>%
          dplyr::select(concept_id, concept_name, concept_display_name, domain_id, mapped_to_concept_id, merge_mapped_concepts)
        
        r[[paste0(prefix, "_widget_vocabulary_update_selected_concepts_dropdown")]] <- Sys.time()
        r[[paste0(prefix, "_widget_vocabulary_update_selected_concepts_dropdown_type")]] <- "widget_settings"
      }
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer r$..widget_settings_trigger"))
    })
    
    # Close button clicked
    observeEvent(input[[paste0(prefix, "_close_widget_settings")]], {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$..close_widget_settings"))
      shinyjs::hide(paste0(prefix, "_widget_settings"))
      sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)
    })
    
    # Save updates
    observeEvent(input$widget_settings_save, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$..widget_settings_save"))
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      
      new_data <- list()
      
      new_data$name <- coalesce2(type = "char", x = input$widget_settings_name)
      
      widget_id <- r[[paste0(prefix, "_widget_settings")]]
      ids <- r[[paste0(prefix, "_widgets")]] %>% dplyr::filter(id == widget_id) %>% dplyr::slice(1) %>% dplyr::select(plugin_id, tab_id)
      
      # Check if name is not empty
      if (is.na(new_data$name)) shiny.fluent::updateTextField.shinyInput(session, "widget_settings_name", errorMessage = i18n$t("provide_valid_name"))
      else shiny.fluent::updateTextField.shinyInput(session, "widget_settings_name", errorMessage = NULL)
      req(!is.na(new_data$name))
      
      # Check if values required to be unique are unique
      
      table <- paste0(prefix, "_widgets")
      
      sql <- glue::glue_sql("SELECT DISTINCT(name) FROM {`table`} WHERE deleted IS FALSE AND tab_id = {ids$tab_id} AND id != {widget_id}", .con = r$db)
      distinct_values <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
      if (new_data$name %in% distinct_values) show_message_bar(output,  "name_already_used", "severeWarning", i18n = i18n, ns = ns)
      req(new_data$name %not_in% distinct_values)
      
      # Update name in database & r var
      r[[paste0(prefix, "_widgets")]] <- r[[paste0(prefix, "_widgets")]] %>%
        dplyr::mutate(name = dplyr::case_when(
          id == widget_id ~ new_data$name,
          TRUE ~ name
        ))
      sql <- glue::glue_sql("UPDATE {`table`} SET name = {new_data$name} WHERE id = {widget_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      # Get last_row nb
      last_row_widgets_concepts <- get_last_row(m$db, paste0(prefix, "_widgets_concepts"))
      
      has_vocabulary_concepts <- TRUE
      selected_concepts <- tibble::tibble(concept_id = integer(), concept_name = character(), concept_display_name = character(), domain_id = character(),
        mapped_to_concept_id = integer(), merge_mapped_concepts = logical())
      
      if (length(r[[paste0(prefix, "_widget_settings_vocabulary_selected_concepts")]]) == 0) has_vocabulary_concepts <- FALSE
      if (length(r[[paste0(prefix, "_widget_settings_vocabulary_selected_concepts")]]) > 0) if (nrow(r[[paste0(prefix, "_widget_settings_vocabulary_selected_concepts")]]) == 0) has_vocabulary_concepts <- FALSE
      
      if (has_vocabulary_concepts){
        
        new_data <-
          r[[paste0(prefix, "_widget_settings_vocabulary_selected_concepts")]] %>%
          dplyr::transmute(
            id = 1:dplyr::n() + last_row_widgets_concepts + 1, widget_id = !!widget_id,
            concept_id, concept_name, concept_display_name, domain_id, mapped_to_concept_id, merge_mapped_concepts, 
            creator_id = r$user_id, datetime = as.character(Sys.time()), deleted = FALSE
          )
        
        # Remove old data
        sql <- glue::glue_sql("UPDATE {`paste0(prefix, '_widgets_concepts')`} SET deleted = TRUE WHERE widget_id = {widget_id}", .con = m$db)
        query <- DBI::dbSendStatement(m$db, sql)
        DBI::dbClearResult(query)
        r[[paste0(prefix, "_widgets_concepts")]] <- r[[paste0(prefix, "_widgets_concepts")]] %>% dplyr::filter(widget_id != !!widget_id)
        
        # Add new data
        DBI::dbAppendTable(m$db, paste0(prefix, "_widgets_concepts"), new_data)
        r[[paste0(prefix, "_widgets_concepts")]] <- r[[paste0(prefix, "_widgets_concepts")]] %>% dplyr::bind_rows(new_data)
        
        selected_concepts <- r[[paste0(prefix, "_widget_settings_vocabulary_selected_concepts")]]
      }
      
      show_message_bar(output, message = "modif_saved", type = "success", i18n = i18n, ns = ns)
      
      # Load translations file
      
      plugin_translations <- r$code %>% dplyr::filter(link_id == ids$plugin_id, category == "plugin_translations") %>% dplyr::pull(code)
      
      if (plugin_translations != ""){
        
        tryCatch({
          # Get plugin unique_id
          plugin_unique_id <- r$options %>% dplyr::filter(category == "plugin", name == "unique_id", link_id == ids$plugin_id) %>% dplyr::pull(value)
          
          # Create plugin folder in translations folder if doesn't exist
          new_dir <- paste0(r$app_folder, "/translations/", plugin_unique_id)
          if (!dir.exists(new_dir)) dir.create(new_dir)
          
          # Create a csv with all languages
          data <- read.csv(text = plugin_translations, header = TRUE, stringsAsFactors = FALSE)
          
          # Create one csv by language
          for(lang in names(data)[-1]){
            file_name <- paste0(new_dir, "/translation_", lang, ".csv")
            
            if (!file.exists(file_name)){
              # Create a new dataframe with base & current language cols
              data_lang <- data[, c("base", lang)]
              
              # Create csv
              write.csv(data_lang, file_name, row.names = FALSE)
            }
          }
        },
          error = function(e) report_bug(r = r, output = output, error_message = "error_creating_translations_file",
            error_name = paste0(id, " - create translations files - plugin_id ", ids$plugin_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
        
        tryCatch({
          i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = new_dir))
          i18np$set_translation_language(language)},
          error = function(e) report_bug(r = r, output = output, error_message = "error_creating_new_translator",
            error_name = paste0(id, " - create i18np translator - plugin_id ", ids$plugin_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
      }
      
      # Run server code
      
      code_server_card <- r$code %>%
        dplyr::filter(link_id == ids$plugin_id, category == "plugin_server") %>%
        dplyr::pull(code) %>%
        stringr::str_replace_all("%tab_id%", as.character(ids$tab_id)) %>%
        stringr::str_replace_all("%widget_id%", as.character(widget_id)) %>%
        stringr::str_replace_all("%req%", "req(m[[session_code]] == session_num)\nreq(m$selected_study == %study_id%)") %>%
        stringr::str_replace_all("%study_id%", as.character(m$selected_study)) %>%
        stringr::str_replace_all("\r", "\n") %>%
        stringr::str_replace_all("''", "'")
      
      # If it is an aggregated plugin, change %study_id% with current selected study
      if (length(m$selected_study) > 0) code_server_card <- code_server_card %>% stringr::str_replace_all("%study_id%", as.character(m$selected_study))
      
      session_code <- paste0(prefix, "_widget_", widget_id)
      if (length(m[[session_code]]) == 0) session_num <- 1L
      if (length(m[[session_code]]) > 0) session_num <- m[[session_code]] + 1
      m[[session_code]] <- session_num
      
      # Variables to hide
      new_env_vars <- list("r" = NA)
      
      # Variables to keep
      variables_to_keep <- c("d", "m", "session_code", "session_num", "i18n", "selected_concepts", "debug")
      if (exists("i18np")) variables_to_keep <- c(variables_to_keep, "i18np")
      
      for (var in variables_to_keep) new_env_vars[[var]] <- eval(parse(text = var))
      new_env <- rlang::new_environment(data = new_env_vars, parent = pryr::where("r"))
      
      tryCatch(eval(parse(text = code_server_card), envir = new_env),
        error = function(e) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code",
          error_name = paste0(id, " - save_widget_settings - run_plugin_code - plugin_id ", ids$plugin_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
      
      # Update toggles
      
      widgets <- r[[paste0(prefix, "_widgets")]] %>% dplyr::filter(tab_id == ids$tab_id) %>% dplyr::rename(widget_id = id) %>% dplyr::arrange(display_order)
      
      # Get widget widget_id
      distinct_widgets <- unique(widgets$widget_id)
      
      toggles <- tagList()
      
      # Loop over distinct cards (tabs elements), for this tab
      # Use sapply instead of for loop, cause with for loop, widget_id doesn't change
      sapply(distinct_widgets, function(widget_id){
        
        # Get name of widget
        widget_name <- widgets %>% dplyr::filter(widget_id == !!widget_id) %>% dplyr::slice(1) %>% dplyr::pull(name)
        
        toggles <<- tagList(toggles,
          shiny.fluent::Toggle.shinyInput(ns(paste0(prefix, "_widget_", widget_id, "_toggle")), value = TRUE, style = "margin-top:10px;"),
          div(class = "toggle_title", widget_name, style = "padding-top:10px;"))
        
        # Add to the list of opened cards
        r[[paste0(prefix, "_opened_cards")]] <- c(r[[paste0(prefix, "_opened_cards")]], paste0(prefix, "_widget_", widget_id))
      })
      
      toggles_div <- div(
        make_card("",
          shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
            shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_widget_", ids$tab_id)), i18n$t("new_widget"), iconProps = list(iconName = "Add"),
              onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_add_widget_trigger', Math.random())"))),
            shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_edit_tab_", ids$tab_id)), i18n$t("edit_tab"), iconProps = list(iconName = "Edit"),
              onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-edit_tab_trigger', Math.random())"))),
            div(style = "width:20px;"),
            toggles
          )
        )
      )
      
      output[[paste0(prefix, "_toggles_", ids$tab_id)]] <- renderUI(toggles_div)
      
      # Hide settings card and show opened cards
      shinyjs::hide(paste0(prefix, "_widget_settings"))
      sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer input$..widget_settings_save"))
    })
    
    # --- --- --- --- --- --- --- --- --- -- -
    ## Sortable / change pivotitems order ----
    # --- --- --- --- --- --- --- --- --- -- -
    
    observeEvent(input$study_pivot_order, {

      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$study_pivot_order"))
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      
      new_pivot_order <- tibble::tibble(name = stringr::str_split(input$study_pivot_order, "\n") %>% unlist() %>% sub("^ ", "", .)) %>%
        dplyr::mutate(display_order = 1:dplyr::n())

      table <- paste0(prefix, "_tabs")
      tab_group_id <- r[[table]] %>% dplyr::filter(id == r[[paste0(prefix, '_selected_tab')]]) %>% dplyr::pull(tab_group_id)
      
      if (is.na(tab_group_id)) all_tabs <- r[[table]] %>% dplyr::filter(is.na(tab_group_id))
      else all_tabs <- r[[table]] %>% dplyr::filter(tab_group_id == !!tab_group_id)

      all_tabs <- all_tabs %>%
        dplyr::select(-display_order) %>%
        dplyr::inner_join(new_pivot_order, by = "name") %>%
        dplyr::relocate(display_order, .after = "parent_tab_id")
      
      r[[paste0(prefix, "_display_tabs")]] <- r[[paste0(prefix, "_display_tabs")]] %>%
        dplyr::left_join(all_tabs %>% dplyr::select(id, new_display_order = display_order), by = "id") %>%
        dplyr::mutate(display_order = dplyr::case_when(!is.na(new_display_order) ~ new_display_order, TRUE ~ display_order)) %>%
        dplyr::select(-new_display_order)

      sql <- glue::glue_sql("DELETE FROM {`table`} WHERE id IN ({all_tabs %>% dplyr::pull(id)*})", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)

      DBI::dbAppendTable(r$db, table, all_tabs)

      r[[table]] <- r[[table]] %>%
        dplyr::anti_join(all_tabs %>% dplyr::select(id), by = "id") %>%
        dplyr::bind_rows(all_tabs) %>%
        dplyr::arrange(id)

      r[[paste0(prefix, "_load_display_tabs")]] <- Sys.time()

      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer input$study_pivot_oder"))
    })
    
    # --- --- --- --- --- --- --- --- ---
    ## When a pivot item is selected ----
    # --- --- --- --- --- --- --- --- ---
    
    observeEvent(r[[paste0(prefix, "_selected_tab")]], {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer r$..selected_tab"))
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      
      req(!grepl("show_tab", r[[paste0(prefix, "_selected_tab")]]))
      
      # Hide all cards
      sapply(r[[paste0(prefix, "_cards")]], shinyjs::hide)
      
      # Hide Add widget card, Add tab card & Edit tab card
      sapply(c(paste0(prefix, "_add_tab"), paste0(prefix, "_edit_tab"), paste0(prefix, "_add_widget"), paste0(prefix, "_widget_settings")), shinyjs::hide)
      
      # Show toggles for this tab
      shinyjs::show(paste0(prefix, "_toggles_", r[[paste0(prefix, "_selected_tab")]]))
      
      # Add to the list of open cards and reset the list
      r[[paste0(prefix, "_opened_cards")]] <- paste0(prefix, "_toggles_", r[[paste0(prefix, "_selected_tab")]])
      
      widgets <- r[[paste0(prefix, "_widgets")]] %>% dplyr::filter(tab_id == r[[paste0(prefix, "_selected_tab")]]) %>% 
        dplyr::rename(widget_id = id)
      distinct_widgets <- unique(widgets$widget_id)
      
      # Use sapply instead of for loop, cause with for loop, widget_id doesn't change
      sapply(distinct_widgets, function(widget_id){
        
        # If toggle is ON
        if (length(input[[paste0(prefix, "_widget_", widget_id, "_toggle")]]) > 0){
          
          if (input[[paste0(prefix, "_widget_", widget_id, "_toggle")]]){
            
            # Show card
            shinyjs::show(paste0(prefix, "_widget_", widget_id))
            
            # Add to the list of open cards
            r[[paste0(prefix, "_opened_cards")]] <- c(r[[paste0(prefix, "_opened_cards")]], paste0(prefix, "_widget_", widget_id))
          }
        }
        else {
          shinyjs::show(paste0(prefix, "_widget_", widget_id))
          r[[paste0(prefix, "_opened_cards")]] <- c(r[[paste0(prefix, "_opened_cards")]], paste0(prefix, "_widget_", widget_id))
        }
        
      })
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer r$..selected_tab"))
    })
    
    # --- --- --- - -
    ## Add a tab ----
    # --- --- --- - -
    
    observeEvent(input$study_add_new_tab, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$study_add_new_tab"))

      sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
      shinyjs::hide(paste0(prefix, "_add_widget"))
      shinyjs::hide(paste0(prefix, "_edit_tab"))
      shinyjs::hide(paste0(prefix, "_widget_settings"))
      shinyjs::show(paste0(prefix, "_add_tab"))
      shinyjs::hide(paste0(prefix, "_no_tabs_to_display"))
    })
    
    # Close creation div
    observeEvent(input[[paste0(prefix, "_close_add_tab")]], {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$..close_add_tab"))
      
      # Show opened cards before opening Add widget div
      sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)
      
      # Hide Add tab div
      shinyjs::hide(paste0(prefix, "_add_tab"))
    })
    
    # Add button clicked
    observeEvent(input$add_tab_button, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$add_tab_button"))
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      
      study <- r$studies %>% dplyr::filter(id == m$selected_study)
      tab <- r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(id == r[[paste0(prefix, "_selected_tab")]])
      
      new_data <- list()
      new_data$name <- coalesce2(type = "char", x = input$tab_name)
      new_data$description <- ""
      
      # Required textfields
      required_textfields <- "name"
      
      # Fields requiring unique value
      req_unique_values <- "name"
      
      # Get tab_group_id
      new_data$tab_group <- study %>% dplyr::pull(paste0(prefix, "_tab_group_id"))
      
      # If it is the first tab to be created
      if (nrow(tab) == 0){
        new_data$parent_tab <- NA_integer_
        new_data$display_order <- 1
      }
      
      # If already existing tabs
      if (nrow(tab) > 0){
        
        # If tab is at the same level of current tab, get common parent_tab_id
        # Calculate display order
        
        if (input$add_tab_type == "same_level") new_data$parent_tab <- tab %>% dplyr::pull(parent_tab_id)
        if (input$add_tab_type == "level_under") new_data$parent_tab <- tab %>% dplyr::pull(id)
        
        # Calculate display order
        if (!is.na(new_data$parent_tab)) sql <- glue::glue_sql("SELECT COALESCE(MAX(display_order), 0) FROM {`paste0(prefix, '_tabs')`}
            WHERE tab_group_id = {new_data$tab_group} AND parent_tab_id = {new_data$parent_tab}", .con = r$db)
        if (is.na(new_data$parent_tab)) sql <- glue::glue_sql("SELECT COALESCE(MAX(display_order), 0) FROM {`paste0(prefix, '_tabs')`}
            WHERE tab_group_id = {new_data$tab_group} AND parent_tab_id IS NULL", .con = r$db)
        
        new_data$display_order <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull() + 1
        
        # Can't add a tab at the level under if there are tabs elements attached to current tab
        if (input$add_tab_type == "level_under"){
          widgets <- r[[paste0(prefix, "_widgets")]] %>% dplyr::filter(tab_id == r[[paste0(prefix, "_selected_tab")]], !deleted) %>%
            dplyr::rename(widget_id = id)
          if (nrow(widgets) > 0) show_message_bar(output, message = "add_tab_has_widgets", i18n = i18n, ns = ns)
          req(nrow(widgets) == 0)
        }
      }
      
      if (is.na(new_data$name)) shiny.fluent::updateTextField.shinyInput(session, "tab_name", errorMessage = i18n$t("provide_valid_name"))
      req(!is.na(new_data$name))
      
      add_settings_new_data(session = session, output = output, r = r, m = m, i18n = i18n, id = id,
        data = new_data, table = paste0(prefix, "_tabs"), required_textfields = required_textfields, req_unique_values = req_unique_values)
      
      # Reset fields
      
      shiny.fluent::updateTextField.shinyInput(session, "tab_name", value = "")
      shiny.fluent::updateChoiceGroup.shinyInput(session, "add_tab_type", value = "same_level")
      
      # Reload UI, with new tab opened
      
      tab_id <- get_last_row(r$db, paste0(prefix, "_tabs"))
      r[[paste0(prefix, "_selected_tab")]] <- tab_id
      r[[paste0(prefix, "_load_display_tabs")]] <- Sys.time()
      
      # Add Toggles div
      
      toggles_div <- div(
        make_card("",
          shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
            shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_widget_", tab_id)), i18n$t("new_widget"), iconProps = list(iconName = "Add"),
              onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_add_widget_trigger', Math.random())"))),
            shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_edit_tab_", tab_id)), i18n$t("edit_tab"), iconProps = list(iconName = "Edit"),
              onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-edit_tab_trigger', Math.random())"))),
            div(style = "width:20px;")
          )
        )
      )
      
      insertUI(selector = paste0("#", ns("study_cards")), where = "beforeEnd", ui = uiOutput(ns(paste0(prefix, "_toggles_", tab_id))))
      output[[paste0(prefix, "_toggles_", tab_id)]] <- renderUI(toggles_div)
      
      # If this is a sub-tab, change toggles div of parent tab also
      parent_tab_id <- r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(id == tab_id) %>% dplyr::pull(parent_tab_id)
      if(!is.na(parent_tab_id)){
        
        shinyjs::hide(paste0(prefix, "_toggles_", parent_tab_id))
        
        parent_toggles_div <- div(
          make_card("",
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_edit_tab_", tab_id)), i18n$t("edit_tab"), iconProps = list(iconName = "Edit"),
                onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-edit_tab_trigger', Math.random())"))),
              div(shiny.fluent::MessageBar(i18n$t("tab_contains_sub_tabs"), messageBarType = 5), style = "margin-top:4px;")
            )
          )
        )
        
        insertUI(selector = paste0("#", ns("study_cards")), where = "beforeEnd", ui = shinyjs::hidden(uiOutput(ns(paste0(prefix, "_toggles_", parent_tab_id)))))
        output[[paste0(prefix, "_toggles_", parent_tab_id)]] <- renderUI(parent_toggles_div)
      }
      
      # Add toggles div to vector of cards
      r[[paste0(prefix, "_cards")]] <- c(isolate(r[[paste0(prefix, "_cards")]]), paste0(prefix, "_toggles_", tab_id))
      
      # Reload UI menu and set to added tab
      r[[paste0(prefix, "_selected_tab")]] <- tab_id
      r[[paste0(prefix, "_load_ui_menu")]] <- Sys.time()
      
      # Hide currently opened cards
      sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer input$add_tab_button"))
    })
    
    # --- --- --- -- -
    ## Edit a tab ----
    # --- --- --- -- -
    
    observeEvent(input$edit_tab_trigger, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$edit_tab_trigger"))
      
      sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
      shinyjs::hide(paste0(prefix, "_add_widget"))
      shinyjs::hide(paste0(prefix, "_widget_settings"))
      shinyjs::show(paste0(prefix, "_edit_tab"))
      
      # Update textfield with current tab name
      shiny.fluent::updateTextField.shinyInput(session, "edit_tab_name", 
        value = r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(id == r[[paste0(prefix, "_selected_tab")]]) %>% dplyr::pull(name))
    })
    
    # Save updates
    observeEvent(input$edit_tab_save, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$edit_tab_save"))
      
      # Check if the name is not empty
      if (input$edit_tab_name == "") shiny.fluent::updateTextField.shinyInput(session, "edit_tab_name", errorMessage = i18n$t("provide_valid_name"))
      
      req(input$edit_tab_name != "")
      
      # Check if the name is already used
      tab <- r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(id == r[[paste0(prefix, "_selected_tab")]])
      
      if (!is.na(tab$parent_tab_id)) same_level_tabs <- r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(id != tab$id, parent_tab_id == tab$parent_tab_id)
      if (is.na(tab$parent_tab_id)) same_level_tabs <- r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(id != tab$id, is.na(parent_tab_id))
      
      if (input$edit_tab_name %in% same_level_tabs$name) show_message_bar(output, "name_already_used", "severeWarning", i18n = i18n, ns = ns)
      
      req(input$edit_tab_name %not_in% same_level_tabs$name)
      
      # Update database
      
      sql <- glue::glue_sql("UPDATE {`paste0(prefix, '_tabs')`} SET name = {input$edit_tab_name} WHERE id = {r[[paste0(prefix, '_selected_tab')]]}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      # Update r vars
      r[[paste0(prefix, "_tabs")]] <- r[[paste0(prefix, "_tabs")]] %>% dplyr::mutate(name = dplyr::case_when(
        id == r[[paste0(prefix, "_selected_tab")]] ~ input$edit_tab_name, TRUE ~ name))
      
      r[[paste0(prefix, "_display_tabs")]] <- r[[paste0(prefix, "_display_tabs")]] %>% dplyr::mutate(name = dplyr::case_when(
        id == r[[paste0(prefix, "_selected_tab")]] ~ input$edit_tab_name, TRUE ~ name))
      
      # Notify user
      show_message_bar(output, message = "modif_saved", type = "success", i18n = i18n, ns = ns)
      
      # Show opened cards before opening Add widget div
      sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)
      
      # Hide Edit widget div
      shinyjs::hide(paste0(prefix, "_edit_tab"))
      
      # Reload output
      r[[paste0(prefix, "_load_ui_menu")]] <- Sys.time()
    })
    
    # Close edition div
    observeEvent(input[[paste0(prefix, "_close_edit_tab")]], {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$..close_edit_tab"))
      
      # Show opened cards before opening Add widget div
      sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)
      
      # Hide Edit widget div
      shinyjs::hide(paste0(prefix, "_edit_tab"))
    })
    
    # --- --- --- --- --- --- --- --- --- --
    ## Show / hide widget creation card ----
    # --- --- --- --- --- --- --- --- --- --
    
    # Code to make Add widget button work
    observeEvent(input[[paste0(prefix, "_add_widget_trigger")]], {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$..add_widget_trigger"))
      
      # Hide opened cards
      sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
      
      # Show Add widget div
      shinyjs::show(paste0(prefix, "_add_widget"))
      
      r[[paste0(prefix, "_widget_card_selected_type")]] <- "widget_creation"
    })
    
    # --- --- --- --- --
    ## Delete a tab ----
    # --- --- --- --- --
    
    # Code to make Remove tab button work
    observeEvent(input$remove_tab, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$remove_tab"))
      r[[tab_delete_variable]] <- TRUE
    })
    
    tab_delete_prefix <- paste0(prefix, "_tab")
    tab_dialog_title <- paste0(prefix, "_tabs_delete")
    tab_dialog_subtext <- paste0(prefix, "_tabs_delete_subtext")
    tab_react_variable <- "tab_delete_confirm"
    tab_table <- paste0(prefix, "_tabs")
    tab_id_var_sql <- "id"
    tab_id_var_r <- paste0(prefix, "_selected_tab")
    tab_delete_message <- paste0(prefix, "_tab_deleted")
    tab_reload_variable <- paste0(prefix, "_load_ui")
    tab_information_variable <- paste0(prefix, "_tab_deleted")
    tab_delete_variable <- paste0(tab_delete_prefix, "_open_dialog")
    
    delete_element(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
      delete_prefix = tab_delete_prefix, dialog_title = tab_dialog_title, dialog_subtext = tab_dialog_subtext,
      react_variable = tab_react_variable, table = tab_table, id_var_sql = tab_id_var_sql, id_var_r = tab_id_var_r,
      delete_message = tab_delete_message, translation = TRUE, reload_variable = tab_reload_variable,
      information_variable = tab_information_variable)
    
    # When a tab is deleted, change current tab variable
    # Reload toggles if necessary
    # Delete sub-tabs either
    
    observeEvent(r[[tab_information_variable]], {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer r$..tab_deleted"))
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      
      table <- paste0(prefix, "_tabs")
      deleted_tab_id <- r[[paste0(prefix, "_tab_deleted")]]
      sql <- glue::glue_sql("SELECT * FROM {`table`} WHERE id = {deleted_tab_id}", .con = r$db)
      tab_deleted <- DBI::dbGetQuery(r$db, sql)
      
      # If we are at level one, take first tab of level one
      if (is.na(tab_deleted$parent_tab_id)){
        show_tab_id <- r[[table]] %>%
          dplyr::filter(tab_group_id == tab_deleted$tab_group_id & is.na(parent_tab_id) & id != tab_deleted$id) %>%
          dplyr::arrange(display_order) %>%
          dplyr::slice(1) %>%
          dplyr::pull(id)
      }
      
      # Else, take first tab of the same level
      if (!is.na(tab_deleted$parent_tab_id)){
        show_tab <- r[[table]] %>%
          dplyr::filter(parent_tab_id == tab_deleted$parent_tab_id & id != tab_deleted$id)
        
        # If not any tab in this level, take lower level
        if (nrow(show_tab) == 0) show_tab <- r[[table]] %>%
            dplyr::filter(id == tab_deleted$parent_tab_id)
        
        show_tab_id <- show_tab %>%
          dplyr::arrange(display_order) %>%
          dplyr::slice(1) %>%
          dplyr::pull(id)
      }
      
      r[[paste0(prefix, "_selected_tab")]] <- show_tab_id
      sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::hide)
      shinyjs::show(paste0(prefix, "_toggles_", show_tab_id))
      
      # Reload UI menu
      r[[paste0(prefix, "_load_display_tabs")]] <- Sys.time()
      
      # Check if parent tab still have children and reload toggles div if not
      sql <- glue::glue_sql("SELECT parent_tab_id FROM {`paste0(prefix, '_tabs')`} WHERE id = {deleted_tab_id}", .con = r$db)
      parent_tab_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(parent_tab_id)
      if (!is.na(parent_tab_id)){
        
        has_children <- r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(parent_tab_id == !!parent_tab_id) %>% nrow()
        if(has_children == 0){
          
          shinyjs::hide(paste0(prefix, "_toggles_", parent_tab_id))
          
          parent_toggles_div <- div(
            make_card("",
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_widget_", parent_tab_id)), i18n$t("new_widget"), iconProps = list(iconName = "Add"),
                  onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_add_widget_trigger', Math.random())"))),
                shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_edit_tab_", parent_tab_id)), i18n$t("edit_tab"), iconProps = list(iconName = "Edit"),
                  onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-edit_tab_trigger', Math.random())"))),
                div(style = "width:20px;")
              )
            )
          )
          
          insertUI(selector = paste0("#", ns("study_cards")), where = "beforeEnd", ui = uiOutput(ns(paste0(prefix, "_toggles_", parent_tab_id))))
          output[[paste0(prefix, "_toggles_", parent_tab_id)]] <- renderUI(parent_toggles_div)
        }
      }
      
      # Hide edit_tab div
      shinyjs::hide(paste0(prefix, "_edit_tab"))
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer r$..tab_deleted"))
    })
    
    # --- --- --- --- --
    ## Add a widget ----
    # --- --- --- --- --
    
    # --- --- --- --- --- --- --
    ## Vocabulary datatable ----
    # --- --- --- --- --- --- --
    
    # Load vocabularies attached to this dataset
    observeEvent(r$dataset_vocabularies, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer r$dataset_vocabularies"))
      
      if (nrow(r$dataset_vocabularies) == 0) vocabulary_options = list()
      if (nrow(r$dataset_vocabularies) > 0) vocabulary_options <- convert_tibble_to_list(data = r$dataset_vocabularies, 
        key_col = "vocabulary_id", text_col = "vocabulary_id", i18n = i18n)
      
      for(name in c("widget_creation_vocabulary", "widget_settings_vocabulary")) shiny.fluent::updateComboBox.shinyInput(
        session, name, options = vocabulary_options, value = NULL)
    })
    
    # Reload vocabulary concepts
    
    observeEvent(input$widget_creation_vocabulary, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$widget_creation_vocabulary"))
      r[[paste0(prefix, "_reload_widget_vocabulary_concepts")]] <- Sys.time()
      r[[paste0(prefix, "_reload_widget_vocabulary_concepts_type")]] <- "widget_creation"
    })
    
    observeEvent(input$widget_settings_vocabulary, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$widget_settings_vocabulary"))
      r[[paste0(prefix, "_reload_widget_vocabulary_concepts")]] <- Sys.time()
      r[[paste0(prefix, "_reload_widget_vocabulary_concepts_type")]] <- "widget_settings"
    })
    
    observeEvent(input$widget_creation_show_mapped_concepts, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$widget_creation_show_mapped_concepts"))
      r[[paste0(prefix, "_reload_widget_vocabulary_concepts")]] <- Sys.time()
      r[[paste0(prefix, "_reload_widget_vocabulary_concepts_type")]] <- "widget_creation"
      if (input$widget_creation_show_mapped_concepts & !input$widget_creation_hide_concepts_datatables) shinyjs::show("widget_creation_vocabulary_mapped_concepts")
      else shinyjs::hide("widget_creation_vocabulary_mapped_concepts")
      if (input$widget_creation_show_mapped_concepts) shinyjs::show("widget_creation_show_mapped_concepts_div")
      else shinyjs::hide("widget_creation_show_mapped_concepts_div")
    })
    
    observeEvent(input$widget_settings_show_mapped_concepts, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$widget_settings_show_mapped_concepts"))
      r[[paste0(prefix, "_reload_widget_vocabulary_concepts")]] <- Sys.time()
      r[[paste0(prefix, "_reload_widget_vocabulary_concepts_type")]] <- "widget_settings"
      if (input$widget_settings_show_mapped_concepts) shinyjs::show("widget_settings_vocabulary_mapped_concepts")
      else shinyjs::hide("widget_settings_vocabulary_mapped_concepts")
    })
    
    observeEvent(r[[paste0(prefix, "_reload_widget_vocabulary_concepts")]], {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " observer r$..reload_widget_vocabulary_concepts"))
      
      req(length(d$dataset_all_concepts) > 0, nrow(d$dataset_all_concepts) > 0)
      
      type <- r[[paste0(prefix, "_reload_widget_vocabulary_concepts_type")]]
      
      vocabulary_id <- input[[paste0(type, "_vocabulary")]]$key
      req(length(vocabulary_id) > 0)
      
      widget_vocabulary_concepts <- d$dataset_all_concepts %>%
        dplyr::filter(vocabulary_id_1 == vocabulary_id) %>%
        dplyr::select(concept_id = concept_id_1, concept_name = concept_name_1, concept_display_name = concept_display_name_1,
          relationship_id, domain_id, concept_class_id, standard_concept, concept_code,
          count_persons_rows, count_concepts_rows, add_concept_input)
      
      if (input[[paste0(type, "_show_mapped_concepts")]]) widget_vocabulary_concepts <- widget_vocabulary_concepts %>%
        dplyr::group_by(concept_id) %>%
        dplyr::summarize(count_persons_rows = max(count_persons_rows), count_concepts_rows = sum(count_concepts_rows)) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(
          widget_vocabulary_concepts %>% dplyr::group_by(concept_id) %>% dplyr::slice(1) %>% dplyr::ungroup() %>% dplyr::select(-count_persons_rows, -count_concepts_rows),
          by = "concept_id"
        ) %>%
        dplyr::relocate(count_persons_rows, count_concepts_rows, .after = "concept_code") %>%
        dplyr::left_join(
          widget_vocabulary_concepts %>% dplyr::filter(is.na(relationship_id)) %>% dplyr::transmute(concept_id, no_mapping = TRUE),
          by = "concept_id"
        ) %>%
        dplyr::mutate(add_concept_input = ifelse(is.na(no_mapping), "", add_concept_input)) %>%
        dplyr::select(-no_mapping)
      
      if (!input[[paste0(type, "_show_mapped_concepts")]]) widget_vocabulary_concepts <- widget_vocabulary_concepts %>%
        dplyr::filter(is.na(relationship_id))
      
      widget_vocabulary_concepts <- widget_vocabulary_concepts %>%
        dplyr::select(-relationship_id) %>%
        dplyr::mutate_at(c("add_concept_input"), stringr::str_replace_all, "%ns%", id) %>%
        dplyr::mutate_at("add_concept_input", stringr::str_replace_all, "%input_prefix%", paste0(type, "_add_concept")) %>%
        dplyr::mutate_at("add_concept_input", stringr::str_replace_all, "%input_prefix_2%", paste0(type, "_")) %>%
        dplyr::mutate_at("concept_id", as.character) %>%
        dplyr::mutate(add_concept_input = stringr::str_replace_all(add_concept_input, "%concept_id_1%", concept_id))
      
      r[[paste0(prefix, "_", type, "_vocabulary_concepts")]] <- widget_vocabulary_concepts
      
      if (length(r[[paste0("widget_", type, "_vocabulary_concepts_proxy")]]) == 0){
        editable_cols <- c("concept_display_name")
        searchable_cols <- c("concept_id", "concept_name", "concept_display_name")
        column_widths <- c("concept_id" = "80px", "action" = "80px")
        sortable_cols <- c("concept_id", "concept_name", "concept_display_name", "count_persons_rows", "count_concepts_rows")
        centered_cols <- c("concept_id", "count_persons_rows", "count_concepts_rows", "add_concept_input")
        col_names <- get_col_names("plugins_vocabulary_concepts_with_counts", i18n)
        hidden_cols <- c("domain_id", "concept_class_id", "standard_concept", "concept_code")
        column_widths <- c("concept_id" = "120px", "count_persons_rows" = "80px", "count_concepts_rows" = "80px",
          "add_concept_input" = "80px")
        
        # Render datatable
        render_datatable(output = output, ns = ns, i18n = i18n, data = widget_vocabulary_concepts,
          output_name = paste0(type, "_vocabulary_concepts"), col_names =  col_names,
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, hidden_col = hidden_cols)
        
        # Create a proxy for datatatable
        r[[paste0(prefix, "_", type, "_vocabulary_concepts_proxy")]] <- DT::dataTableProxy(paste0(type, "_vocabulary_concepts"), deferUntilFlush = FALSE)
        
        if (input[[paste0(type, "_hide_concepts_datatables")]]) shinyjs::show(paste0(type, "_blank_space")) else shinyjs::hide(paste0(type, "_blank_space"))
      }
      else DT::replaceData(r[[paste0(prefix, "_", type, "_vocabulary_concepts_proxy")]], r[[paste0(prefix, "_", type, "_vocabulary_concepts")]], resetPaging = FALSE, rownames = FALSE)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer r$..reload_widget_vocabulary_concepts"))
    })
    
    # Update which cols are hidden
    
    observeEvent(input$widget_creation_vocabulary_concepts_table_cols, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$widget_creation_vocabulary_concepts_table_cols"))
      
      req(length(r[[paste0(prefix, "_widget_creation_vocabulary_concepts_proxy")]]) > 0)
      
      r[[paste0(prefix, "_widget_creation_vocabulary_concepts_proxy")]] %>%
        DT::showCols(0:9) %>%
        DT::hideCols(setdiff(0:9, input$widget_creation_vocabulary_concepts_table_cols))
    })
    
    observeEvent(input$widget_settings_vocabulary_concepts_table_cols, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$widget_settings_vocabulary_concepts_table_cols"))
      
      req(length(r[[paste0(prefix, "_widget_settings_vocabulary_concepts_proxy")]]) > 0)
      
      r[[paste0(prefix, "_widget_settings_vocabulary_concepts_proxy")]] %>%
        DT::showCols(0:9) %>%
        DT::hideCols(setdiff(0:9, input$widget_settings_vocabulary_concepts_table_cols))
    })
    
    observeEvent(input$widget_creation_vocabulary_mapped_concepts_table_cols, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$widget_creation_vocabulary_mapped_concepts_table_cols"))
      
      req(length(r[[paste0(prefix, "_widget_creation_vocabulary_mapped_concepts_proxy")]]) > 0)
      
      r[[paste0(prefix, "_widget_creation_vocabulary_mapped_concepts_proxy")]] %>%
        DT::showCols(0:8) %>%
        DT::hideCols(setdiff(0:8, input$widget_creation_vocabulary_mapped_concepts_table_cols))
    })
    
    observeEvent(input$widget_settings_vocabulary_mapped_concepts_table_cols, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$widget_settings_vocabulary_mapped_concepts_table_cols"))
      
      req(length(r[[paste0(prefix, "_widget_settings_vocabulary_mapped_concepts_proxy")]]) > 0)
      
      r[[paste0(prefix, "_widget_settings_vocabulary_mapped_concepts_proxy")]] %>%
        DT::showCols(0:8) %>%
        DT::hideCols(setdiff(0:8, input$widget_settings_vocabulary_mapped_concepts_table_cols))
    })
    
    # Hide datatables
    
    observeEvent(input$widget_creation_hide_concepts_datatables, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_vocabularies - observer input$widget_creation_hide_concepts_datatables"))
      
      req(input$widget_creation_vocabulary)
      
      sapply(c("widget_creation_vocabulary_concepts", "widget_creation_vocabulary_mapped_concepts"), function(datatable) if (input$widget_creation_hide_concepts_datatables)
        shinyjs::hide(datatable) else shinyjs::show(datatable))
      if (input$widget_creation_hide_concepts_datatables) shinyjs::show("widget_creation_blank_space") else shinyjs::hide("widget_creation_blank_space")
    })
    
    observeEvent(input$widget_settings_hide_concepts_datatables, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_vocabularies - observer input$widget_settings_hide_concepts_datatables"))
      
      req(input$widget_settings_vocabulary)
      
      sapply(c("widget_settings_vocabulary_concepts", "widget_settings_vocabulary_mapped_concepts"), function(datatable) if (input$widget_settings_hide_concepts_datatables)
        shinyjs::hide(datatable) else shinyjs::show(datatable))
      if (input$widget_settings_hide_concepts_datatables) shinyjs::show("widget_settings_blank_space") else shinyjs::hide("widget_settings_blank_space")
    })
    
    # Updates in datatable
    
    observeEvent(input$widget_creation_vocabulary_concepts_cell_edit, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$widget_creation_vocabulary_concepts_cell_edit"))
      
      edit_info <- input$widget_creation_vocabulary_concepts_cell_edit
      r[[paste0(prefix, "_widget_creation_vocabulary_concepts")]] <- DT::editData(r[[paste0(prefix, "_widget_creation_vocabulary_concepts")]], edit_info, rownames = FALSE)
    })
    
    observeEvent(input$widget_creation_vocabulary_mapped_concepts_cell_edit, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$widget_creation_vocabulary_mapped_concepts_cell_edit"))
      
      edit_info <- input$widget_creation_vocabulary_mapped_concepts_cell_edit
      r[[paste0(prefix, "_widget_creation_vocabulary_mapped_concepts")]] <- DT::editData(r[[paste0(prefix, "_widget_creation_vocabulary_mapped_concepts")]], edit_info, rownames = FALSE)
    })
    
    observeEvent(input$widget_settings_vocabulary_concepts_cell_edit, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$widget_settings_vocabulary_concepts_cell_edit"))
      
      edit_info <- input$widget_settings_vocabulary_concepts_cell_edit
      r[[paste0(prefix, "_widget_settings_vocabulary_concepts")]] <- DT::editData(r[[paste0(prefix, "_widget_settings_vocabulary_concepts")]], edit_info, rownames = FALSE)
    })
    
    observeEvent(input$widget_settings_vocabulary_mapped_concepts_cell_edit, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$widget_settings_vocabulary_mapped_concepts_cell_edit"))
      
      edit_info <- input$widget_settings_vocabulary_mapped_concepts_cell_edit
      r[[paste0(prefix, "_widget_settings_vocabulary_mapped_concepts")]] <- DT::editData(r[[paste0(prefix, "_widget_settings_vocabulary_mapped_concepts")]], edit_info, rownames = FALSE)
    })
    
    # Show mapped concepts
    
    observeEvent(input$widget_creation_vocabulary_concepts_rows_selected, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$widget_creation_vocabulary_concepts_rows_selected"))
      r[[paste0(prefix, "_reload_widget_vocabulary_mapped_concepts")]] <- Sys.time()
      r[[paste0(prefix, "_reload_widget_vocabulary_mapped_concepts_type")]] <- "widget_creation"
    })
    
    observeEvent(input$widget_settings_vocabulary_concepts_rows_selected, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$widget_settings_vocabulary_concepts_rows_selected"))
      r[[paste0(prefix, "_reload_widget_vocabulary_mapped_concepts")]] <- Sys.time()
      r[[paste0(prefix, "_reload_widget_vocabulary_mapped_concepts_type")]] <- "widget_settings"
    })
    
    observeEvent(r[[paste0(prefix, "_reload_widget_vocabulary_mapped_concepts")]], {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer r$..reload_widget_vocabulary_mapped_concepts"))
      
      type <- r[[paste0(prefix, "_reload_widget_vocabulary_mapped_concepts_type")]]
      
      req(input[[paste0(type, "_show_mapped_concepts")]])
      
      selected_concept <- r[[paste0(prefix, "_", type, "_vocabulary_concepts")]][input[[paste0(type, "_vocabulary_concepts_rows_selected")]], ]
      
      r[[paste0(prefix, "_", type, "_vocabulary_mapped_concepts")]] <- d$dataset_all_concepts %>%
        dplyr::filter(concept_id_1 == selected_concept$concept_id, !is.na(relationship_id)) %>%
        dplyr::transmute(concept_id_1, relationship_id, concept_id_2, concept_name_2,
          count_persons_rows, count_concepts_rows) %>%
        dplyr::left_join(
          d$dataset_all_concepts %>%
            dplyr::select(concept_id_2 = concept_id_1, concept_display_name_2 = concept_display_name_1, domain_id, add_concept_input),
          by = "concept_id_2"
        ) %>%
        dplyr::relocate(concept_display_name_2, .after = "concept_name_2") %>%
        dplyr::relocate(domain_id, .after = "concept_display_name_2") %>%
        dplyr::mutate(id = NA_integer_, .before = "concept_id_1")
      
      if (nrow(r[[paste0(prefix, "_", type, "_vocabulary_mapped_concepts")]]) > 0) r[[paste0(prefix, "_", type, "_vocabulary_mapped_concepts")]] <-
        r[[paste0(prefix, "_", type, "_vocabulary_mapped_concepts")]] %>%
        dplyr::group_by_all() %>% dplyr::slice(1) %>% dplyr::ungroup() %>%
        dplyr::mutate_at(c("add_concept_input"), stringr::str_replace_all, "%ns%", id) %>%
        dplyr::mutate_at("add_concept_input", stringr::str_replace_all, "%input_prefix%", paste0(type, "_add_mapped_concept")) %>%
        dplyr::mutate_at("add_concept_input", stringr::str_replace_all, "%input_prefix_2%", paste0(type, "_")) %>%
        dplyr::mutate_at(c("concept_id_1", "concept_id_2"), as.character) %>%
        # Add a unique id (rows are not unique with only concept_id_2, cause there can be multiple concept_relationship)
        dplyr::mutate(id = 1:dplyr::n()) %>%
        dplyr::mutate(add_concept_input = stringr::str_replace_all(add_concept_input, "%concept_id_1%", as.character(id)))
      
      if (length(r[[paste0(prefix, "_", type, "_vocabulary_mapped_concepts_proxy")]]) == 0){
        editable_cols <- c("concept_display_name_2")
        searchable_cols <- c("relationship_id", "concept_id_2", "concept_name_2", "concept_display_name_2")
        column_widths <- c("concept_id_1" = "120px", "concept_id_2" = "120px", "count_persons_rows" = "80px", "count_concepts_rows" = "80px",
          "add_concept_input" = "80px")
        sortable_cols <- c("relationship_id", "concept_id_2", "concept_name_2", "concept_display_name_2", "count_persons_rows", "count_concepts_rows")
        centered_cols <- c("concept_id_1", "concept_id_2", "count_persons_rows", "count_concepts_rows", "add_concept_input")
        col_names <- get_col_names("plugins_vocabulary_mapped_concepts_with_counts", i18n)
        hidden_cols <- c("id", "concept_id_1", "domain_id")
        column_widths <- c("concept_id" = "100px", "count_persons_rows" = "80px", "count_concepts_rows" = "80px",
          "add_concept_input" = "80px")
        
        # Render datatable
        
        render_datatable(output = output, ns = ns, i18n = i18n, data = r[[paste0(prefix, "_", type, "_vocabulary_mapped_concepts")]],
          output_name = paste0(type, "_vocabulary_mapped_concepts"), col_names =  col_names,
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, hidden_col = hidden_cols)
        
        # Create a proxy for datatatable
        r[[paste0(prefix, "_", type, "_vocabulary_mapped_concepts_proxy")]] <- DT::dataTableProxy(paste0(type, "_vocabulary_mapped_concepts"), deferUntilFlush = FALSE)
      }
      else DT::replaceData(r[[paste0(prefix, "_", type, "_vocabulary_mapped_concepts_proxy")]], r[[paste0(prefix, "_", type, "_vocabulary_mapped_concepts")]], resetPaging = FALSE, rownames = FALSE)
    })
    
    # --- --- --- --- --- --- -
    ## Vocabulary concepts ----
    # --- --- --- --- --- --- -
    
    # When add button is clicked
    
    observeEvent(input$widget_creation_concept_selected, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$widget_creation_concept_selected"))
      r[[paste0(prefix, "_widget_concept_selected")]] <- Sys.time()
      r[[paste0(prefix, "_widget_concept_selected_type")]] <- "widget_creation"
    })
    
    observeEvent(input$widget_settings_concept_selected, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$widget_settings_concept_selected"))
      r[[paste0(prefix, "_widget_concept_selected")]] <- Sys.time()
      r[[paste0(prefix, "_widget_concept_selected_type")]] <- "widget_settings"
    })
    
    observeEvent(r[[paste0(prefix, "_widget_concept_selected")]], {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer r$..widget_concept_selected"))
      
      type <- r[[paste0(prefix, "_widget_concept_selected_type")]]
      
      # Initiate r variable if doesn't exist
      if (length(r[[paste0(prefix, "_", type, "_vocabulary_selected_concepts")]]) == 0) r[[paste0(prefix, "_", type, "_vocabulary_selected_concepts")]] <- tibble::tibble(
        concept_id = integer(), concept_name = character(), concept_display_name = character(), domain_id = character(),
        mapped_to_concept_id = integer(), merge_mapped_concepts = logical())
      
      if (grepl("mapped", input[[paste0(type, "_concept_selected")]])) sub_type <- "mapped_concept"
      else sub_type <- "concept"
      
      # Get ID of selected concept
      link_id <- as.integer(substr(input[[paste0(type, "_concept_selected")]], nchar(paste0(id, "-", type, "_add_", sub_type, "_")) + 1, nchar(input[[paste0(type, "_concept_selected")]])))
      if (sub_type == "concept") concept_id <- link_id
      if (sub_type == "mapped_concept") concept_id <- r[[paste0(prefix, "_", type, "_vocabulary_mapped_concepts")]] %>% 
        dplyr::filter(id == link_id) %>% dplyr::pull(concept_id_2)
      
      # If this concept is not already selected, add it to the vocabulary_selected_concepts dropdown
      
      if (concept_id %not_in% r[[paste0(prefix, "_", type, "_vocabulary_selected_concepts")]]$concept_id){
        
        if (sub_type == "concept") new_data <- r[[paste0(prefix, "_", type, "_vocabulary_concepts")]] %>%
            dplyr::mutate_at("concept_id", as.integer) %>%
            dplyr::filter(concept_id == link_id) %>%
            dplyr::transmute(concept_id, concept_name, concept_display_name, domain_id, mapped_to_concept_id = NA_integer_, merge_mapped_concepts = FALSE)
        
        if (sub_type == "mapped_concept"){
          
          selected_concept <- r[[paste0(prefix, "_", type, "_vocabulary_mapped_concepts")]] %>%
            dplyr::mutate_at(c("concept_id_1", "concept_id_2"), as.integer) %>%
            dplyr::filter(id == link_id)
          
          new_data <- r[[paste0(prefix, "_", type, "_vocabulary_concepts")]] %>%
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
              r[[paste0(prefix, "_", type, "_vocabulary_selected_concepts")]] %>% 
                dplyr::filter(mapped_to_concept_id == selected_concept$concept_id_1) %>%
                dplyr::mutate(merge_mapped_concepts = input[[paste0(type, "_merge_mapped_concepts")]])
            )
          
          # Add also original concept, which concepts are mapped from
          r[[paste0(prefix, "_", type, "_vocabulary_selected_concepts")]] <- r[[paste0(prefix, "_", type, "_vocabulary_selected_concepts")]] %>% 
            dplyr::filter(concept_id != selected_concept$concept_id_1, (is.na(mapped_to_concept_id) | mapped_to_concept_id != selected_concept$concept_id_1))
        }
        
        r[[paste0(prefix, "_", type, "_vocabulary_selected_concepts")]] <- new_data %>% dplyr::bind_rows(r[[paste0(prefix, "_", type, "_vocabulary_selected_concepts")]])
      }
      
      # Update dropdown of selected concepts
      
      r[[paste0(prefix, "_widget_vocabulary_update_selected_concepts_dropdown")]] <- Sys.time()
      r[[paste0(prefix, "_widget_vocabulary_update_selected_concepts_dropdown_type")]] <- type
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer r$..widget_concept_selected"))
      
    })
    
    # When reset button is clicked
    observeEvent(input$widget_creation_reset_vocabulary_concepts, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$reset_vocabulary_concepts"))
      
      r[[paste0(prefix, "_widget_creation_vocabulary_selected_concepts")]] <- r[[paste0(prefix, "_widget_creation_vocabulary_selected_concepts")]] %>% dplyr::slice(0)
      r[[paste0(prefix, "_widget_vocabulary_selected_concepts_trigger")]] <- Sys.time()
      r[[paste0(prefix, "_widget_vocabulary_selected_concepts_trigger_type")]] <- "widget_creation"
    })
    
    observeEvent(input$widget_settings_reset_vocabulary_concepts, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$reset_vocabulary_concepts"))
      
      r[[paste0(prefix, "_widget_settings_vocabulary_selected_concepts")]] <- r[[paste0(prefix, "_widget_settings_vocabulary_selected_concepts")]] %>% dplyr::slice(0)
      r[[paste0(prefix, "_widget_vocabulary_selected_concepts_trigger")]] <- Sys.time()
      r[[paste0(prefix, "_widget_vocabulary_selected_concepts_trigger_type")]] <- "widget_settings"
    })
    
    # When dropdown is modified
    
    observeEvent(input$widget_creation_vocabulary_selected_concepts_trigger, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$widget_creation_vocabulary_selected_concepts_trigger"))
      r[[paste0(prefix, "_widget_vocabulary_selected_concepts_trigger")]] <- Sys.time()
      r[[paste0(prefix, "_widget_vocabulary_selected_concepts_trigger_type")]] <- "widget_creation"
    })
    observeEvent(input$widget_settings_vocabulary_selected_concepts_trigger, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$widget_settings_vocabulary_selected_concepts_trigger"))
      r[[paste0(prefix, "_widget_vocabulary_selected_concepts_trigger")]] <- Sys.time()
      r[[paste0(prefix, "_widget_vocabulary_selected_concepts_trigger_type")]] <- "widget_settings"
    })
    
    observeEvent(r[[paste0(prefix, "_widget_vocabulary_selected_concepts_trigger")]], {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer r$..widget_vocabulary_selected_concepts_trigger"))
      
      type <- r[[paste0(prefix, "_widget_vocabulary_selected_concepts_trigger_type")]]
      
      if (length(input[[paste0(type, "_vocabulary_selected_concepts")]]) == 0) r[[paste0(prefix, "_", type, "_vocabulary_selected_concepts")]] <- r[[paste0(prefix, "_", type, "_vocabulary_selected_concepts")]] %>% dplyr::slice(0)
      if (length(input[[paste0(type, "_vocabulary_selected_concepts")]]) > 0) {
        r[[paste0(prefix, "_", type, "_vocabulary_selected_concepts")]] <- r[[paste0(prefix, "_", type, "_vocabulary_selected_concepts")]] %>%
          dplyr::filter(concept_id %in% input[[paste0(type, "_vocabulary_selected_concepts")]])
        
        # Delete also mapped concepts
        r[[paste0(prefix, "_", type, "_vocabulary_selected_concepts")]] <- r[[paste0(prefix, "_", type, "_vocabulary_selected_concepts")]] %>%
          dplyr::filter(is.na(mapped_to_concept_id) | mapped_to_concept_id %in% r[[paste0(prefix, "_", type, "_vocabulary_selected_concepts")]]$concept_id)
      }
      
      r[[paste0(prefix, "_widget_vocabulary_update_selected_concepts_dropdown")]] <- Sys.time()
      r[[paste0(prefix, "_widget_vocabulary_update_selected_concepts_dropdown_type")]] <- type
    })
    
    # Update dropdown of selected concepts
    
    observeEvent(r[[paste0(prefix, "_widget_vocabulary_update_selected_concepts_dropdown")]], {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer r$..widget_vocabulary_update_selected_concepts_dropdown"))
      
      type <- r[[paste0(prefix, "_widget_vocabulary_update_selected_concepts_dropdown_type")]]
      
      options <- convert_tibble_to_list(
        r[[paste0(prefix, "_", type, "_vocabulary_selected_concepts")]] %>%
          dplyr::mutate(concept_name = ifelse(!is.na(mapped_to_concept_id), paste0("--- ", concept_name), concept_name)), 
        key_col = "concept_id", text_col = "concept_name", i18n = i18n)
      value <- r[[paste0(prefix, "_", type, "_vocabulary_selected_concepts")]] %>% dplyr::pull(concept_id)
      shiny.fluent::updateDropdown.shinyInput(session, paste0(type, "_vocabulary_selected_concepts"),
        options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
    })
    
    # --- --- --- --- --- --- --- ---
    ## Widget add button clicked ----
    # --- --- --- --- --- --- --- ---
    
    observeEvent(input$widget_creation_save, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer input$widget_creation_save"))
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      
      new_data <- list()
      
      new_data$name <- coalesce2(type = "char", x = input$widget_creation_name)
      new_data$tab_group <- r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(id == r[[paste0(prefix, "_selected_tab")]]) %>% dplyr::pull(tab_group_id)
      new_data$tab_new_element <- r[[paste0(prefix, "_selected_tab")]]
      new_data$plugin <- input$widget_creation_plugin$key
      
      # Check if name is not empty
      if (is.na(new_data$name)) shiny.fluent::updateTextField.shinyInput(session, "widget_creation_name", errorMessage = i18n$t("provide_valid_name"))
      else shiny.fluent::updateTextField.shinyInput(session, "widget_creation_name", errorMessage = NULL)
      req(!is.na(new_data$name))
      
      # Check if values required to be unique are unique
      
      table <- paste0(prefix, "_widgets")
      
      sql <- glue::glue_sql("SELECT DISTINCT(name) FROM {`table`} WHERE deleted IS FALSE AND tab_id = {new_data$tab_new_element}", .con = r$db)
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
      # last_row_widgets <- get_last_row(r$db, paste0(prefix, "_widgets"))
      widget_id <- get_last_row(r$db, paste0(prefix, "_widgets")) + 1
      last_display_order <- DBI::dbGetQuery(r$db, paste0("SELECT COALESCE(MAX(display_order), 0) FROM ", paste0(prefix, "_widgets"), " WHERE tab_id = ", new_data$tab_new_element)) %>% dplyr::pull() %>% as.integer()
      
      new_data <- tibble::tribble(~id, ~name, ~tab_id, ~plugin_id, ~display_order, ~creator_id, ~datetime, ~deleted,
        widget_id, as.character(new_data$name), as.integer(new_data$tab_new_element),
        as.integer(new_data$plugin), last_display_order + 1, r$user_id, as.character(Sys.time()), FALSE)
      
      DBI::dbAppendTable(r$db, paste0(prefix, "_widgets"), new_data)
      add_log_entry(r = r, category = paste0(table, " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_data))
      r[[paste0(prefix, "_widgets")]] <- r[[paste0(prefix, "_widgets")]] %>% dplyr::bind_rows(new_data)
      
      
      last_row_widgets_concepts <- get_last_row(m$db, paste0(prefix, "_widgets_concepts"))
      
      has_vocabulary_concepts <- TRUE
      selected_concepts <- tibble::tibble(concept_id = integer(), concept_name = character(), concept_display_name = character(), domain_id = character(),
        mapped_to_concept_id = integer(), merge_mapped_concepts = logical())
      
      if (length(r[[paste0(prefix, "_widget_creation_vocabulary_selected_concepts")]]) == 0) has_vocabulary_concepts <- FALSE
      if (length(r[[paste0(prefix, "_widget_creation_vocabulary_selected_concepts")]]) > 0) if (nrow(r[[paste0(prefix, "_widget_creation_vocabulary_selected_concepts")]]) == 0) has_vocabulary_concepts <- FALSE
      
      if (has_vocabulary_concepts){
        
        new_data <-
          r[[paste0(prefix, "_widget_creation_vocabulary_selected_concepts")]] %>%
          dplyr::transmute(
            id = 1:dplyr::n() + last_row_widgets_concepts + 1, widget_id = !!widget_id,
            concept_id, concept_name, concept_display_name, domain_id, mapped_to_concept_id, merge_mapped_concepts, 
            creator_id = r$user_id, datetime = as.character(Sys.time()), deleted = FALSE
          )
        
        DBI::dbAppendTable(m$db, paste0(prefix, "_widgets_concepts"), new_data)
        add_log_entry(r = r, category = paste0(table, " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_data))
        r[[paste0(prefix, "_widgets_concepts")]] <- r[[paste0(prefix, "_widgets_concepts")]] %>% dplyr::bind_rows(new_data)
        
        # Vocabulary concepts for server code
        selected_concepts <- r[[paste0(prefix, "_widget_creation_vocabulary_selected_concepts")]]
        
        # Reset r$..widget_vocabulary_selected_concepts & dropdown
        r[[paste0(prefix, "_widget_creation_vocabulary_selected_concepts")]] <- tibble::tibble(
          concept_id = integer(), concept_name = character(), concept_display_name = character(), domain_id = character(),
          mapped_to_concept_id = integer(), merge_mapped_concepts = logical())
        
        shiny.fluent::updateDropdown.shinyInput(session, "widget_creation_vocabulary_selected_concepts", options = list(), multiSelect = TRUE, multiSelectDelimiter = " || ")
      }
      
      show_message_bar(output, message = paste0(get_singular(paste0(prefix, "_widgets")), "_added"), type = "success", i18n = i18n, ns = ns)
      
      # Reset name textfield & dropdowns
      shiny.fluent::updateTextField.shinyInput(session, "widget_creation_name", value = "")
      
      # Load translations file
      
      plugin_id <- input$widget_creation_plugin$key
      
      plugin_translations <- r$code %>% dplyr::filter(link_id == plugin_id, category == "plugin_translations") %>% dplyr::pull(code)
      
      if (plugin_translations != ""){
        
        tryCatch({
          # Get plugin unique_id
          plugin_unique_id <- r$options %>% dplyr::filter(category == "plugin", name == "unique_id", link_id == plugin_id) %>% dplyr::pull(value)
          
          # Create plugin folder in translations folder if doesn't exist
          new_dir <- paste0(r$app_folder, "/translations/", plugin_unique_id)
          if (!dir.exists(new_dir)) dir.create(new_dir)
          
          # Create a csv with all languages
          data <- read.csv(text = plugin_translations, header = TRUE, stringsAsFactors = FALSE)
          
          # Create one csv by language
          for(lang in names(data)[-1]){
            file_name <- paste0(new_dir, "/translation_", lang, ".csv")
            
            if (!file.exists(file_name)){
              # Create a new dataframe with base & current language cols
              data_lang <- data[, c("base", lang)]
              
              # Create csv
              write.csv(data_lang, file_name, row.names = FALSE)
            }
          }
        },
          error = function(e) report_bug(r = r, output = output, error_message = "error_creating_translations_file",
            error_name = paste0(id, " - create translations files - plugin_id ", plugin_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
        
        tryCatch({
          i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = new_dir))
          i18np$set_translation_language(language)},
          error = function(e) report_bug(r = r, output = output, error_message = "error_creating_new_translator",
            error_name = paste0(id, " - create i18np translator - plugin_id ", plugin_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
      }
      
      # Run server code
      
      trace_code <- paste0(prefix, "_", widget_id, "_", m$selected_study)
      # if (trace_code %in% r$server_tabs_groups_loaded) print(trace_code)
      if (trace_code %not_in% r$server_tabs_groups_loaded){
        
        # Add the trace_code to loaded plugins list
        r$server_tabs_groups_loaded <- c(r$server_tabs_groups_loaded, trace_code)
        
        # Get plugin code
        
        # Check if plugin has been deleted
        check_deleted_plugin <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM plugins WHERE id = ", input$widget_creation_plugin$key)) %>% dplyr::pull(deleted)
        if (!check_deleted_plugin){
          
          # NB : req(m[[session_code]] == session_num) & req(m$selected_study == %study_id%) must be put at the beginning of each observeEvent in plugins code
          
          code_server_card <- r$code %>%
            dplyr::filter(link_id == input$widget_creation_plugin$key, category == "plugin_server") %>%
            dplyr::pull(code) %>%
            stringr::str_replace_all("%tab_id%", as.character(r[[paste0(prefix, "_selected_tab")]])) %>%
            stringr::str_replace_all("%widget_id%", as.character(widget_id)) %>%
            stringr::str_replace_all("%req%", "req(m[[session_code]] == session_num)\nreq(m$selected_study == %study_id%)") %>%
            stringr::str_replace_all("%study_id%", as.character(m$selected_study)) %>%
            stringr::str_replace_all("\r", "\n") %>%
            stringr::str_replace_all("''", "'")
          
          # If it is an aggregated plugin, change %study_id% with current selected study
          if (length(m$selected_study) > 0) code_server_card <- code_server_card %>% stringr::str_replace_all("%study_id%", as.character(m$selected_study))
        }
        else code_server_card <- ""
        
        # Create a session number, to inactivate older observers
        # Reset all older observers for this widget_id
        
        session_code <- paste0(prefix, "_widget_", widget_id)
        if (length(m[[session_code]]) == 0) session_num <- 1L
        if (length(m[[session_code]]) > 0) session_num <- m[[session_code]] + 1
        m[[session_code]] <- session_num
        
        # Variables to hide
        new_env_vars <- list("r" = NA)
        
        # Variables to keep
        variables_to_keep <- c("d", "m", "session_code", "session_num", "i18n", "selected_concepts", "debug")
        if (exists("i18np")) variables_to_keep <- c(variables_to_keep, "i18np")
        
        for (var in variables_to_keep) new_env_vars[[var]] <- eval(parse(text = var))
        new_env <- rlang::new_environment(data = new_env_vars, parent = pryr::where("r"))
        
        tryCatch(eval(parse(text = code_server_card), envir = new_env),
          error = function(e) report_bug(r = r, output = output, error_message = "error_run_plugin_server_code",
            error_name = paste0(id, " - add_new_widget - run_plugin_code - plugin_id ", input$widget_creation_plugin$key), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
        
        # Code for toggle reactivity
        toggle <- paste0(prefix, "_widget_", widget_id)
        
        observeEvent(input[[paste0(toggle, "_toggle")]], {
          if(input[[paste0(toggle, "_toggle")]]) shinyjs::show(toggle)
          else shinyjs::hide(toggle)
        })
        
        # Code for removing widget
        
        observeEvent(input[[paste0(prefix, "_remove_widget_", widget_id)]], {
          r[[paste0(prefix, "_selected_widget")]] <- widget_id
          r[[widget_delete_variable]] <- TRUE
        })
        
        # Code for widget settings
        
        observeEvent(input[[paste0(prefix, "_widget_settings_", widget_id)]], {
          r[[paste0(prefix, "_widget_settings_trigger")]] <- Sys.time()
          r[[paste0(prefix, "_widget_settings")]] <- widget_id
        })
        
      }
      
      # Prepare widget UI code
      tab_id <- r[[paste0(prefix, "_selected_tab")]]
      
      code_ui_card <- isolate(r$code) %>% dplyr::filter(link_id == input$widget_creation_plugin$key, category == "plugin_ui") %>% dplyr::pull(code)
      element_code <- div()
      widget_name <- r[[paste0(prefix, "_widgets")]] %>% dplyr::filter(widget_id == !!widget_id) %>% dplyr::slice(1) %>% dplyr::pull(name)
      
      code_ui_card <- code_ui_card %>%
        stringr::str_replace_all("%tab_id%", as.character(tab_id)) %>%
        stringr::str_replace_all("%widget_id%", as.character(widget_id)) %>%
        stringr::str_replace_all("%req%", "req(m[[session_code]] == session_num)\nreq(m$selected_study == %study_id%)") %>%
        stringr::str_replace_all("\r", "\n") %>%
        stringr::str_replace_all("''", "'") %>%
        stringr::str_replace_all("%study_id%", as.character(isolate(m$selected_study)))
      
      code_ui <- tryCatch(eval(parse(text = code_ui_card)), error = function(e){
        report_bug(r = r, output = output, error_message = "error_run_plugin_ui_code",
          error_name = paste0(id, " - run ui code - ", widget_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns)
        p(toString(e))
      }, warning = function(w){
        report_bug(r = r, output = output, error_message = "error_run_plugin_ui_code",
          error_name = paste0(id, " - run ui code - ", widget_id), category = "Error", error_report = toString(w), i18n = i18n, ns = ns)
        p(toString(w))
      })
      
      element_code <- div(
        make_shiny_ace_card("",
          div(
            div(id = ns(paste0(prefix, "_widget_plugin_ui_", widget_id)), code_ui),
            div(
              id = ns(paste0(prefix, "_widget_settings_remove_buttons_", widget_id)),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 2),
                actionButton(ns(paste0(prefix, "_widget_settings_", widget_id)), "", icon = icon("cog")),
                actionButton(ns(paste0(prefix, "_remove_widget_", widget_id)), "", icon = icon("trash-alt"))
              ),
              style = "position:absolute; top:8px; right: 10px;"
            )
          ),
          style = "position:relative;"
        )
      )
      
      # Remove toggles UI for this tab
      # removeUI(selector = paste0("#", ns(paste0(prefix, "_toggles_", tab_id))))
      shinyjs::hide(paste0(prefix, "_toggles_", tab_id))
      
      # Add the new toggles UI for this tab
      
      toggles <- tagList()
      # update_r(r = r, table = paste0(prefix, "_widgets"))
      widgets <- r[[paste0(prefix, "_widgets")]] %>% dplyr::filter(tab_id == !!tab_id) %>%
        dplyr::rename(widget_id = id) %>% dplyr::arrange(display_order)
      
      # Get widget widget_id
      distinct_widgets <- unique(widgets$widget_id)
      
      # Reset opened cards
      r[[paste0(prefix, "_opened_cards")]] <- ""
      
      # Loop over distinct cards (widgets), for this tab
      
      # Use sapply instead of for loop, cause with for loop, widget_id doesn't change
      sapply(distinct_widgets, function(widget_id){
        
        # Get name of widget
        widget_name <- widgets %>% dplyr::filter(widget_id == !!widget_id) %>% dplyr::slice(1) %>% dplyr::pull(name)
        
        toggles <<- tagList(toggles,
          shiny.fluent::Toggle.shinyInput(ns(paste0(prefix, "_widget_", widget_id, "_toggle")), value = TRUE, style = "margin-top:10px;"),
          div(class = "toggle_title", widget_name, style = "padding-top:10px;"))
        
        # Add to the list of opened cards
        r[[paste0(prefix, "_opened_cards")]] <- c(r[[paste0(prefix, "_opened_cards")]], paste0(prefix, "_widget_", widget_id))
      })
      
      toggles_div <- div(
        make_card("",
          shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
            shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_widget_", tab_id)), i18n$t("new_widget"), iconProps = list(iconName = "Add"),
              onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_add_widget_trigger', Math.random())"))),
            shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_edit_tab_", tab_id)), i18n$t("edit_tab"), iconProps = list(iconName = "Edit"),
              onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-edit_tab_trigger', Math.random())"))),
            div(style = "width:20px;"),
            toggles
          )
        )
      )
      
      r[[paste0(prefix, "_opened_cards")]] <- c(r[[paste0(prefix, "_opened_cards")]], paste0(prefix, "_toggles_", tab_id))
      
      # Show opened cards
      sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)
      
      # Add tab toggles UI
      # insertUI(selector = paste0("#", ns("study_cards")), where = "afterBegin", ui = uiOutput(ns(paste0(prefix, "_toggles_", tab_id))))
      output[[paste0(prefix, "_toggles_", tab_id)]] <- renderUI(toggles_div)
      
      # Add widget UI
      insertUI(selector = paste0("#", ns("study_cards")), where = "beforeEnd", ui = uiOutput(ns(paste0(prefix, "_widget_", widget_id))))
      output[[paste0(prefix, "_widget_", widget_id)]] <- renderUI(element_code)
      
      # Hide Add widget div
      shinyjs::hide(paste0(prefix, "_add_widget"))
      
      # Add this div to vector of cards
      r[[paste0(prefix, "_cards")]] <- c(isolate(r[[paste0(prefix, "_cards")]]), paste0(prefix, "_widget_", widget_id))
      
      # Reload UI menu
      r[[paste0(prefix, "_load_display_tabs")]] <- Sys.time()
      # r[[paste0(prefix, "_load_ui_menu")]] <- Sys.time()
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_data - ", id, " - observer input$widget_creation_save"))
    })
    
    # --- --- --- --- --- -
    ## Delete a widget ----
    # --- --- --- --- --- -
    
    widget_delete_prefix <- paste0(prefix, "_widget")
    widget_dialog_title <- paste0(prefix, "_widgets_delete")
    widget_dialog_subtext <- paste0(prefix, "_widgets_delete_subtext")
    widget_react_variable <- "widget_delete_confirm"
    widget_table <- paste0(prefix, "_widgets")
    widget_id_var_sql <- "id"
    widget_id_var_r <- paste0(prefix, "_selected_widget")
    widget_delete_message <- paste0(prefix, "_widget_deleted")
    widget_reload_variable <- paste0(prefix, "_load_ui")
    widget_delete_variable <- paste0(widget_delete_prefix, "_open_dialog")
    widget_information_variable <- paste0(prefix, "_widget_deleted")
    
    delete_element(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
      delete_prefix = widget_delete_prefix, dialog_title = widget_dialog_title, dialog_subtext = widget_dialog_subtext,
      react_variable = widget_react_variable, table = widget_table, id_var_sql = widget_id_var_sql, id_var_r = widget_id_var_r,
      delete_message = widget_delete_message, translation = TRUE, reload_variable = widget_reload_variable,
      information_variable = widget_information_variable)
    
    # When a tab is element deleted, remove UI and reload toggles UI
    observeEvent(r[[widget_information_variable]], {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_data - ", id, " - observer r$..widget_deleted"))
      
      # table <- paste0(prefix, "_tabs")
      # deleted_tab_id <- r[[paste0(prefix, "_widget_group_deleted")]]
      # sql <- glue::glue_sql("SELECT * FROM {`table`} WHERE id = {deleted_tab_id}", .con = r$db)
      # tab_deleted <- DBI::dbGetQuery(r$db, sql)
      
      widget_id <- r[[paste0(prefix, "_widget_deleted")]]
      # Remove UI card
      removeUI(selector = paste0("#", ns(paste0(prefix, "_widget_", r[[paste0(prefix, "_widget_deleted")]]))))
      
      sql <- glue::glue_sql("SELECT DISTINCT(tab_id) FROM {`paste0(prefix, '_widgets')`} WHERE id = {widget_id}", .con = r$db)
      tab_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
      
      # Remove toggles UI for this tab
      # removeUI(selector = paste0("#", ns(paste0(prefix, "_toggles_", tab_id))))
      shinyjs::hide(paste0(prefix, "_toggles_", tab_id))
      
      # Add the new toggles UI for this tab
      
      toggles <- tagList()
      r[[paste0(prefix, "_widgets")]] <- r[[paste0(prefix, "_widgets")]] %>% dplyr::filter(id != widget_id)
      r[[paste0(prefix, "_widgets_concepts")]] <- r[[paste0(prefix, "_widgets_concepts")]] %>% dplyr::filter(widget_id != !!widget_id)
      
      # update_r(r = r, table = paste0(prefix, "_widgets"))
      widgets <- r[[paste0(prefix, "_widgets")]] %>% dplyr::filter(tab_id == !!tab_id) %>% 
        dplyr::rename(widget_id = id) %>% dplyr::arrange(display_order)
      
      # Get widget widget_id
      distinct_widgets <- unique(widgets$widget_id)
      
      # Reset opened cards
      r[[paste0(prefix, "_opened_cards")]] <- ""
      
      # Loop over distinct cards (tabs elements), for this tab
      # Use sapply instead of for loop, cause with for loop, widget_id doesn't change
      sapply(distinct_widgets, function(widget_id){
        
        # Get name of widget
        widget_name <- widgets %>% dplyr::filter(widget_id == !!widget_id) %>% dplyr::slice(1) %>% dplyr::pull(name)
        
        toggles <<- tagList(toggles,
          shiny.fluent::Toggle.shinyInput(ns(paste0(prefix, "_widget_", widget_id, "_toggle")), value = TRUE, style = "margin-top:10px;"),
          div(class = "toggle_title", widget_name, style = "padding-top:10px;"))
        
        # Add to the list of opened cards
        r[[paste0(prefix, "_opened_cards")]] <- c(r[[paste0(prefix, "_opened_cards")]], paste0(prefix, "_widget_", widget_id))
      })
      
      # Does this tab have sub-tabs ?
      if (r[[paste0(prefix, "_tabs")]] %>% dplyr::filter(parent_tab_id == tab_id) %>% nrow() > 0) toggles_div <- div(
        make_card("",
          shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
            shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_edit_tab_", tab_id)), i18n$t("edit_tab"), iconProps = list(iconName = "Edit"),
              onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-edit_tab_trigger', Math.random())"))),
            div(shiny.fluent::MessageBar(i18n$t("tab_contains_sub_tabs"), messageBarType = 5), style = "margin-top:4px;")
          )
        )
      )
      
      else toggles_div <- div(
        make_card("",
          shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
            shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_widget_", tab_id)), i18n$t("new_widget"), iconProps = list(iconName = "Add"),
              onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", prefix, "_add_widget_trigger', Math.random())"))),
            shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_edit_tab_", tab_id)), i18n$t("edit_tab"), iconProps = list(iconName = "Edit"),
              onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-edit_tab_trigger', Math.random())"))),
            div(style = "width:20px;"),
            toggles
          )
        )
      )
      
      r[[paste0(prefix, "_opened_cards")]] <- c(r[[paste0(prefix, "_opened_cards")]], paste0(prefix, "_toggles_", tab_id))
      
      # Show opened cards
      sapply(r[[paste0(prefix, "_opened_cards")]], shinyjs::show)
      
      # Hide edit tab card
      shinyjs::hide(paste0(prefix, "_edit_tab"))
      
      # Add tab toggles UI
      output[[paste0(prefix, "_toggles_", tab_id)]] <- renderUI(toggles_div)
    })
  })
}
