#' settings_data_management UI Function
#'
#' @description Shiny tab of settings / data management
#'
#' @param id ID of the tab (character)
#' @param language Language used (character)
#' @noRd 
#' @importFrom shiny NS tagList 

mod_settings_data_management_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  result <- div()
 
  # Dropdowns shown in datatable for each page
  dropdowns <- tibble::tribble(~id, ~dropdowns,
    "settings_data_sources", "",
    "settings_datasets", "data_source",
    "settings_vocabularies", "data_source")
  
  cards <- c("datatable_card", "edit_code_card", "options_card", "vocabularies_tables_datatable_card", "import_vocabulary_card")
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  })
  
  # --- --- --- --- --- --
  # Data sources card ----
  # --- --- --- --- --- --
  
  if (id == "settings_data_sources"){
      div(class = "main",
        render_settings_default_elements(ns = ns),
        shiny.fluent::reactOutput(ns("help_panel")),
        shiny.fluent::reactOutput(ns("help_modal")),
        shiny.fluent::Breadcrumb(items = list(
          list(key = "data_sources", text = i18n$t("data_sources"))
        ), maxDisplayedItems = 3),
        
        # --- --- --- --- -
        ## Pivot items ----
        # --- --- --- --- -
        
        shiny.fluent::Pivot(
          onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
          shiny.fluent::PivotItem(id = "datatable_card", itemKey = "datatable_card", headerText = i18n$t("data_sources_management"))
        ),
        
        forbidden_cards,
        
        # --- --- --- --- -- -
        # Management card ----
        # --- --- --- --- -- -
        
        render_settings_datatable_card(i18n = i18n, ns = ns, div_id = "datatable_card", title = "data_sources_management", inputs = c("name" = "textfield"))
      ) -> result
    }
  
  # --- --- --- --- ---
  # Datasets card ----
  # --- --- --- --- ---
  
  if (id == "settings_datasets"){
    div(class = "main",
      render_settings_default_elements(ns = ns),
      shiny.fluent::reactOutput(ns("help_panel")),
      shiny.fluent::reactOutput(ns("help_modal")),
      shiny.fluent::Breadcrumb(items = list(
        list(key = "datasets", text = i18n$t("datasets"))
      ), maxDisplayedItems = 3),
      
      # --- --- --- --- -
      ## Pivot items ----
      # --- --- --- --- -
      
      shiny.fluent::Pivot(
        id = ns("datasets_pivot"),
        onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
        shiny.fluent::PivotItem(id = "datatable_card", itemKey = "datatable_card", headerText = i18n$t("datasets_management")),
        shiny.fluent::PivotItem(id = "edit_code_card", itemKey = "edit_code_card", headerText = i18n$t("edit_dataset_code")),
        shiny.fluent::PivotItem(id = "options_card", itemKey = "options_card", headerText = i18n$t("dataset_options"))
      ),
      
      forbidden_cards,
      
      # --- --- --- --- -- -
      # Management card ----
      # --- --- --- --- -- -
      
      render_settings_datatable_card(i18n = i18n, ns = ns, div_id = "datatable_card", title = "datasets_management", 
        inputs = c("name" = "textfield", "data_source" = "dropdown")),
      
      # --- --- --- --- -- -
      # Edit code card ----
      # --- --- --- --- -- -
      
      div(id = ns("edit_code_card"), 
        make_shiny_ace_card(i18n$t("edit_dataset_code"),
          div(
            shiny.fluent::Stack(
              tokens = list(childrenGap = 5),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                make_combobox(i18n = i18n, ns = ns, label = "dataset", id = "code_selected_dataset_or_vocabulary",
                  width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
                div(shiny.fluent::Toggle.shinyInput(ns("hide_editor"), value = FALSE), style = "margin-top:45px;"),
                div(i18n$t("hide_editor"), style = "font-weight:bold; margin-top:45px; margin-right:30px;")
              ),
              conditionalPanel(condition = "input.hide_editor == true", ns = ns, br())
            ),
            conditionalPanel(condition = "input.hide_editor == false", ns = ns,
              div(shinyAce::aceEditor(
                ns("ace_edit_code"), "", mode = "r", 
                code_hotkeys = list("r", list(
                  run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
                  run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
                  save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
                  comment = list(win = "CTRL-SHIFT-C", mac = "CTRL-SHIFT-C|CMD-SHIFT-C"))
                ),
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000),
                style = "width: 100%;"
              )
            ),
            shiny.fluent::Stack(
              tokens = list(childrenGap = 5),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("execute_code"), i18n$t("run_code")),
                shiny.fluent::DefaultButton.shinyInput(ns("edit_code_save"), i18n$t("save"))
              ), br(), br(),
              div(textOutput(ns("datetime_code_execution")), style = "color:#878787;"), br(),
              div(shiny::uiOutput(ns("code_result")), 
                style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;"), br(), br(),
              shiny.fluent::Stack(
                horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::Toggle.shinyInput(ns("show_imported_data"), value = FALSE),
                div(i18n$t("show_imported_data"), style = "font-weight:bold;"),
              ),
              conditionalPanel(condition = "input.show_imported_data == true", ns = ns,
                DT::DTOutput(ns("code_datatable")))
            )
          )
        ), br()
      ),
      
      # --- --- --- --- --- ---
      # Edit options card ----
      # --- --- --- --- --- ---
      
      div(id = ns("options_card"),
        make_card(i18n$t("dataset_options"),
          div(
            make_combobox(i18n = i18n, ns = ns, label = "dataset", id = "options_selected_dataset_or_vocabulary", width = "300px", allowFreeform = FALSE, multiSelect = FALSE), br(),
            make_dropdown(i18n = i18n, ns = ns, label = "omop_version", width = "300px", 
              options = list(
                list(key = "5.3", text = "5.3"),
                list(key = "5.4", text = "5.4"),
                list(key = "6.0", text = "6.0")
              )), br(), br(),
            shiny.fluent::Stack(
              horizontal = TRUE, tokens = list(childrenGap = 10),
              make_toggle(i18n = i18n, ns = ns, label = "show_only_aggregated_data", inline = TRUE)
            ), br(),
            div(
              div(class = "input_title", paste0(i18n$t("grant_access_to"), " :")),
              shiny.fluent::ChoiceGroup.shinyInput(ns("users_allowed_read_group"), options = list(
                list(key = "everybody", text = i18n$t("everybody")),
                list(key = "people_picker", text = i18n$t("choose_users"))
              ), className = "inline_choicegroup"),
              conditionalPanel(condition = "input.users_allowed_read_group == 'people_picker'", ns = ns,
                uiOutput(ns("users_allowed_read_div"))
              )
            ), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("options_save"), i18n$t("save"))
          )
        ), br()
      )
    ) -> result
  }
  
  # --- --- --- --- --- --
  # Vocabularies card ----
  # --- --- --- --- --- --
  
  if (id == "settings_vocabularies"){
    div(class = "main",
      render_settings_default_elements(ns = ns),
      shiny.fluent::reactOutput(ns("help_panel")),
      shiny.fluent::reactOutput(ns("help_modal")),
      shiny.fluent::reactOutput(ns("vocabularies_table_delete_confirm")),
      shiny.fluent::Breadcrumb(items = list(
        list(key = "vocabularies", text = i18n$t("vocabularies"))
      ), maxDisplayedItems = 3),
      
      # --- --- --- --- -
      ## Pivot items ----
      # --- --- --- --- -
      
      shiny.fluent::Pivot(
        id = ns("vocabularies_pivot"),
        onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
        shiny.fluent::PivotItem(id = "datatable_card", itemKey = "datatable_card", headerText = i18n$t("vocabularies_management")),
        shiny.fluent::PivotItem(id = "edit_code_card", itemKey = "edit_code_card", headerText = i18n$t("edit_vocabulary_code")),
        shiny.fluent::PivotItem(id = "vocabularies_tables_datatable_card", itemKey = "vocabularies_tables_datatable_card", headerText = i18n$t("vocabularies_tables")),
        shiny.fluent::PivotItem(id = "import_vocabulary_card", itemKey = "import_vocabulary_card", headerText = i18n$t("import_vocabulary"))
      ),
      
      
      forbidden_cards,
      
      # --- --- --- --- -- -
      # Management card ----
      # --- --- --- --- -- -
      
      div(id = ns("datatable_card"),
        make_card(i18n$t("vocabularies_management"),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              make_textfield(i18n = i18n, ns = ns, label = "id", id = "vocabulary_id", width = "300px"),
              make_textfield(i18n = i18n, ns = ns, label = "name", id = "vocabulary_name", width = "300px"),
              make_dropdown(i18n = i18n, ns = ns, label = "data_source", id = "data_source", width = "300px", multiSelect = TRUE),
            div(shiny.fluent::PrimaryButton.shinyInput(ns("add"), i18n$t("add")), style = "margin-top:38px;")),
            div(DT::DTOutput(ns("management_datatable")), style = "z-index:2"),
            div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("management_save"), i18n$t("save")),
                shiny.fluent::DefaultButton.shinyInput(ns("delete_selection"), i18n$t("delete_selection"))
              ),
              style = "position:relative; z-index:1; margin-top:-30px; width:500px;"
            )
          )
        ), br()
      ),
      
      # --- --- --- --- -- -
      # Edit code card ----
      # --- --- --- --- -- -
      
      div(id = ns("edit_code_card"), 
        make_shiny_ace_card(i18n$t("edit_vocabulary_code"),
          div(
            shiny.fluent::Stack(
              tokens = list(childrenGap = 5),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                make_combobox(i18n = i18n, ns = ns, label = "vocabulary", id = "code_selected_dataset_or_vocabulary",
                  width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
                div(shiny.fluent::Toggle.shinyInput(ns("hide_editor"), value = FALSE), style = "margin-top:45px;"),
                div(i18n$t("hide_editor"), style = "font-weight:bold; margin-top:45px; margin-right:30px;"), 
              ),
              conditionalPanel(condition = "input.hide_editor == true", ns = ns, br())
            ),
            conditionalPanel(condition = "input.hide_editor == false", ns = ns,
              div(shinyAce::aceEditor(
                ns("ace_edit_code"), "", mode = "r", 
                code_hotkeys = list(
                  "r", list(
                    run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
                    run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
                    save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
                    comment = list(win = "CTRL-SHIFT-C", mac = "CTRL-SHIFT-C|CMD-SHIFT-C"))
                ),
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), 
              style = "width: 100%;")
            ),
            shiny.fluent::Stack(
              tokens = list(childrenGap = 5),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("execute_code"), i18n$t("run_code")),
                shiny.fluent::DefaultButton.shinyInput(ns("edit_code_save"), i18n$t("save"))
              ), br(), br(),
              div(textOutput(ns("datetime_code_execution")), style = "color:#878787;"), br(),
              div(shiny::uiOutput(ns("code_result")), 
                style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
            )
          )
        ), br()
      ),
      
      # --- --- --- --- --- --- --- -
      # Vocabularies tables card ----
      # --- --- --- --- --- --- --- -
      
      div(id = ns("vocabularies_tables_datatable_card"),
        make_card(i18n$t("vocabularies_tables"),
          div(
            shiny.fluent::Stack(
              horizontal = TRUE, tokens = list(childrenGap = 20),
              make_dropdown(i18n = i18n, ns = ns, label = "table", id = "vocabularies_table", width = "300px",
                options = list(
                  list(key = "concept", text = "CONCEPT"),
                  list(key = "domain", text = "DOMAIN"),
                  list(key = "concept_class", text = "CONCEPT_CLASS"),
                  list(key = "concept_relationship", text = "CONCEPT_RELATIONSHIP"),
                  list(key = "relationship", text = "RELATIONSHIP"),
                  list(key = "concept_synonym", text = "CONCEPT_SYNONYM"),
                  list(key = "concept_ancestor", text = "CONCEPT_ANCESTOR"),
                  list(key = "drug_strength", text = "DRUG_STRENGTH")
                )),
              conditionalPanel(condition = "['concept', 'concept_relationship', 'concept_synonym', 'concept_ancestor', 'drug_strength'].includes(input.vocabularies_table)", ns = ns,
                make_combobox(i18n = i18n, ns = ns, label = "vocabulary", id = "vocabularies_table_vocabulary", allowFreeform = FALSE, multiSelect = FALSE, width = "300px"))
            ),
            shiny.fluent::Stack(
              horizontal = TRUE, tokens = list(childrenGap = 20),
              make_dropdown(i18n = i18n, ns = ns, label = "rows", id = "vocabularies_table_rows", width = "300px"),
              make_dropdown(i18n = i18n, ns = ns, label = "columns", id = "vocabularies_table_cols", width = "300px", multiSelect = TRUE),
              conditionalPanel(condition = "input.vocabularies_table == 'concept'", ns = ns,
                div(shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                  make_toggle(i18n = i18n, ns = ns, label = "vocabularies_datatable_show_mapped_concepts", inline = TRUE), style = "margin-top:45px;"))),
              conditionalPanel(condition = "['concept_relationship', 'concept_synonym', 'concept_ancestor', 'drug_strength'].includes(input.vocabularies_table)", ns = ns,
                div(shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                  make_toggle(i18n = i18n, ns = ns, label = "vocabularies_datatable_show_row_details", inline = TRUE), style = "margin-top:45px;")))
            ),
            DT::DTOutput(ns("vocabularies_tables_datatable")), br(),
            conditionalPanel(condition = "input.vocabularies_table == null", ns = ns, div(br(), br(), br())),
            div(
              shiny.fluent::Stack(
                horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("vocabularies_tables_datatable_save"), i18n$t("save")),
                shiny.fluent::DefaultButton.shinyInput(ns("vocabularies_tables_delete_selection"), i18n$t("delete_selection"))
              ),
              style = "position:relative; margin-top:-50px; width:500px;"
            ),
            conditionalPanel(condition = "input.vocabularies_datatable_show_mapped_concepts == true && input.vocabularies_table == 'concept'", ns = ns, 
              br(), hr(), br(),
              DT::DTOutput(ns("vocabularies_tables_mapped_concepts_datatable"))
            ),
            conditionalPanel(condition = paste0("['concept_relationship', 'concept_synonym', 'concept_ancestor', 'drug_strength'].includes(input.vocabularies_table) && ",
              "input.vocabularies_datatable_show_row_details == true"), ns = ns, 
              br(),
              div(uiOutput(ns("vocabularies_datatable_row_details")), 
                style = "width: 99%; border-style: dashed; border-width: 1px; padding: 8px; margin-right: 5px;")
            )
          )
        ), br()
      ),
      
      # --- --- --- --- --- --- ---
      # Import vocabulary card ----
      # --- --- --- --- --- --- ---
      
      div(id = ns("import_vocabulary_card"),
        make_card(i18n$t("import_vocabulary"),
          div(
            shiny.fluent::ChoiceGroup.shinyInput(ns("import_vocabulary_data_type"), value = "zip",
              options = list(
                list(key = "zip", text = i18n$t("zip")),
                list(key = "csv", text = i18n$t("csv"))
              ), className = "inline_choicegroup"
            ), br(),
            conditionalPanel("input.import_vocabulary_data_type == 'zip'", ns = ns,
              shiny.fluent::DefaultButton.shinyInput(ns("import_vocabulary_browse_zip"), i18n$t("choose_zip_file"), style = "width:270px;")),
            conditionalPanel("input.import_vocabulary_data_type == 'csv'", ns = ns,
              shiny.fluent::DefaultButton.shinyInput(ns("import_vocabulary_browse_csv"), i18n$t("choose_csv_files"), style = "width:270px;")), br(),
            uiOutput(ns("import_vocabulary_status")), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("import_vocabulary_button"), i18n$t("import_vocabulary"), iconProps = list(iconName = "Download"), style = "width:270px;"), br(),
            shinyjs::hidden(
              div(
                id = ns("imported_vocabularies_div"), br(),
                strong(i18n$t("imported_data")),
                div(DT::DTOutput(ns("imported_vocabularies")))
              )
            ),
            div(style = "display:none;", fileInput(ns("import_vocabulary_upload_zip"), label = "", multiple = FALSE, accept = ".zip")),
            div(style = "display:none;", fileInput(ns("import_vocabulary_upload_csv"), label = "", multiple = TRUE, accept = ".csv"))
          )
        )
      )
      
    ) -> result
  }
  result
}
    
#' settings_data_management Server Functions
#'
#' @param id ID of the tab (character)
#' @param r Shiny reactive value
#' @param language Language used (character)
#' @noRd 

mod_settings_data_management_server <- function(id = character(), r = shiny::reactiveValues(),
  d = shiny::reactiveValues(), m = shiny::reactiveValues(), i18n = character(), 
  language = "en", perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (perf_monitoring) monitor_perf(r = r, action = "start")
    if (debug) print(paste0(Sys.time(), " - mod_settings_data_management_server - start"))
    
    # Dropdowns in the management datatable, by page
    dropdowns <- tibble::tribble(~id, ~dropdowns,
      "settings_data_sources", "",
      "settings_datasets", "data_source",
      "settings_vocabularies", "data_source")
    
    # Close message bar
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    # Table name
    table <- substr(id, nchar("settings_") + 1, nchar(id))
    if (id == "settings_vocabularies") table <- "vocabulary"
    
    # --- --- --- --- --- -
    # Update dropdowns ----
    # --- --- --- --- --- -
    
    if (table %in% c("datasets", "vocabulary")){
      observeEvent(r[[table]], {
        
        if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$", table, " - updateComboBox"))
        
        if (table == "datasets"){
          options <- convert_tibble_to_list(r[[table]] %>% dplyr::arrange(name), key_col = "id", text_col = "name")
          shiny.fluent::updateComboBox.shinyInput(session, "options_selected_dataset_or_vocabulary", options = options)
        }
        
        if (table == "vocabulary"){
          options <- convert_tibble_to_list(r[[table]] %>% dplyr::arrange(vocabulary_id), key_col = "id", text_col = "vocabulary_id")
          shiny.fluent::updateComboBox.shinyInput(session, "vocabulary_tables_selected_vocabulary", options = options)
        }
        
        shiny.fluent::updateComboBox.shinyInput(session, "code_selected_dataset_or_vocabulary", options = options)
      })
    }
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    # Toggles IDs
    cards <- c("creation_card", "datatable_card", "edit_code_card", "options_card", "vocabularies_tables_datatable_card", "import_vocabulary_card")
    sapply(cards, shinyjs::hide)
    
    show_or_hide_cards(r = r, input = input, session = session, table = table, id = id, cards = cards)
    
    # Show first card
    
    if (paste0(get_plural(table), "_datatable_card") %in% r$user_accesses) shinyjs::show("datatable_card")
    else shinyjs::show("datatable_card_forbidden")
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    observeEvent(input$help, if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r[[paste0("help_settings_data_management_", table, "_open_panel")]] <- TRUE)
    observeEvent(input$hide_panel, r[[paste0("help_settings_data_management_", table, "_open_panel")]] <- FALSE)
    
    r[[paste0("help_settings_data_management_", table, "_open_panel_light_dismiss")]] <- TRUE
    observeEvent(input$show_modal, r[[paste0("help_settings_data_management_", table, "_open_modal")]] <- TRUE)
    observeEvent(input$hide_modal, {
      r[[paste0("help_settings_data_management_", table, "_open_modal")]] <- FALSE
      r[[paste0("help_settings_data_management_", table, "_open_panel_light_dismiss")]] <- TRUE
    })
    
    observeEvent(shiny.router::get_page(), {
      if (debug) print(paste0(Sys.time(), " - mod_settings_data_managemenent - ", id, " - observer shiny_router::change_page"))
      
      # Close help pages when page changes
      r[[paste0("help_settings_data_management_", table, "_open_panel")]] <- FALSE
      r[[paste0("help_settings_data_management_", table, "_open_modal")]] <- FALSE
    })
    
    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_settings_data_management_", table, "_page_", i)]] <- Sys.time())
    })
    
    help_settings_data_management(output = output, r = r, id = id, prefix = table, language = language, i18n = i18n, ns = ns)
    
    observeEvent(input$copy_code_1, r$help_settings_data_management_copy_code_1 <- Sys.time())
    observeEvent(input$copy_code_2, r$help_settings_data_management_copy_code_2 <- Sys.time())
    
    # --- --- --- --- --- --
    # Add a new element ----
    # --- --- --- --- --- --
    
    # Update dropdowns with reactive data
    
    uploaded_dropdowns <- dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist()
    
    sapply(uploaded_dropdowns, 
      function(data_var){
        
        # Create only needed observers for current page
        if (data_var != ""){
          data_var <- get_plural(data_var)
          observeEvent(r[[data_var]], {
            
            if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$", table))
            
            # Convert options to list
            options <- convert_tibble_to_list(data = r[[data_var]] %>% dplyr::arrange(name), key_col = "id", text_col = "name")
            shiny.fluent::updateDropdown.shinyInput(session, get_singular(word = data_var), options = options)
          })
        }
      })
    
    # When add button is clicked
    observeEvent(input$add, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$add"))
      
      # Create a list with new data
      # If page = vocabulary, data_source is character, not integer (multiple choices)
      new_data <- list()
      if (id == "settings_vocabularies") new_data_var <- c("vocabulary_id" = "char", "vocabulary_name" = "char", "data_source" = "char")
      else new_data_var <- c("name" = "char", "description" = "char", "data_source" = "int", "dataset" = "int", 
        "study" = "int", "patient_lvl_tab_group" = "int", "aggregated_tab_group" = "int")
      
      sapply(names(new_data_var),
        function(input_name){
          new_data[[input_name]] <<- coalesce2(type = new_data_var[[input_name]], x = input[[input_name]])
      })
      
      # Convert data_source to string, for page vocabularies
      if (id == "settings_vocabularies"){
        if (length(new_data$data_source) == 1) new_data$data_source <- coalesce2(type = "char", x = input$data_source)
        else new_data$data_source <- toString(as.integer(new_data$data_source))
        
        add_settings_new_data(session = session, output = output, r = r, m = m, i18n = i18n, id = id, 
          data = new_data, table = "vocabulary", required_textfields = "vocabulary_id", req_unique_values = "vocabulary_id",
          dropdowns = dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist())
      }
      else add_settings_new_data(session = session, output = output, r = r, m = m, i18n = i18n, id = id, 
        data = new_data, table = substr(id, nchar("settings_") + 1, nchar(id)), 
        required_textfields = "name", req_unique_values = "name",
        dropdowns = dropdowns %>% dplyr::filter(id == !!id) %>% dplyr::pull(dropdowns) %>% unlist())
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer input$add"))
    })
    
    # --- --- --- --- --- ---
    # Generate datatable ----
    # --- --- --- --- --- ---
  
    dropdowns_datatable <- switch(table,
      "data_sources" = "",
      "datasets" = "",
      "vocabulary" = c("data_source_id" = "data_sources"))
    
    # Dropdowns with multiSelect
    dropdowns_multiselect <- ""
    if (table == "vocabulary") dropdowns_multiselect <- "data_source_id"
    
    # Action buttons for each tab / page
    action_buttons = switch(table,
      "data_sources" = "delete",
      "datasets" = c("delete", "edit_code", "options"),
      "vocabulary" = c("delete", "edit_code")
    )
    
    # Editable cols
    if (table == "vocabulary") editable_cols <- c("vocabulary_id", "vocabulary_name")
    else editable_cols <- c("name", "description")
    
    # Sortable cols
    if (table == "vocabulary") sortable_cols <- c("vocabulary_id", "vocabulary_name", "datetime")
    else sortable_cols <- c("id", "name", "description", "dataset_id", "data_source_id", "study_id", "creator_id", "datetime")
    
    # Column widths
    column_widths <- c("datetime" = "130px", "creator_id" = "200px", "action" = "100px")
    
    # Centered columns
    centered_cols <- c("creator_id", "datetime", "action")
    
    # Searchable_cols
    if (table == "vocabulary") searchable_cols <- c("vocabulary_id", "vocabulary_name")
    else searchable_cols <- c("name", "description", "data_source_id", "dataset_id", "study_id", "creator_id")
    
    # Factorize_cols
    factorize_cols <- switch(table,
      "data_sources" = "creator_id",
      "datasets" = c("data_source_id", "creator_id"),
      "vocabulary" = "creator_id")
    
    if (table == "vocabulary") hidden_cols <- c("id", "vocabulary_reference", "vocabulary_version", "vocabulary_concept_id",
      "display_order", "creator_id", "deleted", "modified")
    else hidden_cols <- c("id", "description", "deleted", "modified")
    
    # Reload datatable
    
    observeEvent(r[[table]], {
      
      if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$", table, " - reload datatable"))
      
      r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
    })
    
    observeEvent(r[[paste0(table, "_temp")]], {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$", table, "_temp"))
      
      if (nrow(r[[paste0(table, "_temp")]]) == 0){
        
        if (table == "data_sources") data <- tibble::tibble(id = integer(), name = character(), description = character(),
          creator_id = integer(), datetime = character(), deleted = integer(), modified = logical(), action = character())
        if (table == "datasets") data <- tibble::tibble(id = integer(), name = character(), description = character(),
          data_source_id = character(), creator_id = integer(), datetime = character(), deleted = integer(), modified = logical(), action = character())
        if (table == "vocabulary") data <- tibble::tibble(id = integer(), vocabulary_id = character(), vocabulary_name = character(), 
          vocabulary_reference = character(), vocabulary_version = character(), vocabulary_concept_id = character(), data_source_id = character(), 
          display_order = integer(), creator_id = integer(), datetime = character(), deleted = integer(), modified = logical(), action = character())
      }
      
      if (nrow(r[[paste0(table, "_temp")]]) > 0){

        r[[paste0(table, "_datatable_temp")]] <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
          table = table, dropdowns = dropdowns_datatable, dropdowns_multiselect = dropdowns_multiselect, factorize_cols = factorize_cols,
          action_buttons = action_buttons, data_input = r[[paste0(table, "_temp")]])
        
        # Replace empty data_source_id by "deleted data source"
        if (table == "datasets"){
          r[[paste0(table, "_datatable_temp")]] <- r[[paste0(table, "_datatable_temp")]] %>%
            dplyr::mutate_at("data_source_id", as.character) %>%
            dplyr::mutate(data_source_id = dplyr::case_when(
              data_source_id == "" ~ i18n$t("deleted_data_source"), TRUE ~ data_source_id
            ))
        }
        data <- r[[paste0(table, "_datatable_temp")]]
      }
      
      if (length(r[[paste0(table, "_datatable_proxy")]]) == 0){
        
        render_datatable(output = output, ns = ns, i18n = i18n, data = data,
          output_name = "management_datatable", col_names = get_col_names(table_name = table, i18n = i18n),
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols, selection = "multiple")

        r[[paste0(table, "_datatable_proxy")]] <- DT::dataTableProxy("management_datatable", deferUntilFlush = FALSE)
      }
      
      if (length(r[[paste0(table, "_datatable_proxy")]]) > 0){
        DT::replaceData(r[[paste0(table, "_datatable_proxy")]], data, resetPaging = FALSE, rownames = FALSE)
      }
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer r$", table, "_temp"))
    })
    
    # --- --- --- --- --- --- --- --
    # Save updates in datatable ----
    # --- --- --- --- --- --- --- --
    
      # Each time a row is updated, modify temp variable
      # Do that for main datatable (management_datatable) & vocabularies_tables_datatable
      observeEvent(input$management_datatable_cell_edit, {
        
        if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$management_datatable_cell_edit"))
        
        edit_info <- input$management_datatable_cell_edit
        r[[paste0(table, "_temp")]] <- DT::editData(r[[paste0(table, "_temp")]], edit_info, rownames = FALSE)
        # Store that this row has been modified
        r[[paste0(table, "_temp")]][[edit_info$row, "modified"]] <- TRUE
      })
  
      # Each time a dropdown is updated, modify temp variable
      observeEvent(r[[table]], {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$", table, " - update dropdowns"))
        
        update_settings_datatable(input = input, tab_id = id, r = r, ns = ns, table = table, 
          dropdowns = dropdowns %>% dplyr::filter(id == id) %>% dplyr::pull(dropdowns) %>% unlist(), i18n = i18n)
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer r$", table, " - update dropdowns"))
      })
    
      # When save button is clicked
      # Do that for main datatable (management_datatable) & vocabularies_tables_datatable
      observeEvent(input$management_save, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$management_save"))
        
        req(nrow(r[[paste0(table, "_temp")]]) > 0)
        
        save_settings_datatable_updates(output = output, r = r, m = m, ns = ns, table = table, i18n = i18n, duplicates_allowed = FALSE)
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer input$management_save"))
      })
      
      # --- --- --- --- --- --- --- --
      # Delete a row in datatable ----
      # --- --- --- --- --- --- --- --
      
      settings_delete_prefix <- table
      settings_dialog_title <- paste0(table, "_delete")
      settings_dialog_subtext <- paste0(table, "_delete_subtext")
      settings_react_variable <- "delete_confirm"
      settings_id_var_sql <- "id"
      settings_id_var_r <- paste0("delete_", get_plural(table))
      settings_delete_message <- paste0(table, "_deleted")
      settings_reload_variable <- paste0("reload_" , get_plural(table))
      settings_information_variable <- paste0(table, "_deleted")
      settings_delete_variable <- paste0(table, "_open_dialog")
      
      delete_element(r = r, m = m, input = input, output = output, session = session, ns = ns, i18n = i18n,
        delete_prefix = settings_delete_prefix, dialog_title = settings_dialog_title, dialog_subtext = settings_dialog_subtext,
        react_variable = settings_react_variable, table = table, id_var_sql = settings_id_var_sql, id_var_r = settings_id_var_r,
        delete_message = settings_delete_message, translation = TRUE, reload_variable = settings_reload_variable,
        information_variable = settings_information_variable)
      
      # Delete one row (with icon on DT)
      
      observeEvent(input$deleted_pressed, {
        
        if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$deleted_pressed"))
        
        r[[paste0("delete_", get_plural(table))]] <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
        r[[settings_delete_variable]] <- TRUE
        
        # Reload datatable (to unselect rows)
        DT::replaceData(r[[paste0(table, "_datatable_proxy")]], r[[paste0(table, "_datatable_temp")]], resetPaging = FALSE, rownames = FALSE)
      })
      
      # Delete multiple rows (with "Delete selection" button)
      
      observeEvent(input$delete_selection, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$delete_selection"))
        
        req(length(input[["management_datatable_rows_selected"]]) > 0)
        
        r[[paste0("delete_", get_plural(table))]] <- r[[paste0(table, "_temp")]][input[["management_datatable_rows_selected"]], ] %>% dplyr::pull(id)
        r[[settings_delete_variable]] <- TRUE
      })
      
      # --- --- --- --- --- --- --- --- -- -
      # Edit options by selecting a row ----
      # --- --- --- --- --- --- --- --- -- -
        
      if (table == "datasets"){
          
        observeEvent(input$options, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$options"))
          
          # Get link_id variable, to update options div
          link_id <- as.integer(substr(input$options, nchar("options_") + 1, nchar(input$options)))
          
          options <- convert_tibble_to_list(r$datasets %>% dplyr::arrange(name), key_col = "id", text_col = "name")
          value <- list(key = link_id, text = r$datasets %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
          
          shiny.fluent::updateComboBox.shinyInput(session, "code_selected_dataset_or_vocabulary", options = options, value = value)
          shiny.fluent::updateComboBox.shinyInput(session, "options_selected_dataset_or_vocabulary", options = options, value = value)
          
          # Reload datatable (to unselect rows)
          DT::replaceData(r[[paste0(table, "_datatable_proxy")]], r[[paste0(table, "_datatable_temp")]], resetPaging = FALSE, rownames = FALSE)
          
          # Set current pivot to options_card
          shinyjs::runjs(glue::glue("$('#{id}-{paste0(get_plural(table), '_pivot')} button[name=\"{i18n$t(paste0(get_singular(table), '_options'))}\"]').click();"))
        })
        
        observeEvent(input$options_selected_dataset_or_vocabulary, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$options_selected_dataset_or_vocabulary"))
          
          if (length(input$options_selected_dataset_or_vocabulary) > 1) link_id <- input$options_selected_dataset_or_vocabulary$key
          else link_id <- input$options_selected_dataset_or_vocabulary
          if (length(input$code_selected_dataset_or_vocabulary) > 0){
            if (length(input$code_selected_dataset_or_vocabulary) > 1) code_link_id <- input$code_selected_dataset_or_vocabulary$key
            else code_link_id <- input$code_selected_dataset_or_vocabulary
          }
          else code_link_id <- 0L
          
          if (link_id != code_link_id){
            options <- convert_tibble_to_list(r$datasets %>% dplyr::arrange(name), key_col = "id", text_col = "name")
            value <- list(key = link_id, text = r$datasets %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
            shiny.fluent::updateComboBox.shinyInput(session, "code_selected_dataset_or_vocabulary", options = options, value = value)
          }
          
          category <- get_singular(word = id)
          
          options <- r$options %>% dplyr::filter(category == "dataset", link_id == !!link_id)
          
          picker_options <-
            r$users %>%
            dplyr::left_join(r$users_statuses %>% dplyr::select(user_status_id = id, user_status = name), by = "user_status_id") %>%
            dplyr::transmute(
              key = id, 
              imageInitials = paste0(substr(firstname, 0, 1), substr(lastname, 0, 1)),
              text = paste0(firstname, " ", lastname), 
              secondaryText = user_status)
          
          value <-
            picker_options %>%
            dplyr::mutate(n = 1:dplyr::n()) %>%
            dplyr::inner_join(
              options %>%
                dplyr::filter(name == "user_allowed_read") %>%
                dplyr::select(key = value_num),
              by = "key"
            ) %>%
            dplyr::pull(key)
          
          selected_items <- picker_options %>% dplyr::filter(key %in% value)
          
          shiny.fluent::updateToggle.shinyInput(session, "show_only_aggregated_data", value = options %>% dplyr::filter(name == "show_only_aggregated_data") %>% dplyr::pull(value_num) %>% as.logical)
          shiny.fluent::updateChoiceGroup.shinyInput(session, "users_allowed_read_group", value = options %>% dplyr::filter(name == "users_allowed_read_group") %>% dplyr::pull(value))
          shiny.fluent::updateDropdown.shinyInput(session, "omop_version", options = list(
            list(key = "5.3", text = "5.3"),
            list(key = "5.4", text = "5.4"),
            list(key = "6.0", text = "6.0")
          ),
            value = options %>% dplyr::filter(name == "omop_version") %>% dplyr::pull(value))
          output$users_allowed_read_div <- renderUI({
            make_people_picker(
              i18n = i18n, ns = ns, id = "users_allowed_read", label = "users", options = picker_options, value = value,
              width = "100%", style = "padding-bottom:10px;")
          })
          
        })
        
        observeEvent(input$options_save, {
          
          if (perf_monitoring) monitor_perf(r = r, action = "start")
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$options_save"))
          
          req(input$options_selected_dataset_or_vocabulary)
          
          if (length(input$options_selected_dataset_or_vocabulary) > 1) link_id <- input$options_selected_dataset_or_vocabulary$key
          else link_id <- input$options_selected_dataset_or_vocabulary
          
          category <- get_singular(id)
          
          data <- list()
          data$show_only_aggregated_data <- as.integer(input$show_only_aggregated_data)
          data$users_allowed_read <- input$users_allowed_read
          data$users_allowed_read_group <- input$users_allowed_read_group
          data$omop_version <- input$omop_version
  
          save_settings_options(output = output, r = r, id = id, category = category, code_id_input = paste0("options_", link_id), 
            i18n = i18n, data = data, page_options = c("show_only_aggregated_data", "users_allowed_read", "omop_version"))
          
          if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer input$options_save"))
        })
      }
      
      # --- --- --- --- --- --- --- -- --
      # Edit code by selecting a row ----
      # --- --- --- --- --- --- --- -- --
      
      if (table %in% c("datasets", "vocabulary")){
        
        # Button "Edit code" is clicked on the datatable
        observeEvent(input$edit_code, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$edit_code"))

          # Get link_id variable, to update code editor
          link_id <- as.integer(substr(input$edit_code, nchar("edit_code_") + 1, nchar(input$edit_code)))
          
          if (table == "vocabulary") {
            options <- convert_tibble_to_list(r$vocabulary %>% dplyr::arrange(vocabulary_id), key_col = "id", text_col = "vocabulary_id")
            value <- list(key = link_id, text = r$vocabulary %>% dplyr::filter(id == link_id) %>% dplyr::pull(vocabulary_id))
          }
          else {
            options <- convert_tibble_to_list(r[[table]] %>% dplyr::arrange(name), key_col = "id", text_col = "name")
            value <- list(key = link_id, text = r[[table]] %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
          }
          
          shiny.fluent::updateComboBox.shinyInput(session, "code_selected_dataset_or_vocabulary", options = options, value = value)
          if (table == "datasets") shiny.fluent::updateComboBox.shinyInput(session, "options_selected_dataset_or_vocabulary", options = options, value = value)
          if (table == "vocabulary") shiny.fluent::updateComboBox.shinyInput(session, "vocabulary_tables_selected_vocabulary", options = options, value = value)
          
          # Reload datatable (to unselect rows)
          DT::replaceData(r[[paste0(table, "_datatable_proxy")]], r[[paste0(table, "_datatable_temp")]], resetPaging = FALSE, rownames = FALSE)
          
          # Set current pivot to edit_code_card
          shinyjs::runjs(glue::glue("$('#{id}-{paste0(get_plural(table), '_pivot')} button[name=\"{i18n$t(paste0('edit_', get_singular(table), '_code'))}\"]').click();"))
          
        })
        
        observeEvent(input$code_selected_dataset_or_vocabulary, {
          
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$code_selected_dataset_or_vocabulary"))

          if (length(input$code_selected_dataset_or_vocabulary) > 1) link_id <- input$code_selected_dataset_or_vocabulary$key
          else link_id <- input$code_selected_dataset_or_vocabulary
          
          if (table == "datasets"){
            
            # Reset datatable of results
            render_datatable(output = output, ns = ns, i18n = i18n, data = tibble::tibble(name = character(), rows = integer()),
              output_name = "code_datatable", col_names = c(i18n$t("table_name"), i18n$t("rows")),
              column_widths = c("rows" = "150px"), datatable_dom = "", page_length = 30)
            
            if (length(input$options_selected_dataset_or_vocabulary) > 0){
              if (length(input$options_selected_dataset_or_vocabulary) > 1) options_link_id <- input$options_selected_dataset_or_vocabulary$key
              else options_link_id <- input$options_selected_dataset_or_vocabulary
            }
            else options_link_id <- 0L

            if (link_id != options_link_id){
              options <- convert_tibble_to_list(r$datasets %>% dplyr::arrange(name), key_col = "id", text_col = "name")
              value <- list(key = link_id, text = r$datasets %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
              shiny.fluent::updateComboBox.shinyInput(session, "options_selected_dataset_or_vocabulary", options = options, value = value)
            }
          }
          
          if (table == "vocabulary"){
            if (length(input$vocabulary_tables_selected_vocabulary) > 0){
              if (length(input$vocabulary_tables_selected_vocabulary) > 1) items_link_id <- input$vocabulary_tables_selected_vocabulary$key
              else items_link_id <- input$vocabulary_tables_selected_vocabulary
            }
            else items_link_id <- 0L
            
            if (link_id != items_link_id){
              options <- convert_tibble_to_list(r$vocabulary %>% dplyr::arrange(vocabulary_id), key_col = "id", text_col = "vocabulary_id")
              value <- list(key = link_id, text = r$vocabulary %>% dplyr::filter(id == link_id) %>% dplyr::pull(vocabulary_id))
              shiny.fluent::updateComboBox.shinyInput(session, "vocabulary_tables_selected_vocabulary", options = options, value = value)
            }
          }

          # Save ID value in r variable, to get this during code execution
          # Before, restart these variables
          r$dataset_id <- NA_integer_
          r$subset_id <- NA_integer_
          r$vocabulary_id <- NA_character_

          if (id == "settings_datasets") r$dataset_id <- link_id
          if (id == "settings_vocabularies") r$vocabulary_id <- r$vocabulary %>% dplyr::filter(id == link_id) %>% dplyr::pull(vocabulary_id)

          category <- get_singular(id)

          # Get code from database
          code <- r$code %>% dplyr::filter(category == !!category & link_id == !!link_id) %>% dplyr::pull(code) %>% stringr::str_replace_all("''", "'")
          shinyAce::updateAceEditor(session, "ace_edit_code", value = code)

          # Reset code_result textOutput
          output$datetime_code_execution <- renderText("")
          output$code_result <- renderUI("")
        })
        
        # When save button is clicked, or CTRL+C or CMD+C is pushed
        observeEvent(input$edit_code_save, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$edit_code_save"))
          r[[paste0(id, "_save")]] <- Sys.time()
        })
        observeEvent(input$ace_edit_code_save, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$ace_edit_code_save"))
          r[[paste0(id, "_save")]] <- Sys.time()
          shinyjs::runjs(sprintf("
            var editor = ace.edit('%s-ace_edit_code');
            editor.moveCursorTo(%d, %d);
            editor.focus();
              ", id, input$ace_edit_code_save$range$end$row, input$ace_edit_code_save$range$end$column))
        })
        observeEvent(r[[paste0(id, "_save")]], {
          
          if (perf_monitoring) monitor_perf(r = r, action = "start")
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$..save"))
          
          req(input$code_selected_dataset_or_vocabulary)
          
          if (length(input$code_selected_dataset_or_vocabulary) > 1) link_id <- input$code_selected_dataset_or_vocabulary$key
          else link_id <- input$code_selected_dataset_or_vocabulary
          
          save_settings_code(output = output, r = r, id = id, category = get_singular(id),
            code_id_input = paste0("edit_code_", link_id), edited_code = input$ace_edit_code, i18n = i18n)
          
          if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer r$..save"))
        })
        
        # Comment text
        
        observeEvent(input$ace_edit_code_comment, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$ace_edit_code_comment"))
          
          lines <- strsplit(input$ace_edit_code, "\n")[[1]]
          
          start_row <- input$ace_edit_code_comment$range$start$row + 1
          end_row <- input$ace_edit_code_comment$range$end$row + 1
          
          for (i in start_row:end_row) {
            if (startsWith(lines[i], "# ")) {
              lines[i] <- substr(lines[i], 3, nchar(lines[i]))
            } else {
              lines[i] <- paste0("# ", lines[i])
            }
          }
          
          shinyAce::updateAceEditor(session, "ace_edit_code", value = paste0(lines, collapse = "\n"))
          
          shinyjs::runjs(sprintf("
            var editor = ace.edit('%s-ace_edit_code');
            editor.moveCursorTo(%d, %d);
            editor.focus();
              ", id, input$ace_edit_code_comment$range$end$row, input$ace_edit_code_comment$range$end$column))
        })
        
        # When Execute code button is clicked
        
        observeEvent(input$execute_code, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$execute_code"))
          r[[paste0(id, "_code")]] <- input$ace_edit_code
          r[[paste0(id, "_code_trigger")]] <- Sys.time()
        })
        
        observeEvent(input$ace_edit_code_run_selection, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$ace_edit_code_run_selection"))
          if(!shinyAce::is.empty(input$ace_edit_code_run_selection$selection)) r[[paste0(id, "_code")]] <- input$ace_edit_code_run_selection$selection
          else r[[paste0(id, "_code")]] <- input$ace_edit_code_run_selection$line
          r[[paste0(id, "_code_trigger")]] <- Sys.time()
        })
        
        observeEvent(input$ace_edit_code_run_all, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$ace_edit_code_run_all"))
          r[[paste0(id, "_code")]] <- input$ace_edit_code
          r[[paste0(id, "_code_trigger")]] <- Sys.time()
        })
      
        observeEvent(r[[paste0(id, "_code_trigger")]], {
          
          if (perf_monitoring) monitor_perf(r = r, action = "start")
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$..code_trigger"))
          
          # Reset d variable
          if (table == "datasets"){
            main_tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
              "observation", "death", "note", "note_nlp", "specimen", "fact_relationship", "payer_plan_period", "cost", 
              "drug_era", "dose_era", "condition_era",
              "person", "observation_period", "visit_occurrence", "visit_detail", "location", "care_site", "provider")
            sapply(main_tables, function(table) d[[table]] <- tibble::tibble())
          }
          
          edited_code <- r[[paste0(id, "_code")]] %>% stringr::str_replace_all("\r", "\n")
          # Prevent "NULL" at the end of console output
          edited_code <- paste0(edited_code, "\ncat()")
          
          output$datetime_code_execution <- renderText(format_datetime(Sys.time(), language))
          
          console_result <- isolate(execute_settings_code(input = input, output = output, session = session, id = id, ns = ns, 
            i18n = i18n, r = r, d = d, m = m, edited_code = edited_code))
          
          output$code_result <- renderUI(HTML(paste0("<pre>", console_result, "</pre>")))

          if (table == "datasets") r[[paste0(id, "_code_datatable_trigger")]] <- Sys.time()
          
          if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer r$..code"))
        })
        
        observeEvent(r[[paste0(id, "_code_datatable_trigger")]], {
          
          if (perf_monitoring) monitor_perf(r = r, action = "start")
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$..code_datatable_trigger"))
          
          data <- tibble::tibble(name = character(), rows = integer())
          
          vars <- c("person", "observation_period", "visit_occurrence", "visit_detail", "condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure",
            "measurement", "observation", "death", "note", "note_nlp", "specimen", "fact_relationship", "location", "location_history", "care_site", "provider", 
            "payer_plan_period", "cost", "drug_era", "dose_era", "condition_era")
          
          for (var in vars){
            if (length(d[[var]]) == 0) n_rows <- 0L
            else n_rows <- nrow(d[[var]])
            data <- data %>% dplyr::bind_rows(tibble::tibble(name = var, rows = n_rows))
          }
          
          render_datatable(output = output, ns = ns, i18n = i18n, data = data,
            output_name = "code_datatable", col_names = c(i18n$t("table_name"), i18n$t("rows")),
            column_widths = c("rows" = "150px"), datatable_dom = "", page_length = 30)
          
          if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer r$..code_datatable_trigger"))
        })
      }
      
      # --- --- --- --- --- --- --- --- --- --- -
      # Generate vocabulary tables datatable ----
      # --- --- --- --- --- --- --- --- --- --- -
      
      if (table == "vocabulary"){
        
        # Col types for csv files
        
        col_types <- list()
        col_types$concept <- "iccccccccc"
        col_types$domain <- "cci"
        col_types$concept_class <- "cci"
        col_types$concept_relationship <- "iicccc"
        col_types$relationship <- "ccccci"
        col_types$concept_synonym <- "ici"
        col_types$concept_ancestor <- "iiii"
        col_types$drug_strength <- "iinininiiccc"
        

        # Cols names depending on table

        table_cols_options <- list()
        table_cols_options_value <- list()

        table_cols_options$concept <- list(
          list(key = 1, text = "concept_id"),
          list(key = 2, text = "concept_name"),
          list(key = 3, text = "domain_id"),
          list(key = 4, text = "vocabulary_id"),
          list(key = 5, text = "concept_class_id"),
          list(key = 6, text = "standard_concept"),
          list(key = 7, text = "concept_code"),
          list(key = 8, text = "valid_start_date"),
          list(key = 9, text = "valid_end_date"),
          list(key = 10, text = "invalid_reason"))
        table_cols_options_value$concept <- c(1, 2, 3, 4, 5)

        table_cols_options$domain <- list(
          list(key = 1, text = "domain_id"),
          list(key = 2, text = "domain_name"),
          list(key = 3, text = "domain_concept_id"))
        table_cols_options_value$domain <- c(1, 2, 3)

        table_cols_options$concept_class <- list(
          list(key = 1, text = "concept_class_id"),
          list(key = 2, text = "concept_class_name"),
          list(key = 3, text = "concept_class_concept_id"))
        table_cols_options_value$concept_class <- c(1, 2, 3)

        table_cols_options$concept_relationship <- list(
          list(key = 1, text = "concept_id_1"),
          list(key = 2, text = "concept_id_2"),
          list(key = 3, text = "relationship_id"),
          list(key = 4, text = "valid_stard_date"),
          list(key = 5, text = "valid_end_date"),
          list(key = 6, text = "invalid_reason"))
        table_cols_options_value$concept_relationship <- c(1, 2, 3)

        table_cols_options$relationship <- list(
          list(key = 1, text = "relationship_id"),
          list(key = 2, text = "relationship_name"),
          list(key = 3, text = "is_hierachical"),
          list(key = 4, text = "defines_ancestry"),
          list(key = 5, text = "reverse_relationship_id"),
          list(key = 6, text = "relationship_concept_id"))
        table_cols_options_value$relationship <- c(1, 2, 3, 4, 5, 6)

        table_cols_options$concept_synonym <- list(
          list(key = 1, text = "concept_id"),
          list(key = 2, text = "concept_synonym"),
          list(key = 3, text = "language_concept_id"))
        table_cols_options_value$concept_synonym <- c(1, 2, 3)

        table_cols_options$concept_ancestor <- list(
          list(key = 1, text = "ancestor_concept_id"),
          list(key = 2, text = "descendant_concept_id"),
          list(key = 3, text = "min_levels_of_separation"),
          list(key = 4, text = "max_levels_of_separation"))
        table_cols_options_value$concept_ancestor <- c(1, 2, 3, 4)

        table_cols_options$drug_strength <- list(
          list(key = 1, text = "drug_concept_id"),
          list(key = 2, text = "ingredient_concept_id"),
          list(key = 3, text = "amount_value"),
          list(key = 4, text = "amount_unit_concept_id"),
          list(key = 5, text = "numerator_value"),
          list(key = 6, text = "numerator_unit_concept_id"),
          list(key = 7, text = "denominator_value"),
          list(key = 8, text = "denominator_unit_concept_id"),
          list(key = 9, text = "box_size"),
          list(key = 10, text = "valid_start_date"),
          list(key = 11, text = "valid_end_date"),
          list(key = 12, text = "invalid_reason"))
        table_cols_options_value$drug_strength <- c(1, 2, 3, 4, 5, 6, 7, 8)

        # Datatable args

        editable_cols_vocab <- list()
        editable_cols_vocab$concept <- c("concept_name", "domain_id", "vocabulary_id", "concept_class_id", "standard_concept", "concept_code",
          "valid_start_date", "valid_end_date", "invalid_reason")
        editable_cols_vocab$domain <- c("domain_id", "domain_name", "domain_concept_id")
        editable_cols_vocab$concept_class <- c("concept_class_id", "concept_class_name", "concept_class_concept_id")
        editable_cols_vocab$concept_relationship <- c("relationship_id", "valid_start_date", "valid_end_date", "invalid_reason")
        editable_cols_vocab$relationship <- c("relationship_id", "relationship_name", "is_hierarchical", "defines_ancestry", "reverse_relationship_id", "relationship_concept_id")
        editable_cols_vocab$concept_synonym <- c("concept_synonym", "language_concept_id")
        editable_cols_vocab$concept_ancestor <- c("min_levels_of_separation", "max_levels_of_separation")
        editable_cols_vocab$drug_strength <- c("amount_value", "amount_unit_concept_id", "numerator_value", "numerator_unit_concept_id",
          "denominator_value", "denominator_unit_concept_id", "box_size", "valid_start_date", "valid_end_date", "invalid_reason")

        sortable_cols_vocab <- list()
        sortable_cols_vocab$concept <- c("concept_id", "concept_name", "domain_id", "vocabulary_id", "concept_class_id", "standard_concept", "concept_code",
          "valid_start_date", "valid_end_date", "invalid_reason")
        sortable_cols_vocab$domain <- c("domain_id", "domain_name", "domain_concept_id")
        sortable_cols_vocab$concept_class <- c("concept_class_id", "concept_class_name", "concept_class_concept_id")
        sortable_cols_vocab$concept_relationship <- c("concept_id_1", "concept_id_2", "relationship_id", "valid_start_date", "valid_end_date", "invalid_reason")
        sortable_cols_vocab$relationship <- c("relationship_id", "relationship_name", "is_hierarchical", "defines_ancestry", "reverse_relationship_id", "relationship_concept_id")
        sortable_cols_vocab$concept_synonym <- c("concept_id", "concept_synonym", "language_concept_id")
        sortable_cols_vocab$concept_ancestor <- c("ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation", "max_levels_of_separation")
        sortable_cols_vocab$drug_strength <- c("drug_concept_id", "ingredient_concept_id", "amount_value", "amount_unit_concept_id", "numerator_value", "numerator_unit_concept_id",
          "denominator_value", "denominator_unit_concept_id", "box_size", "valid_start_date", "valid_end_date", "invalid_reason")

        centered_cols_vocab <- list()
        centered_cols_vocab$concept <- c("concept_id", "domain_id", "vocabulary_id", "concept_class_id", "standard_concept",
            "concept_code", "valid_start_date", "valid_end_date", "invalid_reason")
        centered_cols_vocab$domain <- "domain_concept_id"
        centered_cols_vocab$concept_class <- "concept_class_id"
        centered_cols_vocab$concept_relationship <- c("concept_id_1", "concept_id_2", "relationship_id", "valid_start_date", "valid_end_date", "invalid_reason")
        centered_cols_vocab$relationship <- c("is_hierarchical", "defines_ancestry", "relationship_concept_id")
        centered_cols_vocab$concept_synonym <- c("concept_id", "language_concept_id")
        centered_cols_vocab$concept_ancestor <- c("ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation", "max_levels_of_separation")
        centered_cols_vocab$drug_strength <- c("drug_concept_id", "ingredient_concept_id",  "amount_value", "amount_unit_concept_id", "numerator_value", "numerator_unit_concept_id",
          "denominator_value", "denominator_unit_concept_id", "box_size", "valid_start_date", "valid_end_date", "invalid_reason")

        searchable_cols_vocab <- list()
        searchable_cols_vocab$concept <- c("concept_id", "concept_name", "domain_id", "vocabulary_id", "concept_class_id")
        searchable_cols_vocab$domain <- c("domain_id", "domain_name", "domain_concept_id")
        searchable_cols_vocab$concept_class <- c("concept_class_id", "concept_class_name", "concept_class_concept_id")
        searchable_cols_vocab$concept_relationship <- c("concept_id_1", "concept_id_2", "relationship_id")
        searchable_cols_vocab$relationship <- c("relationship_id", "relationship_name", "is_hierarchical", "defines_ancestry", "reverse_relationship_id", "relationship_concept_id")
        searchable_cols_vocab$concept_synonym <- c("concept_id", "concept_synonym", "language_concept_id")
        searchable_cols_vocab$concept_ancestor <- c("ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation", "max_levels_of_separation")
        searchable_cols_vocab$drug_strength <- c("drug_concept_id", "ingredient_concept_id",  "amount_value", "amount_unit_concept_id", "numerator_value", "numerator_unit_concept_id",
          "denominator_value", "denominator_unit_concept_id", "box_size")

        factorize_cols_vocab <- list()
        factorize_cols_vocab$concept <- c("domain_id", "vocabulary_id", "concept_class_id")
        factorize_cols_vocab$domain <- ""
        factorize_cols_vocab$concept_class <- ""
        factorize_cols_vocab$concept_relationship <- "relationship_id"
        factorize_cols_vocab$relationship <- ""
        factorize_cols_vocab$concept_synonym <- "language_concept_id"
        factorize_cols_vocab$concept_ancestor <- ""
        factorize_cols_vocab$drug_strength <- ""

        # Transform integer cols to character, to be searchable
        cols_to_char <- list()
        cols_to_char$concept = "concept_id"
        cols_to_char$domain = "domain_concept_id"
        cols_to_char$concept_class = "concept_class_concept_id"
        cols_to_char$concept_relationship = c("concept_id_1", "concept_id_2")
        cols_to_char$relationship = "relationship_concept_id"
        cols_to_char$concept_synonym = c("concept_id", "language_concept_id")
        cols_to_char$concept_ancestor = c("ancestor_concept_id", "descendant_concept_id")
        cols_to_char$drug_strength = c("drug_concept_id", "ingredient_concept_id", "amount_unit_concept_id", "numerator_unit_concept_id", "denominator_unit_concept_id")

        cols_order <- list()
        cols_order$concept <- "concept_id"
        cols_order$domain <- "domain_concept_id"
        cols_order$concept_class <- "concept_class_concept_id"
        cols_order$concept_relationship <- "concept_id_1"
        cols_order$relationship <- "relationship_concept_id"
        cols_order$concept_synonym <- "concept_id"
        cols_order$concept_ancestor <- "ancestor_concept_id"
        cols_order$drug_strength <- "drug_concept_id"
                
        # When a vocabulary table is selected
        
        observeEvent(input$vocabularies_table, {

          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_table"))

          # Reset vocabularies_datatable_row_details div
          output$vocabularies_datatable_row_details <- renderUI("")

          # Update dropdown with which cols to show

          shiny.fluent::updateDropdown.shinyInput(session, "vocabularies_table_cols",
            options = table_cols_options[[input$vocabularies_table]], value = table_cols_options_value[[input$vocabularies_table]])
          
          # Get data from database
          if (length(d[[input$vocabularies_table]]) == 0){
            
            # Filter concept_relationship with validated evaluations
            
            if (input$vocabularies_table == "concept_relationship") sql <- glue::glue_sql(paste0(
              "SELECT cr.* FROM concept_relationship cr WHERE cr.id NOT IN ( ",
              "WITH cr AS (",
              "SELECT cru.concept_relationship_id, ",
              "SUM(CASE WHEN cre.evaluation_id = 1 THEN 1 ELSE 0 END) AS positive_evals, ",
              "SUM(CASE WHEN cre.evaluation_id = 2 THEN 1 ELSE 0 END) AS negative_evals ",
              "FROM concept_relationship_user cru ",
              "LEFT JOIN concept_relationship_evals cre ON cru.concept_relationship_id = cre.concept_relationship_id ",
              "GROUP BY cru.concept_relationship_id ",
              "HAVING positive_evals = 0 OR (positive_evals > 0 AND positive_evals <= negative_evals) ",
              ") ",
              "SELECT cr.concept_relationship_id FROM cr ",
              ")"), .con = m$db)
           
            else sql <- glue::glue_sql("SELECT * FROM {`input$vocabularies_table`}", .con = m$db)
            
            d[[input$vocabularies_table]] <- DBI::dbGetQuery(m$db, sql) %>%
              tibble::as_tibble() %>%
              dplyr::arrange(cols_order[[input$vocabularies_table]]) %>%
              dplyr::mutate_at(cols_to_char[[input$vocabularies_table]], as.character) %>%
              dplyr::mutate(modified = FALSE)
          }
          
          # Update dropdown with vocabulary
          if (length(r$vocabularies_table_vocabulary_options) == 0){
            sql <- glue::glue_sql("SELECT DISTINCT(vocabulary_id) FROM concept", .con = m$db)
            vocabularies <- DBI::dbGetQuery(m$db, sql) %>% dplyr::arrange(vocabulary_id)
            
            dropdown_options <- convert_tibble_to_list(
              data = tibble::tibble(vocabulary_id = "all_vocabularies", vocabulary_name = i18n$t("all_vocabularies")) %>%
                dplyr::bind_rows(vocabularies %>% dplyr::mutate(vocabulary_name = vocabulary_id) %>% dplyr::arrange(vocabulary_name)), 
              key_col = "vocabulary_id", text_col = "vocabulary_name")
            
            # dropdown_options <- convert_tibble_to_list(data = vocabularies, key_col = "vocabulary_id", text_col = "vocabulary_id")
            # dropdown_options <- rlist::list.append(list(key = "all_vocabularies", text = i18n$t("all_vocabularies")), dropdown_options)
            
            shiny.fluent::updateComboBox.shinyInput(session, "vocabularies_table_vocabulary", options = dropdown_options, value = "all_vocabularies")
            r$vocabularies_table_vocabulary_options <- dropdown_options
          }
          else shiny.fluent::updateComboBox.shinyInput(session, "vocabularies_table_vocabulary", options = r$vocabularies_table_vocabulary_options, value = "all_vocabularies")
          
          # Render datatable rows dropdown
          r$vocabularies_table_update_table_rows_dropdown <- Sys.time()
        })

        # Reload datatable when toggle row_details or mapped_concepts is updated, to change selection (single / multiple)
        observeEvent(input$vocabularies_datatable_show_row_details, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_datatable_show_row_details"))
          r$vocabularies_table_render_datatable <- Sys.time()
        })

        observeEvent(input$vocabularies_datatable_show_mapped_concepts, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_datatable_show_mapped_concepts"))
          r$vocabularies_table_render_datatable <- Sys.time()
        })

        # Reload datatable when rows nums selected
        observeEvent(input$vocabularies_table_rows, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_table_rows"))
          r$vocabularies_table_render_datatable <- Sys.time()
        })
        
        # Reload datatable rows dropdown & datatable when a vocabulary is selected
        observeEvent(input$vocabularies_table_vocabulary, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_table_vocabulary"))
          r$vocabularies_table_update_table_rows_dropdown <- Sys.time()
        })
        
        # Reload datatable rows dropdown
        observeEvent(r$vocabularies_table_update_table_rows_dropdown, {
          
          if (perf_monitoring) monitor_perf(r = r, action = "start")
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$vocabularies_table_update_table_rows_dropdown"))
          
          req(input$vocabularies_table_vocabulary)
          if (length(input$vocabularies_table_vocabulary) > 1) vocabularies_table_vocabulary <- input$vocabularies_table_vocabulary$key
          else vocabularies_table_vocabulary <- input$vocabularies_table_vocabulary
          
          # Count rows of current table
          if (input$vocabularies_table %in% c("concept", "concept_relationship", "concept_synonym", "concept_ancestor", "drug_strength") &
              vocabularies_table_vocabulary != "all_vocabularies"){
            
            if (length(d$concept) == 0) result <- show_message_bar(output,  "load_concept_data_before", "severeWarning", i18n = i18n, ns = ns)
            req(length(d$concept) > 0)
            
            d$concept_filtered <- d$concept %>% dplyr::filter(vocabulary_id == vocabularies_table_vocabulary)
            
            if (input$vocabularies_table == "concept") n_rows <- nrow(d$concept_filtered)
            else if (input$vocabularies_table %in% c("concept_relationship", "concept_ancestor", "drug_strength")){
              
              if (input$vocabularies_table == "concept_relationship"){
                d[[paste0(input$vocabularies_table, "_filtered")]] <- 
                  d[[input$vocabularies_table]] %>%
                  dplyr::left_join(d$concept_filtered %>% dplyr::select(concept_id_1 = concept_id, vocabulary_id_1 = vocabulary_id), by = "concept_id_1") %>%
                  dplyr::left_join(d$concept_filtered %>% dplyr::select(concept_id_2 = concept_id, vocabulary_id_2 = vocabulary_id), by = "concept_id_2")
              }
              else if (input$vocabularies_table == "concept_ancestor"){
                d[[paste0(input$vocabularies_table, "_filtered")]] <- 
                  d[[input$vocabularies_table]] %>%
                  dplyr::left_join(d$concept_filtered %>% dplyr::select(ancestor_concept_id = concept_id, vocabulary_id_1 = vocabulary_id), by = "ancestor_concept_id") %>%
                  dplyr::left_join(d$concept_filtered %>% dplyr::select(descendant_concept_id = concept_id, vocabulary_id_2 = vocabulary_id), by = "descendant_concept_id")
              }
              else if (input$vocabularies_table == "drug_strength"){
                d[[paste0(input$vocabularies_table, "_filtered")]] <- 
                  d[[input$vocabularies_table]] %>%
                  dplyr::left_join(d$concept_filtered %>% dplyr::select(drug_concept_id = concept_id, vocabulary_id_1 = vocabulary_id), by = "drug_concept_id") %>%
                  dplyr::left_join(d$concept_filtered %>% dplyr::select(ingredient_concept_id = concept_id, vocabulary_id_2 = vocabulary_id), by = "ingredient_concept_id")
              }
              
              d[[paste0(input$vocabularies_table, "_filtered")]] <- 
                d[[paste0(input$vocabularies_table, "_filtered")]] %>% 
                dplyr::filter(vocabulary_id_1 == vocabularies_table_vocabulary | vocabulary_id_2 == vocabularies_table_vocabulary) %>%
                dplyr::select(-vocabulary_id_1, -vocabulary_id_2)
              
              n_rows <- nrow(d[[paste0(input$vocabularies_table, "_filtered")]])
            }
            else if (input$vocabularies_table == "concept_synonym"){
              d$concept_synonym_filtered <-
                d$concept_synonym %>% 
                dplyr::inner_join(d$concept_filtered %>% dplyr::select(concept_id, vocabulary_id), by = "concept_id")
              n_rows <- nrow(d$concept_synonym_filtered)
            }
          }
          else n_rows <- nrow(d[[input$vocabularies_table]])
          
          if (n_rows == 0){
            dropdown_options <- list(list(key = "0;0", text = "0"))
            dropdown_value <- "0;0"
          }
          else {
            dropdown_options <- list()
            step <- 10**5
            for (i in 1:ceiling(n_rows / step)){
              
              if (i == 1){
                if (i == ceiling(n_rows / step)) dropdown_value <- paste0(as.integer((i-1)*step) + 1L, ";", n_rows) 
                else dropdown_value <- paste0(as.integer((i-1)*step) + 1L, ";", as.integer(i*step)) 
              }
              
              if (i == ceiling(n_rows / step)) dropdown_options <- rlist::list.append(dropdown_options, list(
                key = paste0(as.integer((i-1)*step) + 1L, ";", n_rows),
                text = paste0(as.integer((i-1)*step) + 1L, " - ", n_rows)))
              
              else dropdown_options <- rlist::list.append(dropdown_options, list(
                key = paste0(as.integer((i-1)*step) + 1L, ";", as.integer(i*step)),
                text = paste0(as.integer((i-1)*step) + 1L, " - ", as.integer(i*step))))
            }
          }
          
          shiny.fluent::updateDropdown.shinyInput(session, "vocabularies_table_rows", options = list(), value = NULL)
          shiny.fluent::updateDropdown.shinyInput(session, "vocabularies_table_rows", options = dropdown_options, value = dropdown_value)
          
          if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer r$vocabularies_table_update_table_rows_dropdown"))
        })

        # Render datatable
        observeEvent(r$vocabularies_table_render_datatable, {

          if (perf_monitoring) monitor_perf(r = r, action = "start")
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$vocabularies_table_render_datatable"))

          req(input$vocabularies_table, input$vocabularies_table_rows, input$vocabularies_table_vocabulary)
          if (length(input$vocabularies_table_vocabulary) > 1) vocabularies_table_vocabulary <- input$vocabularies_table_vocabulary$key
          else vocabularies_table_vocabulary <- input$vocabularies_table_vocabulary

          # Reset vocabularies_datatable_row_details output
          output$vocabularies_datatable_row_details <- renderUI("")

          # Reset mapped concepts datatable
          render_datatable(output = output, ns = ns, i18n = i18n, output_name = "vocabularies_tables_mapped_concepts_datatable",
            data = tibble::tibble(concept_id_1 = character(), relationship_id = character(), concept_id_2 = character(), concept_name_2 = character()))

          n_rows <- stringr::str_split_1(input$vocabularies_table_rows, ";")
          n_rows_start <- as.integer(n_rows[1])
          n_rows_end <- as.integer(n_rows[2])
          
          if (input$vocabularies_table %in% c("concept", "concept_relationship", "concept_synonym", "concept_ancestor", "drug_strength") &
              vocabularies_table_vocabulary != "all_vocabularies"){
            
            data <- d[[paste0(input$vocabularies_table, "_filtered")]] %>% dplyr::slice(n_rows_start:n_rows_end)
          }
          else data <- d[[input$vocabularies_table]] %>% dplyr::slice(n_rows_start:n_rows_end)

          if ((input$vocabularies_table %in% c("concept_relationship", "concept_synonym", "concept_ancestor", "drug_strength") & input$vocabularies_datatable_show_row_details) |
              (input$vocabularies_table == "concept" & input$vocabularies_datatable_show_mapped_concepts)) selection <- "single" else selection <- "multiple"

          render_datatable(output = output, ns = ns, i18n = i18n, data = data,
            output_name = "vocabularies_tables_datatable", editable_cols = editable_cols_vocab[[input$vocabularies_table]],
            sortable_cols = sortable_cols_vocab[[input$vocabularies_table]], centered_cols = centered_cols_vocab[[input$vocabularies_table]],
            hidden_cols = c("id", "modified", "standard_concept", "concept_code", "valid_start_date", "valid_end_date", "invalid_reason", "box_size"), 
            searchable_cols = searchable_cols_vocab[[input$vocabularies_table]], filter = TRUE,
            factorize_cols = factorize_cols_vocab[[input$vocabularies_table]], selection = selection)

          # Create a proxy

          if (length(r$vocabularies_tables_datatable_proxy) == 0) r$vocabularies_tables_datatable_proxy <-
              DT::dataTableProxy("vocabularies_tables_datatable", deferUntilFlush = FALSE)
          
          if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer r$vocabularies_table_render_datatable"))
        })

        # Update which cols are hidden
        observeEvent(input$vocabularies_table_cols, {

          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_table_cols"))

          req(length(r$vocabularies_tables_datatable_proxy) > 0)

          r$vocabularies_tables_datatable_proxy %>%
            DT::showCols(1:length(table_cols_options[[input$vocabularies_table]])) %>%
            DT::hideCols(setdiff(1:(length(table_cols_options[[input$vocabularies_table]])), input$vocabularies_table_cols))
        })

        # Show row details or mapped concepts
        observeEvent(input$vocabularies_tables_datatable_rows_selected, {

          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_tables_datatable_rows_selected"))

          n_rows <- stringr::str_split_1(input$vocabularies_table_rows, ";")
          n_rows_start <- as.integer(n_rows[1])
          n_rows_end <- as.integer(n_rows[2])
          
          if (length(input$vocabularies_table_vocabulary) > 1) vocabularies_table_vocabulary <- input$vocabularies_table_vocabulary$key
          else vocabularies_table_vocabulary <- input$vocabularies_table_vocabulary
          
          if (input$vocabularies_table %in% c("concept", "concept_relationship", "concept_synonym", "concept_ancestor", "drug_strength") &
              vocabularies_table_vocabulary != "all_vocabularies"){
            
            data <- d[[paste0(input$vocabularies_table, "_filtered")]] %>% dplyr::slice(n_rows_start:n_rows_end)
          }
          else data <- d[[input$vocabularies_table]] %>% dplyr::slice(n_rows_start:n_rows_end)
          
          
          # Show row details

          if(input$vocabularies_table %in% c("concept_relationship", "concept_synonym", "concept_ancestor", "drug_strength")){
            req(input$vocabularies_datatable_show_row_details)

            if (length(input$vocabularies_tables_datatable_rows_selected) == 0) output$vocabularies_datatable_row_details <- renderUI("")
            else if (length(input$vocabularies_tables_datatable_rows_selected) == 1){

              result <- ""
              
              if (length(d$concept) == 0) result <- div(i18n$t("load_concept_data_before"))

              selected_row <- data[input$vocabularies_tables_datatable_rows_selected, ]
              
              if (length(d$concept) > 0){
                if (input$vocabularies_table == "concept_relationship"){
                  result <- div(
                    strong("concept_id_1"), " : ", selected_row$concept_id_1, br(),
                    strong("concept_name_1"), " : ", d$concept %>% dplyr::filter(concept_id == selected_row$concept_id_1) %>% dplyr::pull(concept_name), br(),
                    strong("relationship_id"), " : ", selected_row$relationship_id, br(),
                    strong("concept_id_2"), " : ", selected_row$concept_id_2, br(),
                    strong("concept_name_2"), " : ", d$concept %>% dplyr::filter(concept_id == selected_row$concept_id_2) %>% dplyr::pull(concept_name), br(),
                  )
                }
                
                else if (input$vocabularies_table == "concept_synonym"){
                  result <- div(
                    strong("concept_id"), " : ", d$concept %>% dplyr::filter(concept_id == selected_row$concept_id) %>% dplyr::pull(concept_name), br(),
                    strong("concept_synonym_name"), " : ", selected_row$concept_synonym_name, br(),
                    strong("language_concept_name"), " : ", d$concept %>% dplyr::filter(concept_id == selected_row$language_concept_id) %>% dplyr::pull(concept_name), br()
                  )
                }
                
                else if (input$vocabularies_table == "concept_ancestor"){
                  result <- div(
                    strong("ancestor_concept_id"), " : ", d$concept %>% dplyr::filter(concept_id == selected_row$ancestor_concept_id) %>% dplyr::pull(concept_name), br(),
                    strong("descendant_concept_id"), " : ", d$concept %>% dplyr::filter(concept_id == selected_row$descendant_concept_id) %>% dplyr::pull(concept_name), br(),
                    strong("min_level_of_separation"), " : ", selected_row$min_levels_of_separation, br(),
                    strong("max_level_of_separation"), " : ", selected_row$max_levels_of_separation, br()
                  )
                }
                
                else if (input$vocabularies_table == "drug_strength"){
                  result <- div(
                    strong("drug_concept_id"), " : ", selected_row$drug_concept_id, br(),
                    strong("drug_concept_name"), " : ", d$concept %>% dplyr::filter(concept_id == selected_row$drug_concept_id) %>% dplyr::pull(concept_name), br(),
                    strong("ingredient_concept_id"), " : ", selected_row$ingredient_concept_id, br(),
                    strong("ingredient_concept_name"), " : ", d$concept %>% dplyr::filter(concept_id == selected_row$ingredient_concept_id) %>% dplyr::pull(concept_name), br(),
                    strong("amount_value"), " : ", selected_row$amount_value, br(),
                    strong("amount_unit_concept_id"), " : ", selected_row$amount_unit_concept_id, br(),
                    strong("amount_unit_concept_name"), " : ", d$concept %>% dplyr::filter(concept_id == selected_row$amount_unit_concept_id) %>% dplyr::pull(concept_name), br(),
                    strong("numerator_value"), " : ", selected_row$numerator_value, br(),
                    strong("numerator_unit_concept_id"), " : ", selected_row$numerator_unit_concept_id, br(),
                    strong("numerator_unit_concept_name"), " : ", d$concept %>% dplyr::filter(concept_id == selected_row$numerator_unit_concept_id) %>% dplyr::pull(concept_name), br(),
                    strong("denominator_value"), " : ", selected_row$denominator_value, br(),
                    strong("denominator_unit_concept_id"), " : ", selected_row$denominator_unit_concept_id, br(),
                    strong("denominator_unit_concept_name"), " : ", d$concept %>% dplyr::filter(concept_id == selected_row$denominator_unit_concept_id) %>% dplyr::pull(concept_name), br(),
                    strong("box_size"), " : ", selected_row$box_size, br(),
                  )
                }
              }

              output$vocabularies_datatable_row_details <- renderUI(result)
            }
          }

          # Show mapped concepts

          if (input$vocabularies_table == "concept"){
            req(input$vocabularies_datatable_show_mapped_concepts)

            selected_row <- data[input$vocabularies_tables_datatable_rows_selected, ]
            if (length(d$concept_relationship) == 0) result <- show_message_bar(output, "load_concept_relationship_data_before", "severeWarning", i18n = i18n, ns = ns)
            
            req(length(d$concept_relationship) > 0)
            
            mapped_concepts <- d$concept_relationship %>%
              dplyr::filter(concept_id_1 == selected_row$concept_id) %>%
              dplyr::left_join(d$concept %>% dplyr::select(concept_id_2 = concept_id, concept_name_2 = concept_name), by = "concept_id_2") %>%
              dplyr::select(concept_id_1, relationship_id, concept_id_2, concept_name_2) %>%
              dplyr::mutate_at(c("concept_id_1", "concept_id_2"), as.character)

            render_datatable(output = output, ns = ns, i18n = i18n, data = mapped_concepts,
              output_name = "vocabularies_tables_mapped_concepts_datatable", sortable_cols = c("concept_id_1", "concept_id_2", "concept_name_2", "relationship_id"),
              centered_cols = c("concept_id_1", "concept_id_2", "relationship_id"), searchable_cols = c("concept_id_1", "concept_id_2", "concept_name_2", "relationship_id"),
              filter = TRUE, factorize_cols = c("relationship_id"))
          }
        })

        # --- --- --- --- --- --- --- --- --- -- -
        # Update rows in vocabulary datatable ----
        # --- --- --- --- --- --- --- --- --- -- -

        observeEvent(input$vocabularies_tables_datatable_cell_edit, {

          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_tables_datatable_cell_edit"))

          edit_info <- input$vocabularies_tables_datatable_cell_edit
          d[[input$vocabularies_table]] <- DT::editData(d[[input$vocabularies_table]], edit_info, rownames = FALSE)

          # Store that this row has been modified
          d[[input$vocabularies_table]][[edit_info$row, "modified"]] <- TRUE
        })

        # Save updates

        observeEvent(input$vocabularies_tables_datatable_save, {

          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_tables_datatable_save"))

          if (nrow(d[[input$vocabularies_table]] %>% dplyr::filter(modified)) == 0) show_message_bar(output,  "modif_saved", "success", i18n = i18n, ns = ns)

          ids_to_del <- d[[input$vocabularies_table]] %>% dplyr::filter(modified) %>% dplyr::pull(id)
          req(length(ids_to_del) > 0)

          # Check if there are duplicates

          data_check_duplicates_cols <- switch(input$vocabularies_table,
            "concept" = "",
            "domain" = "domain_id",
            "concept_class" = "concept_class_id",
            "concept_relationship" = "",
            "relationship" = "relationship_id",
            "concept_synonym" = "",
            "concept_ancestor" = "",
            "drug_strength" = ""
          )

          if (data_check_duplicates_cols == "") check_duplicates <- 0
          else check_duplicates <- d[[input$vocabularies_table]] %>% dplyr::group_by_at(data_check_duplicates_cols) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()

          if (check_duplicates > 0) show_message_bar(output, "vocab_tables_duplicates_cols", "severeWarning", i18n, ns = ns)

          data_required_cols <- switch(input$vocabularies_table,
            "concept" = c("concept_id", "concept_name", "domain_id", "vocabulary_id", "concept_class_id",
              "concept_code", "valid_start_date", "valid_end_date"),
            "domain" = c("domain_id", "domain_name", "domain_concept_id"),
            "concept_class" = c("concept_class_id", "concept_class_name", "concept_class_concept_id"),
            "concept_relationship" = c("concept_id_1", "concept_id_2", "relationship_id", "valid_start_date", "valid_end_date"),
            "relationship" = "relationship_id",
            "concept_synonym" = c("concept_id", "concept_synonym_name", "language_concept_id"),
            "concept_ancestor" = c("ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation", "max_levels_of_separation"),
            "drug_strength" = c("drug_concept_id", "ingredient_concept_id", "valid_start_date", "valid_end_date")
          )

          check_required_cols <- d[[input$vocabularies_table]] %>% dplyr::filter(dplyr::if_any(dplyr::all_of(data_required_cols), ~ . %in% c("", NA_character_, NA_integer_))) %>% nrow()

          if (check_required_cols > 0) show_message_bar(output, "vocab_tables_empty_cols", "severeWarning", i18n, ns = ns)

          req(check_duplicates == 0, check_required_cols == 0)

          sql <- glue::glue_sql("DELETE FROM {`input$vocabularies_table`} WHERE id IN ({ids_to_del*})", .con = m$db)
          DBI::dbSendStatement(m$db, sql) -> query
          DBI::dbClearResult(query)

          data <- d[[input$vocabularies_table]] %>% dplyr::filter(modified) %>% dplyr::select(-modified)

          DBI::dbAppendTable(m$db, input$vocabularies_table, data)

          show_message_bar(output,  "modif_saved", "success", i18n = i18n, ns = ns)
        })

        # --- --- --- --- --- --- --- --- --- --- --- --
        # Delete rows in vocabulary table datatable ----
        # --- --- --- --- --- --- --- --- --- --- --- --

        r$vocabularies_table_open_dialog <- FALSE

        # IDs of selected rows

        observeEvent(input$vocabularies_tables_delete_selection, {

          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_tables_delete_selection"))

          req(length(input$vocabularies_tables_datatable_rows_selected) > 0)

          r$delete_vocabularies_table_rows <- d[[input$vocabularies_table]][input$vocabularies_tables_datatable_rows_selected, ] %>% dplyr::pull(id)
          r$vocabularies_table_open_dialog <- TRUE
        })

        # React for deletion confirmation

        output$vocabularies_table_delete_confirm <- shiny.fluent::renderReact({

          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - output$vocabularies_table_delete_confirm"))

          shiny.fluent::Dialog(
            hidden = !r$vocabularies_table_open_dialog,
            onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('vocabularies_table_hide_dialog', Math.random()); }")),
            dialogContentProps = list(
              type = 0,
              title = i18n$t("vocabularies_table_delete"),
              closeButtonAriaLabel = "Close",
              subText = tagList( i18n$t("vocabularies_table_delete_subtext"), br(), br())
            ),
            shiny.fluent::DialogFooter(
              shiny.fluent::PrimaryButton.shinyInput(ns("vocabularies_table_delete_confirmed"), text = i18n$t("delete")),
              shiny.fluent::DefaultButton.shinyInput(ns("vocabularies_table_delete_canceled"), text = i18n$t("dont_delete"))
            )
          )
        })

        # Close dialog box if deletion canceled
        observeEvent(input$vocabularies_table_delete_canceled, r$vocabularies_table_open_dialog <- FALSE)

        # When the deletion is confirmed
        observeEvent(input$vocabularies_table_delete_confirmed, {

          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$vocabularies_table_delete_confirmed"))

          # Close dialog box
          r$vocabularies_table_open_dialog <- FALSE

          sql <- glue::glue_sql("DELETE FROM {`input$vocabularies_table`} WHERE id IN ({r$delete_vocabularies_table_rows*})" , .con = m$db)
          DBI::dbSendStatement(m$db, sql) -> query
          DBI::dbClearResult(query)

          d[[input$vocabularies_table]] <- d[[input$vocabularies_table]] %>% dplyr::filter(id %not_in% r$delete_vocabularies_table_rows)

          r$vocabulary_table_reload_datatable <- Sys.time()

          show_message_bar(output, "vocabulary_table_rows_deleted", type ="severeWarning", i18n = i18n, ns = ns)
        })

        # Reload datatable

        observeEvent(r$vocabulary_table_reload_datatable, {

          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer r$vocabulary_table_reload_datatable"))

          DT::replaceData(r$vocabularies_tables_datatable_proxy, d[[input$vocabularies_table]], resetPaging = FALSE, rownames = FALSE)
        })
      }
      
      # --- --- --- --- --- -- -
      # Import a vocabulary ----
      # --- --- --- --- --- -- -
      
      if (table == "vocabulary"){

        # Import a zip file
        observeEvent(input$import_vocabulary_browse_zip, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$import_vocabulary_browse_zip"))
          shinyjs::click("import_vocabulary_upload_zip")
        })
        
        # Import CSV files
        observeEvent(input$import_vocabulary_browse_csv, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$import_vocabulary_browse_csv"))
          shinyjs::click("import_vocabulary_upload_csv")
        })
        
        # Import files status
        observeEvent(input$import_vocabulary_upload_zip, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - input$import_vocabulary_upload_zip"))
          r$import_vocabulary_status_trigger <- Sys.time()
        })
        
        observeEvent(input$import_vocabulary_upload_csv, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - input$import_vocabulary_upload_csv"))
          r$import_vocabulary_status_trigger <- Sys.time()
        })
        
        observeEvent(r$import_vocabulary_status_trigger, {
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - r$import_vocabulary_status_trigger"))
          
          result <- div()
          
          if (input$import_vocabulary_data_type == "zip"){
            result <- tagList(div(
              span(i18n$t("loaded_file"), " : ", style = "padding-top:5px;"),
              span(input$import_vocabulary_upload_zip$name, style = "font-weight:bold; color:#0078D4;"), style = "padding-top:5px;"))
          }
          
          if (input$import_vocabulary_data_type == "csv"){
            result <- tagList(div(
              span(i18n$t("loaded_files"), " : ", style = "padding-top:5px;"),
              span(toString(input$import_vocabulary_upload_csv$name), style = "font-weight:bold; color:#0078D4;"), style = "padding-top:5px;"))
          }
          
          output$import_vocabulary_status <- renderUI(result)
        })

        # Import button clicked
        observeEvent(input$import_vocabulary_button, {

          if (perf_monitoring) monitor_perf(r = r, action = "start")
          if (debug) print(paste0(Sys.time(), " - mod_settings_data_management - observer input$import_vocabulary_button"))

          # Reset count_rows vars
          r$import_vocabulary_count_rows <- tibble::tibble(table_name = character(), n_rows = integer())

          if(input$import_vocabulary_data_type == "zip" & length(input$import_vocabulary_upload_zip) > 0){
            tryCatch({
              
              # Extract ZIP file
              
              temp_dir <- paste0(r$app_folder, "/temp_files/", Sys.time() %>% stringr::str_replace_all(":| |-", ""), paste0(sample(c(0:9, letters[1:6]), 24, TRUE), collapse = ''))
              zip::unzip(input$import_vocabulary_upload_zip$datapath, exdir = temp_dir)
              
              csv_files <- zip::zip_list(input$import_vocabulary_upload_zip$datapath)
              
              lapply(csv_files$filename, function(file_name){
                
                if (grepl(".csv$", file_name)){
                  
                  # Name of the table
                  table_name <- tolower(substr(file_name, 1, nchar(file_name) - 4))
                  
                  if (table_name %in% c("concept", "domain", "concept_class", "concept_relationship", "relationship", "concept_synonym",
                    "concept_ancestor", "drug_strength")){
                    
                    # Load CSV file
                    data <- vroom::vroom(paste0(temp_dir, "/", file_name), col_types = col_types[[table_name]], progress = FALSE)
                    
                    if ("valid_start_date" %in% names(data)) data <- data %>% dplyr::mutate_at(c("valid_start_date", "valid_end_date"), lubridate::ymd)
                    
                    # Import vocabulary with import_vocabulary_table
                    import_vocabulary_table(output = output, ns = ns, i18n = i18n, r = r, m = m, table_name = table_name, data = data)
                  }
                }
              })
              
              # Render datatable with rows inserted
              
              shinyjs::show("imported_vocabularies_div")
              
              render_datatable(output = output, ns = ns, i18n = i18n, data = r$import_vocabulary_count_rows,
                output_name = "imported_vocabularies", col_names = c(i18n$t("table_name"), i18n$t("num_rows")),
                centered_cols = c("n_rows"), column_widths = c("n_rows" = "200px"))
              
              show_message_bar(output,  "success_importing_vocabulary", "success", i18n = i18n, time = 15000, ns = ns)
            },
              error = function(e) report_bug(r = r, output = output, error_message = "error_importing_vocabulary",
                error_name = paste0(id, " - import vocabulary"), category = "Error", error_report = toString(e), i18n = i18n, ns = ns),
              warning = function(w) report_bug(r = r, output = output, error_message = "error_importing_vocabulary",
                error_name = paste0(id, " - import vocabulary"), category = "Error", error_report = w, i18n = i18n, ns = ns))
          }
          
          else if (input$import_vocabulary_data_type == "csv" & length(input$import_vocabulary_upload_csv) > 0){
            
            if (nrow(input$import_vocabulary_upload_csv) > 0){
              tryCatch({
                
                tables_names <- c("concept", "domain", "concept_class", "concept_relationship", "relationship", "concept_synonym",
                  "concept_ancestor", "drug_strength")
                
                for (i in 1:nrow(input$import_vocabulary_upload_csv)){
                  row <- input$import_vocabulary_upload_csv[i, ]
                  
                  table_name <- substr(row$name, 1, nchar(row$name) - 4) %>% tolower()
                  
                  if (table_name %in% tables_names){
                    
                    # Load CSV file
                    data <- vroom::vroom(row$datapath, col_types = col_types[[table_name]], progress = FALSE)
                    
                    if ("valid_start_date" %in% names(data)) data <- data %>% dplyr::mutate_at(c("valid_start_date", "valid_end_date"), lubridate::ymd)
                    
                    # Import vocabulary with import_vocabulary_table
                    import_vocabulary_table(output = output, ns = ns, i18n = i18n, r = r, m = m, table_name = table_name, data = data)
                  }
                }
                
                # Render datatable with rows inserted
                
                shinyjs::show("imported_vocabularies_div")
                
                render_datatable(output = output, ns = ns, i18n = i18n, data = r$import_vocabulary_count_rows,
                  output_name = "imported_vocabularies", col_names = c(i18n$t("table_name"), i18n$t("row_number")),
                  centered_cols = c("n_rows"), column_widths = c("n_rows" = "200px"))
                
                show_message_bar(output,  "success_importing_vocabulary", "success", i18n = i18n, time = 15000, ns = ns)
              },
                error = function(e) report_bug(r = r, output = output, error_message = "error_importing_vocabulary",
                  error_name = paste0(id, " - import vocabulary"), category = "Error", error_report = toString(e), i18n = i18n, ns = ns),
                warning = function(w) report_bug(r = r, output = output, error_message = "error_importing_vocabulary",
                  error_name = paste0(id, " - import vocabulary"), category = "Error", error_report = w, i18n = i18n, ns = ns))
            }
          }

          if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_data_management - observer input$import_vocabulary_button"))
        })
      }
  })
}
