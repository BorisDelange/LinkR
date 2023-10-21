#' my_studies UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_my_studies_ui <- function(id = character(), i18n = character(), language = "en", languages = tibble::tibble()){
  ns <- NS(id)
  
  cards <- c("all_studies_card", "studies_datatable_card", "studies_options_card", "import_study_card", "export_study_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  })
  
  # Studies options & description divs (with distinct languages fields)
  study_options_divs <- tagList()
  study_description_divs <- tagList()
  
  for (lang in languages$code){
    
    study_options_div <- shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
      div(
        div(class = "input_title", paste0(i18n$t("name"), " (", toupper(lang), ")")),
        div(shiny.fluent::TextField.shinyInput(ns(paste0("study_name_", lang))), style = "width:320px;")
      ),
      div(
        div(class = "input_title", paste0(i18n$t("category"), " (", toupper(lang), ")")),
        div(shiny.fluent::TextField.shinyInput(ns(paste0("study_category_", lang))), style = "width:320px;")
      )
    )
    
    study_description_div <- div(
      div(paste0(i18n$t("description"), " (", toupper(lang), ") :"), style = "font-weight:bold; margin-top:7px; margin-right:5px;"),
      shinyAce::aceEditor(ns(paste0("study_description_", lang)), "", mode = "markdown", 
        code_hotkeys = list(
          "markdown", 
          list(
            save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
            run_all = list(win = "CTRL-SHIFT-ENTER|CTRL-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER|CTRL-ENTER|CMD-ENTER"),
            comment = list(win = "CTRL-SHIFT-C", mac = "CTRL-SHIFT-C|CMD-SHIFT-C")
          )
        ),
        autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;")
    
    if (lang == language) condition <- paste0("input.study_language == '", lang, "' || input.study_language == null")
    else condition <- paste0("input.study_language == '", lang, "'")
    
    study_options_div <- conditionalPanel(condition = paste0("input.study_language == '", lang, "'"), ns = ns, study_options_div)
    
    study_options_divs <- tagList(study_options_divs, conditionalPanel(condition = condition, ns = ns, study_options_div))
    study_description_divs <- tagList(study_description_divs, conditionalPanel(condition = condition, ns = ns, study_description_div))
  }
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    shiny.fluent::reactOutput(ns("study_delete_confirm")),
    shiny.fluent::reactOutput(ns("study_image_delete_confirm")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "dataset_main", text = i18n$t("my_studies"))
    ), maxDisplayedItems = 3),
    
    # --- --- -- -- --
    # Pivot items ----
    # --- --- -- -- --
    
    shinyjs::hidden(
      div(id = ns("menu"),
        shiny.fluent::Pivot(
          id = ns("studies_pivot"),
          onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
          shiny.fluent::PivotItem(id = "all_studies_card", itemKey = "all_studies_card", headerText = i18n$t("all_studies")),
          shiny.fluent::PivotItem(id = "studies_datatable_card", itemKey = "studies_datatable_card", headerText = i18n$t("studies_management")),
          shiny.fluent::PivotItem(id = "studies_options_card", itemKey = "studies_options_card", headerText = i18n$t("study_options")),
          shiny.fluent::PivotItem(id = "import_study_card", itemKey = "import_study_card", headerText = i18n$t("import_study")),
          shiny.fluent::PivotItem(id = "export_study_card", itemKey = "export_study_card", headerText = i18n$t("export_study"))
        )
      )
    ),
    div(
      id = ns("choose_a_dataset_card"),
      make_card("", div(shiny.fluent::MessageBar(i18n$t("choose_a_damatart_left_side"), messageBarType = 5), style = "margin-top:10px;"))
    ),
    forbidden_cards,
    
    # --- --- --- --- --- -
    # All studies card ----
    # --- --- --- --- --- -
    
    shinyjs::hidden(
      div(
        id = ns("all_studies_card"),
        make_card(i18n$t("all_studies"),
          div(
            
          )
        )
      )
    ),
    
    # --- --- --- --- --- -- -- --
    # Studies management card ----
    # --- --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("studies_datatable_card"),
        make_card(i18n$t("studies_management"),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              make_textfield(i18n = i18n, ns = ns, label = "name", id = "study_name", width = "300px"),
              div(shiny.fluent::PrimaryButton.shinyInput(ns("add_study"), i18n$t("add")), style = "margin-top:39px;"),
              style = "position:relative; z-index:1; width:500px;"
            ),
            div(DT::DTOutput(ns("studies_datatable")), style = "margin-top:-30px; z-index:2"),
            div(
              # shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                # shiny.fluent::PrimaryButton.shinyInput(ns("save_studies_management"), i18n$t("save")),
                shiny.fluent::DefaultButton.shinyInput(ns("delete_selection"), i18n$t("delete_selection")),
              # ),
              style = "position:relative; z-index:2; margin-top:-30px;"
            )
          )
        ), br()
      )
    ),
    
    # --- --- --- --- --- ---
    # Study options card ----
    # --- --- --- --- --- ---
    
    shinyjs::hidden(
      div(
        id = ns("studies_options_card"),
        make_shiny_ace_card(i18n$t("study_options"),
          div(
            shiny.fluent::Stack(
              tokens = list(childrenGap = 5),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
                make_combobox(i18n = i18n, ns = ns, label = "study", id = "options_selected_study",
                  width = "320px", allowFreeform = FALSE, multiSelect = FALSE),
                make_dropdown(i18n = i18n, ns = ns, label = "language", id = "study_language", 
                  options = convert_tibble_to_list(languages, key_col = "code", text_col = "language"), value = language, width = "320px"),
                make_textfield(i18n = i18n, ns = ns, label = "version", id = "study_version", width = "80px"),
              ), 
              make_textfield(i18n = i18n, ns = ns, label = "author_s", id = "study_author", width = "660px"), 
              study_options_divs, br(),
              div(
                div(class = "input_title", paste0(i18n$t("grant_access_to"), " :")),
                shiny.fluent::ChoiceGroup.shinyInput(ns("users_allowed_read_group"), options = list(
                  list(key = "everybody", text = i18n$t("everybody_who_has_access_to_dataset")),
                  list(key = "people_picker", text = i18n$t("choose_users"))
                ), className = "inline_choicegroup"),
                conditionalPanel(condition = "input.users_allowed_read_group == 'people_picker'", ns = ns,
                  uiOutput(ns("users_allowed_read_div"))
                )
              ), br(),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
                make_dropdown(i18n = i18n, ns = ns, label = "image", id = "study_image", width = "320px"),
                div(shiny.fluent::DefaultButton.shinyInput(ns("delete_image"), i18n$t("delete_this_image")), style = "margin-top:39px;"),
                div(shiny.fluent::DefaultButton.shinyInput(ns("import_image"), i18n$t("import_image")), style = "margin-top:39px;"),
              )
            ), br(),
            study_description_divs,
            shiny.fluent::Stack(
              tokens = list(childrenGap = 5),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("options_save"), i18n$t("save")), " ",
                shiny.fluent::DefaultButton.shinyInput(ns("execute_options_description"), i18n$t("preview"))
              ),
              br(),
              div(id = ns("description_markdown_output"),
                uiOutput(ns("description_markdown_result")), 
                style = "width: 99%; border-style: dashed; border-width: 1px; padding:0px 8px 0px 8px; margin-right: 5px;"),
              div(style = "display:none;", fileInput(ns("import_image_file"), label = "", multiple = FALSE, accept = c(".jpg", ".jpeg", ".png", ".svg")))
            )
          )
        )
      )
    ),
    
    # --- --- --- --- -- -- -- 
    # Import a study card ----
    # --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("import_study_card"),
        make_card(i18n$t("import_study"),
          div(
            
          )
        )
      )
    ),
    
    # --- --- --- --- -- -- -- 
    # Export a study card ----
    # --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("export_study_card"),
        make_shiny_ace_card(i18n$t("export_study"),
          div(
            shiny.fluent::Stack(
              horizontal = TRUE, tokens = list(childrenGap = 10),
              div(
                div(id = ns("studies_to_export_title"), class = "input_title", i18n$t("studies_to_export")),
                shiny.fluent::Dropdown.shinyInput(ns("studies_to_export"), multiSelect = TRUE,
                  onChanged = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-studies_to_export_trigger', Math.random())"))),
                style = "width:400px;"
              ),
              div(shiny.fluent::PrimaryButton.shinyInput(ns("export_selected_studies"), 
                i18n$t("export_studies"), iconProps = list(iconName = "Upload")), style = "margin-top:39px;")
            ),
            div(DT::DTOutput(ns("studies_to_export_datatable"))),
            div(style = "visibility:hidden;", downloadButton(ns("export_studies_download"), label = ""))
          )
        )
      )
    ),
    br()
  )
}
    
#' my_studies Server Functions
#'
#' @noRd 
mod_my_studies_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), 
  i18n = character(), language = "en", perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - start"))
    
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("all_studies_card", "studies_datatable_card", "studies_options_card", "import_study_card", "export_study_card")
    show_or_hide_cards(r = r, input = input, session = session, id = id, cards = cards)

    # --- --- --- --- --- -
    # Show message bar ----
    # --- --- --- --- --- -
    
    # This allows to show message in multiple pages at the same time (eg when loading a dataset in Studies page, render message bar in Subsets page)
    
    observeEvent(r$show_message_bar, show_message_bar(output, r$show_message_bar$message, r$show_message_bar$type, i18n = i18n, ns = ns))
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    observeEvent(input$help, if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r$help_my_studies_open_panel <- TRUE)
    observeEvent(input$hide_panel, r$help_my_studies_open_panel <- FALSE)
    
    r$help_my_studies_open_panel_light_dismiss <- TRUE
    observeEvent(input$show_modal, r$help_my_studies_open_modal <- TRUE)
    observeEvent(input$hide_modal, {
      r$help_my_studies_open_modal <- FALSE
      r$help_my_studies_open_panel_light_dismiss <- TRUE
    })
    
    # observeEvent(shiny.router::get_page(), {
    #   if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - ", id, " - observer shiny_router::change_page"))
    # 
    #   # Close help pages when page changes
    #   r$help_my_studies_open_panel <- FALSE
    #   r$help_my_studies_open_modal <- FALSE
    # })
    
    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_my_studies_page_", i)]] <- Sys.time())
    })
    
    help_my_studies(output = output, r = r, id = id, language = language, i18n = i18n, ns = ns)
    
    observeEvent(input$copy_code_1, r$help_my_studies_copy_code_1 <- Sys.time())
    
    # --- --- --- --- --- -
    # Update dropdowns ----
    # --- --- --- --- --- -
    
    observeEvent(r$studies, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - observer r$studies"))
      
      options <- convert_tibble_to_list(r$studies %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_study", options = options)
      
      r$reload_local_studies_datatable <- Sys.time()
    })
    
    # --- --- --- --- --- --- --- --
    # When a dataset is selected ----
    # --- --- --- --- --- --- --- --
    
    observeEvent(r$selected_dataset, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - observer r$selected_dataset"))
      
      # Show first card & hide "choose a dataset" card
      shinyjs::hide("choose_a_dataset_card")
      shinyjs::show("menu")
      if (length(input$current_tab) == 0){
        if ("all_studies_card" %in% r$user_accesses) shinyjs::show("all_studies_card")
        else shinyjs::show("all_studies_card_forbidden")
      }
      else{
        if (input$current_tab %in% r$user_accesses) shinyjs::show(input$current_tab)
        else shinyjs::show(paste0(input$current_tab, "_forbidden"))
      }
      
      # The dataset is loaded here, and not in sidenav
      # Placed in sidenav, the dataset is loaded multiple times (each time a page loads its own sidenav)
      
      # Initiate selected_key for study UI
      r$patient_lvl_selected_key <- NA_integer_
      r$aggregated_selected_key <- NA_integer_
      
      # Reset d variables
      
      visit_detail_tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
        "observation", "note", "note_nlp", "fact_relationship", "payer_plan_period", "cost")
      person_tables <- c(visit_detail_tables, "specimen", "death", "drug_era", "dose_era", "condition_era")
      subset_tables <- c(person_tables, "person", "observation_period", "visit_occurrence", "visit_detail")
      main_tables <- c(subset_tables, "location", "care_site", "provider")
      
      sapply(main_tables, function(table) d[[table]] <- tibble::tibble())
      sapply(subset_tables, function(table) d$data_subset[[table]] <- tibble::tibble())
      sapply(person_tables, function(table) d$data_person[[table]] <- tibble::tibble())
      sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())

      # Reset selected_study variable
      m$selected_study <- NA_integer_
      m$selected_person <- NA_integer_ # To prevent bug when execute plugin code from plugin page
      
      # A r variable to update study dropdown, when the load of dataset is finished
      r$loaded_dataset <- r$selected_dataset
      
      # Load studies & scripts related to this dataset
      update_r(r = r, m = m, table = "studies")
      
      r$force_reload_scripts_cache <- FALSE
      
      # Try to load dataset
      tryCatch({
        
        captured_output <- capture.output(run_dataset_code(output, r = r, d = d, dataset_id = r$selected_dataset, i18n = i18n))
        
        # If an error occured
        if (grepl(paste0("\\*\\*", i18n$t("error"), "\\*\\*"), toString(captured_output))){
          r$show_message_bar <- tibble::tibble(message = "fail_load_dataset", type = "severeWarning", trigger = Sys.time())
          report_bug(r = r, output = output, error_message = "fail_load_dataset",
            error_name = paste0(id, " - run server code"), category = "Error", error_report = toString(captured_output), i18n = i18n)
        }
        else {
          r$show_message_bar <- tibble::tibble(message = "import_dataset_success", type = "success", trigger = Sys.time())
          r$load_scripts <- Sys.time() 
        }
      },
      error = function(e){
        r$show_message_bar <- tibble::tibble(message = "fail_load_dataset", type = "severeWarning", trigger = Sys.time())
        report_bug(r = r, output = output, error_message = "fail_load_dataset",
          error_name = paste0(id, " - run server code"), category = "Error", error_report = toString(e), i18n = i18n)
      })
      
      r$reload_studies_datatable <- Sys.time()
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer r$selected_dataset"))
    })
    
    # Load scripts
    
    observeEvent(r$load_scripts, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - observer r$load_scripts"))
      
      # Try to run the scripts associated with this dataset
      # Save runned scripts and success status
      
      r$dataset_loaded_scripts <- tibble::tibble(id = integer(), status = character(), datetime = character())
      r$dataset_loaded_scripts_cache <- tibble::tibble(table = character(), status = character(), datetime = character())
      
      if (nrow(r$scripts) > 0){
        
        scripts <- r$scripts %>% dplyr::inner_join(
          r$options %>% dplyr::filter(category == "dataset_scripts", link_id == r$selected_dataset) %>% dplyr::select(id = value_num),
          by = "id"
        ) %>%
          dplyr::inner_join(r$code %>% dplyr::filter(category == "script") %>% dplyr::select(id = link_id, code), by = "id")
        
        if (nrow(scripts) > 0){
          
          cache_activated <- r$options %>% dplyr::filter(category == "dataset", name == "activate_scripts_cache", link_id == r$selected_dataset) %>% dplyr::pull(value_num) == 1
          
          execute_scripts_files <- FALSE
          
          # If cache activated, load cache
          if(cache_activated){
            loaded_scripts_file_path <- paste0(r$app_folder, "/datasets_files/", r$selected_dataset, "/loaded_scripts.csv")
            if (!file.exists(loaded_scripts_file_path) | r$force_reload_scripts_cache) execute_scripts_files <- TRUE
            
            if (r$force_reload_scripts_cache){
              # Try to load dataset
              tryCatch(run_dataset_code(output, r = r, d = d, dataset_id = r$selected_dataset, i18n = i18n),
                error = function(e){
                  r$show_message_bar <- tibble::tibble(message = "fail_load_dataset", type = "severeWarning", trigger = Sys.time())
                  report_bug(r = r, output = output, error_message = "fail_load_dataset",
                    error_name = paste0(id, " - run server code"), category = "Error", error_report = toString(e), i18n = i18n)
                })
            }
          }
          
          # Else, run scripts
          else execute_scripts_files <- TRUE
          
          # Run scripts
          
          if (execute_scripts_files){
            for (i in 1:nrow(scripts)){
              
              script <- scripts[i, ]
              
              r$dataset_loaded_scripts <- r$dataset_loaded_scripts %>% dplyr::bind_rows(
                tibble::tibble(id = script$id, status = "failure", datetime = as.character(Sys.time())))
              
              # Execute script code
              captured_output <- capture.output(
                tryCatch({
                  eval(parse(text = script$code %>% stringr::str_replace_all("\r", "\n") %>% stringr::str_replace_all("''", "'")))
                  r$dataset_loaded_scripts <- r$dataset_loaded_scripts %>% dplyr::mutate(status = dplyr::case_when(
                    id == script$id ~ "success", TRUE ~ status
                  ))
                },
                  error = function(e){
                    # r$show_message_bar <- tibble::tibble(message = "fail_load_scripts", type = "severeWarning", trigger = Sys.time())
                    report_bug(r = r, output = output, error_message = "fail_load_scripts",
                      error_name = paste0(id, " - run server code"), category = "Error", error_report = toString(e), i18n = i18n)})
              )
            }
          }
          
          if (cache_activated) r$reload_scripts_cache <- Sys.time()
          else {
            if (nrow(r$dataset_loaded_scripts %>% dplyr::filter(status == "failure")) > 0) r$show_message_bar <- 
              tibble::tibble(message = "fail_load_scripts", type = "severeWarning", trigger = Sys.time())
            else r$show_message_bar <- tibble::tibble(message = "run_scripts_success", type = "success", trigger = Sys.time()) 
          }
        }
      }
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer r$load_scripts"))
    })
    
    # Reload scripts cache
    
    observeEvent(r$reload_scripts_cache, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - observer r$reload_scripts_cache"))
      
      req(!is.na(r$selected_dataset))
      
      r$dataset_loaded_scripts_cache <- tibble::tibble(table = character(), status = character(), datetime = character())
      
      # If activate_scripts_cache option activated and if cache doesn't exists, save data as CSV files
      if(r$options %>% dplyr::filter(category == "dataset", name == "activate_scripts_cache", link_id == r$selected_dataset) %>% dplyr::pull(value_num) == 1){
        
        tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
          "observation", "death", "note", "note_nlp", "specimen", "fact_relationship", "payer_plan_period", "cost", 
          "drug_era", "dose_era", "condition_era", "person", "observation_period", "visit_occurrence", "visit_detail",
          "location", "care_site", "provider")
        
        dataset_file_path <- paste0(r$app_folder, "/datasets_files/", r$selected_dataset)
        loaded_scripts_file_path <- paste0(r$app_folder, "/datasets_files/", r$selected_dataset, "/loaded_scripts.csv")
        
        # If dataset folder doesn't exist, create it
        if (!dir.exists(dataset_file_path)) dir.create(dataset_file_path)
        
        # If cache doesn't exist, create cache
        if (!file.exists(loaded_scripts_file_path) | r$force_reload_scripts_cache){
          
          # Save data as CSV or parquet files
          
          for (table in tables){
            
            if (d[[table]] %>% dplyr::count() %>% dplyr::pull() > 0){
              
              r$dataset_loaded_scripts_cache <- r$dataset_loaded_scripts_cache %>% dplyr::bind_rows(
                tibble::tibble(table = table, status = "failure", datetime = as.character(Sys.time())))
              
              # Do the same as when the dataset was loaded : save as CSV if save_as = "csv" for import_dataset ...
              save_as <- r$dataset_loaded_tables %>% dplyr::filter(table == !!table) %>% dplyr::pull(save_as)
              if (save_as == "csv"){
                tryCatch({
                  readr::write_csv(d[[table]] %>% dplyr::collect(), paste0(r$app_folder, "/datasets_files/", r$selected_dataset, "/", table, "_with_scripts.csv"))
                  r$dataset_loaded_scripts_cache <- r$dataset_loaded_scripts_cache %>% dplyr::mutate(status = dplyr::case_when(table == !!table ~ "success", TRUE ~ status))
                },
                  error = function(e)add_log_entry(r = r, category = "Error", name = paste0("reload_scripts_cache - error_saving_csv - id = ", r$selected_dataset), value = toString(e)))
              }
              else if (save_as == "parquet") {
                if (!requireNamespace("arrow", quietly = TRUE)) add_log_entry(r = r, category = "Error", name = paste0("reload_scripts_cache - error_loading_parquet - id = ", r$selected_dataset), value = i18n$t("package_arrow_not_installed"))
                else {
                  tryCatch({
                    arrow::write_parquet(d[[table]] %>% dplyr::collect(), paste0(r$app_folder, "/datasets_files/", r$selected_dataset, "/", table, "_with_scripts.parquet"))
                    r$dataset_loaded_scripts_cache <- r$dataset_loaded_scripts_cache %>% dplyr::mutate(status = dplyr::case_when(table == !!table ~ "success", TRUE ~ status))
                  },
                  error = function(e)add_log_entry(r = r, category = "Error", name = paste0("reload_scripts_cache - error_saving_parquet - id = ", r$selected_dataset), value = toString(e)))
                }
              }
              # readr::write_csv(d[[table]] %>% dplyr::select(-dplyr::contains("concept_name"), -dplyr::contains("unit_concept_code")),
              #   paste0(r$app_folder, "/datasets_files/", r$selected_dataset, "/", table, "_with_scripts.csv"))
            }
          }
          
          # Save a CSV file for informations on loaded scripts
          readr::write_csv(r$dataset_loaded_scripts, paste0(r$app_folder, "/datasets_files/", r$selected_dataset, "/loaded_scripts.csv"))
        }
        
        # Load cache if already exists
        
        if (file.exists(loaded_scripts_file_path)){
          for (table in tables){
            table_args <- r$dataset_loaded_tables %>% dplyr::filter(table == !!table)
            
            if (nrow(table_args) > 0){
              if (table_args$save_as %in% c("csv", "parquet")){
                dataset_folder <- paste0(r$app_folder, "/datasets_files/", r$selected_dataset)
                table_file_path <- paste0(dataset_folder, "/", table, "_with_scripts.", table_args$save_as)
                
                if (file.exists(table_file_path)){
                  
                  omop_version <- r$options %>% dplyr::filter(category == "dataset" & link_id == r$selected_dataset & name == "omop_version") %>% dplyr::pull(value)
                  
                  if (table_args$save_as == "csv"){
                    col_types <- switch(table, 
                      "person" = "iiiiiTTiiiiiccicici",
                      "observation_period" = "iiDDi",
                      "visit_occurrence" = "iiiDTDTiiiciicici",
                      "visit_detail" = "iiiDTDTiiiciiciciii",
                      "condition_occurrence" = "iiiDTDTiiciiicic",
                      "drug_exposure" = "iiiDTDTDiciniciciiicicc",
                      "procedure_occurrence" = "iiiDTiiiiiicic",
                      "device_exposure" = "iiiDTDTiciiiici",
                      "measurement" = "iiiDTciiniinniiicicc",
                      "observation" = "iiiDTinciiiiiicicciiT",
                      "death" = "iDTiici",
                      "note" = "iiiiDTiicciiiiic",
                      "note_nlp" = "iiiccciicDTccc",
                      "specimen" = "iiiiDTniiiccccc",
                      "fact_relationship" = "iiiii",
                      "location" = "icccccccnn",
                      "location_hisTory" = "iiciDD",
                      "care_site" = "iciicc",
                      "provider" = "iccciiiiccici",
                      "payer_plan_period" = "iiiDDiciiciiciicicici",
                      "cost" = "iiiiiiicinDDDiicci",
                      "drug_era" = "iiiTTii",
                      "dose_era" = "iiiinTT",
                      "condition_era" = "iiiTTi"
                    )
                    if (table == "person" & omop_version %in% c("5.3", "5.4")) col_types <- "iiiiiTiiiiiccicici"
                    if (table == "observation" & omop_version == "5.3") col_types <-  "iiiDTinciiiiiicicc"
                    if (table == "observation" & omop_version == "5.4") col_types <-  "iiiDTinciiiiiicicccii"
                    if (table == "location" & omop_version == "5.3") col_types <-  "iccccccc"
                    if (table == "drug_era" & omop_version %in% c("5.3", "5.4")) col_types <- "iiiDDii"
                    if (table == "dose_era" & omop_version %in% c("5.3", "5.4")) col_types <- "iiiinDD"
                    if (table == "condition_era" & omop_version %in% c("5.3", "5.4")) col_types <- "iiiDDi"
                    
                    tryCatch({
                      if (table_args$read_with == "vroom") d[[table]] <- vroom::vroom(table_file_path, col_types = col_types) 
                      else if (table_args$read_with == "duckdb"){
                        if (!requireNamespace("duckdb", quietly = TRUE)) add_log_entry(r = r, category = "Error", name = paste0("reload_scripts_cache - error_loading_duckdb - id = ", dataset_id), value = i18n$t("package_duckdb_not_installed"))
                        else {
                          duckdb_drv <- duckdb::duckdb()
                          if (length(r$duckdb_drv) == 0) r$duckdb_drv <- c(duckdb_drv)
                          else r$duckdb_drv <- c(r$duckdb_drv, duckdb_drv)
                          con <- DBI::dbConnect(duckdb_drv, dbdir = paste0(dataset_folder, "/dataset.duckdb"))
                          
                          table_exists <- table %in% DBI::dbListTables(con)
                          if (table_exists){
                            DBI::dbExecute(con, paste0("DROP TABLE ", table))
                            request <- paste0("CREATE TABLE ", table, " AS SELECT * FROM read_csv_auto('", table_file_path, "');")
                            DBI::dbExecute(con, request)
                          }
                          
                          d[[table]] <- dplyr::tbl(con, table)
                        }
                      }
                      else if (table_args$read_with == "spark"){
                        if (!requireNamespace("sparklyr", quietly = TRUE)) add_log_entry(r = r, category = "Error", name = paste0("reload_scripts_cache - error_loading_sparklyr - id = ", dataset_id), value = i18n$t("package_sparklyr_not_installed"))
                        else {
                          con <- sparklyr::spark_connect(master = "local")
                          
                          table_exists <- table %in% dplyr::src_tbls(con)
                          
                          if (table_exists) DBI::dbExecute(con, paste0("DROP TABLE ", table))
                          sparklyr::spark_read_csv(con, name = table, path = table_file_path)
                          
                          d[[table]] <- dplyr::tbl(con, table)
                        }
                      }
                      
                      r$dataset_loaded_scripts_cache <- r$dataset_loaded_scripts_cache %>% dplyr::mutate(status = dplyr::case_when(table == !!table ~ "success", TRUE ~ status))
                    },
                    error = function(e) add_log_entry(r = r, category = "Error", name = paste0("reload_scripts_cache - error_loading_csv - id = ", r$selected_dataset), value = toString(e)))
                  }
                  else if (table_args$save_as == "parquet"){
                    tryCatch({
                      if (table_args$read_with == "arrow") d[[table]] <- arrow::read_parquet(table_file_path)
                      else if (table_args$read_with == "duckdb"){
                        if (!requireNamespace("duckdb", quietly = TRUE)) add_log_entry(r = r, category = "Error", name = paste0("reload_scripts_cache - error_loading_duckdb - id = ", dataset_id), value = i18n$t("package_duckdb_not_installed"))
                        else {
                          duckdb_drv <- duckdb::duckdb()
                          if (length(r$duckdb_drv) == 0) r$duckdb_drv <- c(duckdb_drv)
                          else r$duckdb_drv <- c(r$duckdb_drv, duckdb_drv)
                          con <- DBI::dbConnect(duckdb_drv, dbdir = paste0(dataset_folder, "/dataset.duckdb"))
                          
                          table_exists <- table %in% DBI::dbListTables(con)
                          if (table_exists){
                            DBI::dbExecute(con, paste0("DROP TABLE ", table))
                            request <- paste0("CREATE TABLE ", table, " AS SELECT * FROM parquet_scan('", table_file_path, "');")
                            DBI::dbExecute(con, request)
                          }
                          
                          d[[table]] <- dplyr::tbl(con, table)
                        }
                      }
                      else if (table_args$read_with == "spark"){
                        if (!requireNamespace("sparklyr", quietly = TRUE)) add_log_entry(r = r, category = "Error", name = paste0("reload_scripts_cache - error_loading_sparklyr - id = ", dataset_id), value = i18n$t("package_sparklyr_not_installed"))
                        else {
                          con <- sparklyr::spark_connect(master = "local")
                          
                          table_exists <- table %in% dplyr::src_tbls(con)
                          
                          if (table_exists) DBI::dbExecute(con, paste0("DROP TABLE ", table))
                          sparklyr::spark_read_parquet(con, name = table, path = table_file_path)
                          
                          d[[table]] <- dplyr::tbl(con, table)
                        }
                      }
                      r$dataset_loaded_scripts_cache <- r$dataset_loaded_scripts_cache %>% dplyr::mutate(status = dplyr::case_when(table == !!table ~ "success", TRUE ~ status))
                    },
                    error = function(e) add_log_entry(r = r, category = "Error", name = paste0("reload_scripts_cache - error_loading_parquet - id = ", r$selected_dataset), value = toString(e)))
                  }
                } 
              } 
            }
          }
        }
      }
      
      if (nrow(r$dataset_loaded_scripts %>% dplyr::filter(status == "failure")) > 0 |
          nrow(r$dataset_loaded_scripts_cache %>% dplyr::filter(status == "failure")) > 0) r$show_message_bar <- 
        tibble::tibble(message = "fail_load_scripts", type = "severeWarning", trigger = Sys.time())
      else r$show_message_bar <- tibble::tibble(message = "run_scripts_success", type = "success", trigger = Sys.time())
      
      r$force_reload_scripts_cache <- FALSE
      r$update_scripts_cache_card <- Sys.time()
      
      # Join d tables with d$dataset_all_concepts
      
      r$merge_concepts_and_d_vars <- Sys.time()
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer r$reload_scripts_cache"))
    })
    
    # Once the dataset is loaded, load studies & scripts
    observeEvent(r$loaded_dataset, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - observer r$loaded_dataset"))
      
      # Load studies datatable
      # r$reload_studies_datatable <- Sys.time()
      
      # Update dropdown for study options
      options <- convert_tibble_to_list(r$studies %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_study", options = options)
    })
    
    # --- --- --- --- --- --- --- --
    # When a study is selected ----
    # --- --- --- --- --- --- --- --
    
    observeEvent(m$selected_study, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - observer r$selected_study"))
      
      req(!is.na(m$selected_study))
      # Show first card & hide "choose a dataset" card
      shinyjs::hide("choose_a_study_card_description")
      shinyjs::show("studies_description_content")
      
      # Reset d variables
      
      visit_detail_tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
        "observation", "note", "note_nlp", "fact_relationship", "payer_plan_period", "cost")
      person_tables <- c(visit_detail_tables, "specimen", "death", "drug_era", "dose_era", "condition_era")
      subset_tables <- c(person_tables, "person", "observation_period", "visit_occurrence", "visit_detail")
      main_tables <- c(subset_tables, "location", "care_site", "provider")
      
      sapply(subset_tables, function(table) d$data_subset[[table]] <- tibble::tibble())
      sapply(person_tables, function(table) d$data_person[[table]] <- tibble::tibble())
      sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())
      
      # Update study options combobox
      options <- convert_tibble_to_list(r$studies %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = m$selected_study, text = r$studies %>% dplyr::filter(id == m$selected_study) %>% dplyr::pull(name))
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_study", options = options, value = value)
      
      # Subsets depending on the selected study
      update_r(r = r, m = m, table = "subsets")

      # Reset selected_subset, selected_person & selected_visit_detail
      m$selected_subset <- NA_integer_
      m$selected_person <- NA_integer_
      m$selected_visit_detail <- NA_integer_
      
      # Select patients belonging to subsets of this study
      update_r(r = r, m = m, table = "subsets_persons")
      
      # Load patients options
      sql <- glue::glue_sql("SELECT * FROM persons_options WHERE study_id = {m$selected_study}", .con = m$db)
      m$persons_options <- DBI::dbGetQuery(m$db, sql)
      
      # Load study description
      
      # Get description from database
      # study_description <- r$options %>% dplyr::filter(category == "study" & name == "markdown_description" & link_id == m$selected_study) %>% 
      #   dplyr::pull(value) %>% stringr::str_replace_all("\r", "\n") %>% stringr::str_replace_all("''", "'")
      # 
      # tryCatch({
      #   
      #   # Clear temp dir
      #   unlink(paste0(r$app_folder, "/temp_files/", r$user_id), recursive = TRUE, force = TRUE)
      #   
      #   markdown_settings <- paste0("```{r setup, include=FALSE}\nknitr::opts_knit$set(root.dir = '", 
      #     r$app_folder, "/temp_files/", r$user_id')\n",
      #     "knitr::opts_chunk$set(root.dir = '", r$app_folder, "/temp_files/", r$user_id, "/', fig.path = '", r$app_folder, "/temp_files/')\n```\n")
      #   
      #   markdown_file <- paste0(markdown_settings, study_description)
      #   
      #   # Create temp dir
      #   dir <- paste0(r$app_folder, "/temp_files/", r$user_id)
      #   file <- paste0(dir, "/", as.character(Sys.time()) %>% stringr::str_replace_all(":", "_"), ".Md")
      #   if (!dir.exists(dir)) dir.create(dir)
      #   
      #   # Create the markdown file
      #   knitr::knit(text = markdown_file, output = file, quiet = TRUE)
      #   
      #   output$studies_description_markdown_result <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(file))))
      # }, error = function(e) "")
    })
    
    # --- --- --- --- --- --- --- --
    # When a subset is selected ----
    # --- --- --- --- --- --- --- --
    
    observeEvent(m$selected_subset, {
      req(!is.na(m$selected_subset))
      
      # Reset d variables

      visit_detail_tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
        "observation", "note", "note_nlp", "fact_relationship", "payer_plan_period", "cost")
      person_tables <- c(visit_detail_tables, "specimen", "death", "drug_era", "dose_era", "condition_era")

      sapply(person_tables, function(table) d$data_person[[table]] <- tibble::tibble())
      sapply(visit_detail_tables, function(table) d$data_visit_detail[[table]] <- tibble::tibble())
      
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
    
    # --- --- --- --- ---
    # Create a study ----
    # --- --- --- --- ---
    
    observeEvent(input$add_study, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - observer input$add_study"))
      
      new_data <- list()
      new_data$name <- coalesce2(type = "char", x = input$study_name)
      new_data$study_name <- new_data$name
      new_data$patient_lvl_tab_group <- get_last_row(r$db, "patient_lvl_tabs_groups") + 1
      new_data$aggregated_tab_group <- get_last_row(r$db, "aggregated_tabs_groups") + 1
      new_data$dataset <- r$selected_dataset
      
      add_settings_new_data(session = session, output = output, r = r, d = d, m = m, i18n = i18n, id = "my_studies", 
        data = new_data, table = "studies", required_textfields = "study_name", req_unique_values = "name")
      
      # Reload datatable
      r$reload_studies_datatable <- Sys.time()
      # r$studies_temp <- r$studies %>% dplyr::filter(dataset_id == r$selected_dataset) %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer input$add_study"))
    })
    
    # --- --- --- --- --- ---
    # Studies management ----
    # --- --- --- --- --- ---
    
    # Action buttons for each tab / page
    action_buttons <- c("options", "delete")
    
    studies_management_editable_cols <- ""
    studies_management_sortable_cols <- c("id", "name", "dataset_id", "data_source_id", "study_id", "creator_id", "creation_datetime", "update_datetime")
    studies_management_column_widths <- c("id" = "80px", "creation_datetime" = "130px", "update_datetime" = "130px", "action" = "80px", "creator_id" = "200px")
    studies_management_centered_cols <- c("id", "creator", "creation_datetime", "update_datetime", "action")
    studies_management_searchable_cols <- c("name", "data_source_id", "dataset_id", "study_id", "creator_id")
    studies_management_factorize_cols <- c("dataset_id", "creator_id")
    studies_management_hidden_cols <- c("id", "dataset_id", "patient_lvl_tab_group_id", "aggregated_tab_group_id", "deleted", "modified", "creator_id")
    studies_management_col_names <- get_col_names("studies", i18n)
    
    # Prepare data for datatable
    # This is on a different observer, because r$studies is loaded just before r$reload_studies_datatable is set to Sys.time()
    # If we put this code in the observer of r$selected_dataset, it has no time to execute update_r for studies
    # So r$studies is not updated
    
    observeEvent(r$reload_studies_datatable, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - observer r$reload_studies_datatable"))
      
      if (nrow(r$studies) == 0) {
        
        data_studies_datatable <- tibble::tibble(id = integer(), name = character(), dataset_id = factor(),
          patient_lvl_tab_group_id = integer(), aggregated_tab_group_id = integer(), creator_id = factor(), 
          creation_datetime = character(), update_datetime = character(), deleted = integer(), modified = logical(), action = character())
      }
      
      if (nrow(r$studies) > 0){
        
        r$studies_temp <- r$studies %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
        
        # Reset selected studies for export_studies and export_studies_selected
        r$export_studies_temp <- r$studies_temp
        r$export_studies_selected <- r$export_studies_temp %>% dplyr::slice(0)
        
        # Prepare data for datatable
        
        r$studies_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
          table = "studies", factorize_cols = studies_management_factorize_cols, action_buttons = action_buttons, data_input = r$studies_temp)
        data_studies_datatable <- r$studies_datatable_temp
        
        r$export_studies_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
          table = "studies", action_buttons = "add", data_input = r$export_studies_temp)
        data_export_studies_datatable <- r$export_studies_datatable_temp
      }
        
      if (length(r$studies_datatable_proxy) == 0){
        
        # Render datatables
        
        render_datatable(output = output, ns = ns, i18n = i18n, data = data_studies_datatable,
          output_name = "studies_datatable", col_names = get_col_names("studies", i18n),
          editable_cols = studies_management_editable_cols, sortable_cols = studies_management_sortable_cols, centered_cols = studies_management_centered_cols, 
          column_widths = studies_management_column_widths, searchable_cols = studies_management_searchable_cols, 
          filter = TRUE, factorize_cols = studies_management_factorize_cols, hidden_cols = studies_management_hidden_cols,
          selection = "multiple")
        
        render_datatable(output = output, ns = ns, i18n = i18n, data = data_export_studies_datatable,
          output_name = "studies_to_export_datatable", col_names = get_col_names("studies", i18n),
          sortable_cols = studies_management_sortable_cols, centered_cols = studies_management_centered_cols, 
          column_widths = studies_management_column_widths, searchable_cols = studies_management_searchable_cols, 
          filter = TRUE, hidden_cols = studies_management_hidden_cols)
        
        # Create a proxy for datatable
        
        r$studies_datatable_proxy <- DT::dataTableProxy("studies_datatable", deferUntilFlush = FALSE)
        r$studies_to_export_datatable_proxy <- DT::dataTableProxy("studies_to_export_datatable", deferUntilFlush = FALSE)
      }
      
      if (length(r$studies_datatable_proxy) > 0){
        DT::replaceData(r$studies_datatable_proxy, data_studies_datatable, resetPaging = FALSE, rownames = FALSE)
        DT::replaceData(r$studies_to_export_datatable_proxy, data_export_studies_datatable, resetPaging = FALSE, rownames = FALSE)
      }
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer r$reload_studies_datatable"))
    })
    
    # Reload datatable
    # observeEvent(r$studies_temp, {
    #   
    #   if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - observer r$studies_temp"))
    # 
    #   # Reload datatable_temp variable
    #   if (nrow(r$studies_temp) == 0) r$studies_datatable_temp <- tibble::tibble(id = integer(), name = character(), dataset_id = factor(),
    #     patient_lvl_tab_group_id = integer(), aggregated_tab_group_id = integer(), creator_id = factor(),
    #     creation_datetime = character(), update_datetime = character(), deleted = integer(), modified = logical(), action = character())
    #   
    #   if (nrow(r$studies_temp) > 0) r$studies_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
    #     table = "studies", factorize_cols = studies_management_factorize_cols, action_buttons = action_buttons, data_input = r$studies_temp)
    # 
    #   # Reload data of datatable
    #   if (length(r$studies_datatable_proxy) > 0) DT::replaceData(r$studies_datatable_proxy, 
    #     r$studies_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    # })
    
    # Updates on datatable data
    observeEvent(input$studies_datatable_cell_edit, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - observer input$studies_datatable_cell_edit"))
      
      edit_info <- input$studies_datatable_cell_edit
      r$studies_temp <- DT::editData(r$studies_temp, edit_info, rownames = FALSE)
      
      # Store that this row has been modified
      r$studies_temp[[edit_info$row, "modified"]] <- TRUE
    })
    
    # Save updates
    observeEvent(input$save_studies_management, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - observer input$save_studies_management"))
      
      req(nrow(r$studies %>% dplyr::filter(dataset_id == r$selected_dataset)) > 0)
      
      save_settings_datatable_updates(output = output, r = r, ns = ns, 
        table = "studies", r_table = "studies", i18n = i18n, duplicates_allowed = FALSE)
      
      # Update sidenav dropdown with the new study
      r$reload_studies <- Sys.time()
    })
    
    # Delete a row in datatable
    
    study_delete_prefix <- "study"
    study_dialog_title <- "studies_delete"
    study_dialog_subtext <- "studies_delete_subtext"
    study_react_variable <- "study_delete_confirm"
    study_table <- "studies"
    study_id_var_sql <- "id"
    study_id_var_r <- "delete_study"
    study_delete_message <- "studies_deleted"
    study_reload_variable <- "reload_studies"
    study_information_variable <- "study_deleted"
    study_delete_variable <- paste0(study_delete_prefix, "_open_dialog")
    
    delete_element(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
      delete_prefix = study_delete_prefix, dialog_title = study_dialog_title, dialog_subtext = study_dialog_subtext,
      react_variable = study_react_variable, table = study_table, id_var_sql = study_id_var_sql, id_var_r = study_id_var_r, 
      delete_message = study_delete_message, translation = TRUE, reload_variable = study_reload_variable, 
      information_variable = study_information_variable)
    
    # Delete one row (with icon on DT)
    
    observeEvent(input$deleted_pressed, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - observer input$deleted_pressed"))
      
      r$delete_study <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
      r[[study_delete_variable]] <- TRUE
      
      # Reload datatable (to unselect rows)
      DT::replaceData(r$studies_datatable_proxy, r$studies_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Delete multiple rows (with "Delete selection" button)
    
    observeEvent(input$delete_selection, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - observer input$delete_selection"))
      
      req(length(input$studies_datatable_rows_selected) > 0)
      
      r$delete_study <- r$studies_temp[input$studies_datatable_rows_selected, ] %>% dplyr::pull(id)
      r[[study_delete_variable]] <- TRUE
    })
    
    observeEvent(r$reload_studies, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - observer r$reload_studies"))
      
      r$studies_temp <- r$studies %>% dplyr::filter(dataset_id == r$selected_dataset) %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      r$reload_studies_datatable <- Sys.time()
      
      # Reset selected study
      m$selected_study <- NA_integer_
    })
    
    # --- --- --- --- --
    # Study options ----
    # --- --- --- --- --
    
    observeEvent(input$options, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - observer input$options"))
      
      # Get link_id variable, to update options div
      link_id <- as.integer(substr(input$options, nchar("options_") + 1, nchar(input$options)))

      options <- convert_tibble_to_list(r$studies %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = link_id, text = r$studies %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))

      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_study", options = options, value = value)

      # Reload datatable (to unselect rows)
      DT::replaceData(r$studies_datatable_proxy, r$studies_datatable_temp, resetPaging = FALSE, rownames = FALSE)

      # Set current pivot to options_card
      button_name <- gsub("'", "\\\\'", i18n$t('study_options'))
      shinyjs::runjs(glue::glue("$('#{id}-studies_pivot button[name=\"{button_name}\"]').click();"))
    })
    
    observeEvent(input$options_selected_study, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - observer input$options_selected_study"))
      
      if (length(input$options_selected_study) > 1) link_id <- input$options_selected_study$key
      else link_id <- input$options_selected_study
      
      options <- r$options %>% dplyr::filter(category == "study", link_id == !!link_id)
      
      picker_options <-
        r$users %>%
        dplyr::left_join(r$users_statuses %>% dplyr::select(user_status_id = id, user_status = name), by = "user_status_id") %>%
        dplyr::transmute(
          key = id, 
          imageInitials = paste0(substr(firstname, 0, 1), substr(lastname, 0, 1)),
          text = paste0(firstname, " ", lastname), 
          secondaryText = user_status)
      
      picker_value <-
        picker_options %>%
        dplyr::mutate(n = 1:dplyr::n()) %>%
        dplyr::inner_join(
          options %>%
            dplyr::filter(name == "user_allowed_read") %>%
            dplyr::select(key = value_num),
          by = "key"
        ) %>%
        dplyr::pull(key)
      
      # Users allowed read group
      value_group <- options %>% dplyr::filter(name == "users_allowed_read_group") %>% dplyr::pull(value)
      
      shiny.fluent::updateChoiceGroup.shinyInput(session, "users_allowed_read_group",
        value = options %>% dplyr::filter(name == "users_allowed_read_group") %>% dplyr::pull(value))
      output$users_allowed_read_div <- renderUI({
        make_people_picker(
          i18n = i18n, ns = ns, id = "users_allowed_read", label = "users", options = picker_options, value = picker_value,
          width = "100%", style = "padding-bottom:10px;")
      })
      
      # Study version, author, images and descriptions
      
      study_folder <- paste0(r$app_folder, "/studies/", options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
      files_list <- list.files(path = study_folder, pattern = "(?i)*.\\.(jpeg|jpg|JPG|JPEG|png|PNG|SVG|svg)$")
      shiny.fluent::updateDropdown.shinyInput(session, "study_image", 
        options = convert_tibble_to_list(tibble::tibble(text = c("", files_list), key = c("", files_list)), key_col = "key", text_col = "text"))
      
      for (field in c("version", "author", paste0("name_", r$languages$code), paste0("category_", r$languages$code))){
        value <- options %>% dplyr::filter(name == field) %>% dplyr::pull(value) %>% stringr::str_replace_all("''", "'")
        if (length(value) == 0) value <- ""
        if (length(value) > 0) if (is.na(value)) value <- ""
        
        shiny.fluent::updateTextField.shinyInput(session, paste0("study_", field), value = value)
      } 
      
      for (field in c(paste0("description_", r$languages$code))){
        value <- options %>% dplyr::filter(name == field) %>% dplyr::pull(value) %>% stringr::str_replace_all("''", "'")
        if (length(value) == 0) value <- ""
        if (length(value) > 0) if (is.na(value)) value <- ""
        shinyAce::updateAceEditor(session, paste0("study_", field), value = value) 
      }
    })
    
    # Save updates
    
    observeEvent(input$study_description_fr_save, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - observer input$study_description_fr_save"))
      r$study_save_options <- Sys.time()
    })
    observeEvent(input$study_description_en_save, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - observer input$study_description_en_save"))
      r$study_save_options <- Sys.time()
    })
    observeEvent(input$options_save, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - observer input$options_save"))
      r$study_save_options <- Sys.time()
    })
    
    observeEvent(r$study_save_options, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - observer r$study_save_options"))

      req(length(input$options_selected_study) > 0)
      if (length(input$options_selected_study) > 1) link_id <- input$options_selected_study$key
      else link_id <- input$options_selected_study
      
      study_name <- input[[paste0("study_name_", language)]]
      
      if (is.na(study_name) | study_name == "") shiny.fluent::updateTextField.shinyInput(session, 
        paste0("study_name_", language), errorMessage = i18n$t("provide_valid_name"))
      
      req(!is.na(study_name) & study_name != "")
      
      duplicate_names <- FALSE
      current_names <- r$studies_temp %>% dplyr::filter(id != link_id) %>% dplyr::pull(name)
      if (study_name %in% current_names){
        duplicate_names <- TRUE
        shiny.fluent::updateTextField.shinyInput(session, paste0("study_name_", language), errorMessage = i18n$t("name_already_used"))
      }
      
      req(!duplicate_names)
      
      if (!is.na(study_name) & study_name != "") shiny.fluent::updateTextField.shinyInput(session, 
        paste0("study_name_", language), errorMessage = NULL)
      
      data <- list()
      for (field in c("study_version", "study_author", "users_allowed_read", "users_allowed_read_group",
        paste0("study_name_", r$languages$code), paste0("study_category_", r$languages$code), paste0("study_description_", r$languages$code))) data[[stringr::str_replace(field, "study_", "")]] <- input[[field]]
      
      save_settings_options(output = output, r = r, id = id, category = "study", code_id_input = paste0("options_", link_id),
        i18n = i18n, data = data, page_options = c("version", "author", "users_allowed_read", paste0("description_", r$languages$code),
          paste0("name_", r$languages$code), paste0("category_", r$languages$code)))
      
      # Change study_name & update_datetime in studies table
      new_update_datetime <- as.character(Sys.time())
      sql <- glue::glue_sql("UPDATE studies SET name = {study_name}, update_datetime = {new_update_datetime} WHERE id = {link_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      r$studies <- r$studies %>% dplyr::mutate(
        name = dplyr::case_when(id == link_id ~ study_name, TRUE ~ name),
        update_datetime = dplyr::case_when(id == link_id ~ new_update_datetime, TRUE ~ update_datetime))
      r$studies_temp <- r$studies %>%
        dplyr::mutate_at(c("creation_datetime", "update_datetime"), format_datetime, language = language, sec = FALSE) %>%
        dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      r$reload_studies_datatable <- Sys.time()
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer input$options_save"))
    })
    
    # Delete an image
    
    observeEvent(input$delete_image, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_studies - observer input$delete_image"))
      req(length(input$study_image) > 0 & input$study_image != "")
      r$studies_delete_image <- TRUE
    })
    
    r$studies_delete_image <- FALSE
    output$study_image_delete_confirm <- shiny.fluent::renderReact({
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_studies - output$study_image_delete_confirm"))
      
      shiny.fluent::Dialog(
        hidden = !r$studies_delete_image,
        onDismiss = htmlwidgets::JS("function() { Shiny.setInputValue('study_delete_image_hide_dialog', Math.random()); }"),
        dialogContentProps = list(
          type = 0,
          title = i18n$t("study_image_delete"),
          closeButtonAriaLabel = "Close",
          subText = tagList(i18n$t("study_image_delete_subtext"), br(), br())
        ),
        modalProps = list(),
        shiny.fluent::DialogFooter(
          shiny.fluent::PrimaryButton.shinyInput(ns("study_delete_image_delete_confirmed"), text = i18n$t("delete")),
          shiny.fluent::DefaultButton.shinyInput(ns("study_delete_image_delete_canceled"), text = i18n$t("dont_delete"))
        )
      )
    })
    
    observeEvent(input$study_delete_image_hide_dialog, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_studies - observer input$study_delete_image_hide_dialog"))
      r$studies_delete_image <- FALSE
    })
    observeEvent(input$study_delete_image_delete_canceled, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_studies - observer input$study_delete_image_delete_canceled"))
      r$studies_delete_image <- FALSE
    })
    
    observeEvent(input$study_delete_image_delete_confirmed, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_studies - observer input$study_delete_image_delete_confirmed"))
      
      req(input$study_image != "")
      tryCatch({
        if (length(input$options_selected_study) > 1) link_id <- input$options_selected_study$key
        else link_id <- input$options_selected_study
        
        study <- r$studies %>% dplyr::filter(id == link_id) %>%
          dplyr::left_join(r$options %>% dplyr::filter(category == "study", name == "unique_id") %>% dplyr::select(id = link_id, unique_id = value), by = "id")
        
        study_folder <- paste0(r$app_folder, "/studies/", study$unique_id)
        unlink(paste0(study_folder, "/", input$study_image))
        
        files_list <- list.files(path = study_folder, pattern = "(?i)*.\\.(jpeg|jpg|JPG|JPEG|png|PNG|SVG|svg)$")
        shiny.fluent::updateDropdown.shinyInput(session, "study_image",
          options = convert_tibble_to_list(tibble::tibble(text = c("", files_list), key = c("", files_list)), key_col = "key", text_col = "text"), value = "")
        
        show_message_bar(output,  "image_deleted", "success", i18n = i18n, ns = ns)
        
      }, error = function(e) report_bug(r = r, output = output, error_message = "error_deleting_image",
        error_name = paste0(id, " - delete study image"), category = "Error", error_report = toString(e), i18n = i18n))
      
      r$studies_delete_image <- FALSE
    })
    
    # Import an image
    
    observeEvent(input$import_image, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_studies - observer input$import_image"))
      req(input$options_selected_study)
      shinyjs::click("import_image_file")
    })
    
    observeEvent(input$import_image_file, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_studies - observer input$import_image_file"))
      
      tryCatch({
        
        if (length(input$options_selected_study) > 1) link_id <- input$options_selected_study$key
        else link_id <- input$options_selected_study
        
        study <- r$studies %>%
          dplyr::filter(id == link_id) %>%
          dplyr::left_join(r$options %>% dplyr::filter(category == "study", name == "unique_id") %>% dplyr::select(id = link_id, unique_id = value), by = "id")
        
        study_folder <- paste0(r$app_folder, "/studies/", study$unique_id)
        
        if (!dir.exists(study_folder)) dir.create(study_folder, recursive = TRUE)
        
        file.copy(input$import_image_file$datapath, paste0(study_folder, "/", input$import_image_file$name), overwrite = TRUE)
        
        # Update dropdown
        
        files_list <- list.files(path = study_folder, pattern = "(?i)*.\\.(jpeg|jpg|JPG|JPEG|png|PNG|SVG|svg)$")
        shiny.fluent::updateDropdown.shinyInput(session, "study_image",
          options = convert_tibble_to_list(tibble::tibble(text = c("", files_list), key = c("", files_list)), key_col = "key", text_col = "text"))
        
        show_message_bar(output,  "image_imported", "success", i18n = i18n, ns = ns)
        
      }, error = function(e) report_bug(r = r, output = output, error_message = "error_importing_image",
        error_name = paste0(id, " - import study image"), category = "Error", error_report = toString(e), i18n = i18n))
    })
    
    # Render markdown
    
    observeEvent(input$execute_options_description, {
      if (debug) cat(paste0("\n", Sys.time(), " - mod_studies - observer input$execute_options_description"))
      r$study_options_description_trigger <- Sys.time()
    })
    
    sapply(r$languages$code, function(lang){
      observeEvent(input[[paste0("study_description_", lang, "_run_all")]], {
        if (debug) cat(paste0("\n", Sys.time(), " - mod_studies - observer input$study_description_", lang, "_run_all"))
        r$study_options_description_trigger <- Sys.time()
      })
    })
    
    observeEvent(r$study_options_description_trigger, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) cat(paste0("\n", Sys.time(), " - mod_my_studies - observer r$study_options_description_trigger"))
      
      if (length(input$options_selected_study) > 1) link_id <- input$options_selected_study$key
      else link_id <- input$options_selected_study
      
      options <- r$options %>% dplyr::filter(category == "study", link_id == !!link_id)
      study_folder <- paste0(r$app_folder, "/studies/", options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
      
      options_description <- isolate(input[[paste0("study_description_", input$study_language)]] %>% 
        stringr::str_replace_all("\r", "\n")) %>%
        stringr::str_replace_all("%study_folder%", study_folder)
      
      tryCatch({
        
        # Clear temp dir
        
        markdown_settings <- paste0("```{r setup, include=FALSE}\nknitr::opts_knit$set(root.dir = '", 
          r$app_folder, "/temp_files/", r$user_id, "/markdowns')\n",
          "knitr::opts_chunk$set(root.dir = '", r$app_folder, "/temp_files/", r$user_id, "/markdowns', fig.path = '", r$app_folder, "/temp_files/", r$user_id, "/markdowns')\n```\n")
        
        markdown_file <- paste0(markdown_settings, options_description)
        
        # Create temp dir
        dir <- paste0(r$app_folder, "/temp_files", r$user_id, "//markdowns")
        file <- paste0(dir, "/", paste0(sample(c(0:9, letters[1:6]), 8, TRUE), collapse = ''), "_", as.character(Sys.time()) %>% stringr::str_replace_all(":", "_"), ".Md")
        if (!dir.exists(dir)) dir.create(dir)
        
        # Create the markdown file
        knitr::knit(text = markdown_file, output = file, quiet = TRUE)
        
        output$description_markdown_result <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(file))))
      }, error = function(e) "")
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer input$execute_options_description"))
    })
    
    # --- --- --- --- ---
    # Import a study ----
    # --- --- --- --- ---
    
    # --- --- --- --- ---
    # Export a study ----
    # --- --- --- --- ---
    
    # When add button is clicked
    observeEvent(input$add_item, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_studies - observer input$add_item"))
      
      # Get ID of selected study
      link_id <- as.integer(substr(input$add_item, nchar("add_item_") + 1, nchar(input$add_item)))
      
      # If this study is not already selected, add it to the selected items dropdown
      
      value <- integer(1)
      if (nrow(r$export_studies_selected) > 0) value <- r$export_studies_selected %>% dplyr::pull(id)
      
      if (link_id %not_in% value){
        
        r$export_studies_selected <- r$export_studies_temp %>% dplyr::filter(id == link_id) %>%
          dplyr::bind_rows(r$export_studies_selected)
        
        # Update dropdown of selected items
        options <- convert_tibble_to_list(r$export_studies_selected, key_col = "id", text_col = "name", i18n = i18n)
        value <- r$export_studies_selected %>% dplyr::pull(id)
        shiny.fluent::updateDropdown.shinyInput(session, "studies_to_export",
          options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
      }
      
    })
    
    # When dropdown is modified
    observeEvent(input$studies_to_export_trigger, {

      if (debug) cat(paste0("\n", Sys.time(), " - mod_studies - observer input$studies_to_export"))

      r$export_studies_selected <- r$export_studies_selected %>%
        dplyr::filter(id %in% input$studies_to_export)

      options <- convert_tibble_to_list(r$export_studies_selected, key_col = "id", text_col = "name", i18n = i18n)
      value <- r$export_studies_selected %>% dplyr::pull(id)
      shiny.fluent::updateDropdown.shinyInput(session, "studies_to_export",
        options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
    })
    
    # Export scripts
    observeEvent(input$export_selected_studies, {

      if (debug) cat(paste0("\n", Sys.time(), " - mod_studies - observer input$export_studies"))

      req(nrow(r$export_studies_selected) > 0)

      shinyjs::click("export_studies_download")
    })
    
    output$export_studies_download <- downloadHandler(

      filename = function() paste0("linkr_export_studies_",
        Sys.time() %>% stringr::str_replace_all(" ", "_") %>% stringr::str_replace_all(":", "_") %>% as.character(), ".zip"),

      content = function(file){
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) cat(paste0("\n", Sys.time(), " - mod_studies - output$export_studies_download"))

        owd <- setwd(tempdir())
        on.exit(setwd(owd))

        temp_dir <- paste0(r$app_folder, "/temp_files/", r$user_id, "/studies/", Sys.time() %>% stringr::str_replace_all(":| |-", ""), paste0(sample(c(0:9, letters[1:6]), 24, TRUE), collapse = ''))
        dir.create(temp_dir, recursive = TRUE)

        for (study_id in r$export_studies_selected %>% dplyr::pull(id)){

          study <- r$studies %>% dplyr::filter(id == study_id)
          options <- r$options %>% dplyr::filter(category == "study", link_id == study_id)

          # Create folder if doesn't exist
          study_dir <- paste0(r$app_folder, "/studies/", options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
          if (!dir.exists(study_dir)) dir.create(study_dir, recursive = TRUE)

          # Create XML file
          xml <- XML::newXMLDoc()
          studies_node <- XML::newXMLNode("studies", doc = xml)
          study_node <- XML::newXMLNode("study", parent = studies_node, doc = xml)
          XML::newXMLNode("app_version", r$app_version, parent = study_node)
          for(name in c("unique_id", "version", "author", paste0("name_", r$languages$code), paste0("category_", r$languages$code))) XML::newXMLNode(name,
            options %>% dplyr::filter(name == !!name) %>% dplyr::pull(value), parent = study_node)
          for(name in c(paste0("description_", r$languages$code))) XML::newXMLNode(name,
            options %>% dplyr::filter(name == !!name) %>% dplyr::pull(value) %>% stringr::str_replace_all("''", "'"), parent = study_node)
          for (name in c("creation_datetime", "update_datetime")) XML::newXMLNode(name, study %>% dplyr::pull(get(!!name)), parent = study_node)

          list_of_files <- list.files(study_dir)

          # Add images filenames in the XML
          images <- list_of_files[grepl("\\.(png|jpg|jpeg|svg)$", tolower(list_of_files))]
          images_node <- XML::newXMLNode("images", paste(images, collapse = ";;;"), parent = study_node)

          # Create XML file
          XML::saveXML(xml, file = paste0(study_dir, "/study.xml"))

          # Copy files to temp dir
          temp_dir_copy <- paste0(temp_dir, "/", options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
          if (!dir.exists(temp_dir_copy)) dir.create(temp_dir_copy, recursive = TRUE)
          XML::saveXML(xml, file = paste0(temp_dir_copy, "/study.xml"))
          file.copy(
            paste0(study_dir, "/", list_of_files),
            paste0(temp_dir_copy, "/", list_of_files),
            overwrite = TRUE
          )

          # Export CSV files

          # Tables :
          # - patient_lvl_tabs_groups
          # - patient_lvl_tabs
          # - patient_lvl_widgets
          # - patient_lvl_widgets_concepts
          # - patient_lvl_widgets_options
          # - aggregated_tabs_groups
          # - aggregated_tabs
          # - aggregated_widgets
          # - aggregated_widgets_concepts
          # - aggregated_widgets_options
          # - plugins
          # - code
          # - options

          corresponding_ids <- list()
          for (table in c("plugins", "options", "code")) corresponding_ids[[table]] <- tibble::tibble(old_id = integer(), new_id = integer())
          data <- list()

          for (type in c("patient_lvl", "aggregated")){

            # Get data
            
            ### tabs_groups
            sql <- glue::glue_sql("SELECT * FROM {`paste0(type, '_tabs_groups')`} WHERE id = {study[[paste0(type, '_tab_group_id')]]} AND deleted IS FALSE", .con = r$db)
            data[[paste0(type, "_tabs_groups")]] <- DBI::dbGetQuery(r$db, sql) %>% dplyr::mutate(creator_id = NA_integer_)
            corresponding_ids[[paste0(type, "_tabs_groups")]] <- tibble::tibble(old_id = as.integer(data[[paste0(type, "_tabs_groups")]]$id), new_id = 1L)
            
            ### tabs
            sql <- glue::glue_sql("SELECT * FROM {`paste0(type, '_tabs')`} WHERE tab_group_id = {study[[paste0(type, '_tab_group_id')]]} AND deleted IS FALSE", .con = r$db)
            data[[paste0(type, "_tabs")]] <- DBI::dbGetQuery(r$db, sql) %>% dplyr::mutate(creator_id = NA_integer_)
            if (nrow(data[[paste0(type, "_tabs")]]) > 0) corresponding_ids[[paste0(type, "_tabs")]] <- data[[paste0(type, "_tabs")]] %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = 1:dplyr::n())
            
            ### widgets
            sql <- glue::glue_sql("SELECT * FROM {`paste0(type, '_widgets')`} WHERE tab_id IN ({data[[paste0(type, '_tabs')]] %>% dplyr::pull(id)*}) AND deleted IS FALSE", .con = r$db)
            data[[paste0(type, "_widgets")]] <- DBI::dbGetQuery(r$db, sql) %>% dplyr::mutate(creator_id = NA_integer_)
            if (nrow(data[[paste0(type, "_widgets")]]) > 0) corresponding_ids[[paste0(type, "_widgets")]] <- data[[paste0(type, "_widgets")]] %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = 1:dplyr::n())
            
            ### widgets_concepts
            sql <- glue::glue_sql("SELECT * FROM {`paste0(type, '_widgets_concepts')`} WHERE widget_id IN ({data[[paste0(type, '_widgets')]] %>% dplyr::pull(id)*}) AND deleted IS FALSE", .con = m$db)
            data[[paste0(type, "_widgets_concepts")]] <- DBI::dbGetQuery(m$db, sql) %>% dplyr::mutate(creator_id = NA_integer_)
            if (nrow(data[[paste0(type, "_widgets_concepts")]]) > 0) corresponding_ids[[paste0(type, "_widgets_concepts")]] <- data[[paste0(type, "_widgets_concepts")]] %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = 1:dplyr::n())
            
            ### widgets_options
            sql <- glue::glue_sql("SELECT * FROM {`paste0(type, '_widgets_options')`} WHERE widget_id IN ({data[[paste0(type, '_widgets')]] %>% dplyr::pull(id)*}) AND deleted IS FALSE", .con = m$db)
            data[[paste0(type, "_widgets_options")]] <- DBI::dbGetQuery(m$db, sql) %>% dplyr::mutate(creator_id = NA_integer_, person_id = NA_integer_)
            if (nrow(data[[paste0(type, "_widgets_options")]]) > 0) corresponding_ids[[paste0(type, "_widgets_options")]] <- data[[paste0(type, "_widgets_options")]] %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = 1:dplyr::n())
            
            ### plugins
            if (type == "patient_lvl") tab_type_id <- 1L else tab_type_id <- 2L
            sql <- glue::glue_sql("SELECT * FROM plugins WHERE tab_type_id = {tab_type_id} AND id IN ({data[[paste0(type, '_widgets')]] %>% dplyr::distinct(plugin_id) %>% dplyr::pull()*}) AND deleted IS FALSE", .con = r$db)
            data[[paste0(type, "_plugins")]] <- DBI::dbGetQuery(r$db, sql)
            if (nrow(data[[paste0(type, "_plugins")]]) > 0){
              if (nrow(corresponding_ids$plugins) > 0) last_row <- max(corresponding_ids$plugins$new_id)
              else last_row <- 0L
              corresponding_ids$plugins <- corresponding_ids$plugins %>% dplyr::bind_rows(
                data[[paste0(type, "_plugins")]] %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = last_row + 1:dplyr::n()))
            }
            
            ### options (for plugins)
            sql <- glue::glue_sql("SELECT * FROM options WHERE category = 'plugin' AND link_id IN ({data[[paste0(type, '_plugins')]] %>% dplyr::distinct(id) %>% dplyr::pull()*}) AND deleted IS FALSE", .con = r$db)
            data[[paste0(type, "_options")]] <- DBI::dbGetQuery(r$db, sql)
            if (nrow(data[[paste0(type, "_options")]]) > 0){
              if (nrow(corresponding_ids$options) > 0) last_row <- max(corresponding_ids$options$new_id)
              else last_row <- 0L
              corresponding_ids$options <- corresponding_ids$options %>% dplyr::bind_rows(
                data[[paste0(type, "_options")]] %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = last_row + 1:dplyr::n()))
            }
            
            ### code (for plugins)
            sql <- glue::glue_sql("SELECT * FROM code WHERE category = 'plugin' AND link_id IN ({data[[paste0(type, '_plugins')]] %>% dplyr::distinct(id) %>% dplyr::pull()*}) AND deleted IS FALSE", .con = r$db)
            data[[paste0(type, "_code")]] <- DBI::dbGetQuery(r$db, sql)
            if (nrow(data[[paste0(type, "_code")]]) > 0){
              if (nrow(corresponding_ids$code) > 0) last_row <- max(corresponding_ids$code$new_id)
              else last_row <- 0L
              corresponding_ids$code <- corresponding_ids$code %>% dplyr::bind_rows(
                data[[paste0(type, "_code")]] %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = last_row + 1:dplyr::n()))
            }
            
            # Change IDs
            
            ### tabs_groups
            data[[paste0(type, "_tabs_groups")]] <- data[[paste0(type, "_tabs_groups")]] %>% dplyr::mutate(id = 1L)
            
            ### tabs
            if (nrow(data[[paste0(type, "_tabs")]]) > 0) data[[paste0(type, "_tabs")]] <- data[[paste0(type, "_tabs")]] %>%
              dplyr::left_join(corresponding_ids[[paste0(type, "_tabs")]] %>% dplyr::select(id = old_id, new_id), by = "id") %>%
              dplyr::left_join(corresponding_ids[[paste0(type, "_tabs")]] %>% dplyr::select(parent_tab_id = old_id, new_parent_tab_id = new_id), by = "parent_tab_id") %>%
              dplyr::select(-id, -parent_tab_id) %>%
              dplyr::rename(id = new_id, parent_tab_id = new_parent_tab_id) %>%
              dplyr::relocate(id, .before = "name") %>%
              dplyr::relocate(parent_tab_id, .before = "display_order") %>%
              dplyr::mutate(tab_group_id = 1L)
            
            ### widgets
            if (nrow(data[[paste0(type, "_widgets")]]) > 0) data[[paste0(type, "_widgets")]] <- data[[paste0(type, "_widgets")]] %>%
              dplyr::left_join(corresponding_ids[[paste0(type, "_widgets")]] %>% dplyr::select(id = old_id, new_id = new_id), by = "id") %>%
              dplyr::left_join(corresponding_ids[[paste0(type, "_tabs")]] %>% dplyr::select(tab_id = old_id, new_tab_id = new_id), by = "tab_id") %>%
              dplyr::left_join(corresponding_ids$plugins %>% dplyr::select(plugin_id = old_id, new_plugin_id = new_id), by = "plugin_id") %>%
              dplyr::select(-id, -tab_id, -plugin_id) %>%
              dplyr::rename(id = new_id, tab_id = new_tab_id, plugin_id = new_plugin_id) %>%
              dplyr::relocate(id, .before = "name") %>%
              dplyr::relocate(tab_id, .after = "name") %>%
              dplyr::relocate(plugin_id, .before = "display_order")
            
            ### widgets_concepts
            if (nrow(data[[paste0(type, "_widgets_concepts")]]) > 0) data[[paste0(type, "_widgets_concepts")]] <- data[[paste0(type, "_widgets_concepts")]] %>%
              dplyr::left_join(corresponding_ids[[paste0(type, "_widgets_concepts")]] %>% dplyr::select(id = old_id, new_id = new_id), by = "id") %>%
              dplyr::left_join(corresponding_ids[[paste0(type, "_widgets")]] %>% dplyr::select(widget_id = old_id, new_widget_id = new_id), by = "widget_id") %>%
              dplyr::select(-id, -widget_id) %>%
              dplyr::rename(id = new_id, widget_id = new_widget_id) %>%
              dplyr::relocate(id, .before = "concept_id") %>%
              dplyr::relocate(widget_id, .after = "id")
            
            ### widgets_options
            if (nrow(data[[paste0(type, "_widgets_options")]]) > 0) data[[paste0(type, "_widgets_options")]] <- data[[paste0(type, "_widgets_options")]] %>%
              dplyr::left_join(corresponding_ids[[paste0(type, "_widgets_options")]] %>% dplyr::select(id = old_id, new_id = new_id), by = "id") %>%
              dplyr::left_join(corresponding_ids[[paste0(type, "_widgets")]] %>% dplyr::select(widget_id = old_id, new_widget_id = new_id), by = "widget_id") %>%
              dplyr::select(-id, -widget_id) %>%
              dplyr::rename(id = new_id, widget_id = new_widget_id) %>%
              dplyr::relocate(id, .before = "person_id") %>%
              dplyr::relocate(widget_id, .after = "id")
            
            ### plugins
            if (nrow(data[[paste0(type, "_plugins")]]) > 0) data[[paste0(type, "_plugins")]] <- data[[paste0(type, "_plugins")]] %>%
              dplyr::left_join(corresponding_ids$plugins %>% dplyr::select(id = old_id, new_id), by = "id") %>%
              dplyr::select(-id) %>%
              dplyr::rename(id = new_id) %>%
              dplyr::relocate(id, .before = "name")
            
            ### options
            if (nrow(data[[paste0(type, "_options")]]) > 0) data[[paste0(type, "_options")]] <- data[[paste0(type, "_options")]] %>%
              dplyr::left_join(corresponding_ids$options %>% dplyr::select(id = old_id, new_id), by = "id") %>%
              dplyr::left_join(corresponding_ids$plugins %>% dplyr::select(link_id = old_id, new_link_id = new_id), by = "link_id") %>%
              dplyr::select(-id, -link_id) %>%
              dplyr::rename(id = new_id, link_id = new_link_id) %>%
              dplyr::relocate(id, .before = "category") %>%
              dplyr::relocate(link_id, .before = "name")
            
            ### code
            if (nrow(data[[paste0(type, "_code")]]) > 0) data[[paste0(type, "_code")]] <- data[[paste0(type, "_code")]] %>%
              dplyr::left_join(corresponding_ids$code %>% dplyr::select(id = old_id, new_id), by = "id") %>%
              dplyr::left_join(corresponding_ids$plugins %>% dplyr::select(link_id = old_id, new_link_id = new_id), by = "link_id") %>%
              dplyr::select(-id, -link_id) %>%
              dplyr::rename(id = new_id, link_id = new_link_id) %>%
              dplyr::relocate(id, .before = "category") %>%
              dplyr::relocate(link_id, .before = "code")
          }
          
          # Put plugins, options & code together
          data$plugins <- data$patient_lvl_plugins %>% dplyr::bind_rows(data$aggregated_plugins)
          data$options <- data$patient_lvl_options %>% dplyr::bind_rows(data$aggregated_options)
          data$code <- data$patient_lvl_code %>% dplyr::bind_rows(data$aggregated_code)
          
          # Create CSV files
          dir.create(paste0(temp_dir_copy, "/app_database"))
          for (csv_file in c("patient_lvl_tabs_groups", "patient_lvl_tabs", "patient_lvl_widgets", "patient_lvl_widgets_concepts", "patient_lvl_widgets_options", 
            "aggregated_tabs_groups", "aggregated_tabs", "aggregated_widgets", "aggregated_widgets_concepts", "aggregated_widgets_options",
            "plugins", "options", "code")) readr::write_csv(data[[csv_file]],  paste0(temp_dir_copy, "/app_database/", csv_file, ".csv"))
          
          # Copy plugins folders
          dir.create(paste0(temp_dir_copy, "/plugins"))
          dir.create(paste0(temp_dir_copy, "/plugins/patient_lvl"))
          dir.create(paste0(temp_dir_copy, "/plugins/aggregated"))
          
          if (nrow(data$plugins) > 0){
            for (i in 1:nrow(data$plugins)){
              plugin <- data$plugins[i, ]
              unique_id <- data$options %>% dplyr::filter(category == "plugin" & name == "unique_id" & link_id == plugin$id) %>% dplyr::pull(value)
              if (plugin$tab_type_id == 1) type <- "patient_lvl" else type <- "aggregated"
              
              plugin_dir <- paste0(r$app_folder, "/plugins/", type, "/", unique_id)
              if (dir.exists(plugin_dir)){
                list_of_files <- list.files(plugin_dir)
                dir.create(paste0(temp_dir_copy, "/plugins/", type, "/", unique_id))
                file.copy(paste0(plugin_dir, "/", list_of_files), paste0(temp_dir_copy, "/plugins/", type, "/", unique_id, "/", list_of_files))
              }
            }
          }
        }

        # Create XML file with all exported studies

        studies_tibble <- tibble::tibble(app_version = character(), unique_id = character(), version = character(), author = character())
        prefixes <- c("description", "name", "category")
        new_cols <- outer(prefixes, r$languages$code, paste, sep = "_") %>% as.vector()
        for(col in new_cols) if(!col %in% colnames(studies_tibble)) studies_tibble <- studies_tibble %>% dplyr::mutate(!!col := "")
        studies_tibble <- studies_tibble %>% dplyr::mutate(creation_datetime = character(), update_datetime = character())

        dirs <- list.dirs(temp_dir, full.names = TRUE)
        for (dir in dirs){
          if (dir != temp_dir & !grepl("/plugins", dir) & !grepl("/app_database", dir)){
            studies_tibble <-
              studies_tibble %>%
              dplyr::bind_rows(
                xml2::read_xml(paste0(dir, "/study.xml")) %>%
                  XML::xmlParse() %>%
                  XML::xmlToDataFrame(nodes = XML::getNodeSet(., "//study")) %>%
                  tibble::as_tibble()
              )
          }
        }

        studies_xml <- XML::newXMLDoc()
        studies_node <- XML::newXMLNode("studies", doc = studies_xml)

        studies_nodes <- apply(studies_tibble, 1, function(x) {
          study_node <- XML::newXMLNode("study")
          XML::addChildren(study_node, lapply(names(x), function(y) XML::newXMLNode(y, x[y])))
        })

        XML::xmlParent(studies_nodes) <- studies_node

        XML::saveXML(studies_xml, file = paste0(temp_dir, "/studies.xml"))

        # Create a ZIP

        zip::zipr(file, list.files(temp_dir, full.names = TRUE))

        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_studies - output$export_studies_download"))
      }
    )
  })
}
