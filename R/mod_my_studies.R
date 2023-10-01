#' my_studies UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_my_studies_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  
  cards <- c("studies_description_card", "studies_datatable_card", "studies_options_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  })
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    shiny.fluent::reactOutput(ns("study_delete_confirm")),
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
          # shiny.fluent::PivotItem(id = "studies_description_card", itemKey = "studies_description_card", headerText = i18n$t("study_description")),
          shiny.fluent::PivotItem(id = "studies_datatable_card", itemKey = "studies_datatable_card", headerText = i18n$t("studies_management")),
          shiny.fluent::PivotItem(id = "studies_options_card", itemKey = "studies_options_card", headerText = i18n$t("study_options"))
        )
      )
    ),
    div(
      id = ns("choose_a_dataset_card"),
      make_card("", div(shiny.fluent::MessageBar(i18n$t("choose_a_damatart_left_side"), messageBarType = 5), style = "margin-top:10px;"))
    ),
    forbidden_cards,
    
    # --- --- --- --- --- --- ---
    # Study description card ----
    # --- --- --- --- --- --- ---
    
    shinyjs::hidden(
      div(
        id = ns("studies_description_card"),
        div(id = ns("studies_description_content"),
          make_card(i18n$t("studies_description_card"),
            # uiOutput(ns("studies_description_markdown_result"))
          ), br()
        ),
        div(
          id = ns("choose_a_study_card_description"),
          make_card("", div(shiny.fluent::MessageBar(i18n$t("choose_a_study_left_side"), messageBarType = 5), style = "margin-top:10px;"))
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
              div(shiny.fluent::PrimaryButton.shinyInput(ns("add_study"), i18n$t("add")), style = "margin-top:38px;"),
              style = "position:relative; z-index:1; width:500px;"
            ),
            div(DT::DTOutput(ns("studies_datatable")), style = "margin-top:-30px; z-index:2"),
            div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("save_studies_management"), i18n$t("save")),
                shiny.fluent::DefaultButton.shinyInput(ns("delete_selection"), i18n$t("delete_selection"))
              ),
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
                make_textfield(i18n = i18n, ns = ns, label = "author", id = "study_author", width = "320px"),
                make_textfield(i18n = i18n, ns = ns, label = "version", id = "study_version", width = "80px")
              ), 
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
                make_textfield(i18n = i18n, ns = ns, label = "name_fr", id = "study_name_fr", width = "320px"),
                make_textfield(i18n = i18n, ns = ns, label = "name_en", id = "study_name_en", width = "320px")
              ),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
                make_textfield(i18n = i18n, ns = ns, label = "category_fr", id = "study_category_fr", width = "320px"),
                make_textfield(i18n = i18n, ns = ns, label = "category_en", id = "study_category_en", width = "320px")
              ), br(),
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
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                div(paste0(i18n$t("description"), " :"), style = "font-weight:bold; margin-top:7px; margin-right:5px;"),
                shiny.fluent::ChoiceGroup.shinyInput(ns("study_description_language"), value = "fr", options = list(
                  list(key = "fr", text = "FR"),
                  list(key = "en", text = "EN")
                ), className = "inline_choicegroup")
              )
            ),
            conditionalPanel(condition = "input.study_description_language == 'fr'", ns = ns,
              div(shinyAce::aceEditor(ns("study_description_fr"), "", mode = "markdown", 
                code_hotkeys = list(
                  "markdown", 
                  list(
                    save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
                    run_all = list(win = "CTRL-SHIFT-ENTER|CTRL-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER|CTRL-ENTER|CMD-ENTER") 
                  )
                ),
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;")),
            conditionalPanel(condition = "input.study_description_language == 'en'", ns = ns,
              div(shinyAce::aceEditor(ns("study_description_en"), "", mode = "markdown", 
                code_hotkeys = list(
                  "markdown", 
                  list(
                    save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
                    run_all = list(win = "CTRL-SHIFT-ENTER|CTRL-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER|CTRL-ENTER|CMD-ENTER") 
                  )
                ),
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;")),
            shiny.fluent::Stack(
              tokens = list(childrenGap = 5),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("options_save"), i18n$t("save")),
                shiny.fluent::DefaultButton.shinyInput(ns("execute_options_description"), i18n$t("preview"))
              ),
              br(),
              div(id = ns("description_markdown_output"),
                uiOutput(ns("description_markdown_result")), 
                style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
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
          div(br(),
            div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5)), br(),
            div(shiny.fluent::MessageBar(
              div(
                strong("A faire"),
                p("Importer une étude nécessite d'importer :",
                  tags$ul(
                    tags$li("Importer l'étude en elle-même (table études de la BDD)"),
                    tags$li("Importer les données relatives à l'étude (tabs, données modifiées sur patients et sur tabs)"),
                    tags$li("S'assurer que les plugins sont tous installés et à la bonne version")
                  )
                ),
                p("Comment faire pour les correspondances entre membres ?")
              ),
              messageBarType = 0)
            )
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
        make_card(i18n$t("export_study"),
          div(br(),
            div(shiny.fluent::MessageBar(i18n$t("in_progress"), messageBarType = 5)), br(),
            div(shiny.fluent::MessageBar(
              div(
                strong("A faire"),
                p("Même principe que pour l'import d'une étude")
              ),
              messageBarType = 0)
            )
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
    
    if (debug) print(paste0(Sys.time(), " - mod_my_studies - start"))
    
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("studies_description_card", "studies_datatable_card", "studies_options_card")
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
    #   if (debug) print(paste0(Sys.time(), " - mod_my_studies - ", id, " - observer shiny_router::change_page"))
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
    
    # --- --- --- --- --- --- --- --
    # When a dataset is selected ----
    # --- --- --- --- --- --- --- --
    
    observeEvent(r$selected_dataset, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$selected_dataset"))
      
      # Show first card & hide "choose a dataset" card
      shinyjs::hide("choose_a_dataset_card")
      shinyjs::show("menu")
      if (length(input$current_tab) == 0){
        if ("studies_datatable_card" %in% r$user_accesses) shinyjs::show("studies_datatable_card")
        else shinyjs::show("studies_datatable_card_forbidden")
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
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$load_scripts"))
      
      # Try to run the scripts associated with this dataset
      # Save runned scripts and success status
      
      r$dataset_loaded_scripts <- tibble::tibble(id = integer(), status = character(), datetime = character())
      
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
          
          if (nrow(r$dataset_loaded_scripts %>% dplyr::filter(status == "failure")) > 0) r$show_message_bar <- 
            tibble::tibble(message = "fail_load_scripts", type = "severeWarning", trigger = Sys.time())
          else r$show_message_bar <- tibble::tibble(message = "run_scripts_success", type = "success", trigger = Sys.time())
        }
      }
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer r$load_scripts"))
    })
    
    # Reload scripts cache
    
    observeEvent(r$reload_scripts_cache, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$reload_scripts_cache"))
      
      req(!is.na(r$selected_dataset))
      
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
          
          # Save data as CSV files
          for (table in tables){
            if (d[[table]] %>% dplyr::count() %>% dplyr::pull() > 0){
              # Select cols without merged cols
              readr::write_csv(d[[table]] %>% dplyr::select(-dplyr::contains("concept_name"), -dplyr::contains("unit_concept_code")),
                paste0(r$app_folder, "/datasets_files/", r$selected_dataset, "/", table, "_with_scripts.csv"))
            }
          }
          
          # Save a CSV file for informations on loaded scripts
          readr::write_csv(r$dataset_loaded_scripts, paste0(r$app_folder, "/datasets_files/", r$selected_dataset, "/loaded_scripts.csv"))
        }
        
        # Load cache if already exists
        
        if (file.exists(loaded_scripts_file_path)){
          for (table in tables){
            table_file_path <- paste0(r$app_folder, "/datasets_files/", r$selected_dataset, "/", table, "_with_scripts.csv")
            
            if (file.exists(table_file_path)){
              
              omop_version <- r$options %>% dplyr::filter(category == "dataset" & link_id == r$selected_dataset & name == "omop_version") %>% dplyr::pull(value)
              
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
              
              d[[table]] <- vroom::vroom(table_file_path, col_types = col_types)
            }
          }
        }
      }
      
      if (nrow(r$dataset_loaded_scripts %>% dplyr::filter(status == "failure")) > 0) r$show_message_bar <- 
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
      
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$loaded_dataset"))
      
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
      
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$selected_study"))
      
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
      #   unlink(paste0(r$app_folder, "/temp_files"), recursive = TRUE, force = TRUE)
      #   
      #   markdown_settings <- paste0("```{r setup, include=FALSE}\nknitr::opts_knit$set(root.dir = '", 
      #     r$app_folder, "/temp_files')\n",
      #     "knitr::opts_chunk$set(root.dir = '", r$app_folder, "/temp_files/', fig.path = '", r$app_folder, "/temp_files/')\n```\n")
      #   
      #   markdown_file <- paste0(markdown_settings, study_description)
      #   
      #   # Create temp dir
      #   dir <- paste0(r$app_folder, "/temp_files")
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
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$add_study"))
      
      new_data <- list()
      new_data$name <- coalesce2(type = "char", x = input$study_name)
      new_data$study_name <- new_data$name
      new_data$patient_lvl_tab_group <- get_last_row(r$db, "patient_lvl_tabs_groups") + 1
      new_data$aggregated_tab_group <- get_last_row(r$db, "aggregated_tabs_groups") + 1
      new_data$dataset <- r$selected_dataset
      
      add_settings_new_data(session = session, output = output, r = r, d = d, m = m, i18n = i18n, id = "my_studies", 
        data = new_data, table = "studies", required_textfields = "study_name", req_unique_values = "name")
      
      # Reload datatable
      r$studies_temp <- r$studies %>% dplyr::filter(dataset_id == r$selected_dataset) %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer input$add_study"))
    })
    
    # --- --- --- --- --- ---
    # Studies management ----
    # --- --- --- --- --- ---
    
    # Action buttons for each tab / page
    action_buttons <- c("options", "delete")
    
    studies_management_editable_cols <- c("name")
    studies_management_sortable_cols <- c("id", "name", "dataset_id", "data_source_id", "study_id", "creator_id", "datetime")
    studies_management_column_widths <- c("id" = "80px", "creation_datetime" = "130px", "update_datetime" = "130px", "action" = "80px", "creator_id" = "200px")
    studies_management_centered_cols <- c("id", "creator", "creation_datetime", "update_datetime", "action")
    studies_management_searchable_cols <- c("name", "data_source_id", "dataset_id", "study_id", "creator_id")
    studies_management_factorize_cols <- c("dataset_id", "creator_id")
    studies_management_hidden_cols <- c("id", "dataset_id", "patient_lvl_tab_group_id", "aggregated_tab_group_id", "deleted", "modified")
    studies_management_col_names <- get_col_names("studies", i18n)
    
    # Prepare data for datatable
    # This is on a different observer, because r$studies is loaded just before r$reload_studies_datatable is set to Sys.time()
    # If we put this code in the observer of r$selected_dataset, it has no time to execute update_r for studies
    # So r$studies is not updated
    
    observeEvent(r$reload_studies_datatable, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$reload_studies_datatable"))
      
      if (nrow(r$studies) == 0) {
        
        data <- tibble::tibble(id = integer(), name = character(), dataset_id = factor(),
          patient_lvl_tab_group_id = integer(), aggregated_tab_group_id = integer(), creator_id = factor(), 
          creation_datetime = character(), update_datetime = character(), deleted = integer(), modified = logical(), action = character())
      }
      
      if (nrow(r$studies) > 0){
        
        r$studies_temp <- r$studies %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
        
        # Prepare data for datatable
        
        r$studies_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
          table = "studies", factorize_cols = studies_management_factorize_cols, action_buttons = action_buttons, data_input = r$studies_temp)
        data <- r$studies_datatable_temp
      }
        
      if (length(r$studies_datatable_proxy) == 0){
        
        # Render datatable
        
        render_datatable(output = output, ns = ns, i18n = i18n, data = data,
          output_name = "studies_datatable", col_names = get_col_names("studies", i18n),
          editable_cols = studies_management_editable_cols, sortable_cols = studies_management_sortable_cols, centered_cols = studies_management_centered_cols, 
          column_widths = studies_management_column_widths, searchable_cols = studies_management_searchable_cols, 
          filter = TRUE, factorize_cols = studies_management_factorize_cols, hidden_cols = studies_management_hidden_cols,
          selection = "multiple")
        
        # Create a proxy for datatable
        
        r$studies_datatable_proxy <- DT::dataTableProxy("studies_datatable", deferUntilFlush = FALSE)
      }
      
      if (length(r$studies_datatable_proxy) > 0) DT::replaceData(r$studies_datatable_proxy, data, resetPaging = FALSE, rownames = FALSE)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer r$reload_studies_datatable"))
    })
    
    # Reload datatable
    observeEvent(r$studies_temp, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$studies_temp"))

      # Reload datatable_temp variable
      if (nrow(r$studies_temp) == 0) r$studies_datatable_temp <- tibble::tibble(id = integer(), name = character(), dataset_id = factor(),
        patient_lvl_tab_group_id = integer(), aggregated_tab_group_id = integer(), creator_id = factor(),
        creation_datetime = character(), update_datetime = character(), deleted = integer(), modified = logical(), action = character())
      
      if (nrow(r$studies_temp) > 0) r$studies_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
        table = "studies", factorize_cols = studies_management_factorize_cols, action_buttons = action_buttons, data_input = r$studies_temp)

      # Reload data of datatable
      if (length(r$studies_datatable_proxy) > 0) DT::replaceData(r$studies_datatable_proxy, 
        r$studies_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Updates on datatable data
    observeEvent(input$studies_datatable_cell_edit, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$studies_datatable_cell_edit"))
      
      edit_info <- input$studies_datatable_cell_edit
      r$studies_temp <- DT::editData(r$studies_temp, edit_info, rownames = FALSE)
      
      # Store that this row has been modified
      r$studies_temp[[edit_info$row, "modified"]] <- TRUE
    })
    
    # Save updates
    observeEvent(input$save_studies_management, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$save_studies_management"))
      
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
      
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$deleted_pressed"))
      
      r$delete_study <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
      r[[study_delete_variable]] <- TRUE
      
      # Reload datatable (to unselect rows)
      DT::replaceData(r$studies_datatable_proxy, r$studies_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Delete multiple rows (with "Delete selection" button)
    
    observeEvent(input$delete_selection, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$delete_selection"))
      
      req(length(input$studies_datatable_rows_selected) > 0)
      
      r$delete_study <- r$studies_temp[input$studies_datatable_rows_selected, ] %>% dplyr::pull(id)
      r[[study_delete_variable]] <- TRUE
    })
    
    observeEvent(r$reload_studies, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$reload_studies"))
      
      r$studies_temp <- r$studies %>% dplyr::filter(dataset_id == r$selected_dataset) %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      # Reset selected study
      m$selected_study <- NA_integer_
    })
    
    # --- --- --- --- --
    # Study options ----
    # --- --- --- --- --
    
    observeEvent(input$options, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$options"))
      
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
      
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$options_selected_study"))
      
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
      
      # Users allowed read group
      value_group <- options %>% dplyr::filter(name == "users_allowed_read_group") %>% dplyr::pull(value)
      
      selected_items <- picker_options %>% dplyr::filter(key %in% value)
      
      shiny.fluent::updateChoiceGroup.shinyInput(session, "users_allowed_read_group",
        value = options %>% dplyr::filter(name == "users_allowed_read_group") %>% dplyr::pull(value))
      output$users_allowed_read_div <- renderUI({
        make_people_picker(
          i18n = i18n, ns = ns, id = "users_allowed_read", label = "users", options = picker_options, value = value,
          width = "100%", style = "padding-bottom:10px;")
      })
      
      # Update other fields
      
      for (field in c("version", "author", "name_fr", "name_en", "category_fr", "category_en")){
        value <- options %>% dplyr::filter(name == field) %>% dplyr::pull(value) %>% stringr::str_replace_all("''", "'")
        if (is.na(value)) value <- ""
        shiny.fluent::updateTextField.shinyInput(session, paste0("study_", field), value = value)
      }
      
      for (field in c("description_fr", "description_en")) shinyAce::updateAceEditor(session,
        paste0("study_", field), value = options %>% dplyr::filter(name == field) %>% dplyr::pull(value)) %>% stringr::str_replace_all("''", "'")
    })
    
    # Save updates
    
    observeEvent(input$study_description_fr_save, {
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$study_description_fr_save"))
      r$study_save_options <- Sys.time()
    })
    observeEvent(input$study_description_en_save, {
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$study_description_en_save"))
      r$study_save_options <- Sys.time()
    })
    observeEvent(input$options_save, {
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$options_save"))
      r$study_save_options <- Sys.time()
    })
    
    observeEvent(r$study_save_options, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer r$study_save_options"))

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
        "study_name_fr", "study_name_en", "study_category_fr", "study_category_en",
        "study_description_fr", "study_description_en")) data[[stringr::str_replace(field, "study_", "")]] <- input[[field]]
      
      save_settings_options(output = output, r = r, id = id, category = "study", code_id_input = paste0("options_", link_id),
        i18n = i18n, data = data, page_options = c("version", "author", "description_fr", "description_en",
          "name_fr", "name_en", "category_fr", "category_en", "users_allowed_read"))
      
      # Change study_name & update_datetime in studies table
      new_update_datetime <- as.character(Sys.time())
      sql <- glue::glue_sql("UPDATE studies SET name = {study_name}, update_datetime = {new_update_datetime} WHERE id = {link_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      r$studies <- r$studies %>% dplyr::mutate(
        name = dplyr::case_when(id == link_id ~ study_name, TRUE ~ name),
        update_datetime = dplyr::case_when(id == link_id ~ new_update_datetime, TRUE ~ update_datetime))
      r$studies_temp <- r$studies %>%
        dplyr::mutate_at(c("creation_datetime", "update_datetime"), format_datetime, language = "en", sec = FALSE) %>%
        dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      # req(input$options_selected_study)
      # 
      # if (length(input$options_selected_study) > 1) link_id <- input$options_selected_study$key
      # else link_id <- input$options_selected_study
      # 
      # data <- list()
      # data$users_allowed_read <- input$users_allowed_read
      # data$users_allowed_read_group <- input$users_allowed_read_group
      # data$markdown_description <- input$ace_options_description
      # save_settings_options(output = output, r = r, id = id, category = "study", code_id_input = paste0("options_", link_id),
      #   i18n = i18n, data = data, page_options = c("users_allowed_read", "markdown_description"))
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer input$options_save"))
    })
    
    # Render markdown
    
    observeEvent(input$execute_options_description, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_my_studies - observer input$execute_options_description"))
      
      options_description <- isolate(input$ace_options_description %>% stringr::str_replace_all("\r", "\n"))
      
      tryCatch({
        
        # Clear temp dir
        # unlink(paste0(r$app_folder, "/temp_files"), recursive = TRUE, force = TRUE)
        
        markdown_settings <- paste0("```{r setup, include=FALSE}\nknitr::opts_knit$set(root.dir = '", 
          r$app_folder, "/temp_files')\n",
          "knitr::opts_chunk$set(root.dir = '", r$app_folder, "/temp_files', fig.path = '", r$app_folder, "/temp_files')\n```\n")
        
        markdown_file <- paste0(markdown_settings, options_description)
        
        # Create temp dir
        dir <- paste0(r$app_folder, "/temp_files")
        file <- paste0(dir, "/", paste0(sample(c(0:9, letters[1:6]), 8, TRUE), collapse = ''), "_", as.character(Sys.time()) %>% stringr::str_replace_all(":", "_"), ".Md")
        if (!dir.exists(dir)) dir.create(dir)
        
        # Create the markdown file
        knitr::knit(text = markdown_file, output = file, quiet = TRUE)
        
        output$description_markdown_result <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(file))))
      }, error = function(e) "")
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_studies - observer input$execute_options_description"))
    })
    
    # Render markdown
    
    observeEvent(input$execute_options_description, {
      if (debug) print(paste0(Sys.time(), " - mod_studies - observer input$execute_options_description"))
      r$study_options_description_trigger <- Sys.time()
    })
    
    observeEvent(input$study_description_fr_run_all, {
      if (debug) print(paste0(Sys.time(), " - mod_studies - observer input$study_description_fr_run_all"))
      r$study_options_description_trigger <- Sys.time()
    })
    
    observeEvent(input$study_description_en_run_all, {
      if (debug) print(paste0(Sys.time(), " - mod_studies - observer input$study_description_en_run_all"))
      r$study_options_description_trigger <- Sys.time()
    })
    
    observeEvent(r$study_options_description_trigger, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_studies - observer input$execute_options_description"))
      
      options_description <- isolate(input[[paste0("study_description_", input$study_description_language)]] %>% stringr::str_replace_all("\r", "\n"))
      
      tryCatch({
        
        # Clear temp dir
        # unlink(paste0(r$app_folder, "/temp_files"), recursive = TRUE, force = TRUE)
        
        markdown_settings <- paste0("```{r setup, include=FALSE}\nknitr::opts_knit$set(root.dir = '", 
          r$app_folder, "/temp_files')\n",
          "knitr::opts_chunk$set(root.dir = '", r$app_folder, "/temp_files', fig.path = '", r$app_folder, "/temp_files')\n```\n")
        
        markdown_file <- paste0(markdown_settings, options_description)
        
        # Create temp dir
        dir <- paste0(r$app_folder, "/temp_files")
        file <- paste0(dir, "/", paste0(sample(c(0:9, letters[1:6]), 8, TRUE), collapse = ''), "_", as.character(Sys.time()) %>% stringr::str_replace_all(":", "_"), ".Md")
        if (!dir.exists(dir)) dir.create(dir)
        
        # Create the markdown file
        knitr::knit(text = markdown_file, output = file, quiet = TRUE)
        
        output$description_markdown_result <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(file))))
      }, error = function(e) "")
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_studies - observer input$execute_options_description"))
    })
    
    # --- --- --- --- ---
    # Import a study ----
    # --- --- --- --- ---
    
    # --- --- --- --- ---
    # Export a study ----
    # --- --- --- --- ---
    
  })
}
