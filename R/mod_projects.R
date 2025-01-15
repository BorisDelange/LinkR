#' @noRd
mod_projects_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  
  pivot_item_js <- paste0("
    Shiny.setInputValue('", id, "-current_tab', this.id);
    Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());"
  )
  
  dataset_details <- function(tab){
    
    if (tab == "summary") base_onclick <- paste0(
      "Shiny.setInputValue('", id, "-current_tab', 'dataset');",
      "Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());"
    )
    else base_onclick <- ""
    
    div(
      div(
        tags$span(uiOutput(ns(paste0(tab, "_num_care_sites"))), class = "dataset_details_widget_num"),
        tags$span(i18n$t("care_sites"), class = "dataset_details_widget_text"),
        onclick = paste0(
          base_onclick,
          "Shiny.setInputValue('", id, "-dataset_details', 'care_sites');",
          "Shiny.setInputValue('", id, "-dataset_details_trigger', Math.random());"
        ),
        class = "dataset_details_widget",
      ),
      div(
        tags$span(uiOutput(ns(paste0(tab, "_num_patients"))), class = "dataset_details_widget_num"),
        tags$span(i18n$t("patients"), class = "dataset_details_widget_text"),
        onclick = paste0(
          base_onclick,
          "Shiny.setInputValue('", id, "-dataset_details', 'patients');",
          "Shiny.setInputValue('", id, "-dataset_details_trigger', Math.random());"
        ),
        class = "dataset_details_widget",
      ),
      div(
        tags$span(uiOutput(ns(paste0(tab, "_num_stays"))), class = "dataset_details_widget_num"),
        tags$span(i18n$t("stays"), class = "dataset_details_widget_text"),
        onclick = paste0(
          base_onclick,
          "Shiny.setInputValue('", id, "-dataset_details', 'stays');",
          "Shiny.setInputValue('", id, "-dataset_details_trigger', Math.random());"
        ),
        class = "dataset_details_widget",
      ),
      style = "display: flex; gap: 10px; align-items: center; height: calc(100% - 45px);"
    )
  }
  
  div(class = "main",
    
    # Load widget UI ----
    
    mod_widgets_ui(id, language, languages, i18n),
    
    # Project details ----
    
    shinyjs::hidden(
      div(
        id = ns("one_element"),
        div(
          div(uiOutput(ns("breadcrumb")), style = "flex: 1;"),
          div(
            id = ns("project_pivot"),
            tags$button(id = ns("summary"), i18n$t("summary"), class = "pivot_item selected_pivot_item", onclick = pivot_item_js),
            tags$button(id = ns("dataset"), i18n$t("data"), class = "pivot_item", onclick = pivot_item_js),
            tags$button(id = ns("data_cleaning_scripts"), i18n$t("data_cleaning"), class = "pivot_item", onclick = pivot_item_js),
            tags$button(id = ns("share"), i18n$t("share"), class = "pivot_item", onclick = pivot_item_js),
            class = "pivot"
          ),
          style = "display:flex; justify-content:space-between;"
        ),
        
        ## Summary ----
        div(
          id = ns("summary_div"),
          div(
            shinyjs::hidden(
              div(
                id = ns("edit_description_div"),
                div(
                  h1(i18n$t("edit_description")),
                  div(
                    create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("run_description_code"), iconProps = list(iconName = "Play")), text = i18n$t("run_code")),
                    style = "margin-top: 5px;"
                  ),
                  class = "small_icon_button",
                  style = "display: flex; justify-content: space-between;"
                ),
                div(
                  shinyAce::aceEditor(
                    ns("description_code"), mode = "markdown",
                    hotkeys = list(
                      save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
                      run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER")
                    ),
                    autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, fontSize = 11, showPrintMargin = FALSE
                  ),
                  style = "width: 100%; height: calc(100% - 45px); display: flex; flex-direction: column;"
                ),
                class = "widget", style = "height: 100%;"
              )
            ),
            div(
              id = ns("summary_informations_div"),
              shinyjs::hidden(
                div(
                  id = ns("summary_edit_informations_div"),
                  div(
                    h1(i18n$t("edit_informations")),
                    div(
                      shiny.fluent::Dropdown.shinyInput(
                        ns("language"), i18n$t("language"),
                        options = convert_tibble_to_list(languages, key_col = "code", text_col = "language"), value = language
                      ),
                      style = "width: 100px; margin-top: 8px; height: 30px;"
                    ),
                    style = "display: flex; justify-content: space-between;"
                  ),
                  lapply(1:nrow(languages), function(i) {
                    row <- languages[i, ]
                    result <- div(
                      id = ns(paste0("name_", row$code, "_div")),
                      shiny.fluent::TextField.shinyInput(ns(paste0("name_", row$code)), label = i18n$t("name")),
                      style = "width: 200px;"
                    )
                    if (row$code != language) result <- shinyjs::hidden(result)
                    result
                  }),
                  div(shiny.fluent::TextField.shinyInput(ns("author"), label = i18n$t("authors")), style = "width: 200px;"),
                  lapply(1:nrow(languages), function(i) {
                    row <- languages[i, ]
                    result <- div(
                      id = ns(paste0("short_description_", row$code, "_div")),
                      shiny.fluent::TextField.shinyInput(ns(paste0("short_description_", row$code)), label = i18n$t("short_description")),
                      style = "width: 400px;"
                    )
                    if (row$code != language) result <- shinyjs::hidden(result)
                    result
                  }),
                  div(
                    shiny.fluent::Dropdown.shinyInput(
                      ns("users_allowed_read_group"), label = i18n$t("give_access_to"),
                      options = list(
                        list(key = "everybody", text = i18n$t("everybody")),
                        list(key = "people_picker", text = i18n$t("some_users"))
                      )
                    ),
                    style = "width: 200px;"
                  ),
                  shinyjs::hidden(uiOutput(ns("users_allowed_read_ui"))),
                )
              ),
              div(
                id = ns("summary_view_informations_div"),
                h1(i18n$t("informations")),
                uiOutput(ns("summary_informations_ui")),
                div(
                  div(
                    div(shiny.fluent::Icon(iconName = "Contact"), style = "font-size: 4vh; height: 100%; display: flex; align-items: center; justify-content: center;"),
                    tags$h1(i18n$t("patient_lvl_data")),
                    tags$p(i18n$t("patient_lvl_data_explanation"), style = "color: grey;"),
                    class = "data_page_widget",
                    onclick = paste0("window.location.href='", shiny.router::route_link("data?type=patient_lvl"), "';")
                  ),
                  div(
                    div(shiny.fluent::Icon(iconName = "People"), style = "font-size: 4vh; height: 100%; display: flex; align-items: center; justify-content: center;"),
                    tags$h1(i18n$t("aggregated_data")),
                    tags$p(i18n$t("aggregated_data_explanation"), style = "color: grey;"),
                    class = "data_page_widget",
                    onclick = paste0("window.location.href='", shiny.router::route_link("data?type=aggregated"), "';")
                  ),
                  class = "projects_summary_data_pages_widgets",
                  style = "padding-top: 30px;"
                ),
                style = "height: calc(100% - 10px);"
              ),
              class = "widget", style = "height: 50%;"
            ),
            div(
              h1(i18n$t("data")),
              dataset_details("summary"),
              class = "widget", style = "height: 50%;"
            ),
            class = "projects_summary_left"
          ),
          div(
            div(
              div(
                h1(i18n$t("description")),
                div(
                  shinyjs::hidden(
                    div(
                      id = ns("edit_description_button"),
                      create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("edit_description"), iconProps = list(iconName = "Edit")), text = i18n$t("edit_description"))
                    )
                  ),
                  shinyjs::hidden(
                    div(
                      id = ns ("save_and_cancel_description_buttons"),
                      div(
                        id = ns("cancel_description_button"),
                        create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("cancel_description"), iconProps = list(iconName = "Cancel")), text = i18n$t("cancel_description_updates"))
                      ),
                      div(
                        id = ns("save_description_button"),
                        create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("save_description"), iconProps = list(iconName = "Accept")), text = i18n$t("save_description")),
                      ),
                      style = "display: flex;"
                    )
                  ),
                  style = "margin-top: 5px;"
                ),
                class = "small_icon_button",
                style = "display: flex; justify-content: space-between;"
              ),
              uiOutput(ns("description_ui")),
              class = "widget", style = "height: calc(100% - 10px); padding-top: 1px; overflow: auto;"
            ),
            class = "projects_summary_right"
          ),
          class = "projects_summary_container"
        ),
        
        ## Dataset ----
        shinyjs::hidden(
          div(
            id = ns("dataset_div"),
            div(
              div(
                tags$h1(i18n$t("dataset")),
                div(
                  id = ns("dataset_forbidden_access"),
                  shiny.fluent::MessageBar(i18n$t("unauthorized_access_area"), messageBarType = 5),
                  style = "display: inline-block; margin-top: 5px;"
                ),
                shinyjs::hidden(
                  div(
                    id = ns("project_dataset_div"),
                    div(
                      div(shiny.fluent::Dropdown.shinyInput(ns("project_dataset"), label = i18n$t("dataset")), style = "width: 200px"),
                      div(
                        shiny.fluent::IconButton.shinyInput(ns("reload_dataset"), iconProps = list(iconName = "Play")),
                        class = "small_icon_button",
                        style = "margin-top: 26px;"
                      ),
                      style = "display: flex; gap: 5px;"
                    ),
                    dataset_details("dataset"),
                    style = "height: calc(100% - 45px);"
                  )
                ),
                class = "widget", style = "height: 50%;"
              ),
              div(
                tags$h1(i18n$t("tables")),
                div(tableOutput(ns("dataset_tables")), class = "dataset_details_table", style = "margin-bottom: 20px;"),
                class = "widget", style = "height: 50%;"
              ),
              class = "projects_dataset_left"
            ),
            div(
              div(
                tags$h1(i18n$t("details")),
                shinyjs::hidden(
                  div(
                    id = ns("dataset_care_sites_details"),
                    div(tableOutput(ns("dataset_care_sites_locations_table")), class = "dataset_details_table", style = "margin-bottom: 20px;")
                  )
                ),
                shinyjs::hidden(
                  div(
                    id = ns("dataset_patients_details"),
                    div(plotOutput(ns("dataset_patients_age_plot"), width = "80%", height = "300px"), class = "dataset_details_plot"),
                    div(tableOutput(ns("dataset_patients_age_table")), class = "dataset_details_table", style = "margin-top: 50px;"),
                    div(plotOutput(ns("dataset_patients_gender_plot"), width = "80%", height = "300px"), class = "dataset_details_plot", style = "margin-top: 50px;"),
                    div(tableOutput(ns("dataset_patients_age_gender_table")), class = "dataset_details_table", style = "margin: 50px 0 20px 0;")
                  )
                ),
                shinyjs::hidden(
                  div(
                    id = ns("dataset_stays_details"),
                    div(plotOutput(ns("dataset_admissions_plot"), width = "80%", height = "300px"), class = "dataset_details_plot"),
                    div(tableOutput(ns("dataset_care_sites_table")), class = "dataset_details_table", style = "margin: 50px 0 20px 0;")
                  )
                ),
                class = "widget", style = "height: calc(100% - 25px); overflow: auto;"
              ),
              class = "projects_dataset_right"
            ),
            class = "projects_dataset_container"
          )
        ),
        
        ## Data cleaning scripts ----
        shinyjs::hidden(
          div(
            id = ns("data_cleaning_scripts_div"),
            div(
              tags$h1(i18n$t("data_cleaning_scripts")),
              div(
                id = ns("data_cleaning_forbidden_access"),
                shiny.fluent::MessageBar(i18n$t("unauthorized_access_area"), messageBarType = 5),
                style = "display: inline-block; margin-top: 5px;"
              ),
              shinyjs::hidden(
                div(
                  id = ns("project_data_cleaning_div"),
                  div()
                )
              ),
              class = "widget", style = "height: 50%; width: 50%;"
            ),
            class = "projects_summary_container"
          )
        ),
        
        ## Share ----
        shinyjs::hidden(
          div(
            id = ns("share_div"),
            div(
              div(
                h1(i18n$t("synchronize_with_git_repo")),
                div(
                  id = ns("share_forbidden_access"),
                  shiny.fluent::MessageBar(i18n$t("unauthorized_access_area"), messageBarType = 5),
                  style = "display: inline-block; margin-top: 5px;"
                ),
                shinyjs::hidden(
                  div(
                    id = ns("share_content_div"),
                    div(shiny.fluent::Dropdown.shinyInput(ns("git_repo"), label = i18n$t("git_repo")), style = "width: 200px;"),
                    div(uiOutput(ns("git_repo_element_ui")), style = "margin-top:10px;"),
                    div(
                      uiOutput(ns("synchronize_git_buttons")),
                      class = "projects_share_buttons"
                    ),
                    # Button to download a project (sidenav button)
                    div(downloadButton(ns("export_element_download")), style = "visibility: hidden; position: absolute; right: 0; bottom: 0;")
                  )
                ),
                class = "widget", style = "height: 50%; padding-top: 1px;"
              ),
              class = "projects_share_left",
            ),
            class = "projects_share_container"
          )
        ),
        
        style = "height: 100%; display: flex; flex-direction: column;"
      )
    ),
    
    # Create a project modal ----
    
    shinyjs::hidden(
      div(
        id = ns("create_element_modal"),
        div(
          div(
            tags$h1(i18n$t("create_project")),
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
          class = "create_project_modal_content"
        ),
        class = "create_element_modal"
      )
    ),
    
    # Import project plugins modal ----
    
    shinyjs::hidden(
      div(
        id = ns("update_project_plugins_modal"),
        div(
          tags$h1(i18n$t("update_project_plugins_title")), tags$p(i18n$t("update_project_plugins_text")),
          div(
            shiny.fluent::DefaultButton.shinyInput(ns("close_project_plugins_import_modal"), i18n$t("dont_update")),
            div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_project_plugins_import"), i18n$t("update"))),
            class = "import_modal_buttons"
          ),
          class = "import_modal_content"
        ),
        class = "import_modal"
      )
    )
  )
}

#' @noRd 
mod_projects_server <- function(id, r, d, m, language, i18n, debug, user_accesses, user_settings){
  
  # |-------------------------------- -----
  
  if (debug) cat(paste0("\n", now(), " - mod_projects - ", id, " - start"))
  
  # Load widgets ----
  
  all_divs <- c("summary", "dataset", "data_cleaning_scripts", "share")
  mod_widgets_server(id, r, d, m, language, i18n, all_divs, debug, user_accesses, user_settings)
  
  # Projects module ----
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    # Current user accesses ----
    
    if ("projects_management" %in% user_accesses) sapply(c("create_element_button", "edit_summary_div", "delete_element_div"), shinyjs::show)
    if ("projects_import" %in% user_accesses) shinyjs::show("import_element_button")
    
    if ("projects_share" %in% user_accesses){
      sapply(c("share_content_div", "export_element_button"), shinyjs::show)
      shinyjs::hide("share_forbidden_access")
    }
    
    for (type in c("dataset", "data_cleaning")){
      if (paste0("projects_", type) %in% user_accesses){
        shinyjs::show(paste0("project_", type, "_div"))
        shinyjs::hide(paste0(type, "_forbidden_access"))
      }
    }
    
    # --- --- --- --- ---
    # Load a project ----
    # --- --- --- --- ---
    
    # Load a specific project if noticed in loading_options
    
    observeEvent(r$projects_wide, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer r$projects_wide"))
      
      if (length(r$loading_options$project_id) > 0){
        
        project_id <- r$loading_options$project_id
        
        if (project_id %in% r$projects_wide$id){
          shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-selected_element', ", project_id, ");"))
          shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-selected_element_trigger', Math.random());"))
          
          if (length(r$loading_options$load_data_page) > 0){
            
            data_page <- r$loading_options$load_data_page
            
            if (data_page %in% c("patient_lvl", "aggregated")) shiny.router::change_page(paste0("data?type=", data_page))
            else cat(paste0("\n", now(), " - server - ", data_page, " is not a valid data_page"))
          }
        }
        else cat(paste0("\n", now(), " - mod_projects - ", project_id, " is not a valid project ID"))
        
        r$loading_options$project_id <- NULL
      }
    })
    
    # We can load a project without loading data, and load data after
    observeEvent(shiny.router::get_page(), {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer shiny.router::get_page()"))
      
      # If data is not already loaded, load data
      req(shiny.router::get_page() %in% c("subsets", "project_messages", "project_console", "tasks"))
      
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer shiny.router::get_page()"))
      r$load_project_trigger <- now()
    })
    
    # Reload data rows UI
    observeEvent(d$care_site, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer d$person"))
      
      num_care_sites <- 0
      
      if (d$location %>% dplyr::count() %>% dplyr::pull() > 0 & d$care_site %>% dplyr::count() %>% dplyr::pull() > 0){
        num_care_sites <-
          d$location %>%
          dplyr::inner_join(d$care_site, by = "location_id") %>%
          dplyr::distinct(location_id) %>%
          dplyr::count() %>%
          dplyr::pull()
      }
      
      output$summary_num_care_sites <- renderUI(num_care_sites)
      output$dataset_num_care_sites <- renderUI(num_care_sites)
    })
    
    observeEvent(d$person, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer d$person"))
      
      num_patients <- d$person %>% dplyr::count() %>% dplyr::pull()
      output$summary_num_patients <- renderUI(num_patients)
      output$dataset_num_patients <- renderUI(num_patients)
      
      # Update tables rows count
      
      req(m$omop_version)

      omop_tables <- c(
        "condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement", "observation", "note", "note_nlp", "payer_plan_period", "cost",
        "specimen", "drug_era", "dose_era", "condition_era", "observation_period", "visit_occurrence", "visit_detail",
        "person", "location", "care_site", "provider"
      )

      if (m$omop_version %in% c("5.3", "5.4")) omop_tables <- c(omop_tables, "death")

      tables_count <- tibble::tibble(table = character(), num_pat = integer(), num_rows = integer())

      for (table in omop_tables){
        
        num_rows <- d[[table]] %>% dplyr::count() %>% dplyr::pull()
        num_pat <- 0
        
        if (num_rows > 0) if ("person_id" %in% colnames(d[[table]])) num_pat <- d[[table]] %>% dplyr::distinct(person_id) %>% dplyr::count() %>% dplyr::pull()

        tables_count <- tables_count %>% dplyr::bind_rows(
          tibble::tibble(table = table, num_pat = as.integer(num_pat), num_rows = as.integer(num_rows))
        )

        output$dataset_tables <- renderTable(
          tables_count %>% 
            dplyr::arrange(dplyr::desc(num_rows)) %>%
            dplyr::rename(
              !!i18n$t("table") := table,
              !!i18n$t("num_patients") := num_pat,
              !!i18n$t("num_rows") := num_rows
            )
        )
      }
    })
    
    observeEvent(d$visit_occurrence, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer d$visit_occurrence"))
      
      num_stays <- d$visit_occurrence %>% dplyr::count() %>% dplyr::pull()
      output$summary_num_stays <- renderUI(num_stays)
      output$dataset_num_stays <- renderUI(num_stays)
    })
    
    # --- --- --- --- -- -
    # Project dataset ----
    # --- --- --- --- -- -
    
    ## Save updates ----
    
    observeEvent(input$project_dataset, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer input$save_dataset"))
      req(length(input$project_dataset) > 0)
      
      # Save each time a dataset is selected
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_dataset', Math.random());"))
    })
    
    observeEvent(input$save_dataset, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer input$save_dataset"))
      
      req("projects_dataset" %in% user_accesses)
      
      sql <- glue::glue_sql("UPDATE studies SET dataset_id = {input$project_dataset} WHERE id = {input$selected_element}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      r$projects_wide <- 
        r$projects_wide %>% 
        dplyr::mutate(dataset_id = dplyr::case_when(id == input$selected_element ~ input$project_dataset, TRUE ~ dataset_id))
    })
    
    ## Reload dataset ----
    
    observeEvent(input$reload_dataset, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer input$reload_dataset"))
      
      req(length(input$project_dataset) > 0)
      req("projects_dataset" %in% user_accesses)
      
      # Hide dataset details
      sapply(c("dataset_care_sites_details", "dataset_patients_details", "dataset_stays_details"), shinyjs::hide)
      
      # Load dataset
      load_dataset(r, m, d, input$project_dataset, r$main_tables, m$selected_study)
    })
    
    ## Dataset details ----
    
    observeEvent(input$dataset_details_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer input$dataset_details_trigger"))
      
      categories <- c("care_sites", "patients", "stays")
      sapply(categories[categories != input$dataset_details], function(category) shinyjs::hide(paste0("dataset_", category, "_details")))
      
      ### Care sites details ----
      
      if (input$dataset_details == "care_sites"){
        
        num_care_sites <- 0
        
        if (d$location %>% dplyr::count() %>% dplyr::pull() > 0 & d$care_site %>% dplyr::count() %>% dplyr::pull() > 0){
          care_sites <-
            d$location %>%
            dplyr::inner_join(d$care_site, by = "location_id")
          
          num_care_sites <- care_sites %>% dplyr::distinct(location_id) %>% dplyr::count() %>% dplyr::pull()
        }
        
        if (num_care_sites == 0){
          
        }
        
        req(num_care_sites > 0)
        
        output$dataset_care_sites_locations_table <- renderTable(
          care_sites %>%
            dplyr::distinct(location_source_value) %>%
            dplyr::rename(!!i18n$t("care_site") := location_source_value)
        )
      }
      
      ## Patients details ----
      
      else if (input$dataset_details == "patients"){
        
        num_rows <- d$person %>% dplyr::count() %>% dplyr::pull()
        
        if (num_rows == 0){
          
        }
        
        req(num_rows > 0)
        
        age_data <-
          d$visit_occurrence %>%
          dplyr::left_join(
            d$person %>% dplyr::select(person_id, birth_datetime),
            by = "person_id"
          ) %>%
          dplyr::collect() %>%
          dplyr::group_by(person_id) %>%
          dplyr::summarise(
            first_admission = min(visit_start_datetime, na.rm = TRUE),
            birth_datetime = dplyr::first(birth_datetime),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            age = round(as.numeric(difftime(first_admission, birth_datetime, units = "days")) / 365.25, 1)
          )
        
        gender_data <-
          d$person %>%
          dplyr::collect() %>%
          dplyr::select(person_id, gender_concept_id) %>%
          dplyr::filter(gender_concept_id %in% c(8507, 8532)) %>%
          dplyr::mutate(gender = dplyr::case_when(
            gender_concept_id == 8507 ~ i18n$t("male"),
            gender_concept_id == 8532 ~ i18n$t("female")
          ))
        
        age_gender_data <- age_data %>% dplyr::left_join(gender_data, by = "person_id")
        
        output$dataset_patients_age_plot <- renderPlot(
          age_data %>%
            ggplot2::ggplot(ggplot2::aes(x = age)) +
            ggplot2::geom_histogram(bins = 40, color = "white", fill = "#2874A6") +
            ggplot2::theme_minimal() +
            ggplot2::labs(title = i18n$t("age_at_first_hospit"), x = i18n$t("age"), y = i18n$t("occurrences")) +
            ggplot2::scale_x_continuous(breaks = seq(0, max(age_data$age, na.rm = TRUE), by = 10), limits = c(0, max(age_data$age, na.rm = TRUE))) +
            ggplot2::theme(
              plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
            )
        )
        
        output$dataset_patients_age_table <- renderTable({
          age_data %>%
          dplyr::mutate(age_group = cut(
            age,
            breaks = c(0, 2, 10, 18, 25, seq(35, 75, by = 10), Inf),
            labels = c(
              paste0("0 - 2 ", tolower(i18n$t("years"))),
              paste0("2 - 10 ", tolower(i18n$t("years"))),
              paste0("10 - 18 ", tolower(i18n$t("years"))),
              paste0("18 - 25 ", tolower(i18n$t("years"))),
              paste0("25 - 35 ", tolower(i18n$t("years"))),
              paste0("35 - 45 ", tolower(i18n$t("years"))),
              paste0("45 - 55 ", tolower(i18n$t("years"))),
              paste0("55 - 65 ", tolower(i18n$t("years"))),
              paste0("65 - 75 ", tolower(i18n$t("years"))),
              paste0("75 ", tolower(i18n$t("years_and_over")))
            ),
            right = FALSE
          )) %>%
          dplyr::group_by(age_group) %>%
          dplyr::summarise(
            !!i18n$t("num_patients") := dplyr::n(),
            .groups = "drop"
          ) %>%
            dplyr::rename(!!i18n$t("age_class") := age_group)
        }, sanitize.text.function = identity, rownames = FALSE)
        
        output$dataset_patients_gender_plot <- renderPlot(
          gender_data %>%
          dplyr::count(gender) %>%
          dplyr::mutate(percentage = n / sum(n) * 100) %>%
          dplyr::mutate(label = paste0(gender, "\n", round(percentage, 1), "%")) %>%
          ggplot2::ggplot(ggplot2::aes(x = "", y = percentage, fill = gender)) +
          ggplot2::geom_bar(width = 1, stat = "identity", color = "white") +
          ggplot2::coord_polar("y", start = 0) +
          ggplot2::theme_void() +
          ggplot2::scale_fill_manual(values = setNames(c("#1f77b4", "#6baed6"), c(i18n$t("female"), i18n$t("male")))) +
          ggplot2::labs(fill = i18n$t("gender"), title = i18n$t("gender_distribution")) +
          ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"), legend.position = "none") +
          ggplot2::geom_text(
            ggplot2::aes(label = label),
            position = ggplot2::position_stack(vjust = 0.5),
            color = "white",
            size = 5,
            fontface = "bold"
          )
        )
        
        calculate_stats <- function(data) {
          data %>%
            dplyr::summarise(
              !!i18n$t("number") := dplyr::n(),
              !!i18n$t("min") := min(age, na.rm = TRUE),
              !!i18n$t("iq1") := quantile(age, 0.25, na.rm = TRUE),
              !!i18n$t("mean") := mean(age, na.rm = TRUE),
              !!i18n$t("std") := sd(age, na.rm = TRUE),
              !!i18n$t("median") := median(age, na.rm = TRUE),
              !!i18n$t("iq3") := quantile(age, 0.75, na.rm = TRUE),
              !!i18n$t("max") := max(age, na.rm = TRUE)
            )
        }
        
        output$dataset_patients_age_gender_table <- renderTable({
          dplyr::bind_rows(
            !!i18n$t("all_patients") := calculate_stats(age_gender_data),
            !!i18n$t("male") := calculate_stats(age_gender_data %>% dplyr::filter(gender == i18n$t("male"))),
            !!i18n$t("female") := calculate_stats(age_gender_data %>% dplyr::filter(gender == i18n$t("female"))),
            .id = "Groupe"
          )
        }, sanitize.text.function = identity, rownames = FALSE)
        
      }
      
      ## Stays details ----
      
      else if (input$dataset_details == "stays"){
        
        admissions_data <- d$visit_occurrence %>%
          dplyr::select(visit_start_date) %>%
          dplyr::collect() %>%
          dplyr::mutate(visit_start_date = as.Date(visit_start_date)) %>%
          dplyr::mutate(month = lubridate::floor_date(visit_start_date, "month")) %>%
          dplyr::group_by(month) %>%
          dplyr::summarize(admissions_count = dplyr::n()) %>%
          dplyr::ungroup()
        
        filtered_admissions_data <- admissions_data %>%
          dplyr::arrange(desc(month)) %>%
          dplyr::slice(1:30) %>%
          dplyr::arrange(month)
        
        output$dataset_admissions_plot <- renderPlot(
          ggplot2::ggplot(filtered_admissions_data, ggplot2::aes(x = month, y = admissions_count)) +
            ggplot2::geom_col(fill = "#2874A6", alpha = 0.7) +
            ggplot2::scale_x_date(breaks = scales::breaks_pretty(n = 10), date_labels = "%b %Y") +
            ggplot2::theme_minimal() +
            ggplot2::labs(
              x = "Date",
              y = "Number of admissions",
              title = "Number of admissions over time"
            ) +
            ggplot2::theme(
              plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
            )
        )
        
        care_site_visits <-
          d$visit_detail %>%
          dplyr::left_join(d$care_site, by = "care_site_id") %>%
          dplyr::group_by(care_site_name) %>%
          dplyr::summarise(num = as.integer(dplyr::n())) %>%
          dplyr::ungroup() %>%
          dplyr::collect() %>%
          dplyr::arrange(desc(num)) %>%
          dplyr::slice_head(n = 20)
        
        colnames(care_site_visits) <- c(i18n$t("hospital_unit_name"), i18n$t("admissions_number"))
        
        output$dataset_care_sites_table <- renderTable(care_site_visits)
      }
      
      shinyjs::show(paste0("dataset_", input$dataset_details, "_details"))
    })
    
    # --- --- --- --- --- -
    # Import a project ----
    # --- --- --- --- --- -
    
    # Do plugins need to be updated?
    observeEvent(input$ask_plugins_update, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer input$ask_plugins_update"))
      
      shinyjs::show("update_project_plugins_modal")
    })
    
    observeEvent(input$confirm_project_plugins_import, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer input$confirm_project_plugins_import"))
      
      shinyjs::hide("update_project_plugins_modal")
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-import_project_plugins', true);"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-confirm_element_import_2', Math.random());"))
    })
    
    observeEvent(input$close_project_plugins_import_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer input$close_project_plugins_import_modal"))
      
      shinyjs::hide("update_project_plugins_modal")
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-import_project_plugins', false);"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-confirm_element_import_2', Math.random());"))
    })
  })
}
