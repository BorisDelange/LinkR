#' @noRd
mod_projects_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  
  pivot_item_js <- paste0("
    Shiny.setInputValue('", id, "-current_tab', this.id);
    Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());"
  )
  
  div(class = "main",
    
    # Load widget UI ----
    
    mod_widgets_ui(id, language, languages, i18n),
    
    # Project details ----
    
    shinyjs::hidden(
      div(
        id = ns("one_element"),
        div(
          uiOutput(ns("breadcrumb")),
          div(
            id = ns("project_pivot"),
            tags$button(id = ns("summary"), i18n$t("summary"), class = "pivot_item selected_pivot_item", onclick = pivot_item_js),
            tags$button(id = ns("dataset"), i18n$t("dataset"), class = "pivot_item", onclick = pivot_item_js),
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
                uiOutput(ns("summary_informations_ui"))
              ),
              class = "widget", style = "min-height: 50%;"
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
              class = "widget", style = "height: calc(100% - 25px); padding-top: 1px; overflow: auto;"
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
              tags$h1(i18n$t("dataset")),
              div(
                id = ns("dataset_forbidden_access"),
                shiny.fluent::MessageBar(i18n$t("unauthorized_access_area"), messageBarType = 5),
                style = "display: inline-block; margin-top: 5px;"
              ),
              shinyjs::hidden(
                div(
                  id = ns("project_dataset_div"),
                  div(shiny.fluent::Dropdown.shinyInput(ns("project_dataset"), label = i18n$t("dataset")), style = "width: 200px")
                )
              ),
              class = "widget", style = "height: 50%; width: 50%;"
            ),
            class = "projects_summary_container"
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
mod_projects_server <- function(id, r, d, m, language, i18n, debug, user_accesses){
  
  # |-------------------------------- -----
  
  if (debug) cat(paste0("\n", now(), " - mod_projects - ", id, " - start"))
  
  # Load widgets ----
  
  all_divs <- c("summary", "dataset", "data_cleaning_scripts", "share")
  mod_widgets_server(id, r, d, m, language, i18n, all_divs, debug, user_accesses)
  
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
    
    # We can load a project without loading data, and load data after
    observeEvent(shiny.router::get_page(), {
      
      # If data is not already loaded, load data
      req(shiny.router::get_page() %in% c("subsets", "project_messages", "project_console", "tasks"))
      
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer shiny.router::get_page()"))
      r$load_project_trigger <- now()
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
      
      # Notify user
      # show_message_bar(output,  "modif_saved", "success", i18n = i18n, ns = ns)
    })
    
    ## Reload dataset ----
    
    observeEvent(input$reload_dataset, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer input$reload_dataset"))
      
      req(length(input$project_dataset) > 0)
      req("projects_dataset" %in% user_accesses)
      load_dataset(r, m, d, input$project_dataset, r$main_tables, m$selected_study)
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
