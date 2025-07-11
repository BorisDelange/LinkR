#' @noRd
mod_plugins_ui <- function(id){
  
  pages_variables_list <- get("pages_variables_list", envir = parent.frame())
  for (obj_name in pages_variables_list) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  ns <- NS(id)
  
  pivot_item_js <- paste0("
    Shiny.setInputValue('", id, "-current_tab', this.id);
    Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());"
  )
  
  # Language-specific fields
  # div(shiny.fluent::TextField.shinyInput(ns("name"), label = i18n$t("name")), style = "width: 200px;"),
  
  div(
    class = "main",
    
    # Load widgets UI ----
    
    mod_widgets_ui(id),
    
    # Plugin details ----
    
    shinyjs::hidden(
      div(
        id = ns("one_element"),
        div(
          div(uiOutput(ns("breadcrumb")), class = "breadcrumb"),
          div(
            id = ns("plugin_pivot"),
            tags$button(id = ns("summary"), i18n$t("summary"), class = "pivot_item selected_pivot_item", onclick = pivot_item_js),
            tags$button(id = ns("edit_code"), i18n$t("code"), class = "pivot_item", onclick = pivot_item_js),
            tags$button(id = ns("run_code"), i18n$t("test_code"), class = "pivot_item", onclick = pivot_item_js),
            tags$button(id = ns("share"), i18n$t("share"), class = "pivot_item", onclick = pivot_item_js),
            class = "pivot"
          ),
          style = "display: flex; justify-content: space-between; z-index: 100;"
        ),
        
        ## Summary ----
        div(
          id = ns("summary_div"),
          div(
            shinyjs::hidden(
              div(
                id = ns("edit_description_div"),
                h1(i18n$t("edit_description")),
                div(
                  create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("run_description_code"), iconProps = list(iconName = "Play")), text = i18n$t("run_code")),
                  class = "small_icon_button top-right-button"
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
                  style = "width: 100%; height: calc(100% - 40px); display: flex; flex-direction: column;"
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
                  div(shiny.fluent::TextField.shinyInput(ns("version"), label = i18n$t("version")), style = "width: 200px;"),
                  div(
                    shiny.fluent::Dropdown.shinyInput(
                      ns("tab_type_id"), label = i18n$t("plugin_for"), multiSelect = TRUE,
                      options = list(
                        list(key = 1, text = i18n$t("patient_lvl_data")),
                        list(key = 2, text = i18n$t("aggregated_data"))
                      )
                    ),
                    style = "width: 400px;"
                  ),
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
            class = "plugins_summary_left"
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
                  class = "small_icon_button top-right-button"
                )
              ),
              uiOutput(ns("description_ui")),
              class = "widget", style = "height: calc(100% - 25px); padding-top: 1px; overflow: auto;"
            ),
            class = "plugins_summary_right"
          ),
          class = "plugins_summary_container"
        ),
        
        ## Edit code ----
        div(
          id = ns("edit_code_div"),
          div(
            id = ns("edit_code_forbidden_access"),
            shiny.fluent::MessageBar(i18n$t("unauthorized_access_area"), messageBarType = 5),
            style = "display: inline-block; margin: 5px;"
          ),
          shinyjs::hidden(div(id = ns("edit_code_tabs_div"), uiOutput(ns("edit_code_tabs_ui")), style = "height: 24px; position: relative;")),
          shinyjs::hidden(div(id = ns("edit_code_editors_div"), style = "height: calc(100% - 24px);")),
          style = "height: 100%;"
        ),
        
        ## Test code ----
        shinyjs::hidden(
          div(
            id = ns("run_code_div"),
            div(
              id = ns("run_code_forbidden_access"),
              shiny.fluent::MessageBar(i18n$t("unauthorized_access_area"), messageBarType = 5),
              style = "display: inline-block; margin: 5px;"
            ),
            div(
              id = ns("run_code_content_div"),
              div(textOutput(ns("run_code_datetime_code_execution")), style = "color: #878787; font-size: 12px; margin-left: 8px;"),
              div(id = ns("gridstack_plugin_run_code"), class = "grid-stack"),
              div(verbatimTextOutput(ns("run_code_result_server")), style = "font-size: 12px; margin-left: 8px; padding-top: 10px;"),
              style = "margin-top: 10px; width: 100%;"
            ),
            class = "plugins_test_container"
          )
        ),
        
        ## Share ----
        shinyjs::hidden(
          div(
            id = ns("share_div"),
            div(
              div(
                h1(i18n$t("download_plugin")),
                div(
                  id = ns("download_content_div"),
                  tags$p(
                    i18n$t("download_plugin_help_1"), tags$br(),
                    i18n$t("download_plugin_help_2"),
                    style = "color: #5e5d5d"
                  ),
                  div(
                    shiny.fluent::PrimaryButton.shinyInput(ns("export_element"), i18n$t("download")),
                    style = "position: absolute; right: 8px; bottom: 8px;"
                  ),
                  div(downloadButton(ns("export_element_download")), style = "visibility: hidden; position: absolute; right: 0; bottom: 0;")
                ),
                class = "widget", style = "min-height: 50%; padding-top: 1px;"
              ),
              class = "plugins_share_left"
            ),
            div(
              div(
                h1(i18n$t("synchronize_with_git_repo")),
                div(
                  id = ns("share_forbidden_access"),
                  shiny.fluent::MessageBar(i18n$t("unauthorized_access_area"), messageBarType = 5),
                  style = "display: inline-block; margin: 5px;"
                ),
                shinyjs::hidden(
                  div(
                    id = ns("share_content_div"),
                    tags$p(
                      i18n$t("plugin_git_synchro_help_1"),
                      style = "color: #5e5d5d"
                    ),
                    div(shiny.fluent::Dropdown.shinyInput(ns("git_repo"), label = i18n$t("git_repo")), style = "width: 200px;"),
                    div(uiOutput(ns("git_repo_element_ui")), style = "margin-top:10px;"),
                    div(
                      uiOutput(ns("synchronize_git_buttons")),
                      class = "datasets_share_buttons"
                    )
                  )
                ),
                class = "widget", style = "min-height: 50%; padding-top: 1px;"
              ),
              class = "plugins_share_right"
            ),
            class = "plugins_share_container"
          )
        ),
        style = "height: 100%; display: flex; flex-direction: column;"
      )
    ),
    
    # Create a plugin modal ----
    
    shinyjs::hidden(
      div(
        id = ns("create_element_modal"),
        div(
          div(
            tags$h1(i18n$t("create_plugin")),
            shiny.fluent::IconButton.shinyInput(ns("close_create_element_modal"), iconProps = list(iconName = "ChromeClose")),
            class = "create_element_modal_head small_close_button"
          ),
          div(
            div(
              div(shiny.fluent::TextField.shinyInput(ns("element_creation_name"), label = i18n$t("name")), style = "width: 200px;"),
              div(
                shiny.fluent::Dropdown.shinyInput(
                  ns("plugin_creation_type"), label = i18n$t("data_type"), multiSelect = TRUE,
                  options = list(
                    list(key = 1, text = i18n$t("patient_lvl_data")),
                    list(key = 2, text = i18n$t("aggregated_data"))
                  ), value = 1
                ),
                style = "width: 200px;"
              ),
              style = "display: flex; gap: 20px;"
            ),
            div(
              div(shiny.fluent::Toggle.shinyInput(ns("plugin_copy_existing_plugin"), label = i18n$t("copy_existing_plugin")), style = "width: 200px; margin-top: 20px;"),
              div(id = ns("plugin_to_copy_div"), shiny.fluent::Dropdown.shinyInput(ns("plugin_to_copy")), style = "width: 200px; margin-top: 18px;"),
              style = "display: flex; gap: 20px;"
            ),
            div(
              shiny.fluent::PrimaryButton.shinyInput(ns("add_element"), i18n$t("add")),
              class = "create_element_modal_buttons"
            ),
          ),
          class = "create_plugin_modal_content"
        ),
        class = "create_element_modal"
      )
    ),
    
    # Delete a plugin modal ----
    
    shinyjs::hidden(
      div(
        id = ns("delete_plugin_modal"),
        div(
          tags$h1(i18n$t("delete_plugin_title")), tags$p(i18n$t("delete_plugin_text")),
          div(
            shiny.fluent::DefaultButton.shinyInput(ns("close_plugin_deletion_modal"), i18n$t("dont_delete")),
            div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_plugin_deletion"), i18n$t("delete")), class = "delete_button"),
            class = "delete_modal_buttons"
          ),
          class = "delete_modal_content"
        ),
        class = "delete_modal"
      )
    ),
    
    # Delete a file modal ----
    
    shinyjs::hidden(
      div(
        id = ns("delete_file_modal"),
        div(
          tags$h1(i18n$t("delete_file_title")), tags$p(i18n$t("delete_file_text")),
          div(
            shiny.fluent::DefaultButton.shinyInput(ns("close_file_deletion_modal"), i18n$t("dont_delete")),
            div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_file_deletion"), i18n$t("delete")), class = "delete_button"),
            class = "delete_modal_buttons"
          ),
          class = "delete_modal_content"
        ),
        class = "delete_modal"
      )
    ),
    
    # Select concepts modal ----
    
    mod_select_concepts_ui(id)
  )
}

#' @noRd 
mod_plugins_server <- function(id){
  
  pages_variables_list <- get("pages_variables_list", envir = parent.frame())
  for (obj_name in pages_variables_list) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  # |-------------------------------- -----
  
  # Load widgets ----
  
  all_divs <- c("summary", "edit_code", "run_code", "share")
  mod_widgets_server(id, all_divs)
  
  # Load concepts backend ----
  
  mod_select_concepts_server(id)
  
  # |-------------------------------- -----
  
  # Plugins module ----
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    # Initiate vars ----
    
    r$edit_plugin_code_tabs <- tibble::tibble(id = integer(), plugin_id = integer(), filename = character(), position = integer())
    r$edit_plugin_code_deleted_files_id <- c()
    
    # Current user accesses ----
    
    if ("plugins_management" %in% user_accesses) sapply(c("create_element_button", "edit_summary_div", "delete_element_div"), shinyjs::show)
    if ("plugins_import" %in% user_accesses) shinyjs::show("import_element_button")
    
    if ("plugins_share" %in% user_accesses){
      sapply(c("share_content_div", "export_element_button"), shinyjs::show)
      shinyjs::hide("share_forbidden_access")
    }
    
    if ("plugins_edit_code" %in% user_accesses){
      sapply(c(
        "edit_code_tabs_div", "edit_code_editors_div", "run_code_content_div", "edit_page_on_div", "reload_plugin_code_button",
        "edit_code_buttons", "edit_code_files_browser_div"
        ), shinyjs::show)
      sapply(c("edit_code_forbidden_access", "run_code_forbidden_access"), shinyjs::hide)
    }

    # |-------------------------------- -----
    
    # --- --- --- --- - -
    # Plugin summary ----
    # --- --- --- --- - -
    
    # |-------------------------------- -----
    
    # --- --- --- --- -- -
    # Create a plugin ----
    # --- --- --- --- -- -
    
    # Show / hide plugin_to_copy dropdown
    
    observe_event(input$plugin_copy_existing_plugin, {
      
      if (input$plugin_copy_existing_plugin) shinyjs::show("plugin_to_copy_div")
      else shinyjs::hide("plugin_to_copy_div")
    })
    
    # --- --- --- --- --- -
    # Edit plugin code ----
    # --- --- --- --- --- -
    
    ## Load plugin files  ----
    
    observe_event(input$reload_code_files, {
      
      if ("plugins_edit_code" %not_in% user_accesses) return()
        
      plugin_folder <- paste0(r$app_folder, "/plugins/", input$selected_plugin_unique_id)
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-selected_plugin_folder', '", plugin_folder, "');"))
      
      file_names <- list.files(plugin_folder, full.names = FALSE)
      plugin_files <- r$edit_plugin_code_files_list %>% dplyr::filter(plugin_id == input$selected_element) %>% dplyr::pull(filename)
      file_names <- setdiff(file_names, plugin_files)
      
      if (length(file_names) > 0){
        
        if (nrow(r$edit_plugin_code_files_list) > 0) max_id <- max(r$edit_plugin_code_files_list$id)
        else max_id <- 0
        
        r$edit_plugin_code_files_list <-
          r$edit_plugin_code_files_list %>%
          dplyr::bind_rows(tibble::tibble(id = max_id + 1:length(file_names), plugin_id = input$selected_element, filename = file_names)) %>%
          dplyr::arrange(filename)
      }
      
      # Reload files browser + tabs
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-edit_code_reload_files_browser', '", now(format = "%Y-%m-%d %H:%M:%OS3"), "');"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-edit_code_reload_files_tab', '", now(format = "%Y-%m-%d %H:%M:%OS3"), "');"))
      
      # Initiate plugin tabs and files, if not already loaded
      if (nrow(r$edit_plugin_code_tabs %>% dplyr::filter(plugin_id == input$selected_element)) == 0){
        r$edit_plugin_code_tabs <-
          r$edit_plugin_code_tabs %>%
          dplyr::bind_rows(
            r$edit_plugin_code_files_list %>%
            dplyr::filter(
              id %not_in% r$edit_plugin_code_tabs$id,
              plugin_id == input$selected_element,
              filename %in% c("server.R", "ui.R", "translations.csv")
            ) %>%
            dplyr::mutate(position = dplyr::case_when(filename == "ui.R" ~ 1, filename == "server.R" ~ 2, filename == "translations.csv" ~ 3)) %>%
            dplyr::arrange(position) %>%
            dplyr::select(id, plugin_id, filename, position)
          )
        
        # Open server.R, ui.R & translations.csv tabs
        
        # Create ace editors for server.R, ui.R and translations.csv
        for (filename in c("ui.R", "server.R", "translations.csv")){
          file_code <- readLines(paste0(plugin_folder, "/", filename), warn = FALSE) %>% paste(collapse = "\n")
          file_id <- r$edit_plugin_code_files_list %>% dplyr::filter(plugin_id == input$selected_element, filename == !!filename) %>% dplyr::pull(id)
          file_ext <- sub(".*\\.", "", tolower(filename))
          ace_mode <- switch(file_ext, "r" = "r", "py" = "python", "")
          
          if (filename == "ui.R") shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-edit_code_selected_file', ", file_id, ");"))
          
          if (file_id %in% r$edit_plugin_code_editors$id & filename == "ui.R") shinyjs::show(paste0("edit_code_editor_div_", file_id))
          
          if (file_id %not_in% r$edit_plugin_code_editors$id){
            ui_div <- div(
              id = ns(paste0("edit_code_editor_div_", file_id)),
              shinyAce::aceEditor(
                ns(paste0("edit_code_editor_", file_id)), value = file_code, mode = ace_mode,
                hotkeys =  get_ace_editor_code_hotkeys(),
                theme = user_settings$ace_theme, fontSize = user_settings$ace_font_size,
                autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, showPrintMargin = FALSE
              ),
              style = "width: 100%; height: 100%; display: flex; flex-direction: column;"
            )
            
            if (filename != "ui.R") ui_div <- shinyjs::hidden(ui_div)
            
            insertUI(selector = paste0("#", ns("edit_code_editors_div")), where = "beforeEnd", ui = ui_div)
            
            r$edit_plugin_code_editors <- r$edit_plugin_code_editors %>% dplyr::bind_rows(r$edit_plugin_code_files_list %>% dplyr::filter(id == file_id))
            
            # Add observers for editor hotkeys
            shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-add_code_editor_hotkeys', ", file_id, ");"))
          }
        }
      }
      
      else {
        first_file_id <- r$edit_plugin_code_tabs %>% dplyr::filter(plugin_id == input$selected_element) %>% dplyr::arrange(position) %>% dplyr::slice(1) %>% dplyr::pull(id)
        plugin_editors_ids <- r$edit_plugin_code_editors %>% dplyr::filter(plugin_id == input$selected_element) %>% dplyr::pull(id)
        
        if (input$edit_code_selected_file %in% plugin_editors_ids) shinyjs::show(paste0("edit_code_editor_div_", input$edit_code_selected_file))
        else shinyjs::show(paste0("edit_code_editor_div_", first_file_id))
      }
    })
    
    ## Reload files browser ----
    
    observe_event(input$edit_code_reload_files_browser, {
      
      filenames_order <- c("ui.R", "server.R", "translations.csv")
      
      edit_plugin_code_files_list <- 
        r$edit_plugin_code_files_list %>%
        dplyr::filter(plugin_id == input$selected_element) %>%
        dplyr::arrange(filename) %>%
        dplyr::mutate(filename = factor(filename, levels = c(filenames_order, setdiff(unique(filename), filenames_order)))) %>%
        dplyr::mutate_at("filename", as.character) %>%
        dplyr::mutate(short_filename = ifelse(nchar(filename) >= 23, paste0(substr(filename, 1, 20), "..."), filename))
      
      files_ui <- load_files_browser_ui(input_prefix = "edit_code_", files_list = edit_plugin_code_files_list)
      
      output$edit_code_files_browser <- renderUI(files_ui)
    })
    
    ## Reload code tabs ----
    
    observe_event(input$edit_code_reload_files_tab, {
      
      if ("plugins_edit_code" %not_in% user_accesses) return()
      
      tabs_container <- reload_files_browser_tabs(
        input_prefix = "edit_code_", r_prefix = "edit_plugin_code",
        element_id = input$selected_element, file_id = input$edit_code_selected_file
      )
      
      output$edit_code_tabs_ui <- renderUI(tabs_container)
      
      shinyjs::delay(500, shinyjs::runjs(sprintf("initSortableTabs('%s')", ns("edit_code_tabs"))))
    })
    
    observe_event(input$edit_code_tab_positions, files_browser_edit_tab_positions(positions = input$edit_code_tab_positions, r_prefix = "edit_plugin_code", element_id = input$selected_element))
    
    ## Change file tab ----
    
    observe_event(input$edit_code_selected_tab, files_browser_change_tab(input_prefix = "edit_code_", r_prefix = "edit_plugin_code", file_id = input$edit_code_selected_tab))
    
    ## Open a file ----
    
    observe_event(input$edit_code_selected_file, {
      
      if ("plugins_edit_code" %not_in% user_accesses) return()
      
      files_browser_open_file(
        input_prefix = "edit_code_", r_prefix = "edit_plugin_code", folder = input$selected_plugin_folder,
        element_id = input$selected_element, file_id = input$edit_code_selected_file, user_settings = user_settings
      )
    })
    
    ## Close a file ----
    
    observe_event(input$edit_code_close_selected_tab_trigger, {
      files_browser_close_file(
        input_prefix = "edit_code_", r_prefix = "edit_plugin_code",
        element_id = input$selected_element, file_id = input$edit_code_close_selected_tab
      )
    })
    
    ## Create a file ----
    
    observe_event(input$edit_code_add_file, {
      
      if ("plugins_edit_code" %not_in% user_accesses) return()
      
      files_browser_create_file(
        input_prefix = "edit_code_", r_prefix = "edit_plugin_code", folder = input$selected_plugin_folder,
        element_id = input$selected_element, user_settings = user_settings
      )
    })
    
    ## Editor hotkeys ----
    observe_event(input$add_code_editor_hotkeys, {
      
      file_id <- input$add_code_editor_hotkeys
      editor_id <- paste0("edit_code_editor_", file_id)
      
      observe_event(input[[paste0(editor_id, "_run_all")]], shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_plugin_code_trigger', Math.random());")))
      observe_event(input[[paste0(editor_id, "_comment")]], toggle_comments(input_id = editor_id, code = input[[editor_id]], selection = input[[paste0(editor_id, "_comment")]]$range, session = session))
      observe_event(input[[paste0(editor_id, "_save")]], shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_file_code', Math.random());")))
    })
    
    ## Save updates ----
    
    observe_event(input$save_file_code, {
      
      if ("plugins_edit_code" %not_in% user_accesses) return()
      
      file <- r$edit_plugin_code_files_list %>% dplyr::filter(id == input$edit_code_selected_file)
      
      files_browser_save_file(
        input_prefix = "edit_code_", r_prefix = "edit_plugin_code",
        folder = input$selected_plugin_folder, element_id = input$selected_element, file_id = file$id,
        new_code = input[[paste0("edit_code_editor_", file$id)]]
      )
    })
    
    ## Rename a file ----
    
    # Show textfield
    observe_event(input$edit_code_edit_filename_trigger, {
      
      file_id <- input$edit_code_edit_filename
      file_name <- r$edit_plugin_code_files_list %>% dplyr::filter(id == file_id) %>% dplyr::pull(filename)
      shiny.fluent::updateTextField.shinyInput(session, paste0("edit_code_edit_filename_textfield_", file_id), value = file_name)
      
      shinyjs::delay(50, 
        sapply(c(
          paste0("edit_code_edit_filename_textfield_div_", file_id),
          paste0("edit_code_save_filename_button_div_", file_id),
          paste0("edit_code_cancel_rename_button_div_", file_id)),
          shinyjs::show
        )
      )
      sapply(c(
        paste0("edit_code_filename_div_", file_id),
        paste0("edit_code_delete_file_button_div_", file_id),
        paste0("edit_code_edit_filename_button_div_", file_id)),
        shinyjs::hide
      )
    })
    
    # Cancel rename
    
    observe_event(input$edit_code_cancel_rename_trigger, {
      
      file_id <- input$edit_code_edit_filename
      
      shinyjs::delay(50, 
        sapply(c(
          paste0("edit_code_edit_filename_textfield_div_", file_id),
          paste0("edit_code_save_filename_button_div_", file_id),
          paste0("edit_code_cancel_rename_button_div_", file_id)),
          shinyjs::hide
        )
      )
      sapply(c(
        paste0("edit_code_filename_div_", file_id),
        paste0("edit_code_delete_file_button_div_", file_id),
        paste0("edit_code_edit_filename_button_div_", file_id)),
        shinyjs::show
      )
    })
    
    # Save new name
    observe_event(input$edit_code_save_filename_trigger, {
      
      file_id <- input$edit_code_save_filename
      textfield_id <- paste0("edit_code_edit_filename_textfield_", file_id)
      new_name <- input[[textfield_id]]
      
      files_browser_edit_filename(
        input_prefix = "edit_code_", r_prefix = "edit_plugin_code", folder = input$selected_plugin_folder,
        element_id = input$selected_element, file_id = file_id, new_name = new_name
      )
    })
    
    ## Delete a file ----
    
    observe_event(input$edit_code_delete_file_trigger, shinyjs::show("delete_file_modal"))
    observe_event(input$close_file_deletion_modal, shinyjs::hide("delete_file_modal"))
    
    observe_event(input$confirm_file_deletion, {
      
      if ("plugins_edit_code" %not_in% user_accesses) return()
      
      files_browser_delete_file(
        input_prefix = "edit_code_", r_prefix = "edit_plugin_code",
        folder = input$selected_plugin_folder, element_id = input$selected_element, file_id = input$edit_code_delete_file
      )
    })
    
    ## Select concepts ----
    
    # Open / close modal
    observe_event(input$select_concepts, shinyjs::show("select_concepts_modal"))
    observe_event(input$close_select_concepts_modal, shinyjs::hide("select_concepts_modal"))
    
    ## Run plugin code ----
    
    observe_event(input$run_plugin_code, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_plugin_code_trigger', Math.random());")))
    
    observe_event(input$reload_plugin_code, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_plugin_code_trigger', Math.random());")))
    
    # Initiate widgets var
    r$run_plugin_last_widget_id <- get_last_row(r$db, "widgets") + 10^9 %>% as.integer()
    r$run_plugin_tab_id <- get_last_row(r$db, "tabs") + 10^9 %>% as.integer()
    
    # Initiate gridstack instance
    create_gridstack_instance("plugin_run_code")
    
    observe_event(input$run_plugin_code_trigger, {

      if ("plugins_edit_code" %not_in% user_accesses) return()
        
      # Switch to 'Test' tab
      shinyjs::runjs(paste0("
        Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());
        Shiny.setInputValue('", id, "-current_tab', 'run_code');"))
      
      # Notify user that if there's not a study loaded, server code might not work
      no_study_loaded <- TRUE
      if (length(m$selected_study) > 0) if (!is.na(m$selected_study)) no_study_loaded <- FALSE
      if (no_study_loaded) show_message_bar("plugin_no_selected_project", "warning")

      # Create translations file

        tryCatch({
          # Create plugin folder in translations folder if doesn't exist
          new_dir <- paste0(r$app_folder, "/translations/", input$selected_plugin_unique_id)
          if (!dir.exists(new_dir)) dir.create(new_dir)

          # Get translations file
          translations <- readLines(paste0(input$selected_plugin_folder, "/translations.csv"), warn = FALSE) %>% paste(collapse = "\n")

          # Create a csv with all languages
          data <- read.csv(text = translations, header = TRUE, stringsAsFactors = FALSE)

          # Create one csv by language
          for(lang in names(data)[-1]){
            # Create a new dataframe with base & current language cols
            data_lang <- data[, c("base", lang)]
            filename <- paste0(new_dir, "/translation_", lang, ".csv")
            write.csv(data_lang, filename, row.names = FALSE)
          }
        },
        error = function(e) cat(paste0("\n", now(), " - mod_plugins - error creating translations file - plugin_id = ", input$selected_element, " - ", toString(e))))

        tryCatch({
          i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = new_dir))
          i18np$set_translation_language(language)},
          error = function(e) cat(paste0("\n", now(), " - mod_plugins - error creating new translator - plugin_id = ", input$selected_element, " - ", toString(e))))

      # Get vocabulary concepts
      selected_concepts <- r[[paste0(id, "_selected_concepts")]]

      # Get ui & server code
      code <- list()
      code$ui <- input[[paste0("edit_code_editor_", r$edit_plugin_code_files_list %>% dplyr::filter(plugin_id == input$selected_element, filename == "ui.R") %>% dplyr::pull(id))]]
      code$server <- input[[paste0("edit_code_editor_", r$edit_plugin_code_files_list %>% dplyr::filter(plugin_id == input$selected_element, filename == "server.R") %>% dplyr::pull(id))]]

      previous_widget_id <- r$run_plugin_last_widget_id
      widget_id <- r$run_plugin_last_widget_id + 1
      r$run_plugin_last_widget_id <- widget_id
      
      # Delete rows of previous tests
      
      sql <- glue::glue_sql("DELETE FROM widgets_options WHERE widget_id >= 1000000000", .con = m$db)
      sql_send_statement(m$db, sql)
      
      # sql <- glue::glue_sql("DELETE FROM widgets_concepts WHERE widget_id >= 1000000000", .con = m$db)
      # sql_send_statement(m$db, sql)

      # Create a session number, to inactivate older observers
      # Reset all older observers

      session_code <- "plugin_test"
      if (length(m[[session_code]]) == 0) session_num <- 1L
      if (length(m[[session_code]]) > 0) session_num <- m[[session_code]] + 1
      m[[session_code]] <- session_num
      
      study_id <- NA_integer_
      patient_id <- NA_integer_
      if (length(m$selected_study) > 0) study_id <- m$selected_study
      if (length(m$selected_person) > 0) patient_id <- m$selected_person

      code$ui <- process_widget_code(code$ui, r$run_plugin_tab_id, widget_id, study_id, patient_id, input$selected_plugin_folder)
      code$server <- process_widget_code(code$server, r$run_plugin_tab_id, widget_id, study_id, patient_id, input$selected_plugin_folder)

      code$ui <- tryCatch(
        eval(parse(text = code$ui)),
        error = function(e){
          r$widget_ui_last_error <- e
          show_message_bar("error_run_plugin_ui_code", "severeWarning")
          cat(paste0("\n", now(), " - mod_plugins - error loading UI code - plugin_id = ", input$selected_element, " - ", toString(e)))
        }
      )
      
      # Do we have saved widget size and position ?
      sql <- glue::glue_sql("SELECT * FROM options WHERE category = 'plugin_run_code_options' AND name = 'plugin_widget_position'", .con = r$db)
      widget_position <- DBI::dbGetQuery(r$db, sql)
      
      if (nrow(widget_position) > 0) widget_position <- widget_position %>% dplyr::pull(value)
      else {
        widget_position <- "w=6;h=25;x=0;y=0;"
        new_data <- tibble::tibble(
          id = get_last_row(r$db, "options") + 1, category = "plugin_run_code_options", link_id = NA_integer_, name = "plugin_widget_position",
          value = widget_position, value_num = NA_real_, creator_id = NA_integer_, datetime = now(), deleted = FALSE
        )
        DBI::dbAppendTable(r$db, "options", new_data)
      }
      
      matches <- stringr::str_match(widget_position, "w=(\\d+);h=(\\d+);x=(\\d+);y=(\\d+)")
      widget_pos <- list(w = as.integer(matches[2]), h = as.integer(matches[3]), x = as.integer(matches[4]), y = as.integer(matches[5]))
      
      ui_output <- create_widget(widget_id, code$ui, w = widget_pos$w, h = widget_pos$h, x = widget_pos$x, y = widget_pos$y)
      add_widget_to_gridstack("plugin_run_code", ui_output, widget_id, previous_widget_id)
      output[[paste0("ui_", widget_id)]] <- renderUI(code$ui)
      output[[paste0("edit_buttons_", widget_id)]] <- renderUI(get_widget_edit_buttons(widget_id))
      
      # New environment, to authorize access to selected variables from shinyAce editor
      # We choose which vars to keep access to

      # Variables to hide
      new_env_vars <- list("r" = NA)

      # Variables to keep
      variables_to_keep <- c("d", "m", "session_code", "session_num", "i18n", "selected_concepts", "log_level")
      if (exists("i18np")) variables_to_keep <- c(variables_to_keep, "i18np")

      for (var in variables_to_keep) new_env_vars[[var]] <- eval(parse(text = var))
      new_env <- rlang::new_environment(data = new_env_vars, parent = pryr::where("r"))

      options('cli.num_colors' = 1)

      # Capture console output of our code
      captured_output <- capture.output(
        tryCatch(eval(parse(text = code$server), envir = new_env), error = function(e){
          r$widget_server_last_error <- e
          show_message_bar("error_run_plugin_server_code", "severeWarning")
          cat(paste0("\n", now(), " - mod_plugins - error loading server code - plugin_id = ", input$selected_element, " - ", toString(e)))
        }))

      # Restore normal value
      options('cli.num_colors' = NULL)

      output$run_code_result_server <- renderText(paste(captured_output, collapse = "\n"))

      output$run_code_datetime_code_execution <- renderText(format_datetime(now(), language))
    })
    
    ## Edit code test page ----
    
    r$plugin_run_code_edit_page_activated <- FALSE
    
    observe_event(input$edit_page_on, {
      
      if ("plugins_edit_code" %not_in% user_accesses) return()
        
      # Enable gridstack edition
      shinyjs::runjs("
        const grid = window.gridStackInstances['plugin_run_code'];
        grid.setStatic(false);
      ")
      
      # Show edit and delete widget buttons
      shinyjs::show(paste0("plugins_widget_settings_buttons_", r$run_plugin_last_widget_id))
      
      # Show quit edit page button
      shinyjs::hide("edit_page_on_div")
      shinyjs::show("edit_page_off_div")

      # Hide resize button when sidenav is displayed or not
      r$plugin_run_code_edit_page_activated <- TRUE
    })
    
    observe_event(input$edit_page_off, {
      
      widget_id <- r$run_plugin_last_widget_id
      
      # Disable gridstack edition
      shinyjs::runjs("
        const grid = window.gridStackInstances['plugin_run_code'];
        grid.setStatic(true);
      ")
      
      # Hide edit and delete widget buttons
      shinyjs::hide(paste0("plugins_widget_settings_buttons_", widget_id))
      
      # Show edit page button
      shinyjs::hide("edit_page_off_div")
      shinyjs::delay(50, shinyjs::show("edit_page_on_div"))
      
      # Hide resize button when sidenav is displayed or not
      r$plugin_run_code_edit_page_activated <- FALSE
      
      # Save widget position
      
      shinyjs::runjs(paste0(
        "var widget = document.getElementById('", id, "-plugins_gridstack_item_", widget_id, "');",
        "if (widget) {",
        "  var widgetPosition = {",
        "    id: ", widget_id, ",",
        "    w: widget.getAttribute('gs-w'),",
        "    h: widget.getAttribute('gs-h'),",
        "    x: widget.getAttribute('gs-x'),",
        "    y: widget.getAttribute('gs-y')",
        "  }",
        "};",
        "Shiny.setInputValue('", id, "-widget_position', widgetPosition);",
        "Shiny.setInputValue('", id, "-widget_position_trigger', Math.random());"
      ))
    })
    
    # Save widget position
    observe_event(input$widget_position_trigger, {
      
      widget <- input$widget_position
      
      widget_position <- ""
      for (pos in c("w", "h", "x", "y")){
        if (length(widget[[pos]]) == 0) widget_pos <- 1
        else widget_pos <- widget[[pos]]
        widget_position <- paste0(widget_position, pos, "=", widget_pos, ";")
      }
      
      sql <- glue::glue_sql("UPDATE options SET value = {widget_position} WHERE category = 'plugin_run_code_options' AND name = 'plugin_widget_position'", .con = r$db)
      sql_send_statement(r$db, sql)
    })
  })
}
