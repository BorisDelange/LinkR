#' @noRd
mod_plugins_ui <- function(id, language, languages, i18n){
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
    
    mod_widgets_ui(id, language, languages, i18n),
    
    # Plugin details ----
    
    shinyjs::hidden(
      div(
        id = ns("one_element"),
        div(
          div(uiOutput(ns("breadcrumb")), style = "flex: 1;"),
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
                  div(
                    shiny.fluent::Dropdown.shinyInput(
                      ns("tab_type_id"), label = i18n$t("plugin_for"), multiSelect = TRUE,
                      options = list(
                        list(key = 1, text = i18n$t("patient_lvl_data")),
                        list(key = 2, text = i18n$t("aggregated_data"))
                      )
                    ),
                    style = "width: 300px;"
                  ),
                  div(shiny.fluent::TextField.shinyInput(ns("author"), label = i18n$t("authors")), style = "width: 400px;"),
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
                  style = "margin-top: 5px;"
                ),
                class = "small_icon_button",
                style = "display: flex; justify-content: space-between;"
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
            style = "display: inline-block; margin-top: 15px;"
          ),
          shinyjs::hidden(div(id = ns("edit_code_tabs_div"), uiOutput(ns("edit_code_tabs_ui")))),
          shinyjs::hidden(div(id = ns("edit_code_editors_div"), style = "height: calc(100% - 45px);")),
          style = "height: 100%;"
        ),
        
        ## Test code ----
        shinyjs::hidden(
          div(
            id = ns("run_code_div"),
            div(
              id = ns("run_code_forbidden_access"),
              shiny.fluent::MessageBar(i18n$t("unauthorized_access_area"), messageBarType = 5),
              style = "display: inline-block; margin-top: 15px;"
            ),
            div(
              id = ns("run_code_content_div"),
              div(textOutput(ns("run_code_datetime_code_execution")), style = "color:#878787; font-size:12px; margin-left:8px;"),
              div(id = ns("gridstack_plugin_run_code"), class = "grid-stack"),
              div(verbatimTextOutput(ns("run_code_result_server")), style = "font-size:12px; margin-left:8px; padding-top:10px;")
            )
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
                      class = "datasets_share_buttons"
                    ),
                    # Button to download a plugin (sidenav button)
                    div(downloadButton(ns("export_element_download")), style = "visibility: hidden; position: absolute; right: 0; bottom: 0;")
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
    
    mod_select_concepts_ui(id, language, languages, i18n)
  )
}

#' @noRd 
mod_plugins_server <- function(id, r, d, m, language, i18n, debug, user_accesses, user_settings){
  
  # |-------------------------------- -----
  
  if (debug) cat(paste0("\n", now(), " - mod_plugins - start"))
  
  # Load widgets ----
  
  all_divs <- c("summary", "edit_code", "run_code", "share")
  mod_widgets_server(id, r, d, m, language, i18n, all_divs, debug, user_accesses, user_settings)
  
  # Load concepts backend ----
  
  mod_select_concepts_server(id, r, d, m, language, i18n, debug)
  
  # |-------------------------------- -----
  
  # Plugins module ----
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    # Initiate vars
    r$edit_plugin_code_tabs <- tibble::tibble(id = integer(), plugin_id = integer(), filename = character(), position = integer())
    
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
        "edit_code_buttons", "edit_code_directory_browser_div", "edit_code_files_browser_div"
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
    
    observeEvent(input$plugin_copy_existing_plugin, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$plugin_copy_existing_plugin"))
      
      if (input$plugin_copy_existing_plugin) shinyjs::show("plugin_to_copy_div")
      else shinyjs::hide("plugin_to_copy_div")
    })
    
    # --- --- --- --- --- -
    # Edit plugin code ----
    # --- --- --- --- --- -
    
    code_hotkeys <- list(
      save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
      run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
      run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
      comment = list(win = "CTRL-SHIFT-C", mac = "CTRL-SHIFT-C|CMD-SHIFT-C")
    )
    
    ## Load plugin files  ----
    
    observeEvent(input$reload_code_files, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$reload_code_files"))
      
      req("plugins_edit_code" %in% user_accesses)
      
      plugin_folder <- paste0(r$app_folder, "/plugins/", input$selected_plugin_unique_id)
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-selected_plugin_folder', '", plugin_folder, "');"))
      
      create_plugin_files(id = id, r = r, plugin_id = input$selected_element)
      
      # Reload files browser
      output$edit_code_directory_browser <- renderUI("test")
      r$edit_plugin_code_reload_files_browser <- now()
      
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
          file_code <- readLines(paste0(plugin_folder, "/", filename), warn = FALSE)
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
                hotkeys = code_hotkeys,
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
        shinyjs::show(paste0("edit_code_editor_div_", first_file_id))
      }
      
      r$edit_plugin_code_reload_files_tab <- now()
    })
    
    ## Reload files browser ----
    
    observeEvent(r$edit_plugin_code_reload_files_browser, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer r$edit_plugin_code_reload_files_browser"))
      
      output$edit_code_directory_browser <- renderUI(div(
        class = "directories-browser",
        div(class = "directory-item", tags$i(class = "fa fa-folder")),
        div(class = "directory-sep", " / ")
      ))
      
      filenames_order <- c("ui.R", "server.R", "translations.csv")
      edit_plugin_code_files_list <- 
        r$edit_plugin_code_files_list %>%
        dplyr::filter(plugin_id == input$selected_element) %>%
        dplyr::mutate(filename = factor(filename, levels = c(filenames_order, setdiff(unique(filename), filenames_order)))) %>%
        dplyr::arrange(filename) %>%
        dplyr::mutate_at("filename", as.character) %>%
        dplyr::mutate(short_filename = ifelse(nchar(filename) >= 23, paste0(substr(filename, 1, 20), "..."), filename))
      
      files_ui <- tagList()
      if (nrow(edit_plugin_code_files_list) > 0){
        for (i in 1:nrow(edit_plugin_code_files_list)){
          file <- edit_plugin_code_files_list[i, ]
          
          if (file$filename %in% c("ui.R", "server.R", "translations.csv")) icons_div <- tagList()
          else icons_div <- div(
            class = "file-item-icons",
            shinyjs::hidden(
              div(
                id = ns(paste0("edit_code_save_filename_button_div_", file$id)),
                shiny.fluent::IconButton.shinyInput(paste0("edit_code_save_filename_button_", file$id), iconProps = list(iconName = "CheckMark")), 
                class = "small_icon_button",
                onclick = paste0(
                  "Shiny.setInputValue('", id, "-edit_code_save_filename', ", file$id, ", {priority: 'event'});",
                  "Shiny.setInputValue('", id, "-edit_code_save_filename_trigger', Math.random(), {priority: 'event'});"
                )
              )
            ),
            div(
              id = ns(paste0("edit_code_edit_filename_button_div_", file$id)),
              shiny.fluent::IconButton.shinyInput(paste0("edit_code_edit_filename_button_", file$id), iconProps = list(iconName = "Edit")), 
              class = "small_icon_button",
              onclick = paste0(
                "Shiny.setInputValue('", id, "-edit_code_edit_filename', ", file$id, ", {priority: 'event'});",
                "Shiny.setInputValue('", id, "-edit_code_edit_filename_trigger', Math.random(), {priority: 'event'});"
              )
            ),
            div(
              shiny.fluent::IconButton.shinyInput(paste0("edit_code_delete_file_button_", file$id), iconProps = list(iconName = "Delete")), 
              class = "small_icon_button",
              onclick = paste0(
                "Shiny.setInputValue('", id, "-edit_code_delete_file', ", file$id, ", {priority: 'event'});",
                "Shiny.setInputValue('", id, "-edit_code_delete_file_trigger', Math.random(), {priority: 'event'});"
              )
            ),
            onclick = "event.stopPropagation();"
          )
          
          files_ui <- tagList(files_ui,
            tags$li(
              id = ns(paste0("edit_code_file_div_", file$id)),
              class = "file-item",
              div(
                class = "file-item-title",
                tags$i(class = "fa fa-file"),
                shinyjs::hidden(
                  div(
                    id = ns(paste0("edit_code_edit_filename_textfield_div_", file$id)), 
                    shiny.fluent::TextField.shinyInput(ns(paste0("edit_code_edit_filename_textfield_", file$id)), value = file$filename), 
                    class = "small_textfield",
                    onclick = "event.stopPropagation();"
                  )
                ),
                create_hover_card(ui = div(id = ns(paste0("edit_code_filename_div_", file$id)), file$short_filename), text = file$filename)
              ),
              icons_div,
              onClick = htmlwidgets::JS(paste0("Shiny.setInputValue('", id, "-edit_code_selected_file', ", file$id, ", {priority: 'event'});"))
            )
          )
        }
      }
      
      output$edit_code_files_browser <- renderUI(div(
        class = "files-browser",
        files_ui
      ))
    })
    
    ## Reload edit code tabs ----
    
    observeEvent(r$edit_plugin_code_reload_files_tab, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer r$edit_plugin_code_reload_files_tab"))
      
      req("plugins_edit_code" %in% user_accesses)
      
      tabs_ui <- tagList()
      
      plugin_code_tabs <- r$edit_plugin_code_tabs %>% dplyr::filter(plugin_id == input$selected_element) %>% dplyr::arrange(position)
      
      tabs_container <- div(
        id = ns("edit_code_tabs"),
        class = "tabs",
        `data-id-prefix` = paste0(ns("edit_code_tab_")),
        `data-input-id` = ns("edit_code_tab_positions"),
        `data-draggable-class` = "tab"
      )
      
      if (nrow(plugin_code_tabs) > 0) {
        
        first_file_id <- plugin_code_tabs %>% dplyr::slice(1) %>% dplyr::pull(id)
        selected_tab_id <- first_file_id
        if (length(input$edit_code_selected_file) > 0){
          file_id <- input$edit_code_selected_file
          if (file_id %in% plugin_code_tabs$id) selected_tab_id <- file_id
        }
        
        tabs_list <- lapply(1:nrow(plugin_code_tabs), function(i) {
          file <- plugin_code_tabs[i, ]
          
          tab_class <- "tab"
          if (file$id == selected_tab_id) tab_class <- "tab active"
          
          div(
            id = ns(paste0("edit_code_tab_", file$id)),
            class = tab_class,
            onclick = paste0("Shiny.setInputValue('", id, "-edit_code_selected_tab', ", file$id, ", {priority: 'event'})"),
            div(
              file$filename, 
              style = "overflow: hidden; white-space: nowrap; text-overflow: ellipsis;"
            ),
            div(
              id = ns(paste0("edit_code_close_tab_", file$id)),
              class = "close-tab",
              onclick = paste0(
                "Shiny.setInputValue('", id, "-edit_code_close_selected_tab', ", file$id, ", {priority: 'event'});",
                "Shiny.setInputValue('", id, "-edit_code_close_selected_tab_trigger', Math.random(), {priority: 'event'});",
                "event.stopPropagation();"
              ),
              tags$i(class = "fa fa-times")
            ),
            title = file$filename
          )
        })
        
        tabs_container$children <- tabs_list
      }
      
      output$edit_code_tabs_ui <- renderUI(tabs_container)
      
      shinyjs::delay(500, shinyjs::runjs(sprintf("initSortableTabs('%s')", ns("edit_code_tabs"))))
    })
    
    observeEvent(input$edit_code_tab_positions, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$edit_code_tab_positions"))
      
      positions <- input$edit_code_tab_positions
      
      ids <- positions[seq(1, length(positions), 2)]
      pos <- positions[seq(2, length(positions), 2)]
      
      pos_df <- data.frame(
        id = ids,
        position = pos
      )
      
      for (i in 1:nrow(pos_df)) {
        r$edit_plugin_code_tabs <- r$edit_plugin_code_tabs %>%
          dplyr::mutate(
            position = ifelse(
              id == pos_df$id[i] & plugin_id == input$selected_element,
              pos_df$position[i],
              position
            )
          )
      }
    })
    
    ## Change file tab ----
    
    observeEvent(input$edit_code_selected_tab, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$edit_code_selected_tab"))
      
      selected_file_id <- sub(paste0(id, "-edit_code_tab_"), "", input$edit_code_selected_tab)
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-edit_code_selected_file', ", selected_file_id, ");"))
      
      # Set current file tab active
      shinyjs::removeClass(selector = paste0("#", id, "-edit_code_tabs .tab"), class = "active")
      shinyjs::addClass(selector = paste0("#", input$edit_code_selected_tab), class = "active")
      
      # Show current file editor
      shinyjs::delay(50, shinyjs::show(paste0("edit_code_editor_div_", selected_file_id)))
      
      # Hide other editors
      hide_ids <- setdiff(r$edit_plugin_code_tabs$id, selected_file_id)
      sapply(hide_ids, function(file_id) shinyjs::hide(paste0("edit_code_editor_div_", file_id)))
    })
    
    ## Open a file ----
    
    observeEvent(input$edit_code_selected_file, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$edit_code_selected_file"))
      
      req("plugins_edit_code" %in% user_accesses)
      
      file_id <- input$edit_code_selected_file
      file_row <- 
        r$edit_plugin_code_files_list %>% 
        dplyr::filter(id == file_id) %>%
        dplyr::mutate(position = max(r$edit_plugin_code_tabs %>% dplyr::filter(plugin_id == input$selected_element) %>% dplyr::pull(position)) + 1)
      file_ext <- sub(".*\\.", "", tolower(file_row$filename))
      ace_mode <- switch(file_ext, "r" = "r", "py" = "python", "")
      
      # Open a new tab ?
      if (file_id %not_in% r$edit_plugin_code_tabs$id) r$edit_plugin_code_tabs <- r$edit_plugin_code_tabs %>% dplyr::bind_rows(file_row)
      
      r$edit_plugin_code_reload_files_tab <- now()
      
      # Hide all editors
      sapply(paste0("edit_code_editor_div_", r$edit_plugin_code_editors$id), shinyjs::hide)
      
      # Create an ace editor ?
      if (file_id %not_in% r$edit_plugin_code_editors$id){
        r$edit_plugin_code_editors <- r$edit_plugin_code_editors %>% dplyr::bind_rows(r$edit_plugin_code_files_list %>% dplyr::filter(id == file_id))
        r$edit_plugin_code_open_new_editor <- TRUE
      }
      else r$edit_plugin_code_open_new_editor <- FALSE
      
      # Add editor UI or show it if already exists
      if (r$edit_plugin_code_open_new_editor){
        file_code <- readLines(paste0(input$selected_plugin_folder, "/", file_row$filename), warn = FALSE)
        
        # Create editor
        insertUI(selector = paste0("#", ns("edit_code_editors_div")), where = "beforeEnd", ui = div(
          id = ns(paste0("edit_code_editor_div_", file_id)),
          shinyAce::aceEditor(
            ns(paste0("edit_code_editor_", file_id)), value = file_code, mode = ace_mode,
            hotkeys = code_hotkeys,
            theme = user_settings$ace_theme, fontSize = user_settings$ace_font_size,
            autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, showPrintMargin = FALSE
          ),
          style = "width: 100%; height: 100%; display: flex; flex-direction: column;"
        ))
        
        # Add observers for editor hotkeys
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-add_code_editor_hotkeys', ", file_id, ");"))
      }
      else shinyjs::delay(50, shinyjs::show(paste0("edit_code_editor_div_", file_id)))
    })
    
    ## Close a file ----
    
    observeEvent(input$edit_code_close_selected_tab_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$edit_code_close_selected_tab_trigger"))
      
      file_id <- input$edit_code_close_selected_tab
      
      # Hide all editors
      sapply(paste0("edit_code_editor_div_", r$edit_plugin_code_editors$id), shinyjs::hide)
      
      r$edit_plugin_code_tabs <- r$edit_plugin_code_tabs %>% dplyr::filter(id %not_in% file_id)
      
      # Show editor of first file
      first_file_id <-
        r$edit_plugin_code_tabs %>%
        dplyr::filter(plugin_id == input$selected_element) %>%
        dplyr::slice(1) %>%
        dplyr::pull(id)
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-edit_code_selected_file', ", first_file_id, ");"))
      if (length(first_file_id) > 0) shinyjs::delay(50, shinyjs::show(paste0("edit_code_editor_div_", first_file_id)))
      
      r$edit_plugin_code_reload_files_tab <- now()
    })
    
    ## Create a file ----
    
    observeEvent(input$edit_code_add_file, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$edit_code_add_file"))
      
      req("plugins_edit_code" %in% user_accesses)
      
      # List files to create new filename
      files_list <- list.files(path = input$selected_plugin_folder, pattern = "(?i)*.\\.(r|py|csv|css|js)$")
      base_name <- "untitled"
      extension <- ".R"
      i <- 1
      new_filename <- paste0(base_name, extension)
      while(new_filename %in% files_list) {
        i <- i + 1
        new_filename <- paste0(base_name, i, extension)
      }

      # Create file
      writeLines("", paste0(input$selected_plugin_folder, "/", new_filename))

      # Add file in database
      # If a file is added and then deleted, change ID to create a new editor
      options_new_row_id <- get_last_row(r$db, "options") + 1
      if (length(r$options_new_row_id) > 0) if (r$options_new_row_id >= options_new_row_id) options_new_row_id <- r$options_new_row_id + 1
      r$options_new_row_id <- options_new_row_id

      new_options <- tibble::tibble(
        id = options_new_row_id, category = "plugin_code", link_id = {input$selected_element}, name = "filename",
        value = {new_filename}, value_num = NA_real_, creator_id = r$user_id, datetime = now(), deleted = FALSE
      )
      DBI::dbAppendTable(r$db, "options", new_options)

      new_code <- tibble::tibble(
        id = get_last_row(r$db, "code") + 1, category = "plugin", link_id = {options_new_row_id}, code = "",
        creator_id = r$user_id, datetime = now(), deleted = FALSE
      )
      DBI::dbAppendTable(r$db, "code", new_code)

      # Update files browser
      r$edit_plugin_code_files_list <- r$edit_plugin_code_files_list %>% dplyr::bind_rows(tibble::tibble(id = options_new_row_id, plugin_id = input$selected_element, filename = new_filename))
      
      r$edit_plugin_code_reload_files_browser <- now()
      
      # Hide all editors
      sapply(paste0("edit_code_editor_div_", r$edit_plugin_code_editors$id), shinyjs::hide)

      # Create editor if doesn't exist (when we delete and recreate a plugin, some editors could persist...)
      if (options_new_row_id %not_in% r$edit_plugin_code_editors$id){
        insertUI(selector = paste0("#", ns("edit_code_editors_div")), where = "beforeEnd", ui = div(
          id = ns(paste0("edit_code_editor_div_", options_new_row_id)),
          shinyAce::aceEditor(
            ns(paste0("edit_code_editor_", options_new_row_id)), value = "", mode = "r",
            hotkeys = code_hotkeys,
            theme = user_settings$ace_theme, fontSize = user_settings$ace_font_size,
            autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, showPrintMargin = FALSE
          ),
          style = "width: 100%; height: 100%; display: flex; flex-direction: column;"
        ))
      }
      
      # Add observers for editor hotkeys
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-add_code_editor_hotkeys', ", options_new_row_id, ");"))

      r$edit_plugin_code_editors <- r$edit_plugin_code_editors %>% dplyr::bind_rows(r$edit_plugin_code_files_list %>% dplyr::filter(id == options_new_row_id))

      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-edit_code_selected_file', ", options_new_row_id, ");"))

      file_row <- 
        r$edit_plugin_code_files_list %>% 
        dplyr::filter(id == options_new_row_id) %>%
        dplyr::mutate(position = max(r$edit_plugin_code_tabs %>% dplyr::filter(plugin_id == input$selected_element) %>% dplyr::pull(position)) + 1)
      
      r$edit_plugin_code_tabs <- r$edit_plugin_code_tabs %>% dplyr::bind_rows(file_row)
      r$edit_plugin_code_reload_files_tab <- now()
    })
    
    ## Editor hotkeys ----
    observeEvent(input$add_code_editor_hotkeys, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$add_code_editor_hotkeys"))
      
      file_id <- input$add_code_editor_hotkeys
      editor_id <- paste0("edit_code_editor_", file_id)
      
      observeEvent(input[[paste0(editor_id, "_run_all")]], {
        if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$edit_code_editor..run_all"))
        r$run_plugin_code <- now()
      })
      
      observeEvent(input[[paste0(editor_id, "_run_selection")]], {
        if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$edit_code_editor..run_selection"))
        r$run_plugin_code <- now()
      })
      
      observeEvent(input[[paste0(editor_id, "_comment")]], {
        if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$edit_code_editor..comment"))
        
        toggle_comments(id = id, input_id = editor_id, code = input[[editor_id]], selection = input[[paste0(editor_id, "_comment")]]$range, session = session)
      })
      
      observeEvent(input[[paste0(editor_id, "_save")]], {
        if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$edit_code_editor..save"))
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_file_code', Math.random());"))
      })
    })
    
    ## Save updates ----
    
    observeEvent(input$save_file_code, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$save_file_code"))
      
      req("plugins_edit_code" %in% user_accesses)
      
      req(length(input$edit_code_selected_file) > 0)
      
      file <- r$edit_plugin_code_files_list %>% dplyr::filter(id == input$edit_code_selected_file)
      new_code <- input[[paste0("edit_code_editor_", file$id)]]
      
      # Update file
      writeLines(new_code, paste0(input$selected_plugin_folder, "/", file$filename))
      
      # Update database
      sql <- glue::glue_sql("UPDATE code SET code = {new_code} WHERE category = 'plugin' AND link_id = {file$id}", .con = r$db)
      sql_send_statement(r$db, sql)
      
      # Update update_datetime
      plugin_id <- input$selected_element
      sql_send_statement(r$db, glue::glue_sql("UPDATE plugins SET update_datetime = {now()} WHERE id = {plugin_id}", .con = r$db))
      
      # Reload plugins vars
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_elements_var', Math.random());"))
      
      show_message_bar(output, "modif_saved", "success", i18n = i18n, ns = ns)
    })
    
    ## Rename a file ----
    
    # Show textfield
    observeEvent(input$edit_code_edit_filename_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$edit_code_edit_filename_trigger"))
      
      file_id <- input$edit_code_edit_filename
      
      shinyjs::delay(50, sapply(c(paste0("edit_code_edit_filename_textfield_div_", file_id), paste0("edit_code_save_filename_button_div_", file_id)), shinyjs::show))
      sapply(c(paste0("edit_code_filename_div_", file_id), paste0("edit_code_edit_filename_button_div_", file_id)), shinyjs::hide)
    })
    
    # Save new name
    observeEvent(input$edit_code_save_filename_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$edit_code_save_filename_trigger"))
      
      file_id <- input$edit_code_save_filename
      textfield_id <- paste0("edit_code_edit_filename_textfield_", file_id)
      
      old_name <- r$edit_plugin_code_files_list %>% dplyr::filter(id == file_id) %>% dplyr::pull(filename)
      new_name <- input[[textfield_id]]
      
      # Check extension
      check_extension <- grepl("\\.(r|py|csv|css|js)$", new_name, ignore.case = TRUE)
      if (!check_extension) show_message_bar(output, "invalid_file_extension", "severeWarning", i18n = i18n, ns = ns)
      req(check_extension)
      
      # Check name (no space, no special character and not empty)
      if (new_name == "") check_special_char <- FALSE
      else check_special_char <- grepl("^[\\w\\.]+\\.(r|py|csv|css|js)$", new_name, ignore.case = TRUE, perl = TRUE)
      if (!check_special_char) show_message_bar(output, "invalid_filename", "severeWarning", i18n = i18n, ns = ns)
      req(check_special_char)
      
      # Check if name is already used
      if (new_name == old_name) check_used_name <- TRUE
      else {
        check_used_name <- !file.exists(paste0(input$selected_plugin_folder, "/", new_name))
        if (!check_used_name) {
          sql <- glue::glue_sql("SELECT * FROM options WHERE category = 'plugin_code' AND name = 'filename' AND value = {new_name}", .con = r$db)
          used_name <- DBI::dbGetQuery(r$db, sql)
          check_used_name <- !nrow(used_name) > 0
        }
      }
      if (!check_used_name) show_message_bar(output, "name_already_used", "severeWarning", i18n = i18n, ns = ns)
      req(check_used_name)
      
      # Update file
      file.rename(paste0(input$selected_plugin_folder, "/", old_name), paste0(input$selected_plugin_folder, "/", new_name))
      
      # Update database
      sql <- glue::glue_sql("UPDATE options SET value = {new_name} WHERE id = {file_id}", .con = r$db)
      sql_send_statement(r$db, sql)
      
      # Update tabs
      r$edit_plugin_code_tabs <- r$edit_plugin_code_tabs %>% dplyr::mutate(filename = dplyr::case_when(id == file_id ~ new_name, TRUE ~ filename))
      shinyjs::runjs(paste0("document.getElementById('", id, "-edit_code_tab_", file_id, "').childNodes[0].nodeValue = '", new_name, "';"))
      
      # Update files browser
      r$edit_plugin_code_files_list <- r$edit_plugin_code_files_list %>% dplyr::mutate(filename = dplyr::case_when(id == file_id ~ new_name, TRUE ~ filename))
      shinyjs::runjs(paste0("document.getElementById('", id, "-edit_code_filename_div_", file_id, "').childNodes[0].nodeValue = '", new_name, "';"))
      
      show_message_bar(output,  "modif_saved", "success", i18n = i18n, ns = ns)
      
      shinyjs::delay(50, sapply(c(paste0("edit_code_filename_div_", file_id), paste0("edit_code_edit_filename_button_div_", file_id)), shinyjs::show))
      sapply(c(paste0("edit_code_edit_filename_textfield_div_", file_id), paste0("edit_code_save_filename_button_div_", file_id)), shinyjs::hide)
    })
    
    ## Delete a file ----
    
    observeEvent(input$edit_code_delete_file_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$edit_code_delete_file_trigger"))
      shinyjs::show("delete_file_modal")
    })
    
    observeEvent(input$close_file_deletion_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$close_file_deletion_modal"))
      shinyjs::hide("delete_file_modal")
    })
    
    observeEvent(input$confirm_file_deletion, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$confirm_file_deletion"))
      
      req("plugins_edit_code" %in% user_accesses)
      
      file_id <- input$edit_code_delete_file
      filename <- r$edit_plugin_code_files_list %>% dplyr::filter(id == file_id) %>% dplyr::pull(filename)
      
      # Delete file
      file.remove(paste0(input$selected_plugin_folder, "/", filename))
      
      # Remove from database
      sql <- glue::glue_sql("DELETE FROM options WHERE id = {file_id}", .con = r$db)
      sql_send_statement(r$db, sql)
      
      sql <- glue::glue_sql("DELETE FROM code WHERE category = 'plugin' AND link_id = {file_id}", .con = r$db)
      sql_send_statement(r$db, sql)
      
      # Reload files browser
      r$edit_plugin_code_files_list <- r$edit_plugin_code_files_list %>% dplyr::filter(id != file_id)
      r$edit_plugin_code_reload_files_browser <- now()
      
      # Reload tabs if file is opened
      reload_tabs <- FALSE
      if (file_id %in% r$edit_plugin_code_tabs$id) reload_tabs <- TRUE
      r$edit_plugin_code_tabs <- r$edit_plugin_code_tabs %>% dplyr::filter(id != file_id)
      if (reload_tabs) r$edit_plugin_code_reload_files_tab <- now()
      
      # Hide all editors
      sapply(paste0("edit_code_editor_div_", r$edit_plugin_code_editors$id), shinyjs::hide)
      
      # Show editor of last file
      first_file_id <- r$edit_plugin_code_tabs %>% dplyr::filter(plugin_id == input$selected_element) %>% dplyr::slice(1) %>% dplyr::pull(id)
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-edit_code_selected_file', ", first_file_id, ");"))
      if (length(first_file_id) > 0) shinyjs::delay(50, shinyjs::show(paste0("edit_code_editor_div_", first_file_id)))
      
      shinyjs::hide("delete_file_modal")
      show_message_bar(output,  "file_deleted", "warning", i18n = i18n, ns = ns)
    })
    
    ## Select concepts ----
    
    # Open modal
    observeEvent(input$select_concepts, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$select_concepts"))
      shinyjs::show("select_concepts_modal")
    })
    
    # Close modal
    observeEvent(input$close_select_concepts_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$close_select_concepts_modal"))
      shinyjs::hide("select_concepts_modal")
    })
    
    ## Run plugin code ----
    
    observeEvent(input$run_plugin_code, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$run_plugin_code"))
      r$run_plugin_code <- now()
    })
    
    observeEvent(input$reload_plugin_code, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$reload_plugin_code"))
      r$run_plugin_code <- now()
    })
    
    # Initiate widgets var
    r$run_plugin_last_widget_id <- get_last_row(r$db, "widgets") + 10^6 %>% as.integer()
    r$run_plugin_tab_id <- get_last_row(r$db, "tabs") + 10^6 %>% as.integer()
    
    # Initiate gridstack instance
    create_gridstack_instance(id, "plugin_run_code")
    
    observeEvent(r$run_plugin_code, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer r$run_plugin_code"))

      req("plugins_edit_code" %in% user_accesses)
      
      # Switch to 'Test' tab
      shinyjs::runjs(paste0("
        Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());
        Shiny.setInputValue('", id, "-current_tab', 'run_code');"))
      
      # Notify user that if there's not a study loaded, server code might not work
      no_study_loaded <- TRUE
      if (length(m$selected_study) > 0) if (!is.na(m$selected_study)) no_study_loaded <- FALSE
      if (no_study_loaded) show_message_bar(output, "plugin_no_selected_project", "warning", i18n = i18n, ns = ns)

      # Create translations file

        tryCatch({
          # Create plugin folder in translations folder if doesn't exist
          new_dir <- paste0(r$app_folder, "/translations/", input$selected_plugin_unique_id)
          if (!dir.exists(new_dir)) dir.create(new_dir)

          # Get translations file
          translations <- readLines(paste0(input$selected_plugin_folder, "/translations.csv"), warn = FALSE)

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

      # Create a session number, to inactivate older observers
      # Reset all older observers

      session_code <- "plugin_test"
      if (length(m[[session_code]]) == 0) session_num <- 1L
      if (length(m[[session_code]]) > 0) session_num <- m[[session_code]] + 1
      m[[session_code]] <- session_num

      # NB : req(m[[session_code]] == session_num) & req(m$selected_study == %study_id%) must be put at the beginning of each observeEvent in plugins code

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
          show_message_bar(output,  "error_run_plugin_ui_code", "severeWarning", i18n = i18n, ns = ns)
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
      
      ui_output <- create_widget(id, widget_id, code$ui, w = widget_pos$w, h = widget_pos$h, x = widget_pos$x, y = widget_pos$y)
      add_widget_to_gridstack(id, "plugin_run_code", ui_output, widget_id, previous_widget_id)
      output[[paste0("ui_", widget_id)]] <- renderUI(code$ui)
      output[[paste0("edit_buttons_", widget_id)]] <- renderUI(get_widget_edit_buttons(id, widget_id))
      
      # New environment, to authorize access to selected variables from shinyAce editor
      # We choose which vars to keep access to

      # Variables to hide
      new_env_vars <- list("r" = NA)

      # Variables to keep
      variables_to_keep <- c("d", "m", "session_code", "session_num", "i18n", "selected_concepts", "debug")
      if (exists("i18np")) variables_to_keep <- c(variables_to_keep, "i18np")

      for (var in variables_to_keep) new_env_vars[[var]] <- eval(parse(text = var))
      new_env <- rlang::new_environment(data = new_env_vars, parent = pryr::where("r"))

      options('cli.num_colors' = 1)

      # Capture console output of our code
      captured_output <- capture.output(
        tryCatch(eval(parse(text = code$server), envir = new_env), error = function(e){
          r$widget_server_last_error <- e
          show_message_bar(output,  "error_run_plugin_server_code", "severeWarning", i18n = i18n, ns = ns)
          cat(paste0("\n", now(), " - mod_plugins - error loading server code - plugin_id = ", input$selected_element, " - ", toString(e)))
        }))

      # Restore normal value
      options('cli.num_colors' = NULL)

      output$run_code_result_server <- renderText(paste(captured_output, collapse = "\n"))

      output$run_code_datetime_code_execution <- renderText(format_datetime(now(), language))
    })
    
    ## Edit run code page ----
    
    r$plugin_run_code_edit_page_activated <- FALSE
    
    observeEvent(input$edit_page_on, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$edit_page_on"))
      
      req("plugins_edit_code" %in% user_accesses)
      
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
    
    observeEvent(input$edit_page_off, {
      if (debug) cat(paste0("\n", now(), " - mod_plugins - observer input$edit_page_off"))
      
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
    observeEvent(input$widget_position_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$widget_position_trigger"))
      
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
