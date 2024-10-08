#' @noRd 
mod_data_cleaning_ui <- function(id, language, languages, i18n, code_hotkeys){
  ns <- NS(id)
  
  pivot_item_js <- paste0("
    Shiny.setInputValue('", id, "-current_tab', this.id);
    Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());"
  )
  
  div(class = "main",
      
    # Load widget UI ----
    
    mod_widgets_ui(id, language, languages, i18n),
    
    # Data cleaning script details ----
    
    shinyjs::hidden(
      div(
        id = ns("one_element"),
        div(
          uiOutput(ns("breadcrumb")),
          div(
            id = ns("data_cleaning_pivot"),
            tags$button(id = ns("summary"), i18n$t("summary"), class = "pivot_item selected_pivot_item", onclick = pivot_item_js),
            tags$button(id = ns("edit_code"), i18n$t("code"), class = "pivot_item", onclick = pivot_item_js),
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
            class = "data_cleaning_summary_left"
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
            class = "data_cleaning_summary_right"
          ),
          class = "data_cleaning_summary_container"
        ),
        
        ## Edit code ----
        shinyjs::hidden(
          div(
            id = ns("edit_code_div"),
            div(
              id = ns("edit_code_forbidden_access"),
              shiny.fluent::MessageBar(i18n$t("unauthorized_access_area"), messageBarType = 5),
              style = "display: inline-block; margin-top: 15px;"
            ),
            shinyjs::hidden(
              div(
                id = ns("edit_code_content_div"),
                div(
                  shinyAce::aceEditor(
                    ns("data_cleaning_code"), value = "", mode = "r",
                    code_hotkeys = list("r", code_hotkeys),
                    autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, fontSize = 11, showPrintMargin = FALSE
                  ),
                  class = "element_ace_editor"
                ),
                div(
                  verbatimTextOutput(ns("code_result")),
                  class = "element_code_result"
                ),
                style = "height: 100%; display: flex;"
              )
            ),
            style = "height: 100%;"
          )
        ),
        
        ## Share ----
        shinyjs::hidden(
          div(
            id = ns("share_div"),
            div(
              div(
                h1(i18n$t("synchronize_with_git_repo")),
                div(shiny.fluent::Dropdown.shinyInput(ns("git_repo"), label = i18n$t("git_repo")), style = "width: 200px;"),
                div(uiOutput(ns("git_repo_element_ui")), style = "margin-top:10px;"),
                div(
                  uiOutput(ns("synchronize_git_buttons")),
                  class = "data_cleaning_share_buttons"
                ),
                # Button to download a data cleaning script (sidenav button)
                div(downloadButton(ns("export_element_download")), style = "visibility: hidden; position: absolute; right: 0; bottom: 0;"),
                class = "widget", style = "height: 50%; padding-top: 1px;"
              ),
              class = "data_cleaning_share_left",
            ),
            class = "data_cleaning_share_container"
          )
        ),
        
        style = "height: 100%; display: flex; flex-direction: column;"
      )
    )
  )
}

#' @noRd 
mod_data_cleaning_server <- function(id, r, d, m, language, i18n, debug, user_accesses){
  # |-------------------------------- -----
  
  if (debug) cat(paste0("\n", now(), " - mod_data_cleaning - ", id, " - start"))
  
  # Load widgets ----
  
  all_divs <- c("summary", "edit_code", "share")
  mod_widgets_server(id, r, d, m, language, i18n, all_divs, debug, user_accesses)
  
  # Data cleaning scripts module ----
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Current user accesses ----
    
    if ("data_cleaning_management" %in% user_accesses) sapply(c("create_element_button", "edit_summary_div", "delete_element_div"), shinyjs::show)
    if ("data_cleaning_import" %in% user_accesses) shinyjs::show("import_element_button")
    if ("data_cleaning_edit_code" %in% user_accesses){
      shinyjs::hide("edit_code_forbidden_access")
      sapply(c("edit_code_buttons", "edit_code_content_div"), shinyjs::show)
    }
    
    if ("data_cleaning_share" %in% user_accesses){
      sapply(c("share_content_div", "export_element_button"), shinyjs::show)
      shinyjs::hide("share_forbidden_access")
    }
    
    # |-------------------------------- -----
    
    # --- --- --- --- ---
    # Script summary ----
    # --- --- --- --- ---
    
    # |-------------------------------- -----
    
    # --- --- --- --- --- -
    # Edit script code ----
    # --- --- --- --- --- -
    
    # Load code ----
    
    observeEvent(input$load_data_cleaning_code, {
      if (debug) cat(paste0("\n", now(), " - mod_data_cleaning - observer input$load_data_cleaning_code"))
      
      data_cleaning_script_id <- input$selected_element
      
      sql <- glue::glue_sql("SELECT code FROM code WHERE category = 'data_cleaning' AND link_id = {data_cleaning_script_id}", .con = r$db)
      code <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
      
      shinyAce::updateAceEditor(session, "data_cleaning_code", value = code)
    })
    
    # Comment code ----
    observeEvent(input$data_cleaning_code_comment, {
      if (debug) cat(paste0("\n", now(), " - mod_data_cleaning - observer input$data_cleaning_code_comment"))
      
      lines <- strsplit(input$data_cleaning_code, "\n")[[1]]
      req(length(lines) > 0)
      
      start_row <- input$data_cleaning_code_comment$range$start$row + 1
      end_row <- input$data_cleaning_code_comment$range$end$row + 1
      
      for (i in start_row:end_row) if (startsWith(lines[i], "# ")) lines[i] <- substr(lines[i], 3, nchar(lines[i])) else lines[i] <- paste0("# ", lines[i])
      
      shinyAce::updateAceEditor(session, "data_cleaning_code", value = paste0(lines, collapse = "\n"))
      
      shinyjs::runjs(sprintf("
        var editor = ace.edit('%s-data_cleaning_code');
        editor.moveCursorTo(%d, %d);
        editor.focus();
      ", id, input$data_cleaning_code_comment$range$end$row, input$data_cleaning_code_comment$range$end$column))
    })
    
    # Run code ----
    
    observeEvent(input$run_code, {
      if (debug) cat(paste0("\n", now(), " - mod_data_cleaning - observer input$run_code"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_trigger', Math.random());"))
    })
    
    observeEvent(input$data_cleaning_code_run_selection, {
      
      if (debug) cat(paste0("\n", now(), " - mod_data_cleaning - observer input$data_cleaning_code_run_selection"))
      
      if(!shinyAce::is.empty(input$data_cleaning_code_run_selection$selection)) shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-data_cleaning_code', '", input$data_cleaning_code_run_selection$selection, "');"))
      else shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-data_cleaning_code', '", input$data_cleaning_code_run_selection$line, "');"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_trigger', Math.random());"))
    })
    
    observeEvent(input$data_cleaning_code_run_all, {
      if (debug) cat(paste0("\n", now(), " - mod_data_cleaning - observer input$code_run_all"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_trigger', Math.random());"))
    })
    
    observeEvent(input$run_code_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_data_cleaning - observer input$run_code_trigger"))
      
      data_cleaning_script_id <- input$selected_element
      
      code <- input$data_cleaning_code %>% gsub("\r", "\n", .)
      
      result <- capture.output(tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w)))
      
      output$code_result <- renderText(paste(result, collapse = "\n"))
    })
    
    # Save code ----
    
    observeEvent(input$save_code, {
      if (debug) cat(paste0("\n", now(), " - mod_data_cleaning - observer input$save_code"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_code_trigger', Math.random());"))
    })
    
    observeEvent(input$data_cleaning_code_save, {
      if (debug) cat(paste0("\n", now(), " - mod_data_cleaning - observer input$data_cleaning_code_save"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_code_trigger', Math.random());"))
    })
    
    observeEvent(input$save_code_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_data_cleaning - observer input$save_code_trigger"))
      
      data_cleaning_script_id <- input$selected_element
      sql_send_statement(r$db, glue::glue_sql("UPDATE scripts SET update_datetime = {now()} WHERE id = {data_cleaning_script_id}", .con = r$db))
      
      code_id <- DBI::dbGetQuery(r$db, glue::glue_sql("SELECT id FROM code WHERE category = 'data_cleaning' AND link_id = {data_cleaning_script_id}", .con = r$db)) %>% dplyr::pull()
      
      sql_send_statement(r$db, glue::glue_sql("UPDATE code SET code = {input$data_cleaning_code} WHERE id = {code_id}", .con = r$db))
      
      # Reload data cleaning scripts vars
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_elements_var', Math.random());"))
      
      show_message_bar(output, "modif_saved", "success", i18n = i18n, ns = ns)
    })
  })
}
