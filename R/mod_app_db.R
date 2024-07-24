#' @noRd 
mod_app_db_ui <- function(id, language, languages, i18n, code_hotkeys, db_col_types){
  ns <- NS(id)
  
  pivot_item_js <- paste0("
    Shiny.setInputValue('", id, "-current_tab', this.id);
    Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());"
  )
  
  div(
    class = "main",
    
    # Pivot ----
    
    div(
      div(),
      div(
        id = ns("app_db_pivot"),
        tags$button(id = ns("connection_settings"), i18n$t("connection"), class = "pivot_item selected_pivot_item", onclick = pivot_item_js),
        tags$button(id = ns("request_db"), i18n$t("app_db_request"), class = "pivot_item", onclick = pivot_item_js),
        tags$button(id = ns("backups"), i18n$t("backups"), class = "pivot_item", onclick = pivot_item_js),
        class = "pivot"
      ),
      style = "display: flex; justify-content: space-between; z-index: 100;"
    ),
    
    # Connection settings ----
    
    div(
      id = ns("connection_settings_div"),
      div(
        div(
          h1(i18n$t("connection_settings")),
          shiny.fluent::ChoiceGroup.shinyInput(ns("connection_type"), label = i18n$t("connection"), options = list(
            list(key = "local", text = i18n$t("local")),
            list(key = "remote", text = i18n$t("remote"))
          ), className = "inline_choicegroup"),
          shinyjs::hidden(
            div(
              id = ns("connection_infos_div"),
              div(
                shiny.fluent::Dropdown.shinyInput(ns("sql_lib"), label = i18n$t("sql_lib"),
                  options = list(
                    list(key = "postgres", text = "PostgreSQL"),
                    list(key = "sqlite", text = "SQLite")
                  ), value = "postgres"
                ),
                style = "width: 200px;"
              ),
              div(
                div(shiny.fluent::TextField.shinyInput(ns("host"), label = i18n$t("host")), style = "width: 200px"),
                div(shiny.fluent::TextField.shinyInput(ns("port"), label = i18n$t("port")), style = "width: 200px"),
                style = "display: flex; gap: 10px;"
              ),
              div(
                div(shiny.fluent::TextField.shinyInput(ns("user"), label = i18n$t("user")), style = "width: 200px"),
                div(shiny.fluent::TextField.shinyInput(ns("password"), label = i18n$t("password"), type = "password", canRevealPassword = TRUE), style = "width: 200px"),
                style = "display: flex; gap: 10px;"
              ),
              div(
                div(shiny.fluent::TextField.shinyInput(ns("main_db_name"), label = i18n$t("main_db_name")), style = "width: 200px"),
                div(uiOutput(ns("main_db_result")), style = "margin-top: 29px;"),
                style = "display: flex; gap: 10px;"
              ),
              div(
                div(shiny.fluent::TextField.shinyInput(ns("public_db_name"), label = i18n$t("public_db_name")), style = "width: 200px"),
                div(uiOutput(ns("public_db_result")), style = "margin-top: 29px;"),
                style = "display: flex; gap: 10px;"
              )
            )
          ),
          class = "widget", style = "min-height: 50%; padding-top: 1px; padding: 1px 15px 15px 15px;"
        ),
        class = "app_db_connection_right"
      ),
      class = "app_db_connection_container"
    ),
    
    # Request db ----
    
    shinyjs::hidden(
      div(
        id = ns("request_db_div"),
        div(
          div(
            shiny.fluent::ChoiceGroup.shinyInput(ns("run_sql_code_db"), label = i18n$t("database"), options = list(
              list(key = "main", text = i18n$t("main_db")),
              list(key = "public", text = i18n$t("public_db"))
            ), value = "main", className = "inline_choicegroup"),
            style = "margin-bottom: 10px;"
          ),
          shinyAce::aceEditor(
            ns("sql_code"), value = "", mode = "sql",
            code_hotkeys = list("r", code_hotkeys),
            autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, fontSize = 11, showPrintMargin = FALSE
          ),
          style = "width: 50%; height: calc(100% - 70px);"
        ),
        div(
          textOutput(ns("datetime_code_execution")),
          verbatimTextOutput(ns("sql_result")),
          class = "element_code_result"
        ),
        style = "height: 100%; display: flex;"
      )
    ),
    
    # Backups ----
    
    shinyjs::hidden(
      div(
        id = ns("backups_div"),
        div(
          div(
            tags$h1(i18n$t("export_db")),
            div(uiOutput(ns("last_db_save")), style = "margin-top: 15px;"), br(),
            div(
              div(
                shiny.fluent::Dropdown.shinyInput(
                  ns("main_tables_to_export"), label = i18n$t("main_tables_to_export"), multiSelect = TRUE,
                  options = db_col_types %>% dplyr::filter(db == "main") %>% convert_tibble_to_list(key_col = "table", text_col = "table"),
                  value = db_col_types %>% dplyr::filter(db == "main" & table != "log") %>% dplyr::pull(table)
                ),
                style = "width: 200px;"
              ),
              div(
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("export_db_check_all_main_tables"), iconProps = list(iconName = "CheckboxComposite")), text = i18n$t("select_all_tables")),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("export_db_uncheck_all_main_tables"), iconProps = list(iconName = "Checkbox")), text = i18n$t("unselect_all_tables")),
                style = "margin: 27px 0 0 5px; display: flex;"
              ),
              class = "small_icon_button",
              style = "display: flex;"
            ),
            div(
              div(
                shiny.fluent::Dropdown.shinyInput(
                  ns("public_tables_to_export"), label = i18n$t("public_tables_to_export"), multiSelect = TRUE,
                  options = db_col_types %>% dplyr::filter(db == "public") %>% convert_tibble_to_list(key_col = "table", text_col = "table"),
                  value = db_col_types %>% 
                    dplyr::filter(db == "public" & table %not_in% c(
                      "concept", "concept_dataset", "concept_user", "domain", "concept_class", 
                      "concept_relationship", "concept_relationship_user", "concept_relationship_evals",
                      "relationship", "concept_synonym", "concept_ancestor", "drug_strength"
                    )) %>% 
                    dplyr::pull(table)
                ),
                style = "width: 200px;"
              ),
              div(
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("export_db_check_all_public_tables"), iconProps = list(iconName = "CheckboxComposite")), text = i18n$t("select_all_tables")),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("export_db_uncheck_all_public_tables"), iconProps = list(iconName = "Checkbox")), text = i18n$t("unselect_all_tables")),
                style = "margin: 27px 0 0 5px; display: flex;"
              ),
              class = "small_icon_button",
              style = "display: flex;"
            ),
            div(
              div(style = "visibility:hidden;", downloadButton(ns("db_save"))),
              shiny.fluent::PrimaryButton.shinyInput(ns("export_db"), i18n$t("export_db"), iconProps = list(iconName = "Download")),
              class = "export_db_buttons"
            ),
            class = "widget", style = "min-height: 50%; padding-top: 1px; padding: 1px 15px 15px 15px;"
          ),
          class = "app_db_backups_left"
        ),
        div(
          div(
            tags$h1(i18n$t("restore_db")),
            div(uiOutput(ns("last_db_restore")), style = "margin-top: 15px;"), br(),
            div(
              shiny.fluent::DefaultButton.shinyInput(ns("db_restore_browse"), i18n$t("choose_zip_file"), style = "width: 200px;"), br(),
              uiOutput(ns("db_restore_loaded_file"), style = "margin-left: 10px; height: 24px; display: flex; align-items: center;"),
              style = "display: flex;"
            ), br(),
            div(
              div(style = "display:none;", fileInput(ns("db_restore"), label = "", multiple = FALSE, accept = ".zip")),
              shiny.fluent::PrimaryButton.shinyInput(ns("import_db"), i18n$t("import_db"), iconProps = list(iconName = "Upload")),
              class = "import_db_buttons"
            ),
            shinyjs::hidden(
              div(
                id = ns("import_db_tables_div"),
                div(
                  div(
                    shiny.fluent::Dropdown.shinyInput(
                      ns("main_tables_to_import"), label = i18n$t("main_tables_to_import"), multiSelect = TRUE
                    ),
                    style = "width: 200px;"
                  ),
                  div(
                    create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("import_db_check_all_main_tables"), iconProps = list(iconName = "CheckboxComposite")), text = i18n$t("select_all_tables")),
                    create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("import_db_uncheck_all_main_tables"), iconProps = list(iconName = "Checkbox")), text = i18n$t("unselect_all_tables")),
                    style = "margin: 27px 0 0 5px; display: flex;"
                  ),
                  class = "small_icon_button",
                  style = "display: flex;"
                ),
                div(
                  div(
                    shiny.fluent::Dropdown.shinyInput(
                      ns("public_tables_to_import"), label = i18n$t("public_tables_to_import"), multiSelect = TRUE
                    ),
                    style = "width: 200px;"
                  ),
                  div(
                    create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("import_db_check_all_public_tables"), iconProps = list(iconName = "CheckboxComposite")), text = i18n$t("select_all_tables")),
                    create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("import_db_uncheck_all_public_tables"), iconProps = list(iconName = "Checkbox")), text = i18n$t("unselect_all_tables")),
                    style = "margin: 27px 0 0 5px; display: flex;"
                  ),
                  class = "small_icon_button",
                  style = "display: flex;"
                )
              )
            ),
            class = "widget", style = "min-height: 50%; padding-top: 1px; padding: 1px 15px 15px 15px;"
          ),
          class = "app_db_backups_right"
        ),
        class = "app_db_backups_container"
      )
    ),
    
    style = "height: 100%; display: flex; flex-direction: column;"
  )
}

#' @noRd
mod_app_db_server <- function(id, r, d, m, language, i18n, db_col_types, app_folder, debug){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    all_divs <- c("connection_settings", "request_db", "backups")
    
    # Current tab ----
    
    observeEvent(input$current_tab_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_app_db - observer input$current_tab_trigger"))
      
      current_tab <- gsub(paste0(id, "-"), "", input$current_tab, fixed = FALSE)
      
      # Show or hide pages depending on selected tab
      divs <- setdiff(all_divs, current_tab)
      divs <- c(paste0(divs, "_reduced_sidenav"), paste0(divs, "_large_sidenav"), paste0(divs, "_div"))
      
      # Prevent a bug with scroll into ace editor
      if (current_tab == "request_db") shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
      
      sapply(divs, shinyjs::hide)
      sapply(c(paste0(current_tab, "_div"), paste0(current_tab, "_reduced_sidenav"), paste0(current_tab, "_large_sidenav")), shinyjs::show)
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_hide_sidenav', 'hide');"))
      
      # Change selected tab
      sapply(all_divs, function(button_id) shinyjs::removeClass(class = "selected_pivot_item", selector = paste0("#", id, "-", button_id)))
      shinyjs::addClass(class = "selected_pivot_item", selector = paste0("#", id, "-", current_tab))
    })
    
    # App db settings ----
    
    ## Load saved settings ----
    
    observeEvent(r$local_db, {
      if (debug) cat(paste0("\n", now(), " - mod_app_db - observer r$local_db"))
      
      # Get remote db informations
      db_info <- 
        DBI::dbGetQuery(r$local_db, "SELECT * FROM options WHERE category = 'remote_db'") %>%
        tibble::as_tibble() %>%
        dplyr::pull(value, name) %>%
        as.list()
      
      # Fill textfields & choicegroup with recorded informations in local database
      for (name in names(db_info)){
        if (name == "connection_type") r$db_connection_type <- db_info[[name]]
        if (name != "connection_type") shiny.fluent::updateTextField.shinyInput(session, name, value = db_info[[name]])
      }
    })
    
    ## Update connection type ----
    
    observeEvent(input$connection_type, {
      if (debug) cat(paste0("\n", now(), " - mod_app_db - observer r$connection_type"))
      r$db_connection_type <- input$connection_type
    })
    
    observeEvent(r$db_connection_type, {
      if (debug) cat(paste0("\n", now(), " - mod_app_db - observer r$db_connection_type"))
      
      shiny.fluent::updateChoiceGroup.shinyInput(session, "connection_type", value = r$db_connection_type)
      
      divs <- c("connection_infos_div", "test_connection_div")
      if (r$db_connection_type == "remote") sapply(divs, shinyjs::show)
      else sapply(divs, shinyjs::hide)
    })
    
    ## Test connection ----
    
    observeEvent(input$test_connection, {
      if (debug) cat(paste0("\n", now(), " - mod_app_db - observer input$test_connection"))
      
      # Before testing connection, make sure fields are filled
      db_checks <- c("main_db_name" = FALSE, "public_db_name" = FALSE, "host" = FALSE, "port" = FALSE, "user" = FALSE, "password" = FALSE)
      
      for(name in names(db_checks)){
        shiny.fluent::updateTextField.shinyInput(session, name, errorMessage = NULL)
        if (!is.null(input[[name]])){
          if (name != "port" & input[[name]] != "") db_checks[[name]] <- TRUE
          if (name == "port" & input[[name]] != "" & grepl("^[0-9]+$", input[[name]])) db_checks[[name]] <- TRUE
        }
      }
      
      # Reset output textfields
      output$test_connection_main_db <- renderText("")
      output$test_connection_public_db <- renderText("")
      
      sapply(names(db_checks), function(name) if (!db_checks[[name]]) shiny.fluent::updateTextField.shinyInput(session, name, errorMessage = i18n$t(paste0("provide_valid_", name))))
      
      req(db_checks[["main_db_name"]], db_checks[["public_db_name"]], db_checks[["host"]], db_checks[["port"]], db_checks[["user"]], db_checks[["password"]])
      
      # If checks are OK, test connection
      for(db_type in c("main_db", "public_db")){
        
        result <- capture.output(
          tryCatch(
            DBI::dbConnect(
              RPostgres::Postgres(), dbname = input[[paste0(db_type, "_name")]],
              host = input$host, port = input$host, user = input$user, password = input$password
            ),
            error = function(e) print(e), warning = function(w) print(w)
          )
        ) %>% toString()
        
        result_ui <- tags$style(i18n$t("success"), style = "color: #2E86C4")
        if (grepl("exception|error|warning|fatal", tolower(result))) result_ui <- tags$span(result, style = "color: red;")
        
        output[[paste0(db_type, "_result")]] <- renderUI(result_ui)
      }
    })
    
    ## Save connection settings ----
    
    # When save button is clicked
    
    observeEvent(input$save_app_db_settings, {
      if (debug) cat(paste0("\n", now(), " - mod_app_db - observer input$save_app_db_settings"))
      
      # If connection_type is local, save only connection_type but do not erase other informations (remote DB informations)
      # If connection_type is remote, save connection_type and other remote DB informations
      
      sql <- glue::glue_sql("UPDATE options SET value = {input$connection_type} WHERE category = 'remote_db' AND name = 'connection_type'", .con = r$local_db)
      sql_send_statement(r$local_db, sql)
      
      if (input$connection_type == "remote"){
        
        # Make sure fields are not empty
        # Password is not required
        required_textfields <- c("sql_lib", "main_db_name", "public_db_name", "host", "port", "user")
        textfields_checks <- TRUE
        
        for (textfield in required_textfields){
          
          if (is.na(input[[textfield]]) | input[[textfield]] == "") shiny.fluent::updateTextField.shinyInput(session, textfield, errorMessage = i18n$t(paste0("provide_valid_", textfield)))
          else shiny.fluent::updateTextField.shinyInput(session, textfield, errorMessage = NULL)
          if(is.na(input[[textfield]]) | input[[textfield]] == "") textfields_checks <- FALSE
        }
        
        req(textfields_checks)
        
        for(name in c(required_textfields, "connection_type", "password")){
          sql <- glue::glue_sql("UPDATE options SET value = {as.character(input[[name]])}, creator_id = {r$user_id}, datetime = {now()} WHERE category = 'remote_db' AND name = {name}", .con = r$local_db)
          sql_send_statement(r$local_db, sql)
        }
      }
      
      show_message_bar(output, "modif_saved", "success", i18n = i18n, ns = ns)
      shinyjs::delay(2000, show_message_bar(output,  "reload_app_to_take_into_account_changes", "warning", i18n = i18n, ns = ns))
    })
    
    # Run SQL code ----
    
    observeEvent(input$run_code, {
      if (debug) cat(paste0("\n", now(), " - mod_app_db - observer input$run_code"))
      
      r$sql_code <- input$sql_code
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_trigger', Math.random());"))
    })
    
    observeEvent(input$sql_code_run_selection, {
      if (debug) cat(paste0("\n", now(), " - mod_app_db - observer input$sql_code_run_selection"))
      
      if(!shinyAce::is.empty(input$sql_code_run_selection$selection)) r$sql_code <- input$sql_code_run_selection$selection
      else r$sql_code <- input$sql_code_run_selection$line
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_trigger', Math.random());"))
    })
    
    observeEvent(input$sql_code_run_all, {
      if (debug) cat(paste0("\n", now(), " - mod_app_db - observer input$sql_code_run_all"))
      
      r$sql_code <- input$sql_code
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_trigger', Math.random());"))
    })
    
    observeEvent(input$run_code_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_app_db - observer input$run_code_trigger"))
      
      # Capture console output of our code

      code <- r$sql_code  %>% stringr::str_replace_all("\r", "\n")
      
      if (input$run_sql_code_db == "main") con <- r$db else con <- m$db
      
      if (!grepl("^select", tolower(code))) captured_output <- capture.output(tryCatch({
        DBI::dbSendStatement(con, code) -> query
        print(query)
        DBI::dbClearResult(query)
      }, error = function(e) print(e), warning = function(w) print(w)))
        
      else captured_output <- capture.output(tryCatch(DBI::dbGetQuery(con, code) %>% tibble::as_tibble() %>% print(n = 1000), error = function(e) print(e), warning = function(w) print(w))) %>% paste(collapse = "\n")
        
      captured_output <- captured_output %>% paste(collapse = "\n")
      
      # Display result
      output$datetime_code_execution <- renderText(format_datetime(now(), language))
      output$sql_result <- renderText(captured_output)
    })
    
    # Backups ----
    
    ## Check / uncheck tables
    
    sapply(c("export", "import"), function(category){
      sapply(c("check", "uncheck"), function(action){
        sapply(c("main", "public"), function(type){
          print(paste0(category, "_db_", action, "_all_", type, "_tables"))
          observeEvent(input[[paste0(category, "_db_", action, "_all_", type, "_tables")]], {
            if (debug) cat(paste0("\n", now(), " - mod_app_db - observer input$", category, "_db_", action, "_all_", type, "_tables"))
            
            value <- NULL
            
            if (action == "check"){
              if (category == "export") value <- db_col_types %>% dplyr::filter(db == type) %>% dplyr::pull(table)
              else if (category == "import") if (length(r$import_db_files) > 0) value <- r$import_db_files %>% dplyr::filter(prefix == type) %>% dplyr::pull(name)
            }
            
            if (category == "export") dropdown_options <- db_col_types %>% dplyr::filter(db == type) %>% convert_tibble_to_list(key_col = "table", text_col = "table")
            else if (category == "import") dropdown_options <- convert_tibble_to_list(r$import_db_files %>% dplyr::filter(prefix == type), key_col = "name", text_col = "name")
            
            shiny.fluent::updateDropdown.shinyInput(session, paste0(type, "_tables_to_", category), options = dropdown_options, value = value)
          })
        })
      })
    })
    
    ## Download db ----
    
    ### Export button is clicked
    
    observeEvent(input$export_db, {
      if (debug) cat(paste0("\n", now(), " - mod_app_db - observer input$export_db"))
      
      req(length(input$main_tables_to_export) > 0 | length(input$public_tables_to_export))
      
      last_save <- DBI::dbGetQuery(r$db, "SELECT * FROM options WHERE category = 'last_db_save' AND name = 'last_db_save'")
      
      if (nrow(last_save) == 0){
        
        # Insert last time row
        last_row <- get_last_row(r$db, "options")
        new_data <- tibble::tribble(
          id = as.integer(last_row + 1), category = "last_db_save", link_id = NA_integer_, name = "last_db_save", value = now(),
          value_num = NA_real_, creator_id = r$user_id, datetime = now(), deleted = FALSE
        )
        DBI::dbAppendTable(r$db, "options", new_data)
      }
      
      else {
        sql <- glue::glue_sql("UPDATE options SET value = {now()}, datetime = {now()} WHERE category = 'last_db_save' AND name = 'last_db_save'", .con = r$db)
        sql_send_statement(r$db, sql)
      }
      
      shinyjs::click("db_save")
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_last_db_save', Math.random());"))
    })
    
    ### Download zip file
    
    output$db_save <- downloadHandler(
      filename = function() paste0("linkr_db_backup_", now() %>% as.character() %>% stringr::str_replace_all(" |:|-", "_"), ".zip"),
      
      content = function(file){
        
        if (debug) cat(paste0("\n", now(), " - mod_app_db - output$db_save"))
        
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        files <- NULL
        
        db_list <- c("main", "public")
        
        for (db in db_list){
          
          tables <- input[[paste0(db, "_tables_to_export")]]
          
          if (db == "main") con <- r$db
          if (db == "public") con <- m$db
          
          if (length(tables) > 0){
            for (table in tables){
              file_name <- paste0(db, "_", table, ".csv")
              readr::write_csv(DBI::dbGetQuery(con, paste0("SELECT * FROM ", table)), file_name)
              files <- c(file_name, files)
            }
          }
        }
        
        # XML file for app version
        
        xml <- XML::newXMLDoc()
        db_node <- XML::newXMLNode("db", doc = xml)
        XML::newXMLNode("app_version", r$app_version, parent = db_node)
        XML::saveXML(xml, file = "db_info.xml")
        files <- c("db_info.xml", files)
        
        zip::zipr(file, files, include_directories = FALSE)
      }
    )
    
    ### Update last db save field
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_last_db_save', Math.random());"))
    
    observeEvent(input$reload_last_db_save, {
      if (debug) cat(paste0("\n", now(), " - mod_app_db - observer input$reload_last_db_save"))
      
      last_save <- DBI::dbGetQuery(r$db, "SELECT * FROM options WHERE category = 'last_db_save' AND name = 'last_db_save'")
      
      if (nrow(last_save) > 0) last_save_datetime <- format_datetime(last_save %>% dplyr::pull(value), language = "fr", sec = FALSE)
      else last_save_datetime <- "/"
      
      output$last_db_save <- renderUI(tagList(strong(i18n$t("last_db_save")), " : ", last_save_datetime))
      
    })
    
    ## Restore db ----
    
    ### Browse file
    
    observeEvent(input$db_restore_browse, {
      if (debug) cat(paste0("\n", now(), " - mod_app_db - observer input$db_restore_browse"))
      shinyjs::click("db_restore")
    })
    
    ### Update last db save field
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_last_db_restore', Math.random());"))
    
    observeEvent(input$reload_last_db_restore, {
      if (debug) cat(paste0("\n", now(), " - mod_app_db - observer input$reload_last_db_restore"))
      
      last_restore <- DBI::dbGetQuery(r$db, "SELECT * FROM options WHERE category = 'last_db_restore' AND name = 'last_db_restore'")
      
      if (nrow(last_restore) > 0) last_restore_datetime <- format_datetime(last_restore %>% dplyr::pull(value), language = "fr", sec = FALSE)
      else last_restore_datetime <- "/"
      
      output$last_db_restore <- renderUI(tagList(strong(i18n$t("last_db_restore")), " : ", last_restore_datetime))
    })
    
    ### Restore file
    
    output$db_restore_loaded_file <- renderUI({
      if (debug) cat(paste0("\n", now(), " - mod_app_db - output$db_restore_loaded_file"))
      
      req(input$db_restore$name)
      
      div(input$db_restore$name, class = "selected_file db_backup_file")
    })
    
    observeEvent(input$db_restore, {
      if (debug) cat(paste0("\n", now(), " - mod_app_db - input$db_restore"))
      
      shinyjs::show("import_db_tables_div")
      
      r$import_db_files <- tibble::tibble()
      
      tryCatch({
        
        exdir <- paste0(r$app_folder, "/temp_files/", r$user_id, "/app_db/import_db_", now() %>% stringr::str_replace_all(":| |-", "_"))
        dir.create(exdir)
        
        zip::unzip(input$db_restore$datapath, exdir = exdir)
        
        files <- list.files(exdir)
        
        csv_files <- files[grepl("\\.csv$", files, ignore.case = TRUE)]
        prefix <- ifelse(grepl("^(main|public)_", csv_files, ignore.case = TRUE), sub("_.*", "", csv_files), NA)
        
        names_without_extension <- sub("\\.csv$", "", csv_files, ignore.case = TRUE)
        names_without_extension <- sub("^[^_]*_", "", names_without_extension)
        
        full_path <- file.path(exdir, csv_files)
        
        r$import_db_files <- data.frame(prefix = prefix, name = names_without_extension, path = full_path, stringsAsFactors = FALSE)
        
        for (type in c("main", "public")) shiny.fluent::updateDropdown.shinyInput(
          session, paste0(type, "_tables_to_import"),
          options = convert_tibble_to_list(r$import_db_files %>% dplyr::filter(prefix == type), key_col = "name", text_col = "name"),
          value = r$import_db_files %>% dplyr::filter(prefix == type) %>% dplyr::pull(name)
        )
      },
      error = function(e){
        show_message_bar(output, "error_loading_db_file", "warning", i18n = i18n, ns = ns)
        cat(paste0("\n", now(), " - mod_git_repos - error loading db file - error = ", toString(e)))
      })
    })
    
    observeEvent(input$import_db, {
      if (debug) cat(paste0("\n", now(), " - mod_app_db - input$import_db"))
      
      # Save current database before import new one
      
      db_list <- c("main", "public")
      
      db_folder <- paste0(r$app_folder, "/temp_files/", r$user_id, "/app_db/import_db_rescue_", now() %>% stringr::str_replace_all(":| |-", "_"))
      dir.create(db_folder)
      
      for (db in db_list){
        
        if (db == "main") con <- r$db
        if (db == "public") con <- m$db
        
        tables <- db_col_types %>% dplyr::filter(db == !!db) %>% dplyr::pull(table)
        
        for (table in tables){
          file_name <- paste0(db_folder, "/", db, "_", table, ".csv")
          readr::write_csv(DBI::dbGetQuery(con, paste0("SELECT * FROM ", table)), file_name)
        }
      }
      
      # Try to import restore files
      # Reload current database in case of error
      
      total_success <- TRUE
      
      for(i in 1:nrow(r$import_db_files)){
        
        row <- r$import_db_files[i, ]
        if (row$prefix == "main") con <- r$db
        else if (row$prefix == "public") con <- m$db
        
        if (row$name %in% input[[paste0(row$prefix, "_tables_to_import")]]){
          
          col_types <- db_col_types %>% dplyr::filter(table == row$name, db == row$prefix) %>% dplyr::pull(col_types)
          
          success <- FALSE
          
          tryCatch({
            
            # Load CSV file
            data <- vroom::vroom(row$path, col_types = col_types, progress = FALSE)
              
            # Delete data from old table
            sql <- glue::glue_sql("DELETE FROM {`row$name`}", .con = con)
            sql_send_statement(con, sql)

            # Insert new data in table
            DBI::dbAppendTable(con, row$name, data)
            
            success <- TRUE

          }, error = function(e){
            show_message_bar(output, "error_restoring_database_table", "warning", i18n = i18n, ns = ns)
            cat(paste0("\n", now(), " - mod_git_repos - error restoring database table (", row$name, ") - error = ", toString(e)))
          })
          
          if (!success) total_success <- FALSE
        }
      }
      
      # Restore database in case of failure
      
      if (!total_success){
        
        print("Failure - reimport tables")
        
        tryCatch({
          
          for (db in db_list){
            
            if (db == "main") con <- r$db
            if (db == "public") con <- m$db
            
            tables <- db_col_types %>% dplyr::filter(db == !!db) %>% dplyr::pull(table)
            
            for (table in tables){
              
              file_name <- paste0(db_folder, "/", db, "_", table, ".csv")
              col_types <- db_col_types %>% dplyr::filter(table == !!table, db == !!db) %>% dplyr::pull(col_types)
              
              data <- vroom::vroom(file_name, col_types = col_types, progress = FALSE)
              
              # Delete data from old table
              sql <- glue::glue_sql("DELETE FROM {`table`}", .con = con)
              sql_send_statement(con, sql)
              
              # Insert new data in table
              DBI::dbAppendTable(con, table, data)
            }
          }
          
        }, error = function(e){
          show_message_bar(output, "error_restoring_db_old_table_after_import_failure", "warning", i18n = i18n, ns = ns)
          cat(paste0("\n", now(), " - mod_git_repos - error restoring old database table after import failure (", row$name, ") - error = ", toString(e)))
        })
      }
      
      if (total_success) show_message_bar(output, "db_restored_reload_app_to_take_into_account_changes", "success", i18n = i18n, ns = ns) 
      
    })
    
  })
}
