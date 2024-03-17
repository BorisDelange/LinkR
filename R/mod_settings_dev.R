#' settings_r_console UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_dev_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  
  cards <- c("dev_edit_r_code_card", "dev_edit_python_code_card", "dev_perf_monitoring_card", "dev_to_do_list_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  })
  
  div(class = "main",
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "r_console", text = i18n$t("dev"))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "dev_edit_r_code_card", itemKey = "dev_edit_r_code_card", headerText = i18n$t("r_console")),
      shiny.fluent::PivotItem(id = "dev_edit_python_code_card", itemKey = "dev_edit_python_code_card", headerText = i18n$t("python_console")),
      shiny.fluent::PivotItem(id = "dev_perf_monitoring_card", itemKey = "dev_perf_monitoring_card", headerText = i18n$t("perf_monitoring"))
    ),
    forbidden_cards,
    
    shinyjs::hidden(
      div(id = ns("dev_edit_r_code_card"),
        make_shiny_ace_card(i18n$t("r_console"),
          div(
            div(
              shinyAce::aceEditor(
                outputId = ns("r_code"), value = "", mode = "r",
                code_hotkeys = list("r", list(
                    run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
                    run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
                    comment = list(win = "CTRL-SHIFT-C", mac = "CTRL-SHIFT-C|CMD-SHIFT-C")
                  )
                ),
                wordWrap = TRUE, debounce = 10,
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000
              ),
              style = "width: 100%;"
            ),
            shiny.fluent::PrimaryButton.shinyInput(ns("execute_r_code"), i18n$t("run_code")), br(),
            div(textOutput(ns("datetime_r_code_execution")), style = "color:#878787;"), br(),
            div(uiOutput(ns("r_code_result")),
              style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
          )
        )
      )
    ),
    
    shinyjs::hidden(
      div(id = ns("dev_edit_python_code_card"),
        make_shiny_ace_card(i18n$t("python_console"),
          div(
            div(
              shinyAce::aceEditor(
                outputId = ns("python_code"), value = "", mode = "r",
                code_hotkeys = list("r", list(
                  run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
                  run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
                  comment = list(win = "CTRL-SHIFT-C", mac = "CTRL-SHIFT-C|CMD-SHIFT-C")
                )
                ),
                wordWrap = TRUE, debounce = 10,
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000
              ),
              style = "width: 100%;"
            ),
            shiny.fluent::PrimaryButton.shinyInput(ns("execute_python_code"), i18n$t("run_code")), br(),
            div(textOutput(ns("datetime_python_code_execution")), style = "color:#878787;"), br(),
            div(uiOutput(ns("python_code_result")),
              style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
          )
        )
      )
    ),
    
    shinyjs::hidden(
      div(id = ns("dev_perf_monitoring_card"),
        make_card(i18n$t("perf_monitoring"),
          div(
            br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::DefaultButton.shinyInput(ns("show_datatable"), i18n$t("show_data")),
              shiny.fluent::DefaultButton.shinyInput(ns("reset_perf_monitoring"), i18n$t("reset"))
            ),
            DT::DTOutput(ns("perf_monitoring_datatable"))
          )
        )
      )
    ), br()
  )
}
    
#' settings_r_console Server Functions
#'
#' @noRd 

mod_settings_dev_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), 
  i18n = character(), language = "en", perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("dev_edit_r_code_card", "dev_edit_python_code_card", "dev_perf_monitoring_card", "dev_to_do_list_card")
    show_or_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    if ("dev_edit_r_code_card" %in% r$user_accesses) shinyjs::show("dev_edit_r_code_card")
    else shinyjs::show("dev_edit_r_code_card_forbidden")
    
    # observe({
    #   shiny.router::get_query_param()
    # 
    #   if ("r_console_edit_code_card" %in% r$user_accesses){
    #     shinyjs::show("dev_edit_r_code_card")
    #     shinyjs::hide("dev_edit_r_code_card_forbidden")
    #   }
    #   else {
    #     shinyjs::show("dev_edit_r_code_card_forbidden")
    #     shinyjs::hide("dev_edit_r_code_card")
    #   }
    # })
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    observeEvent(input$help, if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r$help_settings_dev_open_panel <- TRUE)
    observeEvent(input$hide_panel, r$help_settings_dev_open_panel <- FALSE)

    r$help_settings_dev_open_panel_light_dismiss <- TRUE
    observeEvent(input$show_modal, r$help_settings_dev_open_modal <- TRUE)
    observeEvent(input$hide_modal, {
      r$help_settings_dev_open_modal <- FALSE
      r$help_settings_dev_open_panel_light_dismiss <- TRUE
    })

    # observeEvent(shiny.router::get_page(), {
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_dev - ", id, " - observer shiny_router::change_page"))
    # 
    #   # Close help pages when page changes
    #   r$help_settings_dev_open_panel <- FALSE
    #   r$help_settings_dev_open_modal <- FALSE
    # })

    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_settings_dev_page_", i)]] <- now())
    })

    help_settings_dev(output = output, r = r, id = id, language = language, i18n = i18n, ns = ns)
    
    observeEvent(input$copy_code_1, r$help_settings_dev_copy_code_1 <- now())
    observeEvent(input$copy_code_2, r$help_settings_dev_copy_code_2 <- now())
  
    # --- --- --- --- ---
    # Execute R code ----
    # --- --- --- --- ---
    
    observeEvent(input$execute_r_code, {
      
      if (debug) cat(paste0("\n", now(), " - mod_settings_dev - observer input$execute_r_code"))
      
      r$r_console_code <- input$r_code
      r$r_console_code_trigger <- now()
    })
    
    observeEvent(input$r_code_run_selection, {
      
      if (debug) cat(paste0("\n", now(), " - mod_settings_dev - observer input$r_code_run_selection"))
      
      if(!shinyAce::is.empty(input$r_code_run_selection$selection)) r$r_console_code <- input$r_code_run_selection$selection
      else r$r_console_code <- input$r_code_run_selection$line
      r$r_console_code_trigger <- now()
    })

    observeEvent(input$r_code_run_all, {
      
      if (debug) cat(paste0("\n", now(), " - mod_settings_dev - observer input$r_code_run_all"))
      
      r$r_console_code <- input$r_code
      r$r_console_code_trigger <- now()
    })

    observeEvent(r$r_console_code_trigger, {
      
      if (debug) cat(paste0("\n", now(), " - mod_settings_dev - observer r$r_console_code_trigger"))
      
      if ("dev_edit_r_code_card" %in% r$user_accesses){
        edited_code <- r$r_console_code %>% stringr::str_replace_all("\r", "\n")
        
        console_result <- isolate(execute_settings_code(input = input, output = output, session = session, id = id, ns = ns, i18n = i18n, r = r, d = d, m = m,
          edited_code = edited_code, code_type = "server")) %>%
          stringr::str_replace_all(paste0("<(lgl|int|dbl|chr|cpl|raw|list|named list|fct|ord|date|dttm|drtn|time",
            "|int64|blob|df\\[\\,1\\]|tibble\\[\\,1\\]|I|\\?\\?\\?|vctrs_vc|prtl_fctr|prtl|fn|sym|expression|quos)>"), 
            function(x) sprintf("&lt;%s&gt;", substr(x, 2, nchar(x) - 1)))
        
        if (console_result != "NULL") console_result <- console_result %>% gsub("NULL$", "", .)
        
        # If there's a <simpleWarning ...> message, it is interpreted as an HTML tag and doesn't show the console result
        if (!grepl("simpleWarning|simpleError", console_result)) console_result <- HTML(paste0("<pre>", console_result, "</pre>"))
        else console_result <- tags$div(console_result, style = "padding: 15px 0px 15px 0px;")
        
        output$r_code_result <- renderUI(console_result)
        output$datetime_r_code_execution <- renderText(format_datetime(now(), language))
      }
    })
    
    # Comment text
    observeEvent(input$r_code_comment, {
      if (debug) cat(paste0("\n", now(), " - mod_settings_dev - observer input$r_code_comment"))
      
      lines <- strsplit(input$r_code, "\n")[[1]]
      req(length(lines) > 0)
      
      start_row <- input$r_code_comment$range$start$row + 1
      end_row <- input$r_code_comment$range$end$row + 1
      
      for (i in start_row:end_row) if (startsWith(lines[i], "# ")) lines[i] <- substr(lines[i], 3, nchar(lines[i])) else lines[i] <- paste0("# ", lines[i])
      
      shinyAce::updateAceEditor(session, "r_code", value = paste0(lines, collapse = "\n"))
      
      shinyjs::runjs(sprintf("
        var editor = ace.edit('%s-r_code');
        editor.moveCursorTo(%d, %d);
        editor.focus();
          ", id, input$r_code_comment$range$end$row, input$r_code_comment$range$end$column))
    })
    
    # --- --- --- --- --- -- -
    # Execute Python code ----
    # --- --- --- --- --- -- -
    
    observeEvent(r$python_path_trigger, {
      sql <- glue::glue_sql("SELECT * FROM options WHERE category = 'python_config' AND name = 'python_path'")
      python_path <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(value)
      
      tryCatch({
        reticulate::use_python(python_path)
      }, error = function(e) "")
    })
    
    observeEvent(input$execute_python_code, {
      
      if (debug) cat(paste0("\n", now(), " - mod_settings_dev - observer input$execute_python_code"))
      
      r$python_console_code <- input$python_code
      r$python_console_code_trigger <- now()
    })
    
    observeEvent(input$python_code_run_selection, {
      
      if (debug) cat(paste0("\n", now(), " - mod_settings_dev - observer input$python_code_run_selection"))
      
      if(!shinyAce::is.empty(input$python_code_run_selection$selection)) r$python_console_code <- input$python_code_run_selection$selection
      else r$python_console_code <- input$python_code_run_selection$line
      r$python_console_code_trigger <- now()
    })
    
    observeEvent(input$python_code_run_all, {
      
      if (debug) cat(paste0("\n", now(), " - mod_settings_dev - observer input$python_code_run_all"))
      
      r$python_console_code <- input$python_code
      r$python_console_code_trigger <- now()
    })
    
    observeEvent(r$python_console_code_trigger, {
      
      if (debug) cat(paste0("\n", now(), " - mod_settings_dev - observer r$python_console_code_trigger"))
      
      if ("dev_edit_python_code_card" %in% r$user_accesses){
        edited_code <- r$python_console_code %>% stringr::str_replace_all("\r", "\n")
        
        console_result <- capture_python_output(edited_code)
        
        output$python_code_result <- renderUI(HTML(paste0("<pre>", console_result, "</pre>")))
        output$datetime_python_code_execution <- renderText(format_datetime(now(), language))
      }
    })
    
    # Comment text
    observeEvent(input$python_code_comment, {
      if (debug) cat(paste0("\n", now(), " - mod_settings_dev - observer input$python_code_comment"))
      
      lines <- strsplit(input$python_code, "\n")[[1]]
      req(length(lines) > 0)
      
      start_row <- input$python_code_comment$range$start$row + 1
      end_row <- input$python_code_comment$range$end$row + 1
      
      for (i in start_row:end_row) if (startsWith(lines[i], "# ")) lines[i] <- substr(lines[i], 3, nchar(lines[i])) else lines[i] <- paste0("# ", lines[i])
      
      shinyAce::updateAceEditor(session, "python_code", value = paste0(lines, collapse = "\n"))
      
      shinyjs::runjs(sprintf("
        var editor = ace.edit('%s-python_code');
        editor.moveCursorTo(%d, %d);
        editor.focus();
          ", id, input$python_code_comment$range$end$row, input$python_code_comment$range$end$column))
    })
    
    # --- --- --- --- -- -
    # Perf monitoring ----
    # --- --- --- --- -- -
    
    searchable_cols <- c("task")
    column_widths <- c("elapsed_time" = "150px", "datetime_start" = "150px", "datetime_stop" = "150px")
    sortable_cols <- c("elapsed_time", "datetime_start", "datetime_stop", "task")
    centered_cols <- c("elapsed_time", "datetime_start", "datetime_stop")
    col_names <- get_col_names(table_name = "perf_monitoring", i18n = i18n)
    
    observeEvent(input$show_datatable, {
      
      if (debug) cat(paste0("\n", now(), " - mod_settings_dev - observer input$show_datatable"))
      
      if (nrow(r$perf_monitoring_table) > 0) perf_monitoring_table <- r$perf_monitoring_table %>%
        dplyr::mutate(elapsed_time = round(datetime_stop - datetime_start, 2), .before = "task") %>%
        dplyr::mutate_at(c("datetime_start", "datetime_stop"), as.character) %>%
        dplyr::mutate(row_num = 1:dplyr::n()) %>%
        dplyr::arrange(dplyr::desc(row_num)) %>%
        dplyr::select(-row_num)
      
      if (nrow(r$perf_monitoring_table) == 0) perf_monitoring_table <- r$perf_monitoring_table %>% dplyr::mutate(elapsed_time = NA_real_, .before = "task")
      
      # Render datatable
      if (length(r$perf_monitoring_datatable_proxy) == 0){
        
        render_datatable(output = output, ns = ns, i18n = i18n, data = perf_monitoring_table,
          output_name = "perf_monitoring_datatable", col_names = col_names,
          sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, page_length = 100)
        
        # Create a proxy for datatatable
        r$perf_monitoring_datatable_proxy <- DT::dataTableProxy("perf_monitoring_datatable", deferUntilFlush = FALSE)
      }
      
      if (length(r$perf_monitoring_datatable_proxy) > 0) DT::replaceData(r$perf_monitoring_datatable_proxy, 
        perf_monitoring_table, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Reset perf monitoring
    
    observeEvent(input$reset_perf_monitoring, {
      
      if (debug) cat(paste0("\n", now(), " - mod_settings_dev - observer input$reset_perf_monitoring"))
      
      r$perf_monitoring_table <- r$perf_monitoring_table %>% dplyr::slice(0)
    })
  })
}
