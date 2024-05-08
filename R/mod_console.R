#' @noRd 
mod_console_ui <- function(id = character(), language = "en", languages = tibble::tibble(), i18n = character()){
  ns <- NS(id)
  
  code_hotkeys <- list(
    run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
    run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
    comment = list(win = "CTRL-SHIFT-C", mac = "CTRL-SHIFT-C|CMD-SHIFT-C")
  )
  
  div(
    id = ns("console"),
    div(
      shinyAce::aceEditor(
        ns("code"), value = "", mode = "r",
        code_hotkeys = list("r", code_hotkeys),
        autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, fontSize = 10, showPrintMargin = FALSE
      ),
      style = "width: 50%; max-height: calc(100% - 20px);"
    ),
    div(
      verbatimTextOutput(ns("console_code_result")),
      style = "width: 50%; border: dashed grey 1px; margin: 10px 0px 0px 10px; padding: 0px 10px; font-size: 12px; overflow-y: auto;"
    ),
    style = "display: flex; max-height:calc(100vh - 100px);",
    class = "main"
  )
}

#' @noRd 
mod_console_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), language = "en", i18n = character(), debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    r$load_console <- now()
    
    # Unlock reactivity
    observeEvent(shiny.router::get_page(), {
      req(shiny.router::get_page() == "console")
      if (debug) cat(paste0("\n", now(), " - mod_console - observer shiny.router::get_page()"))
      
      shinyjs::show("console")
      
      # Prevent a bug with scroll into ace editor
      shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
    })
    
    # Comment code ----
    observeEvent(input$code_comment, {
      if (debug) cat(paste0("\n", now(), " - mod_console - observer input$code_comment"))

      lines <- strsplit(input$code, "\n")[[1]]
      req(length(lines) > 0)

      start_row <- input$code_comment$range$start$row + 1
      end_row <- input$code_comment$range$end$row + 1

      for (i in start_row:end_row) if (startsWith(lines[i], "# ")) lines[i] <- substr(lines[i], 3, nchar(lines[i])) else lines[i] <- paste0("# ", lines[i])

      shinyAce::updateAceEditor(session, "code", value = paste0(lines, collapse = "\n"))

      shinyjs::runjs(sprintf("
        var editor = ace.edit('%s-rode');
        editor.moveCursorTo(%d, %d);
        editor.focus();
      ", id, input$code_comment$range$end$row, input$code_comment$range$end$column))
    })
    
    # Execute code ----
    observeEvent(input$execute_code, {

      if (debug) cat(paste0("\n", now(), " - mod_console - observer input$execute_code"))

      r$console_code <- input$code
      r$console_code_trigger <- now()
    })

    observeEvent(input$code_run_selection, {

      if (debug) cat(paste0("\n", now(), " - mod_console - observer input$code_run_selection"))

      if(!shinyAce::is.empty(input$code_run_selection$selection)) r$console_code <- input$code_run_selection$selection
      else r$console_code <- input$code_run_selection$line
      r$console_code_trigger <- now()
    })

    observeEvent(input$code_run_all, {

      if (debug) cat(paste0("\n", now(), " - mod_console - observer input$code_run_all"))

      r$console_code <- input$code
      r$console_code_trigger <- now()
    })
    
    # observeEvent(r$python_path_trigger, {
    #   sql <- glue::glue_sql("SELECT * FROM options WHERE category = 'python_config' AND name = 'python_path'")
    #   python_path <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(value)
    #   
    #   tryCatch({
    #     reticulate::use_python(python_path)
    #   }, error = function(e) "")
    # })
    # 
    observeEvent(r$console_code_trigger, {
    
      if (debug) cat(paste0("\n", now(), " - mod_console - observer r$console_code_trigger"))
      
      code <- r$console_code %>% stringr::str_replace_all("\r", "\n")
      
      # console_result <- ""
      #   
      #   if (input$console_language == "r"){
      #     if ("dev_edit_r_code_card" %in% r$user_accesses){
      # console_result <- isolate(execute_settings_code(input = input, output = output, session = session, id = id, ns = ns, i18n = i18n, r = r, d = d, m = m,
      #   edited_code = edited_code, code_type = "server")) %>%
      #   stringr::str_replace_all(paste0("<(lgl|int|dbl|chr|cpl|raw|list|named list|fct|ord|date|dttm|drtn|time",
      #     "|int64|blob|df\\[\\,1\\]|tibble\\[\\,1\\]|I|\\?\\?\\?|vctrs_vc|prtl_fctr|prtl|fn|sym|expression|quos)>"),
      #    function(x) sprintf("&lt;%s&gt;", substr(x, 2, nchar(x) - 1)))
      # 
      # Change this option to display correctly tibble in textbox
      options('cli.num_colors' = 1)
      
      # Capture console output of our code
      captured_output <- 
        capture.output(tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w))) %>%
        paste(collapse = "\n")
      
      # Restore normal value
      options('cli.num_colors' = NULL)
      
      # Display result
      # if (grepl("# A tibble:", toString(captured_output))) captured_output <- paste(captured_output, collapse = "\n")
      # else captured_output <- paste(strwrap(captured_output, width = 150), collapse = "\n")
      
      # if (captured_output != "NULL") captured_output <- captured_output %>% gsub("NULL$", "", .)

      # If there's a <simpleWarning ...> message, it is interpreted as an HTML tag and doesn't show the console result
      # if (!grepl("simpleWarning|simpleError|Rcpp::", console_result)) console_result <- HTML(paste0("<pre>", console_result, "</pre>"))
      # else console_result <- tags$div(console_result, style = "padding: 15px 0px 15px 0px;")
        # }
      # }
    # 
    #   else if (input$console_language == "python"){
    #     if ("dev_edit_python_code_card" %in% r$user_accesses){
    #       console_result <- capture_python_output(edited_code)
    # 
    #       console_result <- HTML(paste0("<pre>", console_result, "</pre>"))
    #     }
    #   }
    # 
    #   else if (input$console_language == "terminal"){
    #     if ("dev_edit_terminal_code_card" %in% r$user_accesses){
    #       console_result <- capture.output(
    #         tryCatch(eval(parse(text = paste0("system('", edited_code, "', intern = TRUE)"))), error = function(e) print(e), warning = function(w) print(w)))
    #       
    #       console_result <- paste(strwrap(console_result, width = 150), collapse = "\n")
    #       console_result <- HTML(paste0("<pre>", console_result, "</pre>"))
    #     }
    #   }
    # 
      
      output$console_code_result <- renderText(captured_output)
    #   output$datetime_code_execution <- renderText(format_datetime(now(), language))
    })
  })
}