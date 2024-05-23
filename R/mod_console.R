#' @noRd 
mod_console_ui <- function(id = character(), language = "en", languages = tibble::tibble(), i18n = character()){
  ns <- NS(id)
  
  code_hotkeys <- list(
    save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
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
        autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, fontSize = 11, showPrintMargin = FALSE
      ),
      style = "width: 50%; max-height: calc(100% - 22px);"
    ),
    div(
      textOutput(ns("datetime_code_execution")),
      verbatimTextOutput(ns("code_output")),
      shinyjs::hidden(div(id = ns("plot_output_div"), plotOutput(ns("plot_output")), style = "padding-top: 10px;")),
      shinyjs::hidden(uiOutput(ns("ui_output"))),
      shinyjs::hidden(div(id = ns("table_output_div"), tableOutput(ns("table_output")), style = "padding-top: 10px;")),
      shinyjs::hidden(div(id = ns("datatable_output_div"), DT::DTOutput(ns("datatable_output")), style = "padding-top: 10px;")),
      shinyjs::hidden(imageOutput(ns("image_output"))),
      style = "width: 50%; border: dashed grey 1px; margin: 10px 0px 0px 10px; padding: 10px; font-size: 12px; overflow-y: auto;"
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
    
    # Update output dropdown ----
    observeEvent(input$programming_language, {
      if (debug) cat(paste0("\n", now(), " - mod_console - observer input$programming_language"))
      
      req(input$output)
      
      if (input$programming_language == "r"){
        output_options <- list(
          list(key = "console", text = i18n$t("console")),
          list(key = "figure", text = i18n$t("figure")),
          list(key = "table", text = i18n$t("table_output")),
          list(key = "datatable", text = i18n$t("datatable_output")),
          list(key = "rmarkdown", text = i18n$t("rmarkdown"))
        )
        output_value <- "console"
        
        if (input$output == "markdown") ace_mode <- "markdown"
        else ace_mode <- "r"
      }
      else if (input$programming_language == "python"){
        output_options <- list(
          list(key = "console", text = i18n$t("console")),
          list(key = "matplotlib", text = i18n$t("matplotlib"))
        )
        output_value <- "console"
        
        ace_mode <- "python"
      }
      else if (input$programming_language == "shell"){
        output_options <- list(
          list(key = "terminal", text = i18n$t("terminal"))
        )
        output_value <- "terminal"
        
        ace_mode <- "sh"
      }
      
      # Update output dropdown options
      shiny.fluent::updateDropdown.shinyInput(session, "output", options = output_options, value = output_value)
      
      # Update ace editor mode
      shinyAce::updateAceEditor(session, "code", mode = ace_mode)
    })
    
    # Show / hide plot size buttons ----
    observeEvent(input$output, {
      if (debug) cat(paste0("\n", now(), " - mod_console - observer input$output"))
      
      if (input$output %in% c("figure", "matplotlib")){
        shinyjs::show("plot_size_div")
      }
      else shinyjs::hide("plot_size_div")
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
      r$console_code_trigger <- runif(1)
    })

    observeEvent(input$code_run_selection, {
      if (debug) cat(paste0("\n", now(), " - mod_console - observer input$code_run_selection"))

      if(!shinyAce::is.empty(input$code_run_selection$selection)) r$console_code <- input$code_run_selection$selection
      else r$console_code <- input$code_run_selection$line
      r$console_code_trigger <- runif(1)
    })

    observeEvent(input$code_run_all, {
      if (debug) cat(paste0("\n", now(), " - mod_console - observer input$code_run_all"))

      r$console_code <- input$code
      r$console_code_trigger <- runif(1)
    })
    
    observeEvent(r$console_code_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_console - observer r$console_code_trigger"))
      
      code <- r$console_code %>% stringr::str_replace_all("\r", "\n")
      
      ## Run R code ----
      
      sapply(c("code_output", "ui_output", "plot_output_div", "table_output_div", "datatable_output_div", "image_output"), shinyjs::hide)
      if (input$output == "console") shinyjs::show("code_output")
      else if (input$output == "figure") shinyjs::show("plot_output_div")
      else if (input$output == "rmarkdown") shinyjs::show("ui_output")
      else if (input$output == "table") shinyjs::show("table_output_div")
      else if (input$output == "datatable") shinyjs::show("datatable_output_div")
      else if (input$output == "terminal") shinyjs::show("code_output")
      else if (input$output == "matplotlib") shinyjs::show("image_output")
      
      if (input$programming_language == "r"){
        
        # Console output
        if (input$output == "console"){
          
          # Change this option to display correctly tibble in textbox
          options('cli.num_colors' = 1)
          
          # Capture console output of our code
          captured_output <- capture.output(tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w))) %>% paste(collapse = "\n")
          
          # Restore normal value
          options('cli.num_colors' = NULL)
          
          # Render result
          output$code_output <- renderText(captured_output)
        }
        
        # Plot output
        else if (input$output == "figure") shinyjs::delay(100, 
          output$plot_output <- renderPlot(
            tryCatch(eval(parse(text = code)), error = function(e) cat(paste0("\n", toString(e))), warning = function(w) cat(paste0("\n", toString(w)))),
            width = isolate(input$plot_width), height = isolate(input$plot_height), res = isolate(input$plot_dpi)
          )
        )
        
        # RMarkdown
        else if (input$output == "rmarkdown"){
          
          # Create temp dir
          dir <- paste0(r$app_folder, "/temp_files/", r$user_id, "/markdowns")
          output_file <- paste0(dir, "/", paste0(sample(c(0:9, letters[1:6]), 8, TRUE), collapse = ''), "_", now() %>% stringr::str_replace_all(":", "_") %>% stringr::str_replace_all(" ", "_"), ".Md")
          if (!dir.exists(dir)) dir.create(dir)
          
          # Create the markdown file
          knitr::knit(text = code, output = output_file, quiet = TRUE)
  
          output$ui_output <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(output_file))))
        }
        
        # Table
        else if (input$output == "table") output$table_output <- renderTable(tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w)))
        
        # DataTable
        else if (input$output == "datatable") output$datatable_output <- DT::renderDT(tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w)))
      }
      
      # Run Python code ----
      
      else if (input$programming_language == "python"){
        
        reticulate::py_run_string("sys.stdout = original_stdout\nsys.stderr = original_stderr\n")
        
        # Console output
        if (input$output == "console") output$code_output <- renderText(capture_python_output(code))
        
        # Plot output
        else if (input$output == "matplotlib"){
          
          # Create a file with plot image
          dir <- paste0(r$app_folder, "/temp_files/", r$user_id, "/images")
          output_file <- paste0(dir, "/", paste0(sample(c(0:9, letters[1:6]), 8, TRUE), collapse = ''), "_", now() %>% stringr::str_replace_all(":", "_") %>% stringr::str_replace_all(" ", "_"), ".png")
          if (!dir.exists(dir)) dir.create(dir)
          
          code <- paste0(
            "import matplotlib.pyplot as plt\n",
            "plt.close('all')\n",
            code, "\n",
            "plt.savefig('", output_file, "', dpi = ", input$plot_dpi, ")"
          )
          
          capture_python_output(code)
          
          shinyjs::delay(100, 
            output$image_output <- renderImage({
              list(src = output_file, contentType = 'image/png', width = isolate(input$plot_width), height = isolate(input$plot_height))
            }, deleteFile = FALSE)
          )
        }
      }
      
      # Run shell code ----
      
      else if (input$programming_language == "shell"){
        
        # Use R system fct
        result <- capture.output(tryCatch(eval(parse(text = paste0("system('", code, "', intern = TRUE)"))), error = function(e) print(e), warning = function(w) print(w))) %>% paste(collapse = "\n")
        
        # Render result
        output$code_output <- renderText(result)
      }
      
      output$datetime_code_execution <- renderText(format_datetime(now(), language))
    })
  })
}