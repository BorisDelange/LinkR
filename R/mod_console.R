#' @noRd 
mod_console_ui <- function(id){
  
  pages_variables_list <- get("pages_variables_list", envir = parent.frame())
  for (obj_name in pages_variables_list) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  ns <- NS(id)
  
  visit_detail_tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement", "observation", "note", "note_nlp", "payer_plan_period", "cost")
  person_tables <- c(visit_detail_tables, "specimen", "death", "drug_era", "dose_era", "condition_era")
  subset_tables <- c(person_tables, "person", "observation_period", "visit_occurrence", "visit_detail")
  main_tables <- c(subset_tables, "location", "care_site", "provider")
  
  div(
    div(
      id = ns("console_forbidden_access"),
      shiny.fluent::MessageBar(i18n$t("unauthorized_access_area"), messageBarType = 5),
      style = "display: inline-block; margin: 5px;"
    ),
    shinyjs::hidden(
      div(
        id = ns("console_div"),
        div(
          shinyAce::aceEditor(
            ns("code"), value = "", mode = "r",
            code_hotkeys = list("r",  get_ace_editor_code_hotkeys()),
            autoComplete = "live", autoCompleters = c("static", "text"), autoCompleteList = get_ace_editor_auto_complete_list(),
            autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, fontSize = 11, showPrintMargin = FALSE
          ),
          class = "resizable-panel left-panel",
          style = "width: 50%;"
        ),
        div(class = "resizer"),
        div(
          id = ns("console_output"),
          div(textOutput(ns("datetime_code_execution")), style = "color: #878787; margin-bottom: 8px;"),
          verbatimTextOutput(ns("code_output")),
          shinyjs::hidden(div(id = ns("plot_output_div"), plotOutput(ns("plot_output")), style = "padding-top: 10px;")),
          shinyjs::hidden(uiOutput(ns("rmarkdown_output"))),
          shinyjs::hidden(uiOutput(ns("ui_output"), style = "padding-top: 10px;")),
          shinyjs::hidden(div(id = ns("table_output_div"), tableOutput(ns("table_output")), style = "padding-top: 10px;")),
          shinyjs::hidden(div(id = ns("datatable_output_div"), DT::DTOutput(ns("datatable_output")), style = "padding-top: 10px;")),
          shinyjs::hidden(imageOutput(ns("image_output"))),
          class = "resizable-panel right-panel",
          style = "width: 50%; padding: 5px 15px; font-size: 12px; overflow-y: auto;"
        ),
        class = "resizable-container",
        style = "height: 100%; display: flex;"
      )
    ),
    style = "width: 100%; padding: 0;",
    class = "main"
  )
}

#' @noRd 
mod_console_server <- function(id){
  
  pages_variables_list <- get("pages_variables_list", envir = parent.frame())
  for (obj_name in pages_variables_list) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Current user accesses ----
    
    if ("console_execute_code" %in% user_accesses){
      sapply(c("console_sidenav_buttons", "reduced_sidenav_execute_code_button", "console_div"), shinyjs::show) 
      shinyjs::hide("console_forbidden_access")
    }
    
    # Apply user settings ----
    
    shinyAce::updateAceEditor(session, "code", theme = user_settings$ace_theme, fontSize = user_settings$ace_font_size)
    
    text_output_theme <- gsub("_", "-", user_settings$ace_theme)
    if (text_output_theme == "terminal") text_output_theme <- paste0(text_output_theme, "-theme")
    shinyjs::addClass("console_output", paste0("ace-", text_output_theme))
    
    # Auto completion starting at 3 characters
    
    shinyjs::runjs("
      $(document).ready(function() {
        // Initialize the Ace editor
        var editor = ace.edit('console-code');

        // Disable default live autocomplete
        editor.setOptions({
          enableLiveAutocompletion: false
        });

        // Watch for changes in the editor
        editor.getSession().on('change', function() {
          var text = editor.getValue();  // Get the full text
          var cursorPosition = editor.getCursorPosition();  // Get current cursor position
          var session = editor.getSession();
          var line = session.getLine(cursorPosition.row);  // Get the current line of the cursor
          var mode = editor.getSession().getMode().$id; // Get the current mode (language)
      
          // Only activate autocompletion if the mode is R
          if (mode !== 'ace/mode/r') {
            editor.setOptions({ enableLiveAutocompletion: false });
            return;  // Exit early if not in R mode
          }
          
          // Get the word just before the cursor
          var charBeforeCursor = text.slice(editor.session.doc.positionToIndex(cursorPosition) - 1, editor.session.doc.positionToIndex(cursorPosition));
          
          // Detect if the user has typed '::' for a package
          var indexOfDoubleColon = line.lastIndexOf('::');
          var charsAfterDoubleColon = cursorPosition.column - indexOfDoubleColon - 2;  // Characters typed after '::'
          
          var lastWord = line.split(/\\s+/).pop();  // Get the last word (or partial word)

          // If 'd$' is typed (even inside parentheses), enable immediate autocompletion
          if (lastWord.startsWith('d$')) {
            editor.setOptions({ enableLiveAutocompletion: true });
          }
          // Disable autocompletion if fewer than 2 characters after '::'
          else if (indexOfDoubleColon >= 0 && charsAfterDoubleColon < 2) {
            editor.setOptions({ enableLiveAutocompletion: false });
          }
          // General rule: enable autocompletion after 3 characters
          else if (lastWord.length >= 3) {
            editor.setOptions({ enableLiveAutocompletion: true });
          } else {
            editor.setOptions({ enableLiveAutocompletion: false });
          }
        });
      });
    ")
    
    # Unlock reactivity
    observeEvent(shiny.router::get_page(), try_catch("shiny.router::get_page()", {
      if (shiny.router::get_page() == "console") shinyjs::show("console")
    }))
    
    # Update output dropdown ----
    observeEvent(input$programming_language, try_catch("input$programming_language", {
      
      if (length(input$output) == 0) return()
      
      if (input$programming_language == "r"){
        output_options <- list(
          list(key = "console", text = i18n$t("console")),
          list(key = "figure", text = i18n$t("figure")),
          list(key = "ui", text = i18n$t("ui_output")),
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
    }))
    
    # Output dropdown ----
    observeEvent(input$output, try_catch("input$output", {
      
      if (input$output %in% c("figure", "matplotlib")){
        shinyjs::show("plot_size_div")
      }
      else shinyjs::hide("plot_size_div")
      
      sapply(c("code_output", "rmarkdown_output", "ui_output", "plot_output_div", "table_output_div", "datatable_output_div", "image_output"), shinyjs::hide)
      if (input$output == "console") shinyjs::show("code_output")
      else if (input$output == "figure") shinyjs::show("plot_output_div")
      else if (input$output == "rmarkdown") shinyjs::show("rmarkdown_output")
      else if (input$output == "ui") shinyjs::show("ui_output")
      else if (input$output == "table") shinyjs::show("table_output_div")
      else if (input$output == "datatable") shinyjs::show("datatable_output_div")
      else if (input$output == "terminal") shinyjs::show("code_output")
      else if (input$output == "matplotlib") shinyjs::show("image_output")
      
      # Output style
      if (input$output %in% c("console", "terminal")) shinyjs::addClass("console_output", paste0("ace-", text_output_theme))
      else shinyjs::removeClass("console_output", paste0("ace-", text_output_theme))
    }))
    
    # Comment code ----
    observeEvent(input$code_comment, try_catch("input$code_comment", {
      
      toggle_comments(input_id = "code", code = input$code, selection = input$code_comment$range, session = session)
    }))
    
    # Execute code ----
    observeEvent(input$execute_code, try_catch("input$execute_code", {
      
      r$console_code <- input$code
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_trigger', Math.random());"))
    }))

    observeEvent(input$code_run_selection, try_catch("input$code_run_selection", {
      
      editor_id <- "code"
      editor_input <- input[[paste0(editor_id, "_run_selection")]]
      full_code <- input[[editor_id]]
      code_store_var <- "console_code"
      
      execute_ace_code(editor_id = editor_id, full_code = full_code, editor_input = editor_input, code_store_var = code_store_var)
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_trigger', Math.random());"))
    }))

    observeEvent(input$code_run_all, try_catch("input$code_run_all", {

      r$console_code <- input$code
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_trigger', Math.random());"))
    }))
    
    observeEvent(input$run_code_trigger, try_catch("input$run_code_trigger", {
      
      if ("console_execute_code" %not_in% user_accesses) return()
      
      code <- gsub("\r", "\n", r$console_code)
      
      ## Run R code ----
      
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
        
        # UI
        
        else if (input$output == "ui") output$ui_output <- renderUI(tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w)))
        
        # RMarkdown
        else if (input$output == "rmarkdown"){
          
          output_file <- create_rmarkdown_file(code)
          output$rmarkdown_output <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(output_file))))
        }
        
        # Table
        else if (input$output == "table") output$table_output <- renderTable(tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w)))
        
        # DataTable
        else if (input$output == "datatable") output$datatable_output <- DT::renderDT(
          DT::datatable(
            tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w)),
            
            rownames = FALSE,
            options = list(
              dom = "<'datatable_length'l><'top't><'bottom'p>",
              compact = TRUE, hover = TRUE,
              pageLength = 25
            ),
            selection = "single",
            
            # CSS for datatable
            callback = htmlwidgets::JS(
              "table.on('draw.dt', function() {",
              "  $('.dataTable tbody tr td').css({",
              "    'height': '12px',",
              "    'padding': '2px 5px'",
              "  });",
              "  $('.dataTable thead tr td div .form-control').css('font-size', '12px');",
              "  $('.dataTable thead tr td').css('padding', '5px');",
              "});"
            )
          )
        )
      }
      
      # Run Python code ----
      
      else if (input$programming_language == "python"){
        
        # reticulate::py_run_string("sys.stdout = original_stdout\nsys.stderr = original_stderr\n")
        
        # Console output
        if (input$output == "console") output$code_output <- renderText(capture_python_output(code))
        
        # Plot output
        else if (input$output == "matplotlib"){
          
          # Create a file with plot image
          dir <- paste0(r$app_folder, "/temp_files/", r$user_id, "/img")
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
    }))
  })
}