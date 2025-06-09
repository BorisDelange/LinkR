# Server - Code

# Init code var
m$code_%widget_id% <- ""

# Outputs
outputs <- list()
outputs$r <- c("console", "ui", "figure", "table", "datatable", "dygraphs", "plotly", "rmarkdown")
outputs$python <- c("console", "matplotlib")

# Prevent a bug with scroll into ace editor
shinyjs::delay(300, shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);"))

# Comment code
observe_event(input$code_%widget_id%_comment, {
    
    toggle_comments(
        id = id, input_id = "code_%widget_id%", code = input$code_%widget_id%,
        selection = input$code_%widget_id%_comment$range, session = session
    )
})

# Run all code with shortcut
observe_event(input$code_%widget_id%_run_all, {
    
    if ("projects_widgets_console" %in% user_accesses){
        m$code_%widget_id% <- input$code_%widget_id%
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
})

# Run code when button is clicked
observe_event(input$display_figure_%widget_id%, {
    
    if ("projects_widgets_console" %in% user_accesses){
        m$code_%widget_id% <- input$code_%widget_id%
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_%widget_id%', Math.random());"))
    }
})

# Run code
observe_event(input$run_code_%widget_id%, {
    
    if (length(input$prog_language_%widget_id%) == 0) return()
    
    language <- input$prog_language_%widget_id%
    code_output <- input$output_%widget_id%
    code <- m$code_%widget_id%
    
    isolate_code <- TRUE
    if (length(input$run_code_on_data_update_%widget_id%) > 0) if (input$run_code_on_data_update_%widget_id%) isolate_code <- FALSE
    
    if (isolate_code & language == "r" & code_output != "rmarkdown") code <- paste0("isolate({\\n", code, "\\n})")
    
    # Hide all outputs
    sapply(paste0(outputs[[language]], "_output_%widget_id%"), shinyjs::hide)
    shinyjs::show(paste0(code_output, "_output_%widget_id%"))

    # Language = R
    if (language == "r"){
        
        # Output = console
        if (code_output == "console"){
            captured_output <- capture.output(tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w))) %>% paste(collapse = "\\n")
            output$console_output_%widget_id% <- renderText(captured_output)
        }
        
        # Output = UI
        else if (code_output == "ui") output$ui_output_%widget_id% <- renderUI(eval(parse(text = code)))
        
        # Output = figure
        else if (code_output == "figure") output$figure_output_%widget_id% <- renderPlot(eval(parse(text = code)))
        
        # Output = table
        else if (code_output == "table") output$table_output_%widget_id% <- renderTable(eval(parse(text = code)))
        
        # Output = DataTable
        else if (code_output == "datatable") output$datatable_output_%widget_id% <- DT::renderDT(
            DT::datatable(
                eval(parse(text = code)),
                
                rownames = FALSE,
                options = list(
                    dom = "<'datatable_length'l><'top't><'bottom'p>",
                    compact = TRUE, hover = TRUE
                ),
                
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
        
        # Output = Dygraphs
        
        else if (code_output == "dygraphs"){
            output$dygraphs_output_%widget_id% <- dygraphs::renderDygraph(eval(parse(text = code)))
        }
        
        # Output = Plotly
        
        else if (code_output == "plotly"){
            output$plotly_output_%widget_id% <- plotly::renderPlotly(eval(parse(text = code)))
        }
        
        # Output = RMarkdown
        else if (code_output == "rmarkdown"){
            
            # Create temp dir
            dir <- paste0(m$app_folder, "/temp_files/", m$user_id, "/markdowns")
            output_file <- paste0(dir, "/", paste0(sample(c(0:9, letters[1:6]), 8, TRUE), collapse = ''), "_", now() %>% stringr::str_replace_all(":", "_") %>% stringr::str_replace_all(" ", "_"), ".Md")
            if (!dir.exists(dir)) dir.create(dir)
              
            # Create the markdown file
            knitr::knit(text = code, output = output_file, quiet = TRUE)
      
            output$rmarkdown_output_%widget_id% <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(output_file))))
        }
    }
    
    # Language = Python
    else if (language == "python"){
        
        if (code_output == "console") output$console_output_%widget_id% <- renderText(capture_python_output(code))
        
        # else if (code_output == "matplotlib")
    }
    
    # Go to figure tab
    if (!input$figure_and_settings_side_by_side_%widget_id%) shinyjs::click("figure_button_%widget_id%")
})

# Save code with shortcut
observe_event(input$code_%widget_id%_save, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_params_and_code_%widget_id%', Math.random());")))
