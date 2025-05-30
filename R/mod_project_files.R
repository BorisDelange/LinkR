#' @noRd 
mod_project_files_ui <- function(id){
  
  pages_variables_list <- get("pages_variables_list", envir = parent.frame())
  for (obj_name in pages_variables_list) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  ns <- NS(id)
  
  div(
    class = "main",
    
    div(
      div(
        id = ns("forbidden_access"),
        shiny.fluent::MessageBar(i18n$t("unauthorized_access_area"), messageBarType = 5),
        style = "display: inline-block; margin: 5px;"
      ),
      shinyjs::hidden(
        div(
          id = ns("project_scripts_div"),
          div(id = ns("tabs_div"), uiOutput(ns("tabs_ui")), style = "height: 24px;"),
          div(
            div(
              id = ns("editors_div"),
              class = "resizable-panel left-panel",
              style = "width: 50%;"
            ),
            div(class = "resizer"),
            div(
              id = ns("code_result_div"),
              div(textOutput(ns("datetime_code_execution")), style = "color: #878787; margin-bottom: 8px;"),
              verbatimTextOutput(ns("verbatim_code_output")),
              shinyjs::hidden(
                div(
                  id = ns("plot_output_div"),
                  div(plotOutput(ns("plot_output"))), div(verbatimTextOutput(ns("plot_text_output")), style = "height: 100%;"),
                  style = "padding-top: 10px; height: calc(100% - 10px); display: flex; flex-direction: column;"
                )
              ),
              shinyjs::hidden(uiOutput(ns("rmarkdown_output"))),
              shinyjs::hidden(uiOutput(ns("ui_output"), style = "padding-top: 10px;")),
              shinyjs::hidden(div(id = ns("table_output_div"), tableOutput(ns("table_output")), style = "padding-top: 10px;")),
              shinyjs::hidden(div(id = ns("datatable_output_div"), DT::DTOutput(ns("datatable_output")), style = "padding-top: 10px;")),
              shinyjs::hidden(imageOutput(ns("image_output"))),
              class = "resizable-panel right-panel",
              style = "width: 50%; padding: 5px 15px; font-size: 12px; overflow-y: auto;"
            ),
            class = "resizable-container",
            style = "height: calc(100% - 10px); display: flex;"
          ),
          style = "height: 100%;"
        )
      ),
      style = "height: calc(100% - 14px);"
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
    )
  )
}

#' @noRd 
mod_project_files_server <- function(id){
  
  pages_variables_list <- get("pages_variables_list", envir = parent.frame())
  for (obj_name in pages_variables_list) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Current user accesses ----
    
    if ("projects_scripts" %in% user_accesses){
      sapply(c("edit_code_buttons", "project_scripts_div", "files_browser_div"), shinyjs::show)
      shinyjs::hide("forbidden_access")
    }
    
    # Apply user settings ----
    
    text_output_theme <- gsub("_", "-", user_settings$ace_theme)
    if (text_output_theme == "terminal") text_output_theme <- paste0(text_output_theme, "-theme")
    shinyjs::addClass("code_result_div", paste0("ace-", text_output_theme))
    
    # Initiate vars ----
    
    r$project_files_list <- tibble::tibble(id = integer(), project_id = integer(), filename = character())
    r$project_tabs <- tibble::tibble(id = integer(), project_id = integer(), filename = character(), position = integer())
    r$project_deleted_files_id <- c()
    r$project_editors <- tibble::tibble(id = integer(), project_id = integer(), filename = character())
    
    # Load project files ----
    
    observeEvent(m$selected_project, try_catch("m$selected_project", {
      
      if (!("projects_scripts" %in% user_accesses && !is.na(m$selected_project))) return()
      
      # Unique ID
      selected_project_unique_id <- r$projects_long %>% dplyr::filter(id == m$selected_project, name == "unique_id") %>% dplyr::pull(value)
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-selected_project_unique_id', '", selected_project_unique_id, "');"))
      
      project_folder <- file.path(r$app_folder, "projects_files", selected_project_unique_id)
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-selected_project_folder', '", project_folder, "');"))
      
      if (!dir.exists(project_folder)) dir.create(project_folder, recursive = TRUE)
      else {
        file_names <- list.files(project_folder, full.names = FALSE)
        project_files <- r$project_files_list %>% dplyr::filter(project_id == m$selected_project) %>% dplyr::pull(filename)
        file_names <- setdiff(file_names, project_files)
        
        if (length(file_names) > 0){
          
          if (nrow(r$project_files_list) > 0) max_id <- max(r$project_files_list$id)
          else max_id <- 0
          
          r$project_files_list <-
            r$project_files_list %>%
            dplyr::bind_rows(tibble::tibble(id = max_id + 1:length(file_names), project_id = m$selected_project, filename = file_names)) %>%
            dplyr::arrange(filename)
        }
      }
      
      # Reload files browser + tabs
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_files_browser', Math.random());"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_files_tab', '", now(format = "%Y-%m-%d %H:%M:%OS3"), "');"))
    }))
    
    # Reload files browser ----
    
    observeEvent(input$reload_files_browser, try_catch("input$reload_files_browser", {
      
      project_files_list <- r$project_files_list %>% dplyr::filter(project_id == m$selected_project)
      files_ui <- load_files_browser_ui(input_prefix = "", files_list = project_files_list)

      output$files_browser <- renderUI(files_ui)
    }))
    
    # Reload tabs ----
    
    observeEvent(input$reload_files_tab, try_catch("input$reload_files_tab", {
      
      tabs_container <- reload_files_browser_tabs(
        input_prefix = "", r_prefix = "project",
        element_id = m$selected_project, file_id = input$selected_file
      )
      
      output$tabs_ui <- renderUI(tabs_container)
      
      shinyjs::delay(500, shinyjs::runjs(sprintf("initSortableTabs('%s')", ns("tabs"))))
    }))
    
    observeEvent(input$tab_positions, try_catch("input$tab_positions", {
      
      files_browser_edit_tab_positions(positions = input$tab_positions, r_prefix = "project", element_id = m$selected_project)
    }))
    
    # Change file tab ----
    
    observeEvent(input$selected_tab, try_catch("input$selected_tab", {
      
      files_browser_change_tab(input_prefix = "", r_prefix = "project", file_id = input$selected_tab)
    }))
    
    # Open a file ----
    
    observeEvent(input$selected_file, try_catch("input$selected_file", {
      
      files_browser_open_file(
        input_prefix = "", r_prefix = "project", folder = input$selected_project_folder,
        element_id = m$selected_project, file_id = input$selected_file, user_settings = user_settings
      )
      
      shinyjs::delay(100, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_code_output_dropdown', Math.random());")))
    }))
    
    # Update code output dropdown ----
    
    observeEvent(input$update_code_output_dropdown, try_catch("input$update_code_output_dropdown", {
      
      file_name <- r$project_files_list %>% dplyr::filter(id == input$selected_file) %>% dplyr::pull(filename)
      extension <- tolower(sub(".*\\.", "", file_name))
      
      editor_id <- paste0("editor_", input$selected_file)
      current_content <- input[[editor_id]]
      output_value <- "console"
      
      if (!is.null(current_content)) {
        first_line <- strsplit(current_content, "\n")[[1]][1]
        if (grepl("^#\\s*output:", first_line)) {
          output_value <- trimws(sub("^#\\s*output:\\s*", "", first_line))
        }
      }
      
      output_options <- list()
      output_keys <- c()
      
      if (extension == "r"){
        output_options <- list(
          list(key = "console", text = i18n$t("console")),
          list(key = "figure", text = i18n$t("figure")),
          list(key = "ui", text = i18n$t("ui_output")),
          list(key = "table", text = i18n$t("table_output")),
          list(key = "datatable", text = i18n$t("datatable_output")),
          list(key = "rmarkdown", text = i18n$t("rmarkdown"))
        )
      }
      else if (extension == "py"){
        output_options <- list(
          list(key = "console", text = i18n$t("console")),
          list(key = "matplotlib", text = i18n$t("matplotlib"))
        )
      }
      
      output_keys <- sapply(output_options, function(x) x$key)
      if (output_value %not_in% output_keys) output_value <- "console"
      
      shiny.fluent::updateDropdown.shinyInput(session, "code_output", options = output_options, value = output_value)
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_ace_mode', Math.random());"))
    }))
    
    # Code output ----
    
    observeEvent(input$code_output, try_catch("input$code_output", {
      
      # Output style
      if (input$code_output %in% c("console", "terminal")) shinyjs::addClass("code_result_div", paste0("ace-", text_output_theme))
      else shinyjs::removeClass("code_result_div", paste0("ace-", text_output_theme))
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_ace_mode', Math.random());"))
      
      editor_id <- paste0("editor_", input$selected_file)
      current_content <- input[[editor_id]]
      
      if (!grepl("^#\\s*output:", current_content)) {
        new_content <- paste0("# output: ", input$code_output, "\n\n", current_content)
        shinyAce::updateAceEditor(session, editor_id, value = new_content)
      } else {
        lines <- strsplit(current_content, "\n")[[1]]
        output_line_idx <- which(grepl("^#\\s*output:", lines))[1]
        lines[output_line_idx] <- paste0("# output: ", input$code_output)
        new_content <- paste(lines, collapse = "\n")
        shinyAce::updateAceEditor(session, editor_id, value = new_content)
      }
    }))
    
    observeEvent(input$update_ace_mode, try_catch("input$update_ace_mode", {
      
      # Update ace editor
      file_name <- r$project_files_list %>% dplyr::filter(id == input$selected_file) %>% dplyr::pull(filename)
      extension <- tolower(sub(".*\\.", "", file_name))
      
      ace_mode <- extension
      if (extension == "py") ace_mode <- "python"
      if (length(input$code_output) > 0) if (input$code_output == "rmarkdown") ace_mode <- "markdown"
      
      shinyAce::updateAceEditor(session, paste0("editor_", input$selected_file), mode = ace_mode)
      
      # Reset outputs
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reset_code_outputs', Math.random());"))
      
      # Show / hide output divs
      
      if (length(input$code_output) > 0){
        sapply(c("verbatim_code_output", "rmarkdown_output", "ui_output", "plot_output_div", "table_output_div", "datatable_output_div", "image_output"), shinyjs::hide)
        
        shinyjs::delay(10, {
          if (input$code_output == "console") shinyjs::show("verbatim_code_output")
          else if (input$code_output == "figure") shinyjs::show("plot_output_div")
          else if (input$code_output == "rmarkdown") shinyjs::show("rmarkdown_output")
          else if (input$code_output == "ui") shinyjs::show("ui_output")
          else if (input$code_output == "table") shinyjs::show("table_output_div")
          else if (input$code_output == "datatable") shinyjs::show("datatable_output_div")
          else if (input$code_output == "matplotlib") shinyjs::show("image_output")
        })
      }
    }))
    
    ## Reset outputs
    
    observeEvent(input$reset_code_outputs, try_catch("input$reset_code_outputs", {
      
      output$verbatim_code_output <- renderText("")
      output$plot_output <- renderPlot(NULL)
      output$rmarkdown_output <- renderUI("")
      output$ui_output <- renderUI("")
      output$table_output <- renderTable(NULL)
      output$datatable_output <- DT::renderDT(NULL)
      output$image_output <- renderImage({ 
        list(src = "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7", 
             contentType = 'image/gif', 
             width = 1, 
             height = 1)
      }, deleteFile = FALSE)
    }))
    
    # Close a file ----
    
    observeEvent(input$close_selected_tab_trigger, try_catch("input$close_selected_tab_trigger", {
      
      files_browser_close_file(
        input_prefix = "", r_prefix = "project",
        element_id = m$selected_project, file_id = input$close_selected_tab
      )
    }))
    
    # Create a file ----
    
    observeEvent(input$add_file, try_catch("input$add_file", {
      
      if ("projects_scripts" %not_in% user_accesses) return()
      
      files_browser_create_file(
        input_prefix = "", r_prefix = "project", folder = input$selected_project_folder,
        element_id = m$selected_project, user_settings = user_settings
      )
    }))
    
    # Editor hotkeys ----
    observeEvent(input$add_code_editor_hotkeys, try_catch("input$add_code_editor_hotkeys", {
      
      file_id <- input$add_code_editor_hotkeys
      editor_id <- paste0("editor_", file_id)
      
      observeEvent(input[[paste0(editor_id, "_comment")]], try_catch(paste0("input$", editor_id, "_comment"), {
        toggle_comments(input_id = editor_id, code = input[[editor_id]], selection = input[[paste0(editor_id, "_comment")]]$range, session = session)
      }))
      
      observeEvent(input[[paste0(editor_id, "_save")]], try_catch(paste0("input$", editor_id, "_save"), {
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_file_code', Math.random());"))
      }))
    }))
    
    # Save updates ----
    
    observeEvent(input$save_file_code, try_catch("input$save_file_code", {
      
      if ("projects_scripts" %not_in% user_accesses) return()
      
      file <- r$project_files_list %>% dplyr::filter(id == input$selected_file)
      
      files_browser_save_file(
        input_prefix = "", r_prefix = "project",
        folder = input$selected_project_folder, element_id = m$selected_project, file_id = file$id,
        new_code = input[[paste0("editor_", file$id)]]
      )
    }))
    
    # Rename a file ----
    
    # Show textfield
    
    observeEvent(input$edit_filename_trigger, try_catch("input$edit_filename_trigger", {
      
      file_id <- input$edit_filename
      file_name <- r$project_files_list %>% dplyr::filter(id == file_id) %>% dplyr::pull(filename)
      shiny.fluent::updateTextField.shinyInput(session, paste0("edit_filename_textfield_", file_id), value = file_name)
      
      shinyjs::delay(
        50, 
        sapply(c(
          paste0("edit_filename_textfield_div_", file_id),
          paste0("save_filename_button_div_", file_id),
          paste0("cancel_rename_button_div_", file_id)),
          shinyjs::show
        )
      )
      sapply(c(
        paste0("filename_div_", file_id),
        paste0("delete_file_button_div_", file_id),
        paste0("edit_filename_button_div_", file_id)),
        shinyjs::hide
      )
    }))
    
    # Cancel rename
    
    observeEvent(input$cancel_rename_trigger, try_catch("input$cancel_rename_trigger", {
      
      file_id <- input$edit_filename
      
      shinyjs::delay(
        50, 
        sapply(c(
          paste0("edit_filename_textfield_div_", file_id),
          paste0("save_filename_button_div_", file_id),
          paste0("cancel_rename_button_div_", file_id)),
          shinyjs::hide
        )
      )
      sapply(c(
        paste0("filename_div_", file_id),
        paste0("delete_file_button_div_", file_id),
        paste0("edit_filename_button_div_", file_id)),
        shinyjs::show
      )
    }))
    
    # Save new name
    
    observeEvent(input$save_filename_trigger, try_catch("input$save_filename_trigger", {
      
      file_id <- input$save_filename
      textfield_id <- paste0("edit_filename_textfield_", file_id)
      new_name <- input[[textfield_id]]
      
      files_browser_edit_filename(
        input_prefix = "", r_prefix = "project", folder = input$selected_project_folder,
        element_id = m$selected_project, file_id = file_id, new_name = new_name
      )
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_code_output_dropdown', Math.random());"))
    }))
    
    # Delete a file ----
    
    observeEvent(input$delete_file_trigger, try_catch("input$delete_file_trigger", shinyjs::show("delete_file_modal")))
    
    observeEvent(input$close_file_deletion_modal, try_catch("input$close_file_deletion_modal", shinyjs::hide("delete_file_modal")))
    
    observeEvent(input$confirm_file_deletion, try_catch("input$confirm_file_deletion", {
      
      if ("projects_scripts" %not_in% user_accesses) return()
      
      files_browser_delete_file(
        input_prefix = "", r_prefix = "project",
        folder = input$selected_project_folder, element_id = m$selected_project, file_id = input$delete_file
      )
    }))
    
    # Run code ----
    
    observeEvent(input$run_code, try_catch("input$run_code", {
      
      r$project_file_code <- input[[paste0("editor_", input$selected_file)]]
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_trigger', Math.random());"))
    }))
    
    observeEvent(input[[paste0("editor_", input$selected_file, "_run_selection")]], try_catch(paste0("input$editor_", input$selected_file, "_run_selection"), {
      editor_id <- paste0("editor_", input$selected_file)
      editor_input <- input[[paste0(editor_id, "_run_selection")]]
      full_code <- input[[editor_id]]
      code_store_var <- "project_file_code"
      
      execute_ace_code(editor_id = editor_id, full_code = full_code, editor_input = editor_input, code_store_var = code_store_var)
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_trigger', Math.random());"))
    }))
    
    observeEvent(input[[paste0("editor_", input$selected_file, "_run_all")]], try_catch(paste0("input$editor_", input$selected_file, "_run_all"), {
      
      r$project_file_code <- input[[paste0("editor_", input$selected_file)]]
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_trigger', Math.random());"))
    }))
    
    observeEvent(input$run_code_trigger, try_catch("input$run_code_trigger", {

      if ("projects_scripts" %not_in% user_accesses) return()
        
      code <- gsub("\r", "\n", r$project_file_code)
      
      file_name <- r$project_files_list %>% dplyr::filter(id == input$selected_file) %>% dplyr::pull(filename)
      extension <- tolower(sub(".*\\.", "", file_name))
  
      ## Run R code ----
      
      if (extension == "r"){
  
        # Console output
        if (input$code_output == "console"){
  
          # Change this option to display correctly tibble in textbox
          options('cli.num_colors' = 1)
  
          # Capture console output of our code
          captured_output <- capture.output(tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w))) %>% paste(collapse = "\n")
          options('cli.num_colors' = NULL)
  
          # Render result
          output$verbatim_code_output <- renderText(captured_output)
        }
  
        # Plot output
        else if (input$code_output == "figure"){
          
          captured_output <- capture.output(plot_output <- tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w))) %>% paste(collapse = "\n")
          
          if (nchar(captured_output) > 0) {
            output$plot_text_output <- renderText(captured_output)
            shinyjs::hide("plot_output")
          }
          else {
            output$plot_text_output <- renderText("")
            output$plot_output <- renderPlot(
              plot_output#,
              # width = isolate(input$plot_width), height = isolate(input$plot_height), res = isolate(input$plot_dpi)
            )
            shinyjs::show("plot_output")
          }
        }
  
        # UI
        else if (input$code_output == "ui") output$ui_output <- renderUI(tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w)))
  
        # RMarkdown
        else if (input$code_output == "rmarkdown"){
  
          output_file <- create_rmarkdown_file(code)
          output$rmarkdown_output <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(output_file))))
        }
  
        # Table
        else if (input$code_output == "table") output$table_output <- renderTable(tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w)))
  
        # DataTable
        else if (input$code_output == "datatable") output$datatable_output <- DT::renderDT(
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
  
      else if (extension == "py"){
  
        # Console output
        if (input$code_output == "console") output$verbatim_code_output <- renderText(capture_python_output(code))
  
        # Plot output
        else if (input$code_output == "matplotlib"){
  
          # Create a file with plot image
          dir <- paste0(r$app_folder, "/temp_files/", r$user_id, "/img")
          output_file <- paste0(dir, "/", paste0(sample(c(0:9, letters[1:6]), 8, TRUE), collapse = ''), "_", now() %>% stringr::str_replace_all(":", "_") %>% stringr::str_replace_all(" ", "_"), ".png")
          if (!dir.exists(dir)) dir.create(dir)
  
          code <- paste0(
            "import matplotlib.pyplot as plt\n",
            "plt.close('all')\n",
            code, "\n",
            # "plt.savefig('", output_file, "', dpi = ", input$plot_dpi, ")"
            "plt.savefig('", output_file, "')"
          )
  
          capture_python_output(code)
          
          shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-render_image_trigger', Math.random());"))
          shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-render_image', '", output_file, "');"))
  
          # shinyjs::delay(
          #   100,
          #   output$image_output <- renderImage({
          #     # list(src = output_file, contentType = 'image/png', width = isolate(input$plot_width), height = isolate(input$plot_height))
          #     list(src = output_file, contentType = 'image/png')
          #   }, deleteFile = FALSE)
          # )
        }
      }
      
      output$datetime_code_execution <- renderText(try_catch("output$datetime_code_execution", format_datetime(now(), language)))
    }))
    
    observeEvent(input$render_image_trigger, try_catch("input$render_image_trigger", {
      
      output$image_output <- renderImage(try_catch("output$image_output", {
        # list(src = output_file, contentType = 'image/png', width = isolate(input$plot_width), height = isolate(input$plot_height))
        list(src = input$render_image, contentType = 'image/png')
      }), deleteFile = FALSE)
    }))
  })
}