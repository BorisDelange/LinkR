#' page_header UI Function
#'
#' @description A shiny Module.
#'
#' @param id ID of current page (character)
#' @param language Language used (character)
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_page_header_ui <- function(id = character(), i18n = character()){
  
  ns <- NS(id)
  
  if (!grepl("home", id)) console_input <- shiny.fluent::CommandBarButton.shinyInput(ns("console"), iconProps = list("iconName" = "Code"))
  else console_input <- ""
  
  div(
    class = "header",
    div(
      id = ns("console"),
      style = "display: none; position: fixed; z-index: 1050; left: 0; top: 0; width: 100%; height: 100%; background-color: rgba(0,0,0,0.5);",
      div(
        style = "position: relative; background: #fff; padding: 20px; margin: 3% auto; width: 90%; max-height: 85%; overflow-y: auto;",
        div(
          style = "position: absolute; top: 10px; right: 10px;",
          shiny.fluent::IconButton.shinyInput(ns("hide_console"), iconProps = list(iconName = "Cancel"))
        ),
        div(
          shiny.fluent::Dropdown.shinyInput(ns("console_language"),
            options = list(
              list(key = "r", text = "R"),
              list(key = "python", text = "Python"),
              list(key = "terminal", text = "Terminal")
            ),
            value = "r"
          ),
          style = "width:300px;"
        ),
        div(
          shinyAce::aceEditor(
            outputId = ns("code"), value = "", mode = "r",
            code_hotkeys = list("console", list(
              run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
              run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
              comment = list(win = "CTRL-SHIFT-C", mac = "CTRL-SHIFT-C|CMD-SHIFT-C")
            )),
            wordWrap = TRUE,
            autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 30
          ),
          style = "width: 100%;"
        ),
        shiny.fluent::PrimaryButton.shinyInput(ns("execute_code"), i18n$t("run_code")), br(),
        div(textOutput(ns("datetime_code_execution")), style = "color:#878787;"), br(),
        shinyjs::hidden(
          div(
            id = ns("code_result_div"),
            uiOutput(ns("code_result")),
            style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;"
          )
        )
      )
    ),
    div(htmltools::img(src = "www/logo.png", style = "height:25px;"), class = "logo"),
    div(class = "title", shiny.fluent::Text(variant = "xLarge", "LinkR")),
    div(
      tags$a(
        tags$img(src = "www/sidebar.png", style = "width:16px; height:auto; display:block; margin-top:8px; margin-left:8px;"), 
        href = "javascript:void(0);", 
        onclick = paste0("event.preventDefault(); Shiny.setInputValue('", id, "-show_hide_sidenav', Math.random()); return false;"),
        style = "display: block; width:100%; height:100%;"
      ), 
      class = "link_image_container"),
    div(class = "header_left_bar", 
      shiny.fluent::CommandBar(
        items = list(
          shiny.fluent::CommandBarItem(i18n$t("home"), "Home", href = shiny.router::route_link("/")),
          shiny.fluent::CommandBarItem(i18n$t("data"), "OfflineStorage",
            subMenuProps = list(items = list(
              list(text = i18n$t("access_to_data"), iconProps = list(iconName = "BIDashboard"), href = shiny.router::route_link("data")),
              list(text = i18n$t("data_cleaning"), iconProps = list(iconName = "CodeEdit"), href = shiny.router::route_link("scripts")),
              list(text = i18n$t("my_studies"), iconProps = list(iconName = "CustomList"), href = shiny.router::route_link("my_studies")),
              list(text = i18n$t("my_subsets"), iconProps = list(iconName = "People"), href = shiny.router::route_link("my_subsets"))
          ))),
          shiny.fluent::CommandBarItem(i18n$t("messages"), "Chat", href = shiny.router::route_link("messages")),
          shiny.fluent::CommandBarItem(i18n$t("vocabularies"), "AllApps", href = shiny.router::route_link("vocabularies")),
          shiny.fluent::CommandBarItem(i18n$t("plugins"), "Code", subMenuProps = list(items = list(
            list(text = i18n$t("patient_lvl_data"), iconProps = list(iconName = "Contact"), href = shiny.router::route_link("plugins_patient_lvl")),
            list(text = i18n$t("aggregated_data"), iconProps = list(iconName = "Group"), href = shiny.router::route_link("plugins_aggregated"))
          )))
        )
      )
    ),
    div(class = "header_right_bar",
      shiny.fluent::Stack(horizontal = TRUE, tokens = (childrenGap = 0),
        shiny.fluent::CommandBar(),
        div(uiOutput(ns("username")), style = "font-weight:bold; padding: 12px 10px 0px 0px;"),
        console_input,
        shiny.fluent::CommandBarButton.shinyInput(ns("help"), iconProps = list("iconName" = "Help")),
        shiny.fluent::CommandBarButton.shinyInput("settings", iconProps = list("iconName" = "Settings"), href = shiny.router::route_link("settings/general_settings")),
        shiny.fluent::CommandBarButton.shinyInput(".shinymanager_logout", iconProps = list("iconName" = "PowerButton"))
      )
    )
  )
}

#' page_header Server Functions
#'
#' @noRd 
mod_page_header_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(),
  language = "en", i18n = character(),
  perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Show / hide sidenav
    
    r$show_hide_sidenav <- "hide"
  
    observeEvent(input$show_hide_sidenav, {
      if (debug) cat(paste0("\n", now(), " - mod_page_header - observer input$show_hide_sidenav"))
      
      if (r$show_hide_sidenav == "hide"){
        r$show_hide_sidenav <- "show"
        shinyjs::runjs(paste0("$('.extended_sidenav').css('display', 'none');"))
        shinyjs::runjs(paste0("$('.reduced_sidenav').css('display', 'block');"))
        shinyjs::runjs(paste0("$('.grid-container').css('grid-template-areas', '\"header header header\" \"sidenav main main\" \"footer footer footer\"');"))
        shinyjs::runjs(paste0("$('.main').css('left', '20px');"))
      }
      else {
        r$show_hide_sidenav <- "hide"
        shinyjs::runjs(paste0("$('.extended_sidenav').css('display', 'block');"))
        shinyjs::runjs(paste0("$('.reduced_sidenav').css('display', 'none');"))
        shinyjs::runjs(paste0("$('.grid-container').css('grid-template-areas', '\"header header header\" \"sidenav sidenav main\" \"footer footer footer\"');"))
        shinyjs::runjs(paste0("$('.main').css('left', '0px');"))
      }
    })
    
    output$username <- renderUI(r$username)
    
    # Open / close console modal
    
    observeEvent(input$console, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$console"))
      r$open_modal_console <- TRUE
      shinyjs::show("console")
    })
    
    observeEvent(input$hide_console, {
      if (debug) cat(paste0("\n", now(), " - mod_data - ", id, " - observer input$hide_console"))
      shinyjs::hide("console")
    })
    
    observeEvent(input$execute_code, {
      
      if (debug) cat(paste0("\n", now(), " - mod_settings_dev - observer input$execute_code"))
      
      r$console_code <- input$code
      r$console_code_trigger <- now()
    })
    
    observeEvent(input$code_run_selection, {
      
      if (debug) cat(paste0("\n", now(), " - mod_settings_dev - observer input$code_run_selection"))
      
      if(!shinyAce::is.empty(input$code_run_selection$selection)) r$console_code <- input$code_run_selection$selection
      else r$console_code <- input$code_run_selection$line
      r$console_code_trigger <- now()
    })
    
    observeEvent(input$code_run_all, {
      
      if (debug) cat(paste0("\n", now(), " - mod_settings_dev - observer input$code_run_all"))
      
      r$console_code <- input$code
      r$console_code_trigger <- now()
    })
    
    observeEvent(r$python_path_trigger, {
      sql <- glue::glue_sql("SELECT * FROM options WHERE category = 'python_config' AND name = 'python_path'")
      python_path <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(value)
      
      tryCatch({
        reticulate::use_python(python_path)
      }, error = function(e) "")
    })
    
    observeEvent(r$console_code_trigger, {
      
      # req(id == shiny.router::get_page() | (grepl("home", id) & shiny.router::get_page() == "/"))
      req(id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_"))
      
      if (debug) cat(paste0("\n", now(), " - mod_settings_dev - observer r$console_code_trigger"))
        
      shinyjs::show("code_result_div")
      
      edited_code <- r$console_code %>% stringr::str_replace_all("\r", "\n")
      
      console_result <- ""
      
      if (input$console_language == "r"){
        if ("dev_edit_r_code_card" %in% r$user_accesses){
          console_result <- isolate(execute_settings_code(input = input, output = output, session = session, id = id, ns = ns, i18n = i18n, r = r, d = d, m = m,
            edited_code = edited_code, code_type = "server")) %>%
            stringr::str_replace_all(paste0("<(lgl|int|dbl|chr|cpl|raw|list|named list|fct|ord|date|dttm|drtn|time",
              "|int64|blob|df\\[\\,1\\]|tibble\\[\\,1\\]|I|\\?\\?\\?|vctrs_vc|prtl_fctr|prtl|fn|sym|expression|quos)>"),
             function(x) sprintf("&lt;%s&gt;", substr(x, 2, nchar(x) - 1)))

          if (console_result != "NULL") console_result <- console_result %>% gsub("NULL$", "", .)

          # If there's a <simpleWarning ...> message, it is interpreted as an HTML tag and doesn't show the console result
          if (!grepl("simpleWarning|simpleError|Rcpp::", console_result)) console_result <- HTML(paste0("<pre>", console_result, "</pre>"))
          else console_result <- tags$div(console_result, style = "padding: 15px 0px 15px 0px;")
        }
      }

      else if (input$console_language == "python"){
        if ("dev_edit_python_code_card" %in% r$user_accesses){
          console_result <- capture_python_output(edited_code)

          console_result <- HTML(paste0("<pre>", console_result, "</pre>"))
        }
      }

      else if (input$console_language == "terminal"){
        if ("dev_edit_terminal_code_card" %in% r$user_accesses){
          console_result <- capture.output(
            tryCatch(eval(parse(text = paste0("system('", edited_code, "', intern = TRUE)"))), error = function(e) print(e), warning = function(w) print(w)))
          
          console_result <- paste(strwrap(console_result, width = 150), collapse = "\n")
          console_result <- HTML(paste0("<pre>", console_result, "</pre>"))
        }
      }

      output$code_result <- renderUI(console_result)
      output$datetime_code_execution <- renderText(format_datetime(now(), language))
    })
    
    # Comment text
    observeEvent(input$code_comment, {
      if (debug) cat(paste0("\n", now(), " - mod_settings_dev - observer input$code_comment"))
      
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
  })
}