#' @noRd 
mod_user_settings_ui <- function(id, language, languages, i18n, code_hotkeys, auto_complete_list){
  ns <- NS(id)
  
  pivot_item_js <- paste0("
    Shiny.setInputValue('", id, "-current_tab', this.id);
    Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());"
  )
  
  ace_themes <- tibble::tribble(
    ~id, ~text,
    "ambiance", "Ambiance",
    "chaos", "Chaos",
    "chrome", "Chrome",
    "clouds_midnight", "Clouds Midnight",
    "clouds", "Clouds",
    "cobalt", "Cobalt",
    "crimson_editor", "Crimson Editor",
    "dawn", "Dawn",
    "dracula", "Dracula",
    "dreamweaver", "Dreamweaver",
    "eclipse", "Eclipse",
    "github", "GitHub",
    "gob", "Gob",
    "gruvbox", "Gruvbox",
    "idle_fingers", "Idle Fingers",
    "iplastic", "iPlastic",
    "katzenmilch", "Katzenmilch",
    "kr_theme", "KR Theme",
    "kuroir", "Kuroir",
    "merbivore_soft", "Merbivore Soft",
    "merbivore", "Merbivore",
    "mono_industrial", "Mono Industrial",
    "monokai", "Monokai",
    "pastel_on_dark", "Pastel on Dark",
    "solarized_dark", "Solarized Dark",
    "solarized_light", "Solarized Light",
    "sqlserver", "SQL Server",
    "terminal", "Terminal",
    "textmate", "TextMate",
    "tomorrow_night_blue", "Tomorrow Night Blue",
    "tomorrow_night_bright", "Tomorrow Night Bright",
    "tomorrow_night_eighties", "Tomorrow Night Eighties",
    "tomorrow_night", "Tomorrow Night",
    "tomorrow", "Tomorrow",
    "twilight", "Twilight",
    "vibrant_ink", "Vibrant Ink",
    "xcode", "Xcode"
  )
  
  div(
    id = ns("main"),
    class = "main",
    style = "padding: 10px;",
    div(
      div(),
      div(
        id = ns("user_settings_pivot"),
        tags$button(id = ns("code_editor"), i18n$t("code_editor"), class = "pivot_item selected_pivot_item", onclick = pivot_item_js),
        class = "pivot"
      ),
      style = "display:flex; justify-content:space-between;"
    ),
    div(
      id = ns("code_editor_div"),
      div(
        div(
          shiny.fluent::Dropdown.shinyInput(
            ns("ace_theme"), label = i18n$t("ace_theme"),
            options = convert_tibble_to_list(ace_themes, key_col = "id", text_col = "text"), value = "textmate"
          ),
          style = "width: 200px;"
        ),
        div(
          div(tags$strong(i18n$t("ace_font_size"), style = "font-weight: 600;"), style = "margin-bottom: 6px;"),
          div(
            shiny.fluent::SpinButton.shinyInput(
              ns("ace_font_size"), value = 11, min = 1, max = 100
            ),
            style = "width: 200px;"
          ),
          style = "margin-top: 4px;"
        ),
        style = "height: 70px; display: flex; gap: 20px; padding-top: 10px; width: 430px;",
        class = "widget"
      ),
      div(
        div(
          shinyAce::aceEditor(
            ns("ace_editor"), value = "", mode = "r",
            code_hotkeys = list("r", code_hotkeys),
            autoComplete = "live", autoCompleters = c("static", "text"), autoCompleteList = auto_complete_list,
            autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, fontSize = 11, showPrintMargin = FALSE
          ),
          class = "resizable-panel left-panel",
          style = "width: 50%;"
        ),
        div(class = "resizer"),
        div(
          id = ns("console_output"),
          verbatimTextOutput(ns("ace_editor_output")),
          class = "resizable-panel right-panel",
          style = "width: 50%; padding: 0 15px; font-size: 12px; overflow-y: auto;"
        ),
        class = "resizable-container widget",
        style = "height: calc(100% - 100px); width: calc(100% - 20px); margin-top: 20px; display: flex; padding: 0;"
      ),
      style = "height: calc(100% - 50px);"
    )
  )
}

#' @noRd 
mod_user_settings_server <- function(id, r, d, m, language, i18n, debug, user_accesses, user_settings){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    all_divs <- c("code_editor")
    
    # Page change observer ----
    observeEvent(shiny.router::get_page(), {
      
      req(shiny.router::get_page() == id)
      if (debug) cat(paste0("\n", now(), " - mod_user_settings - observer shiny.router::get_page()"))
      
      # Prevent a bug with scroll into ace editor
      shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
      
      # Show a default text output
      captured_output <- capture.output(iris %>% tibble::as_tibble() %>% head(10)) %>% paste(collapse = "\n")
      output$ace_editor_output <- renderText(captured_output)
    })
    
    # Current tab ----
    
    observeEvent(input$current_tab_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_user_settings - observer input$current_tab_trigger"))
      
      current_tab <- gsub(paste0(id, "-"), "", input$current_tab, fixed = FALSE)
      
      # Show or hide pages depending on selected tab
      divs <- setdiff(all_divs, current_tab)
      divs <- c(paste0(divs, "_reduced_sidenav"), paste0(divs, "_large_sidenav"), paste0(divs, "_div"))
      
      # Prevent a bug with scroll into ace editor
      if (current_tab == "code_editor") shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
      
      sapply(divs, shinyjs::hide)
      sapply(c(paste0(current_tab, "_div"), paste0(current_tab, "_reduced_sidenav"), paste0(current_tab, "_large_sidenav")), shinyjs::show)
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_hide_sidenav', 'hide');"))
      
      # Change selected tab
      sapply(all_divs, function(button_id) shinyjs::removeClass(class = "selected_pivot_item", selector = paste0("#", id, "-", button_id)))
      shinyjs::addClass(class = "selected_pivot_item", selector = paste0("#", id, "-", current_tab))
    })
    
    # Load settings ----
    
    print(user_settings)
    shiny.fluent::updateDropdown.shinyInput(session, "ace_theme", value = user_settings$ace_theme)
    shiny.fluent::updateSpinButton.shinyInput(session, "ace_font_size", value = user_settings$ace_font_size)
    
    # Save settings ----
    
    observeEvent(input$save_settings, {
      if (debug) cat(paste0("\n", now(), " - mod_user_settings - observer input$save_settings"))
      
      sql <- glue::glue_sql("DELETE FROM options WHERE category = 'user_settings' AND link_id = {r$user_id}", .con = r$db)
      sql_send_statement(r$db, sql)
      
      last_row <- get_last_row(r$db, "options")
      
      new_data <- tibble::tribble(
        ~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
        last_row + 1, "user_settings", r$user_id, "ace_theme", input$ace_theme, NA_real_, NA_real_, now(), FALSE,
        last_row + 2, "user_settings", r$user_id, "ace_font_size", NA_character_, input$ace_font_size, NA_real_, now(), FALSE
      )
      
      DBI::dbAppendTable(r$db, "options", new_data)
      
      show_message_bar(output, "modif_saved", "success", i18n = i18n, ns = ns)
    })
    
    # Code editor settings ----
    
    observeEvent(input$ace_theme, {
      if (debug) cat(paste0("\n", now(), " - mod_user_settings - observer input$ace_theme"))
      
      # Update Ace edotir theme
      shinyAce::updateAceEditor(session, "ace_editor", theme = input$ace_theme)
      
      shinyjs::runjs(paste0("$('#", id, "-console_output').removeClass(function(index, className) {
        return (className.match(/\\bace-\\S+/g) || []).join(' ');
      });"))
      
      text_output_theme <- gsub("_", "-", input$ace_theme)
      if (text_output_theme == "terminal") text_output_theme <- paste0(text_output_theme, "-theme")
      shinyjs::addClass("console_output", paste0("ace-", text_output_theme))
    })
    
    observeEvent(input$ace_font_size, {
      if (debug) cat(paste0("\n", now(), " - mod_user_settings - observer input$ace_font_size"))
      
      shinyAce::updateAceEditor(session, "ace_editor", fontSize = input$ace_font_size)
    })
  })
}
