#' @noRd 
mod_user_settings_ui <- function(id){
  
  pages_variables_list <- get("pages_variables_list", envir = parent.frame())
  for (obj_name in pages_variables_list) assign(obj_name, get(obj_name, envir = parent.frame()))
  
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
    div(
      div(),
      div(
        id = ns("user_settings_pivot"),
        tags$button(id = ns("user_profile"), i18n$t("user_profile"), class = "pivot_item selected_pivot_item", onclick = pivot_item_js),
        tags$button(id = ns("code_editor"), i18n$t("code_editor"), class = "pivot_item", onclick = pivot_item_js),
        class = "pivot"
      ),
      style = "display:flex; justify-content:space-between;"
    ),
    
    # Code editor ----
    
    shinyjs::hidden(
      div(
        id = ns("code_editor_div"),
        tags$h1(i18n$t("code_editor")),
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
          style = "display: flex; gap: 20px;"
        ),
        div(
          div(
            shinyAce::aceEditor(
              ns("ace_editor"), value = "", mode = "r",
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
            verbatimTextOutput(ns("ace_editor_output")),
            class = "resizable-panel right-panel",
            style = "width: 50%; padding: 10px 15px; font-size: 12px; overflow-y: auto;"
          ),
          class = "resizable-container",
          style = "height: calc(100% - 100px); margin-top: 10px; display: flex;"
        ),
        class = "widget",
        style = "height: calc(100% - 50px); padding: 0 15px;"
      )
    ),
    
    # User profile ----
    
    div(
      id = ns("user_profile_div"),
      tags$h1(i18n$t("user_profile")),
      div(shiny.fluent::TextField.shinyInput(ns("username"), label = i18n$t("username")), style = "width: 200px;"),
      div(
        div(shiny.fluent::TextField.shinyInput(ns("firstname"), label = i18n$t("firstname")), style = "width: 200px;"),
        div(shiny.fluent::TextField.shinyInput(ns("lastname"), label = i18n$t("lastname")), style = "width: 200px;"),
        style = "display: flex; gap: 10px;"
      ),
      div(shiny.fluent::DefaultButton.shinyInput(ns("change_password"), i18n$t("change_password"), style = "width: 200px; margin-top: 20px;")),
      class = "widget",
      style = "height: calc(100% - 50px); padding: 0 15px;"
    ),
    
    # Update password modal ----
    
    shinyjs::hidden(
      div(
        id = ns("change_password_modal"),
        div(
          div(
            tags$h1(i18n$t("change_password")),
            shiny.fluent::IconButton.shinyInput(ns("close_change_password_modal"), iconProps = list(iconName = "ChromeClose")),
            class = "change_password_modal_head small_close_button"
          ),
          div(
            div(shiny.fluent::TextField.shinyInput(ns("current_password"), label = i18n$t("current_password"), type = "password", canRevealPassword = TRUE), style = "width: 200px;"),
            div(
              div(shiny.fluent::TextField.shinyInput(ns("new_password_1"), label = i18n$t("new_password"), type = "password", canRevealPassword = TRUE), style = "width: 200px;"),
              div(shiny.fluent::TextField.shinyInput(ns("new_password_2"), label = i18n$t("repeat_new_password"), type = "password", canRevealPassword = TRUE), style = "width: 200px;"),
              style = "display: flex; gap: 20px;"
            ),
            div(
              shiny.fluent::PrimaryButton.shinyInput(ns("confirm_password_update"), i18n$t("confirm")),
              class = "change_password_modal_buttons"
            ),
          ),
          class = "change_password_modal_content"
        ),
        class = "change_password_modal"
      )
    )
  )
}

#' @noRd 
mod_user_settings_server <- function(id){
  
  pages_variables_list <- get("pages_variables_list", envir = parent.frame())
  for (obj_name in pages_variables_list) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    all_divs <- c("user_profile", "code_editor")
    
    # Page change observer ----
    observe_event(shiny.router::get_page(), {
      
      if (shiny.router::get_page() == id){
      
        # Show a default text output
        captured_output <- capture.output(tibble::tibble(
          firstname = c("John", "Sarah", "Michael", "Emma", "David", "Lisa", "Robert", "Jennifer", "Thomas", "Maria"),
          lastname = c("Smith", "Johnson", "Williams", "Brown", "Jones", "Garcia", "Miller", "Davis", "Rodriguez", "Martinez"),
          birth_date = c("1975-03-15", "1982-07-22", "1968-11-04", "1990-05-18", "1956-09-30", 
                         "1988-01-12", "1972-06-25", "1985-04-07", "1963-12-21", "1978-08-09"),
          death_date = c(NA, NA, "2022-06-15", NA, "2020-11-09", NA, NA, NA, "2021-03-25", NA),
          gender = c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F")
        ) %>% tibble::as_tibble() %>% head(10)) %>% paste(collapse = "\n")
        
        output$ace_editor_output <- renderText(captured_output)
      }
    })
    
    # Current tab ----
    
    observe_event(input$current_tab_trigger, {
      
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
    
    shiny.fluent::updateDropdown.shinyInput(session, "ace_theme", value = user_settings$ace_theme)
    shiny.fluent::updateSpinButton.shinyInput(session, "ace_font_size", value = user_settings$ace_font_size)
    
    # Save settings ----
    
    observe_event(input$save_settings, {
      
      current_tab <- "user_profile"
      if (length(input$current_tab) > 0) current_tab <- gsub(paste0(id, "-"), "", input$current_tab, fixed = FALSE)
      
      if (current_tab == "user_profile"){
        
        # Check if a textfield is empty
        empty_fields <- list()
        
        for (field in c("username", "firstname", "lastname")){
          empty_fields[[field]] <- TRUE
          field_name <- input[[field]]
          if (length(field_name) > 0) if (!is.na(field_name) & field_name != "") empty_fields[[field]] <- FALSE
          if (empty_fields[[field]]) shiny.fluent::updateTextField.shinyInput(session, field, errorMessage = i18n$t("provide_valid_value"))
          else shiny.fluent::updateTextField.shinyInput(session, field, errorMessage = NULL)
        }
        if (TRUE %in% empty_fields) return()
        
        sql_username <- glue::glue_sql("SELECT id FROM users WHERE LOWER(username) = {tolower(input$username)} AND id != {r$user_id}", .con = r$db)
        username_already_used <- nrow(DBI::dbGetQuery(r$db, sql_username)) > 0
        
        if (username_already_used) shiny.fluent::updateTextField.shinyInput(session, "username", errorMessage = i18n$t("username_already_used"))
        
        sql_fullname <- glue::glue_sql(
          "SELECT id FROM users WHERE LOWER(firstname) = {tolower(input$firstname)} AND LOWER(lastname) = {tolower(input$lastname)} AND id != {r$user_id}", 
          .con = r$db
        )
        fullname_already_used <- nrow(DBI::dbGetQuery(r$db, sql_fullname)) > 0
        
        if (fullname_already_used) {
          shiny.fluent::updateTextField.shinyInput(session, "firstname", errorMessage = i18n$t("firstname_and_lastname_already_used"))
          shiny.fluent::updateTextField.shinyInput(session, "lastname", errorMessage = i18n$t("firstname_and_lastname_already_used"))
        }
        
        if (username_already_used || fullname_already_used) return()
        
        sql <- glue::glue_sql("UPDATE users SET username = {input$username}, firstname = {input$firstname}, lastname = {input$lastname} WHERE id = {r$user_id}", .con = r$db)
        DBI::dbExecute(r$db, sql)
        
        show_message_bar("modif_saved", "success")
      }
      
      else if (current_tab == "code_editor"){
        sql <- glue::glue_sql("DELETE FROM options WHERE category = 'user_settings' AND link_id = {r$user_id}", .con = r$db)
        sql_send_statement(r$db, sql)
        
        last_row <- get_last_row(r$db, "options")
        
        new_data <- tibble::tribble(
          ~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
          last_row + 1, "user_settings", r$user_id, "ace_theme", input$ace_theme, NA_real_, NA_real_, now(), FALSE,
          last_row + 2, "user_settings", r$user_id, "ace_font_size", NA_character_, input$ace_font_size, NA_real_, now(), FALSE
        )
        
        DBI::dbAppendTable(r$db, "options", new_data)
        
        show_message_bar("modif_saved", "success")
      }
    })
    
    # Code editor settings ----
    
    observe_event(input$ace_theme, {
      
      # Update Ace edotir theme
      shinyAce::updateAceEditor(session, "ace_editor", theme = input$ace_theme)
      
      shinyjs::runjs(paste0("$('#", id, "-console_output').removeClass(function(index, className) {
        return (className.match(/\\bace-\\S+/g) || []).join(' ');
      });"))
      
      text_output_theme <- gsub("_", "-", input$ace_theme)
      if (text_output_theme == "terminal") text_output_theme <- paste0(text_output_theme, "-theme")
      shinyjs::addClass("console_output", paste0("ace-", text_output_theme))
    })
    
    observe_event(input$ace_font_size, shinyAce::updateAceEditor(session, "ace_editor", fontSize = input$ace_font_size))
    
    # User profile ----
    
    observe_event(r$user_id, {
      
      sql <- glue::glue_sql("SELECT username, firstname, lastname FROM users WHERE id = {r$user_id}", .con = r$db)
      user_infos <- DBI::dbGetQuery(r$db, sql)
      
      for (field in c("username", "firstname", "lastname")) shiny.fluent::updateTextField.shinyInput(session, field, value = user_infos[[field]])
    })
    
    observe_event(input$change_password, shinyjs::show("change_password_modal"))
    observe_event(input$close_change_password_modal, shinyjs::hide("change_password_modal"))
    
    observe_event(input$confirm_password_update, {
      
      empty_fields <- list()
      for (field in c("current_password", "new_password_1", "new_password_2")) {
        empty_fields[[field]] <- TRUE
        field_value <- input[[field]]
        if (length(field_value) > 0) if (!is.na(field_value) & field_value != "") empty_fields[[field]] <- FALSE
        if (empty_fields[[field]]) shiny.fluent::updateTextField.shinyInput(session, field, errorMessage = i18n$t("provide_valid_value"))
        else shiny.fluent::updateTextField.shinyInput(session, field, errorMessage = NULL)
      }
      if (TRUE %in% empty_fields) return()
      
      sql <- glue::glue_sql("SELECT password FROM users WHERE id = {r$user_id}", .con = r$db)
      result <- DBI::dbGetQuery(r$db, sql)
      
      current_password_correct <- FALSE
      if (nrow(result) > 0) {
        current_password_correct <- tryCatch({
          bcrypt::checkpw(input$current_password, result$password)
        }, error = function(e) FALSE)
      }
      
      if (!current_password_correct) {
        shiny.fluent::updateTextField.shinyInput(session, "current_password", errorMessage = i18n$t("incorrect_password"))
        return()
      }
      
      passwords_match <- input$new_password_1 == input$new_password_2
      if (!passwords_match) {
        shiny.fluent::updateTextField.shinyInput(session, "new_password_2", errorMessage = i18n$t("passwords_dont_match"))
        return()
      }
      
      hashed_password <- bcrypt::hashpw(input$new_password_1)
      
      sql <- glue::glue_sql("UPDATE users SET password = {hashed_password} WHERE id = {r$user_id}", .con = r$db)
      DBI::dbExecute(r$db, sql)
      
      for (field in c("current_password", "new_password_1", "new_password_2")) shiny.fluent::updateTextField.shinyInput(session, field, value = "")
      
      shinyjs::hide("change_password_modal")
      
      show_message_bar("password_successfully_updated", "success")
    })
    
  })
}
