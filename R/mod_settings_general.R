#' settings_general UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_general_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  
  cards <- c("change_password_card", "configure_python_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  })
  
  div(class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "general_settings", text = i18n$t("general_settings"))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "change_password_card", itemKey = "change_password", headerText = i18n$t("change_password")),
      shiny.fluent::PivotItem(id = "configure_python_card", itemKey = "configure_python", headerText = i18n$t("configure_python"))
    ),
    forbidden_cards,
    
    # --- --- --- --- --- --- -
    # Change password card ----
    # --- --- --- --- --- --- -
    
    shinyjs::hidden(
      div(id = ns("change_password_card"),
        make_card(i18n$t("change_password"),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
              make_textfield(i18n = i18n, ns = ns, label = "old_password", type = "password", canRevealPassword = TRUE, width = "300px"),
              make_textfield(i18n = i18n, ns = ns, label = "new_password", type = "password", canRevealPassword = TRUE, width = "300px"),
              make_textfield(i18n = i18n, ns = ns, label = "new_password", id = "new_password_bis", type = "password", canRevealPassword = TRUE, width = "300px")
            ), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("save"), i18n$t("save"))
          )
        )
      )
    ),
    
    # --- --- --- --- --- -
    # Configure Python ----
    # --- --- --- --- --- -
    
    shinyjs::hidden(
      div(id = ns("configure_python_card"),
        make_card(i18n$t("configure_python"),
          div(
            make_textfield(i18n = i18n, ns = ns, label = "python_path", width = "600px"), br(),
            uiOutput(ns("python_path_test_result")),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::PrimaryButton.shinyInput(ns("save_python_path"), i18n$t("save")),
              shiny.fluent::DefaultButton.shinyInput(ns("test_python_path"), i18n$t("test_python_path"))
            )
          )
        )
      )
    )
  )
}
    
#' settings_general Server Functions
#'
#' @noRd 

mod_settings_general_server <- function(id = character(), r = shiny::reactiveValues(), i18n = character(),
  perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (debug) cat(paste0("\n", now(), " - mod_settings_general - start"))
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("change_password_card", "configure_python_card")
    
    show_or_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    
    # Show first card
    if ("change_password_card" %in% r$user_accesses) shinyjs::show("change_password_card")
    else shinyjs::show("change_password_card_forbidden")
    
    # Close message bar
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    observeEvent(input$help, if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r$help_settings_general_open_panel <- TRUE)
    observeEvent(input$hide_panel, r$help_settings_general_open_panel <- FALSE)
    
    r$help_settings_general_open_panel_light_dismiss <- TRUE
    observeEvent(input$show_modal, r$help_settings_general_open_modal <- TRUE)
    observeEvent(input$hide_modal, {
      r$help_settings_general_open_modal <- FALSE
      r$help_settings_general_open_panel_light_dismiss <- TRUE
    })
    
    # observeEvent(shiny.router::get_page(), {
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_general - ", id, " - observer shiny_router::change_page"))
    # 
    #   # Close help pages when page changes
    #   r$help_settings_general_open_panel <- FALSE
    #   r$help_settings_general_open_modal <- FALSE
    # })
    
    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_settings_general_page_", i)]] <- now())
    })
    
    help_settings_general(output = output, r = r, id = id, language = language, i18n = i18n, ns = ns)
    
    # --- --- --- --- -- -
    # Change password ----
    # --- --- --- --- -- -
    
    observeEvent(input$save, {
      
      if (debug) cat(paste0("\n", now(), " - mod_settings_general - observer input$save"))
      
      # Check if textfields are not empty
      
      required_textfields <- c("old_password", "new_password", "new_password_bis")
      
      sapply(required_textfields, function(textfield){
        
        if (length(input[[textfield]]) == 0) shiny.fluent::updateTextField.shinyInput(session, textfield, errorMessage = i18n$t("provide_valid_password"))
        
        else {
          if (is.na(input[[textfield]])) shiny.fluent::updateTextField.shinyInput(session, textfield, errorMessage = i18n$t("provide_valid_password"))
          else shiny.fluent::updateTextField.shinyInput(session, textfield, errorMessage = NULL)
        }
          
        req(length(input[[textfield]]) > 0)
        req(!is.na(input[[textfield]]))
      })
      
      # Check if the two new password fields contains the same value
      
      if (input$new_password != input$new_password_bis){
        shiny.fluent::updateTextField.shinyInput(session, "new_password", errorMessage = i18n$t("new_passwords_are_not_the_same"))
        shiny.fluent::updateTextField.shinyInput(session, "new_password_bis", errorMessage = i18n$t("new_passwords_are_not_the_same"))
      }
      else {
        shiny.fluent::updateTextField.shinyInput(session, "new_password", errorMessage = NULL)
        shiny.fluent::updateTextField.shinyInput(session, "new_password_bis", errorMessage = NULL)
      }
      
      req(input$new_password == input$new_password_bis)
      
      # Check if the old password is OK
      
      old_password <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM users WHERE id = ", r$user_id)) %>% dplyr::pull(password)
      
      if (as.character(rlang::hash(input$old_password)) != old_password) shiny.fluent::updateTextField.shinyInput(session, "old_password", 
        errorMessage = i18n$t("invalid_old_password"))
      
      if (as.character(rlang::hash(input$old_password)) == old_password) {
        
        # Everything is OK, change password
        
        new_password <- as.character(rlang::hash(input$new_password))
        sql <- glue::glue_sql("UPDATE users SET password = {new_password} WHERE id = {r$user_id}", .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql)
        DBI::dbClearResult(query)
        
        # Reset textfields
        shiny.fluent::updateTextField.shinyInput(session, "old_password", value = "")
        shiny.fluent::updateTextField.shinyInput(session, "new_password", value = "")
        shiny.fluent::updateTextField.shinyInput(session, "new_password_bis", value = "")
        
        # Notify the user
        
        show_message_bar(output, message = "password_changed", type = "success", i18n = i18n, ns = ns)
        
      }
      
    })
    
    # --- --- --- --- --- -
    # Configure Python ----
    # --- --- --- --- --- -
    
    observeEvent(r$db, {
      if (debug) cat(paste0("\n", now(), " - mod_settings_general - observer r$db"))
      
      sql <- glue::glue_sql("SELECT * FROM options WHERE category = 'python_config' AND name = 'python_path'")
      python_path <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(value)
      
      shiny.fluent::updateTextField.shinyInput(session, "python_path", value = python_path)
      
      r$python_path_trigger <- now()
    })
    
    observeEvent(input$test_python_path, {
      
      if (debug) cat(paste0("\n", now(), " - mod_settings_general - observer input$test_python_path"))
      
      success <- FALSE
      error_message <- ""
      
      tryCatch({
        reticulate::use_python(input$python_path)
        success <- TRUE
      },
        error = function(e) error_message <<- e)
      
      if (success) output$python_path_test_result <- renderUI(HTML(paste0("<span style = 'margin-top:44px; font-weight:bold; color:#0078D4;'>", i18n$t("success"), "</span><br /><br />")))
      else output$python_path_test_result <- renderUI(HTML(paste0("<span style = 'margin-top:44px; font-weight:bold; color:red;'>", i18n$t("failure"), " - ", error_message, "</span><br /><br />")))
    })
    
    observeEvent(input$save_python_path, {
      
      if (debug) cat(paste0("\n", now(), " - mod_settings_general - observer input$save_python_path"))
      
      sql <- glue::glue_sql("DELETE FROM options WHERE category = 'python_config' AND name = 'python_path'", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      new_row <- tibble::tibble(id = get_last_row(r$db, "options") + 1, category = "python_config", link_id = NA_integer_,
        name = "python_path", value = input$python_path, value_num = NA_integer_, creator_id = r$user_id, 
        datetime = now(), deleted = FALSE)
      DBI::dbAppendTable(r$db, "options", new_row)
      
      show_message_bar(output, message = "modif_saved", type = "success", i18n = i18n, ns = ns)
      
      r$python_path_trigger <- now()
    })
  })
}
