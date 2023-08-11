#' settings_users UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_settings_users_ui <- function(id = character(), i18n = character(), users_accesses_toggles_options = tibble::tibble()){
  ns <- NS(id)
  
  # Three distinct pages in the settings/users page : users, accesses & statuses
  # For each "sub page", create a creation & a management cards
  
  cards <- tagList()
  
  # We create one tab by "sub page"
  
  cards_names <- c(
    "users_creation_card", "users_management_card", 
    "users_accesses_management_card", "users_accesses_options_card",
    "users_statuses_management_card")
  
  sapply(cards_names, function(card) cards <<- tagList(cards, 
    div(id = ns(card), mod_settings_sub_users_ui(id = paste0("settings_users_", substr(card, 1, nchar(card) - 5)), i18n = i18n, users_accesses_toggles_options = users_accesses_toggles_options))))
  
  pivots <- tagList()
  forbidden_cards <- tagList()
  sapply(cards_names, function(card){
    pivots <<- tagList(pivots, shiny.fluent::PivotItem(id = card, itemKey = card, headerText = i18n$t(card)))
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  })
  
  div(class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "users", text = i18n$t("users"))
    ), maxDisplayedItems = 3),
    div(id = ns("pivot"),
      shiny.fluent::Pivot(
        id = ns("users_pivot"),
        onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
        pivots
      )
    ),
    forbidden_cards,
    cards
  )
}

mod_settings_sub_users_ui <- function(id = character(), i18n = character(), users_accesses_toggles_options = tibble::tibble()){
  ns <- NS(id)
  
  page <- substr(id, nchar("settings_users_") + 1, nchar(id))
  
  if (page == "users_creation"){
    div(id = ns("creation_card"),
      make_card(
        i18n$t(page),
        div(
          shiny.fluent::Stack(
            horizontal = TRUE, tokens = list(childrenGap = 20),
            make_textfield(i18n = i18n, ns = ns, label = "username", id = "username", width = "300px"),
            make_textfield(i18n = i18n, ns = ns, label = "firstname", id = "firstname", width = "300px"),
            make_textfield(i18n = i18n, ns = ns, label = "lastname", id = "lastname", width = "300px")
          ),
          shiny.fluent::Stack(
            horizontal = TRUE, tokens = list(childrenGap = 20),
            make_textfield(i18n = i18n, ns = ns, label = "password", id = "password", width = "300px", type = "password", canRevealPassword = TRUE),
            make_dropdown(i18n = i18n, ns = ns, label = "user_access", id = "user_access", multiSelect = FALSE, width = "300px"),
            make_dropdown(i18n = i18n, ns = ns, label = "user_status", id = "user_status", multiSelect = FALSE, width = "300px")
          ), br(),
          shiny.fluent::PrimaryButton.shinyInput(ns("add"), i18n$t("add"))
        )
      )
    ) -> result
  }
  
  if (page == "users_management") result <- render_settings_datatable_card(i18n = i18n, ns = ns, title = page)
  
  if (page %in% c("users_accesses_management", "users_statuses_management")) result <- render_settings_datatable_card(i18n = i18n, ns = ns, title = page, 
    inputs = c("name" = "textfield", "description" = "textfield"))
  
  if (page == "users_accesses_options"){
    
    # Create users_accesses_toggles_options_result
    
    users_accesses_toggles_options_result <- tagList()
    
    for (i in 1:nrow(users_accesses_toggles_options)){
      sub_results <- tagList()
      
      if (users_accesses_toggles_options[[i, "toggles"]] != ""){
        # j <- 0
        for (toggle in users_accesses_toggles_options[[i, "toggles"]][[1]]){

          # Create toggle
          sub_results <- tagList(sub_results, shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), 
            make_toggle(i18n = i18n, ns = ns, label = paste0(toggle, "_user_access_description"), id = paste0("toggle_", toggle), inline = TRUE, value = FALSE, bold = FALSE)))
        }
      }
        
      label <- users_accesses_toggles_options[[i, "name"]]
      
      users_accesses_toggles_options_result <- tagList(users_accesses_toggles_options_result, br(), shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
        make_toggle(i18n = i18n, ns = ns, label = label, id = paste0("toggle_", label), inline = TRUE, value = FALSE)),
        conditionalPanel(condition = paste0("input.toggle_", label, " == 1"), ns = ns, br(), sub_results), hr()
      )
    }
    
    tagList(
      forbidden_card(ns = ns, name = "options_card", i18n = i18n),
      div(id = ns("options_card"),
        make_card(i18n$t("accesses_opts"),
          div(
            make_combobox(i18n = i18n, ns = ns, label = "user_access", id = "options_selected",
              width = "300px", allowFreeform = FALSE, multiSelect = FALSE), br(),
            users_accesses_toggles_options_result, br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::DefaultButton.shinyInput(ns("select_all"), i18n$t("select_all")),
              shiny.fluent::DefaultButton.shinyInput(ns("unselect_all"), i18n$t("unselect_all")),
              shiny.fluent::PrimaryButton.shinyInput(ns("options_save"), i18n$t("save")))
          )
        )    
      )
    ) -> result
  }
  
  tagList(render_settings_default_elements(ns = ns), result, br())
}
    
#' settings_users Server Functions
#'
#' @noRd 
mod_settings_users_server <- function(id = character(), r = shiny::reactiveValues(), m = shiny::reactiveValues(), 
  i18n = character(), language = "en", perf_monitoring = FALSE, debug = FALSE, users_accesses_toggles_options = tibble::tibble()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (id == "settings_users"){
      sapply(c("users", "users_accesses", "users_statuses"), function(table) observeEvent(r[[paste0(table, "_show_message_bar")]], 
        show_message_bar(output, r[[paste0(table, "_show_message_bar")]]$message, r[[paste0(table, "_show_message_bar")]]$type, i18n = i18n, ns = ns)))
    }
    
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    cards <- c(
      "users_creation_card", "users_management_card", 
      "users_accesses_creation_card", "users_accesses_management_card", "users_accesses_options_card",
      "users_statuses_creation_card", "users_statuses_management_card")
    
    sapply(cards, shinyjs::hide)
    sapply(cards, function(card) shinyjs::hide(paste0(card, "_forbidden")))
    
    # Show first card
    if ("users_creation_card" %in% r$user_accesses) shinyjs::show("users_creation_card")
    else shinyjs::show("users_creation_card_forbidden")
    
    # Current page
    page <- substr(id, nchar("settings_users_") + 1, nchar(id))
    
    # Corresponding table in the database
    if (grepl("creation", page)) table <- substr(page, 1, nchar(page) - nchar("_creation"))
    if (grepl("management", page)) table <- substr(page, 1, nchar(page) - nchar("_management"))
    if (grepl("options", page)) table <- substr(page, 1, nchar(page) - nchar("_options"))
    
    # Dropdowns used for creation card
    dropdowns <- ""
    if (page %in% c("users_creation", "users_management")) dropdowns <- c("user_access", "user_status")
    
    # --- --- --- --- --- -
    # Update dropdowns ----
    # --- --- --- --- --- -
    
    if (page == "users_accesses_options"){
      
      observeEvent(r$users_accesses, {
        
        if (debug) print(paste0(Sys.time(), " - mod_settings_users - observer r$users_accesses"))
          
        options <- convert_tibble_to_list(r$users_accesses %>% dplyr::arrange(name), key_col = "id", text_col = "name")
        
        shiny.fluent::updateComboBox.shinyInput(session, "options_selected", options = options)
      })
    }
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    # Only for main users page (not for sub-pages)
    if (id == "settings_users"){
      
      # Depending on user_accesses
      observeEvent(r$user_accesses, {
        
        if (debug) print(paste0(Sys.time(), " - mod_settings_users - observer r$user_accesses"))
        
        # Hide Pivot if user has no access
        if ("users" %not_in% r$user_accesses) shinyjs::hide("pivot") else shinyjs::show("pivot")
      })

      # Depending on cards activated
      
      show_or_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    }
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    observeEvent(input$help, if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r$help_settings_users_open_panel <- TRUE)
    observeEvent(input$hide_panel, r$help_settings_users_open_panel <- FALSE)
    
    r$help_settings_users_open_panel_light_dismiss <- TRUE
    observeEvent(input$show_modal, r$help_settings_users_open_modal <- TRUE)
    observeEvent(input$hide_modal, {
      r$help_settings_users_open_modal <- FALSE
      r$help_settings_users_open_panel_light_dismiss <- TRUE
    })
    
    observeEvent(shiny.router::get_page(), {
      if (debug) print(paste0(Sys.time(), " - mod_settings_users - ", id, " - observer shiny_router::change_page"))
      
      # Close help pages when page changes
      r$help_settings_users_open_panel <- FALSE
      r$help_settings_users_open_modal <- FALSE
      
      # Load users_management_card, to load DT (doesn't update with other DT if not already loaded once)
      if (shiny.router::get_page() == "settings/users" & length(r$settings_users_page_loaded) == 0){
        if ("users_management_card" %in% r$user_accesses) shinyjs::show("users_management_card")
        shinyjs::delay(500, shinyjs::hide("users_management_card"))
        r$settings_users_page_loaded <- TRUE
      }
    })
    
    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_settings_users_page_", i)]] <- Sys.time())
    })
    
    help_settings_users(output = output, r = r, id = id, language = language, i18n = i18n, ns = ns)
    
    # --- --- --- --- --- --
    # Add a new element ----
    # --- --- --- --- --- --
    
    # Only for creation subpages
    if (grepl("creation|users_accesses_management|users_statuses_management", id)){
      
      # Update dropdowns with reactive data
      sapply(c("users_accesses", "users_statuses"), 
        function(data_var){
          observeEvent(r[[data_var]], {
            
            if (debug) print(paste0(Sys.time(), " - mod_settings_users - observer r$users_[accesses/statuses]"))
            
            # Convert options to list
            options <- convert_tibble_to_list(data = r[[data_var]], key_col = "id", text_col = "name")
            shiny.fluent::updateDropdown.shinyInput(session, get_singular(word = data_var), options = options)
          })
        })
      
      # When add button is clicked
      observeEvent(input$add, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_settings_users - observer input$add"))
        
        # If user has access
        req(paste0(table, "_creation_card") %in% r$user_accesses)
        
        new_data <- list()
        
        new_data_var <- c("username" = "char", "firstname" = "char", "lastname" = "char", "password" = "char",
          "user_access" = "int", "user_status" = "int", "name" = "char", "description" = "char")
        
        # Transform values of textfields & dropdowns to selected variable type
        sapply(names(new_data_var),
          function(input_name){
            new_data[[input_name]] <<- coalesce2(type = new_data_var[[input_name]], x = input[[input_name]])
          })
        
        # Required textfields
        required_textfields <- switch(table, 
          "users" = c("username", "firstname", "lastname", "password"),
          "users_accesses" = "name",
          "users_statuses" = "name")
        
        # Fields requiring unique value
        req_unique_values <- switch(table, "users" = "username", "users_accesses" = "name", "users_statuses" = "name")
        
        add_settings_new_data(session = session, output = output, r = r, m = m, i18n = i18n, id = id, data = new_data, table = table, 
          required_textfields = required_textfields, req_unique_values = req_unique_values, dropdowns = dropdowns, r_message_bar = TRUE)
        
        # Reload datatable
        r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_users - observer input$add"))
      })
    }
    
    # --- --- --- --- --- ---
    # Generate datatable ----
    # --- --- --- --- --- ---
    
    # Only for data management subpages
    if (grepl("management", id)){
      
      # Dropdowns for each tab / page
      dropdowns_datatable <- switch(table, "users" = c("user_access_id" = "users_accesses", "user_status_id" = "users_statuses"),
        "users_accesses" = "", "users_statuses" = "")
      
      # Action buttons for each tab / page
      action_buttons = switch(table, "users" = "delete", "users_accesses" = c("options", "delete"), "users_statuses" = "delete")
      sortable_cols <- c("id", "name", "description", "username", "firstname", "lastname", "datetime")
      column_widths <- c("id" = "80px", "datetime" = "130px", "action" = "80px")
      editable_cols <- switch(table, "users" = c("username", "firstname", "lastname"),
        "users_accesses" = c("name", "description"), "users_statuses" = c("name", "description"))
      centered_cols <- c("id", "user_access_id", "user_status_id", "datetime", "action")
      searchable_cols <- c("name", "description", "username", "firstname", "lastname")
      
      # If r variable already created, or not
      # if (length(r[[paste0(table, "_datatable_temp")]]) == 0) data_output <- tibble::tibble()
      # else data_output <- r[[paste0(table, "_datatable_temp")]]
      
      # Prepare data for datatable (add code for dropdowns etc)
      r[[paste0(table, "_datatable_temp")]] <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
        table = table, dropdowns = dropdowns_datatable, action_buttons = action_buttons, data_input = r[[paste0(table, "_temp")]])
      
      hidden_cols <- c("id", "password", "deleted", "modified")
      
      # Render datatable
      render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = r[[paste0(table, "_datatable_temp")]],
        output_name = "management_datatable", col_names =  get_col_names(table_name = table, i18n = i18n),
        editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
        searchable_cols = searchable_cols, filter = TRUE, hidden_cols = hidden_cols, selection = "multiple")
      
      # Create a proxy for datatatable
      r[[paste0(table, "_datatable_proxy")]] <- DT::dataTableProxy("management_datatable", deferUntilFlush = FALSE)
      
      # Reload datatable
      
      observeEvent(r[[table]], {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_settings_users - observer r$..[table]"))
        
        r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
        
        # Reload datatable_temp variable
        r[[paste0(table, "_datatable_temp")]] <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
          table = table, dropdowns = dropdowns_datatable, action_buttons = action_buttons, data_input = r[[paste0(table, "_temp")]])
        
        # Reload data of datatable
        DT::replaceData(r[[paste0(table, "_datatable_proxy")]], r[[paste0(table, "_datatable_temp")]], resetPaging = FALSE, rownames = FALSE)
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_users - observer r$..[table]"))
      })
    }
    
    # --- --- --- --- --- --- --- --
    # Save updates in datatable ----
    # --- --- --- --- --- --- --- --
    
    # Only for data management subpages
    if (grepl("management", id)){
      
      # Each time a row is updated, modify temp variable
      observeEvent(input$management_datatable_cell_edit, {
        
        if (debug) print(paste0(Sys.time(), " - mod_settings_users - observer input$management_datatable_cell_edit"))
        
        edit_info <- input$management_datatable_cell_edit
        r[[paste0(table, "_temp")]] <- DT::editData(r[[paste0(table, "_temp")]], edit_info, rownames = FALSE)
        # Store that this row has been modified
        r[[paste0(table, "_temp")]][[edit_info$row, "modified"]] <- TRUE
      })
      
      # Each time a dropdown is updated, modify temp variable
      if (table == "users"){
        observeEvent(r$users, {
          
          if (perf_monitoring) monitor_perf(r = r, action = "start")
          if (debug) print(paste0(Sys.time(), " - mod_settings_users - observer r$users"))
          
          update_settings_datatable(input = input, tab_id = id, r = r, ns = ns, table = table, dropdowns = dropdowns, i18n = i18n)
          
          if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_users - observer r$users"))
        })
      }
  
      # When save button is clicked
      observeEvent(input$management_save, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_settings_users - observer input$management_save"))
        
        save_settings_datatable_updates(output = output, r = r, ns = ns, table = table, i18n = i18n, r_message_bar = TRUE)
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_users - observer input$management_save"))
      })
    }
    
    # --- --- --- --- --- --- --- --
    # Delete a row in datatable ----
    # --- --- --- --- --- --- --- --
    
    # Only for data management subpages
    if (grepl("management", id)){
      
      delete_prefix <- table
      dialog_title <- paste0(table, "_delete")
      dialog_subtext <- paste0(table, "_delete_subtext")
      react_variable <- "delete_confirm"
      id_var_sql <- "id"
      id_var_r <- paste0("delete_", table)
      delete_message <- paste0(get_singular(table), "_deleted")
      reload_variable <- paste0("reload_" , table)
      information_variable <- paste0(table, "_deleted")
      delete_variable <- paste0(table, "_open_dialog")
      
      delete_element(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
        delete_prefix = delete_prefix, dialog_title = dialog_title, dialog_subtext = dialog_subtext,
        react_variable = react_variable, table = table, id_var_sql = id_var_sql, id_var_r = id_var_r,
        delete_message = delete_message, translation = TRUE, reload_variable = reload_variable,
        information_variable = information_variable, r_message_bar = TRUE)
      
      # Delete one row (with icon on DT)
      
      observeEvent(input$deleted_pressed, {
        
        if (debug) print(paste0(Sys.time(), " - mod_settings_users - observer input$deleted_pressed"))
        
        r[[paste0("delete_", table)]] <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
        r[[delete_variable]] <- TRUE
        
        # Reload datatable (to unselect rows)
        DT::replaceData(r[[paste0(table, "_datatable_proxy")]], r[[paste0(table, "_datatable_temp")]], resetPaging = FALSE, rownames = FALSE)
      })
      
      # Delete multiple rows (with "Delete selection" button)
      
      observeEvent(input$delete_selection, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_settings_users - observer input$delete_selection"))
        
        req(length(input[["management_datatable_rows_selected"]]) > 0)
        
        r[[paste0("delete_", table)]] <- r[[paste0(table, "_temp")]][input[["management_datatable_rows_selected"]], ] %>% dplyr::pull(id)
        r[[delete_variable]] <- TRUE
      })
    }
  
    # --- --- --- --- --- --- --- --- -- -
    # Edit options by selecting a row ----
    # --- --- --- --- --- --- --- --- -- -

    # Only for accesses sub-page
    # We have to use same tab than management, to get input from datatable
    if (page == "users_accesses_management"){
    
      observeEvent(input$options, {
        
        if (debug) print(paste0(Sys.time(), " - mod_settings_users - observer input$options"))
        
        # Show options toggle
        r$users_statuses_options <- as.integer(substr(input$options, nchar("options_") + 1, nchar(input$options)))

        # Reload datatable (to unselect rows)
        DT::replaceData(r[[paste0(table, "_datatable_proxy")]], r[[paste0(table, "_datatable_temp")]], resetPaging = FALSE, rownames = FALSE)
      })
    }
    
    if (page == "users_accesses_options"){
      
      observeEvent(r$users_statuses_options, {
        
        if (debug) print(paste0(Sys.time(), " - mod_settings_users - observer r$users_statuses_options"))

        options <- convert_tibble_to_list(r$users_accesses %>% dplyr::arrange(name), key_col = "id", text_col = "name")
        value <- list(key = r$users_statuses_options, text = r$users_accesses %>% dplyr::filter(id == r$users_statuses_options) %>% dplyr::pull(name))

        shiny.fluent::updateComboBox.shinyInput(session, "options_selected", options = options, value = value)
      })

      observeEvent(input$options_selected, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_settings_users - observer input$options_selected"))

        req(input$options_selected)
        if (length(input$options_selected) > 1) link_id <- input$options_selected$key
        else link_id <- input$options_selected

        # Get current data
        current_data <- DBI::dbGetQuery(r$db, paste0("SELECT name, value_num FROM options WHERE category = 'users_accesses' AND ",
          "link_id = ", link_id, " AND value_num = 1"))

        # Inactivate all toggles
        
        for (i in 1:nrow(users_accesses_toggles_options)){
          
          shiny.fluent::updateToggle.shinyInput(session, paste0("toggle_", users_accesses_toggles_options[[i, "name"]]), value = FALSE)
          
          if (users_accesses_toggles_options[[i, "toggles"]] != ""){
            for (toggle in users_accesses_toggles_options[[i, "toggles"]][[1]]){
              shiny.fluent::updateToggle.shinyInput(session, paste0("toggle_", toggle), value = FALSE)
            }
          }
        }
        
        # Activate toggles with loaded data
        
        sapply(1:nrow(current_data), function(i){
          toggle <- current_data[i, "name"]
          
          shiny.fluent::updateToggle.shinyInput(session, paste0("toggle_", toggle), value = TRUE)
        })
        
        shinyjs::runjs(glue::glue("$('#settings_users-users_pivot button[name=\"{i18n$t('users_accesses_options_card')}\"]').click();"))

        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_users - observer input$options_selected"))
      })

      # When select all button is clicked, put all toggles to TRUE
      observeEvent(input$select_all,{
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_settings_users - observer input$select_all"))

        req(input$options_selected)
        r$reload_all_users_accesses_toggles <- Sys.time()
        r$reload_all_users_accesses_toggles_value <- TRUE
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_users - observer input$select_all"))
      })

      # When unselect all button is clicked, put all toggles to FALSE
      observeEvent(input$unselect_all,{
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_settings_users - observer input$unselect_all"))
        
        req(input$options_selected)
        r$reload_all_users_accesses_toggles <- Sys.time()
        r$reload_all_users_accesses_toggles_value <- FALSE
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_users - observer input$unselect_all"))
      })
      
      observeEvent(r$reload_all_users_accesses_toggles, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_settings_users - observer r$reload_all_users_accesses_toggles"))
        
        value <- r$reload_all_users_accesses_toggles_value
        
        for (i in 1:nrow(users_accesses_toggles_options)){
          
          shiny.fluent::updateToggle.shinyInput(session, paste0("toggle_", users_accesses_toggles_options[[i, "name"]]), value = value)
          
          if (users_accesses_toggles_options[[i, "toggles"]] != ""){
            for (toggle in users_accesses_toggles_options[[i, "toggles"]][[1]]){
              shiny.fluent::updateToggle.shinyInput(session, paste0("toggle_", toggle), value = value)
            }
          }
        }
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_users - observer r$reload_all_users_accesses_toggles"))
      })

      # When save button is clicked
      observeEvent(input$options_save, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_settings_users - observer input$options_save"))

        req(input$options_selected)
        if (length(input$options_selected) > 1) link_id <- input$options_selected$key
        else link_id <- input$options_selected

        # Create a data variable to insert data in database
        data <- tibble::tibble(category = character(), link_id = integer(), name = character(), value = character(),
          value_num = numeric(), creator_id = integer(), datetime = character(), deleted = logical())

        # Loop over all toggles, set 0 to value_num is toggle is FALSE, 1 else
        for (i in 1:nrow(users_accesses_toggles_options)){
          data <- data %>% dplyr::bind_rows(
            tibble::tribble(~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
              "users_accesses", as.integer(link_id), users_accesses_toggles_options[[i, "name"]], "", as.numeric(input[[paste0("toggle_", users_accesses_toggles_options[[i, "name"]])]]),
              r$user_id, as.character(Sys.time()), FALSE)
          )
          if (users_accesses_toggles_options[[i, "toggles"]] != ""){
            for (toggle in users_accesses_toggles_options[[i, "toggles"]][[1]]){

              value_num <- as.numeric(isolate(input[[paste0("toggle_", toggle)]]))

              # If category toggle is FALSE, set children to FALSE
              if (isolate(input[[paste0("toggle_", users_accesses_toggles_options[[i, "name"]])]] == 0)) value_num <- 0
              
              data <- data %>% dplyr::bind_rows(
                tibble::tribble(~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
                  "users_accesses", as.integer(link_id), toggle, "", value_num,
                  r$user_id, as.character(Sys.time()), FALSE)
              )
            }
          }
        }

        # Delete old data from options

        sql <- paste0("DELETE FROM options WHERE category = 'users_accesses' AND link_id = ", link_id)
        query <- DBI::dbSendStatement(r$db, sql)
        DBI::dbClearResult(query)
        r$options <- r$options %>% dplyr::filter(category != "users_accesses" | (category == "users_accesses" & link_id != !!link_id))
        add_log_entry(r = r, category = "SQL query", name = "Update users accesses", value = toString(sql))

        # Attribute id values

        last_row <- get_last_row(con = r$db, table = "options")
        data$id <- seq.int(nrow(data)) + last_row

        # Add new values to database
        DBI::dbAppendTable(r$db, "options", data)
        add_log_entry(r = r, category = "SQL query", name = "Update users accesses", value = toString(data))
        r$options <- r$options %>% dplyr::bind_rows(data)

        # Notify user
        r[[paste0(table, "_show_message_bar")]] <- tibble::tibble(message = "modif_saved", type = "success", trigger = Sys.time())

        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_users - observer input$options_save"))
      })
    }
    
  })
}
