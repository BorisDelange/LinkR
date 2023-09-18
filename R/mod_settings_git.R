#' mod_settings_git UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_git_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  
  cards <- c("git_add_repo_card", "git_repos_management_card")
  
  forbidden_cards <- tagList()
  for (card in cards) forbidden_cards <- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  
  div(class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    shiny.fluent::reactOutput(ns("git_repos_delete_confirm")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "remote_git_repos", text = i18n$t("remote_git_repos"))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "git_add_repo_card", itemKey = "git_add_repo_card", headerText = i18n$t("add_git_repo")),
      shiny.fluent::PivotItem(id = "git_repos_management_card", itemKey = "git_repos_management_card", headerText = i18n$t("git_repos_management")),
    ),
    forbidden_cards,
    
    shinyjs::hidden(
      div(id = ns("git_add_repo_card"),
        make_card(i18n$t("add_git_repo"), div(
          shiny.fluent::Stack(
            horizontal = TRUE, tokens = list(childrenGap = 20),
            make_textfield(i18n = i18n, ns = ns, label = "name", id = "name", width = "300px"),
            # make_textfield(i18n = i18n, ns = ns, label = "description", id = "description", width = "600px")
            make_dropdown(i18n = i18n, ns = ns, label = "category", id = "category", multiSelect = FALSE, width = "300px",
              options = list(
                list(key = "plugin", text = i18n$t("plugins")),
                list(key = "script", text = i18n$t("scripts"))
              ), value = "plugin")
          ),
          shiny.fluent::Stack(
            horizontal = TRUE, tokens = list(childrenGap = 20),
            make_textfield(i18n = i18n, ns = ns, label = "url_address", id = "url_address", width = "620px")
          ), br(),
          shiny.fluent::PrimaryButton.shinyInput(ns("add_git_repo"), i18n$t("add"))
        ))
      )
    ),
    shinyjs::hidden(
      div(id = ns("git_repos_management_card"),
        make_card(i18n$t("git_repos_management"),
          div(
            div(DT::DTOutput(ns("git_repos_datatable")), style = "margin-top:-30px; z-index:2"),
            div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("save_git_repos_management"), i18n$t("save"))#,
                # shiny.fluent::DefaultButton.shinyInput(ns("delete_selection"), i18n$t("delete_selection"))
              ),
              style = "position:relative; z-index:2; margin-top:-30px;"
            )
          )
        ), br()
      )
    ), br()
  )
}
    
#' settings_r_console Server Functions
#'
#' @noRd 

mod_settings_git_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), 
  i18n = character(), language = "en", perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("git_add_repo_card", "git_repos_management_card")
    show_or_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    if ("git_add_repo_card" %in% r$user_accesses) shinyjs::show("git_add_repo_card")
    else shinyjs::show("git_add_repo_card_forbidden")
    
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    observeEvent(input$help, if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r$help_settings_git_open_panel <- TRUE)
    observeEvent(input$hide_panel, r$help_settings_git_open_panel <- FALSE)
    
    r$help_settings_git_open_panel_light_dismiss <- TRUE
    observeEvent(input$show_modal, r$help_settings_git_open_modal <- TRUE)
    observeEvent(input$hide_modal, {
      r$help_settings_git_open_modal <- FALSE
      r$help_settings_git_open_panel_light_dismiss <- TRUE
    })
    
    observeEvent(shiny.router::get_page(), {
      if (debug) print(paste0(Sys.time(), " - mod_settings_git - ", id, " - observer shiny_router::change_page"))

      # Close help pages when page changes
      r$help_settings_git_open_panel <- FALSE
      r$help_settings_git_open_modal <- FALSE
    })
    
    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_settings_git_page_", i)]] <- Sys.time())
    })
    
    help_settings_git(output = output, r = r, id = id, language = language, i18n = i18n, ns = ns)
    
    # --- --- --- --- --- --
    # Create a git repo ----
    # --- --- --- --- --- --
    
    observeEvent(input$add_git_repo, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_settings_git - observer input$add_git_repo"))
        
      new_data <- list()
      for (name in c("name", "description", "category", "url_address")) new_data[[name]] <- coalesce2(type = "char", x = input[[name]])
      
      add_settings_new_data(session = session, output = output, r = r, m = m, i18n = i18n, id = id, 
        data = new_data, table = "git_repos", required_textfields = c("name", "url_address"), req_unique_values = "name")
      
      # Reload datatable
      r$git_repos_temp <- r$git_repos %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_git - observer input$add_git_repo"))
    })
    
    # --- --- --- --- --- -- -
    # Git repo management ----
    # --- --- --- --- --- -- -
    
    action_buttons <- c("delete")
    editable_cols <- c("name", "description", "url_address")
    sortable_cols <- c("name", "creator_id", "datetime")
    column_widths <- c("id" = "80px", "datetime" = "130px", "creator_id" = "200px", "action" = "80px", "category" = "130px")
    centered_cols <- c("creator_id", "datetime", "action", "category")
    searchable_cols <- c("name", "creator_id", "category")
    factorize_cols <- c("creator_id", "category")
    hidden_cols <- c("id", "deleted", "modified", "description")
    col_names <- get_col_names("git_repos", i18n)
    shortened_cols <- c("name" = 30, "url_address" = 30, "creator_id" = 20)
    
    # Prepare data for datatable
    
    observeEvent(r$git_repos, {

      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_settings_git - observer r$git_repos"))

      r$git_repos_temp <- r$git_repos %>% dplyr::mutate(modified = FALSE)
      
      # Reset fields

      if(nrow(r$git_repos_temp) == 0){
        render_datatable(output = output, ns = ns, i18n = i18n,
          data = r$git_repos_temp %>% dplyr::mutate(action = character()),
          col_names = col_names, output_name = "git_repos_datatable", shortened_cols = shortened_cols,
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)
      }

      req(nrow(r$git_repos_temp) > 0)

      # Render datatable
      
      r$git_repos_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
        table = "scripts", factorize_cols = factorize_cols, action_buttons = action_buttons, data_input = r$git_repos_temp) %>%
        dplyr::mutate(category = dplyr::case_when(category == "plugin" ~ i18n$t("plugins"), category == "script" ~ i18n$t("scripts")))
      
      if (length(r$git_repos_datatable_proxy) == 0){
        render_datatable(output = output, ns = ns, i18n = i18n, data = r$git_repos_datatable_temp,
          output_name = "git_repos_datatable", col_names = col_names, shortened_cols = shortened_cols,
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)
        
        # Create a proxy for datatable
        
        r$git_repos_datatable_proxy <- DT::dataTableProxy("git_repos_datatable", deferUntilFlush = FALSE)
      }
      
      else  DT::replaceData(r$git_repos_datatable_proxy, r$git_repos_datatable_temp, resetPaging = FALSE, rownames = FALSE)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_git - observer r$git_repos"))
    })
    
    # Updates on datatable data
    observeEvent(input$git_repos_datatable_cell_edit, {

      if (debug) print(paste0(Sys.time(), " - mod_settings_git - observer input$git_repos_datatable_cell_edit"))

      edit_info <- input$git_repos_datatable_cell_edit
      r$git_repos_temp <- DT::editData(r$git_repos_temp, edit_info, rownames = FALSE)

      # Store that this row has been modified
      r$git_repos_temp[[edit_info$row, "modified"]] <- TRUE
    })
    
    # Save updates
    observeEvent(input$save_git_repos_management, {

      if (debug) print(paste0(Sys.time(), " - mod_settings_git - observer input$save_git_repos_management"))

      req(nrow(r$git_repos) > 0)

      save_settings_datatable_updates(output = output, r = r, ns = ns, table = "git_repos", i18n = i18n, duplicates_allowed = FALSE)
      
      # Reload datatable
      r$git_repos_temp <- r$git_repos_temp %>% dplyr::mutate(modified = FALSE)
    })
    
    # Delete a row in datatable
    
    git_repos_delete_prefix <- "git_repos"
    git_repos_dialog_title <- "git_repos_delete"
    git_repos_dialog_subtext <- "git_repos_delete_subtext"
    git_repos_react_variable <- "git_repos_delete_confirm"
    git_repos_table <- "git_repos"
    git_repos_id_var_sql <- "id"
    git_repos_id_var_r <- "delete_git_repo"
    git_repos_delete_message <- "git_repo_deleted"
    git_repos_reload_variable <- "reload_git_repos"
    git_repos_information_variable <- "git_repo_deleted"
    git_repos_delete_variable <- paste0(git_repos_delete_prefix, "_open_dialog")
    
    delete_element(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
      delete_prefix = git_repos_delete_prefix, dialog_title = git_repos_dialog_title, dialog_subtext = git_repos_dialog_subtext,
      react_variable = git_repos_react_variable, table = git_repos_table, id_var_sql = git_repos_id_var_sql, id_var_r = git_repos_id_var_r,
      delete_message = git_repos_delete_message, translation = TRUE, reload_variable = git_repos_reload_variable,
      information_variable = git_repos_information_variable)
    
    observeEvent(input$deleted_pressed, {

      if (debug) print(paste0(Sys.time(), " - mod_settings_git - observer input$deleted_pressed"))

      r$delete_git_repo <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
      r[[git_repos_delete_variable]] <- TRUE

      # Reload datatable (to unselect rows)
      DT::replaceData(r$git_repos_datatable_proxy, r$git_repos_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    })
    
  })
}
