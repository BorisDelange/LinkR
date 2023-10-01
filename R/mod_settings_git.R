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
          shiny.fluent::Pivot(
            id = ns("git_add_repo_pivot"),
            onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-add_repo_tab', item.props.id)")),
            shiny.fluent::PivotItem(id = "git_add_repo_map", itemKey = "git_add_repo_map", headerText = i18n$t("with_map")),
            shiny.fluent::PivotItem(id = "git_add_repo_url", itemKey = "git_add_repo_url", headerText = i18n$t("with_url")),
          ),
          conditionalPanel(condition = "input.add_repo_tab == null || input.add_repo_tab == 'git_add_repo_map'", ns = ns, br(),
            leaflet::leafletOutput(ns("git_repos_map"), height = 600),
            conditionalPanel(condition = "input.git_repos_map_marker_click != null", ns = ns,
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), br(),
                div(
                  id = ns("api_key_git_repo_with_map_div"),
                  shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                    div(strong(i18n$t("api_key")), style = "margin-top:5px;"),
                    div(shiny.fluent::TextField.shinyInput(ns("api_key_git_repo_with_map")), style = "width:600px;")
                  ),
                  style = "margin-top:15px; margin-left:0px;"
                ),
                div(shiny.fluent::PrimaryButton.shinyInput(ns("add_git_repo_with_map"), i18n$t("add")), style = "margin-top:15px;")
              ), br(),
              div(
                uiOutput(ns("render_git_repo_description_with_map")),
                style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px; padding-top: 10px;"
              )
            )
          ),
          conditionalPanel(condition = "input.add_repo_tab == 'git_add_repo_url'", ns = ns,
            make_textfield(i18n = i18n, ns = ns, label = "name", id = "name", width = "300px"),
            make_textfield(i18n = i18n, ns = ns, label = "url_address", id = "url_address", width = "600px"), 
            make_textfield(i18n = i18n, ns = ns, label = "api_key", id = "api_key_git_repo_with_url", width = "600px"), br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::PrimaryButton.shinyInput(ns("add_git_repo_with_url"), i18n$t("add")),
              shiny.fluent::DefaultButton.shinyInput(ns("show_git_repo_description"), i18n$t("show_description"))
            ), br(),
            div(
              uiOutput(ns("render_git_repo_description_with_url")),
              style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px; padding-top: 10px;"
            )
          )
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
    
    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_settings_git_page_", i)]] <- Sys.time())
    })
    
    help_settings_git(output = output, r = r, id = id, language = language, i18n = i18n, ns = ns)
    
    # --- --- --- --- --- --
    # Create a git repo ----
    # --- --- --- --- --- --
    
    # Add with map
    
    if (r$has_internet){
      
      ## Download git repos from InterHop git
      
      git_repos <- tibble::tibble()
      
      filename <- "https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/git_repos/git_repos.csv"
      filename_local <- paste0(r$app_folder, "/temp_files/git_repos.csv")
      
      # Get csv file from remote git
      tryCatch(download.file(filename, filename_local, quiet = TRUE),
          error = function(e) report_bug(r = r, output = output, error_message = "error_downloading_git_repos_csv",
            error_name = "settings_git download git_repos.csv", category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
      
      tryCatch(git_repos <- vroom::vroom(filename_local, col_types = "cnncccl", progress = FALSE),
        error = function(e) report_bug(r = r, output = output, error_message = "error_loading_git_repos_csv",
          error_name = "settings_git load git_repos.csv", category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
      
      new_cols <- outer("name", r$languages$code, paste, sep = "_") %>% as.vector()
      for(col in new_cols) if(!col %in% colnames(git_repos)) git_repos <- git_repos %>% dplyr::mutate(!!col := NA_character_)
      
      git_repos <- git_repos %>% dplyr::mutate(name = ifelse(!is.na(get(paste0("name_", language))), get(paste0("name_", language)), name_en))
      
      ## Render leaflet map
      output$git_repos_map <- leaflet::renderLeaflet({
        
        if (debug) print(paste0(Sys.time(), " - mod_settings_git - output$git_repos_map"))
        
        leaflet::leaflet(git_repos) %>%
          leaflet::addTiles() %>%
          leaflet::addProviderTiles("Stadia.Outdoors") %>%
          leaflet::addMarkers(
            lng = ~lng, lat = ~lat,
            clusterOptions = leaflet::markerClusterOptions(),
            popup = ~paste("<strong>", name, "</strong>")
          )
      })
      
      ## When a repo is selected on the map
      observeEvent(input$git_repos_map_marker_click, {
        if (debug) print(paste0(Sys.time(), " - mod_settings_git - observer input$git_repos_map_marker_click"))
        
        # Update API key field
        shiny.fluent::updateTextField.shinyInput(session = session, "api_key_git_repo_with_map", errorMessage = NULL, value = "")
        
        git_repo <- git_repos %>% dplyr::filter(
          sprintf("%.6f", as.numeric(lat)) == sprintf("%.6f", as.numeric(input$git_repos_map_marker_click$lat)),
          sprintf("%.6f", as.numeric(lng)) == sprintf("%.6f", as.numeric(input$git_repos_map_marker_click$lng))
        )
        
        # Disable add button is git repo already in db
        print(git_repo)
        if (r$git_repos %>% dplyr::inner_join(git_repo %>% dplyr::select(unique_id), by = "unique_id") %>% nrow() > 0){
          shiny.fluent::updateActionButton.shinyInput(session = session, "add_git_repo_with_map", disabled = TRUE)
          shinyjs::hide("api_key_git_repo_with_map_div")
        }
        else {
          shiny.fluent::updateActionButton.shinyInput(session = session, "add_git_repo_with_map", disabled = FALSE)
        
          # If an API key is needed
          if (git_repo$api_key_required) shinyjs::show("api_key_git_repo_with_map_div") else shinyjs::hide("api_key_git_repo_with_map_div")
        }
        
        r$show_git_repo_description_trigger <- Sys.time()
        r$show_git_repo_description_type <- "map"
        r$show_git_repo_description_url <- git_repo$url
      })
      
      ## Add with map
      observeEvent(input$add_git_repo_with_map, {
        if (debug) print(paste0(Sys.time(), " - mod_settings_git - observer input$add_git_repo_with_map"))
        
        req(length(input$git_repos_map_marker_click) > 0)
        git_repo <- git_repos %>% dplyr::filter(
          sprintf("%.6f", as.numeric(lat)) == sprintf("%.6f", as.numeric(input$git_repos_map_marker_click$lat)),
          sprintf("%.6f", as.numeric(lng)) == sprintf("%.6f", as.numeric(input$git_repos_map_marker_click$lng))
        )
        
        new_data <- list()
        if (paste0("name_", language) %in% colnames(git_repo)) new_data$name <- git_repo[[paste0("name_", language)]]
        else new_data$name <- git_repo$name_en
        new_data$api_key <- coalesce2(type = "char", x = input$api_key_git_repo_with_map)
        new_data$url_address <- git_repo$url
        new_data$unique_id <- git_repo$unique_id
        
        # Check if git repo is not already in database
        sql <- glue::glue_sql("SELECT * FROM git_repos WHERE unique_id = {new_data$unique_id} AND deleted IS FALSE", .con = r$db)
        check_unique_id <- DBI::dbGetQuery(r$db, sql) %>% nrow() == 0
        if (!check_unique_id) show_message_bar(output, "git_repo_already_in_db", "severeWarning", i18n = i18n, ns = ns)
        req(check_unique_id)
        
        # Check if name is not already taken
        sql <- glue::glue_sql("SELECT * FROM git_repos WHERE name = {new_data$name} AND deleted IS FALSE", .con = r$db)
        check_unique_name <- DBI::dbGetQuery(r$db, sql) %>% nrow() == 0
        if (!check_unique_name) show_message_bar(output, "name_already_used", "severeWarning", i18n = i18n, ns = ns)
        req(check_unique_name)
        
        if (git_repo$api_key_required){
          if (is.na(input$api_key_git_repo_with_map) | input$api_key_git_repo_with_map == "") shiny.fluent::updateTextField.shinyInput(session, "api_key_git_repo_with_map", errorMessage = i18n$t("provide_valid_api_key"))
          else shiny.fluent::updateTextField.shinyInput(session, "api_key_git_repo_with_map", errorMessage = NULL)
          req(!is.na(input$api_key_git_repo_with_map) & input$api_key_git_repo_with_map != "")
        }
        
        add_settings_new_data(session = session, output = output, r = r, m = m, i18n = i18n, id = id, data = new_data, table = "git_repos")
        
        # Update & hide API key field
        shiny.fluent::updateTextField.shinyInput(session = session, "api_key_git_repo_with_map", errorMessage = NULL, value = "")
        shinyjs::hide("api_key_git_repo_with_map_div")
        
        # Disable add button
        shiny.fluent::updateActionButton.shinyInput(session = session, "add_git_repo_with_map", disabled = TRUE)
        
        # Reload datatable
        r$git_repos_temp <- r$git_repos %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      })
    }
    
    # Add with URL
    
    ## Show description
    observeEvent(input$show_git_repo_description, {
      if (debug) print(paste0(Sys.time(), " - mod_settings_git - observer input$show_git_repo_description"))
      
      req(length(input$url_address) > 0)
      
      r$show_git_repo_description_trigger <- Sys.time()
      r$show_git_repo_description_type <- "url"
      r$show_git_repo_description_url <- input$url_address
    })
    
    ## Add git repo
    observeEvent(input$add_git_repo_with_url, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_settings_git - observer input$add_git_repo_with_url"))
      
      new_data <- list()
      for (name in c("name", "api_key", "url_address")) new_data[[name]] <- coalesce2(type = "char", x = input[[name]])
      
      add_settings_new_data(session = session, output = output, r = r, m = m, i18n = i18n, id = id, 
        data = new_data, table = "git_repos", required_textfields = c("name", "url_address"), req_unique_values = "name")
      
      # Reload datatable
      r$git_repos_temp <- r$git_repos %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_git - observer input$add_git_repo_with_url"))
    })
    
    # Show git repo description (for map & URL)
    
    observeEvent(r$show_git_repo_description_trigger , {
      if (debug) print(paste0(Sys.time(), " - mod_settings_git - observer r$show_git_repo_description_trigger "))
      
      readme <- div(shiny.fluent::MessageBar(i18n$t("error_loading_git_readme"), messageBarType = 5), style = "margin-bottom:8px;")
      
      # Get README on git repo
      tryCatch({
        dir <- paste0(r$app_folder, "/temp_files")
        new_file <- paste0(dir, "/", paste0(sample(c(0:9, letters[1:6]), 8, TRUE), collapse = ''), "_README.md")
        if (!dir.exists(dir)) dir.create(dir)
        
        filename_remote <- paste0(r$show_git_repo_description_url, "/README.md")
        
        download.file(filename_remote, new_file, quiet = TRUE)
        
        con <- textConnection(new_file)
        readme <- div(readLines(con, warn = FALSE) %>% includeMarkdown() %>% withMathJax(), style = "margin-top:-15px;")
        close(con)
      },
        error = function(e) add_log_entry(r = r, category = "Error", name = paste0(id, " - load git repo description"), value = toString(e)),
        warning = function(w) add_log_entry(r = r, category = "Error", name = paste0(id, " - load git repo description"), value = toString(w)))
      
      # Render UI
      output[[paste0("render_git_repo_description_with_", r$show_git_repo_description_type)]] <- renderUI({
        if (debug) print(paste0(Sys.time(), " - mod_settings_git - output$render_git_repo_description_with_map"))
        readme
      })
    })
    
    # --- --- --- --- --- -- -
    # Git repo management ----
    # --- --- --- --- --- -- -
    
    action_buttons <- c("delete")
    editable_cols <- c("name", "description", "url_address")
    sortable_cols <- c("name", "creator_id", "datetime")
    column_widths <- c("id" = "80px", "datetime" = "130px", "creator_id" = "200px", "action" = "80px")
    centered_cols <- c("creator_id", "datetime", "action")
    searchable_cols <- c("name", "creator_id")
    factorize_cols <- c("creator_id")
    hidden_cols <- c("id", "unique_id", "api_key", "url_address", "deleted", "modified")
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
        table = "scripts", factorize_cols = factorize_cols, action_buttons = action_buttons, data_input = r$git_repos_temp)
      
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
