#' settings_log UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_log_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  
  div(class = "main",
      
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "log", text = i18n$t("log"))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "log_card", itemKey = "log_card", headerText = i18n$t("log"))
    ),
    shinyjs::hidden(
      div(
        id = ns("log_card_forbidden"),
        make_card("",
          div(shiny.fluent::MessageBar(i18n$t("unauthorized_access_page"), messageBarType = 5), style = "margin-top:10px;")
        )
      )
    ),
    shinyjs::hidden(
      div(id = ns("log_card"),
        make_card(i18n$t("log"),
          uiOutput(ns("main"))
        )
      )
    ), br()
  )
}
    
#' settings_log Server Functions
#'
#' @noRd 

mod_settings_log_server <- function(id = character(), r = shiny::reactiveValuess(), i18n = character(), language = "en",
  perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    observeEvent(input$help, if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r$help_settings_log_open_panel <- TRUE)
    observeEvent(input$hide_panel, r$help_settings_log_open_panel <- FALSE)
    
    r$help_settings_log_open_panel_light_dismiss <- TRUE
    observeEvent(input$show_modal, r$help_settings_log_open_modal <- TRUE)
    observeEvent(input$hide_modal, {
      r$help_settings_log_open_modal <- FALSE
      r$help_settings_log_open_panel_light_dismiss <- TRUE
    })
    
    observeEvent(shiny.router::get_page(), {
      if (debug) print(paste0(Sys.time(), " - mod_settings_log - ", id, " - observer shiny_router::change_page"))

      # Close help pages when page changes
      r$help_settings_log_open_panel <- FALSE
      r$help_settings_log_open_modal <- FALSE
    })
    
    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_settings_log_page_", i)]] <- Sys.time())
    })
    
    help_settings_log(output = output, r = r, id = id, language = language, i18n = i18n, ns = ns)
    
    # --- -- -
    # Log ----
    # --- -- -
    
    if (debug) print(paste0(Sys.time(), " - mod_settings_log - start"))

    if ("log" %in% r$user_accesses & ("all_users" %in% r$user_accesses | "only_me" %in% r$user_accesses)){
      
      shinyjs::show("log_card")
      shinyjs::hide("log_card_forbidden")
      
      observeEvent(shiny.router::get_query_param(), {
        shinyjs::hide("log_card")
        shinyjs::show("log_card")
      })
    }
    else {
      shinyjs::hide("log_card")
      shinyjs::show("log_card_forbidden")
    }
    
    # Render main UI, depending on r$user_accesses
    
    output$main <- renderUI({
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_settings_log - output$main"))
      
      result <- ""
      
      if ("all_users" %in% r$user_accesses){
  
        options <-
          r$users %>%
          dplyr::left_join(r$users_statuses %>% dplyr::select(user_status_id = id, user_status = name), by = "user_status_id") %>%
          dplyr::transmute(
            key = id, 
            imageInitials = paste0(substr(firstname, 0, 1), substr(lastname, 0, 1)),
            text = paste0(firstname, " ", lastname), 
            secondaryText = user_status)
        
        tagList(
          shiny.fluent::ChoiceGroup.shinyInput(ns("see_log_of"), value = "only_me", options = list(
            list(key = "only_me", text = i18n$t("only_me")),
            list(key = "people_picker", text = i18n$t("people_picker"))
          ), className = "inline_choicegroup"),
          conditionalPanel(condition = "input.see_log_of == 'people_picker'", ns = ns,
            make_people_picker(i18n = i18n, ns = ns, label = "users", options = options)
          ), br()
        ) -> result
      }
      
      else if ("all_users" %not_in% r$user_accesses & "only_me" %in% r$user_accesses){
        tagList(
          shiny.fluent::ChoiceGroup.shinyInput(ns("see_log_of"), value = "only_me", options = list(
            list(key = "only_me", text = i18n$t("only_me"))
          ), className = "inline_choicegroup"), br()
        ) -> result
      }
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_log - output$main"))
      
      div(
        result,
        shiny.fluent::DefaultButton.shinyInput(ns("reload_log"), i18n$t("reload_log")),
        DT::DTOutput(ns("log_datatable")), br(),
        div(verbatimTextOutput(ns("log_details")), 
          style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
      )
    })
    
    col_names <- get_col_names("log", i18n = i18n)
    page_length <- 10
    column_widths <- c("id" = "80px")
    centered_cols <- c("id", "creator_id", "datetime")
    sortable_cols <- c("id", "category", "name", "creator_id", "datetime")
    searchable_cols <- c("category", "name", "creator_id", "datetime")
    factorize_cols <- c("category")
    shortened_cols <- c("category" = 20, "name" = 30, "value" = 30)
    
    # Reload datatable
    
    observeEvent(input$reload_log, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_settings_log - observer input$reload_log"))
      
      r$log <- tibble::tibble()
      
      if (input$see_log_of == "only_me"){
        sql <- glue::glue_sql("SELECT id, category, name, value, creator_id, datetime FROM log WHERE creator_id = {r$user_id} ORDER BY datetime DESC", .con = r$db)
        r$log <- DBI::dbGetQuery(r$db, sql)
      }
      
      if (input$see_log_of == "people_picker"){
        sql <- glue::glue_sql("SELECT id, category, name, value, creator_id, datetime FROM log WHERE creator_id IN ({input$users*}) ORDER BY datetime DESC", .con = r$db)
        r$log <- DBI::dbGetQuery(r$db, sql)
      }
      
      if (nrow(r$log) > 0){
        
        r$log <- r$log %>% dplyr::left_join(
          r$users %>% dplyr::mutate(display_name = paste0(firstname, " ", lastname)) %>% dplyr::select(creator_id = id, display_name),
          by = "creator_id") %>% 
          dplyr::relocate(display_name, .before = "datetime") %>%
          dplyr::select(-creator_id, creator_id = display_name) %>%
          dplyr::arrange(desc(datetime))
      }
      
      if (length(r$log_datatable_proxy) > 0) DT::replaceData(r$log_datatable_proxy, r$log, resetPaging = FALSE, rownames = FALSE) 
      
      if (length(r$log_datatable_proxy) == 0){
        
        render_datatable(output = output, ns = ns, i18n = i18n, data = r$log, output_name = "log_datatable", col_names = col_names,
          page_length = page_length, centered_cols = centered_cols, sortable_cols = sortable_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, shortened_cols = shortened_cols)
        
        r$log_datatable_proxy <- DT::dataTableProxy("log_datatable", deferUntilFlush = FALSE)
      }
        
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_log - observer input$reload_log"))
    })
    
    # When a row is selected
    
    observeEvent(input$log_datatable_rows_selected, {
      
      output$log_details <- renderText(
        paste0(
          r$log[input$log_datatable_rows_selected, ] %>% dplyr::pull(name), "\n\n",
          r$log[input$log_datatable_rows_selected, ] %>% dplyr::pull(value) %>% strwrap(width = 100) %>% paste(collase = "\n"))
      )
    })
    
  })
}
