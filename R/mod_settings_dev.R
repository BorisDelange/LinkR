#' settings_r_console UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_dev_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  
  cards <- c("dev_perf_monitoring_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  })
  
  div(class = "main",
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "r_console", text = i18n$t("dev"))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "dev_perf_monitoring_card", itemKey = "dev_perf_monitoring_card", headerText = i18n$t("perf_monitoring"))
    ),
    forbidden_cards,
    
    shinyjs::hidden(
      div(id = ns("dev_perf_monitoring_card"),
        make_card(i18n$t("perf_monitoring"),
          div(
            br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::DefaultButton.shinyInput(ns("show_datatable"), i18n$t("show_data")),
              shiny.fluent::DefaultButton.shinyInput(ns("reset_perf_monitoring"), i18n$t("reset"))
            ),
            DT::DTOutput(ns("perf_monitoring_datatable"))
          )
        )
      )
    ), br()
  )
}
    
#' settings_r_console Server Functions
#'
#' @noRd 

mod_settings_dev_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), 
  i18n = character(), language = "en", perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("dev_perf_monitoring_card")
    show_or_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    if ("dev_perf_monitoring_card" %in% r$user_accesses) shinyjs::show("dev_perf_monitoring_card")
    else shinyjs::show("dev_perf_monitoring_card_forbidden")
    
    # observe({
    #   shiny.router::get_query_param()
    # 
    #   if ("r_console_edit_code_card" %in% r$user_accesses){
    #     shinyjs::show("dev_edit_r_code_card")
    #     shinyjs::hide("dev_edit_r_code_card_forbidden")
    #   }
    #   else {
    #     shinyjs::show("dev_edit_r_code_card_forbidden")
    #     shinyjs::hide("dev_edit_r_code_card")
    #   }
    # })
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    observeEvent(input$help, if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r$help_settings_dev_open_panel <- TRUE)
    observeEvent(input$hide_panel, r$help_settings_dev_open_panel <- FALSE)

    r$help_settings_dev_open_panel_light_dismiss <- TRUE
    observeEvent(input$show_modal, r$help_settings_dev_open_modal <- TRUE)
    observeEvent(input$hide_modal, {
      r$help_settings_dev_open_modal <- FALSE
      r$help_settings_dev_open_panel_light_dismiss <- TRUE
    })

    # observeEvent(shiny.router::get_page(), {
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_dev - ", id, " - observer shiny_router::change_page"))
    # 
    #   # Close help pages when page changes
    #   r$help_settings_dev_open_panel <- FALSE
    #   r$help_settings_dev_open_modal <- FALSE
    # })

    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_settings_dev_page_", i)]] <- now())
    })

    help_settings_dev(output = output, r = r, id = id, language = language, i18n = i18n, ns = ns)
    
    observeEvent(input$copy_code_1, r$help_settings_dev_copy_code_1 <- now())
    observeEvent(input$copy_code_2, r$help_settings_dev_copy_code_2 <- now())
    
    # --- --- --- --- -- -
    # Perf monitoring ----
    # --- --- --- --- -- -
    
    searchable_cols <- c("task")
    column_widths <- c("elapsed_time" = "150px", "datetime_start" = "150px", "datetime_stop" = "150px")
    sortable_cols <- c("elapsed_time", "datetime_start", "datetime_stop", "task")
    centered_cols <- c("elapsed_time", "datetime_start", "datetime_stop")
    col_names <- get_col_names(table_name = "perf_monitoring", i18n = i18n)
    
    observeEvent(input$show_datatable, {
      
      if (debug) cat(paste0("\n", now(), " - mod_settings_dev - observer input$show_datatable"))
      
      if (nrow(r$perf_monitoring_table) > 0) perf_monitoring_table <- r$perf_monitoring_table %>%
        dplyr::mutate(elapsed_time = round(datetime_stop - datetime_start, 2), .before = "task") %>%
        dplyr::mutate_at(c("datetime_start", "datetime_stop"), as.character) %>%
        dplyr::mutate(row_num = 1:dplyr::n()) %>%
        dplyr::arrange(dplyr::desc(row_num)) %>%
        dplyr::select(-row_num)
      
      if (nrow(r$perf_monitoring_table) == 0) perf_monitoring_table <- r$perf_monitoring_table %>% dplyr::mutate(elapsed_time = NA_real_, .before = "task")
      
      # Render datatable
      if (length(r$perf_monitoring_datatable_proxy) == 0){
        
        render_datatable(output = output, ns = ns, i18n = i18n, data = perf_monitoring_table,
          output_name = "perf_monitoring_datatable", col_names = col_names,
          sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, page_length = 100)
        
        # Create a proxy for datatatable
        r$perf_monitoring_datatable_proxy <- DT::dataTableProxy("perf_monitoring_datatable", deferUntilFlush = FALSE)
      }
      
      if (length(r$perf_monitoring_datatable_proxy) > 0) DT::replaceData(r$perf_monitoring_datatable_proxy, 
        perf_monitoring_table, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Reset perf monitoring
    
    observeEvent(input$reset_perf_monitoring, {
      
      if (debug) cat(paste0("\n", now(), " - mod_settings_dev - observer input$reset_perf_monitoring"))
      
      r$perf_monitoring_table <- r$perf_monitoring_table %>% dplyr::slice(0)
    })
  })
}
