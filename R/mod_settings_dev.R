#' settings_r_console UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_settings_dev_ui <- function(id = character(), i18n = R6::R6Class()){
  ns <- NS(id)
  
  cards <- c("dev_edit_code_card", "dev_perf_monitoring_card", "dev_to_do_list_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  })
  
  div(class = "main",
    shiny.fluent::Breadcrumb(items = list(
      list(key = "r_console", text = i18n$t("dev"))
    ), maxDisplayedItems = 3),
    shiny.fluent::Pivot(
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "dev_edit_code_card", itemKey = "dev_edit_code_card", headerText = i18n$t("r_console")),
      shiny.fluent::PivotItem(id = "dev_perf_monitoring_card", itemKey = "dev_perf_monitoring_card", headerText = i18n$t("perf_monitoring")),
      shiny.fluent::PivotItem(id = "dev_to_do_list_card", itemKey = "dev_to_do_list_card", headerText = i18n$t("to_do_list"))
    ),
    forbidden_cards,
    
    shinyjs::hidden(
      div(id = ns("dev_edit_code_card"),
        make_card_shiny_ace(i18n$t("r_console"),
          div(
            div(
              shinyAce::aceEditor(
                outputId = ns("ace_code"), value = "", mode = "r",
                code_hotkeys = list("r", list(
                    run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
                    run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER")
                  )
                ),
                wordWrap = TRUE, debounce = 10,
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000
              ),
              style = "width: 100%;"
            ),

            shiny.fluent::PrimaryButton.shinyInput(ns("execute_code"), i18n$t("run_code")), br(), br(),
            div(shiny::verbatimTextOutput(ns("code_result")),
              style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
          )
        ), br()
      )
    ),
    
    shinyjs::hidden(
      div(id = ns("dev_perf_monitoring_card"),
        make_card(i18n$t("perf_monitoring"),
          DT::DTOutput(ns("perf_monitoring_datatable"))
        ), br()
      )
    ),
    
    shinyjs::hidden(
      div(id = ns("dev_to_do_list_card"),
        make_card(i18n$t("to_do_list"),
          div(br(),
            shiny.fluent::MessageBar(
              div(
                strong("A faire pour la version 0.2.0"),
                p(
                  tags$ul(
                    tags$li("Résoudre les bugs suivants ",
                      tags$ul(
                        tags$li("Lors connexion à BDD appli distante, switch pour la nouvelle BDD sans changer de compte"),
                      )  
                    )
                  )  
                )
              ),
              messageBarType = 5), br(),
            shiny.fluent::MessageBar(
              div(
                strong("Fait pour la version 0.2.0"),
                p(
                  tags$ul(
                    
                  )  
                )
              ),
              messageBarType = 4)
          )
        ), br()
      )
    )
  )
}
    
#' settings_r_console Server Functions
#'
#' @noRd 

mod_settings_dev_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), i18n = R6::R6Class()){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("dev_edit_code_card", "dev_perf_monitoring_card", "dev_to_do_list_card")
    show_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    if ("dev_edit_code_card" %in% r$user_accesses) shinyjs::show("dev_edit_code_card")
    else shinyjs::show("dev_edit_code_card_forbidden")
    
    # observe({
    #   shiny.router::get_query_param()
    # 
    #   if ("r_console_edit_code_card" %in% r$user_accesses){
    #     shinyjs::show("dev_edit_code_card")
    #     shinyjs::hide("dev_edit_code_card_forbidden")
    #   }
    #   else {
    #     shinyjs::show("dev_edit_code_card_forbidden")
    #     shinyjs::hide("dev_edit_code_card")
    #   }
    # })
  
    # --- --- --- --- -
    # Execute code ----
    # --- --- --- --- -
    
    observeEvent(input$execute_code, {
      r$r_console_code <- input$ace_code
    })
    
    observeEvent(input$ace_code_run_selection, {
      if(!shinyAce::is.empty(input$ace_code_run_selection$selection)) r$r_console_code <- input$ace_code_run_selection$selection
      else r$r_console_code <- input$ace_code_run_selection$line
    })

    observeEvent(input$ace_code_run_all, {
      r$r_console_code <- input$ace_code
    })

    observeEvent(r$r_console_code, {

      if ("dev_edit_code_card" %in% r$user_accesses){
        edited_code <- r$r_console_code %>% stringr::str_replace_all("\r", "\n")
        
        output$code_result <- renderText(
          execute_settings_code(input = input, output = output, session = session, id = id, ns = ns, r = r, d = d, m = m,
            edited_code = edited_code, code_type = "server"))
      }
    })
    
    # --- --- --- --- -- -
    # Perf monitoring ----
    # --- --- --- --- -- -
    
    observeEvent(r$perf_monitoring_table, {
      req(nrow(r$perf_monitoring_table) > 0)
      
      perf_monitoring_table <- r$perf_monitoring_table %>%
        dplyr::mutate(elapsed_time = round(datetime_stop - datetime_start, 2), .before = "task") %>%
        dplyr::mutate_at(c("datetime_start", "datetime_stop"), as.character) %>%
        dplyr::mutate(row_num = 1:dplyr::n()) %>%
        dplyr::arrange(dplyr::desc(row_num)) %>%
        dplyr::select(-row_num)
      
      searchable_cols <- c("task")
      column_widths <- c("elapsed_time" = "120px", "datetime_start" = "150px", "datetime_stop" = "150px")
      sortable_cols <- c("elapsed_time", "datetime_start", "datetime_stop", "task")
      centered_cols <- c("elapsed_time", "datetime_start", "datetime_stop")
      col_names <- get_col_names(table_name = "perf_monitoring", i18n = i18n)
      
      # Render datatable
      if (length(r$perf_monitoring_datatable_proxy) == 0){
        
        render_datatable(output = output, r = r, ns = ns, i18n = i18n, data = perf_monitoring_table,
          output_name = "perf_monitoring_datatable", col_names = col_names,
          sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, page_length = 100)
        
        # Create a proxy for datatatable
        r$perf_monitoring_datatable_proxy <- DT::dataTableProxy("perf_monitoring_datatable", deferUntilFlush = FALSE)
      }
      
      if (length(r$perf_monitoring_datatable_proxy) > 0) DT::replaceData(r$perf_monitoring_datatable_proxy, 
        perf_monitoring_table, resetPaging = FALSE, rownames = FALSE)
    })
  })
}