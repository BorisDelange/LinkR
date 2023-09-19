#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  
  if (id == "home") page <- "home" else page <- substr(id, 6, nchar(id))
  
  main <- div(
    shiny.fluent::Breadcrumb(items = list(
      list(key = page, text = i18n$t(page))
    ), maxDisplayedItems = 3),
    uiOutput(ns(paste0(page, "_pivot"))),
    uiOutput(ns(paste0(page, "_cards")))
  )
  
  div(class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    main, br()
  )
}

#' home Server Functions
#'
#' @noRd 
mod_home_server <- function(id = character(), r, language = "en", i18n = character(), perf_monitoring = FALSE, debug = FALSE, show_home_page = TRUE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if(perf_monitoring) monitor_perf(r = r, action = "start")
    if (debug) print(paste0(Sys.time(), " - mod_home - ", id, " - start"))
    
    if (id == "home") page <- "home" else page <- substr(id, 6, nchar(id))
    
    observeEvent(input$current_tab, {
      
      if (debug) print(paste0(Sys.time(), " - mod_home - ", id, " - observer input$current_tab"))
      
      sapply(cards %>% setdiff(., input$current_tab), shinyjs::hide)
      shinyjs::show(input$current_tab)
    })
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    observeEvent(input$help, {
      if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r[[paste0("help_home_", id, "_open_panel")]] <- TRUE
      else if (id == "home" & shiny.router::get_page() == "/") r$help_home_home_open_panel <- TRUE
    })
    observeEvent(input$hide_panel, r[[paste0("help_home_", id, "_open_panel")]] <- FALSE)
    
    # observeEvent(shiny.router::get_page(), {
    #   if (debug) print(paste0(Sys.time(), " - mod_home - ", id, " - observer shiny_router::change_page"))
    # 
    #   # Close help pages when page changes
    #   r[[paste0("help_home_", id, "_open_panel")]] <- FALSE
    #   r[[paste0("help_home_", id, "_open_modal")]] <- FALSE
    # })
    
    r[[paste0("help_home_", id, "_open_panel_light_dismiss")]] <- TRUE
    observeEvent(input$show_modal, r[[paste0("help_home_", id, "_open_modal")]] <- TRUE)
    observeEvent(input$hide_modal, {
      r[[paste0("help_home_", id, "_open_modal")]] <- FALSE
      r[[paste0("help_home_", id, "_open_panel_light_dismiss")]] <- TRUE
    })
    
    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_home_", id, "_page_", i)]] <- Sys.time())
    })
    
    help_home(output = output, r = r, id = id, language = language, i18n = i18n, ns = ns)
    
    # --- --- --- --- --- --- --- --- --
    # Get CSV file for tabs & cards ----
    # --- --- --- --- --- --- --- --- --
    
    if (show_home_page & id == "home"){
      
      r$tabs_and_cards <- tibble::tibble()
      
      filename <- paste0("https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/", language, "/tabs_and_cards.csv")
      filename_local <- paste0(r$app_folder, "/home/", language, "/tabs_and_cards.csv")
    
      # Get csv file from remote git
      if (r$has_internet){
        tryCatch(download.file(filename, filename_local, quiet = TRUE),
          error = function(e) report_bug(r = r, output = output, error_message = "error_downloading_tabs_and_cards_csv_file",
            error_name = "home download tabs_and_cards.csv", category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
      }
      
      tryCatch(r$tabs_and_cards <- vroom::vroom(filename_local, col_types = "cccccciT", progress = FALSE),
        error = function(e) report_bug(r = r, output = output, error_message = "error_loading_tabs_and_cards_csv_file",
          error_name = "home load tabs_and_cards.csv", category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
    }
    
    # --- --- --- --- --- ---
    # Render UI of pages ----
    # --- --- --- --- --- ---
    
    if (show_home_page){
      
      if (nrow(r$tabs_and_cards) > 0){
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_home - ", id, " - render UI"))
      
        # --- --- --- -- --
        ## Pivot items ----
        # --- --- --- -- --
        
        pivot_items <- tagList()
          
        pivot_items_rows <- r$tabs_and_cards %>% dplyr::filter(page == !!page, type == "pivot_item") %>% dplyr::arrange(display_order)
        
        for (i in 1:nrow(pivot_items_rows)){
          row <- pivot_items_rows[i, ]
          pivot_items <- tagList(pivot_items, shiny.fluent::PivotItem(id = row$key, itemKey = row$key, headerText = row$title))
        }
        
        output[[paste0(page, "_pivot")]] <- renderUI({
          
          if (debug) print(paste0(Sys.time(), " - mod_home - ", id, " - output .._pivot"))
          
          shiny.fluent::Pivot(
            onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", page, "_pivot_current_tab', item.props.id)")),
            pivot_items
          )
        })
        
        page_cards <- r$tabs_and_cards %>% dplyr::filter(page == !!page, type == "card") %>% dplyr::arrange(display_order)
        page_divs <- page_cards %>% dplyr::pull(key)
        categories <- r$tabs_and_cards %>% dplyr::filter(page == !!page, type == "pivot_item") %>% dplyr::arrange(display_order) %>% dplyr::pull(category)
        
        observeEvent(input[[paste0(page, "_pivot_current_tab")]], {
          
          if (debug) print(paste0(Sys.time(), " - mod_home - ", id, " - observer .._pivot_current_tab"))
          
          sapply(page_divs, shinyjs::hide)
          sapply(categories, function(category) shinyjs::hide(paste0(category, "_summary")))
          
          # Show summary & first cards
          if (input[[paste0(page, "_pivot_current_tab")]] %not_in% c("overview", "news", "versions")){
            
            # Show summary
            shinyjs::show(paste0(input[[paste0(page, "_pivot_current_tab")]], "_summary"))
            
            # Show first card
            shinyjs::show(first_card[[input[[paste0(page, "_pivot_current_tab")]]]])
          }
          
          # Show all cards if in news or versions card
          else {
            category <- input[[paste0(page, "_pivot_current_tab")]]
            cards <- r$tabs_and_cards %>% dplyr::filter(type == "card" & category == !!category)
            if (nrow(cards) > 0) sapply(cards$key, shinyjs::show) 
          }
        })
      
        # --- --- --
        # Cards ----
        # --- --- --
        
        ui_result <- tagList()
        first_card <- list()
        
        # Create summary cards
        # Only for pages that are not overview, news or versions, where all the cards are displayed without a summary
        
        if (length(categories) > 0){
          for (i in 1:length(categories)){
            
            category <- categories[i]
            
            cards_rows <- r$tabs_and_cards %>% dplyr::filter(page == !!page, type == "card", category == !!category) %>% dplyr::arrange(display_order)
            first_card[[category]] <- r$tabs_and_cards %>% dplyr::filter(page == !!page, type == "card", category == !!category, display_order == 1) %>% dplyr::pull(key)
            
            # Links displayed in summary div, to show cards
            summary_links <- tagList()
            
            if (nrow(cards_rows) > 0){
              for (j in 1:nrow(cards_rows)){
                row <- cards_rows[j, ]
                summary_links <- tagList(summary_links,
                  shiny.fluent::Link(row$title, onClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-", page, "_current_card', '", row$key , "')"))), br(),
                )
              }
            }
            
            summary_card <- div(
              id = ns(paste0(category, "_summary")),
              make_card(
                i18n$t("summary"),
                div(summary_links)
              )
            )
            
            # If category in overview, news or versions, there is not a summary card
            if (category %not_in% c("overview", "news", "versions")){
              if (i == 1) ui_result <- tagList(ui_result, summary_card)
              else ui_result <- tagList(ui_result, shinyjs::hidden(summary_card))
            }
          }
        }
        
        # Create other cards
        
        markdown_list <- list()
        
        if (nrow(page_cards) > 0){
  
          for (i in 1:nrow(page_cards)){
            row <- page_cards[i, ]
  
            content_div <- div()
            if (!is.na(row$markdown_file) & row$markdown_file != ""){
  
              tryCatch({
                filename <- paste0(r$app_folder, "/home/", language, "/", row$page, "/", row$markdown_file)
  
                # Check if file exists. If not, copy file.
  
                if (!file.exists(filename)){
                  filename_remote <- paste0("https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/home/", language, "/", row$page, "/", row$markdown_file)
                  download.file(filename_remote, filename, quiet = TRUE)
                }
  
                con <- textConnection(filename)
                markdown_list[[row$key]] <- readLines(con, warn = FALSE) %>% includeMarkdown() %>% withMathJax()
                content_div <- uiOutput(ns(paste0(row$key, "_markdown")))
                close(con)
                
              }, error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_loading_home_files",
                error_name = "Home overview_card load markdown", category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
            }
  
            div_result <-  div(
              id = ns(row$key),
              make_card(
                row$title,
                uiOutput(ns(paste0(row$key, "_markdown")))
              ),
              style = "a:visited { color: black; text-decoration: none; }"
            )
  
            if (row$category == categories[1] & row$display_order == 1) ui_result <- tagList(ui_result, div_result)
            else ui_result <- tagList(ui_result, shinyjs::hidden(div_result))
          }
        }
  
        output[[paste0(page, "_cards")]] <- renderUI({
  
          if (debug) print(paste0(Sys.time(), " - mod_home - ", id, " - output .._cards"))
  
          div(ui_result)
        })
        
        if (length(markdown_list) > 0) sapply(names(markdown_list), function(key){
          if (key == "overview") output[[paste0(key, "_markdown")]] <- renderUI(div(markdown_list[[key]], style = "margin-top:-15px;"))
          else output[[paste0(key, "_markdown")]] <- renderUI(markdown_list[[key]])
        })
        
        # Show or hide cards
        
        observeEvent(input[[paste0(page, "_current_card")]], {
          
          if (debug) print(paste0(Sys.time(), " - mod_home - ", id, " - observer .._current_card"))
          
          sapply(page_divs, shinyjs::hide)
          shinyjs::show(input[[paste0(page, "_current_card")]])
        })
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_home - ", id, " - render UI"))
        
      }
    }
    
    if (debug) print(paste0(Sys.time(), " - mod_home - ", id, " - end"))
  })
}
