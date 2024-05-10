#' @noRd 
mod_datasets_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  
  pivot_item_js <- paste0("
    Shiny.setInputValue('", id, "-current_tab', this.id);
    Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());"
  )
  
  code_hotkeys <- list(
    run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
    run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
    comment = list(win = "CTRL-SHIFT-C", mac = "CTRL-SHIFT-C|CMD-SHIFT-C")
  )
  
  div(class = "main",
      
      # Load widget UI ----
      
      mod_widgets_ui(id, language, languages, i18n),
      
      # Dataset details ----

      shinyjs::hidden(
        div(
          id = ns("one_element"),
          div(
            uiOutput(ns("breadcrumb")),
            div(
              id = ns("dataset_pivot"),
              tags$button(id = ns("summary"), i18n$t("summary"), class = "pivot_item selected_pivot_item", onclick = pivot_item_js),
              tags$button(id = ns("edit_code"), i18n$t("code"), class = "pivot_item", onclick = pivot_item_js),
              tags$button(id = ns("share"), i18n$t("share"), class = "pivot_item", onclick = pivot_item_js),
              class = "pivot"
            ),
            style = "display:flex; justify-content:space-between;"
          ),

          ## Summary ----
          div(
            id = ns("summary_div"),
            div(
              div(
                h1(i18n$t("informations")),
                uiOutput(ns("dataset_summary")),
                div(
                  div(shiny.fluent::PrimaryButton.shinyInput(ns("delete_element"), i18n$t("delete")), class = "delete_button"),
                  class = "create_element_modal_buttons"
                ),
                class = "widget", style = "height: 50%;"
              ),
              div(
                h1("?"),
                class = "widget", style = "height: 50%;"
              ),
              class = "datasets_summary_left"
            ),
            div(
              div(
                h1(i18n$t("description")),
                class = "widget", style = "height: calc(100% - 25px); padding-top: 1px;"
              ),
              class = "datasets_summary_right"
            ),
            class = "datasets_summary_container"
          ),

          ## Edit code ----
          shinyjs::hidden(
            div(
              id = ns("edit_code_div"),
              div(
                shinyAce::aceEditor(
                  ns("code_editor"), value = "", mode = "r",
                  code_hotkeys = list("r", code_hotkeys),
                  autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, fontSize = 10, showPrintMargin = FALSE
                ),
                style = "width: 50%; max-height: calc(100% - 20px);"
              ),
              div(
                verbatimTextOutput(ns("code_result")),
                style = "width: 50%; border: dashed grey 1px; margin: 10px 0px 10px 10px; padding: 0px 10px; font-size: 12px; overflow-y: auto;"
              ),
              style = "height: 100%; display: flex;"
            )
          ),

          ## Share ----
          shinyjs::hidden(
            div(
              id = ns("share_div"),
              style = "height: 100%;"
            )
          ),

          style = "height: 100%; display: flex; flex-direction: column;"
        )
      )
  )
}

#' @noRd 
mod_datasets_server <- function(id, r, d, m, language, i18n, debug){
  
  # |-------------------------------- -----
  
  if (debug) cat(paste0("\n", now(), " - mod_datasets - ", id, " - start"))
  
  # Load widgets ----
  
  all_divs <- c("summary", "edit_code", "share")
  mod_widgets_server(id, r, d, m, language, i18n, all_divs, debug)
  
  # Datasets module ----
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # |-------------------------------- -----
    
    # --- --- --- --- -- -
    # Dataset summary ----
    # --- --- --- --- -- -
    
    # |-------------------------------- -----
    
    # --- --- --- --- --- --
    # Edit dataset code ----
    # --- --- --- --- --- --
    
    # Load code ----
    
    observeEvent(input$load_dataset_code, {
      if (debug) cat(paste0("\n", now(), " - mod_datasets - observer input$load_dataset_code"))
      
      dataset_id <- input$selected_element
      
      sql <- glue::glue_sql("SELECT code FROM code WHERE category = 'dataset' AND link_id = {dataset_id}", .con = r$db)
      code <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
      
      shinyAce::updateAceEditor(session, "code_editor", value = code)
    })
    
    # Run code ----
    
    observeEvent(input$run_code, {
      if (debug) cat(paste0("\n", now(), " - mod_datasets - observer input$run_code"))
      
      code <- 
        input$code_editor %>%
        gsub("\r", "\n", .) %>%
        gsub("%dataset_id%", as.character(input$selected_element), .) %>%
        gsub("%omop_version%", "5.3", .)
      
      result <- capture.output(tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w)))
      
      output$code_result <- renderText(paste(result, collapse = "\n"))
    })
    
    # Save code ----
  })
}