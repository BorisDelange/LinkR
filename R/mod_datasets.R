#' @noRd 
mod_datasets_ui <- function(id, language, languages, i18n, code_hotkeys){
  ns <- NS(id)
  
  pivot_item_js <- paste0("
    Shiny.setInputValue('", id, "-current_tab', this.id);
    Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());"
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
                ns("dataset_code"), value = "", mode = "r",
                code_hotkeys = list("r", code_hotkeys),
                autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, fontSize = 11, showPrintMargin = FALSE
              ),
              class = "element_ace_editor"
            ),
            div(
              verbatimTextOutput(ns("code_result")),
              class = "element_code_result"
            ),
            style = "height: 100%; display: flex;"
          )
        ),

        ## Share ----
        shinyjs::hidden(
          div(
            id = ns("share_div"),
            div(
              div(
                h1(i18n$t("synchronize_with_git_repo")),
                div(shiny.fluent::Dropdown.shinyInput(ns("git_repo"), label = i18n$t("git_repo")), style = "width: 200px;"),
                div(uiOutput(ns("git_repo_element_ui")), style = "margin-top:10px;"),
                div(
                  uiOutput(ns("synchronize_git_buttons")),
                  class = "datasets_share_buttons"
                ),
                class = "widget", style = "height: 50%; padding-top: 1px;"
              ),
              class = "datasets_share_left",
            ),
            class = "datasets_share_container"
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
      
      shinyAce::updateAceEditor(session, "dataset_code", value = code)
    })
    
    # Comment code ----
    observeEvent(input$dataset_code_comment, {
      if (debug) cat(paste0("\n", now(), " - mod_datasets - observer input$dataset_code_comment"))
      
      lines <- strsplit(input$dataset_code, "\n")[[1]]
      req(length(lines) > 0)
      
      start_row <- input$dataset_code_comment$range$start$row + 1
      end_row <- input$dataset_code_comment$range$end$row + 1
      
      for (i in start_row:end_row) if (startsWith(lines[i], "# ")) lines[i] <- substr(lines[i], 3, nchar(lines[i])) else lines[i] <- paste0("# ", lines[i])
      
      shinyAce::updateAceEditor(session, "dataset_code", value = paste0(lines, collapse = "\n"))
      
      shinyjs::runjs(sprintf("
        var editor = ace.edit('%s-dataset_code');
        editor.moveCursorTo(%d, %d);
        editor.focus();
      ", id, input$dataset_code_comment$range$end$row, input$dataset_code_comment$range$end$column))
    })
    
    # Run code ----
    
    observeEvent(input$run_code, {
      if (debug) cat(paste0("\n", now(), " - mod_datasets - observer input$run_code"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_trigger', Math.random());"))
    })
    
    observeEvent(input$dataset_code_run_selection, {
      
      if (debug) cat(paste0("\n", now(), " - mod_datasets - observer input$dataset_code_run_selection"))
      
      if(!shinyAce::is.empty(input$dataset_code_run_selection$selection)) shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-dataset_code', '", input$dataset_code_run_selection$selection, "');"))
      else shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-dataset_code', '", input$dataset_code_run_selection$line, "');"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_trigger', Math.random());"))
    })
    
    observeEvent(input$dataset_code_run_all, {
      if (debug) cat(paste0("\n", now(), " - mod_datasets - observer input$code_run_all"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_trigger', Math.random());"))
    })
    
    observeEvent(input$run_code_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_datasets - observer input$run_code_trigger"))
      
      dataset_id <- input$selected_element
      
      omop_version <- DBI::dbGetQuery(r$db, glue::glue_sql("SELECT value FROM options WHERE category = 'dataset' AND name = 'omop_version' AND link_id = {dataset_id}", .con = r$db)) %>% dplyr::pull()
      
      code <- 
        input$dataset_code %>%
        gsub("\r", "\n", .) %>%
        gsub("%dataset_id%", as.character(dataset_id), .) %>%
        gsub("%omop_version%", "5.3", .)
      
      result <- capture.output(tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w)))
      
      output$code_result <- renderText(paste(result, collapse = "\n"))
    })
    
    # Save code ----
    
    observeEvent(input$save_code, {
      if (debug) cat(paste0("\n", now(), " - mod_datasets - observer input$save_code"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_code_trigger', Math.random());"))
    })
    
    observeEvent(input$dataset_code_save, {
      if (debug) cat(paste0("\n", now(), " - mod_datasets - observer input$dataset_code_save"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_code_trigger', Math.random());"))
    })
    
    observeEvent(input$save_code_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_datasets - observer input$save_code_trigger"))
      
      dataset_id <- input$selected_element
      sql_send_statement(r$db, glue::glue_sql("UPDATE datasets SET update_datetime = {now()} WHERE id = {dataset_id}", .con = r$db))
      
      code_id <- DBI::dbGetQuery(r$db, glue::glue_sql("SELECT id FROM code WHERE category = 'dataset' AND link_id = {dataset_id}", .con = r$db)) %>% dplyr::pull()
      
      edited_code <- gsub("'", "''", input$dataset_code)
      sql_send_statement(r$db, glue::glue_sql("UPDATE code SET code = {edited_code} WHERE id = {code_id}", .con = r$db))
      
      show_message_bar(output, "modif_saved", "success", i18n = i18n, ns = ns)
    })
  })
}