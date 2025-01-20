#' @noRd
mod_subsets_ui <- function(id, language, languages, i18n, code_hotkeys, auto_complete_list){
  ns <- NS(id)
  
  pivot_item_js <- paste0("
    Shiny.setInputValue('", id, "-current_tab', this.id);
    Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());"
  )
  
  div(class = "main",
      
    # Load widget UI ----
    
    mod_widgets_ui(id, language, languages, i18n),
    
    # Subset details ----

    shinyjs::hidden(
      div(
        id = ns("one_element"),
        div(
          div(uiOutput(ns("breadcrumb")), style = "flex: 1;"),
          div(
            id = ns("subset_pivot"),
            tags$button(id = ns("summary"), i18n$t("summary"), class = "pivot_item selected_pivot_item", onclick = pivot_item_js),
            tags$button(id = ns("edit_code"), i18n$t("code"), class = "pivot_item", onclick = pivot_item_js),
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
              uiOutput(ns("subset_summary")),
              class = "widget", style = "height: 50%;"
            ),
            # div(
            #   h1("?"),
            #   class = "widget", style = "height: 50%;"
            # ),
            class = "subsets_summary_left"
          ),
          # div(
          #   div(
          #     h1(i18n$t("description")),
          #     class = "widget", style = "height: calc(100% - 25px); padding-top: 1px;"
          #   ),
          #   class = "subsets_summary_right"
          # ),
          class = "subsets_summary_container"
        ),

        ## Edit code ----
        shinyjs::hidden(
          div(
            id = ns("edit_code_div"),
            div(
              shinyAce::aceEditor(
                ns("subset_code"), value = "", mode = "r",
                code_hotkeys = list("r", code_hotkeys),
                autoComplete = "live", autoCompleters = c("static", "text"), autoCompleteList = auto_complete_list,
                autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, fontSize = 11, showPrintMargin = FALSE
              ),
              class = "resizable-panel left-panel",
              style = "width: 50%;"
            ),
            div(class = "resizer"),
            div(
              id = ns("code_result_div"),
              verbatimTextOutput(ns("code_result")),
              class = "resizable-panel right-panel",
              style = "width: 50%; padding: 0 10px; font-size: 12px; overflow-y: auto;"
            ),
            class = "resizable-container",
            style = "height: calc(100% - 10px); display: flex; margin-top: 10px;"
          )
        ),

        style = "height: 100%; display: flex; flex-direction: column;"
      )
    ),
    
    # Create a subset modal ----
    
    shinyjs::hidden(
      div(
        id = ns("create_element_modal"),
        div(
          div(
            tags$h1(i18n$t("create_subset")),
            shiny.fluent::IconButton.shinyInput(ns("close_create_element_modal"), iconProps = list(iconName = "ChromeClose")),
            class = "create_element_modal_head small_close_button"
          ),
          div(
            div(shiny.fluent::TextField.shinyInput(ns("element_creation_name"), label = i18n$t("name")), style = "width: 200px;"),
            div(
              shiny.fluent::PrimaryButton.shinyInput(ns("add_element"), i18n$t("add")),
              class = "create_element_modal_buttons"
            ),
          ),
          class = "create_subset_modal_content"
        ),
        class = "create_element_modal"
      )
    ),
  )
}
    
#' @noRd 
mod_subsets_server <- function(id, r, d, m, language, i18n, debug, user_accesses, user_settings){
  
  # |-------------------------------- -----
  
  if (debug) cat(paste0("\n", now(), " - mod_subsets - ", id, " - start"))
  
  # Load widgets ----
  
  all_divs <- c("summary", "edit_code")
  mod_widgets_server(id, r, d, m, language, i18n, all_divs, debug, user_accesses, user_settings)
  
  # Subsets module ----
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    # Current user accesses ----
    
    if ("projects_subsets_management" %in% user_accesses) shinyjs::show("subset_buttons")
    
    # |-------------------------------- -----
    
    # --- --- --- --- - -
    # Subset summary ----
    # --- --- --- --- - -
    
    sapply(c("edit_summary_div", "delete_element_div"), shinyjs::show)
    
    # |-------------------------------- -----
    
    # --- --- --- --- --- --
    # Edit subset code ----
    # --- --- --- --- --- --
    
    # Load code ----
    
    observeEvent(input$load_subset_code, {
      if (debug) cat(paste0("\n", now(), " - mod_subsets - observer input$load_subset_code"))
      
      subset_id <- input$selected_element
      unique_id <- r$subsets_long %>% dplyr::filter(id == subset_id) %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value)
      code <- load_element_code(id = id, r = r, unique_id = unique_id)
      
      shinyAce::updateAceEditor(session, "subset_code", value = code)
    })
    
    # Comment code ----
    
    observeEvent(input$subset_code_comment, {
      if (debug) cat(paste0("\n", now(), " - mod_subsets - observer input$subset_code_comment"))
      
      toggle_comments(id = id, input_id = "subset_code", code = input$subset_code, selection = input$subset_code_comment$range, session = session)
    })
    
    # Run code ----
    
    observeEvent(input$run_code, {
      if (debug) cat(paste0("\n", now(), " - mod_subsets - observer input$run_code"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_trigger', Math.random());"))
    })
    
    observeEvent(input$subset_code_run_selection, {
      
      if (debug) cat(paste0("\n", now(), " - mod_subsets - observer input$subset_code_run_selection"))
      
      if(!shinyAce::is.empty(input$subset_code_run_selection$selection)) shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-subset_code', '", input$subset_code_run_selection$selection, "');"))
      else shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-subset_code', '", input$subset_code_run_selection$line, "');"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_trigger', Math.random());"))
    })
    
    observeEvent(input$subset_code_run_all, {
      if (debug) cat(paste0("\n", now(), " - mod_subsets - observer input$code_run_all"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_trigger', Math.random());"))
    })
    
    observeEvent(input$run_code_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_subsets - observer input$run_code_trigger"))
      
      subset_id <- input$selected_element
      
      omop_version <- DBI::dbGetQuery(m$db, glue::glue_sql("SELECT value FROM options WHERE category = 'subset' AND name = 'omop_version' AND link_id = {subset_id}", .con = m$db)) %>% dplyr::pull()
      
      code <- 
        input$subset_code %>%
        gsub("\r", "\n", .) %>%
        gsub("%subset_id%", as.character(subset_id), .)
      
      result <- capture.output(tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w)))
      
      output$code_result <- renderText(paste(result, collapse = "\n"))
    })
    
    # Save code ----
    
    observeEvent(input$save_code, {
      if (debug) cat(paste0("\n", now(), " - mod_subsets - observer input$save_code"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_code_trigger', Math.random());"))
    })
    
    observeEvent(input$subset_code_save, {
      if (debug) cat(paste0("\n", now(), " - mod_subsets - observer input$subset_code_save"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_code_trigger', Math.random());"))
    })
    
    observeEvent(input$save_code_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_subsets - observer input$save_code_trigger"))
      
      subset_id <- input$selected_element
      unique_id <- r$subsets_long %>% dplyr::filter(id == subset_id) %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value)
      save_element_code(id = id, i18n = i18n, output = output, r = r, unique_id = unique_id, new_code = input$subset_code)
    })
  })
}
