#' @noRd
mod_subsets_ui <- function(id){
  
  pages_variables_list <- get("pages_variables_list", envir = parent.frame())
  for (obj_name in pages_variables_list) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  ns <- NS(id)
  
  pivot_item_js <- paste0("
    Shiny.setInputValue('", id, "-current_tab', this.id);
    Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());"
  )
  
  div(class = "main",
      
    # Load widget UI ----
    
    mod_widgets_ui(id),
    
    # Subset details ----

    shinyjs::hidden(
      div(
        id = ns("one_element"),
        div(
          div(uiOutput(ns("breadcrumb")), class = "breadcrumb"),
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
            shinyjs::hidden(
              div(
                id = ns("edit_description_div"),
                tags$h1(i18n$t("edit_description")),
                div(
                  create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("run_description_code"), iconProps = list(iconName = "Play")), text = i18n$t("run_code")),
                  class = "small_icon_button top-right-button"
                ),
                div(
                  shinyAce::aceEditor(
                    ns("description_code"), mode = "markdown",
                    hotkeys = list(
                      save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
                      run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER")
                    ),
                    autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, fontSize = 11, showPrintMargin = FALSE
                  ),
                  style = "width: 100%; height: calc(100% - 40px); display: flex; flex-direction: column;"
                ),
                class = "widget", style = "height: 100%;"
              )
            ),
            div(
              id = ns("summary_informations_div"),
              shinyjs::hidden(
                div(
                  id = ns("summary_edit_informations_div"),
                  div(
                    h1(i18n$t("edit_informations")),
                    div(
                      shiny.fluent::Dropdown.shinyInput(
                        ns("language"), i18n$t("language"),
                        options = convert_tibble_to_list(languages, key_col = "code", text_col = "language"), value = language
                      ),
                      style = "width: 100px; margin-top: 8px; height: 30px;"
                    ),
                    style = "display: flex; justify-content: space-between;"
                  ),
                  lapply(1:nrow(languages), function(i) {
                    row <- languages[i, ]
                    result <- div(
                      id = ns(paste0("name_", row$code, "_div")),
                      shiny.fluent::TextField.shinyInput(ns(paste0("name_", row$code)), label = i18n$t("name")),
                      style = "width: 200px;"
                    )
                    if (row$code != language) result <- shinyjs::hidden(result)
                    result
                  }),
                  lapply(1:nrow(languages), function(i) {
                    row <- languages[i, ]
                    result <- div(
                      id = ns(paste0("short_description_", row$code, "_div")),
                      shiny.fluent::TextField.shinyInput(ns(paste0("short_description_", row$code)), label = i18n$t("short_description")),
                      style = "width: 400px;"
                    )
                    if (row$code != language) result <- shinyjs::hidden(result)
                    result
                  })
                )
              ),
              div(
                id = ns("summary_view_informations_div"),
                h1(i18n$t("informations")),
                uiOutput(ns("summary_informations_ui"))
              ),
              class = "widget", style = "min-height: 50%;"
            ),
            class = "subsets_summary_left"
          ),
          div(
            div(
              div(
                h1(i18n$t("description")),
                div(
                  shinyjs::hidden(
                    div(
                      id = ns("edit_description_button"),
                      create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("edit_description"), iconProps = list(iconName = "Edit")), text = i18n$t("edit_description"))
                    )
                  ),
                  shinyjs::hidden(
                    div(
                      id = ns ("save_and_cancel_description_buttons"),
                      div(
                        id = ns("cancel_description_button"),
                        create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("cancel_description"), iconProps = list(iconName = "Cancel")), text = i18n$t("cancel_description_updates"))
                      ),
                      div(
                        id = ns("save_description_button"),
                        create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("save_description"), iconProps = list(iconName = "Accept")), text = i18n$t("save_description")),
                      ),
                      style = "display: flex;"
                    )
                  ),
                  class = "small_icon_button top-right-button"
                )
              ),
              uiOutput(ns("description_ui")),
              class = "widget", style = "height: calc(100% - 25px); padding-top: 1px; overflow: auto;"
            ),
            class = "subsets_summary_right"
          ),
          class = "subsets_summary_container"
        ),

        ## Edit code ----
        shinyjs::hidden(
          div(
            id = ns("edit_code_div"),
            div(
              shinyAce::aceEditor(
                ns("subset_code"), value = "", mode = "r",
                code_hotkeys = list("r",  get_ace_editor_code_hotkeys()),
                autoComplete = "live", autoCompleters = c("static", "text"), autoCompleteList = get_ace_editor_auto_complete_list(),
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
            class = "modal_head small_close_button"
          ),
          div(
            div(shiny.fluent::TextField.shinyInput(ns("element_creation_name"), label = i18n$t("name")), style = "width: 200px;"),
            div(
              shiny.fluent::PrimaryButton.shinyInput(ns("add_element"), i18n$t("add")),
              class = "modal_buttons"
            ),
          ),
          class = "modal_content",
          style = "width: 400px; padding-bottom: 30px;"
        ),
        class = "modal"
      )
    ),
  )
}
    
#' @noRd 
mod_subsets_server <- function(id){
  
  pages_variables_list <- get("pages_variables_list", envir = parent.frame())
  for (obj_name in pages_variables_list) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  # |-------------------------------- -----
  
  # Load widgets ----
  
  all_divs <- c("summary", "edit_code")
  mod_widgets_server(id, all_divs)
  
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
    
    observe_event(input$load_subset_code, {
      
      subset_id <- input$selected_element
      unique_id <- r$subsets_long %>% dplyr::filter(id == subset_id) %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value)
      code <- load_element_code(unique_id = unique_id)
      
      shinyAce::updateAceEditor(session, "subset_code", value = code)
    })
    
    # Comment code ----
    
    observe_event(input$subset_code_comment, toggle_comments(input_id = "subset_code", code = input$subset_code, selection = input$subset_code_comment$range, session = session))
    
    # Run code ----
    
    observe_event(input$run_code, {
      
      r$subset_code <- input$subset_code
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_trigger', Math.random());"))
    })
    
    observe_event(input$subset_code_run_selection, {
      
      subset_id <- input$selected_element
      
      editor_id <- "subset_code"
      editor_input <- input[[paste0(editor_id, "_run_selection")]]
      full_code <- input[[editor_id]]
      code_store_var <- "subset_code"
      
      execute_ace_code(editor_id = editor_id, full_code = full_code, editor_input = editor_input, code_store_var = code_store_var)
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_trigger', Math.random());"))
    })
    
    observe_event(input$subset_code_run_all, {
      
      r$subset_code <- input$subset_code
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_code_trigger', Math.random());"))
    })
    
    observe_event(input$run_code_trigger, {
      
      subset_id <- input$selected_element
      
      code <- r$subset_code %>% gsub("\r", "\n", .)
      
      result <- capture.output(tryCatch(eval(parse(text = code)), error = function(e) print(e), warning = function(w) print(w)))
      
      output$code_result <- renderText(paste(result, collapse = "\n"))
    })
    
    # Save code ----
    
    observe_event(input$save_code, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_code_trigger', Math.random());")))
    
    observe_event(input$subset_code_save, shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_code_trigger', Math.random());")))
    
    observe_event(input$save_code_trigger, {
      
      subset_id <- input$selected_element
      unique_id <- r$subsets_long %>% dplyr::filter(id == subset_id) %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value)
      save_element_code(unique_id = unique_id, new_code = input$subset_code)
    })
  })
}
