#' @noRd 
mod_vocabularies_ui <- function(id, language, languages, i18n, code_hotkeys){
  ns <- NS(id)
  
  pivot_item_js <- paste0("
    Shiny.setInputValue('", id, "-current_tab', this.id);
    Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());"
  )
  
  div(class = "main",
      
      # Load widget UI ----
      
      mod_widgets_ui(id, language, languages, i18n),
      
      # Vocabulary details ----
      
      shinyjs::hidden(
        div(
          id = ns("one_element"),
          div(
            uiOutput(ns("breadcrumb")),
            div(
              id = ns("vocabulary_pivot"),
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
                uiOutput(ns("vocabulary_summary")),
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
              class = "vocabularies_summary_left"
            ),
            div(
              div(
                h1(i18n$t("description")),
                class = "widget", style = "height: calc(100% - 25px); padding-top: 1px;"
              ),
              class = "vocabularies_summary_right"
            ),
            class = "vocabularies_summary_container"
          ),
          
          ## Edit code ----
          shinyjs::hidden(
            div(
              id = ns("edit_code_div"),
              div(
                shinyAce::aceEditor(
                  ns("vocabulary_code"), value = "", mode = "r",
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
              style = "height: 100%;"
            )
          ),
          
          style = "height: 100%; display: flex; flex-direction: column;"
        )
      )
  )
}

#' @noRd 
mod_vocabularies_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), language = "en", i18n = character(), debug = FALSE){
  # |-------------------------------- -----
  
  if (debug) cat(paste0("\n", now(), " - mod_vocabularies - ", id, " - start"))
  
  # Load widgets ----
  
  all_divs <- c("summary", "edit_code", "share")
  mod_widgets_server(id, r, d, m, language, i18n, all_divs, debug)
  
  # Vocabularies module ----
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # |-------------------------------- -----
    
    # --- --- --- --- --- ---
    # Vocabulary summary ----
    # --- --- --- --- --- ---
    
    # |-------------------------------- -----
    
    # --- --- --- --- --- --- -
    # Edit vocabulary code ----
    # --- --- --- --- --- --- -
    
  })
}