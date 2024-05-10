#' @noRd
mod_projects_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  
  pivot_item_js <- paste0("
    Shiny.setInputValue('", id, "-current_tab', this.id);
    Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());"
  )
  
  div(class = "main",
    
    # Load widget UI ----
    
    mod_widgets_ui(id, language, languages, i18n),
    
    # Project details ----
    
    shinyjs::hidden(
      div(
        id = ns("one_element"),
        div(
          uiOutput(ns("breadcrumb")),
          div(
            id = ns("project_pivot"),
            tags$button(id = ns("summary"), i18n$t("summary"), class = "pivot_item selected_pivot_item", onclick = pivot_item_js),
            tags$button(id = ns("datasets"), i18n$t("datasets"), class = "pivot_item", onclick = pivot_item_js),
            tags$button(id = ns("data_cleaning_scripts"), i18n$t("data_cleaning"), class = "pivot_item", onclick = pivot_item_js),
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
              uiOutput(ns("project_summary")),
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
            class = "projects_summary_left"
          ),
          div(
            div(
              h1(i18n$t("description")),
              class = "widget", style = "height: calc(100% - 25px); padding-top: 1px;"
            ),
            class = "projects_summary_right"
          ),
          class = "projects_summary_container"
        ),
        
        ## Datasets ----
        shinyjs::hidden(
          div(
            id = ns("datasets_div"),
            div(
              make_dropdown(i18n, ns, id = "project_dataset", label = "dataset", width = "300px"),
              div(
                div(shiny.fluent::PrimaryButton.shinyInput(ns("save_datasets"), i18n$t("save"))),
                class = "create_element_modal_buttons"
              ),
              class = "widget", style = "height: 50%; width: 50%; padding-top: 10px;"
            ),
            class = "projects_summary_container"
          )
        ),
        
        ## Data cleaning scripts ----
        shinyjs::hidden(
          div(
            id = ns("data_cleaning_scripts_div"),
            style = "height: 100%;"
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
mod_projects_server <- function(id, r, d, m, language, i18n, debug){
  
  # |-------------------------------- -----
  
  if (debug) cat(paste0("\n", now(), " - mod_projects - ", id, " - start"))
  
  # Load widgets ----
  
  all_divs <- c("summary", "datasets", "data_cleaning_scripts", "share")
  mod_widgets_server(id, r, d, m, language, i18n, all_divs, debug)
  
  # Projects module ----
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    # Load a project ----
    observeEvent(r$load_project_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer r$load_project_trigger"))
      
      shinyjs::delay(500, m$selected_study <- input$selected_element)
    })
  })
}