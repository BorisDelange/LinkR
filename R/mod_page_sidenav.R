#' @noRd
mod_page_sidenav_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  result <- ""
  
  show_hide_sidenav <- div(
    id = ns("show_hide_sidenav"),
    class = "button_sidenav button_hide_sidenav",
    onclick = paste0("Shiny.setInputValue('", id, "-show_hide_sidenav', Math.random());"),
  )
  
  # --- --- -
  # Home ----
  # --- --- -
  
  if (id == "home"){
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      show_hide_sidenav
    ) -> result
  }
  
  # --- --- --- -
  # Datasets ----
  # --- --- --- -
  
  if (id == "datasets"){
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      show_hide_sidenav
    ) -> result
  }
  
  # --- --- --- --- -
  # Vocabularies ----
  # --- --- --- --- -
  
  if (id == "vocabularies"){
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      show_hide_sidenav
    ) -> result
  }
  
  # --- --- -- -
  # Console ----
  # --- --- -- -
  
  if (id == "console"){
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        div(
          create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("execute_code"), iconProps = list(iconName = "Play")), text = i18n$t("run_code")),
          class = "reduced_sidenav_buttons"
        ),
        id = ns("reduced_sidenav")
      ),
      show_hide_sidenav
    ) -> result
  }
  
  # --- --- -- -
  # Plugins ----
  # --- --- -- -
  
  if (id %in% c("plugins")){
    
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav"),
        shinyjs::hidden(
          div(
            id = ns("edit_code_sidenav"),
            div(
              class = "sidenav_top",
              div(
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("edit_code_add_file"), iconProps = list(iconName = "Add")), text = i18n$t("add_file")),
                class = "small_icon_button"
              ),
              div(
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("edit_code_add_folder"), iconProps = list(iconName = "FabricNewFolder")), text = i18n$t("add_folder")),
                class = "small_icon_button"
              ),
              div(
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("save_file_code"), iconProps = list(iconName = "Save")), text = i18n$t("save_current_file")),
                class = "small_icon_button"
              ),
              div(
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("run_plugin_code"), iconProps = list(iconName = "Play")), text = i18n$t("run_plugin_code")),
                class = "small_icon_button"
              )
            ),
            uiOutput(ns("edit_code_directory_browser")),
            uiOutput(ns("edit_code_files_browser"))
          )
        )
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      show_hide_sidenav
    ) -> result
  }
  
  # --- --- --- --- --- --- --
  # Data cleaning scripts ----
  # --- --- --- --- --- --- --
  
  if (id == "data_cleaning"){
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      show_hide_sidenav
    ) -> result
  }
  
  # --- --- -- -
  # Catalog ----
  # --- --- -- -
  
  if (id == "catalog"){
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      show_hide_sidenav
    ) -> result
  }
  
  # --- --- --
  # Users ----
  # --- --- --
  
  if (id == "users") {
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      show_hide_sidenav
    ) -> result
  }
  
  # --- --- --- --- -
  # App database ----
  # --- --- --- --- -
  
  if (id == "app_db") {
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      show_hide_sidenav
    ) -> result
  }
  
  # --- --- --- --
  # Git repos ----
  # --- --- --- --
  
  if (id == "git_repos") {
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      show_hide_sidenav
    ) -> result
  }
  
  # --- -- -
  # Log ----
  # --- -- -
  
  if (id == "log") {
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      show_hide_sidenav
    ) -> result
  }
  
  # --- --- --- -
  # Concepts ----
  # --- --- --- -
  
  if (id == "concepts") {
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      show_hide_sidenav
    ) -> result
  }
  
  # --- --- -
  # Data ----
  # --- --- -
  
  if (id == "data"){
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav"),
        div(
          class = "sidenav_top",
          div(
            create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("add_tab"), iconProps = list(iconName = "Boards")), text = i18n$t("add_a_tab")),
            class = "small_icon_button"
          ),
          div(
            create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("add_widget"), iconProps = list(iconName = "RectangularClipping")), text = i18n$t("add_a_widget")),
            class = "small_icon_button"
          ),
          div(
            id = ns("edit_page_on_div"),
            create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("edit_page_on"), iconProps = list(iconName = "Edit")), text = i18n$t("edit_page")),
            class = "small_icon_button",
          ),
          shinyjs::hidden(
            div(
              id = ns("edit_page_off_div"),
              shiny.fluent::IconButton.shinyInput(ns("edit_page_off"), iconProps = list(iconName = "Accept")),
              class = "small_icon_button",
            )
          )
        ), br(),
        make_combobox(i18n, ns, id = "subset", label = "subset", width = "200px"),
        make_combobox(i18n, ns, id = "person", label = "person", allowFreeform = TRUE, autoComplete = TRUE, width = "200px"),
        make_combobox(i18n, ns, id = "visit_detail", label = "visit_detail", width = "200px"),
        div(id = ns("person_info_div"), uiOutput(ns("person_info")), class = "person_info")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      show_hide_sidenav,
    ) -> result
  }
  
  # --- --- -- -
  # Subsets ----
  # --- --- -- -
  
  if (id == "subsets") {
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      show_hide_sidenav
    ) -> result
  }
  
  # --- --- --- -
  # Messages ----
  # --- --- --- -
  
  if (id == "messages") {
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      show_hide_sidenav
    ) -> result
  }
  
  # --- --- --- --- -- -
  # Project console ----
  # --- --- --- --- -- -
  
  if (id == "project_console") {
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      show_hide_sidenav
    ) -> result
  }
  
  # --- --- --
  # Tasks ----
  # --- --- --
  
  if (id == "tasks") {
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      show_hide_sidenav
    ) -> result
  }
  
  result
}

#' @noRd 
mod_page_sidenav_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), language = "en", i18n = character(), debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (debug) cat(paste0("\n", now(), " - mod_page_sidenav (", id, ") - start"))
    
    # Show / hide sidenav
    
    js_vars <- paste0("
      var sidenav = document.getElementById('", id, "-sidenav');
      var currentWidth = window.getComputedStyle(sidenav).getPropertyValue('width');
      var button = document.getElementById('", id, "-show_hide_sidenav');
      var large_sidenav = document.getElementById('", id, "-large_sidenav');
      var reduced_sidenav = document.getElementById('", id, "-reduced_sidenav');
      var header_command_bar = document.getElementById('", id, "-header_command_bar');
    ")
    js_show_sidenav <- paste0("
      sidenav.style.width = '200px';
      sidenav.style.minWidth = '200px';
      sidenav.style.padding = '10px 20px 0px 20px';
      button.classList.add('button_hide_sidenav');
      button.classList.remove('button_show_sidenav');
      large_sidenav.style.display = 'block';
      reduced_sidenav.style.display = 'none';
      header_command_bar.style.marginLeft = '193px';
    ")
    js_hide_sidenav <- paste0("
      sidenav.style.width = '40px';
      sidenav.style.minWidth = '40px';
      sidenav.style.padding = 0;
      button.classList.remove('button_hide_sidenav');
      button.classList.add('button_show_sidenav');
      large_sidenav.style.display = 'none';
      reduced_sidenav.style.display = 'block';
      header_command_bar.style.marginLeft = '10px';
    ")
    
    if (id %in% c("home", "datasets", "vocabularies", "console", "plugins", "data_cleaning", "catalog", "users", "app_db")) r[[paste0(id, "_show_hide_sidenav")]] <- "hide"
    
    observeEvent(r[[paste0(id, "_show_hide_sidenav")]], {
      if (debug) cat(paste0("\n", now(), " - mod_page_sidenav - observer r$.._show_hide_sidenav"))
      
      if (r[[paste0(id, "_show_hide_sidenav")]] == "hide") shinyjs::runjs(paste0(js_vars, js_hide_sidenav))
      else shinyjs::runjs(paste0(js_vars, js_show_sidenav))
    })
    
    observeEvent(input$show_hide_sidenav, {
      if (debug) cat(paste0("\n", now(), " - mod_page_sidenav - ", id, " - observer input$show_hide_sidenav"))
      
      shinyjs::runjs(paste0(js_vars, " if (currentWidth === '200px') {", js_hide_sidenav, "} else { ", js_show_sidenav, "}"))
      
      if (id == "data"){

        category <- r$data_page
        
        if (length(r[[paste0(category, "_selected_tab")]]) > 0){
          if (!is.na(r[[paste0(category, "_selected_tab")]]) & r[[paste0(category, "_selected_tab")]] != 0){
            shinyjs::delay(300, shinyjs::runjs("window.dispatchEvent(new Event('resize'));"))
            sapply(r$data_grids, function(gridster_id){
              if (length(r$edit_page_activated) > 0){
                if (r$edit_page_activated) shinyjs::delay(400, shinyjs::runjs(paste0(gridster_id, ".enable().resize();")))
                else shinyjs::delay(400, shinyjs::runjs(paste0(gridster_id, ".disable().disable_resize();")))
              }
            })
          }
        }
      }
    })
  })
}
