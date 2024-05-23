#' @noRd
mod_page_sidenav_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  result <- ""
  
  hide_sidenav <- div(
    id = ns("show_hide_sidenav"),
    class = "button_sidenav button_hide_sidenav",
    onclick = paste0("Shiny.setInputValue('", id, "-show_hide_sidenav', Math.random());"),
  )
  
  show_sidenav <- div(
    id = ns("show_hide_sidenav"),
    class = "button_sidenav button_show_sidenav",
    onclick = paste0("Shiny.setInputValue('", id, "-show_hide_sidenav', Math.random());"),
  )
  
  reduced_sidenav_style = "width: 40px; min-width: 40px; padding: 0;"
  
  # App database ----
  
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
      hide_sidenav
    ) -> result
  }
  
  # Explore ----
  
  else if (id == "explore"){
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      hide_sidenav
    ) -> result
  }
  
  # Concepts ----
  
  else if (id == "concepts") {
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      hide_sidenav
    ) -> result
  }
  
  # Console ----
  
  else if (id == "console"){
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav"),
        div(
          shiny.fluent::PrimaryButton.shinyInput(ns("r_code"), i18n$t("run_code"), iconProps = list(iconName = "Play"), style = "width: calc(50% - 5px);"),
          onclick = paste0("Shiny.setInputValue('", id, "-execute_code', Math.random());")
        ),
        div(
          div(
            shiny.fluent::Dropdown.shinyInput(ns("programming_language"), label = i18n$t("programming_language"),
              options = list(
                list(key = "r", text = i18n$t("r")),
                list(key = "python", text = i18n$t("python")),
                list(key = "shell", text = i18n$t("shell"))
              ),
              value = "r"
            ),
            style = "width: 50%;"
          ),
          div(
            shiny.fluent::Dropdown.shinyInput(ns("output"), label = i18n$t("output"),
              options = list(
                list(key = "console", text = i18n$t("console")),
                list(key = "figure", text = i18n$t("figure")),
                list(key = "table", text = i18n$t("table_output")),
                list(key = "datatable", text = i18n$t("datatable_output")),
                list(key = "rmarkdown", text = i18n$t("rmarkdown"))
              ),
              value = "console"
            ),
            style = "width: 50%;"
          ),
          style = "display: flex; gap: 10px;"
        ),
        shinyjs::hidden(
          div(
            id = ns("plot_size_div"),
            div(
              div(
                shiny.fluent::SpinButton.shinyInput(ns("plot_width"), min = 0, value = 400, step = 50, label = i18n$t("width")),
                style = "width: calc(50% - 5px);"
              ),
              div(
                shiny.fluent::SpinButton.shinyInput(ns("plot_height"), min = 0, value = 300, step = 50, label = i18n$t("height")),
                style = "width: calc(50% - 5px);"
              ),
              style = "display: flex; gap: 10px;"
            ),
            div(
              div(
                shiny.fluent::SpinButton.shinyInput(ns("plot_dpi"), min = 1, value = 100, step = 50, label = i18n$t("dpi")),
                style = "width: calc(50% - 5px);"
              ),
              style = "display: flex; gap: 10px;"
            ),
          )
        )
      ),
      div(
        id = ns("reduced_sidenav"),
        div(
          create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("execute_code"), iconProps = list(iconName = "Play")), text = i18n$t("run_code")),
          class = "reduced_sidenav_buttons"
        ),
        style = "display: none;"
      ),
      hide_sidenav
    ) -> result
  }
  
  # Data ----
  
  else if (id == "data"){
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
      hide_sidenav
    ) -> result
  }
  
  # Datasets ----
  
  else if (id == "datasets"){
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav"),
        div(
          id = ns("all_elements_reduced_sidenav"),
          create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("create_element"), iconProps = list(iconName = "Add")), text = i18n$t("create_dataset")),
          class = "reduced_sidenav_buttons"
        ),
        shinyjs::hidden(
          div(
            id = ns("edit_code_reduced_sidenav"),
            div(
              div(
                id = ns("run_code_div"),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("run_code"), iconProps = list(iconName = "Play")), text = i18n$t("run_code")),
                class = "reduced_sidenav_buttons",
              ),
              div(
                id = ns("save_code_div"),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("save_code"), iconProps = list(iconName = "Save")), text = i18n$t("save")),
                class = "reduced_sidenav_buttons",
              )
            )
          )
        )
      ),
      hide_sidenav
    ) -> result
  }
  
  # Data cleaning scripts ----
  
  else if (id == "data_cleaning"){
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      hide_sidenav
    ) -> result
  }
  
  # Git repos ----
  
  else if (id == "git_repos") {
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      hide_sidenav
    ) -> result
  }
  
  # Home ----
  
  else if (id == "home"){
    div(
      id = ns("sidenav"),
      class = "sidenav",
      style = reduced_sidenav_style,
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      )
    ) -> result
  }
  
  # Log ----
  
  else if (id == "log") {
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav"),
        div(
          create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("refresh_log"), iconProps = list(iconName = "Refresh")), text = i18n$t("refresh_log")),
          class = "reduced_sidenav_buttons"
        ),
        div(
          create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("reset_log"), iconProps = list(iconName = "Delete")), text = i18n$t("reset_log")),
          class = "reduced_sidenav_buttons"
        ),
      ),
      hide_sidenav
    ) -> result
  }
  
  # Project messages ----
  
  else if (id == "project_messages") {
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      hide_sidenav
    ) -> result
  }
  
  # Plugins ----
  
  else if (id %in% c("plugins")){
    
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav"),
        shinyjs::hidden(
          div(
            id = ns("edit_code_large_sidenav"),
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
        id = ns("reduced_sidenav"),
        div(
          id = ns("all_elements_reduced_sidenav"),
          create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("create_element"), iconProps = list(iconName = "Add")), text = i18n$t("create_plugin")),
          class = "reduced_sidenav_buttons"
        ),
        shinyjs::hidden(
          div(
            id = ns("run_code_reduced_sidenav"),
            div(
              div(
                id = ns("edit_page_on_div"),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("edit_page_on"), iconProps = list(iconName = "Edit")), text = i18n$t("edit_page")),
                class = "reduced_sidenav_buttons_11",
              ),
              shinyjs::hidden(
                div(
                  id = ns("edit_page_off_div"),
                  shiny.fluent::IconButton.shinyInput(ns("edit_page_off"), iconProps = list(iconName = "Accept")),
                  class = "reduced_sidenav_buttons_11",
                )
              )
            )
          )
        )
      ),
      hide_sidenav
    ) -> result
  }
  
  # Projects ----
  
  else if (id == "projects") {
    div(
      id = ns("sidenav"),
      class = "sidenav", style = reduced_sidenav_style,
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav"),
        div(
          id = ns("all_elements_reduced_sidenav"),
          create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("create_element"), iconProps = list(iconName = "Add")), text = i18n$t("create_project"))
        ),
        class = "reduced_sidenav_buttons"
      )#,
      # show_sidenav
    ) -> result
  }
  
  # Project console ----
  
  else if (id == "project_console") {
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      hide_sidenav
    ) -> result
  }
  
  # Subsets ----
  
  else if (id == "subsets") {
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav"),
        div(
          id = ns("all_elements_reduced_sidenav"),
          create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("create_element"), iconProps = list(iconName = "Add")), text = i18n$t("create_subset")),
          class = "reduced_sidenav_buttons"
        ),
        shinyjs::hidden(
          div(
            id = ns("edit_code_reduced_sidenav"),
            div(
              div(
                id = ns("run_code_div"),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("run_code"), iconProps = list(iconName = "Play")), text = i18n$t("run_code")),
                class = "reduced_sidenav_buttons",
              ),
              div(
                id = ns("save_code_div"),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("save_code"), iconProps = list(iconName = "Save")), text = i18n$t("save")),
                class = "reduced_sidenav_buttons",
              )
            )
          )
        )
      ),
      hide_sidenav
    ) -> result
  }
  
  # Tasks ----
  
  else if (id == "tasks") {
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      hide_sidenav
    ) -> result
  }
  
  # Users ----
  
  else if (id == "users") {
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      hide_sidenav
    ) -> result
  }
  
  # Vocabularies ----
  
  else if (id == "vocabularies"){
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      hide_sidenav
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
      var header_command_bar = document.getElementById('", id, "-command_bar_1_div');
    ")
    js_show_sidenav <- paste0("
      sidenav.style.width = '220px';
      sidenav.style.minWidth = '220px';
      sidenav.style.padding = '10px 10px 0px 10px';
      button.classList.add('button_hide_sidenav');
      button.classList.remove('button_show_sidenav');
      reduced_sidenav.style.display = 'none';
      setTimeout(() => large_sidenav.style.display = 'block', 300);
      header_command_bar.style.marginLeft = '193px';
    ")
    js_hide_sidenav <- paste0("
      sidenav.style.width = '40px';
      sidenav.style.minWidth = '40px';
      sidenav.style.padding = 0;
      button.classList.remove('button_hide_sidenav');
      button.classList.add('button_show_sidenav');
      large_sidenav.style.display = 'none';
      setTimeout(() => reduced_sidenav.style.display = 'block', 300);
      header_command_bar.style.marginLeft = '10px';
    ")
    
    if (id %in% c("app_db", "data_cleaning", "datasets", "explore", "home", "log", "plugins", "projects", "subsets", "users", "vocabularies")) r[[paste0(id, "_show_hide_sidenav")]] <- "hide"
    
    observeEvent(r[[paste0(id, "_show_hide_sidenav")]], {
      if (debug) cat(paste0("\n", now(), " - mod_page_sidenav - observer r$.._show_hide_sidenav"))
      
      if (r[[paste0(id, "_show_hide_sidenav")]] == "hide") shinyjs::runjs(paste0(js_vars, js_hide_sidenav))
      else shinyjs::runjs(paste0(js_vars, js_show_sidenav))
    })
    
    observeEvent(input$show_hide_sidenav, {
      if (debug) cat(paste0("\n", now(), " - mod_page_sidenav - ", id, " - observer input$show_hide_sidenav"))
      
      if (input$show_hide_sidenav == "hide") shinyjs::runjs(paste0(js_vars, js_hide_sidenav))
      else if (input$show_hide_sidenav == "show") shinyjs::runjs(paste0(js_vars, js_show_sidenav))
      else shinyjs::runjs(paste0(js_vars, " if (currentWidth === '220px') {", js_hide_sidenav, "} else { ", js_show_sidenav, "}"))
    })
  })
}
