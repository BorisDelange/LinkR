#' @noRd
mod_page_sidenav_ui <- function(id){
  
  pages_variables_list <- get("pages_variables_list", envir = parent.frame())
  for (obj_name in pages_variables_list) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  ns <- NS(id)
  result <- ""
  
  hide_sidenav <- div(
    id = ns("show_hide_sidenav"),
    class = "button_sidenav button_hide_sidenav",
    onclick = paste0("Shiny.setInputValue('", id, "-show_hide_sidenav', Math.random());"),
    span()
  )
  
  show_sidenav <- div(
    id = ns("show_hide_sidenav"),
    class = "button_sidenav button_show_sidenav",
    onclick = paste0("Shiny.setInputValue('", id, "-show_hide_sidenav', Math.random());"),
    span()
  )
  
  reduced_sidenav_style <- "width: 40px; min-width: 40px; padding: 0;"
  
  # Common UI ----
  
  edit_code_buttons <- function(){
    shinyjs::hidden(
      div(
        id = ns("edit_code_buttons"),
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
  }
  
  edit_summary_buttons <- function(){
    tagList(
      shinyjs::hidden(
        div(
          id = ns("edit_summary_div"),
          create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("edit_summary"), iconProps = list(iconName = "Edit")), text = i18n$t("edit_informations")),
          class = "reduced_sidenav_buttons_11"
        )
      ),
      shinyjs::hidden(
        div(
          id = ns("save_summary_div"),
          create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("save_summary"), iconProps = list(iconName = "Accept")), text = i18n$t("save")),
          class = "reduced_sidenav_buttons"
        )
      ),
      shinyjs::hidden(
        div(
          id = ns("delete_element_div"),
          create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("delete_element"), iconProps = list(iconName = "Delete")), text = i18n$t("delete")),
          class = "reduced_sidenav_buttons"
        )
      )
    )
  }
  
  element_management_buttons <- function(type){
    tagList(
      create_hover_card(
        ui = shinyjs::hidden(
          div(
            id = ns("create_element_button"),
            shiny.fluent::IconButton.shinyInput(ns("create_element"), iconProps = list(iconName = "Add"))
          )
        ),
        text = i18n$t(paste0("create_", type))
      ),
      # create_hover_card(ui = div(shiny.fluent::IconButton.shinyInput(ns("reload_elements_var"), iconProps = list(iconName = "SyncOccurence"))), text = i18n$t("reload_list")),
      create_hover_card(
        ui = shinyjs::hidden(
          div(
            id = ns("import_element_button"),
            shiny.fluent::IconButton.shinyInput(ns("import_element"), iconProps = list(iconName = "Upload"))
          )
        ),
        text = i18n$t(paste0("import_", type))
      )
    )
  }
  
  # App database ----
  
  if (id == "app_db") {
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav"),
        shinyjs::hidden(
          div(
            id = ns("connection_settings_reduced_sidenav"),
            shinyjs::hidden(
              div(
                id = ns("connection_settings_buttons"),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("save_app_db_settings"), iconProps = list(iconName = "Save")), text = i18n$t("save_settings")),
                shinyjs::hidden(
                  div(
                    id = ns("test_connection_div"),
                    create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("test_connection"), iconProps = list(iconName = "Play")), text = i18n$t("test_connection"))
                  )
                ),
                class = "reduced_sidenav_buttons"
              )
            )
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("request_db_reduced_sidenav"),
            shinyjs::hidden(
              div(
                id = ns("request_db_buttons"),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("run_code"), iconProps = list(iconName = "Play")), text = i18n$t("run_code")),
                class = "reduced_sidenav_buttons"
              )
            )
          )
        )
      ),
      shinyjs::hidden(show_sidenav)
    ) -> result
  }
  
  # Concepts ----
  
  else if (id == "concepts") {
    div(
      id = ns("sidenav"),
      class = "sidenav", style = reduced_sidenav_style,
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav"),
        # create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("settings"), iconProps = list(iconName = "Settings")), text = i18n$t("settings")),
        create_hover_card(
          ui = shinyjs::hidden(
            div(
              id = ns("reload_concepts_count_button"),
              shiny.fluent::IconButton.shinyInput(ns("reload_concepts_count"), iconProps = list(iconName = "SyncOccurence"))
            )
          ),
          text = i18n$t("reload_concepts_count")
        ),
        class = "reduced_sidenav_buttons"
      ),
      shinyjs::hidden(show_sidenav)
    ) -> result
  }
  
  # Console ----
  
  else if (id == "console"){
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav"),
        shinyjs::hidden(
          div(
            id = ns("console_sidenav_buttons"),
            div(
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
            )
          )
        )
      ),
      div(
        id = ns("reduced_sidenav"),
        div(
          create_hover_card(
            ui = shinyjs::hidden(
              div(
                id = ns("reduced_sidenav_execute_code_button"),
                shiny.fluent::IconButton.shinyInput(ns("execute_code"), iconProps = list(iconName = "Play"))
              )
            ),
            text = i18n$t("run_code")
          ),
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
        shinyjs::hidden(
          div(
            id = ns("project_content_management"),
            div(
              div(shiny.fluent::PrimaryButton.shinyInput(ns("add_tab"), i18n$t("tab"), iconProps = list(iconName = "Add"), style = "width: 100%;"),  style = "width: 50%;"),
              div(shiny.fluent::PrimaryButton.shinyInput(ns("add_widget"), i18n$t("widget"), iconProps = list(iconName = "Add"), style = "width: 100%;"),  style = "width: 50%;"),
              style = "display: flex; gap: 10px;"
            ),
            shiny.fluent::DefaultButton.shinyInput(ns("edit_page"), i18n$t("edit_page"), iconProps = list(iconName = "Edit"), style = "width: 100%; margin-top: 5px;"),
            shiny.fluent::DefaultButton.shinyInput(ns("show_hide_widgets_buttons"), i18n$t("hide_widgets_buttons"), iconProps = list(iconName = "Hide"), style = "width: 100%; margin-top: 5px;")
          )
        ), br(),
        div(
          selectizeInput(ns("subset"), i18n$t("subset"), choices = NULL, multiple = FALSE, selected = FALSE),
          style = "border-top: solid 1px #ccc; padding-top: 10px;"
        ),
        div(
          id = ns("subset_date_div"),
          class = "subset_slider_input",
          tags$strong(i18n$t("filter_subset_data")),
          sliderInput(
            ns("subset_date_slider"),
            label = NULL,
            ticks = FALSE,
            min = as.Date("1970-01-01"),
            max = Sys.Date(),
            value = c(as.Date("1970-01-01"), Sys.Date()),
            timeFormat = ifelse(language == "fr", "%d-%m-%Y", "%Y-%m-%d")
          ),
          div(
            style = "margin-top: 15px; display: flex; gap: 5px;",
            class = "subset_date_pickers",
            shiny.fluent::DatePicker.shinyInput(
              ns("subset_start_date"),
              value = as.Date("1970-01-01"),
              allowTextInput = TRUE,
              minDate = htmlwidgets::JS(sprintf("new Date('%s')", "1970-01-01")),
              maxDate = htmlwidgets::JS(sprintf("new Date('%s')", as.character(Sys.Date()))),
              onSelectDate = htmlwidgets::JS("(date) => Shiny.setInputValue('data-subset_start_date', new Date(date.getTime() - (date.getTimezoneOffset() * 60000)).toISOString().split('T')[0])"),
              formatDate = htmlwidgets::JS(paste0("(date) => date ? new Intl.DateTimeFormat('", language, "-", toupper(language), "').format(date) : ''"))
            ),
            span(tolower(i18n$t("to")), style = "margin: auto;"),
            shiny.fluent::DatePicker.shinyInput(
              ns("subset_end_date"),
              value = Sys.Date(),
              allowTextInput = TRUE,
              minDate = htmlwidgets::JS(sprintf("new Date('%s')", "1970-01-01")),
              maxDate = htmlwidgets::JS(sprintf("new Date('%s')", as.character(Sys.Date()))),
              onSelectDate = htmlwidgets::JS("(date) => Shiny.setInputValue('data-subset_end_date', new Date(date.getTime() - (date.getTimezoneOffset() * 60000)).toISOString().split('T')[0])"),
              formatDate = htmlwidgets::JS(paste0("(date) => date ? new Intl.DateTimeFormat('", language, "-", toupper(language), "').format(date) : ''"))
            )
          ),
          div(
            id = ns("subset_date_filters_button"),
            style = "margin-top: 15px;",
            shiny.fluent::PrimaryButton.shinyInput(ns("apply_subset_date_filters"), i18n$t("filter_data"), iconProps = list(iconName = "Play"))
          )
        ),
        div(id = ns("subset_info_div"), uiOutput(ns("subset_info")), class = "subset_info"),
        div(
          style = "position: relative;",
          div(
            id = ns("person_dropdown_div"),
            selectizeInput(ns("person"), i18n$t("person"), choices = NULL, multiple = FALSE), 
            style = "margin-top: 10px;"
          ),
          shinyjs::hidden(
            div(
              id = ns("patient_switching_buttons_div"),
              shinyjs::hidden(
                div(
                  id = ns("patient_switching_buttons"),
                  div(
                    shiny.fluent::IconButton.shinyInput(ns("previous_patient"), iconProps = list(iconName = "ChevronLeft")),
                    class = "patient_lvl_small_icon_button"
                  ),
                  uiOutput(ns("person_switch_nums")),
                  div(
                    shiny.fluent::IconButton.shinyInput(ns("next_patient"), iconProps = list(iconName = "ChevronRight")),
                    class = "patient_lvl_small_icon_button"
                  ),
                  style = "display: flex; position: absolute; top: 0; right: 0; color: #808080;"
                )
              )
            )
          )
        ),
        div(
          id = ns("visit_detail_dropdown_div"),
          selectizeInput(ns("visit_detail"), i18n$t("visit_detail"), choices = NULL, multiple = FALSE),
          style = "margin-top: 10px;"
        ),
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
      class = "sidenav", style = reduced_sidenav_style,
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav"),
        div(
          id = ns("all_elements_reduced_sidenav"),
          element_management_buttons("dataset"),
          class = "reduced_sidenav_buttons"
        ),
        shinyjs::hidden(
          div(
            id = ns("summary_reduced_sidenav"),
            edit_summary_buttons()
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("edit_code_reduced_sidenav"),
            edit_code_buttons()
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("share_reduced_sidenav")
          )
        )
      ),
      shinyjs::hidden(show_sidenav)
    ) -> result
  }
  
  # Data cleaning scripts ----
  
  else if (id == "data_cleaning"){
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
          element_management_buttons("data_cleaning_script"),
          class = "reduced_sidenav_buttons"
        ),
        shinyjs::hidden(
          div(
            id = ns("summary_reduced_sidenav"),
            edit_summary_buttons()
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("edit_code_reduced_sidenav"),
            edit_code_buttons()
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("share_reduced_sidenav")
          )
        )
      )
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
        id = ns("reduced_sidenav"),
        div(
          id = ns("all_repos_reduced_sidenav"),
          div(
            id = ns("show_list_div"),
            create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("show_list"), iconProps = list(iconName = "BulletedList2")), text = i18n$t("show_git_repos_list"))
          ),
          shinyjs::hidden(
            div(
              id = ns("show_map_div"),
              create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("show_map"), iconProps = list(iconName = "POI")), text = i18n$t("show_git_repos_map")),
            )
          ),
          create_hover_card(
            ui = shinyjs::hidden(
              div(
                id = ns("create_git_repo_button"),
                shiny.fluent::IconButton.shinyInput(ns("create_git_repo"), iconProps = list(iconName = "Add"))
              )
            ),
            text = i18n$t("create_git_repo")
          ),
          class = "reduced_sidenav_buttons"
        ),
        shinyjs::hidden(
          div(
            id = ns("one_repo_reduced_sidenav"),
            shinyjs::hidden(
              div(
                id = ns("save_git_repo_button"),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("save_git_repo"), iconProps = list(iconName = "Save")), text = i18n$t("commit_and_push")),
                class = "reduced_sidenav_buttons"
              )
            )
            # div(
            #   id = ns("reload_git_repo_div"),
            #   create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("reload_git_repo"), iconProps = list(iconName = "SyncOccurence")), text = i18n$t("reload_git_repo")),
            #   class = "reduced_sidenav_buttons"
            # )
          )
        )
      ),
      shinyjs::hidden(show_sidenav)
    ) -> result
  }
  
  # Home ----
  
  else if (id == "home"){
    div(
      id = ns("sidenav"),
      class = "sidenav", style = reduced_sidenav_style,
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
      class = "sidenav", style = reduced_sidenav_style,
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav"),
        shinyjs::hidden(
          div(
            id = ns("user_log_buttons"),
            div(
              create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("refresh_log"), iconProps = list(iconName = "Refresh")), text = i18n$t("refresh_log")),
              class = "reduced_sidenav_buttons"
            ),
            div(
              create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("reset_log"), iconProps = list(iconName = "Delete")), text = i18n$t("reset_log")),
              class = "reduced_sidenav_buttons"
            )
          )
        )
      )
    ) -> result
  }
  
  # Plugins ----
  
  else if (id %in% c("plugins")){
    
    div(
      id = ns("sidenav"),
      class = "sidenav", style = reduced_sidenav_style,
      div(
        id = ns("large_sidenav"),
        shinyjs::hidden(
          div(
            id = ns("edit_code_large_sidenav"),
            shinyjs::hidden(
              div(
                id = ns("edit_code_buttons"),
                class = "sidenav_top",
                div(
                  create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("edit_code_add_file"), iconProps = list(iconName = "Add")), text = i18n$t("add_file")),
                  class = "small_icon_button"
                ),
                div(
                  create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("save_file_code"), iconProps = list(iconName = "Save")), text = i18n$t("save_current_file")),
                  class = "small_icon_button"
                ),
                div(
                  create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("select_concepts"), iconProps = list(iconName = "BulletedList")), text = i18n$t("select_concepts")),
                  class = "small_icon_button"
                ),
                div(
                  create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("run_plugin_code"), iconProps = list(iconName = "Play")), text = i18n$t("run_plugin_code")),
                  class = "small_icon_button"
                )
              )
            ),
            shinyjs::hidden(div(id = ns("edit_code_files_browser_div"), uiOutput(ns("edit_code_files_browser"))))
          )
        )
      ),
      div(
        id = ns("reduced_sidenav"),
        div(
          id = ns("all_elements_reduced_sidenav"),
          element_management_buttons("plugin"),
          class = "reduced_sidenav_buttons"
        ),
        shinyjs::hidden(
          div(
            id = ns("summary_reduced_sidenav"),
            edit_summary_buttons()
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("run_code_reduced_sidenav"),
            div(
              shinyjs::hidden(
                div(
                  id = ns("edit_page_on_div"),
                  create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("edit_page_on"), iconProps = list(iconName = "Edit")), text = i18n$t("edit_page")),
                  class = "reduced_sidenav_buttons_11"
                )
              ),
              shinyjs::hidden(
                div(
                  id = ns("edit_page_off_div"),
                  shiny.fluent::IconButton.shinyInput(ns("edit_page_off"), iconProps = list(iconName = "Accept")),
                  class = "reduced_sidenav_buttons"
                )
              )
            ),
            div(
              create_hover_card(
                ui = shinyjs::hidden(
                  div(
                    id = ns("reload_plugin_code_button"),
                    shiny.fluent::IconButton.shinyInput(ns("reload_plugin_code"), iconProps = list(iconName = "SyncOccurence"))
                  )
                ), 
                text = i18n$t("reload_plugin_code")
              ),
              class = "reduced_sidenav_buttons"
            )
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("share_reduced_sidenav")
          )
        )
      ),
      shinyjs::hidden(show_sidenav)
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
          element_management_buttons("project"),
          class = "reduced_sidenav_buttons"
        ),
        shinyjs::hidden(
          div(
            id = ns("summary_reduced_sidenav"),
            edit_summary_buttons()
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("dataset_reduced_sidenav"),
            # div(
            #   create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("save_dataset"), iconProps = list(iconName = "Save")), text = i18n$t("save")),
            #   class = "reduced_sidenav_buttons"
            # ),
            # div(
            #   create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("reload_dataset"), iconProps = list(iconName = "Play")), text = i18n$t("reload_dataset")),
            #   class = "reduced_sidenav_buttons"
            # )
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("share_reduced_sidenav")
          )
        )
      )
    ) -> result
  }
  
  # Project files ----
  
  else if (id == "project_files") {
    div(
      id = ns("sidenav"),
      class = "sidenav",
      div(
        id = ns("large_sidenav"),
        div(
          shinyjs::hidden(
            div(
              id = ns("edit_code_buttons"),
              class = "sidenav_top", style = "display: flex; justify-content: space-between;",
              div(
                div(
                  create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("add_file"), iconProps = list(iconName = "Add")), text = i18n$t("add_file")),
                  class = "small_icon_button"
                ),
                div(
                  create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("save_file_code"), iconProps = list(iconName = "Save")), text = i18n$t("save_current_file")),
                  class = "small_icon_button"
                ),
                div(
                  create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("run_code"), iconProps = list(iconName = "Play")), text = i18n$t("run_code")),
                  class = "small_icon_button"
                ),
                style = "display: flex;"
              ),
              div(shiny.fluent::Dropdown.shinyInput(ns("code_output"), i18n$t("output")), style = "width: 50%;")
            )
          ),
          shinyjs::hidden(div(id = ns("files_browser_div"), uiOutput(ns("files_browser"))))
        )
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
      class = "sidenav", style = reduced_sidenav_style,
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav"),
        div(
          id = ns("all_elements_reduced_sidenav"),
          shinyjs::hidden(
            div(
              id = ns("subset_buttons"),
              create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("create_element"), iconProps = list(iconName = "Add")), text = i18n$t("create_subset")),
              create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("reload_elements_var"), iconProps = list(iconName = "SyncOccurence")), text = i18n$t("reload_list"))
            )
          ),
          class = "reduced_sidenav_buttons"
        ),
        shinyjs::hidden(
          div(
            id = ns("summary_reduced_sidenav"),
            edit_summary_buttons()
          )
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
      )
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
  
  # User settings ----
  
  else if (id == "user_settings") {
    div(
      id = ns("sidenav"),
      class = "sidenav", style = reduced_sidenav_style,
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav"),
        div(
          id = ns("save_settings_div"),
          create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("save_settings"), iconProps = list(iconName = "Save")), text = i18n$t("save")),
          class = "reduced_sidenav_buttons",
        )
      ),
      shinyjs::hidden(show_sidenav)
    ) -> result
  }
  
  # Users ----
  
  else if (id == "users") {
    div(
      id = ns("sidenav"),
      class = "sidenav", style = reduced_sidenav_style,
      div(
        id = ns("large_sidenav")
      ),
      div(
        id = ns("reduced_sidenav")
      ),
      shinyjs::hidden(show_sidenav)
    ) -> result
  }
  
  # Vocabularies ----
  
  else if (id == "vocabularies"){
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
          create_hover_card(
            ui = shinyjs::hidden(
              div(
                id = ns("create_element_button"),
                shiny.fluent::IconButton.shinyInput(ns("create_element"), iconProps = list(iconName = "Add"))
              )
            ),
            text = i18n$t("create_vocabulary")
          ),
          create_hover_card(
            ui = shinyjs::hidden(
              div(
                id = ns("import_concepts_div_1"),
                shiny.fluent::IconButton.shinyInput(ns("import_concepts_1"), iconProps = list(iconName = "Upload"))
              )
            ),
            text = i18n$t("import_concepts_or_vocabularies")
          ),
          class = "reduced_sidenav_buttons"
        ),
        shinyjs::hidden(
          div(
            id = ns("summary_reduced_sidenav"),
            edit_summary_buttons()
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("concepts_reduced_sidenav"),
            create_hover_card(
              ui = shinyjs::hidden(
                div(
                  id = ns("import_concepts_div_2"),
                  shiny.fluent::IconButton.shinyInput(ns("import_concepts_2"), iconProps = list(iconName = "Upload"))
                )
              ),
              text = i18n$t("import_concepts_or_vocabularies")
            ),
            class = "reduced_sidenav_buttons"
          )
        )
      ),
      shinyjs::hidden(show_sidenav)
    ) -> result
  }
  
  result
}

#' @noRd 
mod_page_sidenav_server <- function(id){
  
  pages_variables_list <- get("pages_variables_list", envir = parent.frame())
  for (obj_name in pages_variables_list) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Show / hide sidenav
    
    js_vars <- paste0("
      var sidenav = document.getElementById('", id, "-sidenav');
      var currentWidth = window.getComputedStyle(sidenav).getPropertyValue('width');
      var button = document.getElementById('", id, "-show_hide_sidenav');
      var large_sidenav = document.getElementById('", id, "-large_sidenav');
      var reduced_sidenav = document.getElementById('", id, "-reduced_sidenav');
    ")
    js_show_sidenav <- paste0("
      if (sidenav && button && large_sidenav && reduced_sidenav) { 
        sidenav.style.width = '220px';
        sidenav.style.minWidth = '220px';
        sidenav.style.padding = '10px 10px 0px 10px';
        button.classList.add('button_hide_sidenav');
        button.classList.remove('button_show_sidenav');
        reduced_sidenav.style.display = 'none';
        setTimeout(() => large_sidenav.style.display = 'block', 300);
      }
    ")
    js_hide_sidenav <- paste0("
      if (sidenav && button && large_sidenav && reduced_sidenav) { 
        sidenav.style.width = '40px';
        sidenav.style.minWidth = '40px';
        sidenav.style.padding = 0;
        button.classList.remove('button_hide_sidenav');
        button.classList.add('button_show_sidenav');
        large_sidenav.style.display = 'none';
        setTimeout(() => reduced_sidenav.style.display = 'block', 300);
      }
    ")
    
    # Prevent display bug
    if (id == "plugins") js_show_sidenav <- js_show_sidenav %>% gsub("setTimeout\\(\\(\\) => large_sidenav.style.display = 'block', 300\\);", "large_sidenav.style.display = 'block';", .)
    
    if (id %in% c("app_db", "data_cleaning", "datasets", "git_repos", "home", "log", "plugins", "projects", "subsets", "users", "vocabularies")) r[[paste0(id, "_show_hide_sidenav")]] <- "hide"
    
    observe_event(r[[paste0(id, "_show_hide_sidenav")]], {
      
      if (r[[paste0(id, "_show_hide_sidenav")]] == "hide") shinyjs::runjs(paste0(js_vars, js_hide_sidenav))
      else shinyjs::runjs(paste0(js_vars, js_show_sidenav))
    })
    
    observe_event(input$show_hide_sidenav, {
      
      if (input$show_hide_sidenav == "hide") shinyjs::runjs(paste0(js_vars, js_hide_sidenav))
      else if (input$show_hide_sidenav == "show") shinyjs::runjs(paste0(js_vars, js_show_sidenav))
      else shinyjs::runjs(paste0(js_vars, " if (currentWidth === '220px') {", js_hide_sidenav, "} else { ", js_show_sidenav, "}"))
    })
  })
}
