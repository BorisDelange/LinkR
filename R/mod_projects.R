#' @noRd
mod_projects_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  
  pivot_item_js <- paste0("
    Shiny.setInputValue('", id, "-current_tab', this.id);
    Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());"
  )
  
  div(class = "main",
      
    # All projects ----
    
    div(
      id = ns("all_projects"),
      shiny.fluent::Breadcrumb(items = list(list(key = "main", text = i18n$t("projects"))), maxDisplayedItems = 3),
      div(shiny.fluent::SearchBox.shinyInput(ns("search_project")), style = "width:320px; margin:10px 0 0 10px;"),
      uiOutput(ns("projects"))
    ),
    
    # Create a project modal ----
    
    shinyjs::hidden(
      div(
        id = ns("create_project_modal"),
        div(
          div(
            tags$h1(i18n$t("create_project")),
            shiny.fluent::IconButton.shinyInput(ns("close_create_project_modal"), iconProps = list(iconName = "ChromeClose")),
            class = "create_element_modal_head small_close_button"
          ),
          div(
            make_textfield(i18n, ns, id = "project_creation_name", label = "name", width = "200px"),
            div(
              shiny.fluent::PrimaryButton.shinyInput(ns("add_project"), i18n$t("add")),
              class = "create_element_modal_buttons"
            ),
          ),
          class = "create_project_modal_content"
        ),
        class = "create_element_modal"
      )
    ),
    
    # Project details ----
    
    shinyjs::hidden(
      div(
        id = ns("one_project"),
        div(
          uiOutput(ns("project_breadcrumb")),
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
                div(shiny.fluent::PrimaryButton.shinyInput(ns("delete_project"), i18n$t("delete")), class = "delete_button"),
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
    ),
    
    # Delete a project modal ----
    
    shinyjs::hidden(
      div(
        id = ns("delete_project_modal"),
        div(
          tags$h1(i18n$t("delete_project_title")), tags$p(i18n$t("delete_project_text")),
          div(
            shiny.fluent::DefaultButton.shinyInput(ns("close_project_deletion_modal"), i18n$t("dont_delete")),
            div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_project_deletion"), i18n$t("delete")), class = "delete_button"),
            class = "delete_modal_buttons"
          ),
          class = "delete_modal_content"
        ),
        class = "delete_modal"
      )
    )
  )
}

#' @noRd 
mod_projects_server <- function(id, r, d, m, language, i18n, debug){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
   
    # |-------------------------------- -----
    
    if (debug) cat(paste0("\n", now(), " - mod_projects - ", id, " - start"))
    
    # Pivot project divs
    all_divs <- c("summary", "datasets", "data_cleaning_scripts", "share")
    
    # Project current tab ----
    
    observeEvent(input$current_tab_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer input$current_tab_trigger"))
      
      r$project_current_tab <- gsub(paste0(id, "-"), "", input$current_tab, fixed = FALSE)
    })
    
    observeEvent(r$project_current_tab, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer r$project_current_tab"))
      
      # Show or hide pages depending on selected tab
      divs <- setdiff(all_divs, r$project_current_tab)
      divs <- c(paste0(divs, "_reduced_sidenav"), paste0(divs, "_large_sidenav"), paste0(divs, "_div"))
      
      sapply(c(divs), shinyjs::hide)
      sapply(c(paste0(r$project_current_tab, "_div"), paste0(r$project_current_tab, "_reduced_sidenav"), paste0(r$project_current_tab, "_large_sidenav")), shinyjs::show)
      
      # Change selected tab
      sapply(all_divs, function(button_id) shinyjs::removeClass(class = "selected_pivot_item", selector = paste0("#", id, "-", button_id)))
      shinyjs::addClass(class = "selected_pivot_item", selector = paste0("#", id, "-", r$project_current_tab))
      
      r$projects_show_hide_sidenav <- "hide"
    })
    
    # |-------------------------------- -----
    
    # Search a project ----
    
    observeEvent(input$search_project, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - ", id, " - observer input$search_project"))
      
      if (input$search_project == "") r$filtered_projects_long <- r$projects_long
      else {
        filtered_ids <- r$projects_long %>% dplyr::filter(name == paste0("name_", language) & grepl(tolower(input$search_project), tolower(value)))
        r$filtered_projects_long <- r$projects_long %>% dplyr::filter(study_id %in% filtered_ids)
      }
      
      r$reload_projects_list <- now()
    })
    
    # Reload projects widgets -----
    
    observeEvent(r$reload_projects_list, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - ", id, " - observer r$reload_projects_list"))
      
      projects_ui <- load_projects_ui(id, r$filtered_projects_long, r$projects_users, language, i18n)
      output$projects <- renderUI(projects_ui)
      
      # Unlock reactivity
      shinyjs::show("projects")
    })
    
    # A project is selected ----
    
    observeEvent(input$selected_project, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer input$selected_project"))
      
      # Load data & concept pages if not already loaded
      # If not already loaded, project is loaded after data pages server side is loaded
      # Else, project is loaded directly
      # Delay to change page before executing server
      
      if (length(r$loaded_pages$data) == 0){
        r$load_page <- "data"
        r$data_page <- "patient_lvl"
      }
      else r$load_project_trigger <- now()
    })
    
    # Trigger to load a project
    observeEvent(r$load_project_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_home - observer r$load_project_trigger"))
      
      req(length(input$selected_project) > 0)
      
      # shiny.router::change_page("patient_level_data")
      shinyjs::delay(500, m$selected_study <- input$selected_project)
    })
    
    observeEvent(input$selected_project_settings_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer input$selected_project_settings_trigger"))
      
      r$selected_project_settings <- input$selected_project_settings
      r$selected_project_settings_trigger <- now()
    })
    
    observeEvent(r$selected_project_settings_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer r$selected_project_settings_trigger"))
      
      sapply(c("all_projects", "all_projects_reduced_sidenav"), shinyjs::hide)
      shinyjs::show("one_project")
      
      project_wide <- r$projects_wide %>% dplyr::filter(id == r$selected_project_settings)
      project_long <- r$projects_long %>% dplyr::filter(id == r$selected_project_settings)
      
      r$selected_project_name <- project_wide$name
      r$selected_project_unique_id <- project_long %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value)
      
      output$project_breadcrumb <- renderUI(
        shiny.fluent::Breadcrumb(items = list(
          list(
            key = "main", text = i18n$t("projects"), href = shiny.router::route_link("projects"), 
            onClick = htmlwidgets::JS(paste0("item => { Shiny.setInputValue('", id, "-show_projects_home', Math.random()); }"))
          ),
          list(key = "main", text = r$selected_project_name))
        )
      )
      
      # Update datasets dropdown
      shiny.fluent::updateDropdown.shinyInput(
        session, "project_dataset",
        options = convert_tibble_to_list(r$datasets, key_col = "id", text_col = "name"),
        value = project_wide$dataset_id
      )
      
      r$project_current_tab <- "summary"
      
      # Change selected tab
      sapply(all_divs, function(button_id) shinyjs::removeClass(class = "selected_pivot_item", selector = paste0("#", id, "-", button_id)))
      shinyjs::addClass(class = "selected_pivot_item", selector = paste0("#", id, "-summary"))
    })
    
    # Return to projects home page ----
    
    observeEvent(input$show_projects_home, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer input$show_projects_home"))
      
      divs <- c(paste0(all_divs, "_reduced_sidenav"), paste0(all_divs, "_large_sidenav"))
      
      sapply(c("one_project", divs), shinyjs::hide)
      sapply(c("all_projects", "all_projects_reduced_sidenav"), shinyjs::show)
      r$projects_show_hide_sidenav <- "hide"
    })
    
    # |-------------------------------- -----
    
    # --- --- --- --- --- -
    # Create a project ----
    # --- --- --- --- --- -
    
    # Open modal
    observeEvent(input$create_project, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer input$create_project"))
      
      shinyjs::show("create_project_modal")
    })
    
    # Close modal
    observeEvent(input$close_create_project_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$close_create_project_modal"))
      shinyjs::hide("create_project_modal")
    })
    
    # Add a tab
    observeEvent(input$add_project, {
      
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$add_project"))
      
      project_name <- input$project_creation_name

      # Check if name is not empty
      empty_name <- TRUE
      if (length(project_name) > 0) if (!is.na(project_name) & project_name != "") empty_name <- FALSE
      if (empty_name) shiny.fluent::updateTextField.shinyInput(session, "project_creation_name", errorMessage = i18n$t("provide_valid_name"))
      req(!empty_name)
      
      # Check if name is not already used
      sql <- glue::glue_sql("SELECT name FROM studies WHERE LOWER(name) = {tolower(project_name)}", .con = r$db)
      name_already_used <- nrow(DBI::dbGetQuery(r$db, sql) > 0)

      if (name_already_used) shiny.fluent::updateTextField.shinyInput(session, "project_creation_name", errorMessage = i18n$t("name_already_used"))
      req(!name_already_used)

      # Add project in db

      ## Project table
      rows <- list()
      rows$project_id <- get_last_row(r$db, "studies") + 1
      rows$patient_lvl_tab_group <- get_last_row(r$db, "tabs_groups") + 1
      rows$aggregated_tab_group <- get_last_row(r$db, "tabs_groups") + 2

      new_data <- tibble::tibble(
        id = rows$project_id, name = project_name, dataset_id = NA_integer_, patient_lvl_tab_group_id = rows$patient_lvl_tab_group, aggregated_tab_group_id = rows$aggregated_tab_group,
        creator_id = r$user_id, creation_datetime = now(), update_datetime = now(), deleted = FALSE
      )

      DBI::dbAppendTable(r$db, "studies", new_data)
      
      ## Options table
      sql <- glue::glue_sql("SELECT CONCAT(firstname, ' ', lastname) AS username FROM users WHERE id = {r$user_id}", .con = r$db)
      username <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()

      new_options <- tibble::tribble(
        ~name, ~value, ~value_num,
        "users_allowed_read_group", "people_picker", 1,
        "user_allowed_read", "", r$user_id,
        "version", "0.0.1.9000", NA_integer_,
        "unique_id", paste0(sample(c(0:9, letters[1:6]), 64, TRUE), collapse = ''), NA_integer_,
        "author", username, NA_integer_,
        "downloaded_from", "", NA_integer_,
        "downloaded_from_url", "", NA_integer_
      ) %>%
        dplyr::bind_rows(
          r$languages %>%
            tidyr::crossing(name = c("description", "category", "name")) %>%
            dplyr::mutate(
              name = paste0(name, "_", code),
              value = ifelse(grepl("name_", name), project_name, ""),
              value_num = NA_integer_
            ) %>%
            dplyr::select(-code, -language)
        ) %>%
        dplyr::mutate(id = get_last_row(r$db, "options") + dplyr::row_number(), category = "study", link_id = rows$project_id, .before = "name") %>%
        dplyr::mutate(creator_id = r$user_id, datetime = now(), deleted = FALSE)

      DBI::dbAppendTable(r$db, "options", new_options)

      # Reset fields
      shiny.fluent::updateTextField.shinyInput(session, "project_creation_name", value = "", errorMessage = NULL)
      
      # Reload projects widgets
      r$reload_projects_data <- now()
      
      # Notify user
      show_message_bar(output, message = "project_added", type = "success", i18n = i18n, ns = ns)
      
      # Close modal
      shinyjs::hide("create_project_modal")
    })
    
    # |-------------------------------- -----
    
    # --- --- --- --- --- -
    # Project datasets ----
    # --- --- --- --- --- -
    
    ## Save updates ----
    
    observeEvent(input$save_datasets, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer input$save_datasets"))
      
      sql <- glue::glue_sql("UPDATE studies SET dataset_id = {input$project_dataset} WHERE id = {r$selected_project_settings}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      r$projects_wide <- 
        r$projects_wide %>% 
        dplyr::mutate(dataset_id = dplyr::case_when(
          id == r$selected_project_settings ~ input$project_dataset,
          TRUE ~ dataset_id
        ))
      
      # Notify user
      show_message_bar(output,  "modif_saved", "success", i18n = i18n, ns = ns)
    })
    
    # |-------------------------------- -----
    
    # --- --- --- --- -- -
    # Project summary ----
    # --- --- --- --- -- -
    
    ## Delete a project ----
    
    observeEvent(input$delete_project, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer input$delete_project"))
      shinyjs::show("delete_project_modal")
    })
    
    observeEvent(input$close_project_deletion_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer input$close_project_deletion_modal"))
      shinyjs::hide("delete_project_modal")
    })
    
    observeEvent(input$confirm_project_deletion, {
      if (debug) cat(paste0("\n", now(), " - mod_projects - observer input$confirm_project_deletion"))
      
      project_id <- r$selected_project_settings
      
      # Delete project in db

      sql <- glue::glue_sql("DELETE FROM studies WHERE id = {project_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      # Delete options in db
      sql <- glue::glue_sql("DELETE FROM options WHERE category = 'study' AND link_id = {project_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      # Delete files
      # ...

      # Reload project list
      r$reload_projects_data <- now()
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_projects_home', Math.random());"))
      
      # Notify user
      show_message_bar(output, "project_deleted", "warning", i18n = i18n, ns = ns)
      
      # Close modal
      shinyjs::hide("delete_project_modal")
    })
  })
}