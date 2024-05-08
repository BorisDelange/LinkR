#' @noRd 
mod_datasets_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  
  pivot_item_js <- paste0("
    Shiny.setInputValue('", id, "-current_tab', this.id);
    Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());"
  )
  
  div(class = "main",
      
      # All datasets ----
      
      div(
        id = ns("all_datasets"),
        shiny.fluent::Breadcrumb(items = list(list(key = "main", text = i18n$t("datasets"))), maxDisplayedItems = 3),
        div(shiny.fluent::SearchBox.shinyInput(ns("search_dataset")), style = "width:320px; margin:10px 0 0 10px;"),
        uiOutput(ns("datasets"))
      ),
      
      # Create a dataset modal ----

      shinyjs::hidden(
        div(
          id = ns("create_dataset_modal"),
          div(
            div(
              tags$h1(i18n$t("create_dataset")),
              shiny.fluent::IconButton.shinyInput(ns("close_create_dataset_modal"), iconProps = list(iconName = "ChromeClose")),
              class = "create_element_modal_head small_close_button"
            ),
            div(
              make_textfield(i18n, ns, id = "dataset_creation_name", label = "name", width = "200px"),
              div(
                shiny.fluent::PrimaryButton.shinyInput(ns("add_dataset"), i18n$t("add")),
                class = "create_element_modal_buttons"
              ),
            ),
            class = "create_dataset_modal_content"
          ),
          class = "create_element_modal"
        )
      ),
      
      # Dataset details ----

      shinyjs::hidden(
        div(
          id = ns("one_dataset"),
          div(
            uiOutput(ns("dataset_breadcrumb")),
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
                  div(shiny.fluent::PrimaryButton.shinyInput(ns("delete_dataset"), i18n$t("delete")), class = "delete_button"),
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
      
      # Delete a dataset modal ----

      shinyjs::hidden(
        div(
          id = ns("delete_dataset_modal"),
          div(
            tags$h1(i18n$t("delete_dataset_title")), tags$p(i18n$t("delete_dataset_text")),
            div(
              shiny.fluent::DefaultButton.shinyInput(ns("close_dataset_deletion_modal"), i18n$t("dont_delete")),
              div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_dataset_deletion"), i18n$t("delete")), class = "delete_button"),
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
mod_datasets_server <- function(id, r, d, m, language, i18n, debug){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # |-------------------------------- -----
    
    if (debug) cat(paste0("\n", now(), " - mod_datasets - ", id, " - start"))
    
    # Pivot dataset divs
    all_divs <- c("summary", "edit_code", "share")
    
    # Search a dataset ----
    
    observeEvent(input$search_dataset, {
      if (debug) cat(paste0("\n", now(), " - mod_datasets - ", id, " - observer input$search_dataset"))
      
      if (input$search_dataset == "") r$filtered_datasets_long <- r$datasets_long
      else {
        filtered_ids <- r$datasets_long %>% dplyr::filter(name == paste0("name_", language) & grepl(tolower(input$search_dataset), tolower(value)))
        r$filtered_datasets_long <- r$datasets_long %>% dplyr::filter(id %in% filtered_ids)
      }
      
      r$reload_datasets_list <- now()
    })
    
    # Reload datasets widgets -----
    
    r$reload_datasets <- now()
    
    observeEvent(r$reload_datasets, {
      if (debug) cat(paste0("\n", now(), " - mod_datasets - observer r$reload_datasets"))
      
      sql <- glue::glue_sql("WITH selected_datasets AS (
        SELECT DISTINCT d.id
        FROM datasets d
        LEFT JOIN options AS r ON d.id = r.link_id AND r.category = 'dataset' AND r.name = 'users_allowed_read_group'
        LEFT JOIN options AS u ON d.id = u.link_id AND u.category = 'dataset' AND u.name = 'user_allowed_read'
        WHERE (r.value = 'everybody' OR (r.value = 'people_picker' AND u.value_num = {r$user_id}))
      )
      SELECT d.id, d.update_datetime, o.name, o.value, o.value_num
        FROM datasets d
        INNER JOIN selected_datasets ON d.id = selected_datasets.id
        LEFT JOIN options o ON o.category = 'dataset' AND d.id = o.link_id", .con = r$db)
      
      r$datasets_long <- DBI::dbGetQuery(r$db, sql)
      r$filtered_datasets_long <- r$datasets_long
      
      sql <- glue::glue_sql("SELECT id, CONCAT(firstname, ' ', lastname) AS name, CONCAT(SUBSTRING(firstname, 1, 1), SUBSTRING(lastname, 1, 1)) AS initials FROM users", .con = r$db)
      r$datasets_users <- DBI::dbGetQuery(r$db, sql)
      
      sql <- glue::glue_sql("SELECT * FROM datasets WHERE id IN ({unique(r$datasets_long$id)*})", .con = r$db)
      r$datasets_wide <- DBI::dbGetQuery(r$db, sql)
      
      r$reload_datasets_list <- now()
    })
    
    observeEvent(r$reload_datasets_list, {
      if (debug) cat(paste0("\n", now(), " - mod_datasets - observer r$reload_datasets_list"))
      
      # Filter datasets with search box

      datasets <- r$filtered_datasets_long

      datasets_ui <- tagList()

      for (i in unique(datasets$id)){
        row <- datasets %>% dplyr::filter(id == i)

        personas <- list()
        dataset_users <- row %>% dplyr::filter(name == "user_allowed_read") %>% dplyr::distinct(value_num) %>% dplyr::pull(value_num)
        for (j in dataset_users){
          user <- r$datasets_users %>% dplyr::filter(id == j)
          personas <- rlist::list.append(personas, list(personaName = user$name))
        }

        users_ui <- shiny.fluent::Facepile(personas = personas)

        dataset_name <- row %>% dplyr::filter(name == paste0("name_", language)) %>% dplyr::pull(value)

        max_length <- 45
        if (nchar(dataset_name) > max_length) dataset_name <- paste0(substr(dataset_name, 1, max_length - 3), "...")

        datasets_ui <- tagList(
          tags$a(
            href = shiny.router::route_link("datasets"),
            onClick = paste0("Shiny.setInputValue('", id, "-selected_dataset', ", i, ", {priority: 'event'});"),
            div(
              class = "dataset_widget",
              div(
                tags$h1(dataset_name),
                users_ui,
                div("Short description of my dataset")
              )
            ),
            class = "no-hover-effect"
          ),
          datasets_ui
        )
      }

      datasets_ui <- div(datasets_ui, class = "datasets_container")

      output$datasets <- renderUI(datasets_ui)

      # Unlock reactivity
      shinyjs::show("datasets")
    })
    
    # Dataset current tab ----
    
    observeEvent(input$current_tab_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_datasets - observer input$current_tab_trigger"))
      
      r$dataset_current_tab <- gsub(paste0(id, "-"), "", input$current_tab, fixed = FALSE)
    })
    
    observeEvent(r$dataset_current_tab, {
      if (debug) cat(paste0("\n", now(), " - mod_datasets - observer r$dataset_current_tab"))
      
      # Show or hide pages depending on selected tab
      divs <- setdiff(all_divs, r$dataset_current_tab)
      divs <- c(paste0(divs, "_reduced_sidenav"), paste0(divs, "_large_sidenav"), paste0(divs, "_div"))
      
      sapply(c(divs), shinyjs::hide)
      sapply(c(paste0(r$dataset_current_tab, "_div"), paste0(r$dataset_current_tab, "_reduced_sidenav"), paste0(r$dataset_current_tab, "_large_sidenav")), shinyjs::show)
      
      # Change selected tab
      sapply(all_divs, function(button_id) shinyjs::removeClass(class = "selected_pivot_item", selector = paste0("#", id, "-", button_id)))
      shinyjs::addClass(class = "selected_pivot_item", selector = paste0("#", id, "-", r$dataset_current_tab))
      
      r$datasets_show_hide_sidenav <- "hide"
    })
    
    # Return to datasets home page ----
    
    observeEvent(input$show_datasets_home, {
      if (debug) cat(paste0("\n", now(), " - mod_datasets - observer input$show_datasets_home"))
      
      divs <- c(paste0(all_divs, "_reduced_sidenav"), paste0(all_divs, "_large_sidenav"))
      
      sapply(c("one_dataset", divs), shinyjs::hide)
      sapply(c("all_datasets", "all_datasets_reduced_sidenav"), shinyjs::show)
      r$datasets_show_hide_sidenav <- "hide"
    })
    
    # |-------------------------------- -----
    
    # --- --- --- --- -- --
    # Create a dataset ----
    # --- --- --- --- -- --
    
    # Open modal
    observeEvent(input$create_dataset, {
      if (debug) cat(paste0("\n", now(), " - mod_datasets - observer input$create_dataset"))
      
      shinyjs::show("create_dataset_modal")
    })
    
    # Close modal
    observeEvent(input$close_create_dataset_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$close_create_dataset_modal"))
      shinyjs::hide("create_dataset_modal")
    })
    
    # Add a dataset
    observeEvent(input$add_dataset, {
      
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$add_dataset"))
      
      # req(length(input$dataset_creation_name) > 0)
      dataset_name <- input$dataset_creation_name

      # Check if name is not empty
      empty_name <- TRUE
      if (length(dataset_name) > 0) if (!is.na(dataset_name) & dataset_name != "") empty_name <- FALSE
      if (empty_name) shiny.fluent::updateTextField.shinyInput(session, "dataset_creation_name", errorMessage = i18n$t("provide_valid_name"))
      req(!empty_name)

      # Check if name is not already used
      sql <- glue::glue_sql("SELECT name FROM datasets WHERE LOWER(name) = {tolower(dataset_name)}", .con = r$db)
      name_already_used <- nrow(DBI::dbGetQuery(r$db, sql) > 0)

      if (name_already_used) shiny.fluent::updateTextField.shinyInput(session, "dataset_creation_name", errorMessage = i18n$t("name_already_used"))
      req(!name_already_used)

      # Add dataset in db

      ## Dataset table
      dataset_id <- get_last_row(r$db, "datasets") + 1

      new_data <- tibble::tibble(
        id = dataset_id, name = dataset_name, data_source_id = NA_integer_, creator_id = r$user_id, 
        creation_datetime = now(), update_datetime = now(), deleted = FALSE)

      DBI::dbAppendTable(r$db, "datasets", new_data)
      r$datasets <- r$datasets %>% dplyr::bind_rows(new_data)

      ## Options table
      sql <- glue::glue_sql("SELECT CONCAT(firstname, ' ', lastname) AS username FROM users WHERE id = {r$user_id}", .con = r$db)
      username <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()

      new_options <- tibble::tribble(
        ~name, ~value, ~value_num,
        "users_allowed_read_group", "everybody", 1,
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
              value = ifelse(grepl("name_", name), dataset_name, ""),
              value_num = NA_integer_
            ) %>%
            dplyr::select(-code, -language)
        ) %>%
        dplyr::mutate(id = get_last_row(r$db, "options") + dplyr::row_number(), category = "dataset", link_id = dataset_id, .before = "name") %>%
        dplyr::mutate(creator_id = r$user_id, datetime = now(), deleted = FALSE)
      
      DBI::dbAppendTable(r$db, "options", new_options)

      ## Code table
      new_code <- tibble::tibble(
        id = get_last_row(r$db, "code") + 1, category = "dataset", link_id = dataset_id, code = "",
        creator_id = r$user_id, datetime = now(), deleted = FALSE
      )

      DBI::dbAppendTable(r$db, "code", new_code)

      # Reset fields
      shiny.fluent::updateTextField.shinyInput(session, "dataset_creation_name", value = "", errorMessage = NULL)
      
      # Update datasets widgets
      r$reload_datasets <- now()
      
      # Notify user
      show_message_bar(output, message = "dataset_added", type = "success", i18n = i18n, ns = ns)
      
      # Close modal
      shinyjs::hide("create_dataset_modal")
    })
    
    # |-------------------------------- -----
    
    # A dataset is selected ----
    
    observeEvent(input$selected_dataset, {
      if (debug) cat(paste0("\n", now(), " - mod_datasets - observer input$selected_dataset"))
      
      r$selected_dataset <- input$selected_dataset
      
      sapply(c("all_datasets", "all_datasets_reduced_sidenav"), shinyjs::hide)
      shinyjs::show("one_dataset")
      
      dataset_wide <- r$datasets_wide %>% dplyr::filter(id == input$selected_dataset)
      dataset_long <- r$datasets_long %>% dplyr::filter(id == input$selected_dataset)
      
      r$selected_dataset_name <- dataset_wide$name
      
      output$dataset_breadcrumb <- renderUI(
        shiny.fluent::Breadcrumb(items = list(
          list(key = "main", text = i18n$t("datasets"), href = shiny.router::route_link("datasets"), 
               onClick = htmlwidgets::JS(paste0("item => { Shiny.setInputValue('", id, "-show_datasets_home', Math.random()); }"))),
          list(key = "main", text = r$selected_dataset_name))
        )
      )
      
      r$dataset_current_tab <- "summary"
      
      # Hide all editors
      sapply(paste0("edit_code_editor_div_", r$edit_dataset_code_editors$id), shinyjs::hide)
      
      # Load dataset code files
      r$reload_dataset_code_files <- now()
      
      # Change selected tab
      sapply(all_divs, function(button_id) shinyjs::removeClass(class = "selected_pivot_item", selector = paste0("#", id, "-", button_id)))
      shinyjs::addClass(class = "selected_pivot_item", selector = paste0("#", id, "-summary"))
    })
    
    # |-------------------------------- -----
    
    # --- --- --- --- - -
    # Dataset summary ----
    # --- --- --- --- - -
    
    ## Delete a dataset ----
    
    observeEvent(input$delete_dataset, {
      if (debug) cat(paste0("\n", now(), " - mod_datasets - observer input$delete_dataset"))
      shinyjs::show("delete_dataset_modal")
    })
    
    observeEvent(input$close_dataset_deletion_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_datasets - observer input$close_dataset_deletion_modal"))
      shinyjs::hide("delete_dataset_modal")
    })
    
    observeEvent(input$confirm_dataset_deletion, {
      if (debug) cat(paste0("\n", now(), " - mod_datasets - observer input$confirm_dataset_deletion"))
      
      dataset_id <- r$selected_dataset
      
      # Delete dataset in db
      
      sql <- glue::glue_sql("DELETE FROM datasets WHERE id = {dataset_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      # Delete code in db
      sql <- glue::glue_sql("DELETE FROM code WHERE category = 'dataset' AND link_id = {dataset_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      # Delete options in db
      sql <- glue::glue_sql("DELETE FROM options WHERE category = 'dataset' AND link_id = {dataset_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      # Delete files
      # ...
      
      # Reload datasets list
      r$reload_datasets <- now()
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_datasets_home', Math.random());"))
      
      # Notify user
      show_message_bar(output, "dataset_deleted", "warning", i18n = i18n, ns = ns)
      
      # Close modal
      shinyjs::hide("delete_dataset_modal")
    })
  })
}