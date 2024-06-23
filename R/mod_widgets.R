#' @noRd 
mod_widgets_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  
  # Initiate vars ----
  
  single_id <- switch(id, 
    "data_cleaning" = "data_cleaning", 
    "datasets" = "dataset",
    "projects" = "project", 
    "plugins" = "plugin", 
    "subsets" = "subset", 
    "vocabularies" = "vocabulary"
  )
  
  add_element_inputs <- tagList()
  
  if (id == "plugins") add_element_inputs <- make_dropdown(
    i18n, ns, id = "plugin_creation_type", label = "data_type",
    options = list(
      list(key = 1, text = i18n$t("patient_lvl_data")),
      list(key = 2, text = i18n$t("aggregated_data"))
    ),
    value = 1,
    width = "200px"
  )
  
  tagList(
      
      # All elements ----
      
      div(
        id = ns("all_elements"),
        shiny.fluent::Breadcrumb(items = list(list(key = "main", text = i18n$t(id))), maxDisplayedItems = 3),
        div(shiny.fluent::SearchBox.shinyInput(ns("search_element")), style = "width:320px; margin:10px 0 0 10px;"),
        uiOutput(ns("elements")),
        div(style = "display:none;", fileInput(ns("import_element_upload"), label = "", multiple = FALSE, accept = ".zip"))
      ),

      # Create an element modal ----

      shinyjs::hidden(
        div(
          id = ns("create_element_modal"),
          div(
            div(
              tags$h1(i18n$t(paste0("create_", single_id))),
              shiny.fluent::IconButton.shinyInput(ns("close_create_element_modal"), iconProps = list(iconName = "ChromeClose")),
              class = "create_element_modal_head small_close_button"
            ),
            div(
              make_textfield(i18n, ns, id = "element_creation_name", label = "name", width = "200px"),
              add_element_inputs,
              div(
                shiny.fluent::PrimaryButton.shinyInput(ns("add_element"), i18n$t("add")),
                class = "create_element_modal_buttons"
              ),
            ),
            class = paste0("create_", single_id, "_modal_content")
          ),
          class = "create_element_modal"
        )
      ),

      # Delete an element modal ----

      shinyjs::hidden(
        div(
          id = ns("delete_element_modal"),
          div(
            tags$h1(i18n$t(paste0("delete_", single_id, "_title"))), tags$p(i18n$t(paste0("delete_", single_id, "_text"))),
            div(
              shiny.fluent::DefaultButton.shinyInput(ns("close_element_deletion_modal"), i18n$t("dont_delete")),
              div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_element_deletion"), i18n$t("delete")), class = "delete_button"),
              class = "delete_modal_buttons"
            ),
            class = "delete_modal_content"
          ),
          class = "delete_modal"
        )
      ),
      
      # Confirm import element modal ----
      
      shinyjs::hidden(
        div(
          id = ns("import_element_modal"),
          div(
            tags$h1(i18n$t(paste0("import_", single_id, "_title"))), tags$p(i18n$t(paste0("import_", single_id, "_text"))),
            div(
              shiny.fluent::DefaultButton.shinyInput(ns("close_element_import_modal"), i18n$t("dont_import")),
              div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_element_import"), i18n$t("import")), class = "delete_button"),
              class = "import_modal_buttons"
            ),
            class = "import_modal_content"
          ),
          class = "import_modal"
        )
      ),
  )
}

#' @noRd 
mod_widgets_server <- function(id, r, d, m, language, i18n, all_divs, debug){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # |-------------------------------- -----
    
    if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - start"))
    
    # Unlock reactivity / show or hide divs ----
    observeEvent(shiny.router::get_page(), {
      req(shiny.router::get_page() == id)
      if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - observer shiny.router::get_page()"))
      
      divs <- c(paste0(all_divs, "_reduced_sidenav"), paste0(all_divs, "_large_sidenav"))
      sapply(c("one_element", divs), shinyjs::hide)
      
      sapply(c("all_elements", "all_elements_reduced_sidenav"), shinyjs::show)
      
      # Prevent a bug with scroll into ace editor
      shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
    })
    
    # Initiate vars ----
    
    single_id <- switch(id, 
      "data_cleaning" = "data_cleaning",
      "datasets" = "dataset",
      "projects" = "project",
      "plugins" = "plugin",
      "subsets" = "subset",
      "vocabularies" = "vocabulary"
    )
    
    sql_table <- switch(id, 
      "data_cleaning" = "scripts",
      "datasets" = "datasets",
      "projects" = "studies",
      "plugins" = "plugins",
      "subsets" = "subsets",
      "vocabularies" = "vocabulary"
    )
    
    # For retro-compatibility : studies -> projects
    sql_category <- switch(id, 
      "data_cleaning" = "script",
      "datasets" = "dataset",
      "projects" = "study",
      "plugins" = "plugin",
      "subsets" = "subset",
      "vocabularies" = "vocabulary"
    )
    
    long_var <- paste0(id, "_long")
    long_var_filtered <- paste0("filtered_", id, "_long")
    wide_var <- paste0(id, "_wide")
    element_added <- paste0(single_id, "_added")
    element_deleted <- paste0(single_id, "_deleted")
    
    # Sql connection
    if (id %in% c("subsets", "vocabularies")) con <- m$db else con <- r$db
    
    if (id == "plugins"){
      r$edit_plugin_code_files_list <- tibble::tibble(id = integer(), plugin_id = integer(), filename = character())
      r$edit_plugin_code_editors <- tibble::tibble(id = integer(), plugin_id = integer(), filename = character())
    }
    
    # Search an element ----
    
    observeEvent(input$search_element, {
      if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - observer input$search_element"))
      
      if (input$search_element == "") r[[long_var_filtered]] <- r[[long_var]]
      else {
        
        # Filter on name or description
        
        filtered_ids <- r[[long_var]] %>% 
          dplyr::filter(
            (name == paste0("name_", language) & grepl(tolower(input$search_element), tolower(value))) |
            (name == paste0("short_description_", language) & grepl(tolower(input$search_element), tolower(value)))
          ) %>%
          dplyr::pull(id)
        
        r[[long_var_filtered]] <- r[[long_var]] %>% dplyr::filter(id %in% filtered_ids)
      }
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_elements_list', Math.random());"))
    })
    
    # Reload widgets -----
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_elements_var', Math.random());"))
    
    observeEvent(input$reload_elements_var, {
      if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - observer input$reload_elements_var"))
      
      reload_elements_var(page_id = id, con = con, r = r, m = m, long_var_filtered = paste0("filtered_", id, "_long"))
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_elements_list', Math.random());"))
    })
    
    observeEvent(input$reload_elements_list, {
      if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - observer input$reload_elements_list"))
      
      elements_ui <- create_elements_ui(page_id = id, elements = r[[long_var_filtered]], r = r, language = language, i18n = i18n)

      output$elements <- renderUI(elements_ui)

      # Unlock reactivity
      shinyjs::show("elements")
    })
    
    # Element current tab ----
    
    observeEvent(input$current_tab_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - observer input$current_tab_trigger"))
      
      current_tab <- gsub(paste0(id, "-"), "", input$current_tab, fixed = FALSE)
      
      # Show or hide pages depending on selected tab
      divs <- setdiff(all_divs, current_tab)
      divs <- c(paste0(divs, "_reduced_sidenav"), paste0(divs, "_large_sidenav"), paste0(divs, "_div"))
      
      sapply(divs, shinyjs::hide)
      sapply(c(paste0(current_tab, "_div"), paste0(current_tab, "_reduced_sidenav"), paste0(current_tab, "_large_sidenav")), shinyjs::show)
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_hide_sidenav', 'hide');"))
      
      # Change selected tab
      sapply(all_divs, function(button_id) shinyjs::removeClass(class = "selected_pivot_item", selector = paste0("#", id, "-", button_id)))
      shinyjs::addClass(class = "selected_pivot_item", selector = paste0("#", id, "-", current_tab))
      
      if (id == "plugins"){
        if (current_tab == "edit_code"){
          # Show "show/hide sidenav button"
          shinyjs::show("show_hide_sidenav")
          
          # Expand sidenav
          shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_hide_sidenav', 'show');"))
          
          # Debug files browser UI
          shinyjs::hide("edit_code_files_browser")
          shinyjs::delay(50, shinyjs::show("edit_code_files_browser"))
        }
        else {
          # Hide "show/hide sidenav button"
          shinyjs::hide("show_hide_sidenav")
          
          shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_hide_sidenav', 'hide');"))
        }
      }
    })
    
    # Go to home page ----
    
    observeEvent(input$show_home, {
      if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - observer input$show_home"))
      
      divs <- c(paste0(all_divs, "_reduced_sidenav"), paste0(all_divs, "_large_sidenav"))
      
      sapply(c("one_element", divs), shinyjs::hide)
      sapply(c("all_elements", "all_elements_reduced_sidenav"), shinyjs::show)
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_hide_sidenav', 'hide');"))
      
      if (id == "projects"){
        
        # Change header
        shinyjs::show("command_bar_1_div")
        sapply(c("command_bar_2_a", "command_bar_2_div"), shinyjs::hide)
      }
    })
    
    # |-------------------------------- -----
    
    # --- --- --- --- --- --
    # Create an element ----
    # --- --- --- --- --- --
    
    # Open modal
    observeEvent(input$create_element, {
      if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - observer input$create_element"))
      shinyjs::show("create_element_modal")
    })
    
    # Close modal
    observeEvent(input$close_create_element_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - observer input$close_create_element_modal"))
      shinyjs::hide("create_element_modal")
    })
    
    # Add an element
    observeEvent(input$add_element, {
      
      if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - observer input$add_element"))
      
      element_name <- input$element_creation_name

      # Check if name is not empty
      empty_name <- TRUE
      if (length(element_name) > 0) if (!is.na(element_name) & element_name != "") empty_name <- FALSE
      if (empty_name) shiny.fluent::updateTextField.shinyInput(session, "element_creation_name", errorMessage = i18n$t("provide_valid_name"))
      req(!empty_name)

      # Check if name is not already used
      if (sql_table == "subsets") sql <- glue::glue_sql("SELECT name FROM subsets WHERE LOWER(name) = {tolower(element_name)} AND study_id = {m$selected_study}", .con = con)
      else if (sql_table == "vocabulary") sql <- glue::glue_sql("SELECT vocabulary_id FROM vocabulary WHERE LOWER(vocabulary_id) = {tolower(element_name)}", .con = con)
      else sql <- glue::glue_sql("SELECT name FROM {sql_table} WHERE LOWER(name) = {tolower(element_name)}", .con = con)
      name_already_used <- nrow(DBI::dbGetQuery(con, sql) > 0)

      if (name_already_used) shiny.fluent::updateTextField.shinyInput(session, "element_creation_name", errorMessage = i18n$t("name_already_used"))
      req(!name_already_used)

      # Add element in db

      ## Element table
      element_id <- get_last_row(con, sql_table) + 1
      
      if (sql_table == "datasets") new_data <- tibble::tibble(
        id = element_id, name = element_name, data_source_id = NA_integer_, creator_id = r$user_id, 
        creation_datetime = now(), update_datetime = now(), deleted = FALSE)
      
      else if (sql_table == "studies"){
        patient_lvl_tab_group_id <- get_last_row(con, "tabs_groups") + 1
        aggregated_tab_group_id <- get_last_row(con, "tabs_groups") + 2
        
        new_data <- tibble::tibble(
          id = element_id, name = element_name, dataset_id = NA_integer_, 
          patient_lvl_tab_group_id = patient_lvl_tab_group_id, aggregated_tab_group_id = aggregated_tab_group_id,
          creator_id = r$user_id, creation_datetime = now(), update_datetime = now(), deleted = FALSE)
        
        ## Tabs groups tables
        new_tabs_groups <- 
          tibble::tibble(
            id = c(patient_lvl_tab_group_id, aggregated_tab_group_id), category = c("patient_lvl", "aggregated"),
            name = element_name, description = "", creator_id = r$user_id, datetime = now(), deleted = FALSE)
        
        DBI::dbAppendTable(con, "tabs_groups", new_tabs_groups)
      }

      else if (sql_table == "plugins") new_data <- tibble::tibble(
        id = element_id, name = element_name, tab_type_id = input$plugin_creation_type,
        creation_datetime = now(), update_datetime = now(), deleted = FALSE)
      
      else if (sql_table == "subsets") new_data <- tibble::tibble(
        id = element_id, name = element_name, description = NA_character_, study_id = m$selected_study,
        creator_id = r$user_id, datetime = now(), deleted = FALSE)
      
      else if (sql_table == "vocabulary"){
        # Add a concept for this vocabulary in concept table
        vocabulary_concept_id <-
          DBI::dbGetQuery(con, glue::glue_sql("SELECT concept_id FROM concept WHERE concept_name = {element_name} AND domain_id = 'Metadata' AND concept_class_id = 'Vocabulary'", .con = con)) %>%
          dplyr::pull()
          
        if (length(vocabulary_concept_id) == 0){
          vocabulary_concept_id <- 
            DBI::dbGetQuery(con, glue::glue_sql("SELECT MAX(concept_id) FROM concept", .con = con)) %>% 
            dplyr::pull() %>%
            as.integer() + 1
          
          new_concept <- tibble::tibble(
            id = get_last_row(con, "concept") + 1, concept_id = vocabulary_concept_id, concept_name = element_name, domain_id = "Metadata",
            vocabulary_id = "Vocabulary", concept_class_id = "Vocabulary", standard_concept = NA_character_, concept_code = NA_character_,
            valid_start_date = now(), valid_end_date = lubridate::ymd("2099-12-31"), invalid_reason = NA_character_
          )
          DBI::dbAppendTable(con, "concept", new_concept)
        }
        
        new_data <- tibble::tibble(
          id = element_id, vocabulary_id = element_name, vocabulary_name = element_name, vocabulary_reference = NA_character_,
          vocabulary_version = NA_character_, vocabulary_concept_id = vocabulary_concept_id, data_source_id = NA_character_,
          display_order = NA_integer_, creator_id = r$user_id, creation_datetime = now(), update_datetime = now(), deleted = FALSE)
      }
      
      else if (sql_table == "scripts") new_data <- tibble::tibble(id = element_id, name = element_name, creation_datetime = now(), update_datetime = now(), deleted = FALSE)
      
      DBI::dbAppendTable(con, sql_table, new_data)
      r[[id]] <- r[[id]] %>% dplyr::bind_rows(new_data)

      ## Options table
      username <- r$users %>% dplyr::filter(id == r$user_id) %>% dplyr::pull(name)
      
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
              value = ifelse(grepl("name_", name), element_name, ""),
              value_num = NA_integer_
            ) %>%
            dplyr::select(-code, -language)
        ) %>%
        dplyr::mutate(id = get_last_row(con, "options") + dplyr::row_number(), category = sql_category, link_id = element_id, .before = "name") %>%
        dplyr::mutate(creator_id = r$user_id, datetime = now(), deleted = FALSE)
      
      ### For plugins table, add a row for each file
      if (id == "plugins"){
        new_options_ids <- max(new_options$id) + 1:3

        new_options <- new_options %>%
          dplyr::bind_rows(
            tibble::tibble(
              id = new_options_ids, category = "plugin_code", link_id = element_id, name = "filename",
              value = c("ui.R", "server.R", "translations.csv"), value_num = NA_real_, creator_id = r$user_id, datetime = now(), deleted = FALSE
            )
          )
      }
      
      ### For datasets table, add a row for omop version
      if (id == "datasets"){
        new_options <- new_options %>%
          dplyr::bind_rows(
            tibble::tibble(
              id = max(new_options$id) + 1, category = "dataset", link_id = element_id, name = "omop_version",
              value = "5.3", value_num = NA_real_, creator_id = r$user_id, datetime = now(), deleted = FALSE
            )
          )
      }
      
      DBI::dbAppendTable(con, "options", new_options)

      ## Code table
      
      ### For plugins table, add 3 files (ui.R, server.R, translations.csv)
      if (id == "plugins") new_code <- tibble::tibble(
        id = get_last_row(con, "code") + 1:3, category = "plugin", link_id = new_options_ids, code = "",
        creator_id = r$user_id, datetime = now(), deleted = FALSE)
      
      else {
        
        code <- ""
        if (id == "subsets") code <- paste0(
          "add_patients_to_subset(\n",
          "    patients = d$person %>% dplyr::pull(person_id),\n",
          "    subset_id = %subset_id%,\n",
          "    output = output, r = r, m = m, i18n = i18n, ns = ns\n",
          ")"
        )
        
        new_code <- tibble::tibble(
          id = get_last_row(con, "code") + 1, category = sql_category, link_id = element_id, code = code,
          creator_id = r$user_id, datetime = now(), deleted = FALSE)
      }

      DBI::dbAppendTable(con, "code", new_code)

      # Reset fields
      shiny.fluent::updateTextField.shinyInput(session, "element_creation_name", value = "", errorMessage = NULL)
      
      # Update elements widgets
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_elements_var', Math.random());"))
      
      # Notify user
      show_message_bar(output, message = element_added, type = "success", i18n = i18n, ns = ns)
      
      # Close modal
      shinyjs::hide("create_element_modal")
    })
    
    # |-------------------------------- -----
    
    # --- --- --- --- --- --
    # Upload an element ----
    # --- --- --- --- --- --
    
    ## Upload file ----
    
    observeEvent(input$import_element, {
      if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - observer input$import_element_browse"))
      shinyjs::click("import_element_upload")
    })
    
    observeEvent(input$import_element_upload, {
      if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - observer input$import_plugins_button"))
    
      tryCatch({
      
        # Extract ZIP file

        r$imported_element_temp_dir <- paste0(r$app_folder, "/temp_files/", r$user_id, "/", id, "/", now() %>% stringr::str_replace_all(":| |-", ""), paste0(sample(c(0:9, letters[1:6]), 24, TRUE), collapse = ''))
        zip::unzip(input$import_element_upload$datapath, exdir = r$imported_element_temp_dir)

        # Read XML file

        r$imported_element <-
          xml2::read_xml(paste0(r$imported_element_temp_dir, "/", single_id, ".xml")) %>%
          XML::xmlParse() %>%
          XML::xmlToDataFrame(nodes = XML::getNodeSet(., paste0("//", single_id)), stringsAsFactors = FALSE) %>%
          tibble::as_tibble()
        
        # Check if this element already exists in our database
        # If this element already exists, confirm its deletion
        
        if (r[[long_var]] %>% dplyr::filter(name == "unique_id" & value == r$imported_element$unique_id) %>% nrow() > 0) shinyjs::show("import_element_modal")
        else shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-confirm_element_import', Math.random());"))
        
      }, error = function(e) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - import element error - ", toString(e))))
    })
    
    observeEvent(input$close_element_import_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - observer input$close_element_import"))
      shinyjs::hide("import_element_modal")
    })
    
    ## Import files ----
    
    observeEvent(input$confirm_element_import, {
      if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - observer input$confirm_element_import"))
      
      shinyjs::hide("import_element_modal")
      
      tryCatch({
        
        # Delete old files and copy new files
        
        element_folder <- paste0(r$app_folder, "/", id, "/", r$imported_element$unique_id)
        unlink(element_folder, recursive = TRUE)
        dir.create(element_folder)
        
        files_list <- list.files(r$imported_element_temp_dir)
        
        file.copy(paste0(r$imported_element_temp_dir, "/", files_list), paste0(element_folder, "/", files_list))
        
        # Delete old entries in database
        
        element_id <- DBI::dbGetQuery(r$db, glue::glue_sql("SELECT link_id FROM options WHERE category = {single_id} AND name = 'unique_id' AND value = {r$imported_element$unique_id}", .con = con)) %>% dplyr::pull()
        
        if (length(element_id) > 0){
          
          # Delete element in db
          sql_send_statement(con, glue::glue_sql("DELETE FROM {sql_table} WHERE id = {element_id}", .con = con))
          
          # For plugins, get options id to delete corresponding code rows
          if (id == "plugins"){
            options_ids <- DBI::dbGetQuery(r$db, glue::glue_sql("SELECT id FROM options WHERE category = 'plugin_code' AND link_id = {element_id}", .con = con)) %>% dplyr::pull()
            
            # Delete code in db
            sql_send_statement(con, glue::glue_sql("DELETE FROM code WHERE link_id IN ({options_ids*})", .con = con))
            sql_send_statement(con, glue::glue_sql("DELETE FROM options WHERE category = 'plugin_code' AND link_id = {element_id}", .con = con))
          }
          
          # Delete code in db
          sql_send_statement(con, glue::glue_sql("DELETE FROM code WHERE category = {sql_category} AND link_id = {element_id}", .con = con))
          
          # Delete options in db
          sql_send_statement(con, glue::glue_sql("DELETE FROM options WHERE category = {sql_category} AND link_id = {element_id}", .con = con))
        }
        
        # Import data in database
        
        # Create columns if don't exist (one column for option and for language)
        prefixes <- c("description", "name", "category")
        new_cols <- outer(prefixes, r$languages$code, paste, sep = "_") %>% as.vector()
        for(col in new_cols) if(!col %in% colnames(r$imported_element)) r$imported_element <- r$imported_element %>% dplyr::mutate(!!col := "")
        
        r$imported_element <- r$imported_element %>% dplyr::mutate(name = get(paste0("name_", language)))
        
        # Element table
        
        if (id == "plugins") new_data <-
          r$imported_element %>%
          dplyr::transmute(id = get_last_row(con, "plugins") + 1, name, tab_type_id = type, creation_datetime, update_datetime, deleted = FALSE)
        
        DBI::dbAppendTable(con, sql_table, new_data)
        
        # Options table
        new_options <- tibble::tribble(
          ~name, ~value, ~value_num,
          "users_allowed_read_group", "everybody", 1,
          "user_allowed_read", "", r$user_id,
          "version", r$imported_element$version, NA_real_,
          "unique_id", r$imported_element$unique_id, NA_real_,
          "author", r$imported_element$author, NA_real_,
          "downloaded_from", "", NA_real_,
          "downloaded_from_url", "", NA_real_
        ) %>%
          dplyr::bind_rows(
            r$imported_element %>%
              dplyr::select(dplyr::starts_with(c("category", "name", "description")), -name) %>%
              tidyr::pivot_longer(dplyr::starts_with(c("category", "name", "description")), names_to = "name", values_to = "value") %>%
              dplyr::mutate(value_num = NA_real_)
          ) %>%
          dplyr::mutate(id = get_last_row(con, "options") + dplyr::row_number(), category = sql_category, link_id = new_data$id, .before = "name") %>%
          dplyr::mutate(creator_id = r$user_id, datetime = now(), deleted = FALSE)
        
        DBI::dbAppendTable(con, "options", new_options)
        
        # Code table
        
        # For plugins, add code in database with create_plugin_files
        if (id == "plugins") create_plugin_files(id = id, r = r, plugin_id = new_data$id)
        else {
          new_code <- tibble::tibble(
            id = get_last_row(r$db, "code") + 1, category = "dataset", link_id = new_data$id,
            code = "", creator_id = r$user_id, datetime = now(), deleted = FALSE
          )
          DBI::dbAppendTable(con, "code", new_code)
        }
        
        # Reload elements var and widgets
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_elements_var', Math.random());"))
       
        show_message_bar(output, paste0("success_importing_", single_id), "success", i18n = i18n, ns = ns)
        
      }, error = function(e) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - import element error - ", toString(e))))
    })
    
    # |-------------------------------- -----
    
    # --- --- --- --- --- --- ---
    # An element is selected ----
    # --- --- --- --- --- --- ---
    
    observeEvent(input$selected_element_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - observer input$selected_element_trigger"))
      
      sapply(c("all_elements", "all_elements_reduced_sidenav"), shinyjs::hide)
      shinyjs::show("one_element")
      
      element_wide <- r[[wide_var]] %>% dplyr::filter(id == input$selected_element)
      element_long <- r[[long_var]] %>% dplyr::filter(id == input$selected_element)
      
      output$breadcrumb <- renderUI({
        
        if (id == "vocabularies") element_name <- element_wide$vocabulary_id
        else element_name <- element_wide$name
        
        shiny.fluent::Breadcrumb(items = list(
          list(key = "main", text = i18n$t(id), href = shiny.router::route_link(id), 
             onClick = htmlwidgets::JS(paste0("item => { Shiny.setInputValue('", id, "-show_home', Math.random()); }"))),
          list(key = "main", text = element_name))
        )
      })
      
      shinyjs::runjs(paste0("
        Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());
        Shiny.setInputValue('", id, "-current_tab', 'summary');"))
      
      # Change selected tab
      sapply(all_divs, function(button_id) shinyjs::removeClass(class = "selected_pivot_item", selector = paste0("#", id, "-", button_id)))
      shinyjs::addClass(class = "selected_pivot_item", selector = paste0("#", id, "-summary"))
      
      # Update git repos dropdown
      shiny.fluent::updateDropdown.shinyInput(session, "git_repo", options = convert_tibble_to_list(r$git_repos, key_col = "id", text_col = "name"), value = NULL)
      
      # Update git repos UI
      output$git_repo_element_ui <- renderUI(div())
      output$synchronize_git_buttons <- renderUI(div())
      
      # Load plugin code files and store selected plugin infos
      if (id == "plugins") {
        
        if (element_wide$tab_type_id == 1) selected_plugin_type <- "patient_lvl" else selected_plugin_type <- "aggregated"
        selected_plugin_unique_id <- element_long %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value)
        
        shinyjs::runjs(paste0("
          Shiny.setInputValue('", id, "-selected_plugin_type', '", selected_plugin_type, "');
          Shiny.setInputValue('", id, "-selected_plugin_unique_id', '", selected_plugin_unique_id, "');
          Shiny.setInputValue('", id, "-reload_code_files', Math.random());
        "))
        
        # Hide all editors
        sapply(paste0("edit_code_editor_div_", r$edit_plugin_code_editors$id), shinyjs::hide)
        
        # Load plugin code files
        r$reload_plugin_code_files <- now()
      }
      
      # Load project
      else if (id == "projects"){
        
        # Change header
        shinyjs::hide("command_bar_1_div")
        sapply(c("command_bar_2_a", "command_bar_2_div"), shinyjs::show)
        
        # Change current project name
        m$selected_study <- input$selected_element
        
        # Update selected dataset dropdown
        shiny.fluent::updateDropdown.shinyInput(
          session, "project_dataset",
          options = convert_tibble_to_list(r$datasets_wide, key_col = "id", text_col = "name"),
          value = element_wide$dataset_id
        )
        
        # We can choose to load only project settings, and loda data after
        r$project_data_loaded <- FALSE
        
        # Load data page if not already loaded
        # If not already loaded, project is loaded after data page server side is loaded
        # Else, project is loaded directly
        # Delay to change page before executing server
        
        if (input$selected_element_type != "project_options"){
          if (length(r$loaded_pages$data) == 0){
            r$load_page <- "data"
            r$data_page <- "patient_lvl"
          }
          else r$load_project_trigger <- now()
        }
      }
      
      # Load dataset
      else if (id == "datasets"){
        
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-load_dataset_code', Math.random());"))
      }
      
      # Load subset
      else if (id == "subsets"){
        
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-load_subset_code', Math.random());"))
      }
      
      else if (id == "vocabularies"){
        
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-load_vocabulary_code', Math.random());"))
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-load_vocabulary_tables', Math.random());"))
      }
    })
    
    # |-------------------------------- -----
    
    # --- --- --- --- -- -
    # Element summary ----
    # --- --- --- --- -- -
    
    ## Delete an element ----
    
    observeEvent(input$delete_element, {
      if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - observer input$delete_element"))
      shinyjs::show("delete_element_modal")
    })
    
    observeEvent(input$close_element_deletion_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - observer input$close_element_deletion"))
      shinyjs::hide("delete_element_modal")
    })
    
    observeEvent(input$confirm_element_deletion, {
      if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - observer input$confirm_element_deletion"))
      
      element_id <- input$selected_element
      
      # Delete element in db
      sql_send_statement(con, glue::glue_sql("DELETE FROM {sql_table} WHERE id = {element_id}", .con = con))
      
      # For plugins, get options id to delete corresponding code rows
      if (id == "plugins"){
        options_ids <- DBI::dbGetQuery(r$db, glue::glue_sql("SELECT id FROM options WHERE category = 'plugin_code' AND link_id = {element_id}", .con = con)) %>% dplyr::pull()
        
        # Delete code in db
        sql_send_statement(con, glue::glue_sql("DELETE FROM code WHERE link_id IN ({options_ids*})", .con = con))
        sql_send_statement(con, glue::glue_sql("DELETE FROM options WHERE category = 'plugin_code' AND link_id = {element_id}", .con = con))
      }
      
      # Delete code in db
      sql_send_statement(con, glue::glue_sql("DELETE FROM code WHERE category = {sql_category} AND link_id = {element_id}", .con = con))
      
      # Delete options in db
      sql_send_statement(con, glue::glue_sql("DELETE FROM options WHERE category = {sql_category} AND link_id = {element_id}", .con = con))
        
      # Delete files
      if (id == "plugins") unlink(input$selected_plugin_folder, recursive = TRUE)
      
      # If it is a vocabulary, delete entry in concept table
      if (id == "vocabularies"){
        
        # Get vocabulary_id
        vocabulary_id <- r$vocabularies_wide %>% dplyr::filter(id == element_id) %>% dplyr::pull(vocabulary_id)
        
        # Delete entry in concept table
        sql_send_statement(con, glue::glue_sql("DELETE FROM concept WHERE concept_name = {vocabulary_id} AND domain_id = 'Metadata' AND concept_class_id = 'Vocabulary'", .con = con))
      }
      
      # Reload widgets
      shinyjs::runjs(paste0("
        Shiny.setInputValue('", id, "-reload_elements_var', Math.random());
        Shiny.setInputValue('", id, "-show_home', Math.random());"))
      
      # Notify user
      show_message_bar(output, element_deleted, "warning", i18n = i18n, ns = ns)
      
      # Close modal
      shinyjs::hide("delete_element_modal")
    })
    
    # |-------------------------------- -----
    
    # --- --- --- --- --
    # Share element ----
    # --- --- --- --- --
    
    ## Git synchronization ----
    
    # Load a git repo
    observeEvent(input$git_repo, {
      if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - observer input$git_repo"))
      
      git_repo <- r$git_repos %>% dplyr::filter(id == input$git_repo)
      
      element_long <- r[[long_var]] %>% dplyr::filter(id == input$selected_element)
      element_ui <- div(
        shiny.fluent::MessageBar(i18n$t("error_downloading_git_repo"), messageBarType = 5), 
        style = "display: inline-block;"
      )
      synchronize_git_buttons <- tagList()
      
      # Clone git repo if not already loaded
      
      local_path <- ""
      tryCatch({
        local_path <- load_git_repo(id, r, git_repo)
        element_ui <- tagList()
      }, error = function(e) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - error downloading git repo - error = ", toString(e))))
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-git_repo_local_path', '", local_path, "');"))
      
      if (local_path != ""){
        
        # Check if this element already exists
        element_path <- paste0(local_path, "/", id, "/", element_long %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
        element_exists <- dir.exists(element_path)
        
        # The element exists in the git repo
        if (element_exists){
          
          # Get element infos from XML file
          xml_file_path <- paste0(element_path, "/", single_id, ".xml")
          
          error_loading_xml_file <- TRUE
          
          tryCatch({
          element <-
            xml2::read_xml(xml_file_path) %>%
            XML::xmlParse() %>%
            XML::xmlToDataFrame(nodes = XML::getNodeSet(., paste0("//", single_id)), stringsAsFactors = FALSE) %>%
            tibble::as_tibble()
          
          error_loading_xml_file <- FALSE
          }, error = function(e) cat(paste0("\n", now(), " - mod_git_repos - error downloading ", current_tab, " readme - error = ", toString(e))))
          
          if (error_loading_xml_file) element_ui <- div(
            shiny.fluent::MessageBar(i18n$t("error_loading_category_xml_file"), messageBarType = 5), 
            style = "display: inline-block;"
          )
          
          else {
            
            diff_time_unit <- "hours"
            diff_time <- difftime(now() %>% lubridate::ymd_hms(), element$update_datetime, unit = diff_time_unit) %>% as.integer()
            
            if (diff_time > 24){
              diff_time_unit <- "days"
              diff_time <- difftime(now() %>% lubridate::ymd_hms(), element$update_datetime, unit = diff_time_unit) %>% as.integer()
            }
            
            element_ui <- div(
              tags$table(
                tags$tr(tags$td(strong(i18n$t("name")), style = "min-width: 80px;"), tags$td(element[[paste0("name_", language)]])),
                tags$tr(tags$td(strong(i18n$t("author_s"))), tags$td(element$author)),
                tags$tr(tags$td(strong(i18n$t("created_on"))), tags$td(element$creation_datetime)),
                tags$tr(
                  tags$td(strong(i18n$t("updated_on"))),
                  tags$td(element$update_datetime, " -", strong(diff_time, " ", tolower(i18n$t(diff_time_unit))), " ", tolower(i18n$t("updated_x_ago")))
                )
              )
            )
            
            # Update synchronize buttons
            synchronize_git_buttons <- shiny.fluent::PrimaryButton.shinyInput(ns("update_git_repo"), i18n$t("update"))
          }
        }
        
        # The element doesn't exist
        else {
          element_ui <- div(
            shiny.fluent::MessageBar(i18n$t(paste0(single_id, "_doesnt_exist_in_git_repo")), messageBarType = 5), 
            style = "display: inline-block;"
          )
        }
      }
      
      output$git_repo_element_ui <- renderUI(element_ui)
      output$synchronize_git_buttons <- renderUI(synchronize_git_buttons)
    })
    
    ## Export element ----
    
    observeEvent(input$export_element, {
      if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - observer input$export_element"))

      shinyjs::click("export_element_download")
    })
    
    output$export_element_download <- downloadHandler(

      filename = function(){
        element_name <-
          r[[wide_var]] %>% 
          dplyr::filter(id == input$selected_element) %>%
          dplyr::pull(name) %>%
          tolower() %>%
          gsub(" ", "_", .) %>%
          gsub("[àáâãäå]", "a", .) %>%
          gsub("[èéêë]", "e", .) %>%
          gsub("[ìíîï]", "i", .) %>%
          gsub("[òóôõö]", "o", .) %>%
          gsub("[ùúûü]", "u", .) %>%
          gsub("ç", "c", .) %>%
          gsub("ñ", "n", .) %>%
          gsub("ÿ", "y", .) %>%
          gsub("ß", "ss", .) %>%
          gsub("[^a-z0-9_]", "", .)
        
        paste0("linkr_", single_id, "_", element_name, "_", now() %>% stringr::str_replace_all(" ", "_") %>% stringr::str_replace_all(":", "_") %>% as.character(), ".zip")
      },

      content = function(file){
        if (debug) cat(paste0("\n", now(), " - mod_widgets - (", id, ") - output$export_elements_download"))

        # Element dir
        sql <- glue::glue_sql("SELECT value FROM options WHERE category = {single_id} AND name = 'unique_id' AND link_id = {input$selected_element}", .con = con)
        element_unique_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
        element_dir <- paste0(r$app_folder, "/", id, "/", element_unique_id)
        
        # Create files if don't exist
        if (id == "plugins") create_plugin_files(id = id, r = r, plugin_id = input$selected_element)
        
        # Get element options
        sql <- glue::glue_sql("SELECT * FROM options WHERE category = {single_id} AND link_id = {input$selected_element}", .con = con)
        element_options <- DBI::dbGetQuery(r$db, sql)
        
        # Create XML file
        xml <- XML::newXMLDoc()
        
        elements_node <- XML::newXMLNode(id, doc = xml)
        element_node <- XML::newXMLNode(single_id, parent = elements_node)
        
        XML::newXMLNode("app_version", r$app_version, parent = element_node)
        
        for(name in c("unique_id", "version", "author", paste0("name_", r$languages$code), paste0("category_", r$languages$code))){
          XML::newXMLNode(name, element_options %>% dplyr::filter(name == !!name) %>% dplyr::pull(value), parent = element_node)
        }
        for(name in c(paste0("description_", r$languages$code))) XML::newXMLNode(name, element_options %>% dplyr::filter(name == !!name) %>%
          dplyr::pull(value) %>% stringr::str_replace_all("''", "'"), parent = element_node)
        
        # Specific nodes depending on current page
        if (id == "plugins"){
          plugin <- r$plugins_wide %>% dplyr::filter(id == input$selected_element)
          
          XML::newXMLNode("type", plugin$tab_type_id, parent = element_node)
          for (name in c("creation_datetime", "update_datetime")) XML::newXMLNode(name, plugin %>% dplyr::pull(get(!!name)), parent = element_node)
        }
        
        # Create XML file
        XML::saveXML(xml, file = paste0(element_dir, paste0("/", single_id, ".xml")))
        
        # Create a ZIP
        zip::zipr(file, list.files(element_dir, full.names = TRUE))
      }
    )
  })
}