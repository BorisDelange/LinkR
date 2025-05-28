#' @noRd 
mod_widgets_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  
  # Initiate vars ----
  
  single_id <- switch(
    id, 
    "data_cleaning" = "data_cleaning", 
    "datasets" = "dataset",
    "projects" = "project", 
    "plugins" = "plugin", 
    "subsets" = "subset", 
    "vocabularies" = "vocabulary"
  )
  
  tagList(
      
      # All elements ----
      
      div(
        id = ns("all_elements"),
        shiny.fluent::Breadcrumb(items = list(list(key = "main", text = i18n$t(id))), maxDisplayedItems = 3),
        div(
          id = ns("search_element_div"),
          shiny.fluent::SearchBox.shinyInput(ns("search_element")), style = "width:280px; margin:10px 0 15px 10px;"
        ),
        div(uiOutput(ns("elements")), style = "margin: 10px 0 0 10px;"),
        div(style = "display:none;", fileInput(ns("import_element_upload"), label = "", multiple = FALSE, accept = ".zip")),
        style = "padding: 0 10px;"
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
      
      # Import element modal ----
      
      shinyjs::hidden(
        div(
          id = ns("import_element_modal"),
          div(
            tags$h1(i18n$t(paste0("import_", single_id, "_title"))), tags$p(i18n$t(paste0("import_", single_id, "_text"))),
            div(
              shiny.fluent::DefaultButton.shinyInput(ns("close_element_import_modal"), i18n$t("dont_import")),
              div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_element_import_1"), i18n$t("import"))),
              class = "import_modal_buttons"
            ),
            class = "import_modal_content"
          ),
          class = "import_modal"
        )
      ),
      
      # Update or delete git element modal ----
      
      shinyjs::hidden(
        div(
          id = ns("update_or_delete_git_element_modal"),
          div(
            div(
              id = ns("update_git_element_text_div"),
              tags$h1(i18n$t(paste0("update_git_element_", single_id, "_title"))), 
              tags$p(i18n$t(paste0("update_git_element_", single_id, "_text")))
            ),
            shinyjs::hidden(
              div(
                id = ns("delete_git_element_text_div"),
                tags$h1(i18n$t(paste0("delete_git_element_", single_id, "_title"))), 
                tags$p(i18n$t(paste0("delete_git_element_", single_id, "_text")))
              )
            ),
            shiny.fluent::TextField.shinyInput(ns("update_or_delete_git_element_api_key"), type = "password", canRevealPassword = TRUE, label = i18n$t("api_key")),
            shiny.fluent::TextField.shinyInput(ns("update_or_delete_git_element_commit_message"), label = i18n$t("commit_message")),
            div(
              shiny.fluent::DefaultButton.shinyInput(ns("close_update_or_delete_git_element_modal"), i18n$t("cancel")),
              div(
                id = ns("confirm_git_element_update_div"),
                shiny.fluent::PrimaryButton.shinyInput(ns("confirm_git_element_update"), i18n$t("update"))
              ),
              shinyjs::hidden(
                div(
                  id = ns("confirm_git_element_deletion_div"),
                  shiny.fluent::PrimaryButton.shinyInput(ns("confirm_git_element_deletion"), i18n$t("delete")), 
                  class = "delete_button"
                )
              ),
              class = "update_or_delete_git_element_modal_buttons"
            ),
            class = "update_or_delete_git_element_modal_content"
          ),
          class = "update_or_delete_git_element_modal"
        )
      )
  )
}

#' @noRd 
mod_widgets_server <- function(id, r, d, m, language, i18n, all_divs, debug, user_accesses, user_settings){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # |-------------------------------- -----
    
    # Page change observer ----
    
    observeEvent(shiny.router::get_page(), try_catch("shiny.router::get_page()", {
      
      if (shiny.router::get_page() == id) shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_elements_var', Math.random());"))
    }))
    
    # Initiate vars ----
    
    single_id <- switch(
      id, 
      "data_cleaning" = "data_cleaning",
      "datasets" = "dataset",
      "projects" = "project",
      "plugins" = "plugin",
      "subsets" = "subset",
      "vocabularies" = "vocabulary"
    )
    
    sql_table <- switch(
      id, 
      "data_cleaning" = "scripts",
      "datasets" = "datasets",
      "projects" = "studies",
      "plugins" = "plugins",
      "subsets" = "subsets",
      "vocabularies" = "vocabulary"
    )
    
    # For retro-compatibility : studies -> projects
    sql_category <- switch(
      id, 
      "data_cleaning" = "data_cleaning",
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
    
    # Apply user settings ----
    
    if (id %in% c("data_cleaning", "datasets", "subsets")){
      shinyAce::updateAceEditor(session, paste0(single_id, "_code"), theme = user_settings$ace_theme, fontSize = user_settings$ace_font_size)
      
      text_output_theme <- gsub("_", "-", user_settings$ace_theme)
      if (text_output_theme == "terminal") text_output_theme <- paste0(text_output_theme, "-theme")
      shinyjs::addClass("code_result_div", paste0("ace-", text_output_theme))
    }
    
    # Search an element ----
    
    observeEvent(input$search_element, try_catch("input$search_element", {
      
      if (length(r[[long_var]]) == 0) return()
        
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
    }))
    
    # Reload widgets -----
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_elements_var', Math.random());"))
    
    observeEvent(input$reload_elements_var, try_catch("input$reload_elements_var", {
      
      reload_elements_var(page_id = id, id = id, con = con, long_var_filtered = paste0("filtered_", id, "_long"))
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_elements_list', Math.random());"))
    }))
    
    observeEvent(input$reload_elements_list, try_catch("input$reload_elements_list", {
      
      if (id %in% user_accesses | (id == "subsets" & "projects_subsets_management" %in% user_accesses)){
        
        if (length(input$selected_element) > 0) selected_element <- input$selected_element
        else selected_element <- NA_integer_
        
        elements_ui <- create_elements_ui(page_id = id, id = id, elements = r[[long_var_filtered]], selected_element = selected_element, r = r, language = language, i18n = i18n)
        
        output$elements <- renderUI(elements_ui)
        
        # For plugins page, reload copy plugin dropdown
        if (id == "plugins") shiny.fluent::updateDropdown.shinyInput(session, "plugin_to_copy", options = convert_tibble_to_list(r$plugins_wide %>% dplyr::arrange(name), key_col = "id", text_col = "name"))
      }
      else {
        shinyjs::hide("search_element_div")
        output$elements <- renderUI(div(shiny.fluent::MessageBar(i18n$t("unauthorized_access_area"), messageBarType = 5), style = "display: inline-block; margin: 5px;"))
      }
        
      # Unlock reactivity
      shinyjs::show("elements")
    }))
    
    # Element current tab ----
    
    observeEvent(input$current_tab_trigger, try_catch("input$current_tab_trigger", {
      
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
      
      # Prevent a bug with scroll into ace editor
      if (current_tab == "edit_code") shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
    }))
    
    # Go to home page ----
    
    observeEvent(input$show_home, try_catch("input$show_home", {
      
      divs <- c(paste0(all_divs, "_reduced_sidenav"), paste0(all_divs, "_large_sidenav"))
      
      sapply(c("show_hide_sidenav", "one_element", divs), shinyjs::hide)
      sapply(c("all_elements", "all_elements_reduced_sidenav"), shinyjs::show)
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_hide_sidenav', 'hide');"))
      
      if (id == "projects"){
        
        # Change header
        sapply(c("command_bar_2_link", "command_bar_2_div"), shinyjs::hide)
      }
    }))
    
    # |-------------------------------- -----
    
    # --- --- --- --- --- --
    # Create an element ----
    # --- --- --- --- --- --
    
    # Open / close modal
    
    observeEvent(input$create_element, try_catch("input$create_element", shinyjs::show("create_element_modal")))
    observeEvent(input$close_create_element_modal, try_catch("input$close_create_element_modal", shinyjs::hide("create_element_modal")))
    
    # Add an element
    
    observeEvent(input$add_element, try_catch("input$add_element", {
      
      if (!(paste0(id, "_management") %in% user_accesses || (id == "subsets" && "projects_subsets_management" %in% user_accesses))) return()
      
      element_name <- input$element_creation_name
      username <- r$users %>% dplyr::filter(id == r$user_id) %>% dplyr::pull(name)

      # Check if name is not empty
      empty_name <- TRUE
      if (length(element_name) > 0) if (!is.na(element_name) & element_name != "") empty_name <- FALSE
      if (empty_name){
        shiny.fluent::updateTextField.shinyInput(session, "element_creation_name", errorMessage = i18n$t("provide_valid_name"))
        return()
      }

      # Check if name is not already used
      if (sql_table == "subsets") sql <- glue::glue_sql("SELECT name FROM subsets WHERE study_id = {m$selected_study}", .con = con)
      else if (sql_table == "vocabulary") sql <- glue::glue_sql("SELECT vocabulary_id FROM vocabulary", .con = con)
      else sql <- glue::glue_sql("SELECT name FROM {`sql_table`}", .con = con)
      
      elements_names <- DBI::dbGetQuery(con, sql) %>% dplyr::pull()
      name_already_used <- remove_special_chars(element_name) %in% remove_special_chars(elements_names)

      if (name_already_used){
        shiny.fluent::updateTextField.shinyInput(session, "element_creation_name", errorMessage = i18n$t("name_already_used"))
        return()
      }
      
      # For plugins page
      if (sql_table == "plugins"){
        
        # Check if plugin_creation_type is empty
        if (length(input$plugin_creation_type) == 0){
          shiny.fluent::updateDropdown.shinyInput(session, "plugin_creation_type", errorMessage = i18n$t("field_cant_be_empty"))
          return()
        }
        
        # Check if plugin_to_copy is empty, if plugin_copy_existing_plugin toggle is activated
        
        plugin_to_copy_options <- convert_tibble_to_list(r$plugins_wide, key_col = "id", text_col = "name")
        
        plugin_to_copy_check <- TRUE
        copy_existing_plugin <- FALSE
        
        if (length(input$plugin_copy_existing_plugin) > 0){
          if (input$plugin_copy_existing_plugin){
            if (length(input$plugin_to_copy) == 0){
              plugin_to_copy_check <- FALSE
              shiny.fluent::updateDropdown.shinyInput(session, "plugin_to_copy", options = plugin_to_copy_options, errorMessage = i18n$t("field_cant_be_empty"))
            }
            else copy_existing_plugin <- TRUE
          }
        }
        
        if (!plugin_to_copy_check) return()
        
      }

      # Add element in db

      ## Element table
      element_id <- get_last_row(con, sql_table) + 1
      
      if (sql_table == "datasets") new_data <- tibble::tibble(
        id = element_id, name = element_name, data_source_id = NA_integer_, creator_id = r$user_id, 
        creation_datetime = now(), update_datetime = now(), deleted = FALSE)
      
      else if (sql_table == "studies"){
        
        patient_lvl_tab_group_id <- get_last_row(con, "tabs_groups") + 1
        aggregated_tab_group_id <- get_last_row(con, "tabs_groups") + 2
        
        dataset_id <- NA_integer_
        if (length(input$element_creation_dataset) > 0) dataset_id <- input$element_creation_dataset
        
        new_data <- tibble::tibble(
          id = element_id, name = element_name, dataset_id = dataset_id, 
          patient_lvl_tab_group_id = patient_lvl_tab_group_id, aggregated_tab_group_id = aggregated_tab_group_id,
          creator_id = r$user_id, creation_datetime = now(), update_datetime = now(), deleted = FALSE)
        
        ## Tabs groups tables
        new_tabs_groups <- 
          tibble::tibble(
            id = c(patient_lvl_tab_group_id, aggregated_tab_group_id), category = c("patient_lvl", "aggregated"),
            name = element_name, description = "", creator_id = r$user_id, datetime = now(), deleted = FALSE)
        
        DBI::dbAppendTable(con, "tabs_groups", new_tabs_groups)
        
        ## Add a default subset
        subset_id <- get_last_row(m$db, "subsets") + 1
        new_subset <- tibble::tibble(
          id = subset_id, name = i18n$t("subset_all_patients"), description = NA_character_, study_id = element_id,
          creator_id = r$user_id, datetime = now(), deleted = FALSE)
        DBI::dbAppendTable(m$db, "subsets", new_subset)
      }

      else if (sql_table == "plugins"){
        plugin_creation_type <- as.integer(paste(input$plugin_creation_type, collapse = ""))
        new_data <- tibble::tibble(
          id = element_id, name = element_name, tab_type_id = plugin_creation_type,
          creation_datetime = now(), update_datetime = now(), deleted = FALSE
        )
      }
      
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
      
      new_options <- create_options_tibble(
        element_id = element_id, element_name = element_name, sql_category = sql_category, username = username, 
        last_options_id = get_last_row(con, "options"))
      
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
      
      ### For a new study, add a default subset
      else if (id == "projects"){
        new_subset_options <- create_options_tibble(
          element_id = subset_id, element_name = i18n$t("subset_all_patients"), sql_category = "subset", username = username, 
          last_options_id = get_last_row(m$db, "options"))
        DBI::dbAppendTable(m$db, "options", new_subset_options)
      }
      
      DBI::dbAppendTable(con, "options", new_options)
      
      new_element_unique_id <- new_options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value)
      new_element_dir <- paste0(r$app_folder, "/", id, "/", new_element_unique_id)
      
      # Copy existing plugin
      if (id == "plugins" && copy_existing_plugin){
        
        if (!dir.exists(new_element_dir)) dir.create(new_element_dir)
        
        sql <- glue::glue_sql("SELECT value FROM options WHERE category = 'plugin' AND name = 'unique_id' AND link_id = {input$plugin_to_copy}", .con = r$db)
        plugin_to_copy_unique_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
        plugin_to_copy_dir <- paste0(r$app_folder, "/", id, "/", plugin_to_copy_unique_id)
        
        files_to_copy <- list.files(plugin_to_copy_dir, full.names = TRUE)
        files_to_copy <- files_to_copy[grepl("\\.r$|\\.py$|translations\\.csv$", files_to_copy, ignore.case = TRUE)]
        
        for (file in files_to_copy){
          file_name <- basename(file)
          file.copy(file, paste0(new_element_dir, "/", file_name), overwrite = TRUE)
        }
      }
      else create_element_scripts(id = id, element_dir = new_element_dir)
      
      # For a new study, create default subset code
      if (id == "projects"){
        new_subset_unique_id <- new_subset_options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value)
        new_subset_dir <- paste0(r$app_folder, "/subsets/", new_subset_unique_id)
        create_element_scripts(id = "subsets", element_dir = new_subset_dir)
      }
      
      # Reset fields
      shiny.fluent::updateTextField.shinyInput(session, "element_creation_name", value = "", errorMessage = NULL)
      
      if (id == "plugins"){
        shiny.fluent::updateDropdown.shinyInput(session, "plugin_creation_type", errorMessage = NULL, value = 1)
        shiny.fluent::updateDropdown.shinyInput(session, "plugin_to_copy", options = plugin_to_copy_options, errorMessage = i18n$t("field_cant_be_empty"))
        shiny.fluent::updateToggle.shinyInput(session, "plugin_copy_existing_plugin", value = FALSE)
      }
      else if (id == "projects") shiny.fluent::updateDropdown.shinyInput(session, "element_creation_dataset", options = convert_tibble_to_list(r$datasets_wide, key_col = "id", text_col = "name"), value = NULL)
      
      # Update elements widgets
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_elements_var', Math.random());"))
      
      # Notify user
      show_message_bar(id, output, message = element_added, type = "success", i18n = i18n, ns = ns)
      
      # Close modal
      shinyjs::hide("create_element_modal")
    }))
    
    # |-------------------------------- -----
    
    # --- --- --- --- --- --
    # Upload an element ----
    # --- --- --- --- --- --
    
    ## Upload file ----
    
    observeEvent(input$import_element, try_catch("input$import_element", shinyjs::click("import_element_upload")))
    
    observeEvent(input$import_element_upload, try_catch("input$import_element_upload", {
    
      if (paste0(id, "_import") %not_in% user_accesses) return()
        
      # Extract ZIP file

      r$imported_element_temp_dir <- paste0(r$app_folder, "/temp_files/", r$user_id, "/", id, "/", now() %>% stringr::str_replace_all(":| |-", ""), paste0(sample(c(0:9, letters[1:6]), 24, TRUE), collapse = ''))
      zip::unzip(input$import_element_upload$datapath, exdir = r$imported_element_temp_dir)
      r$imported_element_temp_dir <- file.path(r$imported_element_temp_dir, list.files(r$imported_element_temp_dir))
      
      # Case 1: import from LinkR download
      top_level_items <- list.files(r$imported_element_temp_dir, full.names = TRUE)
      
      # Case 2: import from git repo download. Only select the first element.
      if (id %in% basename(top_level_items)) {
        
        element_dir <- file.path(r$imported_element_temp_dir, id)
        elements_list <- list.dirs(element_dir, recursive = FALSE, full.names = TRUE)
        
        if (length(elements_list) > 0) r$imported_element_temp_dir <- elements_list[1]
      }

      # Read XML file
      
      r$imported_element <-
        xml2::read_xml(paste0(r$imported_element_temp_dir, "/", single_id, ".xml")) %>%
        XML::xmlParse() %>%
        XML::xmlToDataFrame(nodes = XML::getNodeSet(., paste0("//", single_id)), stringsAsFactors = FALSE) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(authors = stringr::str_replace_all(authors, "(?<=\\p{L})(?=\\p{Lu})", "; "))
      
      # Check if this element already exists in our database
      # If this element already exists, confirm its deletion
      
      if (r[[long_var]] %>% dplyr::filter(name == "unique_id" & value == r$imported_element$unique_id) %>% nrow() > 0) shinyjs::show("import_element_modal")
      else {
        if (id == "projects") shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-ask_plugins_update', Math.random());"))
        else shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-confirm_element_import_2', Math.random());"))
      }
    }))
    
    observeEvent(input$close_element_import_modal, try_catch("input$close_element_import_modal", {
      shinyjs::hide("import_element_modal")
    }))
    
    ## Import files ----
    
    observeEvent(input$confirm_element_import_1, try_catch("input$confirm_element_import_1", {
      
      shinyjs::hide("import_element_modal")
      
      if (id == "projects") shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-ask_plugins_update', Math.random());"))
      else shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-confirm_element_import_2', Math.random());"))
    }))
      
    observeEvent(input$confirm_element_import_2, try_catch("input$confirm_element_import_2", {
      
      tryCatch({
        
        import_element(
          con = con, sql_table = sql_table, sql_category = sql_category, single_id = single_id,
          element = r$imported_element, element_type = id, temp_dir = r$imported_element_temp_dir
        )
      }, error = function(e){
        show_message_bar(id, output, paste0("error_importing_", single_id), "warning", i18n = i18n, ns = ns)
        cat(paste0("\n", now(), " - mod_widgets - (", id, ") - import element error - ", toString(e)))
      })
    }))
    
    # |-------------------------------- -----
    
    # --- --- --- --- --- --- ---
    # An element is selected ----
    # --- --- --- --- --- --- ---
    
    observeEvent(input$selected_element_trigger, try_catch("input$selected_element_trigger", {
      
      sapply(c("all_elements", "all_elements_reduced_sidenav"), shinyjs::hide)
      shinyjs::show("one_element")
      
      element_wide <- r[[wide_var]] %>% dplyr::filter(id == input$selected_element)
      element_long <- r[[long_var]] %>% dplyr::filter(id == input$selected_element)
      
      ## Update breadcrumb ----
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
      
      # Reload informations UI ----
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_informations_ui', Math.random());"))
      
      # Change border of selected element
      sapply(r[[wide_var]]$id, function(i) shinyjs::removeClass(class = paste0("selected_", single_id, "_widget"), selector = paste0("#", id, "-", single_id, "_widget_", i)))
      sapply(r[[wide_var]]$id, function(i) shinyjs::removeClass(class = paste0("selected_", single_id, "_widget"), selector = paste0("#home-", single_id, "_widget_", i)))
      shinyjs::addClass(class = paste0("selected_", single_id, "_widget"), selector = paste0("#", id, "-", single_id, "_widget_", input$selected_element))
      shinyjs::addClass(class = paste0("selected_", single_id, "_widget"), selector = paste0("#home-", single_id, "_widget_", input$selected_element))
      
      # Update summary fields ----
      
      if (id != "vocabularies"){
        shiny.fluent::updateDropdown.shinyInput(session, "language", value = language)
        
        for (lang in r$languages$code){
          
          for (field in c("name", "short_description")) shiny.fluent::updateTextField.shinyInput(
            session, paste0(field, "_", lang), value = element_long %>% dplyr::filter(name == paste0(field, "_", lang)) %>% dplyr::pull(value))
        }
        
        shiny.fluent::updateTextField.shinyInput(session, "author", value = element_long %>% dplyr::filter(name == "author") %>% dplyr::pull(value))
        shiny.fluent::updateDropdown.shinyInput(session, "users_allowed_read_group", value = element_long %>% dplyr::filter(name == "users_allowed_read_group") %>% dplyr::pull(value))
      }
      
      if (id %not_in% c("vocabularies", "subsets")) shiny.fluent::updateTextField.shinyInput(session, "version", value = element_long %>% dplyr::filter(name == "version") %>% dplyr::pull(value))
      
      ## Description
      
      if (id != "vocabularies"){
        description_code <- element_long %>% dplyr::filter(name == paste0("description_", language)) %>% dplyr::pull(value)
        shinyAce::updateAceEditor(session, "description_code", value = description_code)
        
        if (description_code == "" | is.na(description_code)) output$description_ui <- renderUI(div(shiny.fluent::MessageBar(i18n$t("no_description_available"), messageBarType = 5) ,style = "display: inline-block;"))
        else {
          output_file <- create_rmarkdown_file(description_code, interpret_code = FALSE)
          output$description_ui <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(output_file))))
        }
      }
      
      ## Load users UI
      
      if (id %not_in% c("subsets", "vocabularies")){
        picker_options <- r$users %>% dplyr::select(key = id, imageInitials = initials, text = name, secondaryText = user_status)
        picker_value <- element_long %>% dplyr::filter(name == "user_allowed_read") %>% dplyr::pull(value_num) %>% unique()
        picker_value <- intersect(picker_options %>% dplyr::pull(key), picker_value)
        default_selected_items <- picker_options %>% dplyr::filter(key %in% picker_value)
        
        output$users_allowed_read_ui <- renderUI(
          div(
            shiny.fluent::NormalPeoplePicker.shinyInput(
              ns("users_allowed_read"),
              options = picker_options, value = picker_value, defaultSelectedItems = default_selected_items,
              pickerSuggestionsProps = list(
                suggestionsHeaderText = i18n$t("users"),
                noResultsFoundText = i18n$t("no_results_found"),
                showRemoveButtons = TRUE
              )
            ),
            style = "width: 400px; margin-top: 12px"
          )
        )
      }
      
      ## Tab type id
      if (id == "plugins"){
        tab_type_id <- element_wide$tab_type_id
        if(element_wide$tab_type_id %in% c(12, 21)) tab_type_id <- c(1, 2)
        shiny.fluent::updateDropdown.shinyInput(session, "tab_type_id", value = tab_type_id)
      }
      
      ## OMOP version
      if (id == "datasets") shiny.fluent::updateDropdown.shinyInput(
        session, "omop_version",
        value = element_long %>% dplyr::filter(name == "omop_version") %>% dplyr::pull(value)
      )
      
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
      
      # Load vocabulary fields
      if (id == "vocabularies"){
        shiny.fluent::updateTextField.shinyInput(session, "vocabulary_id", value = element_wide %>% dplyr::pull(vocabulary_id))
        shiny.fluent::updateTextField.shinyInput(session, "vocabulary_name", value = element_wide %>% dplyr::pull(vocabulary_name))
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-load_vocabulary_tables', Math.random());"))
      }
      
      # Load project ----
      else if (id == "projects"){
        
        # Change header
        sapply(c("command_bar_2_link", "command_bar_2_div"), shinyjs::show)
        
        # Check if we load another project, or if it is the same
        if (length(m$selected_study) > 0 && m$selected_study == input$selected_element) return()
        
        # Change current project name
        m$selected_study <- input$selected_element
        m$selected_project <- input$selected_element
        
        # Update selected dataset dropdown
        shiny.fluent::updateDropdown.shinyInput(
          session, "project_dataset",
          options = convert_tibble_to_list(r$datasets_wide, key_col = "id", text_col = "name"),
          value = element_wide$dataset_id
        )
        
        # Load data page if not already loaded
        if ("data" %not_in% names(r$loaded_pages)){
          r$load_page <- "data"
          r$data_page <- "patient_lvl"
        }
        else r$load_project_trigger <- now()
      }
      
      # Load dataset ----
      else if (id == "datasets"){
        
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-load_dataset_code', Math.random());"))
        
        # Clear code output
        output$code_result <- renderText("")
      }
      
      # Load subset ----
      else if (id == "subsets"){
        
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-load_subset_code', Math.random());"))
      }
      
      # Load data cleaning scripts ----
      else if (id == "data_cleaning"){
        
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-load_data_cleaning_code', Math.random());"))
      }
    }))
    
    # |-------------------------------- -----
    
    # --- --- --- --- -- -
    # Element summary ----
    # --- --- --- --- -- -
    
    ## Edit summary ----
    
    observeEvent(input$edit_summary, try_catch("input$edit_summary", {
      
      if (!(paste0(id, "_management") %in% user_accesses || (id == "subsets" && "projects_subsets_management" %in% user_accesses))) return()
      
      # Reload markdown
      element_long <- r[[long_var]] %>% dplyr::filter(id == input$selected_element)
      description_code <- element_long %>% dplyr::filter(name == paste0("description_", input$language)) %>% dplyr::pull(value)
      
      if (description_code == "" | is.na(description_code)) output$description_ui <- renderUI(div(shiny.fluent::MessageBar(i18n$t("no_description_available"), messageBarType = 5) ,style = "display: inline-block;"))
      else {
        output_file <- create_rmarkdown_file(description_code, interpret_code = FALSE)
        output$description_ui <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(output_file))))
      }
      
      sapply(c("summary_view_informations_div", "edit_summary_div"), shinyjs::hide)
      sapply(c("summary_edit_informations_div", "save_summary_div", "edit_description_button"), shinyjs::show)
    }))
    
    ## Reload informations UI ----
    
    observeEvent(input$reload_informations_ui, try_catch("input$reload_informations_ui", {
      
      element_wide <- r[[wide_var]] %>% dplyr::filter(id == input$selected_element)
      element_long <- r[[long_var]] %>% dplyr::filter(id == input$selected_element)
      
      if (id == "subsets"){
        output$summary_informations_ui <- renderUI(
          div(
            tags$table(
              tags$tr(tags$td(strong(i18n$t("name"))), tags$td(element_wide$name)),
              tags$tr(tags$td(strong(i18n$t("short_description")), style = "min-width: 150px;"), tags$td(element_long %>% dplyr::filter(name == paste0("short_description_", language)) %>% dplyr::pull(value)))
            )
          )
        )
      }
      else if (id == "vocabularies"){
        output$summary_informations_ui <- renderUI(
          div(
            tags$table(
              tags$tr(tags$td(strong(i18n$t("id"))), tags$td(element_wide$vocabulary_id)),
              tags$tr(tags$td(strong(i18n$t("name"))), tags$td(element_wide$vocabulary_name))
            )
          )
        )
      }
      else {
        specific_row <- tagList()
        if (id == "plugins"){
          if(element_wide$tab_type_id == 1) tab_type <- i18n$t("patient_lvl_data")
          else if (element_wide$tab_type_id == 2) tab_type <- i18n$t("aggregated_data")
          else if (element_wide$tab_type_id %in% c(12, 21)) tab_type <- i18n$t("patient_lvl_or_aggregated_data")
          specific_row <- tags$tr(tags$td(strong(i18n$t("plugin_for"))), tags$td(tab_type))
        }
        else if (id == "datasets") specific_row <- tags$tr(
          tags$td(strong(i18n$t("omop_version"))), 
          tags$td(element_long %>% dplyr::filter(name == "omop_version") %>% dplyr::pull(value))
        )
        
        output$summary_informations_ui <- renderUI(
          div(
            tags$table(
              tags$tr(tags$td(strong(i18n$t("name"))), tags$td(element_wide$name)),
              tags$tr(tags$td(strong(i18n$t("author_s"))), tags$td(element_long %>% dplyr::filter(name == "author") %>% dplyr::pull(value))),
              tags$tr(tags$td(strong(i18n$t("created_on"))), tags$td(format_datetime(element_wide$creation_datetime, language = language, sec = FALSE))),
              tags$tr(tags$td(strong(i18n$t("updated_on"))), tags$td(format_datetime(element_wide$update_datetime, language = language, sec = FALSE))),
              tags$tr(tags$td(strong(i18n$t("version"))), tags$td(element_long %>% dplyr::filter(name == "version") %>% dplyr::pull(value))),
              specific_row,
              tags$tr(tags$td(strong(i18n$t("short_description")), style = "min-width: 150px;"), tags$td(element_long %>% dplyr::filter(name == paste0("short_description_", language)) %>% dplyr::pull(value)))
            )
          )
        )
      }
      
    }))
    
    ## Show / hide users UI ----
    
    observeEvent(input$users_allowed_read_group, try_catch("input$users_allowed_read_group", {
      
      if (input$users_allowed_read_group == "people_picker") shinyjs::show("users_allowed_read_ui")
      else shinyjs::hide("users_allowed_read_ui")
    }))
    
    ## Change language ----
    
    observeEvent(input$language, try_catch("input$language", {
      
      if (length(input$selected_element) == 0) return()
        
      # Update name et short description fields
      sapply(r$languages$code, function(lang){
        if (lang != input$language) sapply(c("name", "short_description"), function(field) shinyjs::hide(paste0(field, "_", lang, "_div")))
      })
      
      # Update description editor
      element_long <- r[[long_var]] %>% dplyr::filter(id == input$selected_element)
      description_code <- element_long %>% dplyr::filter(name == paste0("description_", input$language)) %>% dplyr::pull(value)
      shinyAce::updateAceEditor(session, "description_code", value = description_code)
      
      # Reload markdown
      
      if (description_code == "" | is.na(description_code)) output$description_ui <- renderUI(div(shiny.fluent::MessageBar(i18n$t("no_description_available"), messageBarType = 5) ,style = "display: inline-block;"))
      else {
        output_file <- create_rmarkdown_file(description_code, interpret_code = FALSE)
        output$description_ui <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(output_file))))
      }
      
      sapply(c("name", "short_description"), function(field) shinyjs::show(paste0(field, "_", input$language, "_div")))
    }))
    
    ## Edit description ----
    
    observeEvent(input$edit_description, try_catch("input$edit_description", {
      
      sapply(c("edit_description_button", "summary_informations_div"), shinyjs::hide)
      sapply(c("save_and_cancel_description_buttons", "edit_description_div"), shinyjs::show)
    }))
    
    ## Run description code ----
    
    observeEvent(input$run_description_code, try_catch("input$run_description_code", {
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_description_code_trigger', Math.random());"))
    }))
    
    observeEvent(input$description_code_run_all, try_catch("input$description_code_run_all", {
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_description_code_trigger', Math.random());"))
    }))
    
    observeEvent(input$run_description_code_trigger, try_catch("input$run_description_code_trigger", {
      
      if (input$description_code == "" | is.na(input$description_code)) output$description_ui <- renderUI(div(shiny.fluent::MessageBar(i18n$t("no_description_available"), messageBarType = 5) ,style = "display: inline-block;"))
      else {
        output_file <- create_rmarkdown_file(input$description_code, interpret_code = FALSE)
        output$description_ui <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(output_file))))
      }
    }))
    
    ## Save description updates ----
    
    observeEvent(input$description_code_save, try_catch("input$description_code_save", {
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_description_trigger', Math.random());"))
    }))
    
    observeEvent(input$save_description, try_catch("input$save_description", {
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_description_trigger', Math.random());"))
      
      sapply(c("save_and_cancel_description_buttons", "edit_description_div"), shinyjs::hide)
      sapply(c("edit_description_button", "summary_informations_div"), shinyjs::show)
    }))
    
    observeEvent(input$save_description_trigger, try_catch("input$save_description_trigger", {
      
      # Update database and r var
      sql <- glue::glue_sql("UPDATE options SET value = {input$description_code} WHERE category = {sql_category} AND link_id = {input$selected_element} AND name = {paste0('description_', input$language)}", .con = r$db)
      sql_send_statement(r$db, sql)
      
      r[[long_var]] <- r[[long_var]] %>% dplyr::mutate(
        value = dplyr::case_when(
          id == input$selected_element & name == paste0('description_', input$language) ~ input$description_code,
          TRUE ~ value
        )
      )
      
      # Reload markdown
      if (input$description_code == "" | is.na(input$description_code)) output$description_ui <- renderUI(div(shiny.fluent::MessageBar(i18n$t("no_description_available"), messageBarType = 5) ,style = "display: inline-block;"))
      else {
        output_file <- create_rmarkdown_file(input$description_code, interpret_code = FALSE)
        output$description_ui <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(output_file))))
      }
      
      show_message_bar(id, output, "modif_saved", "success", i18n = i18n, ns = ns)
    }))
    
    ## Cancel description updates ----
    
    observeEvent(input$cancel_description, try_catch("input$cancel_description", {
      
      sapply(c("save_and_cancel_description_buttons", "edit_description_div"), shinyjs::hide)
      sapply(c("edit_description_button", "summary_informations_div"), shinyjs::show)
      
      # Reset description editor with last value
      element_long <- r[[long_var]] %>% dplyr::filter(id == input$selected_element) 
      description_code <- element_long %>% dplyr::filter(name == paste0("description_", input$language)) %>% dplyr::pull(value)
      shinyAce::updateAceEditor(session, "description_code", value = description_code)
      
      # Reload markdown UI
      if (description_code == "" | is.na(description_code)) output$description_ui <- renderUI(div(shiny.fluent::MessageBar(i18n$t("no_description_available"), messageBarType = 5) ,style = "display: inline-block;"))
      else {
        output_file <- create_rmarkdown_file(description_code, interpret_code = FALSE)
        output$description_ui <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(output_file))))
      }
    }))
    
    ## Save summary updates ----
    
    observeEvent(input$save_summary, try_catch("input$save_summary", {
      
      name_field <- paste0("name_", language)
      if (id == "vocabularies") element_name <- input$vocabulary_id
      else element_name <- input[[name_field]]
      
      # Check if name is not empty
      empty_name <- TRUE
      if (length(element_name) > 0) if (!is.na(element_name) & element_name != "") empty_name <- FALSE
      if (empty_name){
        shiny.fluent::updateTextField.shinyInput(session, name_field, errorMessage = i18n$t("provide_valid_name"))
        return()
      }
      
      # Check if name is not already used
      if (id == "subsets") sql <- glue::glue_sql("SELECT name FROM subsets WHERE LOWER(name) = {tolower(element_name)} AND id != {input$selected_element} AND study_id = {m$selected_study}", .con = con)
      else if (id == "vocabularies") sql <- glue::glue_sql("SELECT vocabulary_id FROM vocabulary WHERE LOWER(vocabulary_id) = {tolower(element_name)} AND id != {input$selected_element}", .con = con)
      else sql <- glue::glue_sql("SELECT name FROM {sql_table} WHERE LOWER(name) = {tolower(element_name)} AND id != {input$selected_element}", .con = con)
      name_already_used <- nrow(DBI::dbGetQuery(con, sql) > 0)
      
      if (name_already_used){
        shiny.fluent::updateTextField.shinyInput(session, name_field, errorMessage = i18n$t("name_already_used"))
        return()
      }
      
      # Check if tab_type_id is not empty
      if (id == "plugins"){
        if (length(input$tab_type_id) == 0){
          shiny.fluent::updateDropdown.shinyInput(session, "tab_type_id", errorMessage = i18n$t("field_cant_be_empty"))
          return()
        }
      }
      
      # Change update datetime
      if (id != "subsets"){
        sql <- glue::glue_sql("UPDATE {`sql_table`} SET update_datetime = {now()} WHERE id = {input$selected_element}", .con = con)
        sql_send_statement(con, sql)
      }
        
      if (id != "vocabularies"){
        # List existing fields : add fields that don't exist
        
        fields <- c(
          paste0("name_", r$languages$code),
          paste0("short_description_", r$languages$code),
          "author", "users_allowed_read_group"
        )
        
        # Version
        if (id != "subsets") fields <- c(fields, "version")
        
        # OMOP version field
        if (id == "datasets") fields <- c(fields, "omop_version")
        
        # Only names and description fields for subsets
        else if (id == "subsets") fields <- c(paste0("name_", r$languages$code), paste0("short_description_", r$languages$code))
        
        sql <- glue::glue_sql("SELECT DISTINCT(name) FROM options WHERE category = {sql_category} AND link_id = {input$selected_element}", .con = con)
        existing_fields <- DBI::dbGetQuery(con, sql) %>% dplyr::pull()
        new_fields <- setdiff(fields, existing_fields)
        options_last_row <- get_last_row(con, "options")
        
        if (length(new_fields) > 0){
          new_data <- tibble::tibble(
            id = options_last_row + (1:length(new_fields)), category = sql_category, link_id = input$selected_element,
            name = new_fields, value = "", value_num = NA_integer_, creator_id = r$user_id,
            datetime = now(), deleted = FALSE
          )
          options_last_row <- options_last_row + length(new_fields)
          DBI::dbAppendTable(con, "options", new_data)
        }
          
        # Update sql data
        
        cases <- sapply(fields, function(field) {
          value <- if (length(input[[field]]) > 0) gsub("'", "''", input[[field]]) else ""
          paste0("WHEN name = '", field, "' THEN '", value, "'")
        })
        
        case_statement <- paste(cases, collapse = " ")
        sql <- glue::glue_sql(paste0(
          "UPDATE options SET value = CASE {DBI::SQL(case_statement)} ELSE value END ",
          "WHERE name IN ({fields*}) AND category = {sql_category} AND link_id = {input$selected_element}"), .con = con)
        sql_send_statement(con, sql)
        
        sql <- glue::glue_sql("DELETE FROM options WHERE category = {sql_category} AND link_id = {input$selected_element} AND name = 'user_allowed_read'", .con = con)
        sql_send_statement(con, sql)
        
        # Add users allowed read (one row by user)
        
        if (length(input$users_allowed_read) > 0){
          
          value_num <- unique(input$users_allowed_read)
          
          new_data <- tibble::tibble(
            id = options_last_row + (1:length(value_num)), category = sql_category, link_id = input$selected_element,
            name = "user_allowed_read", value = "", value_num = value_num, creator_id = r$user_id,
            datetime = now(), deleted = FALSE
          )
          DBI::dbAppendTable(con, "options", new_data)
        }
      
        # Update name
        new_name <- gsub("'", "''", input[[paste0('name_', language)]])
        sql <- glue::glue_sql("UPDATE {`sql_table`} SET name = {new_name} WHERE id = {input$selected_element}", .con = con)
        sql_send_statement(con, sql)
        
        # Update tab_type_id
        if (id == "plugins"){
          tab_type_id <- as.integer(paste(input$tab_type_id, collapse = ""))
          sql <- glue::glue_sql("UPDATE {`sql_table`} SET tab_type_id = {tab_type_id} WHERE id = {input$selected_element}", .con = con)
          sql_send_statement(con, sql)
        }
      }
      
      # For vocabularies, update vocabulary_id and vocabulary_name
      else if (id == "vocabularies"){
        new_name <- element_name
        sql <- glue::glue_sql("UPDATE vocabulary SET vocabulary_id = {element_name}, vocabulary_name = {input$vocabulary_name} WHERE id = {input$selected_element}", .con = con)
        sql_send_statement(con, sql)
      }
      
      # Reload widgets
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_elements_var', Math.random());"))
      
      # Reload breadcrumb
      output$breadcrumb <- renderUI(shiny.fluent::Breadcrumb(items = list(
          list(key = "main", text = i18n$t(id), href = shiny.router::route_link(id), 
            onClick = htmlwidgets::JS(paste0("item => { Shiny.setInputValue('", id, "-show_home', Math.random()); }"))),
          list(key = "main", text = new_name))
        )
      )
      
      show_message_bar(id, output, "modif_saved", "success", i18n = i18n, ns = ns)
      
      # Reload informations UI
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_informations_ui', Math.random());"))
      
      # Hide edit description buttons
      sapply(c("save_and_cancel_description_buttons", "edit_description_div"), shinyjs::hide)
      sapply(c("edit_description_button", "summary_informations_div"), shinyjs::show)
      
      # Reload description depending on app language
      element_long <- r[[long_var]] %>% dplyr::filter(id == input$selected_element)
      description_code <- element_long %>% dplyr::filter(name == paste0("description_", language)) %>% dplyr::pull(value)
      
      if (description_code == "" | is.na(description_code)) output$description_ui <- renderUI(div(shiny.fluent::MessageBar(i18n$t("no_description_available"), messageBarType = 5) ,style = "display: inline-block;"))
      else {
        output_file <- create_rmarkdown_file(description_code, interpret_code = FALSE)
        output$description_ui <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(output_file))))
      }
      
      # Close edition mode
      sapply(c("summary_edit_informations_div", "save_summary_div", "edit_description_button"), shinyjs::hide)
      sapply(c("summary_view_informations_div", "edit_summary_div"), shinyjs::show)
    }))
    
    ## Delete an element ----
    
    observeEvent(input$delete_element, try_catch("input$delete_element", shinyjs::show("delete_element_modal")))
    
    observeEvent(input$close_element_deletion_modal, try_catch("input$close_element_deletion_modal", shinyjs::hide("delete_element_modal")))
    
    observeEvent(input$confirm_element_deletion, try_catch("input$confirm_element_deletion", {
      
      if(!(paste0(id, "_management") %in% user_accesses || (id == "subsets" && "projects_subsets_management" %in% user_accesses))) return()
      
      element_id <- input$selected_element
      
      # For plugins, get options id to delete corresponding code rows
      # Set plugin_id to 0 in widgets table
      # Delete plugins files
      if (id == "plugins"){
        options_ids <- DBI::dbGetQuery(r$db, glue::glue_sql("SELECT id FROM options WHERE category = 'plugin_code' AND link_id = {element_id}", .con = con)) %>% dplyr::pull()
        
        # Delete code in db
        sql_send_statement(con, glue::glue_sql("DELETE FROM code WHERE link_id IN ({options_ids*})", .con = con))
        sql_send_statement(con, glue::glue_sql("DELETE FROM options WHERE category = 'plugin_code' AND link_id = {element_id}", .con = con))
        
        # Set plugin_id to 0 in widgets table
        sql_send_statement(con, glue::glue_sql("UPDATE widgets SET plugin_id = 0 WHERE plugin_id = {element_id}", .con = con))
        
        # Delete files from code files list
        r$edit_plugin_code_files_list <- r$edit_plugin_code_files_list %>% dplyr::filter(plugin_id != element_id)
        
        # Delete editor
        editor_ids <- r$edit_plugin_code_editors %>% dplyr::filter(plugin_id == element_id) %>% dplyr::pull(id)
        
        # Clear editors
        editor_ids <- r$edit_plugin_code_editors %>% dplyr::filter(plugin_id == element_id) %>% dplyr::pull(id)
        for (editor_id in editor_ids) shinyAce::updateAceEditor(session, paste0("edit_code_editor_div_", editor_id), value = "")
      }
      
      # If it is a vocabulary, delete entry in concept table
      else if (id == "vocabularies"){
        
        # Get vocabulary_id
        vocabulary_id <- r$vocabularies_wide %>% dplyr::filter(id == element_id) %>% dplyr::pull(vocabulary_id)
        
        # Delete entry in concept table
        sql_send_statement(con, glue::glue_sql("DELETE FROM concept WHERE concept_name = {vocabulary_id} AND domain_id = 'Metadata' AND concept_class_id = 'Vocabulary'", .con = con))
      }
      
      # For datasets, set dataset_id to 0 in other tables having a dataset_id col
      # Delete dataset files
      else if (id == "datasets"){
        
        # Set dataset_id to 0 in widgets in tables having a dataset_id col
        
        for (table in c("persons_options", "concept_dataset")) sql_send_statement(
          m$db, glue::glue_sql("UPDATE {`table`} SET dataset_id = 0 WHERE dataset_id = {element_id}", .con = m$db))
        
        sql_send_statement(con, glue::glue_sql("UPDATE studies SET dataset_id = 0 WHERE dataset_id = {element_id}", .con = con))
        
        # Delete dataset files
        dataset_folder <- paste0(r$app_folder, "/datasets_files/", element_id)
        unlink(dataset_folder, recursive = TRUE)
      }
      
      # For projects, delete subsets, widgets and tabs
      else if (id == "projects") delete_project(element_id)
      
      # Delete element in db
      sql_send_statement(con, glue::glue_sql("DELETE FROM {`sql_table`} WHERE id = {element_id}", .con = con))
      
      # Delete code in db
      sql_send_statement(con, glue::glue_sql("DELETE FROM code WHERE category = {sql_category} AND link_id = {element_id}", .con = con))
      
      # Delete options in db
      sql_send_statement(con, glue::glue_sql("DELETE FROM options WHERE category = {sql_category} AND link_id = {element_id}", .con = con))
      
      # Delete files
      element_long <- r[[long_var]] %>% dplyr::filter(id == element_id)
      element_unique_id <- element_long %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value)
      element_folder <- paste0(r$app_folder, "/", id, "/", element_unique_id)
      unlink(element_folder, recursive = TRUE)
      
      # Reload widgets
      shinyjs::runjs(paste0("
        Shiny.setInputValue('", id, "-reload_elements_var', Math.random());
        Shiny.setInputValue('", id, "-show_home', Math.random());"))
      
      # Notify user
      show_message_bar(id, output, element_deleted, "warning", i18n = i18n, ns = ns)
      
      # Close modal
      shinyjs::hide("delete_element_modal")
    }))
    
    # |-------------------------------- -----
    
    # --- --- --- --- --
    # Share element ----
    # --- --- --- --- --
    
    ## Git synchronization ----
    
    ### Load a git repo ----
    observeEvent(input$git_repo, try_catch("input$git_repo", {
      
      # Reload git repo
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_git_repo', Math.random());"))
    }))
    
    observeEvent(input$reload_git_repo, try_catch("input$reload_git_repo", {
      
      if (paste0(id, "_share") %not_in% user_accesses) return()
      
      git_repo <- r$git_repos %>% dplyr::filter(id == input$git_repo)
      
      # Clone git repo if not already loaded
      
      loaded_git_repo <- tibble::tibble()
      
      tryCatch({
        loaded_git_repo <- load_git_repo(git_repo)
        }, error = function(e){
          show_message_bar(id, output, "error_loading_git_repo", "warning", i18n = i18n, ns = ns)
          cat(paste0("\n", now(), " - mod_widgets - error downloading git repo - error = ", toString(e)))
        }
      )
      
      if (nrow(loaded_git_repo) > 0){
        git_repo_local_path <- loaded_git_repo$local_path
        
        # Show upload git button
        shinyjs::show("reload_git_repo_div")
      }
      else {
        git_repo_local_path <- ""
        r$loaded_git_repos_objects[[git_repo$unique_id]] <- character(0)
        
        # Hide upload git button
        shinyjs::hide("reload_git_repo_div")
      }
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-git_repo_local_path', '", git_repo_local_path, "');"))

      # Reload git element UI
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_git_element_ui', Math.random());"))
    }))
    
    ### Reload git element UI ----
    
    observeEvent(input$reload_git_element_ui, try_catch("input$reload_git_element_ui", {
      
      if (paste0(id, "_share") %not_in% user_accesses) return()
      
      element_long <- r[[long_var]] %>% dplyr::filter(id == input$selected_element)
      git_element_ui <- div(
        shiny.fluent::MessageBar(i18n$t("error_loading_git_repo"), messageBarType = 5),
        style = "display: inline-block;"
      )
      synchronize_git_buttons <- tagList()
      
      git_repo_local_path <- input$git_repo_local_path
      
      if (git_repo_local_path != ""){
        
        elements_folder <- paste0(git_repo_local_path, "/", id)
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-git_repo_elements_folder', '", elements_folder, "');"))
        
        element_unique_id <- element_long %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value)
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-git_repo_element_unique_id', '", element_unique_id, "');"))
        
        element_name_en <- element_long %>% dplyr::filter(name == "name_en") %>% dplyr::pull(value)
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-git_repo_element_name_en', '", element_name_en, "');"))
        
        # Reload git repo element UI
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_git_repo_element_ui', 'Math.random()');"))
        
        element_exists <- FALSE
        
        # Check if this element already exists
        
        elements <- tibble::tibble()
        error_loading_xml_file <- TRUE
        
        tryCatch({  
          category_dir <- file.path(git_repo_local_path, id)
          elements <- get_elements_from_xml(category_dir = category_dir, single_id = single_id)
          error_loading_xml_file <- FALSE
        }, error = function(e) catelement_path(paste0("\n", now(), " - mod_git_repos - error downloading xml file : ", toString(e))))
        
        if (nrow(elements) > 0) if (elements %>% dplyr::filter(unique_id == element_unique_id) %>% nrow() > 0){
          element_exists <- TRUE
          file_name <- elements %>% dplyr::filter(unique_id == element_unique_id) %>% dplyr::pull(name_en) %>% remove_special_chars()
          element_path <- paste0(elements_folder, "/", file_name)
        }
        
        # Error downloading XML files
        if (error_loading_xml_file){
          
          git_element_ui <- div(
            shiny.fluent::MessageBar(i18n$t("error_loading_category_xml_file"), messageBarType = 5), 
            style = "display: inline-block;"
          )
        }
        
        # The element exists in the git repo
        else if (element_exists){
          
          shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-element_git_path', '", element_path, "');"))
          
          sql <- glue::glue_sql("SELECT * FROM {`sql_table`} WHERE id = {input$selected_element}", .con = r$db)
          local_element <- DBI::dbGetQuery(r$db, sql)
          
          # Get element infos from XML file
          xml_file_path <- paste0(element_path, "/", single_id, ".xml")
          
          error_loading_xml_file <- TRUE
          
          tryCatch({
            git_element <-
              xml2::read_xml(xml_file_path) %>%
              XML::xmlParse() %>%
              XML::xmlToDataFrame(nodes = XML::getNodeSet(., paste0("//", single_id)), stringsAsFactors = FALSE) %>%
              tibble::as_tibble() %>%
              dplyr::mutate(authors = stringr::str_replace_all(authors, "(?<=\\p{L})(?=\\p{Lu})", "; "))
            
            error_loading_xml_file <- FALSE
          }, error = function(e) cat(paste0("\n", now(), " - mod_git_repos - error downloading ", current_tab, " readme - error = ", toString(e))))
          
          if (error_loading_xml_file) git_element_ui <- div(
            shiny.fluent::MessageBar(i18n$t("error_loading_category_xml_file"), messageBarType = 5), 
            style = "display: inline-block;"
          )
          
          else {
            
            element_version <- element_long %>% dplyr::filter(name == "version") %>% dplyr::pull(value)
            local_version <- package_version(element_version)
            git_version <- package_version(git_element$version)
            
            update_needed <- FALSE
            if (local_version > git_version) update_needed <- TRUE
            
            git_element_ui <- div(
              tags$table(
                tags$tr(tags$td(strong(i18n$t("name")), style = "min-width: 80px;"), tags$td(git_element[[paste0("name_", language)]])),
                tags$tr(tags$td(strong(i18n$t("author_s"))), tags$td(git_element$authors)),
                tags$tr(tags$td(strong(i18n$t("created_on"))), tags$td(git_element$creation_datetime)),
                tags$tr(tags$td(strong(i18n$t("updated_on"))), tags$td(git_element$update_datetime)),
                tags$tr(tags$td(strong(i18n$t("local_version"))), tags$td(element_version)),
                tags$tr(tags$td(strong(i18n$t("remote_git_version"))), tags$td(git_element$version))
              ),
              class = "git-table"
            )

            # Update synchronize buttons
            
            if (update_needed) update_button <- shiny.fluent::PrimaryButton.shinyInput(ns("git_repo_update_element"), i18n$t("update"))
            else update_button <- div(
              div(
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("reload_git_repo"), iconProps = list(iconName = "SyncOccurence")), text = i18n$t("reload_git_repo")),
                onclick = paste0("Shiny.setInputValue('", id, "-reload_git_repo', Math.random());")
              ),
              shiny.fluent::PrimaryButton.shinyInput(ns("git_repo_update_element"), i18n$t("update"), disabled = TRUE),
              style = "display: flex; gap: 2px;"
            )
            
            synchronize_git_buttons <- div(update_button, style = "display: flex; gap: 5px;")
          }
        }
        
        # The element doesn't exist
        else {
          git_element_ui <- div(
            shiny.fluent::MessageBar(i18n$t(paste0(single_id, "_doesnt_exist_in_git_repo")), messageBarType = 5), 
            style = "display: inline-block;"
          )
          
          # Update synchronize buttons
          synchronize_git_buttons <- shiny.fluent::PrimaryButton.shinyInput(ns("git_repo_update_element"), i18n$t("add"))
        }
      }
      
      output$git_repo_element_ui <- renderUI(git_element_ui)
      output$synchronize_git_buttons <- renderUI(synchronize_git_buttons)
    }))
    
    ### Update or delete git element ----
    observeEvent(input$git_repo_update_element, try_catch("input$git_repo_update_element", {
      
      sapply(c("update_git_element_text_div", "confirm_git_element_update_div"), shinyjs::show)
      sapply(c("delete_git_element_text_div", "confirm_git_element_deletion_div"), shinyjs::hide)
      shinyjs::show("update_or_delete_git_element_modal")
    }))

    observeEvent(input$close_update_or_delete_git_element_modal, try_catch("input$close_update_or_delete_git_element_modal", {
      shinyjs::hide("update_or_delete_git_element_modal")
    }))

    observeEvent(input$confirm_git_element_update, try_catch("input$confirm_git_element_update", {

      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_git_type', 'update');"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_git', Math.random());"))
    }))
    
    # Update / delete git element confirmed
    observeEvent(input$update_git, try_catch("input$update_git", {
      
      if (paste0(id, "_share") %not_in% user_accesses) return()
      
      # Check if API key is empty
      if (input$update_or_delete_git_element_api_key == ""){
        shiny.fluent::updateTextField.shinyInput(session, "update_or_delete_git_element_api_key", errorMessage = i18n$t("provide_valid_api_key"))
        return()
      }
      else shiny.fluent::updateTextField.shinyInput(session, "update_or_delete_git_element_api_key", errorMessage = NULL)
      
      git_repo <- r$git_repos %>% dplyr::filter(id == input$git_repo)
      
      # Local git repo dir
      local_dir <- paste0(r$app_folder, "/", id, "/", input$git_repo_element_unique_id)
      
      # Delete element folder
      unlink(input$element_git_path, recursive = TRUE)
      
      # If this is an update, copy new element
      
      if (input$update_git_type == "update"){
        
        if (!dir.exists(input$git_repo_elements_folder)) dir.create(input$git_repo_elements_folder)
        
        new_dir <- file.path(input$git_repo_elements_folder)
        
        # Reload subsets var
        if (id == "projects") reload_elements_var(page_id = "subsets", id = "subsets", con = m$db, long_var_filtered = "filtered_subsets_long")
        
        # Create element files
        
        element_wide <- r$projects_wide %>% dplyr::filter(id == input$selected_element)
        
        temp_zip_dir <- create_element_files(
          sql_category = sql_category, con = con, single_id = single_id,
          element_id = input$selected_element, element_wide = element_wide
        )
        
        # Copy files
        
        files <- list.files(temp_zip_dir, full.names = TRUE, recursive = TRUE)
        
        # Recreate folders hierarchy
        for (file in files) {
          relative_path <- gsub(paste0("^", temp_zip_dir, "/"), "", file)
          target_path <- file.path(new_dir, relative_path)
          dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
          file.copy(file, target_path, overwrite = TRUE)
        }
      }
      
      # Commit and push
      
      error_update_remote_git_repo <- TRUE
      
      if (length(r$loaded_git_repos_objects[[git_repo$unique_id]]) > 0){
        tryCatch({
          repo <- r$loaded_git_repos_objects[[git_repo$unique_id]]
          username <- r$users %>% dplyr::filter(id == r$user_id) %>% dplyr::pull(name)
          git2r::config(repo, user.name = username)
          git2r::add(repo, ".")

          if (length(git2r::status(repo, unstaged = FALSE, untracked = FALSE, ignored = FALSE)$staged) > 0){

            commit_message <- input$update_or_delete_git_element_commit_message
            if (commit_message == "") commit_message <- paste0("Update ", single_id, " from LinkR")
            git2r::commit(repo, message = commit_message)

            # Create main branch if doesn't exist
            git_branches <- names(git2r::branches(repo))
            if (!"main" %in% git_branches){
              git2r::branch_create(commit = git2r::last_commit(repo), name = "main")
              git2r::checkout(repo, "main")
            }

            credentials <- git2r::cred_user_pass("linkr_user", input$update_or_delete_git_element_api_key)

            git2r::push(repo, "origin", "refs/heads/main", credentials = credentials)

            # Reset commit message (we keep API key, easier when we push files frequently)
            shiny.fluent::updateTextField.shinyInput(session, "update_or_delete_git_element_commit_message", value = "")

            # Notify user
            show_message_bar(id, output, "success_update_remote_git_repo", "success", i18n = i18n, ns = ns)

            # Reload git element UI
            shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_git_element_ui', Math.random());"))

            error_update_remote_git_repo <- FALSE
          }
        }, error = function(e){
          show_message_bar(id, output, message = "error_update_remote_git_repo", type = "severeWarning", i18n = i18n, ns = ns)
          cat(paste0("\n", now(), " - mod_widgets - (", id, ") - update remot git error - ", toString(e)))
        })
      }
      
      if (error_update_remote_git_repo) show_message_bar(id, output, message = "error_update_remote_git_repo", type = "severeWarning", i18n = i18n, ns = ns)
      
      # Close modal
      shinyjs::hide("update_or_delete_git_element_modal")
    }))
    
    ## Export element ----
    
    observeEvent(input$export_element, try_catch("input$export_element", shinyjs::click("export_element_download")))
    
    output$export_element_download <- downloadHandler(

      filename = function(){
        element_name <-
          r[[wide_var]] %>% 
          dplyr::filter(id == input$selected_element) %>%
          dplyr::pull(name) %>%
          remove_special_chars()
        
        paste0("linkr_", single_id, "_", element_name, "_", now() %>% stringr::str_replace_all(" ", "_") %>% stringr::str_replace_all(":", "-") %>% as.character(), ".zip")
      },

      content = function(file) try_catch("", {
        
        if (paste0(id, "_share") %in% user_accesses){
            
          element_wide <- r[[wide_var]] %>% dplyr::filter(id == input$selected_element)
          
          # Reload subsets var
          if (id == "projects") reload_elements_var(page_id = "subsets", id = "subsets", con = m$db, long_var_filtered = "filtered_subsets_long")
          
          temp_zip_dir <- create_element_files(
            sql_category = sql_category, con = con, single_id = single_id,
            element_id = input$selected_element, element_wide = element_wide
          )
          
          # Create a ZIP
          zip::zipr(file, list.files(temp_zip_dir, full.names = TRUE))
        }
      })
    )
    
    # Prevent a bug : show_message_bar in downloadHandler never ends
    observeEvent(input$error_downloading_element, try_catch("input$error_downloading_element", {
      show_message_bar(id, output, message = paste0("error_downloading_", single_id), type = "severeWarning", i18n = i18n, ns = ns)
    }))
  })
}