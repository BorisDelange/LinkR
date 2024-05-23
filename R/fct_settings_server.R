#' Add new settings data
#' 
#' @param session Shiny session variable
#' @param output Shiny output variable
#' @param r Shiny reactive value, used to communicate between modules (reactiveValue)
#' @param i18n Translator object from shiny.i18n library
#' @param id ID of current tab / page (character)
#' @param data A list with data to add (list)
#' @param table Name of the corresponding table in database (character)
#' @param required_textfields Which textfields are required (not empty) before inserting data in database ? (character)
#' @param req_unique_values Which fields require unique values before inserting data in database ? (character)
#' @param required_dropdowns Which dropdowns are required (not empty) before insert data in database ? Default to "all" (character)
#' @param dropdowns Tibble with the values of distinct dropdowns names (tibble)
#' @param r_message_bar If the MessageBar is displayed with a r variable or with show_message_bar function (TRUE for r variable)
#' @examples 
#' \dontrun{
#' data <- list()
#' data$name <- "New dataset"
#' data$description <- "Description of the dataset"
#' data$data_source <- 5
#' add_settings_new_data(output = output, r = r, i18n = i18n, id = "settings_datasets", data = data, table = "datasets", dropdowns = "data_source")
#' }
add_settings_new_data <- function(session, output, r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(),
  i18n = character(), id = character(), data = tibble::tibble(), table = character(), required_textfields = character(), req_unique_values = character(), 
  required_dropdowns = "all", dropdowns = character(), r_message_bar = FALSE){
  
  ns <- shiny::NS(id)
  
  # db variable depending on table
  m_tables <- c("patients_options", "widgets_options", "subsets" , "subset_persons",
    "concept", "vocabulary", "domain", "concept_class", "concept_relationship", "relationship", "concept_synonym", "concept_ancestor", "drug_strength")
  if (table %in% m_tables) db <- m$db
  else db <- r$db
  
  # For patient_lvl & aggregated tables
  if (grepl("patient_lvl", table)) category <- "patient_lvl" else category <- "aggregated"
  
  # --- --- --- --- --- --- --- --- -
  # Check textfields & dropdowns ----
  # --- --- --- --- --- --- --- --- -
  
  # For textfields, we have the choice if empty data is possible or not
  # For dropdowns, we want all of them filled
  
  # Check if required textfields are not empty
  
  sapply(required_textfields, function(textfield){
    
    if (is.na(data[[textfield]])){
      shiny.fluent::updateTextField.shinyInput(session, 
        textfield, errorMessage = i18n$t(paste0("provide_valid_", textfield)))
    }
    else shiny.fluent::updateTextField.shinyInput(session, textfield, errorMessage = NULL)
    
    req(!is.na(data[[textfield]]))
  })
  
  # Check if values required to be unique are unique
  sapply(req_unique_values, function(field){
    
    # If it is a new tab, group by tab family, so a name musn't be unique in distinct tabs families
    if (table %in% c("patient_lvl_tabs", "aggregated_tabs")){
      if (is.na(data$parent_tab)) sql <- glue::glue_sql("SELECT DISTINCT({`field`}) FROM tabs WHERE deleted IS FALSE 
        AND category = {category} AND tab_group_id = {data$tab_group} AND parent_tab_id IS NULL", .con = db)
      if (!is.na(data$parent_tab)) sql <- glue::glue_sql("SELECT DISTINCT({`field`}) FROM tabs WHERE deleted IS FALSE
        AND category = {category} AND tab_group_id = {data$tab_group} AND parent_tab_id = {data$parent_tab}", .con = db)
    }
    else if (table == "studies") sql <- glue::glue_sql("SELECT DISTINCT({`field`}) FROM {`table`} WHERE deleted IS FALSE
      AND dataset_id = {data$dataset}", .con = db)
    else if (table == "subsets") sql <- glue::glue_sql("SELECT DISTINCT({`field`}) FROM {`table`} WHERE deleted IS FALSE
      AND study_id = {data$study_id}", .con = db)
    else if (table == "plugins") sql <- glue::glue_sql("SELECT DISTINCT({`field`}) FROM {`table`} WHERE deleted IS FALSE
      AND tab_type_id = {data$tab_type}", .con = db)
    else sql <- glue::glue_sql("SELECT DISTINCT({`field`}) FROM {`table`} WHERE deleted IS FALSE", .con = db)
    
    distinct_values <- DBI::dbGetQuery(db, sql) %>% dplyr::pull() %>% tolower()
    
    if (tolower(data[[field]]) %in% distinct_values) {
      if (!r_message_bar) show_message_bar(output, message = paste0(field, "_already_used"), type = "severeWarning", i18n = i18n, ns = ns)
      if (r_message_bar) r[[paste0(table, "_show_message_bar")]] <- tibble::tibble(message = paste0(field, "_already_used"), type = "severeWarning", trigger = now())
    }
    req(tolower(data[[field]]) %not_in% distinct_values)
  })
  
  # Check if dropdowns are not empty (if all are required)
  dropdowns_check <- TRUE
  
  if (required_dropdowns == "all"){
    for(dropdown in dropdowns){
      if (dropdown != ""){
        if (length(data[[dropdown]]) > 1) break
        if (is.null(data[[dropdown]])) dropdowns_check <- FALSE
        else if (is.na(data[[dropdown]])) dropdowns_check <- FALSE
      }
    }
  }
  
  else {
    for(dropdown in required_dropdowns){
      if (dropdown != ""){
        if (length(data[[dropdown]]) > 1) break
        if (is.null(data[[dropdown]])) dropdowns_check <- FALSE
        else if (is.na(data[[dropdown]])) dropdowns_check <- FALSE
      }
    }
  }
  
  if (!dropdowns_check){
    if (!r_message_bar) show_message_bar(output, message = "dropdown_empty", type = "severeWarning", i18n = i18n, ns = ns)
    if (r_message_bar) r[[paste0(table, "_show_message_bar")]] <- tibble::tibble(message = "dropdown_empty", type = "severeWarning", trigger = now())
  }
  req(dropdowns_check)
  
  # --- --- --- --- --- --- --- --- -- -
  # Add data in data specific table ----
  # --- --- --- --- --- --- --- --- -- -
  
  # Get last_row nb
  last_row <- list()
  for (var in c("code", "options", "subsets")){
    if (var == "subsets") last_row[[var]] <- get_last_row(m$db, "subsets")
    else last_row[[var]] <- get_last_row(r$db, var)
  }
  
  if (table %in% c("patient_lvl_tabs", "aggregated_tabs")) last_row$data <- get_last_row(db, "tabs")
  # if (table %in% c("patient_lvl_tabs_groups", "aggregated_tabs_groups")) last_row$data <- get_last_row(db, "tabs_groups")
  else last_row$data <- get_last_row(db, table)
  
  # Create a list with all new data
  new_data <- list()
  
  # Creation of new_data$data variable for data_management pages
  # if (table %in% c("data_sources", "datasets", "subsets")){
  if (table %in% c("datasets", "subsets")){
    
    # These columns are found in all of these tables
    new_data$data <- tibble::tribble(~id, ~name, ~description, last_row$data + 1, as.character(data$name), as.character(data$description))
    
    # if (table == "datasets") new_data$data <- new_data$data %>% dplyr::bind_cols(tibble::tribble(~data_source_id, as.integer(data$data_source)))
    if (table == "datasets") new_data$data <- new_data$data %>% dplyr::bind_cols(tibble::tribble(~data_source_id, NA_integer_))
    if (table == "subsets") new_data$data <- new_data$data %>% dplyr::bind_cols(tibble::tribble(~study_id, as.integer(data$study)))
    
    # These columns are also found in all of these tables
    # Add them at last to respect the order of cols
    new_data$data <- new_data$data %>% dplyr::bind_cols(tibble::tribble(~creator_id, ~datetime, ~deleted, r$user_id, now(), FALSE))
  }
  
  if (table == "datasets"){
    # new_data$data <- tibble::tribble(
    #   ~id, ~name, ~data_source_id, ~creator_id, ~creation_datetime, ~update_datetime, ~deleted,
    #   last_row$data + 1, as.character(data$name), as.integer(data$data_source), r$user_id, now(), now(), FALSE)
    new_data$data <- tibble::tribble(
      ~id, ~name, ~data_source_id, ~creator_id, ~creation_datetime, ~update_datetime, ~deleted,
      last_row$data + 1, as.character(data$name), NA_integer_, r$user_id, now(), now(), FALSE)
  }

  # Creation of new_data$data variable for vocabulary page
  else if (table == "vocabulary"){
    # new_data$data <- tibble::tribble(~id, ~vocabulary_id, ~vocabulary_name, ~vocabulary_reference, ~vocabulary_version,
    #   ~vocabulary_concept_id, ~display_order, ~data_source_id, ~creator_id, ~creation_datetime, ~update_datetime, ~deleted,
    #   last_row$data + 1, as.character(data$vocabulary_id), as.character(data$vocabulary_name), "", "", "", NA_integer_,
    #   data$data_source, r$user_id, now(), now(), FALSE)
    new_data$data <- tibble::tribble(~id, ~vocabulary_id, ~vocabulary_name, ~vocabulary_reference, ~vocabulary_version,
      ~vocabulary_concept_id, ~display_order, ~data_source_id, ~creator_id, ~creation_datetime, ~update_datetime, ~deleted,
      last_row$data + 1, as.character(data$vocabulary_id), as.character(data$vocabulary_name), "", "", NA_integer_,
      NA_integer_, "", r$user_id, now(), now(), FALSE)
  }
  
  # Creation of new_data$data variable for studies page
  else if (table == "studies"){
    new_data$data <- tibble::tribble(
      ~id, ~name, ~dataset_id, ~patient_lvl_tab_group_id, ~aggregated_tab_group_id, ~creator_id, ~creation_datetime, ~update_datetime, ~deleted,
      last_row$data + 1, as.character(data$name), as.integer(data$dataset), as.integer(data$patient_lvl_tab_group), as.integer(data$aggregated_tab_group),
      r$user_id, now(), now(), FALSE)
  }
  
  # Creation of new_data$data variable for plugins page
  else if (table == "plugins"){
    new_data$data <- tibble::tribble(~id, ~name, ~tab_type_id, ~creation_datetime, ~update_datetime, ~deleted,
      last_row$data + 1, as.character(data$name), as.integer(data$tab_type), now(), now(), FALSE)
  }
  
  # Creation of new_data$data variable for scripts page
  else if (table == "scripts"){
    new_data$data <- tibble::tribble(~id, ~name, ~creation_datetime, ~update_datetime, ~deleted,
      last_row$data + 1, as.character(data$name), now(), now(), FALSE)
  }
  
  # Creation of new_data$data variable for users sub-pages
  # Password is hashed
  else if (table == "users"){
    new_data$data <- tibble::tribble(~id, ~username, ~firstname, ~lastname, ~password, ~user_access_id, ~user_status_id, ~datetime, ~deleted,
      last_row$data + 1, as.character(data$username), as.character(data$firstname), as.character(data$lastname),
      rlang::hash(data$password), as.integer(data$user_access), as.integer(data$user_status), now(), FALSE)
  }
  
  else if (table %in% c("users_accesses", "users_statuses")){
    new_data$data <- tibble::tribble(~id, ~name, ~description, ~datetime, ~deleted,
      last_row$data + 1, as.character(data$name), as.character(data$description), now(), FALSE)
  }
  
  else if (table %in% c("patient_lvl_tabs", "aggregated_tabs")){
    new_data$data <- tibble::tribble(~id, ~category, ~name,  ~description, ~tab_group_id, ~parent_tab_id,  ~display_order, ~creator_id, ~datetime, ~deleted,
      last_row$data + 1, category, as.character(data$name), as.character(data$description), as.integer(data$tab_group), as.integer(data$parent_tab),
      as.integer(data$display_order), r$user_id, now(), FALSE)
  }
  
  # else if (table %in% c("patient_lvl_tabs_groups", "aggregated_tabs_groups")){
  #   new_data$data <- tibble::tribble(~id, ~category, ~name,  ~description, ~creator_id, ~datetime, ~deleted,
  #     last_row$data + 1, category, as.character(data$name), as.character(data$description), r$user_id, now(), FALSE)
  # }
  
  else if (table == "git_repos"){
    if (length(data$unique_id) > 0) unique_id <- data$unique_id
    else unique_id <- paste0(sample(c(0:9, letters[1:6]), 64, TRUE), collapse = '')
    new_data$data <- tibble::tribble(~id, ~unique_id, ~name, ~api_key, ~repo_url_address, ~raw_files_url_address, ~creator_id, ~datetime, ~deleted,
      last_row$data + 1, unique_id, as.character(data$name), as.character(data$api_key),
      as.character(data$repo_url_address), as.character(data$raw_files_url_address), r$user_id, now(), FALSE)
  }
  
  # Append data to the table and to r / m variables
  if (table %in% c("patient_lvl_tabs", "aggregated_tabs")) DBI::dbAppendTable(db, "tabs", new_data$data)
  # else if (table %in% c("patient_lvl_tabs_groups", "aggregated_tabs_groups")) DBI::dbAppendTable(db, "tabs_groups", new_data$data)
  else DBI::dbAppendTable(db, table, new_data$data)

  if (table %not_in% m_tables | table == "vocabulary") r[[table]] <- r[[table]] %>% dplyr::bind_rows(new_data$data)
  else m[[table]] <- m[[table]] %>% dplyr::bind_rows(new_data$data)
  add_log_entry(r = r, category = paste0(table, " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_data$data))
  
  # Empty new variables
  new_data_vars <- c("options", "subsets", "code", "tabs_groups")
  for(var in new_data_vars) new_data[[var]] <- tibble::tibble()

  # --- --- --- --- --- --- --- --- --- --
  # Add data in code & options tables ----
  # --- --- --- --- --- --- --- --- --- --
  
  username <- r$users %>% dplyr::filter(id == r$user_id) %>% dplyr::mutate(fullname = paste0(firstname, " ", lastname)) %>% dplyr::pull(fullname)
  
  # Add a row in code if table is datasets, vocabulary
  if (table %in% c("datasets", "vocabulary")){
    new_data$code <- tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
      last_row$code + 1, get_singular(word = table), last_row$data + 1, "", r$user_id, now(), FALSE)
  }
  
  # For options of plugins, add one row for long description (Markdown) & 2 rows for users allowed to use this plugin
  # The value is the default syntax of a plugin description
  # For code of plugins, add two rows, one for UI code & one for server code
  if (grepl("plugins", id)){
    
    # Add options rows
    new_data$options <- tibble::tribble(
      ~name, ~value, ~value_num,
      "users_allowed_read_group", "everybody", 1,
      "user_allowed_read", "", r$user_id,
      "version", "0.0.1.9000", NA_integer_,
      "unique_id", paste0(sample(c(0:9, letters[1:6]), 64, TRUE), collapse = ''), NA_integer_,
      "author", username, NA_integer_,
      "image", "", NA_integer_,
      "downloaded_from", "", NA_integer_,
      "downloaded_from_url", "", NA_integer_
    ) %>%
    dplyr::bind_rows(
      r$languages %>%
        tidyr::crossing(name = c("description", "category", "name")) %>%
        dplyr::mutate(
          name = paste0(name, "_", code),
          value = ifelse(grepl("name_", name), as.character(data$name), ""),
          value_num = NA_integer_
        ) %>%
        dplyr::select(-code, -language)
    ) %>%
    dplyr::mutate(id = last_row$options + dplyr::row_number(), category = "plugin", link_id = last_row$data + 1, .before = "name") %>%
    dplyr::mutate(creator_id = r$user_id, datetime = now(), deleted = FALSE)
    
    # Add code rows
    new_data$code <- tibble::tibble(
      id = last_row$code + 1:3,
      category = c("plugin_ui", "plugin_server", "plugin_translations"),
      link_id = last_row$data + 1, code = "", creator_id = r$user_id, datetime = now(), deleted = FALSE
    )
  }

  # For options of scripts, add one row for long description (Markdown)
  if (table == "scripts"){
    
    # Add options rows
    new_data$options <- tibble::tribble(
      ~name, ~value, ~value_num,
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
            value = ifelse(grepl("name_", name), as.character(data$name), ""),
            value_num = NA_integer_
          ) %>%
          dplyr::select(-code, -language)
      ) %>%
      dplyr::mutate(id = last_row$options + dplyr::row_number(), category = "script", link_id = last_row$data + 1, .before = "name") %>%
      dplyr::mutate(creator_id = r$user_id, datetime = now(), deleted = FALSE)
    
    # Add code row
    new_data$code <- tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
      last_row$code + 1, "script", last_row$data + 1, "", r$user_id, now(), FALSE)
  }
  
  # For datasets options
  if (table == "datasets"){
    
    # Add options rows
    new_data$options <- tibble::tribble(
      ~name, ~value, ~value_num,
      "version", "0.0.1.9000", NA_integer_,
      "unique_id", paste0(sample(c(0:9, letters[1:6]), 64, TRUE), collapse = ''), NA_integer_,
      "author", username, NA_integer_,
      "users_allowed_read_group", "everybody", 1,
      "user_allowed_read", "", r$user_id,
      "show_only_aggregated_data", "", 0,
      "omop_version", "6.0", NA_integer_,
      "activate_scripts_cache", "", 0,
      "downloaded_from", "", NA_integer_,
      "downloaded_from_url", "", NA_integer_
    ) %>%
      dplyr::bind_rows(
        r$languages %>%
          tidyr::crossing(name = c("description", "category", "name")) %>%
          dplyr::mutate(
            name = paste0(name, "_", code),
            value = ifelse(grepl("name_", name), as.character(data$name), ""),
            value_num = NA_integer_
          ) %>%
          dplyr::select(-code, -language)
      ) %>%
      dplyr::mutate(id = last_row$options + dplyr::row_number(), category = "dataset", link_id = last_row$data + 1, .before = "name") %>%
      dplyr::mutate(creator_id = r$user_id, datetime = now(), deleted = FALSE)
  }
  
  if (table == "vocabulary"){
    
    # Add options rows
    new_data$options <- tibble::tribble(
      ~name, ~value, ~value_num,
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
            value = ifelse(grepl("name_", name), as.character(data$vocabulary_name), ""),
            value_num = NA_integer_
          ) %>%
          dplyr::select(-code, -language)
      ) %>%
      dplyr::mutate(id = last_row$options + dplyr::row_number(), category = "vocabulary", link_id = last_row$data + 1, .before = "name") %>%
      dplyr::mutate(creator_id = r$user_id, datetime = now(), deleted = FALSE)
  }
  
  # For studies, need to add one row in options and add rows of code for subsets, with default value
  if (table == "studies"){
    
    # Add options rows
    new_data$options <- tibble::tribble(
      ~name, ~value, ~value_num,
      "version", "0.0.1.9000", NA_integer_,
      "unique_id", paste0(sample(c(0:9, letters[1:6]), 64, TRUE), collapse = ''), NA_integer_,
      "author", username, NA_integer_,
      "users_allowed_read_group", "everybody", 1,
      "user_allowed_read", "", r$user_id,
      "downloaded_from", "", NA_integer_,
      "downloaded_from_url", "", NA_integer_
    ) %>%
      dplyr::bind_rows(
        r$languages %>%
          tidyr::crossing(name = c("description", "category", "name")) %>%
          dplyr::mutate(
            name = paste0(name, "_", code),
            value = ifelse(grepl("name_", name), as.character(data$name), ""),
            value_num = NA_integer_
          ) %>%
          dplyr::select(-code, -language)
      ) %>%
      dplyr::mutate(id = last_row$options + dplyr::row_number(), category = "study", link_id = last_row$data + 1, .before = "name") %>%
      dplyr::mutate(creator_id = r$user_id, datetime = now(), deleted = FALSE)
    
    # Add rows in subsets table, for inclusion / exclusion subsets
    # Add also code corresponding to each subset
    new_data$subsets <- tibble::tribble(~id, ~name, ~description, ~study_id, ~creator_id,  ~datetime, ~deleted,
      last_row$subsets + 1, i18n$t("subset_all_patients"), "", last_row$data + 1, r$user_id, now(), FALSE)
    
    # Add code for creating subset with all patients
    code <- paste0("add_persons_to_subset(output = output, m = m, persons = d$person %>% dplyr::select(person_id) %>% dplyr::collect(), subset_id = %subset_id%, i18n = i18n, ns = ns)")
    new_data$code <- tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
      last_row$code + 1, "subset", last_row$subsets + 1, code, r$user_id, now(), FALSE)
    
    # Add patient_lvl & aggregated tabs families
    
    new_data$tabs_groups <- tibble::tribble(~id, ~category, ~name, ~description, ~creator_id, ~datetime, ~deleted,
      get_last_row(r$db, "tabs_groups") + 1, "patient_lvl", data$name, "", r$user_id, now(), FALSE,
      get_last_row(r$db, "tabs_groups") + 2, "aggregated", data$name, "", r$user_id, now(), FALSE)
    
    # new_data$aggregated_tabs_groups <- tibble::tribble(~id, ~name, ~description, ~creator_id, ~datetime, ~deleted,
    #   get_last_row(r$db, "aggregated_tabs_groups") + 1, data$name, "", r$user_id, now(), FALSE)
    
    # Add persons to subset
    tryCatch({
      persons <- d$person %>% dplyr::select(person_id) %>% dplyr::collect() %>% dplyr::mutate_at("person_id", as.integer)
      add_persons_to_subset(output = output, r = r, m = m, persons = persons, subset_id = last_row$subsets + 1, i18n = i18n, ns = ns)
    }, 
      error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_adding_patients_to_subset", 
        error_name = paste0("add study - add_persons_to_subsets - id = ", last_row$subsets + 1), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
    
    # Update sidenav dropdown with the new study
    r$studies_choices <- DBI::dbGetQuery(r$db, paste0("SELECT * FROM studies WHERE dataset_id = ", data$dataset))
    
    # Select new study as current study
    m$selected_study <- last_row$data + 1
    r$study_page <- now()
  }
  
  # For subsets, need to add one row in code
  if (table == "subsets"){
    new_data$code <- tibble::tribble(~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
      last_row$code + 1, "subset", last_row$subsets + 1, "", r$user_id, now(), FALSE)
  }
  
  # For options of patient_lvl & aggregated tabs families, need to add two rows, for users accesses
  # if (table %in% c("patient_lvl_tabs_groups", "aggregated_tabs_groups")){
  #   new_data$options <- tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
  #     last_row$options + 1, get_singular(word = table), last_row$data + 1, "users_allowed_read_group", "everybody", 1, r$user_id, now(), FALSE,
  #     last_row$options + 2, get_singular(word = table), last_row$data + 1, "user_allowed_read", "", r$user_id, r$user_id, now(), FALSE)
  # }
  
  # Hide creation card & options card, show management card
  # Except for tabs (we usually add several tabs)
  # if (table %not_in% c("patient_lvl_tabs", "aggregated_tabs")){
  #   shiny.fluent::updateToggle.shinyInput(session, "options_card_toggle", value = FALSE)
  #   shiny.fluent::updateToggle.shinyInput(session, "creation_card_toggle", value = FALSE)
  #   shiny.fluent::updateToggle.shinyInput(session, "datatable_card_toggle", value = TRUE)
  # }
  
  if (table == "git_repos"){
    # Add options rows
    new_data$options <- 
      tibble::tribble(
        ~name, ~value, ~value_num,
        "users_allowed_read_group", "everybody", 1,
        "user_allowed_read", "", r$user_id
      ) %>%
      dplyr::bind_rows(
        r$languages %>%
          tidyr::crossing(name = "name") %>%
          dplyr::mutate(
            name = paste0(name, "_", code),
            value = ifelse(grepl("name_", name), as.character(data$name), ""),
            value_num = NA_integer_
          ) %>%
          dplyr::select(-code, -language)
      ) %>%
      dplyr::mutate(id = last_row$options + dplyr::row_number(), category = "git_repo", link_id = last_row$data + 1, .before = "name") %>%
      dplyr::mutate(creator_id = r$user_id, datetime = now(), deleted = FALSE)
  }
  
  # Add new data to r variables and database
  for (var in new_data_vars){
    if (nrow(new_data[[var]]) > 0){
      if (var == "subsets"){
        DBI::dbAppendTable(m$db, var, new_data$subsets)
        m$subsets <- m$subsets %>% dplyr::bind_rows(new_data$subsets)
      } 
      else {
        DBI::dbAppendTable(r$db, var, new_data[[var]])
        if (var == "tabs_groups"){
          r$patient_lvl_tabs_groups <- r$patient_lvl_tabs_groups %>% dplyr::bind_rows(new_data$tabs_groups %>% dplyr::filter(category == "patient_lvl"))
          r$aggregated_tabs_groups <- r$aggregated_tabs_groups %>% dplyr::bind_rows(new_data$tabs_groups %>% dplyr::filter(category == "aggregated"))
        }
        else r[[var]] <- r[[var]] %>% dplyr::bind_rows(new_data[[var]]) 
      }
      add_log_entry(r = r, category = paste0(var, " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_data[[var]]))
    }
  }
  
  if (!r_message_bar) show_message_bar(output, message = paste0(get_singular(table), "_added"), type = "success", i18n = i18n, ns = ns)
  if (r_message_bar) r[[paste0(table, "_show_message_bar")]] <- tibble::tibble(message = paste0(get_singular(table), "_added"), type = "success", trigger = now())
  
  # Reset textfields
  if (table == "users") sapply(c("username", "firstname", "lastname", "password"), function(name) shiny.fluent::updateTextField.shinyInput(session, name, value = ""))
  else if (table == "vocabulary") sapply(c("vocabulary_id", "vocabulary_name"), function(name) shiny.fluent::updateTextField.shinyInput(session, name, value = ""))
  else if (table %in% c("plugins", "scripts", "studies", "subsets")) shiny.fluent::updateTextField.shinyInput(session, paste0(get_singular(table), "_name"), value = "")
  else if (table == "datasets") shiny.fluent::updateTextField.shinyInput(session, "name", value = "")
  else if (table == "git_repos") sapply(c("raw_files_url_address", "repo_url_address", "api_key_git_repo_with_url"), function(name) shiny.fluent::updateTextField.shinyInput(session, name, value = ""))
}

#' Delete element
#' 
#' @description Create reactivity to delete a row in a datatable
#' @param r Shiny reactive value, to communicate between modules (reactiveValue)
#' @param m Shiny reactive value, to communicate between modules (reactiveValue)
#' @param session Shiny session variable
#' @param input Shiny input variable
#' @param output Shiny output variable
#' @param ns Shiny namespace
#' @param i18n Translator object from shiny.i18n library
#' @param delete_prefix Prefix used for reactive & input variables (character)
#' @param dialog_title Text for the title of the reactOutput (character)
#' @param dialog_subtext Text for the subText area of the reactOutput (character)
#' @param react_variable Name of r variable used to open or close reactOutput (character)
#' @param table Name of the table in the database where the data will be deleted (character)
#' @param r_table Name of the r variable containing the data to be deleted (charadter)
#' @param id_var_sql Name of the ID column in the database (character)
#' @param id_var_r Name of the ID column in r variable (character)
#' @param delete_message Message displayed when the deletion is confirmed (character)
#' @param reload_variable Name of r variable that triggers the reload of data (character)
#' @param information_variable Name of r variable containing the ID of deleted data (character)
#' @param translation TRUE if dialog_title & dialog_subtext have to be translated (logical)
#' @param app_folder App folder (character)
#' @param prefix A prefix used in Shiny modules (character)
#' @param r_message_bar If the MessageBar is displayed with a r variable or with show_message_bar function (TRUE for r variable)
#' @examples 
#' \dontrun{
#' widget_delete_prefix <- paste0(prefix, "_widget")
#' widget_dialog_title <- paste0(prefix, "_widgets_delete")
#' widget_dialog_subtext <- paste0(prefix, "_widgets_delete_subtext")
#' widget_react_variable <- "widget_delete_confirm"
#' widget_table <- paste0(prefix, "_widgets")
#' widget_id_var_sql <- "id"
#' widget_id_var_r <- paste0(prefix, "_selected_widget")
#' widget_delete_message <- paste0(prefix, "_widget_deleted")
#' widget_reload_variable <- paste0(prefix, "_load_ui")
#' widget_delete_variable <- paste0(widget_delete_prefix, "_open_dialog")
#' widget_information_variable <- paste0(prefix, "_widget_deleted")
#'
#' delete_element(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
#'   delete_prefix = widget_delete_prefix, dialog_title = widget_dialog_title, dialog_subtext = widget_dialog_subtext,
#'   react_variable = widget_react_variable, table = widget_table, id_var_sql = widget_id_var_sql, id_var_r = widget_id_var_r,
#'   delete_message = widget_delete_message, translation = TRUE, reload_variable = widget_reload_variable,
#'   information_variable = widget_information_variable)
#' }
delete_element <- function(r = shiny::reactiveValues(), m = shiny::reactiveValues(), session, input, output, ns = character(), i18n = character(),
  delete_prefix = character(), dialog_title = character(), dialog_subtext = character(),
  react_variable = character(), table = character(), r_table = character(), id_var_sql = character(), id_var_r = character(),
  delete_message = character(), reload_variable = character(), information_variable = character(), translation = TRUE, 
  app_folder = character(), prefix = character(), r_message_bar = FALSE){
  
  delete_variable <- paste0(delete_prefix, "_open_dialog")
  
  r[[delete_variable]] <- FALSE
  
  if (translation){
    dialog_title <- i18n$t(dialog_title)
    dialog_subtext <- i18n$t(dialog_subtext)
  }
  
  dialog_content <- list(
    type = 0,
    title = dialog_title,
    closeButtonAriaLabel = "Close",
    subText = tagList(dialog_subtext, br(), br())
  )
  
  output[[react_variable]] <- shiny.fluent::renderReact({
    
    shiny.fluent::Dialog(
      hidden = !r[[delete_variable]],
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", delete_prefix, "_hide_dialog', Math.random()); }")),
      dialogContentProps = dialog_content,
      modalProps = list(),
      shiny.fluent::DialogFooter(
        shiny.fluent::PrimaryButton.shinyInput(ns(paste0(delete_prefix, "_delete_confirmed")), text = i18n$t("delete")),
        shiny.fluent::DefaultButton.shinyInput(ns(paste0(delete_prefix, "_delete_canceled")), text = i18n$t("dont_delete"))
      )
    )
  })
  
  # Whether to close or not delete dialog box
  observeEvent(input[[paste0(delete_prefix, "_hide_dialog")]], r[[delete_variable]] <- FALSE)
  observeEvent(input[[paste0(delete_prefix, "_delete_canceled")]], r[[delete_variable]] <- FALSE)
  
  # When the deletion is confirmed
  observeEvent(input[[paste0(delete_prefix, "_delete_confirmed")]], {
    
    m_tables <- c("patients_options", "widgets_options", "subsets" , "subset_persons",
      "concept", "vocabulary", "domain", "concept_class", "concept_relationship", "concept_relationship_user", "concept_relationship_evals",
      "relationship", "concept_synonym", "concept_ancestor", "drug_strength")
    
    if (table %in% m_tables) db <- m$db
    else db <- r$db
    
    r[[delete_variable]] <- FALSE
    
    # Remove files if this is a plugin, a script, a dataset or a vocabulary
    if (table %in% c("plugins", "scripts", "datasets", "vocabulary", "studies")){
      for (element_id in r[[id_var_r]]){
        element_options <- r$options %>% dplyr::filter(category == get_singular(table), link_id == element_id)
        if (table == "plugins") element_dir <- paste0(r$app_folder, "/", get_plural(table), "/", prefix, "/", element_options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
        else element_dir <- paste0(r$app_folder, "/", get_plural(table), "/", element_options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
        unlink(element_dir, recursive = TRUE)
      }
    }
    
    # Delete row in DB table and associated tables
    
    # If we delete a vocabulary, delete also associated concepts
    if (table == "vocabulary"){
      vocabulary_id <- r$vocabulary %>% dplyr::filter(id == r[[id_var_r]]) %>% dplyr::pull(vocabulary_id)
      sql <- glue::glue_sql("DELETE FROM concept WHERE vocabulary_id = {vocabulary_id}", .con = m$db)
      DBI::dbSendStatement(db, sql) -> query
      DBI::dbClearResult(query)
    }
    
    if (table %in% c("patient_lvl_tabs", "aggregated_tabs")) sql <- glue::glue_sql("UPDATE tabs SET deleted = TRUE WHERE {`id_var_sql`} IN ({r[[id_var_r]]*})" , .con = db)
    else if (table %in% c("patient_lvl_widgets", "aggregated_widgets")) sql <- glue::glue_sql("UPDATE widgets SET deleted = TRUE WHERE {`id_var_sql`} IN ({r[[id_var_r]]*})" , .con = db)
    else sql <- glue::glue_sql("UPDATE {`table`} SET deleted = TRUE WHERE {`id_var_sql`} IN ({r[[id_var_r]]*})" , .con = db)
    DBI::dbSendStatement(db, sql) -> query
    DBI::dbClearResult(query)
    
    sql <- glue::glue_sql("UPDATE options SET deleted = TRUE WHERE category = {get_singular(table)} AND link_id IN ({r[[id_var_r]]*})" , .con = r$db)
    DBI::dbSendStatement(r$db, sql) -> query
    DBI::dbClearResult(query)
    r$options <- r$options %>% dplyr::filter(category != get_singular(table) | (category == get_singular(table) & link_id %not_in% r[[id_var_r]]))
    
    sql <- glue::glue_sql("UPDATE code SET deleted = TRUE WHERE category = {get_singular(table)} AND link_id IN ({r[[id_var_r]]*})" , .con = r$db)
    DBI::dbSendStatement(r$db, sql) -> query
    DBI::dbClearResult(query)
    r$code <- r$code %>% dplyr::filter(category != get_singular(table) | (category == get_singular(table) & link_id %not_in% r[[id_var_r]]))
    
    if (table %not_in% m_tables | table %in% c("vocabulary", "concept_relationship_user")){
      if (length(r_table) > 0) r[[r_table]] <- r[[r_table]] %>% dplyr::filter(get(id_var_sql) %not_in% r[[id_var_r]])
      else r[[table]] <- r[[table]] %>% dplyr::filter(get(id_var_sql) %not_in% r[[id_var_r]])
    }
    else {
      if (length(r_table) > 0) m[[r_table]] <- m[[r_table]] %>% dplyr::filter(get(id_var_sql) %not_in% r[[id_var_r]])
      else m[[table]] <- m[[table]] %>% dplyr::filter(get(id_var_sql) %not_in% r[[id_var_r]]) 
    }
    
    # Notify user
    if (!r_message_bar) show_message_bar(output, delete_message, type = "warning", i18n = i18n, ns = ns)
    if (r_message_bar) r[[paste0(table, "_show_message_bar")]] <- tibble::tibble(message = delete_message, type = "warning", trigger = now())
    
    # Activate reload variable
    if (length(reload_variable) > 0) r[[reload_variable]] <- now()
    
    # Information variable
    if (length(information_variable) > 0) r[[information_variable]] <- r[[id_var_r]]
    
  })
}

#' Execute / test code after edition
#' 
#' @description Execute code entered in the ShinyAce editor
#' @param input Input Shiny variable
#' @param output Output Shiny variable
#' @param session Session Shiny variable
#' @param id ID of the current page / module 
#' @param ns Shiny namespace
#' @param i18n Translator object from shiny.i18n library
#' @param r Shiny reactive value, used to communicate between modules (reactiveValue)
#' @param d Shiny reactive value, used to communicate between modules (reactiveValue)
#' @param m Shiny reactive value, used to communicate between modules (reactiveValue)
#' @param edited_code New code, after editing it (character)
#' @param code_type For plugins page, code_type could be UI or server (character)
#' @param data A list containing dataframes / tibbles, if data need to be used in the evaluated code (list)
#' @examples 
#' \dontrun{
#'  execute_settings_code(output = output, r = r, edited_code = "print('test')")
#' }
execute_settings_code <- function(input, output, session, id = character(), ns = character(), i18n = character(), 
  r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(),
  edited_code = character(), code_type = "", data = list()){
  
  result <- ""
  
  # If code is UI, execute it only
  if (code_type == "ui"){
    
    tryCatch(result <- eval(parse(text = edited_code)), error = function(e) stop(e), warning = function(w) stop(w))
  }
  
  # If code is server, capture the console output
  # Server is also used by R_console page
  if (code_type == "server"){
    
    # Change this option to display correctly tibble in textbox
    options('cli.num_colors' = 1)
    
    # Capture console output of our code
    captured_output <- capture.output(
      tryCatch(eval(parse(text = edited_code)), error = function(e) print(e), warning = function(w) print(w)))
    
    # Restore normal value
    options('cli.num_colors' = NULL)
    
    # Display result
    if (grepl("# A tibble:", toString(captured_output))) paste(captured_output, collapse = "\n") -> result
    else paste(strwrap(captured_output, width = 150), collapse = "\n") -> result
  }
  
  # If code is not UI or server, capture the console output after replacing %% values
  if (code_type %not_in% c("ui", "server")){
    
    # Replace %CODE% from code to real values
    code <- edited_code %>%
      stringr::str_replace_all("%dataset_id%", as.character(isolate(r$dataset_id))) %>%
      stringr::str_replace_all("%subset_id%", as.character(isolate(r$subset_id))) %>%
      stringr::str_replace_all("%vocabulary_id%", as.character(isolate(r$vocabulary_id)))
    
    # Get OMOP version
    if (!is.na(r$dataset_id)){
      omop_version <- r$options %>% dplyr::filter(category == "dataset" & name == "omop_version" & link_id == r$dataset_id) %>% dplyr::pull(value)
      code <- code %>% stringr::str_replace_all("%omop_version%", paste0("'", omop_version, "'"))
    }
    
    # Change this option to display correctly tibble in textbox
    options('cli.num_colors' = 1)
    
    # Capture console output of our code
    captured_output <- capture.output(
      tryCatch(eval(parse(text = as.character(code))), error = function(e) print(e), warning = function(w) print(w)))
    
    # Restore normal value
    options('cli.num_colors' = NULL)
    
    # captured_output <- captured_output %>% stringr:: str_replace_all("<simpleError", "&lt;simpleError")
    
    # Display result
    if (grepl("# A tibble:", toString(captured_output))) paste(captured_output, collapse = "\n") -> result
    else paste(strwrap(captured_output, width = 150), collapse = "\n") -> result
  }
  
  result
}

#' Monitor performances
#' 
#' @description Monitor performances of the ShinyApp, with calculating time each step takes
#' @param r Shiny reactive value, used to communicate between modules (reactiveValue)
#' @param action If the function is called at the beginning of the end of a task, takes "start" or "stop" values (character)
#' @param task Name of the task (character)
#' @examples 
#' \dontrun{
#'  monitor_perf(r = r, action = "start", task = "Load database")
#' }
monitor_perf <- function(r = shiny::reactiveValues(), action = "stop", task = character()){
  
  # if (!r$perf_monitoring) return()
  if (action == "start") datetime_start <<- now()
  
  if (action == "stop"){
    datetime_stop <<- now()
    
    r$perf_monitoring_table <- 
      isolate(r$perf_monitoring_table) %>% 
      dplyr::bind_rows(tibble::tribble(
        ~task, ~datetime_start, ~datetime_stop, 
        task, datetime_start, datetime_stop))
    
    datetime_start <<- now() 
  }
}

#' Prepare data for datatable
#' 
#' @description Prepare data to be used in a DT datable
#' @param output Shiny output value, to show message bars
#' @param r Shiny reactive value, used to communicate between modules (reactiveValue)
#' @param ns Shiny namespace
#' @param i18n i18n Translator object from shiny.i18n library
#' @param id ID of the current page / module 
#' @param table Table of the database containing the data (character)
#' @param dropdowns Names of the dropdowns displayed in the datatable (character)
#' @param dropdowns_multiselect Names of the dropdowns which have the multiSelect argument set to TRUE (character)
#' @param dropdowns_null_value Names of the dropdowns where a null value is added at the beginning (character)
#' @param factorize_cols Names of the columns which will be factorized, to be sortable (character)
#' @param action_buttons Names of the action buttons needed (character)
#' @param data_input The data which will be transformed (tibble)
#' @examples 
#' \dontrun{
#'  prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = "settings_users",
#' table = "users", dropdowns = "user_status", action_buttons = "delete", data_input = r$users_temp)
#' }
prepare_data_datatable <- function(output, r = shiny::reactiveValues(), ns = character(), i18n = character(), id = character(),
  table = character(), dropdowns = character(), dropdowns_multiselect = character(), dropdowns_null_value = character(), factorize_cols = character(),
  action_buttons = character(), data_input = tibble::tibble()){

  # Initiate data_output, starting from data_input
  data_output <- data_input
  
  # Add tab family column for tabs elements
  if (grepl("widgets", table)){
    if (grepl("patient_lvl", table)) prefix <- "patient_lvl"
    if (grepl("aggregated", table)) prefix <- "aggregated"
    data_output <- data_output %>% dplyr::left_join(r[[paste0(prefix, "_tabs")]] %>%
        dplyr::select(tab_id = id, tab_group_id), by = "tab_id") %>% dplyr::relocate(tab_group_id, .after = name)
  }
  
  # Add a column action in the DataTable
  if (length(action_buttons) != 0) data_output["action"] <- NA_character_
  
  # Dropdowns is a named character vector, with names corresponding to column names (eg data_source_id)
  # and values corresponding to data_var / data variables names (eg data_sources)
  
  # Transform dropdowns columns in the dataframe to character
  if (length(dropdowns) != 0) lapply(names(dropdowns), function(col_name) data_output %>% dplyr::mutate_at(col_name, as.character) ->> data_output)
  
  # For each row of the dataframe :
  # - transform dropdowns columns to show dropdowns in Shiny app
  # - add an Action column with delete action button (+/- options / edit code buttons)
  # - show creator name
  
  # Loop over data only if necessary
  # Not necessary if no dropdowns, no action_buttons & no creator_id col
  if (length(dropdowns) != 0 | length(action_buttons) != 0 | "creator_id" %in% names(data_output)){
    
    for (i in 1:nrow(data_output)){
      
      # --- --- --- --
      # Dropdowns ----
      # --- --- --- --
      
      if (length(dropdowns) != 0){
        lapply(names(dropdowns), function(name){
          
          # name here is like "data_source_id"
          # dropdowns[name] here is like "data_sources"
          # so r[[dropdowns[[name]]]] is like r$data_sources, var containing data_sources data
          
          multiSelect = FALSE
          null_value <- FALSE
          
          # Put a null value in dropdowns
          if (name %in% dropdowns_null_value) null_value <- TRUE
          
          # If this is a multiselect dropdown, split results to integers
          if (name %in% dropdowns_multiselect){
            
            multiSelect <- TRUE
            value <- NULL
            if (!(TRUE %in% grepl("[a-zA-Z]", stringr::str_split(data_output[[i, name]], ", ") %>% unlist()))){
              value <- stringr::str_split(data_output[[i, name]], ", ") %>% unlist() %>% as.integer()
            }
          }
          else value <- as.integer(data_output[[i, name]])
          
          options <- convert_tibble_to_list(data = r[[dropdowns[[name]]]], key_col = "id", text_col = "name", null_value = null_value, i18n = i18n) 
          
          # For dropdown parent_tab in patient_lvl & aggregated_tabs, need to select only tabs depending on the same tab family
          
          if (dropdowns[[name]] %in% c("patient_lvl_tabs", "aggregated_tabs")){
            options <- convert_tibble_to_list(data = r[[dropdowns[[name]]]] %>% dplyr::filter(tab_group_id == data_output[[i, "tab_group_id"]]),
              key_col = "id", text_col = "name", null_value = null_value)
          }
          
          data_output[i, name] <<- as.character(
            div(
              # ID is like "data_sources13" if ID = 13
              shiny.fluent::Dropdown.shinyInput(ns(paste0(dropdowns[[name]], data_output[[i, "id"]])), options = options, value = value, multiSelect = multiSelect),
              # On click, we set variable "dropdown_updated" to the ID of the row (in our example, 13)
              onclick = paste0("Shiny.setInputValue('", id, "-dropdown_updated', '", paste0(dropdowns[[name]], data_output[[i, "id"]]), "', {priority: 'event'})"),
              style = "width:200px")
          )
          
        })
      }
      
      # --- --- --- --- ---
      # Action buttons ----
      # --- --- --- --- ---
      
      # Action buttons : if in action_buttons vector, add action button
      actions <- tagList()
      
      # Add add_item button
      if ("add" %in% action_buttons){
        actions <- tagList(actions,
          actionButton(paste0("add_item_", data_output[i, "id"]), "", icon = icon("plus"),
            onclick = paste0("Shiny.setInputValue('", id, "-add_item", "', this.id, {priority: 'event'})")), "")}
      
      # Add options button
      if ("options" %in% action_buttons){
        actions <- tagList(actions,
          actionButton(paste0("options_", data_output[i, "id"]), "", icon = icon("cog"),
            onclick = paste0("Shiny.setInputValue('", id, "-options", "', this.id, {priority: 'event'})")), "")}
      
      # Add edit code button
      if ("edit_code" %in% action_buttons){
        actions <- tagList(actions,
          actionButton(paste0("edit_code_", data_output[i, "id"]), "", icon = icon("file-code"),
            onclick = paste0("Shiny.setInputValue('", id, "-edit_code", "', this.id, {priority: 'event'})")), "")}
      
      # Add sub datatable button
      if ("sub_datatable" %in% action_buttons){
        actions <- tagList(actions,
          actionButton(paste0("sub_datatable_", data_output[i, "id"]), "", icon = icon("table"),
            onclick = paste0("Shiny.setInputValue('", id, "-sub_datatable", "', this.id, {priority: 'event'})")), "")}
      
      # Add delete button
      if ("delete" %in% action_buttons){
        
        # If row is deletable (we havn't made a function argument for deletable or not, only default subsets are not deletable)
        # Could be changed later
        
        delete <- actionButton(paste0("delete_", data_output[i, "id"]), "", icon = icon("trash-alt"),
          onclick = paste0("Shiny.setInputValue('", id, "-deleted_pressed', this.id, {priority: 'event'})"))
        
        actions <- tagList(actions, delete)
      }
      
      # Update action column in dataframe
      if (length(action_buttons) != 0) data_output[i, "action"] <- as.character(div(actions))
      
      # --- --- --- --- -
      # Creator name ----
      # --- --- --- --- -
      
      if ("creator_id" %in% names(data_output)){
        if (nrow(r$users %>% dplyr::filter(id == data_output[[i, "creator_id"]])) > 0){
          data_output[i, "creator_id"] <-
            r$users %>% dplyr::filter(id == data_output[[i, "creator_id"]]) %>%
            dplyr::mutate(creator = paste0(firstname, " ", lastname)) %>%
            dplyr::pull(creator)
        }
        else data_output[i, "creator_id"] <- i18n$t("deleted_user")
      }
      
      # Get names for other columns if there are not dropdowns
      
      cols <- c("data_source_id" = "data_sources", "dataset_id" = "datasets", "study_id" = "studies", "tab_type_id" = "tab_types")
      sapply(names(cols), function(name){
        if (name %in% names(data_output) & name %not_in% names(dropdowns)){
          row_id <- data_output[[i, name]]
          if (length(row_id) > 0) result <- r[[cols[[name]]]] %>% dplyr::filter(id == as.integer(row_id)) %>% dplyr::pull(name)
          if (length(result) == 0) result <- ""
          data_output[[i, name]] <<- result
        }
      })
      
      cols <- c("tab_group_id" = "tabs_groups", "tab_id" = "tabs", "plugin_id" = "plugins")
      sapply(names(cols), function(name){
        if (name %in% names(data_output) & name %not_in% names(dropdowns)){
          if (grepl("patient_lvl", table)) prefix <- "patient_lvl_"
          if (grepl("aggregated", table)) prefix <- "aggregated_"
          if (name == "plugin_id") prefix <- ""
          
          row_id <- data_output[[i, name]]
          if (length(row_id) > 0) result <- r[[paste0(prefix, cols[[name]])]] %>% dplyr::filter(id == as.integer(row_id)) %>% dplyr::pull(name)
          if (length(result) == 0) result <- ""
          data_output[[i, name]] <<- result
        }
      })
    }
  }
  
  # Factorize cols
  
  data_output <- data_output %>% dplyr::mutate_at(factorize_cols, as.factor)
  
  data_output
}

# prepare_data_shiny_tree <- function(data = tibble::tibble(), stopened = FALSE){
#   full_list <- list()
#   
#   if (nrow(data) > 0){
#     
#     # Loop over categories levels
#     for (i in 1:(max(data$level))){
#       
#       if (i == 1){
#         
#         current_lvl_categories <- data %>% dplyr::filter(level == 1)
#         
#         # Loop over categories
#         for (j in 1:nrow(current_lvl_categories)){
#           
#           row <- current_lvl_categories[j, ]
#           
#           new_list <- list(structure("", stid = row$item_id, sttype = "default", stopened = stopened))
#           names(new_list) <- row$name
#           full_list <- append(full_list, new_list)
#         }
#       }
#       
#       else {
#         
#         filtered_categories <- data %>% dplyr::filter(level == i)
#         
#         # Create path column
#         paths <- data %>% 
#           dplyr::filter(level == i) %>%
#           dplyr::pull(path)
#         
#         # Loop over paths
#         for (j in 1:length(paths)){
#           
#           path <- paths[j]
#           
#           # Select data with the same path
#           same_path_categories <- filtered_categories %>% dplyr::filter(path == !!path)
#           
#           new_list <- list()
#           
#           for (k in 1:nrow(same_path_categories)){
#             
#             row <- same_path_categories[k, ]
#             new_list_child <- list(structure("", stid = row$item_id, sttype = "default", stopened = stopened))
#             names(new_list_child) <- paste0(row$name, " (", row$count_persons_rows, " | ", row$count_concepts_rows, ")")
#             
#             new_list <- new_list %>% append(new_list_child)
#           }
#           
#           eval(parse(text = paste0("full_list$", path, " <- structure(
#             sttype = 'default', stid = attributes(full_list$", path, ")$stid, stopened = ", stopened, ", new_list)")))
#         }
#       }
#     }
#   }
#   
#   full_list
# }

#' Save code edition
#' 
#' @description Save code in code table after editing it
#' @param output Shiny output variable
#' @param r Shiny reactive value, used to communicate between modules (reactiveValue)
#' @param id ID of the current page, format = "settings_[PAGE]" (character)
#' @param category Category column in code table, eg : "dataset", "plugin" (character)
#' @param code_id_input Input of the actionButton containing ID of current row, in datatable, format = "edit_code_[ID]" (character)
#' @param edited_code New code, after editing it (character)
#' @param i18n Translator object from shiny.i18n library
#' @examples
#' \dontrun{
#' save_settings_code(output = output, r = r, id = "settings_dataset", category = "dataset", code_id_input = "edit_code_5",
#'   edited_code = "print('test code edition')", i18n = i18n)
#' }
save_settings_code <- function(output, r = shiny::reactiveValues(), id = character(), category = character(),
  code_id_input = integer(), edited_code = character(), i18n = character()){
  
  # Get link_id variable to update code table
  link_id <- as.integer(substr(code_id_input, nchar("edit_code_") + 1, nchar(code_id_input)))
  
  # Reload r$code before querying
  # update_r(r = r, table = "options")
  code_id <- r$code %>% dplyr::filter(category == !!category, link_id == !!link_id) %>% dplyr::pull(id) %>% as.integer()
  
  # Replace ' with '' and store in the database
  edited_code <- stringr::str_replace_all(edited_code, "'", "''")
  sql <- glue::glue_sql("UPDATE code SET code = {edited_code} WHERE id = {code_id}", .con = r$db)
  query <- DBI::dbSendStatement(r$db, sql)
  DBI::dbClearResult(query)
  # r$code <- DBI::dbGetQuery(r$db, "SELECT * FROM code WHERE deleted IS FALSE ORDER BY id")
  r$code <- r$code %>% dplyr::mutate(
    code = dplyr::case_when(
      id == code_id ~ edited_code,
      TRUE ~ code
    ))
  
  # Notify user
  show_message_bar(output,  "modif_saved", "success", i18n = i18n, ns = shiny::NS(id))
}

#' Save options
#' 
#' @description Save options in settings pages
#' @param output Shiny output variable
#' @param r Shiny reactive value, used to communicate between modules (reactiveValue)
#' @param id ID of the current page, format = "settings_[PAGE]" (character)
#' @param category Category column in code table, eg : "dataset", "plugin" (character)
#' @param code_id_input Input of the actionButton containing ID of current row, in datatable, format = "edit_code_[ID]" (character)
#' @param data New data to store in options table (list)
#' @param i18n Translator object from shiny.i18n library
#' @param page_options Which options need to be saved (character)
#' @examples
#' \dontrun{
#' data <- list()
#' data$show_only_aggregated_data <- TRUE
#' data$users_allowed_read <- c(1, 3, 4)
#' save_settings_options(output = output, r = r, id = "settings_dataset", category = "dataset", code_id_input = "edit_code_3",
#'   data = data, i18n = i18n, page_options = c("show_only_aggregated_data", "users_allowed_read"))
#' }
save_settings_options <- function(output, r = shiny::reactiveValues(), id = character(), category = character(),
  code_id_input = integer(), data = data, i18n = character(), page_options = character()){
  
  ns <- shiny::NS(id)
  
  # Get link_id variable to update code table
  link_id <- as.integer(substr(code_id_input, nchar("options_") + 1, nchar(code_id_input)))
  
  # Get options with category & link_id
  options <- r$options %>% dplyr::filter(category == !!category, link_id == !!link_id)
  
  # if (nrow(options) == 0) show_message_bar(output,  "modif_saved", "success", i18n = i18n, ns = ns)
  # req (nrow(options) > 0)
  
  if (nrow(options) > 0){
    if("show_only_aggregated_data" %in% page_options){
      option_id <- options %>% dplyr::filter(name == "show_only_aggregated_data") %>% dplyr::pull(id)
      sql <- glue::glue_sql("UPDATE options SET value_num = {as.numeric(data$show_only_aggregated_data)} WHERE id = {option_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      r$options <- r$options %>% dplyr::mutate(value_num = dplyr::case_when(id == option_id ~ as.numeric(data$show_only_aggregated_data), TRUE ~ value_num))
    }
    
    if ("users_allowed_read" %in% page_options){
      
      # Save users_allowed_read_group value (everybody or people_picker)
      # Don't need to delete each users allowed if you change from "choose people" to "everybody"
      option_id <- options %>% dplyr::filter(name == "users_allowed_read_group") %>% dplyr::pull(id)
      sql <- glue::glue_sql("UPDATE options SET value = {data$users_allowed_read_group} WHERE id = {option_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      r$options <- r$options %>% dplyr::mutate(value = dplyr::case_when(id == option_id ~ data$users_allowed_read_group, TRUE ~ value))
      
      # The idea is to delete every rows of options for this tab, and then reinsert one row per user
      # Get unique ID (peoplePicker can select twice a user, if he's already checked at the initiation of the input)
      
      # Delete all users allowed in the options table
      rows_to_del <- options %>% dplyr::filter(name == "user_allowed_read") %>% dplyr::pull(id)
      sql <- glue::glue_sql("DELETE FROM options WHERE id IN ({rows_to_del*})", .con = r$db)
      DBI::dbSendStatement(r$db, sql) -> query
      DBI::dbClearResult(query)
      r$options <- r$options %>% dplyr::filter(id %not_in% rows_to_del)
      
      # Add users in the selected list
      if (length(data$users_allowed_read) != 0){
        data$users_allowed_read <- unique(data$users_allowed_read)
        
        if (length(data$users_allowed_read) == 1){
          if (data$users_allowed_read == "everybody") value_num <- NA_real_ 
          else value_num <- as.numeric(data$users_allowed_read)
        } 
        else value_num <- as.numeric(data$users_allowed_read)
        
        last_row <- get_last_row(r$db, "options")
        new_data <- tibble::tibble(id = (last_row + (1:length(data$users_allowed_read))), category = category, link_id = link_id,
          name = "user_allowed_read", value = "", value_num = value_num, creator_id = r$user_id,
          datetime = now(), deleted = FALSE)
        DBI::dbAppendTable(r$db, "options", new_data)
        r$options <- r$options %>% dplyr::bind_rows(new_data)
      }
    }
  }
  
  for (field in c("markdown_description", "version", "author", "image", "omop_version", "activate_scripts_cache",
    paste0("name_", r$languages$code), paste0("category_", r$languages$code), paste0("description_", r$languages$code))){
    if (field %in% page_options){
      option_id <- options %>% dplyr::filter(name == field) %>% dplyr::pull(id)
      new_value <- stringr::str_replace_all(data[[field]], "'", "''")
      
      sql <- glue::glue_sql("SELECT id FROM options WHERE id = {option_id}", .con = r$db)
      
      if (DBI::dbGetQuery(r$db, sql) %>% nrow() == 0){
        
        # Create field if do not already exist
        
        option_id <- get_last_row(r$db, "options") + 1
        new_data <- tibble::tibble(
          id = option_id, category = !!category, link_id = !!link_id,
          name = field, value = new_value, value_num = NA_real_, creator_id = r$user_id, datetime = now(), deleted = FALSE)
        DBI::dbAppendTable(r$db, "options", new_data)
        r$options <- r$options %>% dplyr::bind_rows(new_data)
      }
      else {
        sql <- glue::glue_sql("UPDATE options SET value = {new_value} WHERE id = {option_id}", .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql)
        DBI::dbClearResult(query)
        r$options <- r$options %>% dplyr::mutate(value = dplyr::case_when(id == option_id ~ new_value, TRUE ~ value))
      }
    }
  }
  
  show_message_bar(output, "modif_saved", "success", i18n = i18n, ns = ns)
}

#' Save updates in datatable
#' 
#' @description Save updates in a datatable in settings pages
#' @param output Shiny output variable
#' @param r Shiny reactive value, used to communicate between modules (reactiveValue)
#' @param m Shiny reactive value, used to communicate between modules (reactiveValue)
#' @param ns Shiny namespace
#' @param table Name of the database table (character)
#' @param r_table Name of the r variable containing the data (character)
#' @param duplicates_allowed Are duplicates in the name column allowed (logical)
#' @param i18n Translator object from shiny.i18n library
#' @param r_message_bar If the MessageBar is displayed with a r variable or with show_message_bar function (TRUE for r variable)
#' @examples 
#' \dontrun{
#' save_settings_datatable_updates(output = output, r = r, ns = ns, table = "datasets", 
#'   r_table = "datasets", duplicates_allowed = FALSE, i18n = i18n, r_message_bar = FALSE)
#' }
save_settings_datatable_updates <- function(output, r = shiny::reactiveValues(), m = shiny::reactiveValues(), ns = character(), 
  table = character(), r_table = character(), duplicates_allowed = FALSE, i18n = character(), r_message_bar = FALSE){
  
  m_tables <- c("patients_options", "widgets_options", "subsets" , "subset_persons",
    "concept", "concept_user", "vocabulary", "domain", "concept_class", "concept_relationship", "relationship", "concept_synonym", "concept_ancestor", "drug_strength")

  if (table %in% m_tables) db <- m$db
  else db <- r$db
  
  # Make sure there's no duplicate in names, if duplicates_allowed is set to FALSE
  
  # If r_table is different than table
  if (length(r_table) == 0) r_table <- table
  
  if (!duplicates_allowed){

    duplicates_name <- 0
    duplicates_display_order <- 0
    tab_is_its_own_parent <- 0
    loop_over_tabs <- 0
    
    # For tabs tables (patient_lvl & aggregated, tabs / tabs_groups / widgets)
    # Duplicates names are grouped (by family for tabs, by tab for tabs elements)
    # It's the same with the display order

    if (grepl("tabs", table)){
      if (table %in% c("patient_lvl_tabs", "aggregated_tabs")){
        
        duplicates_name <- r[[paste0(table, "_temp")]] %>% dplyr::mutate_at("name", tolower) %>%
          dplyr::group_by(tab_group_id, name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
        
        duplicates_display_order <- r[[paste0(table, "_temp")]] %>%
          dplyr::group_by(tab_group_id, parent_tab_id, display_order) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
        
        # A tab cannot be its own parent (infinite loop...)
        tab_is_its_own_parent <- r[[paste0(table, "_temp")]] %>% dplyr::filter(id == parent_tab_id) %>% nrow()
        
        if (tab_is_its_own_parent == 0) loop_over_tabs <- r[[paste0(table, "_temp")]] %>% dplyr::filter(!is.na(parent_tab_id)) %>%
          dplyr::left_join(r[[paste0(table, "_temp")]] %>% dplyr::select(parent_tab_id = id, parent_tab_id_bis = parent_tab_id), by = "parent_tab_id") %>%
          dplyr::filter(id == parent_tab_id_bis) %>% nrow()
      }
      
      if (table == "aggregated_widgets"){
        duplicates_name <- r[[paste0(table, "_temp")]] %>% dplyr::mutate_at("name", tolower) %>%
          dplyr::group_by(tab_id, name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
        
        duplicates_display_order <- r[[paste0(table, "_temp")]] %>%
          dplyr::group_by(tab_id, display_order) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
      }
    }
    
    else if (table == "users") duplicates_name <- r[[paste0(r_table, "_temp")]] %>% dplyr::mutate_at("username", tolower) %>%
      dplyr::group_by(username) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
    
    else if (table == "vocabulary") duplicates_name <- r[[paste0(r_table, "_temp")]] %>%
      dplyr::group_by(vocabulary_id) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
    
    # else if (table == "git_repos") duplicates_name <- r$git_repos_temp %>% dplyr::mutate_at("name", tolower) %>%
    #   dplyr::group_by(category, name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
    
    else {
      if (table %in% m_tables) duplicates_name <- m[[paste0(r_table, "_temp")]] %>% dplyr::mutate_at("name", tolower) %>%
          dplyr::group_by(name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow() 
      else duplicates_name <- r[[paste0(r_table, "_temp")]] %>% dplyr::mutate_at("name", tolower) %>%
        dplyr::group_by(name) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow() 
    }
    
    if (duplicates_display_order > 0){
      if (!r_message_bar) show_message_bar(output, "modif_display_order_duplicates", "severeWarning", i18n, ns = ns)
      if (r_message_bar) r[[paste0(table, "_show_message_bar")]] <- tibble::tibble(message = "modif_display_order_duplicates", type = "severeWarning", trigger = now())
    } 
    if (tab_is_its_own_parent > 0){
      if (!r_message_bar) show_message_bar(output, "tab_cannot_be_its_own_parent", "severeWarning", i18n, ns = ns)
      if (r_message_bar) r[[paste0(table, "_show_message_bar")]] <- tibble::tibble(message = "tab_cannot_be_its_own_parent", type = "severeWarning", trigger = now())
    }
    if (loop_over_tabs > 0){
      if (!r_message_bar) show_message_bar(output, "tab_loop_between_tabs", "severeWarning", i18n, ns = ns)
      if (r_message_bar) r[[paste0(table, "_show_message_bar")]] <- tibble::tibble(message = "tab_loop_between_tabs", type = "severeWarning", trigger = now())
    }
    if (duplicates_name > 0){
      if (!r_message_bar) show_message_bar(output, "modif_names_duplicates", "severeWarning", i18n, ns = ns)
      if (r_message_bar) r[[paste0(table, "_show_message_bar")]] <- tibble::tibble(message = "modif_names_duplicates", type = "severeWarning", trigger = now())
    }
    
    req(duplicates_name == 0, duplicates_display_order == 0, tab_is_its_own_parent == 0, loop_over_tabs == 0)
  }
  
  names_empty <- 0
  if (table == "users") names_empty <- r[[paste0(r_table, "_temp")]] %>% dplyr::filter(username == "") %>% nrow()
  else if (table == "vocabulary") names_empty <- r[[paste0(r_table, "_temp")]] %>% dplyr::filter(vocabulary_id == "") %>% nrow()
  else if (table == "concept_user") names_empty <- r[[paste0(r_table, "_temp")]] %>% dplyr::filter(concept_name == "") %>% nrow()
  else if (table %in% m_tables) names_empty <- m[[paste0(r_table, "_temp")]] %>% dplyr::filter(name == "") %>% nrow()
  else names_empty <- r[[paste0(r_table, "_temp")]] %>% dplyr::filter(name == "") %>% nrow()

  if (names_empty > 0){
    if (!r_message_bar) show_message_bar(output, "names_empty", "severeWarning", i18n, ns = ns)
    if (r_message_bar) r[[paste0(table, "_show_message_bar")]] <- tibble::tibble(message = "names_empty", type = "severeWarning", trigger = now())
  }
  
  req(names_empty == 0)
  
  # Save changes in database
  if (table %not_in% m_tables | table == "vocabulary") ids_to_del <- r[[paste0(r_table, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::pull(id)
  else if (table == "concept_user"){
    
    concept_ids <- r$dataset_vocabulary_concept_user_temp %>% dplyr::pull(concept_id) %>% as.integer()
    sql <- glue::glue_sql("SELECT * FROM concept_user WHERE concept_id IN ({concept_ids*}) AND user_id = {r$user_id}", .con = db)
    ids_to_del <- DBI::dbGetQuery(db, sql) %>% dplyr::pull(id)
  }
  else ids_to_del <- m[[paste0(r_table, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::pull(id)

  if (length(ids_to_del) == 0 & table != "concept_user"){
    if (!r_message_bar) show_message_bar(output,  "modif_saved", "success", i18n, ns = ns)
    if (r_message_bar) r[[paste0(table, "_show_message_bar")]] <- tibble::tibble(message = "modif_saved", type = "success", trigger = now())
  }

  req(length(ids_to_del) > 0 | table == "concept_user")

  sql <- glue::glue_sql("DELETE FROM {`table`} WHERE id IN ({ids_to_del*})", .con = db)
  DBI::dbSendStatement(db, sql) -> query
  DBI::dbClearResult(query)

  # If action in columns, remove before insert into database
  # Same with count_concepts_rows (and count_persons_rows, always with count_concepts_rows)
  if (table %not_in% m_tables | table %in% c("vocabulary", "concept_user")) data <- r[[paste0(r_table, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::select(-modified)
  else data <- m[[paste0(r_table, "_temp")]] %>% dplyr::filter(modified) %>% dplyr::select(-modified)
  
  if ("action" %in% names(data)) data <- data %>% dplyr::select(-action)
  if ("count_concepts_rows" %in% names(data)) data <- data %>% dplyr::select(-count_concepts_rows, -count_persons_rows)
  
  DBI::dbAppendTable(db, table, data)

  # Reload r variable
  if (table %not_in% m_tables | table %in% c("vocabulary", "concept_user")) r[[r_table]] <- r[[paste0(r_table, "_temp")]] %>% dplyr::select(-modified)
  else m[[r_table]] <- m[[paste0(r_table, "_temp")]] %>% dplyr::select(-modified)

  # Notify user
  if (!r_message_bar) show_message_bar(output,  "modif_saved", "success", i18n, ns = ns)
  if (r_message_bar) r[[paste0(table, "_show_message_bar")]] <- tibble::tibble(message = "modif_saved", type = "success", trigger = now())
}

#' Show or hide cards
#' 
#' @description Show or hide cards depending of selected shiny.fluent::PivotItem
#' @param r Shiny reactive value, used to communicate between modules (reactiveValue)
#' @param session Shiny session variable
#' @param input Shiny input variable
#' @param table Name of the database table (character)
#' @param id Name of the ID of the page (character)
#' @param cards Character vector containing names of current page cards (character)
#' @examples 
#' \dontrun{
#' cards <- c("subsets_datatable_card", "subsets_persons_card", "subsets_edit_code_card")
#' show_or_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
#' }
show_or_hide_cards <- function(r = shiny::reactiveValues(), session, input, table = character(), id = character(), cards = character()){
    
  # If user has access, show or hide card when Pivot is clicked
  observeEvent(input$current_tab, {
    
    if (length(table > 0)) card_user_access <- paste0(get_plural(table), "_", input$current_tab)
    else card_user_access <- input$current_tab
    
    sapply(cards %>% setdiff(., input$current_tab), shinyjs::hide)
    sapply(cards %>% setdiff(., input$current_tab), function(card) shinyjs::hide(paste0(card, "_forbidden")))
    
    if (card_user_access %in% r$user_accesses) shinyjs::show(input$current_tab)
    else shinyjs::show(paste0(input$current_tab, "_forbidden"))
  })
}

#' Update datatable
#' 
#' @description Update data in r variable when an update is made in the datatable
#' @param input Shiny input variable
#' @param r Shiny reactive value, used to communicate between modules (reactiveValue)
#' @param ns Shiny namespace
#' @param table Name of the database table used (character)
#' @param dropdowns Dropdowns displayed on datatable (character)
#' @param i18n Translator object from shiny.i18n library
#' @examples 
#' \dontrun{
#' update_settings_datatable(r = r, ns = ns, table = "datasets", dropdowns = "data_source", i18n = i18n)
#' }
update_settings_datatable <- function(input, tab_id = character(), r = shiny::reactiveValues(), ns = character(), 
  table = character(), dropdowns = character(), i18n = character()){
  
  sapply(r[[table]] %>% dplyr::pull(id), function(id){
    sapply(dropdowns, function(dropdown){
      
      observer_name <- paste0(get_plural(word = dropdown), id)
      
      # Load the observer only once
      if (paste0(tab_id, "-", observer_name) %not_in% r$loaded_observers){
        observeEvent(input[[observer_name]], {
          
          dropdown_table <- dropdown
          dropdown_input <- dropdown
          
          # When we load a page, every dropdown triggers the event
          # Change temp variable only if new value is different than old value
          old_value <- r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), paste0(dropdown_table, "_id")]]
          new_value <- NA_integer_
          
          # If vocabulary, data_source_id can accept multiple values (converting to string)
          if (table == "vocabulary") new_value <- toString(as.integer(input[[paste0("data_sources", id)]]))
          if (table %in% c("data_sources", "datasets", "studies", "subsets", "plugins", "users", "patient_lvl_tabs", "aggregated_tabs")){
            new_value <- coalesce2("int", input[[paste0(get_plural(word = dropdown_input), id)]])
          }
          
          if (length(new_value) > 0 & length(old_value) > 0){
            if (new_value == "" | is.na(new_value)){
              r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), paste0(dropdown_table, "_id")]] <- NA_integer_
              r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), "modified"]] <- TRUE
            }
            else if (old_value == "" | is.na(old_value) | new_value != old_value){
              r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), paste0(dropdown_table, "_id")]] <- new_value
              r[[paste0(table, "_temp")]][[which(r[[paste0(table, "_temp")]]["id"] == id), "modified"]] <- TRUE
            }
          }
        }, ignoreInit = TRUE)
        
        # Keep trace that this observer has been loaded
        # r$loaded_observers <- c(r$loaded_observers, paste0(tab_id, "-", observer_name))
      }
    })
  })
}

#' Capture python output
#'
#' @description A function to get Python console result
#' @param code Python code that you want to run (character)
capture_python_output <- function(code){
  
  # Clear the global environment in Python
  reticulate::py_run_string("globals().clear()")
  
  # Parameters to get console output
  reticulate::py_run_string(paste0(
    "import sys\n",
    "from io import StringIO\n",
    "original_stdout = sys.stdout\n",
    "original_stderr = sys.stderr\n",
    "sys.stdout = StringIO()\n",
    "sys.stderr = sys.stdout\n"
  ))
  
  # Run python code
  reticulate::py_run_string(code)
  
  # Get console output
  reticulate::py_run_string(paste0(
    "sys.stdout.seek(0)\n",
    "output = sys.stdout.getvalue()\n",
    "sys.stdout = original_stdout\n",
    "sys.stderr = original_stderr\n"
  ))
  
  reticulate::py$output
}
