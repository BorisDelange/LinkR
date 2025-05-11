#' @noRd
add_widget_to_gridstack <- function(id, tab_id, ui_output, widget_id, previous_widget_id = NA_integer_){
  ns <- NS(id)
  
  if (id == "plugins") delete_previous_widget <- paste0(
    "var previous_widget = grid.el.querySelector('#", ns(paste0("plugins_gridstack_item_", previous_widget_id)), "');
    if (previous_widget) grid.removeWidget(previous_widget);"
  )
  else delete_previous_widget <- ""
  
  shinyjs::runjs(paste0("
    function addWidget(tab_id) {
      
      var grid = window.gridStackInstances[tab_id];
      
      if (grid) {
        // Remove previous widget
        ", delete_previous_widget, "
        
        // Add new widget
        grid.addWidget(`", ui_output, "`);
        
        // Load react components
        $(document).on('shiny:idle', function(event) {
          document.querySelectorAll('.react-container').forEach(container => {
            const reactDataScript = container.querySelector('.react-data');
            if (reactDataScript) {
              jsmodule['@/shiny.react'].findAndRenderReactData();
            }
          });
        });
        
        // Rebind Shiny components
        setTimeout(function() {
          $('#", id, "-", id, "_gridstack_item_", widget_id, " *').each(function() {
            Shiny.unbindAll(this);
            Shiny.bindAll(this);
          });
        }, 100);
      } else {
        // Retry after 500 ms
        setTimeout(function() {
          addWidget(tab_id);
        }, 500);
      }
    }
    
    addWidget('", tab_id, "');
  "))
}

#' @noRd
create_gridstack_instance <- function(id, tab_id){
  ns <- NS(id)
  
  shinyjs::delay(200,
    shinyjs::runjs(paste0("
      function initializeGridStack() {

        const totalHeight = window.innerHeight - 120;
        const numRows = 40;
        const cellHeight = totalHeight / numRows;

        if (!window.gridStackInstances['", tab_id, "']) {
          const grid = GridStack.init({
            cellHeight: cellHeight + 'px',
            //maxRow: 40,
            scroll: false,
            column: 12,
            staticGrid: true,
            float: false,
            resizable: { handles: 'se, ne, nw, sw' },
            margin: 10
          }, '#", ns(paste0("gridstack_", tab_id)), "');

          window.gridStackInstances['", tab_id, "'] = grid;
        } else {
          window.gridStackInstances['", tab_id, "'].cellHeight(cellHeight);
        }
      }

      initializeGridStack();

      window.addEventListener('resize', function() {
        initializeGridStack();
      });
    "))
  )
}

#' @noRd
create_widget <- function(id, widget_id, ui_code, w = 6, h = 20, x = 0, y = 0){
  ns <- NS(id)
  
  if (is.na(w) | w == 0) w <- 6
  if (is.na(h) | h == 0) h <- 20
  if (is.na(x)) x <- 0
  if (is.na(y)) y <- 0
  
  div(
    id = ns(paste0(id, "_gridstack_item_", widget_id)),
    class = "grid-stack-item",
    `gs-w` = w, `gs-h` = h, `gs-x` = x, `gs-y` = y,
    div(
      class = "grid-stack-item-content",
      div(
        uiOutput(ns(paste0("ui_", widget_id)), style = "width: 100%; height: 100%;"),
        uiOutput(ns(paste0("edit_buttons_", widget_id))),
        class = "data_widget"
      )
    )
  )
}

#' Query OMOP data from a database connection
#'
#' This function executes SQL queries against an OMOP CDM database using the connection
#' stored in the global 'd' object (d$con).
#'
#' @param query A string containing the SQL query to execute
#'
#' @return A tibble containing the query results
#'
#' @examples
#' get_query("SELECT * FROM person")
#'
#' @export
get_query <- function(query) {
  # Get the connection from the 'd' object in the parent environment
  if(exists("d", envir = parent.frame())) {
    d_obj <- get("d", envir = parent.frame())
    
    # Check if d$con exists
    if(is.null(d_obj$con)) {
      stop("The 'd' object exists but 'd$con' is NULL. Please ensure 'd$con' contains a valid database connection.")
    }
    
    con <- d_obj$con
    
    # Verify that con is a valid connection object
    if(!DBI::dbIsValid(con)) {
      stop("The connection in 'd$con' is not valid. Please provide a valid database connection.")
    }
    
    # Execute the query and convert the result to a tibble
    DBI::dbGetQuery(con, query) %>% tibble::as_tibble()
  } else {
    stop("The 'd' variable is not defined in the parent environment. Please define 'd' with a 'con' element before using this function.")
  }
}

#' @noRd
get_widget_edit_buttons <- function(id, widget_id, show_edit_buttons = FALSE){
  ns <- NS(id)
  
  edit_buttons <- div(
    id = ns(paste0(id, "_widget_settings_buttons_", widget_id)),
    div(
      div(
        shiny.fluent::IconButton.shinyInput(
          ns(paste0(id, "_widget_full_screen_", widget_id)), iconProps = list(iconName = "FullScreen"),
          onClick = htmlwidgets::JS(paste0(
            "item => {",
              "Shiny.setInputValue('", id, "-widget_full_screen_trigger', Math.random());",
              "Shiny.setInputValue('", id, "-widget_full_screen_id', ", widget_id, ");",
            "}"
          ))
        ),
        class = "small_icon_button"
      ),
      div(
        shiny.fluent::IconButton.shinyInput(
          ns(paste0(id, "_widget_settings_", widget_id)), iconProps = list(iconName = "Settings"),
          onClick = htmlwidgets::JS(paste0(
            "item => {",
            "Shiny.setInputValue('", id, "-edit_widget_trigger', Math.random());",
            "Shiny.setInputValue('", id, "-edit_widget_id', ", widget_id, ");",
            "}"
          ))
        ),
        class = "small_icon_button"
      ),
      div(
        shiny.fluent::IconButton.shinyInput(
          ns(paste0(id, "_widget_remove_", widget_id)), iconProps = list(iconName = "Delete"),
          onClick = htmlwidgets::JS(paste0(
            "item => {",
              "Shiny.setInputValue('", id, "-remove_widget_trigger', Math.random());",
              "Shiny.setInputValue('", id, "-remove_widget_id', ", widget_id, ");",
            "}"
          ))
        ),
        class = "small_icon_button"
      ),
      style = "display: flex; gap: 2px;"
    ),
    class = "data_widget_buttons"
  )
  if (!show_edit_buttons) edit_buttons <- shinyjs::hidden(edit_buttons)
  
  edit_buttons
}

#' @noRd
load_dataset <- function(id, output, r, m, d, dataset_id, main_tables, selected_study){
  
  i18n <- r$i18n
  ns <- NS(id)
  
  # Reset data vars
  sapply(main_tables, function(table) d[[table]] <- tibble::tibble())
  
  # Get OMOP version for this dataset
  sql <- glue::glue_sql("SELECT value FROM options WHERE category = 'dataset' AND link_id = {dataset_id} AND name = 'omop_version'", .con = r$db)
  omop_version <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
  m$omop_version <- omop_version
  
  # Get dataset unique ID
  sql <- glue::glue_sql("SELECT value FROM options WHERE category = 'dataset' AND name = 'unique_id' AND link_id = {dataset_id}", .con = r$db)
  unique_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
  
  # Get dataset code
  dataset_code_path <- file.path(r$app_folder, "datasets", unique_id, "main.R")
  dataset_code <- readLines(dataset_code_path, warn = FALSE) %>% paste(collapse = "\n")
  
  # Dataset folder
  dataset_folder <- paste0(r$app_folder, "/datasets_files/", dataset_id)
  if (!dir.exists(dataset_folder)) dir.create(dataset_folder)
  
  dataset_code <-
    dataset_code %>% 
    stringr::str_replace_all("%dataset_id%", as.character(dataset_id)) %>%
    stringr::str_replace_all("%omop_version%", paste0("'", omop_version, "'")) %>%
    stringr::str_replace_all("\r", "\n") %>%
    stringr::str_replace_all("%dataset_folder%", dataset_folder)
  
  tryCatch(capture.output(eval(parse(text = dataset_code))), error = function(e){
    cat(paste0("\n", now(), " - mod_", id, " - error loading dataset - dataset_id = ", dataset_id, " - error = ", toString(e)))
    show_message_bar(id, output, "error_loading_data", "severeWarning", i18n = i18n, ns = ns)
  })
  
  # Subsets depending on the selected study
  sql <- glue::glue_sql("SELECT * FROM subsets WHERE study_id = {selected_study}", .con = m$db)
  m$subsets <- DBI::dbGetQuery(m$db, sql)
  subsets_ids <- m$subsets$id
  
  # If there are no subsets, create one default subset
  if (nrow(m$subsets) == 0){
    
    new_subset_id <- get_last_row(m$db, "subsets") + 1
    new_subset_name <- r$i18n$t("subset_all_patients")
    username <- r$users %>% dplyr::filter(id == r$user_id) %>% dplyr::pull(name)
    
    new_data <- tibble::tibble(
      id = new_subset_id, name = new_subset_name, description = NA_character_, study_id = selected_study,
      creator_id = r$user_id, datetime = now(), deleted = FALSE)
    
    DBI::dbAppendTable(m$db, "subsets", new_data)
    m$subsets <- new_data
    
    new_options <- create_options_tibble(
      element_id = new_subset_id, element_name = new_subset_name, sql_category = "subset", user_id = r$user_id, username = username, 
      languages = r$languages, last_options_id = get_last_row(m$db, "options"))
    
    DBI::dbAppendTable(m$db, "options", new_options)
    
    code <- paste0("add_patients_to_subset(patients = d$visit_detail %>% dplyr::select(person_id, visit_occurrence_id, visit_detail_id))")
    new_code <- tibble::tibble(
      id = get_last_row(m$db, "code") + 1, category = "subset", link_id = new_subset_id, code = code,
      creator_id = r$user_id, datetime = now(), deleted = FALSE)
    
    DBI::dbAppendTable(m$db, "code", new_code)
    
    subsets_ids <- new_subset_id
  }
  
  # Select patients belonging to subsets of this study
  sql <- glue::glue_sql("SELECT * FROM subset_persons WHERE subset_id IN ({subsets_ids*})", .con = m$db)
  m$subsets_persons <- DBI::dbGetQuery(m$db, sql)
  
  # Notify user
  show_message_bar(id, output, "success_loading_data", "success", i18n = i18n, ns = ns)
}

#' @noRd
load_dataset_concepts <- function(){
  
  # Get variables from other environments
  for (obj_name in c("r", "d", "m")) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  # Create dataset folder if doesn't exist
  dataset_folder <- paste0(r$app_folder, "/datasets_files/", r$selected_dataset)
  if (!dir.exists(dataset_folder)) dir.create(dataset_folder)
  
  dataset_concept_filename <- paste0(dataset_folder, "/dataset_concept.parquet")
  dataset_drug_strength_filename <- paste0(dataset_folder, "/dataset_drug_strength.parquet")
  
  # Reload concept Parquet files if don't exist
  
  if (!file.exists(dataset_concept_filename) | !file.exists(dataset_drug_strength_filename)){
  
    ## Select distinct concept IDs on dataset ----
    
    tables <- c(
      "person", "visit_occurrence", "visit_detail", "death", "condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure",
      "measurement", "observation", "specimen", "drug_era", "dose_era", "condition_era", "note", "specimen"
    )
    
    # Get all concept_ids for this dataset
    dataset_concept_ids <- NA_integer_
    
    for (table in tables){
      
      concept_ids <- 
        d[[table]] %>%
        dplyr::select(dplyr::contains("concept_id")) %>%
        dplyr::distinct() %>%
        dplyr::collect() %>%
        unlist(use.names = FALSE) %>%
        as.character() %>%
        purrr::discard(~is.na(.) | !grepl("^[0-9]+$", .)) %>%
        as.integer() %>%
        unique()
      
      dataset_concept_ids <- unique(c(dataset_concept_ids, concept_ids))
    }
    
    ## Merge app database and data concepts, and save all these concepts in Parquet files ----
    
    dataset_tables <- DBI::dbListTables(d$con)
    
    local_concepts <- list()
    dataset_concepts <- list()
    combined_concepts <- list()
    
    where_col <- list(
      concept = "concept_id",
      concept_ancestor = c("ancestor_concept_id", "descendant_concept_id"),
      concept_relationship = c("concept_id_1", "concept_id_2"),
      concept_synonym = "concept_id",
      drug_strength = "drug_concept_id",
      vocabulary = "vocabulary_id",
      concept_class = "concept_class_id",
      relationship = "relationship_id",
      domain = "domain_id"
    )
    
    col_names <- get_omop_col_names(version = "5.4")
    
    tables_with_filter <- c("concept", "concept_ancestor", "concept_relationship", "concept_synonym", "drug_strength")
    tables_without_filter <- c("vocabulary", "concept_class", "relationship", "domain")
    all_tables <- c(tables_with_filter, tables_without_filter)
    
    dataset_concept_ids <- dataset_concept_ids[!is.na(dataset_concept_ids)]
    
    for (table in all_tables) {
      
      cols <- col_names[[table]]
      col_list_sql <- paste(cols, collapse = ", ")
      
      if (table %in% tables_with_filter) {
        if (length(where_col[[table]]) == 1) {
          col <- where_col[[table]]
          values <- paste(dataset_concept_ids, collapse = ", ")
          where_clause <- paste0(col, " IN (", values, ") AND ", col, " != 0")
        } else {
          parts <- lapply(where_col[[table]], function(col) {
            values <- paste(dataset_concept_ids, collapse = ", ")
            paste0("(", col, " IN (", values, ") AND ", col, " != 0)")
          })
          where_clause <- paste(parts, collapse = " OR ")
        }
        
        sql <- paste0("SELECT ", col_list_sql, " FROM ", table, " WHERE ", where_clause)
        
      } else {
        sql <- paste0("SELECT ", col_list_sql, " FROM ", table)
      }
      
      local_concepts[[table]] <- DBI::dbGetQuery(m$db, sql) %>% tibble::as_tibble()
      
      if (table %in% dataset_tables) {
        dataset_concepts[[table]] <- DBI::dbGetQuery(d$con, sql) %>% tibble::as_tibble()
      } else {
        dataset_concepts[[table]] <- local_concepts[[table]] %>% dplyr::slice(0)
      }
    }
    
    for (table in names(local_concepts)) {
      combined <- local_concepts[[table]] %>% dplyr::bind_rows(dataset_concepts[[table]])
      
      uniq_cols <- where_col[[table]]
      combined <- combined %>% dplyr::distinct(dplyr::across(all_of(uniq_cols)), .keep_all = TRUE)
      
      combined_concepts[[table]] <- combined
      file_path <- file.path(dataset_folder, paste0(table, ".parquet"))
      arrow::write_parquet(combined, file_path)
      
      # Reload duckdb views
      if (r$dataset_data_source == "disk"){
        
        DBI::dbExecute(d$con, paste0("DROP VIEW IF EXISTS ", table))
        DBI::dbExecute(d$con, paste0("CREATE VIEW ", table, " AS SELECT * FROM read_parquet('", file_path, "')"))
      }
    }
    
    ## dataset_concept & dataset_drug_strength ----
    
    # Load all concepts for this dataset, with rows count
    
    main_cols <- c(
      "condition_era" = "condition",
      "condition_occurrence" = "condition",
      "device_exposure" = "device",
      "dose_era" = "drug",
      "drug_era" = "drug",
      "drug_exposure" = "drug",
      "measurement" = "measurement",
      "observation" = "observation",
      "procedure_occurrence" = "procedure",
      "specimen" = "specimen"
    )
    
    secondary_cols <- list(
      "condition_occurrence" = c("condition_type", "condition_status"),
      "death" = c("death_type", "cause"),
      "device_exposure" = "device_type",
      "dose_era" = "unit",
      "drug_exposure" = c("drug_type", "route"),
      "measurement" = c("measurement_type", "value_as", "unit"),
      "note" = c("note_type", "note_class", "encoding", "language"),
      "observation" = c("observation_type", "qualifier", "value_as", "unit"),
      "person" = c("gender", "race", "ethnicity"),
      "procedure_occurrence" = "procedure_type",
      "specimen" = c("specimen_type", "unit", "anatomic_site", "disease_status")
    )
    
    omop_version <- DBI::dbGetQuery(r$db, glue::glue_sql("SELECT value FROM options WHERE category = 'dataset' AND link_id = {r$selected_dataset} AND name = 'omop_version'", .con = r$db)) %>% dplyr::pull()
    
    if (omop_version == "5.3"){
      secondary_cols <- rlist::list.append(secondary_cols, "visit_occurrence" = c("visit", "visit_type", "visit_source", "admitting_source", "discharge_to"))
      secondary_cols <- rlist::list.append(secondary_cols, "visit_detail" = c("visit_detail", "visit_detail_type", "visit_detail_source", "admitting_source", "discharge_to"))
    }
    else if (omop_version == "5.4"){
      secondary_cols <- rlist::list.append(secondary_cols, "visit_occurrence" = c("visit", "visit_type", "visit_source", "admitted_from", "discharge_to"))
      secondary_cols <- rlist::list.append(secondary_cols, "visit_detail" = c("visit_detail", "visit_detail_type", "visit_detail_source", "admitted_from", "discharge_to"))
    }
    
    concept <- combined_concepts$concept %>% dplyr::mutate(concept_display_name = NA_character_, .after = "concept_name")
    drug_strength <- combined_concepts$drug_strength
    
    # Count rows
    
    count_rows <- tibble::tibble(concept_id = integer(), count_persons_rows = numeric(), count_concepts_rows = numeric())
    
    for(table in tables){
      
      if (d[[table]] %>% dplyr::count() %>% dplyr::pull() > 0){
        
        if (table %in% names(main_cols)){
          
          # Group by concept_id cols & source_concept_id cols (except for specimen & era tables)
          
          if (paste0(main_cols[[table]], "_concept_id") %in% colnames(d[[table]])){
            count_rows <-
              count_rows %>%
              dplyr::bind_rows(
                d[[table]] %>%
                  dplyr::group_by_at(paste0(main_cols[[table]], "_concept_id")) %>%
                  dplyr::summarize(count_persons_rows = as.numeric(dplyr::n_distinct(person_id)), count_concepts_rows = as.numeric(dplyr::n())) %>%
                  dplyr::ungroup() %>%
                  dplyr::rename(concept_id = paste0(main_cols[[table]], "_concept_id")) %>%
                  dplyr::mutate_at("concept_id", as.integer) %>%
                  dplyr::collect()
              )
            
            if (table %not_in% c("specimen", "drug_era", "dose_era", "condition_era")) count_rows <-
                count_rows %>%
                dplyr::bind_rows(
                  d[[table]] %>%
                    dplyr::group_by_at(paste0(main_cols[[table]], "_source_concept_id")) %>%
                    dplyr::summarize(count_persons_rows = as.numeric(dplyr::n_distinct(person_id)), count_concepts_rows = as.numeric(dplyr::n())) %>%
                    dplyr::ungroup() %>%
                    dplyr::rename(concept_id = paste0(main_cols[[table]], "_source_concept_id")) %>%
                    dplyr::mutate_at("concept_id", as.integer) %>%
                    dplyr::collect()
                )
          }
        }
        
        if (table %in% names(secondary_cols)){
          
          for (col in secondary_cols[[table]]){
            
            if (paste0(col, "_concept_id") %in% colnames(d[[table]])){
              
              count_rows <-
                count_rows %>%
                dplyr::bind_rows(
                  d[[table]] %>%
                    dplyr::group_by_at(paste0(col, "_concept_id")) %>%
                    dplyr::summarize(count_persons_rows = as.numeric(dplyr::n_distinct(person_id)), count_concepts_rows = as.numeric(dplyr::n())) %>%
                    dplyr::ungroup() %>%
                    dplyr::rename(concept_id = paste0(col, "_concept_id")) %>%
                    dplyr::mutate_at("concept_id", as.integer) %>%
                    dplyr::collect()
                )
            }
          }
        }
      }
    }
    
    if (nrow(concept) == 0) concept <- 
      concept %>% 
      dplyr::slice(0) %>%
      dplyr::mutate(count_persons_rows = numeric(), count_concepts_rows = numeric(), add_concept_input = character())
    
    if (nrow(count_rows) > 0 & nrow(concept) > 0) {
      
      # Group count_rows : if a concept_id is in distinct tables, it will produce multiple rows by concept_id
      count_rows <- 
        count_rows %>%
        dplyr::filter(!is.na(concept_id)) %>%
        dplyr::group_by(concept_id) %>%
        dplyr::summarize(count_persons_rows = as.numeric(max(count_persons_rows)), count_concepts_rows = as.numeric(sum(count_concepts_rows))) %>%
        dplyr::ungroup()
      
      # Merge count_rows, transform count_rows cols to integer, to be sortable
      concept <- concept %>%
        dplyr::left_join(count_rows, by = "concept_id") %>%
        dplyr::mutate_at(c("count_persons_rows", "count_concepts_rows"), as.numeric) %>%
        dplyr::filter(count_concepts_rows > 0)
      
      # Add plus col
      concept <- 
        concept %>%
        dplyr::mutate(
          add_concept_input = as.character(
            div(
              shiny::actionButton(
                "%ns%-add_concept_%concept_id%", "", icon = icon("plus"),
                onclick = paste0(
                  "Shiny.setInputValue('%ns%-add_concept_trigger', Math.random());",
                  "Shiny.setInputValue('%ns%-concept_selected', %concept_id%);"
                ),
                style = "width: 22px;"
              ),
              class = "small_icon_button"
            )
          )
        )
    }
    
    # Delete old rows
    sql <- glue::glue_sql("DELETE FROM concept_dataset WHERE dataset_id = {r$selected_dataset}", .con = m$db)
    query <- DBI::dbSendStatement(m$db, sql)
    DBI::dbClearResult(query)
    
    # Add new rows to database
    if (nrow(concept) > 0) DBI::dbAppendTable(
      m$db, "concept_dataset",
      concept %>%
        dplyr::transmute(
          id = get_last_row(m$db, "concept_dataset") + 1:dplyr::n(), concept_id, dataset_id = r$selected_dataset, vocabulary_id,
          count_persons_rows, count_concepts_rows, count_secondary_concepts_rows = as.numeric(0)))
    
    # Save data as Parquet
    
    arrow::write_parquet(concept, dataset_concept_filename)
    arrow::write_parquet(drug_strength, dataset_drug_strength_filename)
    
    # Update d var
    d$dataset_concept <- concept
    d$dataset_drug_strength <- drug_strength
  }
  
  else {
    d$dataset_concept <- arrow::read_parquet(dataset_concept_filename)
    d$dataset_drug_strength <- arrow::read_parquet(dataset_drug_strength_filename)
  }
  
  r$dataset_vocabularies <- d$dataset_concept %>% dplyr::distinct(vocabulary_id) %>% dplyr::arrange(vocabulary_id)
}

#' @noRd
prepare_sortable_pivot_tabs <- function(ns, category, tab_group_id, tab_sub_group, tabs_ui) {
  div(
    id = ns(paste0("study_pivot_sub_div_", tab_group_id, "_", tab_sub_group)),
    tabs_ui,
    class = "pivot",
    `data-id-prefix` = paste0(ns(paste0("tab_"))),
    `data-input-id` = ns(paste0(category, "_tab_positions")), 
    `data-draggable-class` = "pivot_item"
  )
}

#' @noRd
load_element_code <- function(id, r, unique_id){
  
  folder <- paste0(r$app_folder, "/", id, "/", unique_id)
  code_file <- paste0(folder, "/main.R")
  old_code_file <- paste0(folder, "/code.R")
  
  if (!dir.exists(folder)) dir.create(folder)
  
  # Retro-compatibility with older versions (with "code.R" instead of "main.R")
  if (file.exists(code_file)) code <- readLines(code_file, warn = FALSE)
  else {
    if (file.exists(old_code_file)) code <- readLines(old_code_file, warn = FALSE)
    else code <- ""
    
    writeLines(code, code_file)
  }
  
  paste(code, collapse = "\n")
}

#' @noRd
process_widget_code <- function(code, tab_id, widget_id, study_id, patient_id, plugin_folder) {
  
  # Replace %import_script% tags
  import_scripts <- regmatches(code, gregexpr("%import_script\\(['\"](.*?)['\"]\\)%", code, perl = TRUE))[[1]]
  
  for (i in seq_along(import_scripts)){
    tag <- import_scripts[i]
    file_name <- gsub("%import_script\\(['\"](.*?)['\"]\\)%", "\\1", tag)
    file_path <- paste0(plugin_folder, "/", file_name)
    file_ext <- sub(".*\\.", "", tolower(file_name))
    
    if (file.exists(file_path)){
      file_name <- file_name %>% gsub("\\.", "\\\\.", ., fixed = FALSE)
      
      if (file_ext == "r"){
        file_code <- readLines(file_path, warn = FALSE) %>% paste(collapse = "\n")
        
        code <-
          code %>%
          gsub(paste0("%import_script\\('", file_name, "'\\)%"), file_code, ., fixed = FALSE) %>%
          gsub(paste0('%import_script\\("', file_name, '"\\)%'), file_code, ., fixed = FALSE)
      }
      else if (file_ext == "py"){
        file_code <- paste0("reticulate::source_python('", file_path, "')")
        
        code <-
          code %>%
          gsub(paste0("%import_script\\('", file_name, "'\\)%"), file_code, ., fixed = FALSE) %>%
          gsub(paste0('%import_script\\("', file_name, '"\\)%'), file_code, ., fixed = FALSE)
      }
    }
  }
  
  # Replace tab & widget IDs
  code <- gsub("%tab_id%", as.character(tab_id), code, fixed = TRUE)
  code <- gsub("%widget_id%", as.character(widget_id), code, fixed = TRUE)
  
  # Replace req (so that an observer in inactivated when server code is launched more than one time)
  code <- gsub("%req%", "req(m[[session_code]] == session_num)\nreq(m$selected_study == %study_id%)", code, fixed = TRUE)
  
  # Replace study and patients IDS
  code <- gsub("%study_id%", as.character(study_id), code, fixed = TRUE)
  code <- gsub("%patient_id%", as.character(patient_id), code, fixed = TRUE)
  
  gsub("\r", "\n", code, fixed = TRUE)
  
  code
}

#' @noRd
save_element_code <- function(id, i18n, output, r, unique_id, new_code){
  
  ns <- NS(id)
  
  folder <- paste0(r$app_folder, "/", id, "/", unique_id)
  code_file <- paste0(folder, "/main.R")
  
  code <- writeLines(new_code, code_file)
  
  show_message_bar(id, output, "modif_saved", "success", i18n = i18n, ns = ns)
}
