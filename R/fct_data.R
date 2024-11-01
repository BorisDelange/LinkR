#' @noRd
create_gridstack_instance <- function(id, tab_id){
  ns <- NS(id)
  
  shinyjs::delay(200, 
    shinyjs::runjs(paste0("
      if (!window.gridStackInstances['", tab_id, "']) {
        const grid = GridStack.init({
          cellHeight: 15,
          scroll: false,
          column: 12,
          staticGrid: true,
          float: false,
          resizable: { handles: 'se, ne, nw, sw' },
          margin: 10
        }, '#", ns(paste0("gridstack_", tab_id)), "');

        window.gridStackInstances['", tab_id, "'] = grid;
      }
    "))
  )
}

#' @noRd
create_widget <- function(id, widget_id, ui_code, w = 6, h = 25, x = 0, y = 0){
  ns <- NS(id)
  
  if (is.na(w) | w == 0) w <- 6
  if (is.na(h) | h == 0) h <- 25
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

get_widget_edit_buttons <- function(id, widget_id, show_edit_buttons = FALSE){
  ns <- NS(id)
  
  edit_buttons <- div(
    id = ns(paste0(id, "_widget_settings_buttons_", widget_id)),
    div(
      div(
        shiny.fluent::IconButton.shinyInput(ns(paste0(id, "_widget_settings_", widget_id)), iconProps = list(iconName = "Settings")),
        class = "small_icon_button"
      ),
      div(
        shiny.fluent::IconButton.shinyInput(ns(paste0(id, "_widget_remove_", widget_id)), iconProps = list(iconName = "Delete")),
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
load_dataset <- function(r, m, d, dataset_id, main_tables, selected_study){
  
  # Reset data vars
  sapply(main_tables, function(table) d[[table]] <- tibble::tibble())
  
  # Get OMOP version for this dataset
  sql <- glue::glue_sql("SELECT value FROM options WHERE category = 'dataset' AND link_id = {dataset_id} AND name = 'omop_version'", .con = r$db)
  omop_version <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
  m$omop_version <- omop_version
  
  # Get dataset code from db
  sql <- glue::glue_sql("SELECT code FROM code WHERE category = 'dataset' AND link_id = {dataset_id}", .con = r$db)
  dataset_code <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
  
  # Get dataset unique ID
  sql <- glue::glue_sql("SELECT value FROM options WHERE category = 'dataset' AND name = 'unique_id' AND link_id = {dataset_id}", .con = r$db)
  unique_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
  
  # Dataset folder
  dataset_folder <- paste0(r$app_folder, "/datasets_files/", dataset_id)
  if (!dir.exists(dataset_folder)) dir.create(dataset_folder)
  
  dataset_code <-
    dataset_code %>% 
    stringr::str_replace_all("%dataset_id%", as.character(dataset_id)) %>%
    stringr::str_replace_all("%omop_version%", paste0("'", omop_version, "'")) %>%
    stringr::str_replace_all("\r", "\n") %>%
    stringr::str_replace_all("%dataset_folder%", dataset_folder)
  
  tryCatch(capture.output(eval(parse(text = dataset_code))), error = function(e) cat(paste0("\n", now(), " - mod_data - error loading dataset - dataset_id = ", dataset_id)))
  
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
    
    code <- paste0(
      "add_patients_to_subset(\n",
      "    patients = d$person %>% dplyr::pull(person_id),\n",
      "    subset_id = %subset_id%,\n",
      "    output = output, r = r, m = m, i18n = i18n, ns = ns\n",
      ")"
    )
    new_code <- tibble::tibble(
      id = get_last_row(m$db, "code") + 1, category = "subset", link_id = new_subset_id, code = code,
      creator_id = r$user_id, datetime = now(), deleted = FALSE)
    
    DBI::dbAppendTable(m$db, "code", new_code)
    
    subsets_ids <- new_subset_id
  }
  
  # Select patients belonging to subsets of this study
  sql <- glue::glue_sql("SELECT * FROM subset_persons WHERE subset_id IN ({subsets_ids*})", .con = m$db)
  m$subsets_persons <- DBI::dbGetQuery(m$db, sql)
}

#' @noRd
load_dataset_concepts <- function(r, d, m){
  
  req(!is.na(r$selected_dataset), r$selected_dataset != 0)
  
  # Create dataset folder if doesn't exist
  dataset_folder <- paste0(r$app_folder, "/datasets_files/", r$selected_dataset)
  if (!dir.exists(dataset_folder)) dir.create(dataset_folder)
  
  # Load csv file if it exists
  concept_filename <- paste0(dataset_folder, "/concept.csv")
  
  if (file.exists(concept_filename)) d$concept <- vroom::vroom(concept_filename, col_types = "iicccccccDDciic", progress = FALSE)
  
  if (!file.exists(concept_filename)){
    
    # Load all concepts for this dataset, with rows count
    omop_version <- DBI::dbGetQuery(r$db, glue::glue_sql("SELECT value FROM options WHERE category = 'dataset' AND link_id = {r$selected_dataset} AND name = 'omop_version'", .con = r$db)) %>% dplyr::pull()
    
    tables <- c(
      "person", "visit_occurrence", "visit_detail", "condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure",
      "measurement", "observation", "specimen", "drug_era", "dose_era", "condition_era", "note", "specimen")
    
    if (omop_version %in% c("5.3", "5.0")) tables <- c(tables, "death")
    
    main_cols <- c(
      "condition_occurrence" = "condition",
      "drug_exposure" = "drug",
      "procedure_occurrence" = "procedure",
      "device_exposure" = "device",
      "measurement" = "measurement",
      "observation" = "observation",
      "specimen" = "specimen",
      "drug_era" = "drug",
      "dose_era" = "drug",
      "condition_era" = "condition"
    )
    
    secondary_cols <- list(
      "person" = c("gender", "race", "ethnicity"),
      "condition_occurrence" = c("condition_type", "condition_status"),
      "drug_exposure" = c("drug_type", "route"),
      "procedure_occurrence" = "procedure_type",
      "device_exposure" = "device_type",
      "measurement" = c("measurement_type", "value_as", "unit"),
      "observation" = c("observation_type", "qualifier", "value_as", "unit"),
      "note" = c("note_type", "note_class", "encoding", "language"),
      "specimen" = c("specimen_type", "unit", "anatomic_site", "disease_status"),
      "dose_era" = "unit"
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
    
    sql <- glue::glue_sql("SELECT * FROM concept WHERE concept_id IN ({dataset_concept_ids*}) AND concept_id != 0 ORDER BY concept_id", .con = m$db)
    concept <- DBI::dbGetQuery(m$db, sql) %>% tibble::as_tibble() %>% dplyr::mutate(concept_display_name = NA_character_, .after = "concept_name")
    
    # Count rows
    
    count_rows <- tibble::tibble(concept_id = integer(), count_persons_rows = numeric(), count_concepts_rows = numeric())
    
    if (omop_version == "5.3"){
      secondary_cols <- rlist::list.append(secondary_cols, "visit_occurrence" = c("visit", "visit_type", "visit_source", "admitting_source", "discharge_to"))
      secondary_cols <- rlist::list.append(secondary_cols, "visit_detail" = c("visit_detail", "visit_detail_type", "visit_detail_source", "admitting_source", "discharge_to"))
    }
    else if (omop_version %in% c("5.4", "6.0")){
      secondary_cols <- rlist::list.append(secondary_cols, "visit_occurrence" = c("visit", "visit_type", "visit_source", "admitted_from", "discharge_to"))
      secondary_cols <- rlist::list.append(secondary_cols, "visit_detail" = c("visit_detail", "visit_detail_type", "visit_detail_source", "admitted_from", "discharge_to"))
    }
    
    if (omop_version %in% c("5.3", "5.0")) secondary_cols <- rlist::list.append(secondary_cols, "death" = c("death_type", "cause"))
    
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
    
    if (nrow(count_rows) > 0) {
      
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
            # div(
            #   shiny.fluent::IconButton.shinyInput(
            #     "%ns%-concept_info_%concept_id%", iconProps = list(iconName = "Info"),
            #     onClick = paste0("Shiny.setInputValue('%ns%-concept_selected', %concept_id%)")
            #   ),
            #   shiny.fluent::IconButton.shinyInput(
            #     "%ns%-add_concept_%concept_id%", iconProps = list(iconName = "Add"),
            #     onClick = paste0("Shiny.setInputValue('%ns%-concept_selected', %concept_id%)")
            #   ),
            #   class = "small_icon_button",
            #   style = "display: flex; justify-content:center;"
            # )
            div(
              shiny::actionButton(
                "%ns%-show_concept_%concept_id%", "", icon = icon("info"),
                onclick = paste0(
                  "Shiny.setInputValue('%ns%-show_concept_trigger', Math.random());",
                  "Shiny.setInputValue('%ns%-concept_selected', %concept_id%);"
                ),
                style = "width: 22px;"
              ),
              shiny::actionButton(
                "%ns%-add_concept_%concept_id%", "", icon = icon("plus"),
                onclick = paste0(
                  "Shiny.setInputValue('%ns%-add_concept_trigger', Math.random());",
                  "Shiny.setInputValue('%ns%-concept_selected', %concept_id%);"
                ),
                style = "width: 22px;"
              ),
              class = "small_icon_button",
              style = "display: flex; gap: 5px; justify-content:center;"
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
    
    # Save data as csv
    readr::write_csv(concept, concept_filename, progress = FALSE)
    
    # Update d var
    d$concept <- concept
  }
  
  r$dataset_vocabularies <-
    r$vocabularies_wide %>%
    dplyr::inner_join(d$concept %>% dplyr::distinct(vocabulary_id), by = "vocabulary_id") %>%
    dplyr::arrange(vocabulary_id)
  
  # Then load d$dataset_drug_strength
  # r$load_dataset_drug_strength <- now()
}