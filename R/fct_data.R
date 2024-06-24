#' @noRd
create_gridstack_instance <- function(id, tab_id){
  ns <- NS(id)
  
  shinyjs::delay(200, 
    shinyjs::runjs(paste0("
      if (!window.gridStackInstances['", tab_id, "']) {
        import('https://esm.sh/gridstack').then((module) => {
          const GridStack = module.GridStack;
          window.gridStackInstances['", tab_id, "'] = GridStack.init({
            cellHeight: 100,
            staticGrid: true,
            resizable: { handles: 'se, ne, nw, sw' },
            margin: 10
          }, '#", ns(paste0("gridstack_", tab_id)), "');
        });
      }
    "))
  )
}

#' @noRd
create_widget <- function(id, widget_id, ui_code){
  ns <- NS(id)
  
  div(
    id = ns(paste0(id, "_gridstack_item_", widget_id)),
    class = "grid-stack-item",
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
        grid.addWidget(`", ui_output, "`, {w: 6, h: 4});
        
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

load_dataset_concepts <- function(r, d, m){
  
  req(!is.na(r$selected_dataset))
    
  # Create dataset folder if doesn't exist
  dataset_folder <- paste0(r$app_folder, "/datasets_files/", r$selected_dataset)
  if (!dir.exists(dataset_folder)) dir.create(dataset_folder)
  
  # Load csv file if it exists
  dataset_all_concepts_filename <- paste0(dataset_folder, "/dataset_all_concepts.csv")
  
  if (file.exists(dataset_all_concepts_filename)) d$dataset_all_concepts <- vroom::vroom(dataset_all_concepts_filename, col_types = "icicccciccccccccii", progress = FALSE)
  
  if (!file.exists(dataset_all_concepts_filename)){
    
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
      
      concept_ids <- d[[table]] %>%
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
    
    sql <- glue::glue_sql(paste0(
      "SELECT * ",
      "FROM concept ",
      "WHERE concept_id IN ({dataset_concept_ids*}) ",
      "ORDER BY concept_id"), .con = m$db)
    dataset_all_concepts <- DBI::dbGetQuery(m$db, sql) %>% tibble::as_tibble() %>% dplyr::mutate(concept_display_name = NA_character_, .after = "concept_name")
    
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
                    dplyr::collect()
                )
            }
          }
        }
      }
    }
    
    if (nrow(count_rows) > 0){
      
      # Group count_rows : if a concept_id is in distinct tables, it will produce multiple rows by concept_id
      count_rows <- count_rows %>%
        dplyr::filter(!is.na(concept_id)) %>%
        dplyr::group_by(concept_id) %>%
        dplyr::summarize(count_persons_rows = as.numeric(max(count_persons_rows)), count_concepts_rows = as.numeric(sum(count_concepts_rows))) %>%
        dplyr::ungroup()
      
      # Merge count_rows, transform count_rows cols to integer, to be sortable
      dataset_all_concepts <- dataset_all_concepts %>%
        dplyr::left_join(count_rows, by = "concept_id") %>%
        dplyr::mutate_at(c("count_persons_rows", "count_concepts_rows"), as.numeric) %>%
        dplyr::filter(count_concepts_rows > 0)
    }
    
    if (nrow(count_rows) == 0) dataset_all_concepts <- dataset_all_concepts %>% dplyr::slice(0)
    
    dataset_all_concepts <- dataset_all_concepts %>%
      dplyr::rename(concept_id_1 = concept_id, concept_name_1 = concept_name, concept_display_name_1 = concept_display_name, vocabulary_id_1 = vocabulary_id) %>%
      dplyr::mutate(relationship_id = NA_character_, vocabulary_id_2 = NA_character_, concept_id_2 = NA_integer_, concept_name_2 = NA_character_, .after = "concept_display_name_1") %>%
      dplyr::relocate(vocabulary_id_1, .before = "concept_id_1")
    
    # Load d$concept & d$concept_relationship if not already loaded from mod_settings_data_management.R
    # Convert cols to char and arrange cols as done in mod_settings_data_management.R
    
    tables <- c("concept", "concept_relationship")
    
    cols_to_char <- list()
    cols_to_char$concept = "concept_id"
    cols_to_char$relationship = "relationship_concept_id"
    
    cols_order <- list()
    cols_order$concept <- "concept_id"
    cols_order$concept_relationship <- "concept_id_1"
    
    for (table in tables){
      if (length(d[[table]]) == 0){
        if (table == "concept") sql <- glue::glue_sql("SELECT * FROM concept", .con = m$db)
        else if (table == "concept_relationship") sql <- glue::glue_sql(paste0(
          "SELECT cr.* FROM concept_relationship cr WHERE cr.id NOT IN ( ",
          "WITH cr2 AS (",
          "SELECT cru.concept_relationship_id, ",
          "SUM(CASE WHEN cre.evaluation_id = 1 THEN 1 ELSE 0 END) AS positive_evals, ",
          "SUM(CASE WHEN cre.evaluation_id = 2 THEN 1 ELSE 0 END) AS negative_evals ",
          "FROM concept_relationship_user cru ",
          "LEFT JOIN concept_relationship_evals cre ON cru.concept_relationship_id = cre.concept_relationship_id ",
          "GROUP BY cru.concept_relationship_id ",
          "HAVING positive_evals = 0 OR (positive_evals > 0 AND positive_evals <= negative_evals) ",
          ") ",
          "SELECT concept_relationship_id FROM cr2 ",
          ")"), .con = m$db)
        
        d[[table]] <- DBI::dbGetQuery(m$db, sql) %>%
          dplyr::arrange(cols_order[[table]]) %>%
          dplyr::mutate_at(cols_to_char[[table]], as.character) %>%
          dplyr::mutate(modified = FALSE)
      }
    }
    
    # Merge mapped concepts
    
    if (nrow(d$concept) > 0 & nrow(dataset_all_concepts) > 0){
      dataset_all_concepts <- dataset_all_concepts %>%
        dplyr::bind_rows(
          dataset_all_concepts %>%
            dplyr::select(
              vocabulary_id_2 = vocabulary_id_1, concept_id_2 = concept_id_1, concept_name_2 = concept_name_1,
              count_persons_rows, count_concepts_rows) %>%
            dplyr::left_join(
              d$concept_relationship %>%
                dplyr::mutate_at(c("concept_id_1", "concept_id_2"), as.integer) %>%
                dplyr::select(concept_id_1, concept_id_2, relationship_id),
              by = "concept_id_2"
            ) %>%
            dplyr::filter(concept_id_1 != concept_id_2) %>%
            dplyr::left_join(
              d$concept %>%
                dplyr::mutate_at("concept_id", as.integer) %>%
                dplyr::select(
                  vocabulary_id_1 = vocabulary_id, concept_id_1 = concept_id, concept_name_1 = concept_name,
                  domain_id, concept_class_id, standard_concept, concept_code, valid_start_date, valid_end_date, invalid_reason),
              by = "concept_id_1"
            ) %>%
            dplyr::arrange(dplyr::desc(count_concepts_rows)) %>%
            dplyr::mutate(concept_display_name_1 = NA_character_, .after = "concept_name_1")
        )
    }
    
    if (nrow(dataset_all_concepts) == 0){
      dataset_all_concepts <- dataset_all_concepts %>% dplyr::mutate(
        count_persons_rows = numeric(), count_concepts_rows = numeric(), add_concept_input = character())
    }
    
    else {
      # Add plus col
      dataset_all_concepts <- dataset_all_concepts %>%
        dplyr::mutate(
          add_concept_input = as.character(shiny::actionButton(NS("%ns%")("%input_prefix%_%concept_id_1%"), "", icon = icon("plus"),
            onclick = paste0("Shiny.setInputValue('%ns%-%input_prefix_2%concept_selected', this.id, {priority: 'event'})")))
        )
    }
    
    # Delete old rows
    sql <- glue::glue_sql("DELETE FROM concept_dataset WHERE dataset_id = {r$selected_dataset}", .con = m$db)
    query <- DBI::dbSendStatement(m$db, sql)
    DBI::dbClearResult(query)
    
    # Add new rows to database
    if (nrow(dataset_all_concepts) > 0) DBI::dbAppendTable(
      m$db, "concept_dataset",
        dataset_all_concepts %>%
        dplyr::transmute(
          id = get_last_row(m$db, "concept_dataset") + 1:dplyr::n(), concept_id = concept_id_1, dataset_id = r$selected_dataset, vocabulary_id = vocabulary_id_1,
          count_persons_rows, count_concepts_rows, count_secondary_concepts_rows = as.numeric(0)))
    
    # Save data as csv
    readr::write_csv(dataset_all_concepts, dataset_all_concepts_filename, progress = FALSE)
    
    # Update d var
    d$dataset_all_concepts <- dataset_all_concepts
  }
  
  # Get user's modifications on items names & concept_display_names
  
  sql <- glue::glue_sql(paste0(
    "SELECT id, concept_id, concept_name, concept_display_name ",
    "FROM concept_user ",
    "WHERE user_id = {r$user_id}"), .con = m$db)
  dataset_user_concepts <- DBI::dbGetQuery(m$db, sql) %>% tibble::as_tibble()
  
  # Merge tibbles
  if (nrow(dataset_user_concepts) > 0) d$dataset_all_concepts <-
    d$dataset_all_concepts %>%
    dplyr::left_join(
      dataset_user_concepts %>% dplyr::select(concept_id_1 = concept_id, new_concept_name_1 = concept_name, new_concept_display_name_1 = concept_display_name),
      by = "concept_id_1"
    ) %>%
    dplyr::mutate(
      concept_name_1 = dplyr::case_when(!is.na(new_concept_name_1) ~ new_concept_name_1, TRUE ~ concept_name_1),
      concept_display_name_1 = dplyr::case_when(!is.na(new_concept_display_name_1) ~ new_concept_display_name_1, TRUE ~ concept_display_name_1)
    ) %>%
    dplyr::left_join(
      dataset_user_concepts %>% dplyr::select(concept_id_2 = concept_id, new_concept_name_2 = concept_name),
      by = "concept_id_2"
    ) %>%
    dplyr::mutate(
      concept_name_2 = dplyr::case_when(!is.na(new_concept_name_2) ~ new_concept_name_2, TRUE ~ concept_name_2)
    ) %>%
    dplyr::select(-new_concept_name_1, -new_concept_display_name_1, -new_concept_name_2)
  
  r$dataset_vocabularies <-
    r$vocabularies_wide %>%
    dplyr::filter(vocabulary_id %in% unique(c(unique(d$dataset_all_concepts$vocabulary_id_1), unique(d$dataset_all_concepts$vocabulary_id_2)))) %>%
    dplyr::arrange(vocabulary_id)
  
  # Join d$person, d$visit_occurrence & d$visit_detail with d$dataset_all_concepts
  # r$merge_concepts_and_d_vars <- now()
  
  # Then load d$dataset_drug_strength
  # r$load_dataset_drug_strength <- now()
}