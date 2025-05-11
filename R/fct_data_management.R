#' Add patients to a subset
#'
#' @description
#' This function adds patients to a specific subset in the database table subset_persons.
#' It can handle person identifiers alone or combined with visit identifiers.
#' It ensures the validity of the input parameters and prevents duplicate entries.
#'
#' @param patients Either:
#' - A numeric vector of person_ids
#' - A data.frame/tibble containing at least one of these columns:
#'   - person_id: Patient identifier
#'   - visit_occurrence_id: Visit identifier (optional, requires person_id)
#'   - visit_detail_id: Visit detail identifier (optional, requires visit_occurrence_id)
#' @param subset_id An integer representing the ID of the subset to which the patients will be added.
#' The subset ID must be positive and valid.
#'
#' @return
#' A character string indicating the success or failure of the operation. If successful, the message includes
#' the number of patients added to the subset. If an error occurs, it returns an error message.
#'
#' @details
#' The function performs several checks:
#' - Validates subset_id to ensure it is a positive integer
#' - Validates patients to ensure it is either a numeric vector or a data.frame/tibble with a person_id column
#' - If visit identifiers are provided, ensures they are properly structured
#' - Removes duplicate entries based on the provided identification columns
#' - Removes patients that are already in the subset to avoid duplicates
#'
#' The function handles three cases:
#' 1. Basic: Only person_id provided
#' 2. Visits: person_id and visit_occurrence_id provided
#' 3. Detailed visits: person_id, visit_occurrence_id and visit_detail_id provided
#'
#' If the inputs are valid, the function inserts the new patients into the subset_persons table in the database.
#'
#' @examples
#' \dontrun{
#' # Example with a numeric vector of person IDs
#' patients <- c(123, 456, 789)
#' add_patients_to_subset(patients = patients)
#'
#' # Example with person_ids in a tibble
#' patients <- tibble::tibble(person_id = c(123, 456, 789))
#' add_patients_to_subset(patients = patients)
#'
#' # Example with visit_occurrence_ids
#' patients <- tibble::tibble(
#'   person_id = c(123, 456),
#'   visit_occurrence_id = c(1001, 1002)
#' )
#' add_patients_to_subset(patients = patients)
#'
#' # Example with visit_detail_ids
#' patients <- tibble::tibble(
#'   person_id = c(123, 456),
#'   visit_occurrence_id = c(1001, 1002),
#'   visit_detail_id = c(2001, 2002)
#' )
#' add_patients_to_subset(patients = patients)
#' }
add_patients_to_subset <- function(patients = tibble::tibble(), subset_id = integer()){
  
  # Get variables from other environments
  for (obj_name in c("r", "m", "output")) assign(obj_name, get(obj_name, envir = parent.frame()))
  i18n <- r$i18n
  
  # Get subset_id if it is null
  if (length(subset_id) == 0 && r$current_page == "subsets") subset_id <- eval(parse(text = "input$selected_element"), envir = parent.frame())
  
  # Check subset_id
  
  invalid_subset_id <- TRUE
  if (length(subset_id) > 0) if (is.numeric(subset_id)) if (!is.na(subset_id) & subset_id > 0) invalid_subset_id <- FALSE
  if (invalid_subset_id) return(i18n$t("invalid_subset_id_value"))
  
  # Check and process patients
  
  if (is.numeric(patients)) patients <- tibble::tibble(person_id = patients)
  patients <- patients %>% dplyr::collect()
  
  if (!"person_id" %in% colnames(patients)) patients$person_id <- NA_integer_
  if (!"visit_occurrence_id" %in% colnames(patients)) patients$visit_occurrence_id <- NA_integer_
  if (!"visit_detail_id" %in% colnames(patients)) patients$visit_detail_id <- NA_integer_
  
  patients <- patients %>% dplyr::distinct(person_id, visit_occurrence_id, visit_detail_id)
  
  if (any(!is.na(patients$visit_occurrence_id) & is.na(patients$person_id))) return(i18n$t("invalid_visit_occurrence_without_person"))
  if (any(!is.na(patients$visit_detail_id) & is.na(patients$visit_occurrence_id))) return(i18n$t("invalid_visit_detail_without_visit_occurrence"))
  
  # Keep only patients not already in the subset
  
  if ("visit_detail_id" %in% colnames(patients)) {
    actual_patients <- DBI::dbGetQuery(m$db, glue::glue_sql("SELECT person_id, visit_occurrence_id, visit_detail_id FROM subset_persons WHERE subset_id = {subset_id}", .con = m$db))
    patients <- patients %>% dplyr::anti_join(actual_patients, by = c("person_id", "visit_occurrence_id", "visit_detail_id"))
  }
  else if ("visit_occurrence_id" %in% colnames(patients)) {
    actual_patients <- DBI::dbGetQuery(m$db, glue::glue_sql("SELECT person_id, visit_occurrence_id FROM subset_persons WHERE subset_id = {subset_id}", .con = m$db))
    patients <- patients %>% dplyr::anti_join(actual_patients, by = c("person_id", "visit_occurrence_id"))
  }
  else {
    actual_patients <- DBI::dbGetQuery(m$db, glue::glue_sql("SELECT person_id FROM subset_persons WHERE subset_id = {subset_id}", .con = m$db))
    patients <- patients %>% dplyr::anti_join(actual_patients, by = "person_id")
  }
  
  # If there are patients to add
  if (nrow(patients) > 0){
    
    # Add new patients in the subset
    
    last_id <- DBI::dbGetQuery(m$db, "SELECT COALESCE(MAX(id), 0) FROM subset_persons") %>% dplyr::pull()
    other_cols <- tibble::tibble(id = 1:nrow(patients) + last_id, subset_id = subset_id, creator_id = r$user_id, datetime = now(), deleted = FALSE)
    
    if (!"visit_occurrence_id" %in% colnames(patients)) patients$visit_occurrence_id <- NA_integer_
    if (!"visit_detail_id" %in% colnames(patients)) patients$visit_detail_id <- NA_integer_
    
    # Combine and reorder columns
    
    sql <- "SELECT * FROM subset_persons LIMIT 0"
    cols_order <- colnames(DBI::dbGetQuery(m$db, sql))
    
    patients <- patients %>% dplyr::bind_cols(other_cols) %>% dplyr::select(dplyr::all_of(cols_order))
    
    DBI::dbAppendTable(m$db, "subset_persons", patients)
  }
  
  n_rows <- list()
  n_rows$patients <- patients %>% dplyr::distinct(person_id) %>% dplyr::count()
  
  if (n_rows$patients > 0) return(paste0(
    i18n$t("add_patients_subset_success"), " (", n_rows$patients, " ", tolower(i18n$t("patients")), ")"))
  else return(i18n$t("no_patients_to_add_to_subset"))
}

#' Remove entries from a subset
#'
#' @description 
#' This function removes entries from a specific subset in the database table subset_persons.
#' Entries can be identified by person_id, visit_occurrence_id (hospital stay), visit_detail_id (unit stay) 
#' or any valid combination of these. The removal is hierarchical: removing a person_id removes all associated 
#' visits and units, removing a visit removes all associated units.
#' 
#' @param output Shiny output variable, used for displaying outputs in a Shiny application.
#' @param r A `shiny::reactiveValues` object, used to communicate between modules in the Shiny app. 
#' @param m A `shiny::reactiveValues` object, used to communicate between modules in the Shiny app. 
#' @param patients A numeric vector of person_ids or a `data.frame`/`tibble` that can contain any of these columns:
#'   - person_id: Patient identifier
#'   - visit_occurrence_id: Visit identifier (optional, requires person_id)
#'   - visit_detail_id: Visit detail identifier (optional, requires visit_occurrence_id)
#' @param subset_id An integer representing the ID of the subset from which the entries will be removed.
#' The subset ID must be positive and valid.
#' 
#' @return 
#' A character string indicating the success or failure of the operation. If successful, the message includes
#' the number of patients, hospital stays, and unit stays removed from the subset. If an error occurs
#' (e.g., invalid ID combinations), it returns an error message.
#' 
#' @details
#' The function handles three cases:
#' 1. Remove by person_id: Removes all entries for specified patients, including their hospital and unit stays
#' 2. Remove by visit_occurrence_id: Removes specific hospital stays and their associated unit stays
#' 3. Remove by visit_detail_id: Removes only specific unit stays
#'
#' The function performs several validations:
#' - Ensures subset_id is a positive integer
#' - Verifies that visit_occurrence_ids are provided with corresponding person_ids
#' - Verifies that visit_detail_ids are provided with corresponding visit_occurrence_ids
#' 
#' @examples 
#' \dontrun{
#' # Remove by person_ids only
#' patients <- tibble::tibble(person_id = c(123, 456))
#' remove_patients_from_subset(patients = patients)
#'
#' # Remove specific hospital stays
#' patients <- tibble::tibble(
#'   person_id = c(123, 456),
#'   visit_occurrence_id = c(1001, 1002)
#' )
#' remove_patients_from_subset(patients = patients)
#'
#' # Remove specific unit stays
#' patients <- tibble::tibble(
#'   person_id = c(123, 456),
#'   visit_occurrence_id = c(1001, 1002),
#'   visit_detail_id = c(2001, 2002)
#' )
#' remove_patients_from_subset(patients = patients)
#' }
remove_patients_from_subset <- function(patients = tibble::tibble(), subset_id = integer()){
  
  # Get variables from other environments
  for (obj_name in c("r", "m", "output")) assign(obj_name, get(obj_name, envir = parent.frame()))
  i18n <- r$i18n
  
  # Get subset_id if it is null
  if (length(subset_id) == 0 && r$current_page == "subsets") subset_id <- eval(parse(text = "input$selected_element"), envir = parent.frame())
  
  # Check subset_id
  
  invalid_subset_id <- TRUE
  if (length(subset_id) > 0) if (is.numeric(subset_id)) if (!is.na(subset_id) & subset_id > 0) invalid_subset_id <- FALSE
  if (invalid_subset_id) return(i18n$t("invalid_subset_id_value"))
  
  # Check and process patients
  
  if (is.numeric(patients)) patients <- tibble::tibble(person_id = patients)
  patients <- patients %>% dplyr::collect()
  
  if (!"person_id" %in% colnames(patients)) patients$person_id <- NA_integer_
  if (!"visit_occurrence_id" %in% colnames(patients)) patients$visit_occurrence_id <- NA_integer_
  if (!"visit_detail_id" %in% colnames(patients)) patients$visit_detail_id <- NA_integer_
  
  patients <- patients %>% dplyr::distinct(person_id, visit_occurrence_id, visit_detail_id)
  
  if (any(!is.na(patients$visit_occurrence_id) & is.na(patients$person_id))) return(i18n$t("invalid_visit_occurrence_without_person"))
  if (any(!is.na(patients$visit_detail_id) & is.na(patients$visit_occurrence_id))) return(i18n$t("invalid_visit_detail_without_visit_occurrence"))
  
  # If there are patients to remove
  
  n_rows <- list()
  n_rows$patients <- 0
  n_rows$visit_occurrences <- 0
  n_rows$visit_details <- 0
  
  if (nrow(patients) > 0){
    
    count_and_delete_elements <- function(db, where_col, where_values, n_rows) {
      sql <- glue::glue_sql("
        SELECT 
          COUNT(DISTINCT(person_id)) AS n_patients,
          COUNT(DISTINCT(visit_occurrence_id)) AS n_visit_occurrences,
          COUNT(DISTINCT(visit_detail_id)) AS n_visit_details
        FROM subset_persons 
        WHERE {`where_col`} IN ({where_values*})", .con = db)
      n_rows_temp <- DBI::dbGetQuery(db, sql)
      
      n_rows$patients <- n_rows$patients + n_rows_temp$n_patients
      n_rows$visit_occurrences <- n_rows$visit_occurrences + n_rows_temp$n_visit_occurrences
      n_rows$visit_details <- n_rows$visit_details + n_rows_temp$n_visit_details
      
      sql <- glue::glue_sql("DELETE FROM subset_persons WHERE {`where_col`} IN ({where_values*})", .con = db)
      sql_send_statement(db, sql)
      
      return(n_rows)
    }
    
    patients_to_delete <- patients %>% dplyr::filter(!is.na(person_id) & is.na(visit_occurrence_id) & is.na(visit_detail_id))
    if (nrow(patients_to_delete) > 0) n_rows <- count_and_delete_elements(m$db, "person_id", patients_to_delete %>% dplyr::pull(person_id), n_rows)
    
    visit_occurrences_to_delete <- patients %>% dplyr::filter(!is.na(person_id) & !is.na(visit_occurrence_id) & is.na(visit_detail_id))
    if (nrow(visit_occurrences_to_delete) > 0) n_rows <- count_and_delete_elements(m$db, "visit_occurrence_id", visit_occurrences_to_delete %>% dplyr::pull(visit_occurrence_id), n_rows)
    
    visit_details_to_delete <- patients %>% dplyr::filter(!is.na(person_id) & !is.na(visit_occurrence_id) & !is.na(visit_detail_id))
    if (nrow(visit_details_to_delete) > 0) n_rows <- count_and_delete_elements(m$db, "visit_detail_id", visit_details_to_delete %>% dplyr::pull(visit_detail_id), n_rows)
  }
  
  if (n_rows$patients > 0 | n_rows$visit_occurrences > 0 | n_rows$visit_details > 0) return(paste0(
    i18n$t("remove_patients_subset_success"), " (",
    n_rows$patients, " ", tolower(i18n$t("patients")), ", ", n_rows$visit_occurrences, " ",
    tolower(i18n$t("hospital_stays")), ", ", n_rows$visit_details, " ", tolower(i18n$t("unit_stays")), ")"))
  else return(i18n$t("no_patients_to_remove_from_subset"))
}

#' @noRd
join_concepts <- function(df, concept_df, cols) {
  
  for (col in cols) {
    key <- paste0(col, "_concept_id")
    name <- paste0(col, "_concept_name")
    
    df <- df %>%
      dplyr::left_join(
        concept_df %>%
          dplyr::select(!!key := concept_id, !!name := concept_name),
        by = key,
        copy = TRUE
      ) %>%
      dplyr::relocate(!!name, .after = !!key)
  }
  
  return(df)
}

#' @noRd
count_concept_rows <- function(df, group_col, name_col) {
  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_col, name_col)))) %>%
    dplyr::summarize(n = dplyr::n(), .groups = 'drop') %>%
    dplyr::arrange(dplyr::desc(n))
}