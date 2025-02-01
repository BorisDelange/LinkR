#' Add patients to a subset
#'
#' @description
#' #' This function adds patients to a specific subset by inserting their `person_id` into a database table. 
#' It ensures the validity of the input parameters (`subset_id` and `patients`) and prevents duplicate entries.
#' @param output Shiny output variable
#' @param r A shiny::reactiveValues object, used to communicate between modules
#' @param m A shiny::reactiveValues object, used to communicate between modules
#' @param patients A numeric vector of `person_id`s or a `data.frame`/`tibble` containing a `person_id` column. 
#' These are the patients to be added to the subset.
#' @param subset_id An integer representing the ID of the subset to which the patients will be added. 
#' The subset ID must be positive and valid.
#' 
#' @return 
#' A character string indicating the success or failure of the operation. If successful, the message includes 
#' the number of patients added to the subset. If an error occurs, it returns an error message.
#' 
#' @details 
#' The function performs several checks:
#' - Validates `subset_id` to ensure it is a positive integer.
#' - Validates `patients` to ensure it is either a numeric vector or a `data.frame`/`tibble` with a `person_id` column.
#' - Removes patients that are already in the subset to avoid duplicates.
#' 
#' If the inputs are valid, the function inserts the new patients into the `subset_persons` table in the database.
#' 
#' @examples 
#' \dontrun{
#' # Example with a numeric vector of person IDs
#' patients <- c(123, 456, 789)
#' add_patients_to_subset(output = output, r = r, m = m, patients = patients, subset_id = 3)
#' 
#' # Example with a tibble
#' patients <- tibble::tibble(person_id = c(123, 456, 789))
#' add_patients_to_subset(output = output, r = r, m = m, patients = patients, subset_id = 3)
#' }
#' 
#' @export
add_patients_to_subset <- function(
  output, r = shiny::reactiveValues(), m = shiny::reactiveValues(), 
  patients = tibble::tibble(), subset_id = integer()){
  
  i18n <- r$i18n
  
  # Check subset_id
  
  invalid_subset_id <- TRUE
  if (length(subset_id) > 0) if (is.numeric(subset_id)) if (!is.na(subset_id) & subset_id > 0) invalid_subset_id <- FALSE
  if (invalid_subset_id) return(i18n$t("invalid_subset_id_value"))
  
  # Check patients
  
  # Check and process patients
  if (is.numeric(patients)) patients <- tibble::tibble(person_id = patients)
  else if (is.data.frame(patients)) {
    if (!"person_id" %in% colnames(patients)) {
      return(i18n$t("invalid_patients_value_person_id_col_missing"))
    }
  } else return(i18n$t("invalid_patients_value_not_numeric_vector_or_tibble"))
  
  if ("visit_detail_id" %in% colnames(patients) & "visit_occurrence_id" %in% colnames(patients)) patients <- patients %>% dplyr::distinct(person_id, visit_occurrence_id, visit_detail_id)
  else if ("visit_occurrence_id" %in% colnames(patients)) patients <- patients %>% dplyr::distinct(person_id, visit_occurrence_id)
  else patients <- patients %>% dplyr::distinct(person_id)
  
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
    
    sql <- "SELECT * FROM subset_persons LIMIT 1"
    cols_order <- colnames(DBI::dbGetQuery(m$db, sql))
    
    patients <- patients %>% dplyr::bind_cols(other_cols) %>% dplyr::select(dplyr::all_of(cols_order))
    
    DBI::dbAppendTable(m$db, "subset_persons", patients)
  }
  
  return(paste0(i18n$t("add_patients_subset_success"), " (n = ", nrow(patients), ")"))
}

#' Remove patients from a subset
#'
#' @description 
#' This function removes patients from a specific subset by deleting their `person_id` from a database table. 
#' It validates the input parameters (`subset_id` and `patients`) before performing the operation.
#' 
#' @param output Shiny output variable, used for displaying outputs in a Shiny application.
#' @param r A `shiny::reactiveValues` object, used to communicate between modules in the Shiny app. 
#' @param m A `shiny::reactiveValues` object, used to communicate between modules in the Shiny app. 
#' @param patients A `data.frame`/`tibble` containing a `person_id` column representing the patients to remove.
#' @param subset_id An integer representing the ID of the subset from which the patients will be removed. 
#' The subset ID must be positive and valid.
#' 
#' @return 
#' A success message is displayed via a Shiny message bar if the operation is successful. 
#' Errors stop the execution and display corresponding error messages.
#' 
#' @examples 
#' \dontrun{
#' patients <- tibble::tibble(person_id = c(123, 456, 789))
#' remove_patients_from_subset(output = output, r = r, m = m, patients = patients, subset_id = 3)
#' }
#' 
#' @export
remove_patients_from_subset <- function(
  output, r = shiny::reactiveValues(), m = shiny::reactiveValues(),
  patients = tibble::tibble(), subset_id = integer()){
  
  i18n <- r$i18n
  
  # Check subset_id
  
  invalid_subset_id <- TRUE
  if (length(subset_id) > 0) if (is.numeric(subset_id)) if (!is.na(subset_id) & subset_id > 0) invalid_subset_id <- FALSE
  if (invalid_subset_id) return(i18n$t("invalid_subset_id_value"))
  
  # Check patients
  
  # Check and process patients
  if (is.numeric(patients)) patients <- tibble::tibble(person_id = patients)
  else if (is.data.frame(patients)) {
    if (!"person_id" %in% names(patients)) {
      return(i18n$t("invalid_patients_value_person_id_col_missing"))
    }
    patients <- patients %>% dplyr::select(person_id)
  } else return(i18n$t("invalid_patients_value_not_numeric_vector_or_tibble"))
  
  # If there are patients to remove
  if (nrow(patients) > 0){
    
    # Remove patients from the subset
    tryCatch({ 
      existing_patients <- DBI::dbGetQuery(
          m$db, 
          glue::glue_sql(
            "SELECT person_id FROM subset_persons WHERE subset_id = {subset_id} AND person_id IN ({patients %>% dplyr::pull(person_id)*})",
            .con = m$db
          )
        )
      
      sql <- glue::glue_sql("DELETE FROM subset_persons WHERE subset_id = {subset_id} AND person_id IN ({patients %>% dplyr::pull(person_id)*})", .con = m$db)
      query <- DBI::dbSendStatement(m$db, sql)
      DBI::dbClearResult(query)
    }, 
    error = function(e) return(paste0("\n", now(), " - remove_patients_from_subset - error removing patients from subset - ", toString(e)))
    )
  }
  
  return(paste0(i18n$t("remove_patients_subset_success"), " (n = ", nrow(existing_patients), ")"))
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