#' Add patients to a subset
#'
#' @description Add patients to a subset
#' @param output Shiny output variable
#' @param r A shiny::reactiveValues object, used to communicate between modules
#' @param m A shiny::reactiveValues object, used to communicate between modules
#' @param patients numeric vector containing patients / persons (numeric)
#' @param subset_id ID of the subset (integer)
#' @param i18n Translator object from shiny.i18n library
#' @param ns Shiny namespace
#' @examples 
#' \dontrun{
#' patients <- c(123, 456, 789)
#' add_patients_to_subset(output = output, r = r, m = m, patients = patients, subset_id = 3, i18n = i18n, ns = ns)
#' }
add_patients_to_subset <- function(output, r = shiny::reactiveValues(), m = shiny::reactiveValues(), patients = tibble::tibble(),
  subset_id = integer(), i18n = character(), ns = character()){
  
  # Check subset_id
  
  invalid_subset_id <- TRUE
  if (length(subset_id) > 0) if (is.numeric(subset_id)) if (!is.na(subset_id) & subset_id > 0) invalid_subset_id <- FALSE
  if (invalid_subset_id){
    show_message_bar(output, "invalid_subset_id_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_subset_id_value"))
  }
  
  # Check patients
  
  invalid_patients <- TRUE
  if (length(patients) > 0) if (is.numeric(subset_id)) invalid_patients <- FALSE
  if (invalid_patients){
    show_message_bar(output, "invalid_patients_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_patients_value"))
  }
  
  # Transform as tibble
  patients <- tibble::tibble(person_id = patients)
  
  # Keep only persons not already in the subset
  actual_patients <- DBI::dbGetQuery(m$db, glue::glue_sql("SELECT person_id FROM subset_persons WHERE subset_id = {subset_id}", .con = m$db))
  patients <- patients %>% dplyr::anti_join(actual_patients, by = "person_id")
  
  # If there are patients to add
  if (nrow(patients) > 0){
    
    # Add new patients in the subset
    tryCatch({
      last_id <- DBI::dbGetQuery(m$db, "SELECT COALESCE(MAX(id), 0) FROM subset_persons") %>% dplyr::pull()
      other_cols <- tibble::tibble(id = 1:nrow(patients) + last_id, subset_id = subset_id, creator_id = m$user_id, datetime = now(), deleted = FALSE)
      patients <- patients %>% dplyr::bind_cols(other_cols) %>% dplyr::relocate(person_id, .after = "subset_id")
      DBI::dbAppendTable(m$db, "subset_persons", patients)
    },
      error = function(e){
        cat(paste0("add_patients_to_subset - error_inserting_data - ", toString(e)))
        stop(i18n$t("error_inserting_data"))}
    )
  }
  
  show_message_bar(output, "add_persons_subset_success", "success", i18n = i18n, ns = ns)
}

#' Remove patients from a subset
#'
#' @description Remove patients from a subset
#' @param output Shiny output variable
#' @param r A shiny::reactiveValues object, used to communicate between modules
#' @param m A shiny::reactiveValues object, used to communicate between modules
#' @param persons data variable containing patients / persons (data.frame / tibble)
#' @param subset_id ID of subset (integer)
#' @param i18n Translator object from shiny.i18n library
#' @param ns Shiny namespace
#' @examples 
#' \dontrun{
#' persons <- tibble::tribble(~patient_id, 123L, 456L, 789L)
#' remove_persons_from_subset(output = output, r = r, m = m, persons = persons, subset_id = 3, i18n = i18n, ns = ns)
#' }
remove_patients_from_subset <- function(output, r = shiny::reactiveValues(), m = shiny::reactiveValues(), persons = tibble::tibble(), 
  subset_id = integer(), i18n = character(), ns = character()){
  
  # Check subset_id
  
  if (length(subset_id) == 0){
    show_message_bar(output, "invalid_subset_id_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_subset_id_value"))
  }
  
  tryCatch(subset_id <- as.integer(subset_id), 
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "invalid_subset_id_value", 
        error_name = paste0("remove_persons_from_subset - invalid_subset_id_value - id = ", subset_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns)
      stop(i18n$t("invalid_subset_id_value"))},
    warning = function(w) if (nchar(w[1]) > 0){
      report_bug(r = r, output = output, error_message = "invalid_subset_id_value", 
        error_name = paste0("remove_persons_from_subset - invalid_subset_id_value - id = ", subset_id), category = "Warning", error_report = toString(w), i18n = i18n, ns = ns)
      stop(i18n$t("invalid_subset_id_value"))}
  )
  
  if (is.na(subset_id)){
    show_message_bar(output, "invalid_subset_id_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_subset_id_value"))
  }
  
  var_cols <- tibble::tribble(
    ~name, ~type,
    "person_id", "integer")
  
  # Check col names
  if (!identical(names(persons), "person_id")){
    show_message_bar(output, "invalid_col_names", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("valid_col_names_are"), toString(var_cols %>% dplyr::pull(name)))
  }
  
  # Check col types
  sapply(1:nrow(var_cols), function(i){
    var_name <- var_cols[[i, "name"]]
    if (var_cols[[i, "type"]] == "integer" & !is.integer(persons[[var_name]])){
      show_message_bar(output, "invalid_col_types", "severeWarning", i18n = i18n, ns = ns)
      stop(paste0(i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_integer")))
    }
  })
  
  # Transform as tibble
  tryCatch(persons <- tibble::as_tibble(persons), 
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_transforming_tibble", 
        error_name = paste0("remove_persons_from_subset - error_transforming_tibble - id = ", subset_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns)
      stop(i18n$t("error_transforming_tibble"))},
    warning = function(w) if (nchar(w[1]) > 0){
      report_bug(r = r, output = output, error_message = "error_transforming_tibble", 
        error_name = paste0("remove_persons_from_subset - error_transforming_tibble - id = ", subset_id), category = "Warning", error_report = toString(w), i18n = i18n, ns = ns)
      stop(i18n$t("error_transforming_tibble"))}
  )
  
  tryCatch({ 
    sql <- glue::glue_sql(paste0("DELETE FROM subset_persons WHERE subset_id = {subset_id} AND ",
      "person_id IN ({persons %>% dplyr::pull(person_id)*})"), .con = m$db)
    query <- DBI::dbSendStatement(m$db, sql)
    DBI::dbClearResult(query)
  }, 
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_removing_persons_from_subset", 
        error_name = paste0("remove_persons_from_subset - error_removing_persons_from_subset - id = ", subset_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns)
      stop(i18n$t("error_removing_persons_from_subset"))},
    warning = function(w) if (nchar(w[1]) > 0){
      report_bug(r = r, output = output, error_message = "error_removing_persons_from_subset", 
        error_name = paste0("remove_persons_from_subset - error_removing_persons_from_subset - id = ", subset_id), category = "Warning", error_report = toString(w), i18n = i18n, ns = ns)
      stop(i18n$t("error_removing_persons_from_subset"))}
  )
  
  show_message_bar(output, "remove_persons_subset_success", "success", i18n = i18n, ns = ns)
}