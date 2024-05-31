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

#' Add vocabulary concept
#'
#' @description Add a new concept in the database concept table
#' @details See \href{https://ohdsi.github.io/CommonDataModel/cdm60.html#CONCEPT}{OMOP concept table} for more details.\cr\cr
#' Vocabulary_id must be present in the database when you add a new concept.\cr
#' Not null values are required for concept_name, domain_id, concept_class_id and concept_code.\cr
#' The value you provide for domain_id & concept_class_id must be present in the database.
#' @param output Shiny output variable
#' @param m A shiny::reactiveValues object, used to communicate between modules
#' @param vocabulary_id Value of vocabulary_id where the concept will be added (character)
#' @param concept_name Value of concept_name (character)
#' @param domain_id Value of domain_id, must be present in database (character)
#' @param concept_class_id Value of concept_class_id, must be present in database (character)
#' @param concept_code Value of concept_code (character)
#' @param i18n Translator object from shiny.i18n library
#' @param ns Shiny namespace
#' @examples 
#' \dontrun{
#' add_vocabulary_concept(output = output, m = m, i18n = i18n, ns = ns, vocabulary_id = "MIMIC-IV", 
#'   concept_name = "Diuresis", domain_id = "Measurement", concept_class_id = "Clinical Observation", concept_code = "Diuresis")
#' }
add_vocabulary_concept <- function(output, m = shiny::reactiveValues(), vocabulary_id = character(), 
  concept_name = character(), domain_id = character(), concept_class_id = character(), concept_code = character(), i18n = character(), ns = character()){
  
  stop_fct <- FALSE
  
  # Check vocabulary_id
  
  if (length(vocabulary_id) == 0) stop_fct <- TRUE
  else if (is.na(vocabulary_id) | vocabulary_id == "") stop_fct <- TRUE
  
  if (stop_fct){
    show_message_bar(output, "invalid_vocabulary_id_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_vocabulary_id_value"))
  }
  
  # Check concept_name
  
  if (length(concept_name) == 0) stop_fct <- TRUE
  else if (is.na(concept_name) | concept_name == "") stop_fct <- TRUE
  if (stop_fct){
    show_message_bar(output, "invalid_concept_name_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_concept_name_value"))
  }
  
  # Check domain_id
  
  if (length(domain_id) == 0) stop_fct <- TRUE
  else if (is.na(domain_id) | domain_id == "") stop_fct <- TRUE
  else {
    sql <- glue::glue_sql("SELECT * from domain WHERE domain_id == {domain_id}", .con = m$db)
    if (nrow(DBI::dbGetQuery(m$db, sql)) == 0) stop_fct <- TRUE
  }
  if (stop_fct){
    show_message_bar(output, "invalid_domain_id_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_domain_id_value"))
  }
  
  # Check concept_class_id
  
  if (length(concept_class_id) == 0) stop_fct <- TRUE
  else if (is.na(concept_class_id) | concept_class_id == "") stop_fct <- TRUE
  else {
    sql <- glue::glue_sql("SELECT * from concept_class WHERE concept_class_id = {concept_class_id}", .con = m$db)
    if (nrow(DBI::dbGetQuery(m$db, sql)) == 0) stop_fct <- TRUE
  }
  if (stop_fct){
    show_message_bar(output, "invalid_concept_class_id_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_concept_class_id_value"))
  }
  
  # Check concept_name
  
  if (length(concept_code) == 0) stop_fct <- TRUE
  else if (is.na(concept_code) | concept_code == "") stop_fct <- TRUE
  if (stop_fct){
    show_message_bar(output, "invalid_concept_code_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_concept_code_value"))
  }
  
  # Check if the vocabulary_id provided exists in database
  sql <- glue::glue_sql(paste0("SELECT * FROM vocabulary WHERE vocabulary_id = {vocabulary_id}"), .con = m$db)
  check_vocabulary_id <- DBI::dbGetQuery(m$db, sql)
  
  if (nrow(check_vocabulary_id) == 0){
    show_message_bar(output, "vocabulary_id_doesnt_exist", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("vocabulary_id_doesnt_exist"))
  }
  
  # Check if this concept already exists
  sql <- glue::glue_sql(paste0("SELECT * FROM concept WHERE concept_name = {concept_name} AND vocabulary_id = {vocabulary_id} ",
    "AND domain_id == {domain_id} AND concept_class_id = {concept_class_id}"), .con = m$db)
  check_concept <- DBI::dbGetQuery(m$db, sql)
  
  if (nrow(check_concept) > 0){
    show_message_bar(output, "vocabulary_concept_already_exists", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("vocabulary_concept_already_exists"))
  }
  
  if (nrow(check_concept) == 0){
    
    sql <- glue::glue_sql("SELECT COALESCE(MAX(concept_id), 0) FROM concept", .con = m$db)
    last_concept_id <- DBI::dbGetQuery(m$db, sql) %>% dplyr::pull()
    if (last_concept_id < 2000000000) new_concept_id = 2000000000
    else new_concept_id <- last_concept_id + 1
    
    new_data <- tibble::tribble(
      ~id, ~concept_id, ~concept_name, ~domain_id, ~vocabulary_id, ~concept_class_id, ~standard_concept, 
      ~concept_code, ~valid_start_date, ~valid_end_date, ~invalid_reason,
      get_last_row(m$db, "concept") + 1, new_concept_id, concept_name, domain_id, vocabulary_id, 
      concept_class_id, "C", concept_code, "1970-01-01", "2099-12-31", NA_character_)
    
    DBI::dbAppendTable(m$db, "concept", new_data)
  }
  
  show_message_bar(output, "vocabulary_concept_added", "success", i18n = i18n, ns = ns)
}

#' Get vocabulary concept
#'
#' @description Get a vocabulary concept, from the vocabulary_id and the concept_id or the concept_name
#' @param output Shiny output variable
#' @param m A shiny::reactiveValues object, used to communicate between modules
#' @param i18n Translator object from shiny.i18n library
#' @param ns Shiny namespace
#' @param vocabulary_id Value of vocabulary_id where the concept will be added (character)
#' @param concept_name Value of concept_name (character)
#' @param concept_id Valud of concept_id (integer)
#' @param method Method used to get the vocabulary concept. Could be "concept_name" or "concept_id" (character)
#' @examples 
#' \dontrun{
#' get_vocabulary_concept(output = output, m = m,  i18n = i18n, ns = ns,
#'   vocabulary_id = "MIMIC-IV", concept_name = "Diuresis", method = "concept_name")
#' }
get_vocabulary_concept <- function(output, m = shiny::reactiveValues(), i18n = character(), ns = character(),
  vocabulary_id = character(), concept_name = character(), concept_id = integer(), method = character()){
  
  stop_fct <- FALSE
  result <- tibble::tibble()
  
  # Check vocabulary_id
  
  if (length(vocabulary_id) == 0) stop_fct <- TRUE
  else if (is.na(vocabulary_id) | vocabulary_id == "") stop_fct <- TRUE
  
  if (stop_fct){
    show_message_bar(output, "invalid_vocabulary_id_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_vocabulary_id_value"))
  }
  
  # Check method
  
  if (length(method) == 0) stop_fct <- TRUE
  else if (method %not_in% c("concept_id", "concept_name")) stop_fct <- TRUE
  if (stop_fct){
    show_message_bar(output, "invalid_method_value", "severeWarning", i18n = i18n, ns = ns)
    stop(i18n$t("invalid_method_value"))
  }
  
  # Search by concept name
  
  if (method == "concept_name"){
    
    if (length(concept_name) == 0) stop_fct <- TRUE
    else if (is.na(concept_name) | concept_name == "") stop_fct <- TRUE
    if (stop_fct){
      show_message_bar(output, "invalid_concept_name_value", "severeWarning", i18n = i18n, ns = ns)
      stop(i18n$t("invalid_concept_name_value"))
    }
    
    sql <- glue::glue_sql("SELECT * FROM concept WHERE vocabulary_id = {vocabulary_id} AND concept_name = {concept_name}", .con = m$db)
    result <- DBI::dbGetQuery(m$db, sql)
  }
  
  # Search by concept id
  
  else if (method == "concept_id"){
    
    if (length(concept_id) == 0) stop_fct <- TRUE
    else if (is.na(concept_id)) stop_fct <- TRUE
    if (stop_fct){
      show_message_bar(output, "invalid_concept_id_value", "severeWarning", i18n = i18n, ns = ns)
      stop(i18n$t("invalid_concept_id_value"))
    }
    
    sql <- glue::glue_sql("SELECT * FROM concept WHERE vocabulary_id = {vocabulary_id} AND concept_id = {concept_id}", .con = m$db)
    result <- DBI::dbGetQuery(m$db, sql)
  }
  
  if (nrow(result) == 0) result <- tibble::tibble(id = integer(), concept_id = integer(), concept_name = character(),
    domain_id = character(), vocabulary_id = character(), concept_class_id = character(), standard_concept = character(),
    concept_code = character(), valid_start_date = character(), valid_end_date = character(), invalid_reason = character())
  
  result
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
remove_persons_from_subset <- function(output, r = shiny::reactiveValues(), m = shiny::reactiveValues(), persons = tibble::tibble(), 
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

#' Run dataset code 
#'
#' @description Runs the code of a dataset
#' @param output Shiny output variable
#' @param r A shiny::reactiveValues object, used to communicate between modules
#' @param d A shiny::reactiveValues object, used to communicate between modules. Contains data loaded from dataset code (d$patients, d$labs_vitals...).
#' @param m A shiny::reactiveValues object, used to communicate between modules
#' @param dataset_id ID of the dataset we want to load (integer)
#' @param i18n Translator object from shiny.i18n library
#' @examples 
#' \dontrun{
#' run_dataset_code(output = output, r = r, d = d, dataset_id = 3, i18n = i18n)
#' }
run_dataset_code <- function(output, r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), dataset_id = integer(), i18n = character()){
  
  # Reset r$dataset_loaded_tables
  r$dataset_loaded_tables <- tibble::tibble(table = character(), save_as = character(), read_with = character())
  
  # Get code from dataset
  tryCatch(r$code %>% dplyr::filter(category == "dataset" & link_id == dataset_id) %>% dplyr::pull(code),
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_load_code", 
        error_name = paste0("run_dataset_code - load_code - id = ", dataset_id), category = "Error", error_report = toString(e), i18n = i18n)
      stop(i18n$t("fail_load_code"))}
  )
  code <- r$code %>% dplyr::filter(category == "dataset" & link_id == dataset_id) %>% dplyr::pull(code)
  
  # Get OMOP version
  omop_version <- r$options %>% dplyr::filter(category == "dataset" & name == "omop_version" & link_id == dataset_id) %>% dplyr::pull(value)
  
  # Get dataset unique_id
  unique_id <- r$options %>% dplyr::filter(category == "dataset" & link_id == dataset_id & name == "unique_id") %>% dplyr::pull(value)
  
  # Replace %dataset_id% with real dataset_id & %omop_version%
  code <- code %>% 
    stringr::str_replace_all("%dataset_id%", as.character(dataset_id)) %>%
    stringr::str_replace_all("%omop_version%", paste0("'", omop_version, "'")) %>%
    stringr::str_replace_all("%dataset_folder%", paste0(r$app_folder, "/datasets/", unique_id)) %>%
    stringr::str_replace_all("\r", "\n") %>%
    stringr::str_replace_all("''", "'")
  
  # Reset d variables
  main_tables <- c("condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure", "measurement",
    "observation", "death", "note", "note_nlp", "specimen", "fact_relationship", "payer_plan_period", "cost", 
    "drug_era", "dose_era", "condition_era", 
    "person", "observation_period", "visit_occurrence", "visit_detail",
    "location", "care_site", "provider")
  sapply(main_tables, function(table) d[[table]] <- tibble::tibble())
  
  # Run code

  tryCatch(eval(parse(text = code)),
    error = function(e){
      if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "fail_execute_code", 
        error_name = paste0("run_dataset_code - execute_code - id = ", dataset_id), category = "Error", error_report = toString(e), i18n = i18n)
      stop(i18n$t("fail_execute_code"))}
  )
}
