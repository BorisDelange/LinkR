#' Import a dataset
#' 
#' @description Imports a dataset into the application using the OMOP Common Data Model.
#' @param r A `shiny::reactiveValues` object used for communication between modules.
#' @param d A `shiny::reactiveValues` object used for communication between modules.
#' @param dataset_id The ID of the dataset, used to create a specific dataset folder in the application directories. Must be an integer.
#' @param omop_version The OMOP version of the imported data. Accepted values are `"5.3"`, `"5.4"`, and `"6.0"`. Defaults to `"5.4"`.
#' @param data_source The source of the data. Accepted values are `"db"` for a database connection or `"disk"` for disk storage. Defaults to `"disk"`.
#' @param data_folder The folder containing the data. Must be a character string.
#' @param con A `DBI::dbConnect` object representing the database connection, required if `data_source` is `"db"`.
#' @param load_tables A character vector specifying which OMOP tables to load.
#' @param ... Additional arguments to be passed to the `vroom` function (e.g., `delim`).
#' @details ...
import_dataset <- function(
  r, d, dataset_id = integer(), omop_version = "5.4", data_source = "disk", data_folder = character(), con, load_tables = character(), ...
){
  
  i18n <- r$i18n
  
  # Check arguments ----

  # Check dataset_id
  error_dataset_id <- TRUE
  if (length(dataset_id) > 0) if (!is.na(dataset_id)){
    tryCatch(dataset_id <- as.integer(dataset_id))
    error_dataset_id <- FALSE
  }
  if (error_dataset_id) return(i18n$t("invalid_dataset_id_value"))
  
  # Check data_source
  if (data_source %not_in% c("db", "disk")) return(i18n$t("dataset_invalid_data_source"))
  
  # Check omop_version
  if (omop_version %not_in% c("5.3", "5.4", "6.0")) return(i18n$t("invalid_omop_version"))
  
  # Check data_folder
  if (data_source == "disk" & length(data_folder) == 0) return(i18n$t("invalid_data_folder"))
  
  # Load data ----
  
  # Col types depending on OMOP CDM version
  
  col_types <- list(
    "person" = "iiiiiTTiiiiiccicici",
    "observation_period" = "iiDDi",
    "visit_occurrence" = "iiiDTDTiiiciicici",
    "visit_detail" = "iiiDTDTiiiciicciiii",
    "condition_occurrence" = "iiiDTDTiiciiicic",
    "drug_exposure" = "iiiDTDTDiciniciciiicicc",
    "procedure_occurrence" = "iiiDTiiiiiicic",
    "device_exposure" = "iiiDTDTiciiiici",
    "measurement" = "iiiDTciiniinniiicicc",
    "observation" = "iiiDTinciiiiiicicciiT",
    "death" = "iDTiici",
    "note" = "iiiiDTiicciiiiic",
    "note_nlp" = "iiiccciicDTccc",
    "specimen" = "iiiiDTniiiccccc",
    "fact_relationship" = "iiiii",
    "location" = "icccccccnn",
    "location_history" = "iiciDD",
    "care_site" = "iciicc",
    "provider" = "iccciiiiccici",
    "payer_plan_period" = "iiiDDiciiciiciicicici",
    "cost" = "iiiiiiicinDDDiicci",
    "drug_era" = "iiiTTii",
    "dose_era" = "iiiinTT",
    "condition_era" = "iiiTTi"
  )
  
  if (omop_version == "5.3"){
    col_types$person <- "iiiiiTiiiiiccicici"
    col_types$visit_detail <- "iiiDTDTiiiciciciiii"
    col_types$observation <-  "iiiDTinciiiiiicicc"
    col_types$location <-  "iccccccc"
    col_types$drug_era <- "iiiDDii"
    col_types$dose_era <- "iiiinDD"
    col_types$condition_era <- "iiiDDi"
  }
  
  else if (omop_version == "5.4"){
    col_types$person <- "iiiiiTiiiiiccicici"
    col_types$observation <-  "iiiDTinciiiiiicicccii"
    col_types$drug_era <- "iiiDDii"
    col_types$dose_era <- "iiiinDD"
    col_types$condition_era <- "iiiDDi"
  }
  
  # Load tables from load_tables argument if specified. If not specified, load all OMOP tables.
  if (length(load_tables) == 0) load_tables <- names(col_types)
  if (length(load_tables) > 0) load_tables <- intersect(load_tables, names(col_types))
  
  # Record loaded tables
  loaded_tables <- c()
  
  # Folder where data will be saved
  
  data_app_folder <- paste0(r$app_folder, "/datasets_files/", dataset_id)
  if (!dir.exists(data_app_folder)) dir.create(data_app_folder)
  
  # Disconnect db
  # if (length(d$con) > 0) if (DBI::dbIsValid(d$con)) DBI::dbDisconnect(d$con)
  
  ## Import data from disk files ----
  
  if (data_source == "disk"){
    
    # List files of provided folder
    
    file_names <- list.files(path = data_folder)
    
    if (length(file_names) == 0) return(i18n$t("error_getting_files_from_data_folder"))
      
    tryCatch({
      
      connection_loaded <- FALSE
      
      for (file_name in file_names){
        
        table <- sub("\\.[^.]*$", "", file_name)
        file_ext <- sub(".*\\.", "", tolower(file_name))
        
        # If no file_ext, consider it is a folder containing parquet files
        if (file_ext == table) file_ext <- ""
        
        # Check if this is an OMOP table
        
        if (table %in% load_tables){
        
          if (file_ext %in% c("parquet", "")){
            
            # if duckdb not declared, create connection
            if (!connection_loaded){
              d$con <- DBI::dbConnect(duckdb::duckdb())
              connection_loaded <- TRUE
            }
            # if (length(d$con) == 0) d$con <- DBI::dbConnect(duckdb::duckdb())
            
            duckdb::duckdb_unregister_arrow(d$con, table)
            duckdb::duckdb_register_arrow(d$con, table, arrow::open_dataset(paste0(data_folder, "/", file_name)))
            
            d[[table]] <- dplyr::tbl(d$con, table)
            
            loaded_tables <- c(loaded_tables, table)
          }
          
          else if (file_ext == "csv"){
            
            d[[table]] <- vroom::vroom(paste0(data_folder, "/", file_name), col_types = col_types[[table]], progress = FALSE, ...)
            
            loaded_tables <- c(loaded_tables, table)
          }
        }
      }
    },
    error = function(e) return(paste0(i18n$t("error_loading_data"), " - ", toString(e))))
  }
  
  ## Import data from database connection ----
  
  else if (data_source == "db"){
    
    # Test connection
    if (!DBI::dbIsValid(con)) return(i18n$t("dataset_error_with_db_connection"))
    
    d$con <- con
    
    tables <- DBI::dbListTables(con)
    
    for (table in tables){
      if (table %in% load_tables){
        d[[table]] <- dplyr::tbl(con, table)
        
        loaded_tables <- c(loaded_tables, table)
      }
    }
  }
  
  # Transform data ----
  
  ## Select cols ----
  
  data_cols <- list(
    person = c("person_id", "gender_concept_id", "year_of_birth", "month_of_birth", "day_of_birth", "birth_datetime", "death_datetime", "race_concept_id", "ethnicity_concept_id", "location_id", "provider_id", "care_site_id", "person_source_value", "gender_source_value", "gender_source_concept_id", "race_source_value", "race_source_concept_id", "ethnicity_source_value", "ethnicity_source_concept_id"),
    visit_detail = c("visit_detail_id", "person_id", "visit_detail_concept_id", "visit_detail_start_date", "visit_detail_start_datetime", "visit_detail_end_date", "visit_detail_end_datetime", "visit_detail_type_concept_id", "provider_id", "care_site_id", "visit_detail_source_value", "visit_detail_source_concept_id", "admitted_from_source_value", "admitted_from_concept_id", "discharge_to_source_value", "discharge_to_concept_id", "preceding_visit_detail_id", "visit_detail_parent_id", "visit_occurrence_id"),
    note = c("note_id", "person_id", "note_event_id", "note_event_field_concept_id", "note_date", "note_datetime", "note_type_concept_id", "note_class_concept_id", "note_title", "note_text", "encoding_concept_id", "language_concept_id", "provider_id", "visit_occurrence_id", "visit_detail_id", "note_source_value"),
    drug_era = c("drug_era_id", "person_id", "drug_concept_id", "drug_era_start_datetime", "drug_era_end_datetime", "drug_exposure_count", "gap_days"),
    dose_era = c("dose_era_id", "person_id", "drug_concept_id", "unit_concept_id", "dose_value", "dose_era_start_datetime", "dose_era_end_datetime"),
    condition_era = c("condition_era_id", "person_id", "condition_concept_id", "condition_era_start_datetime", "condition_era_end_datetime", "condition_occurrence_count"),
    visit_occurrence = c("visit_occurrence_id", "person_id", "visit_concept_id", "visit_start_date", "visit_start_datetime", "visit_end_date", "visit_end_datetime", "visit_type_concept_id", "provider_id", "care_site_id", "visit_source_value", "visit_source_concept_id", "admitted_from_concept_id", "admitted_from_source_value", "discharge_to_concept_id", "discharge_to_source_value", "preceding_visit_occurrence_id"),
    location = c("location_id", "address_1", "address_2", "city", "state", "zip", "county", "location_source_value", "latitude", "longitude"),
    observation_period = c("observation_period_id", "person_id", "observation_period_start_date", "observation_period_end_date", "period_type_concept_id"),
    condition_occurrence = c("condition_occurrence_id", "person_id", "condition_concept_id", "condition_start_date", "condition_start_datetime", "condition_end_date", "condition_end_datetime", "condition_type_concept_id", "condition_status_concept_id", "stop_reason", "provider_id", "visit_occurrence_id", "visit_detail_id", "condition_source_value", "condition_source_concept_id", "condition_status_source_value"),
    drug_exposure = c("drug_exposure_id", "person_id", "drug_concept_id", "drug_exposure_start_date", "drug_exposure_start_datetime", "drug_exposure_end_date", "drug_exposure_end_datetime", "verbatim_end_date", "drug_type_concept_id", "stop_reason", "refills", "quantity", "days_supply", "sig", "route_concept_id", "lot_number", "provider_id", "visit_occurrence_id", "visit_detail_id", "drug_source_value", "drug_source_concept_id", "route_source_value", "dose_unit_source_value"),
    procedure_occurrence = c("procedure_occurrence_id", "person_id", "procedure_concept_id", "procedure_date", "procedure_datetime", "procedure_type_concept_id", "modifier_concept_id", "quantity", "provider_id", "visit_occurrence_id", "visit_detail_id", "procedure_source_value", "procedure_source_concept_id", "modifier_source_value"),
    device_exposure = c("device_exposure_id", "person_id", "device_concept_id", "device_exposure_start_date", "device_exposure_start_datetime", "device_exposure_end_date", "device_exposure_end_datetime", "device_type_concept_id", "unique_device_id", "quantity", "provider_id", "visit_occurrence_id", "visit_detail_id", "device_source_value", "device_source_concept_id"),
    measurement = c("measurement_id", "person_id", "measurement_concept_id", "measurement_date", "measurement_datetime", "measurement_time", "measurement_type_concept_id", "operator_concept_id", "value_as_number", "value_as_concept_id", "unit_concept_id", "range_low", "range_high", "provider_id", "visit_occurrence_id", "visit_detail_id", "measurement_source_value", "measurement_source_concept_id", "unit_source_value", "value_source_value"),
    observation = c("observation_id", "person_id", "observation_concept_id", "observation_date", "observation_datetime", "observation_type_concept_id", "value_as_number", "value_as_string", "value_as_concept_id", "qualifier_concept_id", "unit_concept_id", "provider_id", "visit_occurrence_id", "visit_detail_id", "observation_source_value", "observation_source_concept_id", "unit_source_value", "qualifier_source_value", "observation_event_id", "obs_event_field_concept_id", "value_as_datetime"),
    death = c("person_id", "death_date", "death_datetime", "death_type_concept_id", "cause_concept_id", "cause_source_value", "cause_source_concept_id"),
    note_nlp = c("note_nlp_id", "note_id", "section_concept_id", "snippet", "offset", "lexical_variant", "note_nlp_concept_id", "note_nlp_source_concept_id", "nlp_system", "nlp_date", "nlp_datetime", "term_exists", "term_temporal", "term_modifiers"),
    specimen = c("specimen_id", "person_id", "specimen_concept_id", "specimen_type_concept_id", "specimen_date", "specimen_datetime", "quantity", "unit_concept_id", "anatomic_site_concept_id", "disease_status_concept_id", "specimen_source_id", "specimen_source_value", "unit_source_value", "anatomic_site_source_value", "disease_status_source_value"),
    fact_relationship = c("domain_concept_id_1", "fact_id_1", "domain_concept_id_2", "fact_id_2", "relationship_concept_id"),
    survey_conduct = c("survey_conduct_id", "person_id", "survey_concept_id", "survey_start_date", "survey_start_datetime", "survey_end_date", "survey_end_datetime", "provider_id", "assisted_concept_id", "respondent_type_concept_id", "timing_concept_id", "collection_method_concept_id", "assisted_source_value", "respondent_type_source_value", "timing_source_value", "collection_method_source_value", "survey_source_value", "survey_source_concept_id", "survey_source_identifier", "validated_survey_concept_id", "validated_survey_source_value", "survey_version_number", "visit_occurrence_id", "response_visit_occurrence_id"),
    location_history = c("location_id", "relationship_type_concept_id", "domain_id", "entity_id", "start_date", "end_date"),
    care_site = c("care_site_id", "care_site_name", "place_of_service_concept_id", "location_id", "care_site_source_value", "place_of_service_source_value"),
    provider = c("provider_id", "provider_name", "npi", "dea", "specialty_concept_id", "care_site_id", "year_of_birth", "gender_concept_id", "provider_source_value", "specialty_source_value", "specialty_source_concept_id", "gender_source_value", "gender_source_concept_id"),
    payer_plan_period = c("payer_plan_period_id", "person_id", "payer_plan_period_start_date", "payer_plan_period_end_date", "payer_concept_id", "payer_source_value", "payer_source_concept_id", "plan_concept_id", "plan_source_value", "plan_source_concept_id", "sponsor_concept_id", "sponsor_source_value", "sponsor_source_concept_id", "family_source_value", "stop_reason_concept_id", "stop_reason_source_value", "stop_reason_source_concept_id"),
    cost = c("cost_id", "person_id", "cost_event_id", "cost_event_field_concept_id", "cost_concept_id", "cost_type_concept_id", "cost_source_concept_id", "cost_source_value", "currency_concept_id", "cost", "incurred_date", "billed_date", "paid_date", "revenue_code_concept_id", "drg_concept_id", "revenue_code_source_value", "drg_source_value", "payer_plan_period_id")
  )
  
  if (omop_version == "5.3"){
    data_cols$person <- c("person_id", "gender_concept_id", "year_of_birth", "month_of_birth", "day_of_birth", "birth_datetime", "race_concept_id", "ethnicity_concept_id", "location_id", "provider_id", "care_site_id", "person_source_value", "gender_source_value", "gender_source_concept_id", "race_source_value", "race_source_concept_id", "ethnicity_source_value", "ethnicity_source_concept_id")
    data_cols$visit_occurrence = c("visit_occurrence_id", "person_id", "visit_concept_id", "visit_start_date", "visit_start_datetime", "visit_end_date", "visit_end_datetime", "visit_type_concept_id", "provider_id", "care_site_id", "visit_source_value", "visit_source_concept_id", "admitting_source_concept_id", "admitting_source_value", "discharge_to_concept_id", "discharge_to_source_value", "preceding_visit_occurrence_id")
    data_cols$visit_detail <- c("visit_detail_id", "person_id", "visit_detail_concept_id", "visit_detail_start_date", "visit_detail_start_datetime", "visit_detail_end_date", "visit_detail_end_datetime", "visit_detail_type_concept_id", "provider_id", "care_site_id", "visit_detail_source_value", "visit_detail_source_concept_id", "admitting_source_value", "admitting_source_concept_id", "discharge_to_source_value", "discharge_to_concept_id", "preceding_visit_detail_id", "visit_detail_parent_id", "visit_occurrence_id")
    data_cols$note <- c("note_id", "person_id", "note_date", "note_datetime", "note_type_concept_id", "note_class_concept_id", "note_title", "note_text", "encoding_concept_id", "language_concept_id", "provider_id", "visit_occurrence_id", "visit_detail_id", "note_source_value")
    data_cols$observation <- c("observation_id", "person_id", "observation_concept_id", "observation_date", "observation_datetime", "observation_type_concept_id", "value_as_number", "value_as_string", "value_as_concept_id", "qualifier_concept_id", "unit_concept_id", "provider_id", "visit_occurrence_id", "visit_detail_id", "observation_source_value", "observation_source_concept_id", "unit_source_value", "qualifier_source_value")
    data_cols$location <- c("location_id", "address_1", "address_2", "city", "state", "zip", "county", "location_source_value")
    data_cols$drug_era <- c("drug_era_id", "person_id", "drug_concept_id", "drug_era_start_date", "drug_era_end_date", "drug_exposure_count", "gap_days")
    data_cols$dose_era <- c("dose_era_id", "person_id", "drug_concept_id", "unit_concept_id", "dose_value", "dose_era_start_date", "dose_era_end_date")
    data_cols$condition_era <- c("condition_era_id", "person_id", "condition_concept_id", "condition_era_start_date", "condition_era_end_date", "condition_occurrence_count")
    data_cols$cost <- c("cost_id", "cost_event_id", "cost_domain_id", "cost_type_concept_id", "currency_concept_id", "total_charge", "total_cost", "total_paid", "paid_by_payer", "paid_by_patient", "paid_patient_copay", "paid_patient_coinsurance", "paid_patient_deductible", "paid_by_primary", "paid_ingredient_cost", "paid_dispensing_fee", "payer_plan_period_id", "amount_allowed", "revenue_code_concept_id", "revenue_code_source_value", "drg_concept_id", "drg_source_value")
  }
  
  if (omop_version == "5.4"){
    data_cols$person <- c("person_id", "gender_concept_id", "year_of_birth", "month_of_birth", "day_of_birth", "birth_datetime", "race_concept_id", "ethnicity_concept_id", "location_id", "provider_id", "care_site_id", "person_source_value", "gender_source_value", "gender_source_concept_id", "race_source_value", "race_source_concept_id", "ethnicity_source_value", "ethnicity_source_concept_id")
    data_cols$visit_detail <- c("visit_detail_id", "person_id", "visit_detail_concept_id", "visit_detail_start_date", "visit_detail_start_datetime", "visit_detail_end_date", "visit_detail_end_datetime", "visit_detail_type_concept_id", "provider_id", "care_site_id", "visit_detail_source_value", "visit_detail_source_concept_id", "admitted_from_concept_id", "admitted_from_source_value", "discharged_to_source_value", "discharged_to_concept_id", "preceding_visit_detail_id", "parent_visit_detail_id", "visit_occurrence_id")
    data_cols$observation <- c("observation_id", "person_id", "observation_concept_id", "observation_date", "observation_datetime", "observation_type_concept_id", "value_as_number", "value_as_string", "value_as_concept_id", "qualifier_concept_id", "unit_concept_id", "provider_id", "visit_occurrence_id", "visit_detail_id", "observation_source_value", "observation_source_concept_id", "unit_source_value", "qualifier_source_value", "value_source_value", "observation_event_id", "obs_event_field_concept_id")
    data_cols$location = c("location_id", "address_1", "address_2", "city", "state", "zip", "county", "location_source_value", "country_concept_id", "latitude", "longitude")
    data_cols$drug_era <- c("drug_era_id", "person_id", "drug_concept_id", "drug_era_start_date", "drug_era_end_date", "drug_exposure_count", "gap_days")
    data_cols$dose_era <- c("dose_era_id", "person_id", "drug_concept_id", "unit_concept_id", "dose_value", "dose_era_start_date", "dose_era_end_date")
    data_cols$condition_era <- c("condition_era_id", "person_id", "condition_concept_id", "condition_era_start_date", "condition_era_end_date", "condition_occurrence_count")
    data_cols$cost <- c("cost_id", "cost_event_id", "cost_domain_id", "cost_type_concept_id", "currency_concept_id", "total_charge", "total_cost", "total_paid", "paid_by_payer", "paid_by_patient", "paid_patient_copay", "paid_patient_coinsurance", "paid_patient_deductible", "paid_by_primary", "paid_ingredient_cost", "paid_dispensing_fee", "payer_plan_period_id", "amount_allowed", "revenue_code_concept_id", "revenue_code_source_value", "drg_concept_id", "drg_source_value")
  }
  
  loaded_data <- tibble::tibble(table = character(), n_rows = integer())
  
  for (table in loaded_tables){
    
    tryCatch({
      d[[table]] <- d[[table]] %>% dplyr::select(data_cols[[table]])
      
      loaded_data <- loaded_data %>% dplyr::bind_rows(tibble::tibble(table = table, n_rows = d[[table]] %>% dplyr::count() %>% dplyr::pull()))
    },
    error = function(e) return(paste0(i18n$t("error_transforming_cols"), " - table = ", table, " - ", toString(e))))
  }
  
  return(print(loaded_data, n = 100))
}

#' @noRd
import_vocabulary_table <- function(r = shiny::reactiveValues(), m = shiny::reactiveValues(), table_name = character(), data = tibble::tibble(), add_new_vocabularies = FALSE){
  
  i18n <- r$i18n
  
  # Reset count rows
  rows_inserted <- 0L
  
  if (table_name %not_in% c("concept", "vocabulary", "domain", "concept_class", "concept_relationship", "relationship", "concept_synonym", "concept_ancestor", "drug_strength")){
    cat(paste0("\n", i18n$t("invalid_vocabulary_table")))
    return(c(0L, i18n$t("invalid_vocabulary_table")))
  }
  
  data <- tibble::as_tibble(data)
  
  # Concept tables col types
  col_types <- list(
    concept = "iccccccccc",
    vocabulary = "cccci",
    domain = "cci",
    concept_class = "cci",
    concept_relationship = "iicccc",
    relationship = "ccccci",
    concept_synonym = "ici",
    concept_ancestor = "iiii",
    drug_strength = "iinininiiccc"
  )
  
  col_names <- list(
    concept = c("concept_id", "concept_name", "domain_id", "vocabulary_id", "concept_class_id", "standard_concept", "concept_code", "valid_start_date", "valid_end_date", "invalid_reason"),
    vocabulary = c("vocabulary_id", "vocabulary_name", "vocabulary_reference", "vocabulary_version", "vocabulary_concept_id"),
    domain = c("domain_id", "domain_name", "domain_concept_id"),
    concept_class = c("concept_class_id", "concept_class_name", "concept_class_concept_id"),
    concept_relationship = c("concept_id_1", "concept_id_2", "relationship_id", "valid_start_date", "valid_end_date", "invalid_reason"),
    relationship = c("relationship_id", "relationship_name", "is_hierarchical", "defines_ancestry", "reverse_relationship_id", "relationship_concept_id"),
    concept_synonym = c("concept_id", "concept_synonym_name", "language_concept_id"),
    concept_ancestor = c("ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation", "max_levels_of_separation"),
    drug_strength = c("drug_concept_id", "ingredient_concept_id", "amount_value", "amount_unit_concept_id", "numerator_value", "numerator_unit_concept_id",
      "denominator_value", "denominator_unit_concept_id", "box_size", "valid_start_date", "valid_end_date", "invalid_reason")
  )
  if (!identical(colnames(data), col_names[[table_name]])){
    cat(paste0("\n", i18n$t("invalid_col_names")))
    return(c(0L, i18n$t("invalid_col_names")))
  }
  
  # Transform col types
  transform_column <- function(column, type){
    switch(
      type,
      "c" = as.character(column),
      "i" = bit64::as.integer64(column),
      "n" = as.numeric(column)
    )
  }
  
  types <- col_types[[table_name]]
  
  for (i in seq_along(types)) {
    col_name <- names(data)[i]
    col_type <- substr(types, i, i)
    data[[col_name]] <- transform_column(data[[col_name]], col_type)
  }
  
  # Add data to database

  tables_with_primary_key <- c("concept", "vocabulary", "domain", "concept_class", "relationship")

  # Case 1 : table has a primary key, add data not already in database, filtered by primary key

  if (table_name %in% tables_with_primary_key){

  # Check if there are duplicates in primary_keys

    primary_keys_duplicates <- data %>% dplyr::group_by_at(paste0(table_name, "_id")) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
    if (primary_keys_duplicates > 0){
      cat(paste0("\n", i18n$t("error_multiple_primary_keys")))
      return(c(0L, i18n$t("error_multiple_primary_keys")))
    }

    if (table_name == "vocabulary") sql <- glue::glue_sql("SELECT vocabulary_id FROM vocabulary WHERE deleted IS FALSE", .con = m$db)
    else sql <- glue::glue_sql("SELECT {`paste0(table_name, '_id')`} FROM {`table_name`}", .con = m$db)
    actual_data <- DBI::dbGetQuery(m$db, sql)

    # Get items to insert with an anti-join
    data_to_insert <- data %>% dplyr::anti_join(actual_data, by = paste0(table_name, "_id"))
  }

  # Case 2 : table has no primary key, add data if not already in database filtered with selected cols

  else {

    data_duplicates_cols <- switch(table_name,
      "concept_relationship" = c("concept_id_1", "concept_id_2", "relationship_id"),
      "concept_synonym" = c("concept_id", "concept_synonym_name", "language_concept_id"),
      "concept_ancestor" = c("ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation", "max_levels_of_separation"),
      "drug_strength" = c("drug_concept_id", "ingredient_concept_id", "amount_value", "amount_unit_concept_id", "numerator_value", "numerator_unit_concept_id",
        "denominator_value", "denominator_unit_concept_id", "box_size")
    )

    data_duplicates <- data %>% dplyr::group_by_at(data_duplicates_cols) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()

    if (data_duplicates > 0){
      cat(paste0("\n", i18n$t("error_multiple_identical_values")))
      return(c(0L, i18n$t("error_multiple_identical_values")))
    }

    sql <- glue::glue_sql("SELECT {`data_duplicates_cols`*} FROM {`table_name`}", .con = m$db)
    actual_data <- DBI::dbGetQuery(m$db, sql)

    # Get items to insert with an anti-join
    data_to_insert <- data %>% dplyr::anti_join(actual_data, by = data_duplicates_cols)
  }

  # Add cols if table is vocabulary
  if (table_name == "vocabulary"){
    data_to_insert <-
      data_to_insert %>%
      dplyr::mutate(id = get_last_row(m$db, "vocabulary") + dplyr::row_number(), .before = "vocabulary_id") %>%
      dplyr::mutate(data_source_id = NA_character_, display_order = NA_integer_, creator_id = NA_integer_, creation_datetime = now(), update_datetime = now(), deleted = FALSE)
  }

  # Insert data

  # Add nrow rows_inserted
  rows_inserted <- as.integer(nrow(data_to_insert))

  # Last ID in vocabulary table
  last_id <- get_last_row(m$db, table_name)

  if (nrow(data_to_insert) == 0){
    cat(paste0("\n", i18n$t("no_data_to_insert_in_table"), " ", table_name))
    return(c(0L, i18n$t("no_data_to_insert")))
  }

  else {
    data_to_insert <- data_to_insert %>% dplyr::mutate(id = 1:dplyr::n() + last_id, .before = 1)
    
    # For vocabulary table, we have to add rows in options and code tables
    if (table_name == "vocabulary"){
      
      if (nrow(data_to_insert) > 0){
        
        new_data <- list()
        last_row <- list()
        
        for (table in c("vocabulary", "options", "code")){
          last_row[[table]] <- get_last_row(m$db, table)
          new_data[[table]] <- tibble::tibble()
        }
        
        for (i in 1:nrow(data_to_insert)){
          
          row <- data_to_insert[i, ]
          
          new_data$vocabulary <- new_data$vocabulary %>% dplyr::bind_rows(
            row %>% dplyr::transmute(
              id = last_row$vocabulary + 1, vocabulary_id, vocabulary_name, vocabulary_reference, vocabulary_version, vocabulary_concept_id,
              data_source_id = "", display_order = NA_integer_, creator_id = r$user_id, creation_datetime = now(), update_datetime = now(), deleted = FALSE
            )
          )
          
          new_data$options <- new_data$options %>% dplyr::bind_rows(
            tibble::tribble(
              ~name, ~value, ~value_num,
              "users_allowed_read_group", "everybody", 1,
              "user_allowed_read", "", r$user_id,
              "version", "0.0.1.9000", NA_integer_,
              "unique_id", paste0(sample(c(0:9, letters[1:6]), 64, TRUE), collapse = ''), NA_integer_,
              "author", "", NA_integer_,
              "downloaded_from", "", NA_integer_,
              "downloaded_from_url", "", NA_integer_
            ) %>%
              dplyr::bind_rows(
                r$languages %>%
                  tidyr::crossing(name = c("description", "category", "name")) %>%
                  dplyr::mutate(
                    name = paste0(name, "_", code),
                    value = ifelse(grepl("name_", name), row$vocabulary_id, ""),
                    value_num = NA_integer_
                  ) %>%
                  dplyr::select(-code, -language)
              ) %>%
              dplyr::mutate(id = last_row$options + dplyr::row_number(), category = "vocabulary", link_id = last_row$vocabulary + 1, .before = "name") %>%
              dplyr::mutate(creator_id = r$user_id, datetime = now(), deleted = FALSE)
          )
          
          new_data$code <- new_data$code %>% dplyr::bind_rows(
            tibble::tibble(
              id = last_row$code + 1, category = "vocabulary", link_id = last_row$vocabulary + 1, code = "",
              creator_id = r$user_id, datetime = now(), deleted = FALSE
            )
          )
          
          last_row$code <- last_row$code + 1
          last_row$vocabulary <- last_row$vocabulary + 1
          last_row$options <- max(new_data$options$id)
        }
        
        for (table in c("vocabulary", "options", "code")) DBI::dbAppendTable(m$db, table, new_data[[table]])
      }
    }
    
    # Other tables than vocabulary
    else DBI::dbAppendTable(m$db, table_name, data_to_insert)
  }
  
  cat(paste0("\n", rows_inserted, " ", i18n$t("rows_inserted_in_table") , " ", table_name))

  return(c(rows_inserted, i18n$t("success_importing_concepts")))
}