#' Import a dataset
#' 
#' @description Import a dataset in the application, with OMOP CDM
#' @param output variable from Shiny, used to render messages on the message bar
#' @param ns Shiny namespace
#' @param i18n shiny.i18n object for translations
#' @param r A shiny::reactiveValues object, used to communicate between modules
#' @param d A shiny::reactiveValues object, used to communicate between modules
#' @param dataset_id ID of the dataset, used to create directory in data folder (integer)
#' @param data Data variable (data.frame or tibble)
#' @param type Name of the OMOP table to import (character)
#' @param omop_version OMOP versions of the imported data, accepts "5.3", "5.4" or "6.0" (character)
#' @param save_as_csv Save or not the data to CSV file (logical)
#' @param rewrite If save_as_csv is TRUE, rewrite or not existing CSV file (logical)
#' @description Load +/- save data when a dataset is selected by the user.
#' @details The function is used in a dataset code and is launched each time a user selects a dataset. \cr\cr
#' You can choose to \strong{load data each time} the function is used with save_as_csv set to FALSE (eg when dataset is small and the
#' data is loaded from a data warehouse) or you can \strong{save the data in a CSV file} with save_as_csv set to TRUE. \cr\cr
#' The data you import must respect the format of \href{https://ohdsi.github.io/CommonDataModel/cdm60.html}{\strong{OMOP common data model}}.\cr\cr
#' See help pages in the app for more informations.
#' @examples
#' \dontrun{
#' persons <- tibble::tibble(person_id = 1L, gender_concept_id = 8532L, year_of_birth = NA_integer_, month_of_birth = NA_integer_,
#'   day_of_birth = NA_integer_, birth_datetime = lubridate::ymd_hms("1990-01-03 00:00:00"), race_concept_id = NA_integer_,
#'   ethnicity_concept_id = NA_integer_, location_id = NA_integer_, provider_id = NA_integer_, care_site_id = NA_integer_,
#'   person_source_value = NA_character_, gender_source_value = "F", race_source_value = NA_character_, ethnicity_source_value = NA_character_)
#'     
#' import_dataset(output = output, ns = ns, i18n = i18n, r = r, d = d, dataset_id = 5, data = persons, type = "persons", 
#'   omop_versions = "5.4", save_as_csv = FALSE, rewrite = FALSE)
#' }
import_dataset <- function(output, ns = character(), i18n = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), 
  dataset_id = integer(), data = tibble::tibble(), type = "", omop_version = "6.0", save_as_csv = TRUE, rewrite = FALSE){
  
  # Check dataset_id
  if (length(dataset_id) == 0){
    add_log_entry(r = r, category = "Error", name = paste0("import_dataset - invalid_dataset_id_value"), value = i18n$t("invalid_dataset_id_value"))
    cat(paste0("<span style = 'font-weight:bold; color:red;'>**", i18n$t("error"), "** ", i18n$t("invalid_dataset_id_value"), "</span>\n"))
    return(NULL)
  }
  
  if (!is.numeric(dataset_id) | is.na(dataset_id)){
    add_log_entry(r = r, category = "Error", name = paste0("import_dataset - invalid_dataset_id_value - id"), value = i18n$t("invalid_dataset_id_value"))
    cat(paste0("<span style = 'font-weight:bold; color:red;'>**", i18n$t("error"), "** ", i18n$t("invalid_dataset_id_value"), "</span>\n"))
    return(NULL)
  }
  
  if (is.numeric(dataset_id) & floor(dataset_id) != dataset_id){
    add_log_entry(r = r, category = "Error", name = paste0("import_dataset - invalid_dataset_id_value - id"), value = i18n$t("invalid_dataset_id_value"))
    cat(paste0("<span style = 'font-weight:bold; color:red;'>**", i18n$t("error"), "** ", i18n$t("invalid_dataset_id_value"), "</span>\n"))
    return(NULL)
  }
  
  dataset_id <- as.integer(dataset_id)
  
  # Check omop_version
  if (omop_version %not_in% c("5.3", "5.4", "6.0")){
    add_log_entry(r = r, category = "Error", name = paste0("import_dataset - invalid_omop_version - id = ", dataset_id), value = i18n$t("invalid_omop_version"))
    cat(paste0("<span style = 'font-weight:bold; color:red;'>**", i18n$t("error"), "** ", i18n$t("invalid_omop_version"), "</span>\n"))
    return(NULL)
  }
  
  # Check if type is valid
  if (is.na(type) | type %not_in% c("person", "observation_period", "visit_occurrence", "visit_detail",
    "condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure",
    "measurement", "observation", "death", "note", "note_nlp", "specimen", "fact_relationship", "location",
    "location_history", "care_site", "provider", "payer_plan_period", "cost", "drug_era",
    "dose_era", "condition_era") | (type == "death" & omop_version %not_in% c("5.3", "5.4"))){
    add_log_entry(r = r, category = "Error", name = paste0("import_dataset - var_type_not_valid - id = ", dataset_id), value = i18n$t("var_type_not_valid"))
    cat(paste0("<span style = 'font-weight:bold; color:red;'>**", i18n$t("error"), "** ", i18n$t("var_type_not_valid"), "</span>\n"))
    return(NULL)
  }
 
  # If a datasets_folder is provided, take this value
  # Take package working directory else
  folder <- paste0(r$app_folder, "/datasets/", dataset_id)
  path <- paste0(folder, "/", type, ".csv")
  
  # If files already exists and we do not want to rewrite it
  if (save_as_csv & !rewrite & file.exists(path)){
    tryCatch({
      return({
        col_types <- switch(type, 
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
        if (type == "person" & omop_version %in% c("5.3", "5.4")) col_types <- "iiiiiTiiiiiccicici"
        if (type == "visit_detail" & omop_version == "5.3") col_types <- "iiiDTDTiiiciciciiii"
        if (type == "observation" & omop_version == "5.3") col_types <-  "iiiDTinciiiiiicicc"
        if (type == "observation" & omop_version == "5.4") col_types <-  "iiiDTinciiiiiicicccii"
        if (type == "location" & omop_version == "5.3") col_types <-  "iccccccc"
        if (type == "drug_era" & omop_version %in% c("5.3", "5.4")) col_types <- "iiiDDii"
        if (type == "dose_era" & omop_version %in% c("5.3", "5.4")) col_types <- "iiiinDD"
        if (type == "condition_era" & omop_version %in% c("5.3", "5.4")) col_types <- "iiiDDi"
          
        d[[type]] <- vroom::vroom(path, col_types = col_types, progress = FALSE)
        cat(paste0("<span style = 'font-weight:bold; color:#0078D4;'>", i18n$t(paste0("import_dataset_success_", type)), "</span>\n"))
      })
    },
      
      error = function(e){
        add_log_entry(r = r, category = "Error", name = paste0("import_dataset - error_loading_csv - id = ", dataset_id), value = i18n$t("error_loading_csv"))
        cat(paste0("<span style = 'font-weight:bold; color:red;'>**", i18n$t("error"), "** ", i18n$t("error_loading_csv"), "</span>\n"))
        return(NULL)},
      warning = function(w){
        add_log_entry(r = r, category = "Error", name = paste0("import_dataset - error_loading_csv - id = ", dataset_id), value = i18n$t("error_loading_csv"))
        cat(paste0("<span style = 'font-weight:bold; color:red;'>**", i18n$t("error"), "** ", i18n$t("error_loading_csv"), "</span>\n"))
        return(NULL)}
    )
  }
  
  # Transform as tibble
  if (!is.data.frame(data) & "tbl" %not_in% class(data)){
    add_log_entry(r = r, category = "Error", name = paste0("import_dataset - error_transforming_tibble - id = ", dataset_id), value = i18n$t("error_transforming_tibble"))
    cat(paste0("<span style = 'font-weight:bold; color:red;'>**", i18n$t("error"), "** ", i18n$t("error_transforming_tibble"), "</span>\n"))
    return(NULL)
  }
  
  if ("tbl" %not_in% class(data)) data <- tibble::as_tibble(data)
  
  # Data cols
  
  if (omop_version %in% c("5.3", "5.4")){
    
    data_cols <- tibble::tribble(
      ~var, ~cols,
      "person", tibble::tribble(
        ~name, ~type,
        "person_id", "integer",
        "gender_concept_id", "integer",
        "year_of_birth", "integer",
        "month_of_birth", "integer",
        "day_of_birth", "integer",
        "birth_datetime", "datetime",
        "race_concept_id", "integer",
        "ethnicity_concept_id", "integer",
        "location_id", "integer",
        "provider_id", "integer",
        "care_site_id", "integer",
        "person_source_value", "character",
        "gender_source_value", "character",
        "gender_source_concept_id", "integer",
        "race_source_value", "character",
        "race_source_concept_id", "integer",
        "ethnicity_source_value", "character",
        "ethnicity_source_concept_id", "integer"
      ),
      "death", tibble::tribble(
        ~name, ~type,
        "person_id", "integer",
        "death_date", "date",
        "death_datetime", "datetime",
        "death_type_concept_id", "integer",
        "cause_concept_id", "integer",
        "cause_source_value", "character",
        "cause_source_concept_id", "integer"
      ),
      "drug_era", tibble::tribble(
        ~name, ~type,
        "drug_era_id", "integer",
        "person_id", "integer",
        "drug_concept_id", "integer",
        "drug_era_start_date", "date",
        "drug_era_end_date", "date",
        "drug_exposure_count", "integer",
        "gap_days", "integer"
      ),
      "dose_era", tibble::tribble(
        ~name, ~type,
        "dose_era_id", "integer",
        "person_id", "integer",
        "drug_concept_id", "integer",
        "unit_concept_id", "integer",
        "dose_value", "numeric",
        "dose_era_start_date", "date",
        "dose_era_end_date", "date"
      ),
      "condition_era", tibble::tribble(
        ~name, ~type,
        "condition_era_id", "integer",
        "person_id", "integer",
        "condition_concept_id", "integer",
        "condition_era_start_date", "date",
        "condition_era_end_date", "date",
        "condition_occurrence_count", "integer"
      )
    )
  }
  
  if (omop_version == "6.0"){
    data_cols <- tibble::tribble(
      ~var, ~cols,
      "person", tibble::tribble(
        ~name, ~type,
        "person_id", "integer",
        "gender_concept_id", "integer",
        "year_of_birth", "integer",
        "month_of_birth", "integer",
        "day_of_birth", "integer",
        "birth_datetime", "datetime",
        "death_datetime", "datetime",
        "race_concept_id", "integer",
        "ethnicity_concept_id", "integer",
        "location_id", "integer",
        "provider_id", "integer",
        "care_site_id", "integer",
        "person_source_value", "character",
        "gender_source_value", "character",
        "gender_source_concept_id", "integer",
        "race_source_value", "character",
        "race_source_concept_id", "integer",
        "ethnicity_source_value", "character",
        "ethnicity_source_concept_id", "integer"
      ),
      "visit_detail", tibble::tribble(
        ~name, ~type,
        "visit_detail_id", "integer",
        "person_id", "integer",
        "visit_detail_concept_id", "integer",
        "visit_detail_start_date", "date",
        "visit_detail_start_datetime", "datetime",
        "visit_detail_end_date", "date",
        "visit_detail_end_datetime", "datetime",
        "visit_detail_type_concept_id", "integer",
        "provider_id", "integer",
        "care_site_id", "integer",
        "visit_detail_source_value", "character",
        "visit_detail_source_concept_id", "integer",
        "admitted_from_concept_id", "integer",
        "admitted_from_source_value", "character",
        "discharge_to_source_value", "character",
        "discharge_to_concept_id", "integer",
        "preceding_visit_detail_id", "integer",
        "visit_detail_parent_id", "integer",
        "visit_occurrence_id", "integer"
      ),
      "drug_era", tibble::tribble(
        ~name, ~type,
        "drug_era_id", "integer",
        "person_id", "integer",
        "drug_concept_id", "integer",
        "drug_era_start_datetime", "datetime",
        "drug_era_end_datetime", "datetime",
        "drug_exposure_count", "integer",
        "gap_days", "integer"
      ),
      "dose_era", tibble::tribble(
        ~name, ~type,
        "dose_era_id", "integer",
        "person_id", "integer",
        "drug_concept_id", "integer",
        "unit_concept_id", "integer",
        "dose_value", "numeric",
        "dose_era_start_datetime", "datetime",
        "dose_era_end_datetime", "datetime"
      ),
      "condition_era", tibble::tribble(
        ~name, ~type,
        "condition_era_id", "integer",
        "person_id", "integer",
        "condition_concept_id", "integer",
        "condition_era_start_datetime", "datetime",
        "condition_era_end_datetime", "datetime",
        "condition_occurrence_count", "integer"
      )
    )
  }
  
  if (omop_version == "5.3"){
    data_cols <- data_cols %>% dplyr::bind_rows(
      tibble::tribble(
        ~var, ~cols,
        "visit_occurrence", tibble::tribble(
          ~name, ~type,
          "visit_occurrence_id", "integer",
          "person_id", "integer",
          "visit_concept_id", "integer",
          "visit_start_date", "date",
          "visit_start_datetime", "datetime",
          "visit_end_date", "date",
          "visit_end_datetime", "datetime",
          "visit_type_concept_id", "integer",
          "provider_id", "integer",
          "care_site_id", "integer",
          "visit_source_value", "character",
          "visit_source_concept_id", "integer",
          "admitting_source_concept_id", "integer",
          "admitting_source_value", "character",
          "discharge_to_concept_id", "integer",
          "discharge_to_source_value", "character",
          "preceding_visit_occurrence_id", "integer"
        ),
        "visit_detail", tibble::tribble(
          ~name, ~type,
          "visit_detail_id", "integer",
          "person_id", "integer",
          "visit_detail_concept_id", "integer",
          "visit_detail_start_date", "date",
          "visit_detail_start_datetime", "datetime",
          "visit_detail_end_date", "date",
          "visit_detail_end_datetime", "datetime",
          "visit_detail_type_concept_id", "integer",
          "provider_id", "integer",
          "care_site_id", "integer",
          "visit_detail_source_value", "character",
          "visit_detail_source_concept_id", "integer",
          "admitting_source_value", "character",
          "admitting_source_concept_id", "integer",
          "discharge_to_source_value", "character",
          "discharge_to_concept_id", "integer",
          "preceding_visit_detail_id", "integer",
          "visit_detail_parent_id", "integer",
          "visit_occurrence_id", "integer"
        ),
        "observation", tibble::tribble(
          ~name, ~type,
          "observation_id", "integer",
          "person_id", "integer",
          "observation_concept_id", "integer",
          "observation_date", "date",
          "observation_datetime", "datetime",
          "observation_type_concept_id", "integer",
          "value_as_number", "numeric",
          "value_as_string", "character",
          "value_as_concept_id", "integer",
          "qualifier_concept_id", "integer",
          "unit_concept_id", "integer",
          "provider_id", "integer",
          "visit_occurrence_id", "integer",
          "visit_detail_id", "integer",
          "observation_source_value", "character",
          "observation_source_concept_id", "integer",
          "unit_source_value", "character",
          "qualifier_source_value", "character"
        ),
        "location", tibble::tribble(
          ~name, ~type,
          "location_id", "integer",
          "address_1", "character",
          "address_2", "character",
          "city", "character",
          "state", "character",
          "zip", "character",
          "county", "character",
          "location_source_value", "character"
        )
      )
    )
  }
  
  if (omop_version == "5.4"){
    data_cols <- data_cols %>% dplyr::bind_rows(
      tibble::tribble(
        ~var, ~cols,
        "visit_detail", tibble::tribble(
          ~name, ~type,
          "visit_detail_id", "integer",
          "person_id", "integer",
          "visit_detail_concept_id", "integer",
          "visit_detail_start_date", "date",
          "visit_detail_start_datetime", "datetime",
          "visit_detail_end_date", "date",
          "visit_detail_end_datetime", "datetime",
          "visit_detail_type_concept_id", "integer",
          "provider_id", "integer",
          "care_site_id", "integer",
          "visit_detail_source_value", "character",
          "visit_detail_source_concept_id", "integer",
          "admitted_from_concept_id", "integer",
          "admitted_from_source_value", "character",
          "discharge_to_source_value", "character",
          "discharge_to_concept_id", "integer",
          "preceding_visit_detail_id", "integer",
          "parent_visit_detail_id", "integer",
          "visit_occurrence_id", "integer"
        ),
        "observation", tibble::tribble(
          ~name, ~type,
          "observation_id", "integer",
          "person_id", "integer",
          "observation_concept_id", "integer",
          "observation_date", "date",
          "observation_datetime", "datetime",
          "observation_type_concept_id", "integer",
          "value_as_number", "numeric",
          "value_as_string", "character",
          "value_as_concept_id", "integer",
          "qualifier_concept_id", "integer",
          "unit_concept_id", "integer",
          "provider_id", "integer",
          "visit_occurrence_id", "integer",
          "visit_detail_id", "integer",
          "observation_source_value", "character",
          "observation_source_concept_id", "integer",
          "unit_source_value", "character",
          "qualifier_source_value", "character",
          "value_source_value", "character",
          "observation_event_id", "integer",
          "obs_event_field_concept_id", "integer"
        )
      )
    )
  }
  
  if (omop_version %in% c("5.4", "6.0")){
    data_cols <- data_cols %>% dplyr::bind_rows(
      tibble::tribble(
        ~var, ~cols,
        "visit_occurrence", tibble::tribble(
          ~name, ~type,
          "visit_occurrence_id", "integer",
          "person_id", "integer",
          "visit_concept_id", "integer",
          "visit_start_date", "date",
          "visit_start_datetime", "datetime",
          "visit_end_date", "date",
          "visit_end_datetime", "datetime",
          "visit_type_concept_id", "integer",
          "provider_id", "integer",
          "care_site_id", "integer",
          "visit_source_value", "character",
          "visit_source_concept_id", "integer",
          "admitted_from_concept_id", "integer",
          "admitted_from_source_value", "character",
          "discharge_to_concept_id", "integer",
          "discharge_to_source_value", "character",
          "preceding_visit_occurrence_id", "integer"
        ),
        "location", tibble::tribble(
          ~name, ~type,
          "location_id", "integer",
          "address_1", "character",
          "address_2", "character",
          "city", "character",
          "state", "character",
          "zip", "character",
          "county", "character",
          "location_source_value", "character",
          "latitude", "numeric",
          "longitude", "numeric"
        )
      )
    )
  }
  
  data_cols <- data_cols %>% dplyr::bind_rows(
    tibble::tribble(
      ~var, ~cols,
      "observation_period", tibble::tribble(
        ~name, ~type,
        "observation_period_id", "integer",
        "person_id", "integer",
        "observation_period_start_date", "date",
        "observation_period_end_date", "date",
        "period_type_concept_id", "integer"
      ),
      "condition_occurrence", tibble::tribble(
        ~name, ~type,
        "condition_occurrence_id", "integer",
        "person_id", "integer",
        "condition_concept_id", "integer",
        "condition_start_date", "date",
        "condition_start_datetime", "datetime",
        "condition_end_date", "date",
        "condition_end_datetime", "datetime",
        "condition_type_concept_id", "integer",
        "condition_status_concept_id", "integer",
        "stop_reason", "character",
        "provider_id", "integer",
        "visit_occurrence_id", "integer",
        "visit_detail_id", "integer",
        "condition_source_value", "character",
        "condition_source_concept_id", "integer",
        "condition_status_source_value", "character"
      ),
      "drug_exposure", tibble::tribble(
        ~name, ~type,
        "drug_exposure_id", "integer",
        "person_id", "drug_concept_id",
        "drug_concept_id", "integer",
        "drug_exposure_start_date", "date",
        "drug_exposure_start_datetime", "datetime",
        "drug_exposure_end_date", "date",
        "drug_exposure_end_datetime", "datetime",
        "verbatim_end_date", "date",
        "drug_type_concept_id", "integer",
        "stop_reason", "character",
        "refills", "integer",
        "quantity", "numeric",
        "days_supply", "integer",
        "sig", "character",
        "route_concept_id", "integer",
        "lot_number", "character",
        "provider_id", "integer",
        "visit_occurrence_id", "integer",
        "visit_detail_id", "integer",
        "drug_source_value", "character",
        "drug_source_concept_id", "integer",
        "route_source_value", "character",
        "dose_unit_source_value", "character"
      ),
      "procedure_occurrence", tibble::tribble(
        ~name, ~type,
        "procedure_occurrence_id", "integer",
        "person_id", "integer",
        "procedure_concept_id", "integer",
        "procedure_date", "date",
        "procedure_datetime", "datetime",
        "procedure_type_concept_id", "integer",
        "modifier_concept_id", "integer",
        "quantity", "integer",
        "provider_id", "integer",
        "visit_occurrence_id", "integer",
        "visit_detail_id", "integer",
        "procedure_source_value", "character",
        "procedure_source_concept_id", "integer",
        "modifier_source_value", "character"
      ),
      "device_exposure", tibble::tribble(
        ~name, ~type,
        "device_exposure_id", "integer",
        "person_id", "integer",
        "device_concept_id", "integer",
        "device_exposure_start_date", "date",
        "device_exposure_start_datetime", "datetime",
        "device_exposure_end_date", "date",
        "device_exposure_end_datetime", "datetime",
        "device_type_concept_id", "integer",
        "unique_device_id", "character",
        "quantity", "integer",
        "provider_id", "integer",
        "visit_occurrence_id", "integer",
        "visit_detail_id", "integer",
        "device_source_value", "character",
        "device_source_concept_id", "integer"
      ),
      "measurement", tibble::tribble(
        ~name, ~type,
        "measurement_id", "integer",
        "person_id", "integer",
        "measurement_concept_id", "integer",
        "measurement_date", "date",
        "measurement_datetime", "datetime",
        "measurement_time", "time",
        "measurement_type_concept_id", "integer",
        "operator_concept_id", "integer",
        "value_as_number", "numeric",
        "value_as_concept_id", "integer",
        "unit_concept_id", "integer",
        "range_low", "numeric",
        "range_high", "numeric",
        "provider_id", "integer",
        "visit_occurrence_id", "integer",
        "visit_detail_id", "integer",
        "measurement_source_value", "character",
        "measurement_source_concept_id", "integer",
        "unit_source_value", "character",
        "value_source_value", "character"
      ),
      "observation", tibble::tribble(
        ~name, ~type,
        "observation_id", "integer",
        "person_id", "integer",
        "observation_concept_id", "integer",
        "observation_date", "date",
        "observation_datetime", "datetime",
        "observation_type_concept_id", "integer",
        "value_as_number", "numeric",
        "value_as_string", "character",
        "value_as_concept_id", "integer",
        "qualifier_concept_id", "integer",
        "unit_concept_id", "integer",
        "provider_id", "integer",
        "visit_occurrence_id", "integer",
        "visit_detail_id", "integer",
        "observation_source_value", "character",
        "observation_source_concept_id", "integer",
        "unit_source_value", "character",
        "qualifier_source_value", "character",
        "observation_event_id", "integer",
        "obs_event_field_concept_id", "integer",
        "value_as_datetime", "datetime"
      ),
      "death", tibble::tribble(
        ~name, ~type,
        "person_id", "integer",
        "death_date", "date",
        "death_datetime", "datetime",
        "death_type_concept_id", "integer",
        "cause_concept_id", "integer",
        "cause_source_value", "character",
        "cause_source_concept_id", "integer"
      ),
      "note", tibble::tribble(
        ~name, ~type,
        "note_id", "integer",
        "person_id", "integer",
        "note_event_id", "integer",
        "note_event_field_concept_id", "integer",
        "note_date", "date",
        "note_datetime", "datetime",
        "note_type_concept_id", "integer",
        "note_class_concept_id", "integer",
        "note_title", "character",
        "note_text", "character",
        "encoding_concept_id", "integer",
        "language_concept_id", "integer",
        "provider_id", "integer",
        "visit_occurrence_id", "integer",
        "visit_detail_id", "integer",
        "note_source_value", "character"
      ),
      "note_nlp", tibble::tribble(
        ~name, ~type,
        "note_nlp_id", "integer",
        "note_id", "integer",
        "section_concept_id", "integer",
        "snippet", "character",
        "offset", "character",
        "lexical_variant", "character",
        "note_nlp_concept_id", "integer",
        "note_nlp_source_concept_id", "integer",
        "nlp_system", "character",
        "nlp_date", "date",
        "nlp_datetime", "datetime",
        "term_exists", "character",
        "term_temporal", "character",
        "term_modifiers", "character"
      ),
      "specimen", tibble::tribble(
        ~name, ~type,
        "specimen_id", "integer",
        "person_id", "integer",
        "specimen_concept_id", "integer",
        "specimen_type_concept_id", "integer",
        "specimen_date", "date",
        "specimen_datetime", "datetime",
        "quantity", "numeric",
        "unit_concept_id", "integer",
        "anatomic_site_concept_id", "integer",
        "disease_status_concept_id", "integer",
        "specimen_source_id", "character",
        "specimen_source_value", "character",
        "unit_source_value", "character",
        "anatomic_site_source_value", "character",
        "disease_status_source_value", "character"
      ),
      "fact_relationship", tibble::tribble(
        ~name, ~type,
        "domain_concept_id_1", "integer",
        "fact_id_1", "integer",
        "domain_concept_id_2", "integer",
        "fact_id_2", "integer",
        "relationship_concept_id", "integer"
      ),
      "survey_conduct", tibble::tribble(
        ~name, ~type,
        "survey_conduct_id", "integer",
        "person_id", "integer",
        "survey_concept_id", "integer",
        "survey_start_date", "date",
        "survey_start_datetime", "datetime",
        "survey_end_date", "date",
        "survey_end_datetime", "datetime",
        "provider_id", "integer",
        "assisted_concept_id", "integer",
        "respondent_type_concept_id", "integer",
        "timing_concept_id", "integer",
        "collection_method_concept_id", "integer",
        "assisted_source_value", "character",
        "respondent_type_source_value", "character",
        "timing_source_value", "character",
        "collection_method_source_value", "chracter",
        "survey_source_value", "character",
        "survey_source_concept_id", "integer",
        "survey_source_identifier", "character",
        "validated_survey_concept_id" , "integer",
        "validated_survey_source_value", "integer",
        "survey_version_number", "character",
        "visit_occurrence_id", "integer",
        "response_visit_occurrence_id", "integer"
      ),
      "location_history", tibble::tribble(
        ~name, ~type,
        "location_id", "integer",
        "relationship_type_concept_id", "integer",
        "domain_id", "character",
        "entity_id", "integer",
        "start_date", "date",
        "end_date", "date"
      ),
      "care_site", tibble::tribble(
        ~name, ~type,
        "care_site_id" ,"integer",
        "care_site_name", "character",
        "place_of_service_concept_id", "integer",
        "location_id", "integer",
        "care_site_source_value", "character",
        "place_of_service_source_value", "character"
      ),
      "provider", tibble::tribble(
        ~name, ~type,
        "provider_id", "integer",
        "provider_name", "character",
        "npi", "character",
        "dea", "character",
        "specialty_concept_id", "integer",
        "care_site_id", "integer",
        "year_of_birth", "integer",
        "gender_concept_id", "integer",
        "provider_source_value", "character",
        "specialty_source_value", "character",
        "specialty_source_concept_id", "integer",
        "gender_source_value", "character",
        "gender_source_concept_id", "integer"
      ),
      "payer_plan_period", tibble::tribble(
        ~name, ~type,
        "payer_plan_period_it", "integer",
        "person_id", "integer",
        "contract_person_id", "integer",
        "payer_plan_period_start_date", "date",
        "payer_plan_period_end_date", "date",
        "payer_concept_id", "integer",
        "payer_source_value", "character",
        "payer_source_concept_id", "integer",
        "plan_concept_id", "integer",
        "plan_source_value", "character",
        "plan_source_concept_id", "integer",
        "contract_concept_id", "integer",
        "contract_source_value", "character",
        "contract_source_concept_id", "integer",
        "sponsor_concept_id", "integer",
        "sponsor_source_value", "character",
        "sponsor_source_concept_id", "integer",
        "family_source_value", "character",
        "stop_reason_concept_id", "integer",
        "stop_reason_source_value", "character",
        "stop_reason_source_concept_id", "integer"
      ),
      "cost", tibble::tribble(
        ~name, ~type,
        "cost_id", "integer",
        "person_id", "integer",
        "cost_event_id", "integer",
        "cost_event_field_concept_id", "integer",
        "cost_concept_id", "integer",
        "cost_type_concept_id", "integer",
        "cost_source_concept_id", "integer",
        "cost_source_value", "character",
        "currency_concept_id", "integer",
        "cost", "numeric",
        "incurred_date", "date",
        "billed_date", "date",
        "paid_date", "date",
        "revenue_code_concept_id", "integer",
        "drg_concept_id", "integer",
        "revenue_code_source_value", "character",
        "drg_source_value", "character",
        "payer_plan_period_id", "integer"
      )
    )
  )
  
  # Check columns var types & names
  var_cols <- data_cols %>% dplyr::filter(var == type) %>% dplyr::pull(cols)
  var_cols <- var_cols[[1]]
  
  if (!identical(names(data), var_cols %>% dplyr::pull(name))){
    add_log_entry(r = r, category = "Error", name = paste0("import_dataset - invalid_col_names - id = ", dataset_id), value = paste0(i18n$t("valid_col_names_are"), "</span>\n", toString(var_cols %>% dplyr::pull(name))))
    cat(paste0("<span style = 'font-weight:bold; color:red;'>**", i18n$t("error"), "** ", i18n$t("valid_col_names_are"), "</span>\n", toString(var_cols %>% dplyr::pull(name)), "\n"))
    return(NULL)
  }
  
  error_message <- ""
  # cols_to_char <- character(0)
  
  var_types <- c("integer", "character", "numeric", "datetime", "date")
  
  conversion_functions <- list(
    "integer" = as.integer,
    "character" = as.character,
    "numeric" = as.numeric,
    "datetime" = as.POSIXct,
    "date" = as.Date
  )
  
  check_functions <- list(
    "integer" = is.integer,
    "character" = is.character,
    "numeric" = is.numeric,
    "datetime" = lubridate::is.POSIXct,
    "date" = lubridate::is.Date
  )
  
  data_test <- data %>% dplyr::slice_sample(n = 1) %>% dplyr::collect()
  
  for (i in 1:nrow(var_cols)){
    
    error <- TRUE
    
    var_name <- var_cols[[i, "name"]]
    
    for (var_type in var_types) {
      if (var_cols[[i, "type"]] == var_type) {
        # if ("tbl_lazy" %in% class(data)) {
        #   tryCatch({
        #     # data <- data %>% dplyr::mutate_at(var_name, conversion_functions[[var_type]])
        #     if (var_type == "datetime") data <- data %>% dplyr::mutate_at(var_name, as.POSIXct)
        #     print(data %>% dplyr::select(var_name))
        #     error <- FALSE
        #   })
        # } else if (do.call(check_functions[[var_type]], list(data[[var_name]]))) error <- FALSE
        
        if (do.call(check_functions[[var_type]], list(data_test[[var_name]]))) error <- FALSE
        print(var_type)
        print(error)
        
        if (error) {
          type_error_msg <- paste0("type_must_be_", var_type)
          
          add_log_entry(r = r, category = "Error", name = paste0("import_dataset - invalid_col_types - id = ", dataset_id), 
            value = paste0(i18n$t("column"), " ", var_name, " ", i18n$t(type_error_msg)))
          
          error_message <- paste0(error_message, "<span style = 'font-weight:bold; color:red;'>**",  i18n$t("error"), "** ",
            i18n$t("column"), " ", var_name, " ", i18n$t(type_error_msg), "</span>\n"
          )
        }
      }
    }
    # if (var_cols[[i, "type"]] %in% c("datetime", "date")) cols_to_char <- c(cols_to_char, var_name)
  }
  
  if (error_message != ""){
    cat(error_message)
    return(NULL) 
  }
  
  # Transform date cols to character
  # if (length(cols_to_char) > 0) data <- data %>% dplyr::mutate_at(cols_to_char, as.character)
  
  # if  save_as_csv is TRUE, save data in dataset folder
  if (save_as_csv){
    if (!file.exists(folder)) dir.create(folder, recursive = TRUE)
    if (!file.exists(path)) tryCatch(readr::write_csv(data, path, progress = FALSE),
      error = function(e){
        add_log_entry(r = r, category = "Error", name = paste0("import_dataset - error_saving_csv - id = ", dataset_id), value = i18n$t("error_saving_csv"))
        cat(paste0("<span style = 'font-weight:bold; color:red;'>", i18n$t("error_saving_csv"), "</span>\n"))
        return(NULL)}
    )
    if (file.exists(path) & rewrite) tryCatch({
      # file.remove(path)
      readr::write_csv(data, path, progress = FALSE)}, 
      error = function(e){
        add_log_entry(r = r, category = "Error", name = paste0("import_dataset - error_saving_csv - id = ", dataset_id), value = i18n$t("error_saving_csv"))
        cat(paste0("<span style = 'font-weight:bold; color:red;'>", i18n$t("error_saving_csv"), "</span>\n"))
        return(NULL)}
    )
  }
  
  d[[type]] <- data
  
  cat(paste0("<span style = 'font-weight:bold; color:#0078D4;'>", i18n$t(paste0("import_dataset_success_", type)), "</span>\n"))
}

# dataset_csv_exists <- function(r = shiny::reactiveValues(), table = character(), dataset_id = integer()){
#   if (file.exists(paste0(r$app_folder, "/datasets/", dataset_id, "/", table, ".csv"))) return(TRUE)
#   else return(FALSE)
# }

#' Import a vocabulary table
#' 
#' @description Import an OMOP vocabulary and save it in app database
#' @param output variable from Shiny, used to render messages on the message bar
#' @param ns Shiny namespace
#' @param i18n shiny.i18n object for translations
#' @param r A shiny::reactiveValues object, used to communicate between modules
#' @param m A shiny::reactiveValues object, used to communicate between modules
#' @param table_name Name of the vocabulary table we import (concept, concept_relationship or other) (character)
#' @param data A tibble containing the data
#' @param vocabulary_id ID of the vocabulary, for example LOINC (character)
#' @details The function is used in a vocabulary code, it is launched only when you click on "Run code" on the vocabulary page.\cr\cr
#' See \href{https://ohdsi.github.io/CommonDataModel/cdm60.html}{\strong{OMOP common data model}} for more information.
#' @examples
#' \dontrun{
#' concept <- tibble::tibble(concept_id = 3027018, concept_name = "Heart rate", domain_id = "Measurement",
#'   concept_class_id = "Clinical Observation", standard_concept = "S", concept_code = "8867-4",
#'   valid_start_date = "1970-01-01", valid_end_date = "2099-12-31", invalid_reason = NA_character_)
#'   
#' import_vocabulary_table(output = output, ns = ns, i18n = i18n, r = r, m = m,
#'   table_name = "concept", data = concept, vocabulary_id = "LOINC")
#' }
import_vocabulary_table <- function(output, ns = character(), i18n = character(), r = shiny::reactiveValues(), m = shiny::reactiveValues(),
  table_name = character(), data = tibble::tibble(), vocabulary_id = character()){
 
  # Create var to count rows if doesn't exist
  if (length(r$import_vocabulary_count_rows) == 0) r$import_vocabulary_count_rows <- tibble::tibble(table_name = character(), n_rows = integer())
  
  if (table_name %not_in% c("concept", "domain", "concept_class", "concept_relationship", "relationship", "concept_synonym", "concept_ancestor", "drug_strength")){
    add_log_entry(r = r, category = "Error", name = paste0("import_vocabulary_table - invalid_vocabulary_table"), value = i18n$t("invalid_vocabulary_table"))
    cat(paste0("<span style = 'font-weight:bold; color:red;'>**", i18n$t("error"), "** ", i18n$t("invalid_vocabulary_table"), "</span>\n"))
    return(NULL)
  }
  
  if (table_name == "concept") var_cols <- tibble::tribble(
    ~name, ~type,
    "concept_id", "integer",
    "concept_name", "character",
    "domain_id", "character",
    "vocabulary_id", "character",
    "concept_class_id", "character",
    "standard_concept", "character",
    "concept_code", "character",
    "valid_start_date", "date",
    "valid_end_date", "date",
    "invalid_reason", "character")
  
  else if (table_name == "domain") var_cols <- tibble::tribble(
    ~name, ~type,
    "domain_id", "character",
    "domain_name", "character",
    "domain_concept_id", "integer")
  
  else if (table_name == "concept_class") var_cols <- tibble::tribble(
    ~name, ~type,
    "concept_class_id", "character",
    "concept_class_name", "character",
    "concept_class_concept_id", "integer")
  
  else if (table_name == "concept_relationship") var_cols <- tibble::tribble(
    ~name, ~type,
    "concept_id_1", "integer",
    "concept_id_2", "integer",
    "relationship_id", "character",
    "valid_start_date", "date",
    "valid_end_date", "date",
    "invalid_reason", "character")
  
  else if (table_name == "relationship") var_cols <- tibble::tribble(
    ~name, ~type,
    "relationship_id", "character",
    "relationship_name", "character",
    "is_hierarchical", "character",
    "defines_ancestry", "character",
    "reverse_relationship_id", "character",
    "relationship_concept_id", "integer")
  
  else if (table_name == "concept_synonym") var_cols <- tibble::tribble(
    ~name, ~type,
    "concept_id", "integer",
    "concept_synonym_name", "character",
    "language_concept_id", "integer")
  
  else if (table_name == "concept_ancestor") var_cols <- tibble::tribble(
    ~name, ~type,
    "ancestor_concept_id", "integer",
    "descendant_concept_id", "integer",
    "min_levels_of_separation", "integer",
    "max_levels_of_separation", "integer")
  
  else if (table_name == "drug_strength") var_cols <- tibble::tribble(
    ~name, ~type,
    "drug_concept_id", "integer",
    "ingredient_concept_id", "integer",
    "amount_value", "numeric",
    "amount_unit_concept_id", "integer",
    "numerator_value", "numeric",
    "numerator_unit_concept_id", "integer",
    "denominator_value", "numeric",
    "denominator_unit_concept_id", "integer",
    "box_size", "integer",
    "valid_start_date", "date",
    "valid_end_date", "date",
    "invalid_reason", "character")

  # Check columns var types & names
  
  if (!identical(names(data), var_cols$name)){
    add_log_entry(r = r, category = "Error", name = paste0("import_vocabulary_table - invalid_col_names"), value = paste0(i18n$t("valid_col_names_are"), "</span>\n", toString(var_cols %>% dplyr::pull(name))))
    cat(paste0("<span style = 'font-weight:bold; color:red;'>**", i18n$t("error"), "** ", i18n$t("valid_col_names_are"), "</span>\n", toString(var_cols %>% dplyr::pull(name)), "\n"))
    return(NULL)
  }
  
  # Check col types
  
  error_message <- ""
  cols_to_char <- character(0)
  
  for (i in 1:nrow(var_cols)){
    var_name <- var_cols[[i, "name"]]
    if (var_cols[[i, "type"]] == "integer" & !is.integer(data[[var_name]])){
      add_log_entry(r = r, category = "Error", name = paste0("import_vocabulary_table - invalid_col_types"), value = paste0(i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_integer")))
      error_message <- paste0(error_message, "<span style = 'font-weight:bold; color:red;'>**", i18n$t("error"), "** ", i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_integer"), "</span>\n")
    }
    else if (var_cols[[i, "type"]] == "character" & !is.character(data[[var_name]])){
      add_log_entry(r = r, category = "Error", name = paste0("import_vocabulary_table - invalid_col_types - id"), value = paste0(i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_character")))
      error_message <- paste0(error_message, "<span style = 'font-weight:bold; color:red;'>**", i18n$t("error"), "** ", i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_character"), "</span>\n")
    }
    else if (var_cols[[i, "type"]] == "numeric" & !is.numeric(data[[var_name]])){
      add_log_entry(r = r, category = "Error", name = paste0("import_vocabulary_table - invalid_col_types - id"), value = paste0(i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_numeric")))
      error_message <- paste0(error_message, "<span style = 'font-weight:bold; color:red;'>**", i18n$t("error"), "** ", i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_numeric"), "</span>\n")
    }
    else if (var_cols[[i, "type"]] == "date" & !lubridate::is.Date(data[[var_name]])){
      add_log_entry(r = r, category = "Error", name = paste0("import_vocabulary_table - invalid_col_types - id"), value = paste0(i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_date")))
      error_message <- paste0(error_message, "<span style = 'font-weight:bold; color:red;'>**", i18n$t("error"), "** ", i18n$t("column"), " ", var_name, " ", i18n$t("type_must_be_date"), "</span>\n")
    }
    
    if (var_cols[[i, "type"]] == "date") cols_to_char <- c(cols_to_char, var_name)
  }
  
  if (error_message != ""){
    cat(error_message)
    return(NULL) 
  }
  
  # Transform date cols to character
  if (length(cols_to_char) > 0) data <- data %>% dplyr::mutate_at(cols_to_char, as.character)

  # Transform as tibble
  if (!is.data.frame(data)){
    add_log_entry(r = r, category = "Error", name = paste0("import_vocabulary_table - error_transforming_tibble"), value = i18n$t("error_transforming_tibble"))
    cat(paste0("<span style = 'font-weight:bold; color:red;'>**", i18n$t("error"), "** ", i18n$t("error_transforming_tibble"), "</span>\n"))
    return(NULL)
  }
  
  data <- tibble::as_tibble(data)
  
  # Change vocabulary_id col if value is not null
  if (table_name == "concept" & length(vocabulary_id) > 0) data <- data %>% dplyr::mutate(vocabulary_id = !!vocabulary_id)

  # Add data to database
  
  tables_with_primary_key <- c("concept", "domain", "concept_class", "relationship")

  # Case 1 : table has a primary key, add data not already in database, filtered by primary key
  
  if (table_name %in% tables_with_primary_key){
  
  # Check if there are duplicates in primary_keys

    primary_keys_duplicates <- data %>% dplyr::group_by_at(paste0(table_name, "_id")) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
    if (primary_keys_duplicates > 0){
      add_log_entry(r = r, category = "Error", name = paste0("import_vocabulary_table - error_multiple_primary_keys"), value = i18n$t("error_multiple_primary_keys"))
      cat(paste0("<span style = 'font-weight:bold; color:red;'>**", i18n$t("error"), "** ", i18n$t("error_multiple_primary_keys"), "</span>\n"))
      return(NULL)
    }

    tryCatch({
      sql <- glue::glue_sql("SELECT {`paste0(table_name, '_id')`} FROM {`table_name`}", .con = m$db)
      actual_data <- DBI::dbGetQuery(m$db, sql)
    },
      error = function(e){
        if (nchar(e[1]) > 0) add_log_entry(r = r, category = "Error", name = paste0("import_vocabulary_table - error_get_actual_primary_keys"), value = toString(e))
        cat(paste0("<span style = 'font-weight:bold; color:red;'>**", i18n$t("error"), "** ", i18n$t("error_get_actual_primary_keys"), "</span>\n"))
        return(NULL)}
    )

    # Get items to insert with an anti-join
    data_to_insert <- data %>% dplyr::anti_join(actual_data, by = paste0(table_name, "_id"))
  }

  # Case 2 : table has no primary key, add data if not already in database filtered with selected cols
  
  if (table_name %not_in% tables_with_primary_key){
    
    data_duplicates_cols <- switch(table_name,
      "concept_relationship" = c("concept_id_1", "concept_id_2", "relationship_id"),
      "concept_synonym" = c("concept_id", "concept_synonym_name", "language_concept_id"),
      "concept_ancestor" = c("ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation", "max_levels_of_separation"),
      "drug_strength" = c("drug_concept_id", "ingredient_concept_id", "amount_value", "amount_unit_concept_id", "numerator_value", "numerator_unit_concept_id",
        "denominator_value", "denominator_unit_concept_id", "box_size")
    )
    
    data_duplicates <- data %>% dplyr::group_by_at(data_duplicates_cols) %>% dplyr::summarize(n = dplyr::n()) %>% dplyr::filter(n > 1) %>% nrow()
    
    if (data_duplicates > 0){
      add_log_entry(r = r, category = "Error", name = paste0("import_vocabulary_table - error_multiple_identical_values"), value = i18n$t("error_multiple_identical_values"))
      cat(paste0("<span style = 'font-weight:bold; color:red;'>**", i18n$t("error"), "** ", i18n$t("error_multiple_identical_values"), "</span>\n"))
      return(NULL)
    }
    
    tryCatch({
      sql <- glue::glue_sql("SELECT {`data_duplicates_cols`*} FROM {`table_name`}", .con = m$db)
      actual_data <- DBI::dbGetQuery(m$db, sql)
    },
      error = function(e){
        if (nchar(e[1]) > 0) add_log_entry(r = r, category = "Error", name = paste0("import_vocabulary_table - error_get_actual_primary_keys"), value = toString(e))
        cat(paste0("<span style = 'font-weight:bold; color:red;'>**", i18n$t("error"), "** ", i18n$t("error_get_actual_primary_keys"), "</span>\n"))
        return(NULL)}
    )
    
    # Get items to insert with an anti-join
    data_to_insert <- data %>% dplyr::anti_join(actual_data, by = data_duplicates_cols)
  }
  
  # Insert data
  
  # Last ID in vocabulary table
  last_id <- get_last_row(m$db, table_name)
  
  if (nrow(data_to_insert) == 0){
    add_log_entry(r = r, category = "Error", name = paste0("import_vocabulary_table - vocabulary_no_data_to_insert"), value = i18n$t("vocabulary_no_data_to_insert"))
    cat(paste0("<span style = 'font-weight:bold; color:#0078D4;'>", i18n$t("vocabulary_no_data_to_insert"), "</span>\n"))
    return(NULL)
  }
  
  else {
    data_to_insert <- data_to_insert %>% dplyr::mutate(id = 1:dplyr::n() + last_id, .before = 1)
    
    tryCatch(DBI::dbAppendTable(m$db, table_name, data_to_insert),
      error = function(e){
        if (nchar(e[1]) > 0) add_log_entry(r = r, category = "Error", name = paste0("import_vocabulary_table - vocabulary_error_append_table"), value = toString(e))
        cat(paste0("<span style = 'font-weight:bold; color:red;'>**", i18n$t("error"), "** ", i18n$t("vocabulary_error_append_table"), "</span>\n"))
        return(NULL)}
    )
  }
  
  # Add nrow to r$import_vocabulary_count_rows
  if (length(r$import_vocabulary_count_rows) > 0) r$import_vocabulary_count_rows <- r$import_vocabulary_count_rows %>%
    dplyr::bind_rows(tibble::tibble(table_name = table_name, n_rows = as.integer(nrow(data_to_insert))))
  
  cat(paste0("<span style = 'font-weight:bold; color:#0078D4;'>", i18n$t("import_vocabulary_table_success"), ". ", nrow(data_to_insert), " ", tolower(i18n$t("rows_inserted")), ".</span>\n"))
}
