#' Get OMOP column names per table for a given version
#'
#' @param version OMOP CDM version ("5.3" or "5.4")
#' @return Named list of character vectors (column names)
get_omop_col_names <- function(version = "5.4") {
  if (!(version %in% c("5.3", "5.4"))) stop("Unsupported OMOP version")
  
  # v5.3: https://github.com/OHDSI/CommonDataModel/blob/main/inst/ddl/5.3/duckdb/OMOPCDM_duckdb_5.3_ddl.sql
  # v5.4: https://github.com/OHDSI/CommonDataModel/blob/main/inst/ddl/5.4/duckdb/OMOPCDM_duckdb_5.4_ddl.sql
  
  col_names <- list(
    care_site = c("care_site_id", "care_site_name", "place_of_service_concept_id", "location_id", "care_site_source_value", "place_of_service_source_value"),
    cdm_source = c("cdm_source_name", "cdm_source_abbreviation", "cdm_holder", "source_description", "source_documentation_reference", "cdm_etl_reference", "source_release_date", "cdm_release_date", "cdm_version", "cdm_version_concept_id", "vocabulary_version"),
    cohort = c("cohort_definition_id", "subject_id", "cohort_start_date", "cohort_end_date"),
    cohort_definition = c("cohort_definition_id", "cohort_definition_name", "cohort_definition_description", "definition_type_concept_id", "cohort_definition_syntax", "subject_concept_id", "cohort_initiation_date"),
    concept = c("concept_id", "concept_name", "domain_id", "vocabulary_id", "concept_class_id", "standard_concept", "concept_code", "valid_start_date", "valid_end_date", "invalid_reason"),
    concept_ancestor = c("ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation", "max_levels_of_separation"),
    concept_class = c("concept_class_id", "concept_class_name", "concept_class_concept_id"),
    concept_relationship = c("concept_id_1", "concept_id_2", "relationship_id", "valid_start_date", "valid_end_date", "invalid_reason"),
    concept_synonym = c("concept_id", "concept_synonym_name", "language_concept_id"),
    condition_era = c("condition_era_id", "person_id", "condition_concept_id", "condition_era_start_date", "condition_era_end_date", "condition_occurrence_count"),
    condition_occurrence = c("condition_occurrence_id", "person_id", "condition_concept_id", "condition_start_date", "condition_start_datetime", "condition_end_date", "condition_end_datetime", "condition_type_concept_id", "condition_status_concept_id", "stop_reason", "provider_id", "visit_occurrence_id", "visit_detail_id", "condition_source_value", "condition_source_concept_id", "condition_status_source_value"),
    death = c("person_id", "death_date", "death_datetime", "death_type_concept_id", "cause_concept_id", "cause_source_value", "cause_source_concept_id"),
    domain = c("domain_id", "domain_name", "domain_concept_id"),
    dose_era = c("dose_era_id", "person_id", "drug_concept_id", "unit_concept_id", "dose_value", "dose_era_start_date", "dose_era_end_date"),
    drug_era = c("drug_era_id", "person_id", "drug_concept_id", "drug_era_start_date", "drug_era_end_date", "drug_exposure_count", "gap_days"),
    drug_exposure = c("drug_exposure_id", "person_id", "drug_concept_id", "drug_exposure_start_date", "drug_exposure_start_datetime", "drug_exposure_end_date", "drug_exposure_end_datetime", "verbatim_end_date", "drug_type_concept_id", "stop_reason", "refills", "quantity", "days_supply", "sig", "route_concept_id", "lot_number", "provider_id", "visit_occurrence_id", "visit_detail_id", "drug_source_value", "drug_source_concept_id", "route_source_value", "dose_unit_source_value"),
    drug_strength = c("drug_concept_id", "ingredient_concept_id", "amount_value", "amount_unit_concept_id", "numerator_value", "numerator_unit_concept_id", "denominator_value", "denominator_unit_concept_id", "box_size", "valid_start_date", "valid_end_date", "invalid_reason"),
    episode = c("episode_id", "person_id", "episode_concept_id", "episode_start_date", "episode_start_datetime", "episode_end_date", "episode_end_datetime", "episode_parent_id", "episode_number", "episode_object_concept_id", "episode_type_concept_id", "episode_source_value", "episode_source_concept_id"),
    episode_event = c("episode_id", "event_id", "episode_event_field_concept_id"),
    fact_relationship = c("domain_concept_id_1", "fact_id_1", "domain_concept_id_2", "fact_id_2", "relationship_concept_id"),
    location_history = c("location_id", "relationship_type_concept_id", "domain_id", "entity_id", "start_date", "end_date"),
    note_nlp = c("note_nlp_id", "note_id", "section_concept_id", "snippet", "offset", "lexical_variant", "note_nlp_concept_id", "note_nlp_source_concept_id", "nlp_system", "nlp_date", "nlp_datetime", "term_exists", "term_temporal", "term_modifiers"),
    observation_period = c("observation_period_id", "person_id", "observation_period_start_date", "observation_period_end_date", "period_type_concept_id"),
    payer_plan_period = c("payer_plan_period_id", "person_id", "payer_plan_period_start_date", "payer_plan_period_end_date", "payer_concept_id", "payer_source_value", "payer_source_concept_id", "plan_concept_id", "plan_source_value", "plan_source_concept_id", "sponsor_concept_id", "sponsor_source_value", "sponsor_source_concept_id", "family_source_value", "stop_reason_concept_id", "stop_reason_source_value", "stop_reason_source_concept_id"),
    person = c("person_id", "gender_concept_id", "year_of_birth", "month_of_birth", "day_of_birth", "birth_datetime", "race_concept_id", "ethnicity_concept_id", "location_id", "provider_id", "care_site_id", "person_source_value", "gender_source_value", "gender_source_concept_id", "race_source_value", "race_source_concept_id", "ethnicity_source_value", "ethnicity_source_concept_id"),
    provider = c("provider_id", "provider_name", "npi", "dea", "specialty_concept_id", "care_site_id", "year_of_birth", "gender_concept_id", "provider_source_value", "specialty_source_value", "specialty_source_concept_id", "gender_source_value", "gender_source_concept_id"),
    relationship = c("relationship_id", "relationship_name", "is_hierarchical", "defines_ancestry", "reverse_relationship_id", "relationship_concept_id"),
    source_to_concept_map = c("source_code", "source_concept_id", "source_vocabulary_id", "source_code_description", "target_concept_id", "target_vocabulary_id", "valid_start_date", "valid_end_date", "invalid_reason"),
    specimen = c("specimen_id", "person_id", "specimen_concept_id", "specimen_type_concept_id", "specimen_date", "specimen_datetime", "quantity", "unit_concept_id", "anatomic_site_concept_id", "disease_status_concept_id", "specimen_source_id", "specimen_source_value", "unit_source_value", "anatomic_site_source_value", "disease_status_source_value"),
    survey_conduct = c("survey_conduct_id", "person_id", "survey_concept_id", "survey_start_date", "survey_start_datetime", "survey_end_date", "survey_end_datetime", "provider_id", "assisted_concept_id", "respondent_type_concept_id", "timing_concept_id", "collection_method_concept_id", "assisted_source_value", "respondent_type_source_value", "timing_source_value", "collection_method_source_value", "survey_source_value", "survey_source_concept_id", "survey_source_identifier", "validated_survey_concept_id", "validated_survey_source_value", "survey_version_number", "visit_occurrence_id", "response_visit_occurrence_id"),
    vocabulary = c("vocabulary_id", "vocabulary_name", "vocabulary_reference", "vocabulary_version", "vocabulary_concept_id")
  )
  
  if (version == "5.3"){
    
    col_names$attribute_definition <- c("attribute_definition_id", "attribute_name", "attribute_description", "attribute_type_concept_id", "attribute_syntax")
    col_names$cost <- c("cost_id", "cost_event_id", "cost_domain_id", "cost_type_concept_id", "currency_concept_id", "total_charge", "total_cost", "total_paid", "paid_by_payer", "paid_by_patient", "paid_patient_copay", "paid_patient_coinsurance", "paid_patient_deductible", "paid_by_primary", "paid_ingredient_cost", "paid_dispensing_fee", "payer_plan_period_id", "amount_allowed", "revenue_code_concept_id", "revenue_code_source_value", "drg_concept_id", "drg_source_value")
    col_names$device_exposure <- c("device_exposure_id", "person_id", "device_concept_id", "device_exposure_start_date", "device_exposure_start_datetime", "device_exposure_end_date", "device_exposure_end_datetime", "device_type_concept_id", "unique_device_id", "quantity", "provider_id", "visit_occurrence_id", "visit_detail_id", "device_source_value", "device_source_concept_id")
    col_names$location <- c("location_id", "address_1", "address_2", "city", "state", "zip", "county", "location_source_value")
    col_names$measurement <- c("measurement_id", "person_id", "measurement_concept_id", "measurement_date", "measurement_datetime", "measurement_time", "measurement_type_concept_id", "operator_concept_id", "value_as_number", "value_as_concept_id", "unit_concept_id", "range_low", "range_high", "provider_id", "visit_occurrence_id", "visit_detail_id", "measurement_source_value", "measurement_source_concept_id", "unit_source_value", "value_source_value")
    col_names$metadata <- c("metadata_concept_id", "metadata_type_concept_id", "name", "value_as_string", "value_as_concept_id", "metadata_date", "metadata_datetime")
    col_names$note <- c("note_id", "person_id", "note_date", "note_datetime", "note_type_concept_id", "note_class_concept_id", "note_title", "note_text", "encoding_concept_id", "language_concept_id", "provider_id", "visit_occurrence_id", "visit_detail_id", "note_source_value")
    col_names$observation <- c("observation_id", "person_id", "observation_concept_id", "observation_date", "observation_datetime", "observation_type_concept_id", "value_as_number", "value_as_string", "value_as_concept_id", "qualifier_concept_id", "unit_concept_id", "provider_id", "visit_occurrence_id", "visit_detail_id", "observation_source_value", "observation_source_concept_id", "unit_source_value", "qualifier_source_value")
    col_names$procedure_occurrence <- c("procedure_occurrence_id", "person_id", "procedure_concept_id", "procedure_date", "procedure_datetime", "procedure_type_concept_id", "modifier_concept_id", "quantity", "provider_id", "visit_occurrence_id", "visit_detail_id", "procedure_source_value", "procedure_source_concept_id", "modifier_source_value")
    col_names$visit_detail <- c("visit_detail_id", "person_id", "visit_detail_concept_id", "visit_detail_start_date", "visit_detail_start_datetime", "visit_detail_end_date", "visit_detail_end_datetime", "visit_detail_type_concept_id", "provider_id", "care_site_id", "visit_detail_source_value", "visit_detail_source_concept_id", "admitting_source_value", "admitting_source_concept_id", "discharge_to_source_value", "discharge_to_concept_id", "preceding_visit_detail_id", "visit_detail_parent_id", "visit_occurrence_id")
    col_names$visit_occurrence = c("visit_occurrence_id", "person_id", "visit_concept_id", "visit_start_date", "visit_start_datetime", "visit_end_date", "visit_end_datetime", "visit_type_concept_id", "provider_id", "care_site_id", "visit_source_value", "visit_source_concept_id", "admitting_source_concept_id", "admitting_source_value", "discharge_to_concept_id", "discharge_to_source_value", "preceding_visit_occurrence_id")
  } 
  
  else if (version == "5.4"){
    
    col_names$cost <- c("cost_id", "cost_event_id", "cost_domain_id", "cost_type_concept_id", "currency_concept_id", "total_charge", "total_cost", "total_paid", "paid_by_payer", "paid_by_patient", "paid_patient_copay", "paid_patient_coinsurance", "paid_patient_deductible", "paid_by_primary", "paid_ingredient_cost", "paid_dispensing_fee", "payer_plan_period_id", "amount_allowed", "revenue_code_concept_id", "revenue_code_source_value", "drg_concept_id", "drg_source_value")
    col_names$device_exposure <- c("device_exposure_id", "person_id", "device_concept_id", "device_exposure_start_date", "device_exposure_start_datetime", "device_exposure_end_date", "device_exposure_end_datetime", "device_type_concept_id", "unique_device_id", "production_id", "quantity", "provider_id", "visit_occurrence_id", "visit_detail_id", "device_source_value", "device_source_concept_id", "unit_concept_id", "unit_source_value", "unit_source_concept_id")
    col_names$location <- c("location_id", "address_1", "address_2", "city", "state", "zip", "county", "location_source_value", "country_concept_id", "country_source_value", "latitude", "longitude")
    col_names$measurement <- c("measurement_id", "person_id", "measurement_concept_id", "measurement_date", "measurement_datetime", "measurement_time", "measurement_type_concept_id", "operator_concept_id", "value_as_number", "value_as_concept_id", "unit_concept_id", "range_low", "range_high", "provider_id", "visit_occurrence_id", "visit_detail_id", "measurement_source_value", "measurement_source_concept_id", "unit_source_value", "unit_source_concept_id", "value_source_value", "measurement_event_id", "meas_event_field_concept_id")
    col_names$metadata <- c("metadata_id", "metadata_concept_id", "metadata_type_concept_id", "name", "value_as_string", "value_as_concept_id", "value_as_number", "metadata_date", "metadata_datetime")
    col_names$note <- c("note_id", "person_id", "note_date", "note_datetime", "note_type_concept_id", "note_class_concept_id", "note_title", "note_text", "encoding_concept_id", "language_concept_id", "provider_id", "visit_occurrence_id", "visit_detail_id", "note_source_value", "note_event_id", "note_event_field_concept_id")
    col_names$observation <- c("observation_id", "person_id", "observation_concept_id", "observation_date", "observation_datetime", "observation_type_concept_id", "value_as_number", "value_as_string", "value_as_concept_id", "qualifier_concept_id", "unit_concept_id", "provider_id", "visit_occurrence_id", "visit_detail_id", "observation_source_value", "observation_source_concept_id", "unit_source_value", "qualifier_source_value", "value_source_value", "observation_event_id", "obs_event_field_concept_id")
    col_names$procedure_occurrence <- c("procedure_occurrence_id", "person_id", "procedure_concept_id", "procedure_date", "procedure_datetime", "procedure_end_date", "procedure_end_datetime", "procedure_type_concept_id", "modifier_concept_id", "quantity", "provider_id", "visit_occurrence_id", "visit_detail_id", "procedure_source_value", "procedure_source_concept_id", "modifier_source_value")
    col_names$visit_detail <- c("visit_detail_id", "person_id", "visit_detail_concept_id", "visit_detail_start_date", "visit_detail_start_datetime", "visit_detail_end_date", "visit_detail_end_datetime", "visit_detail_type_concept_id", "provider_id", "care_site_id", "visit_detail_source_value", "visit_detail_source_concept_id", "admitted_from_concept_id", "admitted_from_source_value", "discharged_to_source_value", "discharged_to_concept_id", "preceding_visit_detail_id", "parent_visit_detail_id", "visit_occurrence_id")
    col_names$visit_occurrence <- c("visit_occurrence_id", "person_id", "visit_concept_id", "visit_start_date", "visit_start_datetime", "visit_end_date", "visit_end_datetime", "visit_type_concept_id", "provider_id", "care_site_id", "visit_source_value", "visit_source_concept_id", "admitted_from_concept_id", "admitted_from_source_value", "discharged_to_concept_id", "discharged_to_source_value", "preceding_visit_occurrence_id")
  }
  
  return(col_names)
}

#' Get OMOP column types per table for a given version
#'
#' @param version OMOP CDM version ("5.3" or "5.4")
#' @return Named list of character strings encoding column types
get_omop_col_types <- function(version = "5.4") {
  if (!(version %in% c("5.3", "5.4"))) stop("Unsupported OMOP version")
  
  # v5.3: https://github.com/OHDSI/CommonDataModel/blob/main/inst/ddl/5.3/duckdb/OMOPCDM_duckdb_5.3_ddl.sql
  # v5.4: https://github.com/OHDSI/CommonDataModel/blob/main/inst/ddl/5.4/duckdb/OMOPCDM_duckdb_5.4_ddl.sql
  
  # Base: common to 5.3 and 5.4 (shared)
  col_types <- list(
    care_site = "iciicc",
    cdm_source = "ccccccDDcci",
    cohort = "iiDD",
    cohort_definition = "icciici",
    concept = "iccccccDDc",
    concept_ancestor = "iiii",
    concept_class = "cci",
    concept_relationship = "iicDDc",
    concept_synonym = "ici",
    condition_era = "iiiDDi",
    condition_occurrence = "iiiDTDTiiciiicic",
    death = "iDTiici",
    domain = "cci",
    dose_era = "iiiinDD",
    drug_era = "iiiDDii",
    drug_exposure = "iiiDTDTDiciniciciiicicc",
    drug_strength = "iininininDDc",
    episode = "iiDTDTiiiicci",
    episode_event = "iii",
    fact_relationship = "iiiii",
    location_history = "iiciDD",
    note_nlp = "iiiccciicDTccc",
    observation_period = "iiDDi",
    payer_plan_period = "iiDDiciiciicicici",
    person = "iiiiiTiiiiiccicici",
    provider = "iccciiiiccici",
    relationship = "ccccci",
    source_to_concept_map = "cicicDDc",
    specimen = "iiiiDTniiiccccc",
    survey_conduct = "iiiDTDTiiiiiiccccciciiic",
    vocabulary = "cccci"
  )
  
  if (version == "5.3") {
    
    col_types$attribute_definition <- "iccic"
    col_types$cost <- "iiciinnnnnnnnnnninicic"
    col_types$device_exposure <- "iiiDTDTiciiiici"
    col_types$location <- "iccccccc"
    col_types$measurement <- "iiiDTciiniinniiicicc"
    col_types$metadata <- "iicciDT"
    col_types$note <- "iiDTiicciiiiic"
    col_types$observation <- "iiiDTinciiiiiicicc"
    col_types$procedure_occurrence <- "iiiDTiiiiiicic"
    col_types$visit_detail <- "iiiDTDTiiiciciciiii"
    col_types$visit_occurrence <- "iiiDTDTiiiciicici"
  } 
  
  else if (version == "5.4") {
    
    col_types$cost <- "iiiiiiicinDDDiicci"
    col_types$device_exposure <- "iiiDTDTicciiiiciici"
    col_types$location <- "icccccccicnn"
    col_types$measurement <- "iiiDTciiniinniiicicicii"
    col_types$metadata <- "iiiccinDT"
    col_types$note <- "iiDTiicciiiiicii"
    col_types$observation <- "iiiDTinciiiiiicicccii"
    col_types$procedure_occurrence <- "iiiDTDTiiiiiicic"
    col_types$visit_detail <- "iiiDTDTiiiciicciiii"
    col_types$visit_occurrence <- "iiiDTDTiiiciicici"
  }
  
  return(col_types)
}

#' Get OMOP vocabulary table names
#'
#' @return A character vector of OMOP vocabulary table names
get_omop_vocabulary_tables <- function() {
  c(
    "concept", "vocabulary", "domain", "concept_class",
    "concept_relationship", "relationship", "concept_synonym",
    "concept_ancestor", "drug_strength"
  )
}
