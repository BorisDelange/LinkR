# URL where CSV files are
base_url <- "https://www.physionet.org/files/mimic-iv-demo-omop/0.9/1_omop_data_csv/"

# Files to download
tables <- c(
    "person", "death", "visit_detail", "visit_occurrence", "observation_period",
    "measurement", "observation", "device_exposure", "procedure_occurrence", "condition_occurrence", "drug_exposure",
    "care_site", "location", "provider", "payer_plan_period", "cost",
    "condition_era", "dose_era",  "drug_era",
    "fact_relationship", "specimen",
    "concept", "concept_relationship", "vocabulary"
)

# Local database files

# Variable to store visit_detail in memory
visit_detail_data <- NULL

# Function to update visit_detail_id using dplyr
update_visit_detail_id <- function(data, visit_detail, table_name, id_column, datetime_column, limit) {
    
    # Count rows with NULL visit_detail_id
    null_count <- sum(is.na(data$visit_detail_id))
    
    if (null_count > limit) {
        # Join with visit_detail to find appropriate visit_detail_id
        data_with_visits <- data %>%
            dplyr::left_join(
                visit_detail %>% 
                    dplyr::select(person_id, visit_detail_id, visit_detail_start_datetime, visit_detail_end_datetime),
                by = "person_id"
            ) %>%
            # Filter visits that encompass the event datetime
            dplyr::filter(
                is.na(visit_detail_id.x) & 
                !!rlang::sym(datetime_column) >= visit_detail_start_datetime & 
                !!rlang::sym(datetime_column) <= visit_detail_end_datetime
            ) %>%
            # Group by event ID and take the first matching visit
            dplyr::group_by(!!rlang::sym(id_column)) %>%
            dplyr::arrange(!!rlang::sym(datetime_column), visit_detail_id.y) %>%
            dplyr::slice_head(n = 1) %>%
            dplyr::ungroup() %>%
            dplyr::select(!!rlang::sym(id_column), visit_detail_id_new = visit_detail_id.y)
        
        # Update visit_detail_id in original data
        data <- data %>%
            dplyr::left_join(data_with_visits, by = id_column) %>%
            dplyr::mutate(
                visit_detail_id = dplyr::if_else(
                    is.na(visit_detail_id) & !is.na(visit_detail_id_new),
                    visit_detail_id_new,
                    visit_detail_id
                )
            ) %>%
            dplyr::select(-visit_detail_id_new)
    }
    
    return(data)
}

# Configuration for tables to process
tables_info <- list(
    measurement = c("measurement_id", "measurement_datetime", 20000),
    observation = c("observation_id", "observation_datetime", 1000),
    condition_occurrence = c("condition_occurrence_id", "condition_start_datetime", 1000),
    device_exposure = c("device_exposure_id", "device_exposure_start_datetime", 1),
    drug_exposure = c("drug_exposure_id", "drug_exposure_start_datetime", 300),
    procedure_occurrence = c("procedure_occurrence_id", "procedure_datetime", 3000)
)

# Download files
for (table in tables) {
    
    if (table %in% c("concept", "concept_relationship", "vocabulary")) file_url <- paste0(base_url, "2b_", table, ".csv")
    else file_url <- paste0(base_url, table, ".csv")
    local_file <- file.path("%dataset_folder%", paste0(table, ".parquet"))

    if (!file.exists(local_file)) {
    
        # Download and read the CSV file
        data <- suppressWarnings(vroom::vroom(file_url, progress = FALSE, show_col_types = FALSE))
        
        # Correct _id cols, with large positive and negative numeric values
        correct_id_columns <- function(data) {
            id_columns <- colnames(data)[grepl("_id$", colnames(data)) & !grepl("concept_id$", colnames(data))]
            if (nrow(data) > 0) data <- data %>% dplyr::mutate(dplyr::across(dplyr::all_of(id_columns), ~ if (is.numeric(.)) as.integer(. / 10^12 + 10^7) else .))
            
            colnames(data) <- tolower(colnames(data))
            return(data)
        }
        data <- correct_id_columns(data)
        
        # Specific transformations per table
        if (table == "person") {
            data <- data %>%
                dplyr::mutate(
                    birth_datetime = as.POSIXct(paste0(year_of_birth, "-01-01"), format = "%Y-%m-%d", tz = "UTC")
                )
        } else if (table == "condition_occurrence") {
            data <- data %>% dplyr::relocate(condition_status_concept_id, .after = "condition_type_concept_id")
        } else if (table == "visit_detail") {
            # Store visit_detail in memory for later use
            visit_detail_data <- data
            
            # Relocate columns for visit_detail
            data <- data %>%
                dplyr::relocate(visit_detail_source_value, .after = "care_site_id") %>%
                dplyr::relocate(visit_detail_source_concept_id, .after = "visit_detail_source_value") %>%
                dplyr::relocate(admitting_source_value, .after = "visit_detail_source_concept_id") %>%
                dplyr::relocate(discharge_to_source_value, .after = "admitting_source_concept_id")
        }
        
        # Update visit_detail_id if necessary AND if visit_detail_data is available
        if (table %in% names(tables_info) && !is.null(visit_detail_data)) {
            id_column <- tables_info[[table]][1]
            datetime_column <- tables_info[[table]][2]
            limit <- as.numeric(tables_info[[table]][3])
            
            data <- update_visit_detail_id(data, visit_detail_data, table, id_column, datetime_column, limit)
        }
        
        # Write the data to a Parquet file
        arrow::write_parquet(data, local_file)
    }
}

# Import data
import_dataset(omop_version = "5.3", data_folder = "%dataset_folder%")
