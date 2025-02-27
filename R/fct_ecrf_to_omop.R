#' @title Convert eCRF patient data to OMOP CDM Person format
#' @description 
#' Converts patient data from an eCRF (electronic Case Report Form) source format to the 
#' OMOP CDM Person format (version 5.4). The function creates a unique identifier for each 
#' patient and handles demographic data such as gender and birth date. If duplicate records 
#' are detected for the same patient (different values for gender or birth date), the function 
#' keeps the first value encountered and issues a warning.
#'
#' @param data A dataframe or tibble containing source patient data from the eCRF
#' @param person_id_col Name of the column containing the patient identifier (character string)
#' @param gender_col Name of the column containing the patient's gender (character string)
#' @param gender_concepts A named vector where names are the gender values in the source data
#'    and values are the corresponding OMOP concept_ids from the ATHENA vocabulary.
#'    For example: c("M" = 8507, "F" = 8532, "O" = 0)
#' @param birth_datetime_col Name of the column containing the birth date (character string)
#'
#' @return A tibble conforming to the structure of the PERSON table in OMOP CDM v5.4
#'
#' @examples
#' # Create sample eCRF data
#' ecrf_patient_data <- tibble::tibble(
#'   patient_id = c("P001", "P002", "P001"),
#'   hospital_id = c("H01", "H01", "H01"),
#'   visit_id = c("V0001", "V0023", "V0012"),
#'   gender = c("M", "F", "M"),
#'   age = c(67, 53, 67),
#'   admission_date = as.Date(c("2024-01-05", "2024-01-07", "2024-03-12")),
#'   birth_date = as.Date(c("1957-01-05", "1971-01-07", "1957-03-12"))
#' )
#'
#' # Create a gender mapping
#' gender_map <- c("M" = 8507, "F" = 8532, "O" = 0)
#'
#' # Convert the data
#' person_table <- ecrf_to_omop_person(
#'   data = ecrf_patient_data,
#'   person_id_col = "patient_id",
#'   gender_col = "gender",
#'   gender_concepts = gender_map,
#'   birth_datetime_col = "birth_date"
#' )
#'
#' # View the resulting OMOP Person table
#' print(person_table)
#' # Output will include a warning about patient P001 having conflicting demographic info,
#' # and will contain the following 2 rows (simplified):
#' # # A tibble: 2 × 18
#' #   person_id gender_concept_id year_of_birth month_of_birth day_of_birth
#' #       <int>            <int>         <int>          <int>        <int>
#' # 1         1             8507          1957              1            5
#' # 2         2             8532          1971              1            7
#' # # … with 13 more variables including birth_datetime, person_source_value, etc.
#'
#' @importFrom dplyr %>% select mutate group_by summarise first n left_join distinct row_number filter
#' @importFrom tidyr replace_na
#' @importFrom lubridate year month day
#' @importFrom rlang .data sym
#'
#' @export
ecrf_to_omop_person <- function(data, person_id_col, gender_col = NULL, gender_concepts = NULL, birth_datetime_col = NULL) {
  
  # Check arguments
  if (!person_id_col %in% colnames(data)) {
    stop(paste0("Column '", person_id_col, "' not found in the data."))
  }
  
  # Extract only relevant columns to minimize memory usage
  cols_to_extract <- c(person_id_col)
  if (!is.null(gender_col) && gender_col %in% colnames(data)) {
    cols_to_extract <- c(cols_to_extract, gender_col)
  }
  if (!is.null(birth_datetime_col) && birth_datetime_col %in% colnames(data)) {
    cols_to_extract <- c(cols_to_extract, birth_datetime_col)
  }
  
  # Select relevant columns
  person_data <- data %>%
    dplyr::select(dplyr::all_of(cols_to_extract))
  
  # Check for duplicate patient IDs with different demographic information
  duplicates <- person_data %>%
    dplyr::group_by(!!rlang::sym(person_id_col)) %>%
    dplyr::summarise(
      distinct_genders = ifelse(!is.null(gender_col) && gender_col %in% colnames(data), dplyr::n_distinct(.data[[gender_col]], na.rm = TRUE), 1),
      distinct_birth_dates = ifelse(!is.null(birth_datetime_col) && birth_datetime_col %in% colnames(data), dplyr::n_distinct(.data[[birth_datetime_col]], na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    dplyr::filter(distinct_genders > 1 | distinct_birth_dates > 1)
  
  # Warn about duplicates
  if (nrow(duplicates) > 0) {
    warning(paste0(
      "Found ", nrow(duplicates), " patients with conflicting demographic information. ",
      "First encountered values will be used for each patient. ",
      "Affected patient IDs: ", paste(duplicates[[person_id_col]], collapse = ", ")
    ))
  }
  
  # Get unique person records (keeping first occurrence in case of duplicates)
  unique_persons <- person_data %>%
    dplyr::group_by(!!rlang::sym(person_id_col)) %>%
    dplyr::summarise(
      gender_source_value = ifelse(!is.null(gender_col) && gender_col %in% colnames(data), dplyr::first(.data[[gender_col]]), NA_character_),
      birth_datetime = dplyr::first(.data[[birth_datetime_col]]),
      .groups = "drop"
    )
  
  # Explicitly ensure birth_datetime is a date object (if not NA)
  unique_persons <- unique_persons %>%
    dplyr::mutate(
      birth_datetime = dplyr::if_else(
        !is.na(birth_datetime),
        lubridate::as_date(as.character(birth_datetime)),  # Force conversion via character
        lubridate::as_date(NA)
      )
    )
  
  # Generate OMOP person_id
  unique_persons <- unique_persons %>%
    dplyr::mutate(person_id = dplyr::row_number())
  
  # Handle gender mapping
  if (!is.null(gender_col) && gender_col %in% colnames(data) && !is.null(gender_concepts)) {
    # Map gender values using the named vector
    unique_persons <- unique_persons %>%
      dplyr::mutate(
        gender_concept_id = dplyr::if_else(
          !is.na(.data$gender_source_value) & .data$gender_source_value %in% names(gender_concepts),
          as.integer(gender_concepts[.data$gender_source_value]),
          0L
        ),
        gender_source_concept_id = 0
      )
  } else {
    # Default values if gender mapping is not provided
    unique_persons <- unique_persons %>%
      dplyr::mutate(
        gender_concept_id = 0L,
        gender_source_concept_id = 0L
      )
  }
  
  # Handle birth date components
  if (!is.null(birth_datetime_col) && birth_datetime_col %in% colnames(data)) {
    unique_persons <- unique_persons %>%
      dplyr::mutate(
        # Extract components with safer fallbacks
        year_of_birth = dplyr::if_else(
          !is.na(birth_datetime),
          as.integer(format(birth_datetime, "%Y")),
          NA_integer_
        ),
        month_of_birth = dplyr::if_else(
          !is.na(birth_datetime),
          as.integer(format(birth_datetime, "%m")),
          NA_integer_
        ),
        day_of_birth = dplyr::if_else(
          !is.na(birth_datetime),
          as.integer(format(birth_datetime, "%d")),
          NA_integer_
        )
      )
  } else {
    # Default values if birth date is not provided
    unique_persons <- unique_persons %>%
      dplyr::mutate(
        birth_datetime = NA,
        year_of_birth = NA_integer_,
        month_of_birth = NA_integer_,
        day_of_birth = NA_integer_
      )
  }
  
  # Complete the PERSON table with all required OMOP CDM v5.4 fields
  omop_person <- unique_persons %>%
    dplyr::mutate(
      # Default values for standard OMOP Person fields
      race_concept_id = 0,
      ethnicity_concept_id = 0,
      location_id = NA_integer_,
      provider_id = NA_integer_,
      care_site_id = NA_integer_,
      person_source_value = as.character(.data[[rlang::sym(person_id_col)]]),
      race_source_value = NA_character_,
      race_source_concept_id = 0,
      ethnicity_source_value = NA_character_,
      ethnicity_source_concept_id = 0
    ) %>%
    # Select and order columns according to OMOP CDM v5.4 spec
    dplyr::select(
      person_id, 
      gender_concept_id,
      year_of_birth,
      month_of_birth,
      day_of_birth,
      birth_datetime,
      race_concept_id,
      ethnicity_concept_id,
      location_id,
      provider_id,
      care_site_id,
      person_source_value,
      gender_source_value,
      gender_source_concept_id,
      race_source_value,
      race_source_concept_id,
      ethnicity_source_value,
      ethnicity_source_concept_id
    )
  
  # Return as tibble to ensure consistent output format
  return(tibble::as_tibble(omop_person))
}
