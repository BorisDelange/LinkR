#' Import a Dataset in OMOP Format
#'
#' @description 
#' Imports an OMOP Common Data Model dataset into the application, either from a local folder (CSV or Parquet files)
#' or from a live database connection. This function populates the in-app data environment (`d$`) and is typically 
#' used when configuring or reloading a dataset in LinkR.
#'
#' If `data_folder` is provided, the function assumes a file-based import; otherwise, it expects a database connection via `con`.
#'
#' @param omop_version OMOP CDM version of the dataset. Accepted values are `"5.3"` and `"5.4"`. Default is `"5.4"`.
#' @param data_folder Path to the folder containing OMOP tables in `.csv` or `.parquet` format. If specified, `data_source = "disk"` is assumed.
#' @param con A live `DBI` database connection (`DBI::dbConnect(...)`), required only if `data_folder` is not specified. Used when `data_source = "db"`.
#' @param tables_to_load A character vector specifying which OMOP tables to load. If empty, all supported tables are loaded automatically.
#'
#' @details 
#' This function supports two data sources:
#' 
#' - **Disk**: When `data_folder` is specified, the app will try to load OMOP tables from CSV or Parquet files located in that folder.
#'    You can also organize tables into subfolders (one per table) if needed.
#'
#' - **Database**: When `data_folder` is empty and a valid connection is passed via `con`, the data is pulled directly from the database.
#'
#' The function uses an internal DuckDB connection when reading from disk.
#' It supports automatic detection of column types based on the OMOP version.
#'
#' For development and debugging purposes, imported tables are printed with the number of rows loaded (only when used from the "datasets" page).
#'
#' @return A tibble summarizing the number of rows per loaded table (only if current page is "datasets").
#' 
#' @examples
#' \dontrun{
#' # Import from a local folder containing OMOP CSV files
#' import_dataset(data_folder = "~/data/omop_export", omop_version = "5.4")
#'
#' # Import only a few specific tables from Parquet files
#' import_dataset(
#'   data_folder = "~/data/parquet_omop",
#'   tables_to_load = c("person", "observation", "visit_occurrence")
#' )
#'
#' # Import data from a live PostgreSQL database
#' con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "omop", host = "localhost")
#' import_dataset(con = con, tables_to_load = c("person", "measurement"))
#' }
#'
import_dataset <- function(omop_version = "5.4", data_folder = character(), con, tables_to_load = character()){
  
  # Get variables from other environments
  for (obj_name in c("r", "d")) assign(obj_name, get(obj_name, envir = parent.frame()))
  i18n <- r$i18n
  if (r$current_page == "datasets") dataset_id <- eval(parse(text = "input$selected_element"), envir = parent.frame())
  else if (r$current_page == "projects") dataset_id <- r$selected_dataset
  
  # If a data_folder is provided, then data_source is "disk", else it is "db"
  if (length(data_folder) > 0) data_source <- "disk" else data_source <- "db"
  r$dataset_data_source <- data_source
  
  # Check omop_version
  if (omop_version %not_in% c("5.3", "5.4")) return(i18n$t("invalid_omop_version"))
  
  # Load data ----
  
  # Vocabulary tables
  
  vocabulary_tables <- get_omop_vocabulary_tables()
  
  # Col types depending on OMOP CDM version
  
  col_types <- get_omop_col_types(omop_version)
  col_names <- get_omop_col_names(omop_version)
  
  convert_to_duckdb_type <- function(type_code) {
    switch(
      type_code,
       "i" = "BIGINT",
       "n" = "DOUBLE",
       "c" = "VARCHAR",
       "D" = "DATE",
       "T" = "TIMESTAMP",
       "VARCHAR"
    )
  }
  
  # Load tables from tables_to_load argument if specified. If not specified, load all OMOP tables.
  if (length(tables_to_load) == 0) tables_to_load <- names(col_types)
  if (length(tables_to_load) > 0) tables_to_load <- intersect(tables_to_load, names(col_types))
  
  # Record loaded tables
  loaded_tables <- c()
  
  # Folder where data will be saved
  
  data_app_folder <- paste0(r$app_folder, "/datasets_files/", dataset_id)
  if (!dir.exists(data_app_folder)) dir.create(data_app_folder)
  
  # Disconnect db
  if (length(d$con) > 0) if (DBI::dbIsValid(d$con)) DBI::dbDisconnect(d$con)
  
  ## Import data from disk files ----
  
  if (data_source == "disk"){
    
    # List files of provided folder
    
    if (!dir.exists(data_folder)) return(i18n$t("folder_doesnt_exist"))
    
    file_names <- list.files(path = data_folder)
    
    if (length(file_names) == 0) return(i18n$t("folder_doesnt_contain_any_file"))
    
    # Load files with DuckDB in-memory database
    
    d$con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    
    for (file_name in file_names){
      
      table <- sub("\\.[^.]*$", "", file_name)
      file_ext <- sub(".*\\.", "", tolower(file_name))
      
      # If no file_ext, consider it is a folder containing parquet or CSV files
      if (file_ext == table) file_ext <- ""
      
      if (table %not_in% loaded_tables){
        
        # Check if this is an OMOP table
        
        if (table %in% tables_to_load){
          
          if (file_ext %in% c("csv", "parquet", "")){
            
            # If no file_ext, consider it is a folder and try to find extension of first file
            if (file_ext == "") {
              folder_path <- file.path(data_folder, file_name)
              
              if (dir.exists(folder_path)) {
                folder_files <- list.files(folder_path)
                
                if (length(folder_files) > 0) {
                  # Get extension of first file
                  first_file_ext <- sub(".*\\.", "", tolower(folder_files[1]))
                  if (first_file_ext %in% c("csv", "parquet")) {
                    file_ext <- first_file_ext
                  }
                }
              }
            }
            
            if (file_ext %in% c("csv", "parquet")){
              
              if (!DBI::dbExistsTable(d$con, table)){
                
                file_path <- file.path(data_folder, file_name)
                
                if (file_ext == "csv"){
                  type_codes <- strsplit(col_types[[table]], "")[[1]]
                  
                  col_names <- col_names[[table]]
                  col_types_sql <- paste(
                    mapply(
                      function(col, type) {
                        sprintf('"%s" %s', col, convert_to_duckdb_type(type))
                      },
                      col_names,
                      type_codes
                    ),
                    collapse = ", "
                  )
                  
                  col_types_csv <- paste(
                    mapply(
                      function(col, type) {
                        sprintf("'%s': '%s'", col, convert_to_duckdb_type(type))
                      },
                      col_names,
                      type_codes
                    ),
                    collapse = ", "
                  )
                  
                  DBI::dbExecute(
                    d$con,
                    sprintf(
                      "CREATE VIEW %s SELECT * FROM read_csv('%s', nullstr='NA', columns={%s})",
                      table,
                      file.path(data_folder, file_name),
                      col_types_csv
                    )
                  )
                }
                else if (file_ext == "parquet") DBI::dbExecute(d$con, paste0("CREATE VIEW ", table, " AS SELECT * FROM read_parquet('", file_path, "')"))
              }
            }
          }
        }
      }
    }
  }
  
  ## Import data from database connection ----
  
  else if (data_source == "db"){
    
    # Test connection
    if (!DBI::dbIsValid(con)) return(i18n$t("dataset_error_with_db_connection"))
    
    d$con <- con
  }
  
  # Load data in a d$ var
  
  tables <- DBI::dbListTables(d$con)
  
  for (table in tables){
    if (table %in% tables_to_load){
      d[[table]] <- dplyr::tbl(d$con, table) %>% dplyr::rename_with(tolower)
      loaded_tables <- c(loaded_tables, table)
    }
  }
  
  # Count imported tables rows (only for datasets page)
  
  loaded_data <- tibble::tibble(table = character(), n_rows = integer())
  
  if (r$current_page == "datasets"){
    for (table in loaded_tables){
      loaded_data <- loaded_data %>% dplyr::bind_rows(tibble::tibble(table = table, n_rows = d[[table]] %>% dplyr::count() %>% dplyr::pull()))
    }
  }
  
  return(print(loaded_data, n = 100))
}

#' @noRd
import_vocabulary_table <- function(table_name = character(), data = tibble::tibble()){
  
  # Get variables from other environments
  for (obj_name in c("r", "m")) assign(obj_name, get(obj_name, envir = parent.frame()))
  i18n <- r$i18n
  
  # Reset count rows
  rows_inserted <- 0L
  
  if (table_name %not_in% c("concept", "vocabulary", "domain", "concept_class", "concept_relationship", "relationship", "concept_synonym", "concept_ancestor", "drug_strength")){
    cat(paste0("\n", i18n$t("invalid_vocabulary_table")))
    return(c(0L, i18n$t("invalid_vocabulary_table")))
  }
  
  data <- tibble::as_tibble(data)
  
  # Concept tables col types
  col_types <- get_omop_col_types("5.4")
  col_names <- get_omop_col_names("5.4")
  
  if (!identical(colnames(data), col_names[[table_name]])){
    cat(paste0("\n", i18n$t("invalid_col_names")))
    return(c(0L, i18n$t("invalid_col_names")))
  }
  
  # Transform col types
  
  types <- unlist(strsplit(col_types[[table_name]], ""))
  
  for (i in seq_along(types)) {
    col_name <- names(data)[i]
    col_type <- types[[i]]
    
    if (col_type %in% c("c", "D")) data <- data %>% dplyr::mutate_at(col_name, as.character)
    else if (col_type %in% c("i", "n")) data <- data %>% dplyr::mutate_at(col_name, as.numeric)
  }
  
  # Add data to database

  tables_with_primary_key <- c("concept", "vocabulary", "domain", "concept_class", "relationship")

  # Case 1 : table has a primary key, add data not already in database, filtered by primary key

  if (table_name %in% tables_with_primary_key){
    
    # Remove duplicates by keeping first occurrence
    data <-
      data %>% 
      dplyr::group_by_at(paste0(table_name, "_id")) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()

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
    
    data <-
      data %>% 
      dplyr::group_by_at(data_duplicates_cols) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()

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