#' Insert default data
#'
#' @description Insert default data in app database, from a remote git if an internet connection is available
#' @param output Shiny output variable
#' @param r Shiny reactive value, used to communicate between modules
#' @param m Shiny reactive value, used to communicate between modules
#' @param i18n Translator object from shiny.i18n library
#' @param language Selected language, "en" or "fr" (character) 
#' @param db_col_types A tibble containing the col_types by table, used by vroom or readr to read csv files (tibble)
#' @param users_accesses_toggles_options A tibble containing users accesses, to add in database if no internet access (tibble)
#' @examples 
#' \dontrun{
#' insert_default_data(output = output, r = r, m = m, i18n = i18n, language = "en",
#'   db_col_types = db_col_types, users_accesses_toggles_options = users_accesses_toggles_options)
#' }

insert_default_data <- function(output, r = shiny::reactiveValues(), m = shiny::reactiveValues(), 
  i18n = character(), language = "en", db_col_types = tibble::tibble(), users_accesses_toggles_options = tibble::tibble()){
  
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM users")) == 0) {
    
    # If internet connection, download default database
    if (r$has_internet){
      
      error_loading_database <- FALSE
      
      # Download csv files
      tryCatch({
        
        db_folder <- paste0("https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/app_database/v", r$app_version, "/", language)
        
        sapply (1:nrow(db_col_types), function(i){
          
          row <- db_col_types[i, ]
          table <- row$table
          if (row$db == "main") con <- r$db
          if (row$db == "public") con <- m$db
          
          if (table != "log"){
            col_types_temp <- db_col_types %>% dplyr::filter(table == !!table) %>% dplyr::pull(col_types)
            temp <- readr::read_csv(paste0(db_folder, "/", table, ".csv"), col_types = col_types_temp, progress = FALSE)
            
            sql <- glue::glue_sql("DELETE FROM {table}", .con = con)
            query <- DBI::dbSendStatement(con, sql)
            DBI::dbClearResult(query)
            
            DBI::dbAppendTable(con, table, temp)
          }
        })
        
      }, error = function(e) {
        report_bug(r = r, output = output, error_message = "error_importing_default_database",
          error_name = "import_default_database", category = "Error", error_report = toString(e), i18n = i18n)
        error_loading_database <<- TRUE
        })
    }
    
    # Else, add only users accounts
    
    if (!r$has_internet | error_loading_database){
      
      # Add default users
      if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM users")) == 0){
        DBI::dbAppendTable(r$db, "users", tibble::tribble(
          ~id, ~username, ~firstname, ~lastname, ~password, ~user_access_id, ~user_status_id, ~datetime, ~deleted,
          1, "admin", "Alan", "Turing", rlang::hash("admin"), 1, 1, as.character(Sys.time()), FALSE,
          2, "test1", "Ada", "Lovelace", rlang::hash("test1"), 2, 1, as.character(Sys.time()), FALSE,
          3, "test2", "Yann", "LeCun", rlang::hash("test2"), 2, 1, as.character(Sys.time()), FALSE,
          4, "test3", "Andrew", "Ng", rlang::hash("test3"), 2, 1, as.character(Sys.time()), FALSE,
          5, "test4", "Hadley", "Wickham", rlang::hash("test4"), 2, 1, as.character(Sys.time()), FALSE))
      }
      
      # Add default user access
      if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM users_accesses")) == 0){
        DBI::dbAppendTable(r$db, "users_accesses", tibble::tribble(
          ~id, ~name, ~description, ~datetime, ~deleted,
          1, i18n$t("admin"), i18n$t("admin_access"), as.character(Sys.time()), FALSE,
          2, i18n$t("user"), i18n$t("user_access"), as.character(Sys.time()), FALSE))
      }
      
      # Add default user status
      if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM users_statuses")) == 0){
        DBI::dbAppendTable(r$db, "users_statuses", tibble::tribble(
          ~id, ~name, ~description, ~datetime, ~deleted,
          1, i18n$t("data_scientist"), "", as.character(Sys.time()), FALSE,
          2, i18n$t("clinician"), "", as.character(Sys.time()), FALSE,
          3, i18n$t("statistician"), "", as.character(Sys.time()), FALSE))
      }
      
      # Add default users accesses
      last_row <- get_last_row(r$db, "options")
      
      # Users accesses
      data <- tibble::tribble(~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted)
      
      # Loop over all toggles, set 1 to value_num for admin, 0 for test
      for(link_id in 1:2){
        for(i in 1:nrow(users_accesses_toggles_options)){
          data <- data %>% dplyr::bind_rows(
            tibble::tribble(
              ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
              "users_accesses", link_id, users_accesses_toggles_options[[i, "name"]], "", ifelse(link_id == 1, 1, 0), 1, as.character(Sys.time()), FALSE)
          )
          if (users_accesses_toggles_options[[i, "toggles"]] != ""){
            for(toggle in users_accesses_toggles_options[[i, "toggles"]][[1]]){
              
              data <- data %>% dplyr::bind_rows(
                tibble::tribble(~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
                  "users_accesses", link_id, toggle, "", ifelse(link_id == 1, 1, 0), 1, as.character(Sys.time()), FALSE)
              )
            }
          }
        }
      }
      
      # Attribute id values
      last_row <- get_last_row(r$db, "options")
      data$id <- seq.int(nrow(data)) + last_row
      
      # Set rights to test user
      test_user_rights <- c(
        "general_settings", "change_password_card",
        "studies", "studies_messages_card", "studies_description_card", "studies_datatable_card", "studies_options_card",
        "subsets", "subsets_datatable_card", "subsets_edit_code_card", "subsets_persons_card",
        "vocabularies", "vocabularies_concepts_card", "vocabularies_mapping_card",
        "plugins", "all_plugins_card",
        "log", "only_me"
      )
      
      data <- data %>% dplyr::mutate(value_num = dplyr::case_when((link_id == 2 & name %in% test_user_rights) ~ 1, TRUE ~ value_num))
      
      # Add new values to database
      DBI::dbAppendTable(r$db, "options", data)
    }
  }
}
