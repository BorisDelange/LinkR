#' @noRd
insert_default_data <- function(output, r, m, i18n, language, db_col_types, users_accesses_toggles_options){
  
  error_loading_database <- FALSE
  
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM users")) == 0) {
    
    # Add default users
    if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM users")) == 0){
      DBI::dbAppendTable(r$db, "users", tibble::tribble(
        ~id, ~username, ~firstname, ~lastname, ~password, ~user_access_id, ~user_status_id, ~datetime, ~deleted,
        1, "admin", "Jane", "Doe", bcrypt::hashpw("admin"), 1, 1, now(), FALSE,
        2, "test", "John", "Doe", bcrypt::hashpw("test"), 2, 1, now(), FALSE))
    }
    
    # Add default user access
    if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM users_accesses")) == 0){
      DBI::dbAppendTable(r$db, "users_accesses", tibble::tribble(
        ~id, ~name, ~description, ~datetime, ~deleted,
        1, i18n$t("admin"), i18n$t("admin_access"), now(), FALSE,
        2, i18n$t("user"), i18n$t("user_access"), now(), FALSE))
    }
    
    # Add default user status
    if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM users_statuses")) == 0){
      DBI::dbAppendTable(r$db, "users_statuses", tibble::tribble(
        ~id, ~name, ~description, ~datetime, ~deleted,
        1, i18n$t("data_scientist"), "", now(), FALSE,
        2, i18n$t("clinician"), "", now(), FALSE,
        3, i18n$t("statistician"), "", now(), FALSE))
    }
    
    # Add default users accesses
    last_row <- get_last_row(r$db, "options")
    
    # Users accesses
    data <- tibble::tibble(
      category = character(), link_id = integer(), name = character(), value = character(),
      value_num = numeric(), creator_id = integer(), datetime = character(), deleted = logical())
    
    # Loop over all toggles, set 1 to value_num for admin, 0 for test
    for(link_id in 1:2){
      for(i in 1:nrow(users_accesses_toggles_options)){
        data <- data %>% dplyr::bind_rows(
          tibble::tribble(
            ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
            "users_accesses", link_id, users_accesses_toggles_options[[i, "name"]], "", ifelse(link_id == 1, 1, 0), 1, now(), FALSE)
        )
        if (users_accesses_toggles_options[[i, "toggles"]] != ""){
          for(toggle in users_accesses_toggles_options[[i, "toggles"]][[1]]){
            
            data <- data %>% dplyr::bind_rows(
              tibble::tribble(
                ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
                "users_accesses", link_id, toggle, "", ifelse(link_id == 1, 1, 0), 1, now(), FALSE)
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
      "projects", "projects_dataset", "projects_data_cleaning",
      "log", "log_user_log",
      "concepts", "concepts_reload_dataset_concepts"
    )
    
    data <- data %>% dplyr::mutate(value_num = dplyr::case_when((link_id == 2 & name %in% test_user_rights) ~ 1, TRUE ~ value_num))
    
    # Add new values to database
    DBI::dbAppendTable(r$db, "options", data)
  }
}
