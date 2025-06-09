#' @noRd
insert_default_users <- function(){
  
  users_accesses_toggles_options <- get_users_accesses_toggles_options()
  db_col_types <- get_app_db_col_types()
  
  # Get variables from other environments
  for (obj_name in c("id", "r", "m", "output")) assign(obj_name, get(obj_name, envir = parent.frame()))
  i18n <- r$i18n
  language <- r$language
  
  # Add default users ----
  
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

#' @noRd
insert_default_content <- function(){
  
  # Get variables from other environments
  for (obj_name in c("id", "r", "m", "output", "user_accesses")) assign(obj_name, get(obj_name, envir = parent.frame()))
  i18n <- r$i18n
  language <- r$language
  
  if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM studies")) == 0) {
    
    project_path <- system.file("extdata/projects/linkr_demo", package = "linkr")
    
    xml_file <- file.path(project_path, "project.xml")
    element <- xml2::read_xml(xml_file) %>%
      XML::xmlParse() %>%
      XML::xmlToDataFrame(nodes = XML::getNodeSet(., "//project"), stringsAsFactors = FALSE) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(authors = stringr::str_replace_all(authors, "(?<=\\p{L})(?=\\p{Lu})", "; "))
    
    import_element(
      con = r$db, sql_table = "studies", sql_category = "study", single_id = "project",
      element = element, element_type = "projects", temp_dir = project_path
    )
    
    if (nrow(DBI::dbGetQuery(r$db, "SELECT * FROM datasets")) == 0) {
      
      dataset_path <- system.file("extdata/datasets/mimiciv_demo_set", package = "linkr")
      
      xml_file <- file.path(dataset_path, "dataset.xml")
      element <- xml2::read_xml(xml_file) %>%
        XML::xmlParse() %>%
        XML::xmlToDataFrame(nodes = XML::getNodeSet(., "//dataset"), stringsAsFactors = FALSE) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(authors = stringr::str_replace_all(authors, "(?<=\\p{L})(?=\\p{Lu})", "; "))
      
      import_element(
        con = r$db, sql_table = "datasets", sql_category = "dataset", single_id = "dataset",
        element = element, element_type = "datasets", temp_dir = dataset_path
      )
      
      dataset_files_path <- system.file("extdata/datasets_files/mimiciv_demo_set", package = "linkr")
      dataset_files_target_path <- file.path(r$app_folder, "datasets_files/1")
      if (!dir.exists(dataset_files_target_path)) dir.create(dataset_files_target_path, recursive = TRUE)
      files_to_copy <- list.files(dataset_files_path, full.names = TRUE)
      file.copy(files_to_copy, dataset_files_target_path, overwrite = TRUE)
      
      # Link demo project to demo data
      sql <- glue::glue_sql("UPDATE studies SET dataset_id = 1 WHERE id = 1", .con = r$db)
      DBI::dbExecute(r$db, sql)
    }
  }
}
