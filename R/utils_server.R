#' Update r variable
#' 
#' @description Update r reactive variable, requesting the corresponding table in the database
#' @param r A shiny reactiveValue, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param m A shiny reactiveValue, used to communicate between modules in the ShinyApp (reactiveValues object)
#' @param table Database table name (character)
#' @param i18n Translator object from shiny.i18n library
#' @examples
#' \dontrun{
#' update_r(r = r, table = "subsets", i18n = i18n)
#' }
update_r <- function(r = shiny::reactiveValues(), m = shiny::reactiveValues(), table = character(), i18n = character()){
  tables <- c("users", "users_accesses", "users_statuses",
    "data_sources", "datasets", "studies", "subsets", "subset_persons", "subsets_persons", "vocabulary",
    "plugins", "scripts",
    # Update : these tables (with category patient_lvl or aggregated) actually don't exist
    # We have kept this for simplicity (easier to update the code)
    "patient_lvl_tabs_groups", "patient_lvl_tabs", "patient_lvl_widgets", "patient_lvl_widgets_concepts", "aggregated_widgets_concepts",
    "aggregated_tabs_groups", "aggregated_tabs", "aggregated_widgets",
    "code", 
    "options",
    "widgets_options", "patients_options")
  
  if (table %not_in% tables) stop(paste0(i18n$t("invalid_table_name"), ". ", i18n$t("tables_allowed"), " : ", toString(tables)))
  
  if (table %in% c("patients_options", "subsets", "subset_persons", "subsets_persons", 
    "patient_lvl_widgets_concepts", "aggregated_widgets_concepts")){
    db <- m$db
    
    if (table %in% c("patient_lvl_widgets_concepts", "aggregated_widgets_concepts")){
      
      if (grepl("patient_lvl", table)) category <- "patient_lvl" else category <- "aggregated"
      
      if (nrow(r[[paste0(category, "_widgets")]]) > 0) widget_ids <- r[[paste0(category, "_widgets")]] %>% dplyr::pull(id)
      else widget_ids <- 0L
      sql <- glue::glue_sql("SELECT * FROM widgets_concepts WHERE deleted IS FALSE AND widget_id IN ({widget_ids*})", .con = db)
      r[[paste0(category, "_widgets_concepts")]] <- DBI::dbGetQuery(db, sql)
    }
    
    else if (table %in% c("subsets", "subset_persons", "subsets_persons", "patients_options")){
      
      if (table == "subsets_persons"){
        
        if (nrow(m$subsets) > 0) subsets_ids <- m$subsets %>% dplyr::pull(id)
        else subsets_ids <- 0L
        sql <- glue::glue_sql("SELECT * FROM subset_persons WHERE deleted IS FALSE AND subset_id IN ({subsets_ids*})", .con = db)
        m$subsets_persons <- DBI::dbGetQuery(db, sql)
      }
      
      else {

        tables <- tibble::tribble(~name, ~col_name, ~col_value,
          "subsets", "study_id", m$selected_study,
          "subset_persons", "subset_id", m$selected_subset,
          "patients_options", "study_id", m$selected_study)

        row <- tables %>% dplyr::filter(name == table)
        
        sql <- glue::glue_sql("SELECT * FROM {`row$name`} WHERE deleted IS FALSE AND {`row$col_name`} = {row$col_value}", .con = db)
        m[[row$name]] <- DBI::dbGetQuery(db, sql)
      }
    }
    
    else {
      
      m[[table]] <- DBI::dbGetQuery(db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
      m[[paste0(table, "_temp")]] <- m[[table]] %>% dplyr::mutate(modified = FALSE)
    }
  }
  
  else {
    db <- r$db
    
    if (table %in% c("datasets", "plugins", "data_sources")){
      
      r[[table]] <- DBI::dbGetQuery(db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
      
      # Filter to data user has access to
      
      if (paste0(table, "_see_all_data") %not_in% r$user_accesses){
        if (nrow(r[[table]]) > 0){
          r[[table]] <- DBI::dbGetQuery(db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
          r[[table]] <- get_authorized_data(r = r, table = table)
          r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
        }
      }
    }
    
    else if (grepl("tab", table) | grepl("widget", table)){
      
      # if (table == "widgets_options"){
      #   sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE deleted IS FALSE AND study_id = {m$selected_study}", .con = db)
      #   r$widgets_options <- DBI::dbGetQuery(db, sql)
      # }
      
      # else {
      if (grepl("patient_lvl", table)) category <- "patient_lvl" else category <- "aggregated"
      
      if (table == paste0(category, "_tabs_groups")){
        tab_group_id <- r$studies %>% dplyr::filter(id == m$selected_study) %>% dplyr::pull(paste0(category, "_tab_group_id"))
        sql <- glue::glue_sql("SELECT * FROM tabs_groups WHERE deleted IS FALSE AND category = {category} AND id = {tab_group_id}", .con = db)
        r[[paste0(category, "_tabs_groups")]] <- DBI::dbGetQuery(db, sql)
      }
      
      else if (table == paste0(category, "_tabs")){
        tab_group_id <- r$studies %>% dplyr::filter(id == m$selected_study) %>% dplyr::pull(paste0(category, "_tab_group_id"))
        sql <- glue::glue_sql("SELECT * FROM tabs WHERE deleted IS FALSE AND category = {category} AND tab_group_id = {tab_group_id}", .con = db)
        r[[paste0(category, "_tabs")]] <- DBI::dbGetQuery(db, sql)
      }
      
      else if (table == paste0(category, "_widgets")){
        if (nrow(r[[paste0(category, "_tabs")]]) > 0) tabs_ids <- r[[paste0(category, "_tabs")]] %>% dplyr::pull(id)
        else tabs_ids <- 0L
        sql <- glue::glue_sql("SELECT * FROM widgets WHERE deleted IS FALSE AND category = {category} AND tab_id IN ({tabs_ids*})", .con = db)
        r[[paste0(category, "_widgets")]] <- DBI::dbGetQuery(db, sql)
      }
      # }
    }
    
    else if (table == "studies"){
      
      tables <- tibble::tribble(~name, ~col_name, ~col_value,
        "studies", "dataset_id", r$selected_dataset)
      
      row <- tables %>% dplyr::filter(name == table)
      
      sql <- glue::glue_sql("SELECT * FROM {`row$name`} WHERE deleted IS FALSE AND {`row$col_name`} = {row$col_value}", .con = db)
      r[[row$name]] <- DBI::dbGetQuery(db, sql)
      
      # For studies, filter to data user has access to
      if ("studies_see_all_data" %not_in% r$user_accesses){
        if (nrow(r$studies) > 0){
          r$studies <- get_authorized_data(r = r, table = "studies")
          r$studies_temp <- r$studies %>% dplyr::mutate(modified = FALSE)
        }
      }
    }
    
    else {
      
      r[[table]] <- DBI::dbGetQuery(db, paste0("SELECT * FROM ", table, " WHERE deleted IS FALSE ORDER BY id"))
      r[[paste0(table, "_temp")]] <- r[[table]] %>% dplyr::mutate(modified = FALSE)
    }
  }
}

#' Get column names
#' 
#' @description Get the columns names of a table
#' @param table_name Name of the table (character)
#' @param i18n Translator object from shiny.i18n library
#' @examples 
#' get_col_names(table_name = "datasets", i18n = i18n)
get_col_names <- function(table_name = character(), i18n = character()){
  result <- ""
  
  if (table_name %in% c("data_sources", "subsets")){
    result <- c(i18n$t("id"), i18n$t("name"), i18n$t("description"))
    c(result, switch(table_name,
      "datasets" = i18n$t("data_source"),
      "subsets" = i18n$t("study"))) -> result
    result <- c(result, i18n$t("creator"), i18n$t("datetime"),
      i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  }
  
  else if (table_name == "datasets") result <- c(i18n$t("id"), i18n$t("name"), i18n$t("data_source"),
    i18n$t("creator"), i18n$t("created_on"), i18n$t("updated_on"), i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  
  else if (table_name == "vocabulary") result <- c(i18n$t("id"), i18n$t("vocabulary_id_col"), i18n$t("vocabulary_name_col"),
    i18n$t("vocabulary_reference"), i18n$t("vocabulary_version"), i18n$t("vocabulary_concept_id"), i18n$t("data_source"),
    i18n$t("display_order"), i18n$t("creator"), i18n$t("created_on"), i18n$t("updated_on"), i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  
  else if (table_name == "studies"){
    result <- c(i18n$t("id"), i18n$t("name"), i18n$t("dataset"), i18n$t("patient_lvl_tab_group"),
      i18n$t("aggregated_tab_group"), i18n$t("creator"), i18n$t("created_on"), i18n$t("updated_on"),
      i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  }
  
  else if (table_name == "studies_no_data"){
    result <- c(i18n$t("name"), i18n$t("creator"), i18n$t("datetime"), i18n$t("action"))
  }
  
  else if (table_name == "study_conversations"){
    result <- c(i18n$t("conversation_id"), i18n$t("object"), i18n$t("datetime"), 
      i18n$t("unread_messages"), i18n$t("action"), i18n$t("modified"))
  }
  
  else if (table_name == "subsets"){
    result <- c(i18n$t("id"), i18n$t("name"), i18n$t("description"),
      i18n$t("study"), i18n$t("creator"), i18n$t("datetime"),
      i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  }
  
  else if (table_name == "subset_persons"){
    result <- c(i18n$t("id"), i18n$t("subset"), i18n$t("patient"),
      i18n$t("creator"), i18n$t("datetime"), i18n$t("deleted"), i18n$t("modified"))
  }
  
  else if (table_name == "subset_add_patients"){
    result <- c(i18n$t("patient"))
  }
  
  else if (table_name == "dataset_vocabulary_concepts_with_counts"){
    result <- c(i18n$t("id"), i18n$t("vocabulary_id_1"), i18n$t("concept_id_1"), i18n$t("concept_name_1"), i18n$t("concept_display_name_1"),
      i18n$t("relationship_id"), i18n$t("vocabulary_id_2"), i18n$t("concept_id_2"), i18n$t("concept_name_2"),
      i18n$t("domain_id"), i18n$t("concept_class_id"), i18n$t("standard_concept"), i18n$t("concept_code"), 
      i18n$t("valid_start_date"), i18n$t("valid_end_date"), i18n$t("invalid_reason"),
      i18n$t("num_patients"), i18n$t("num_rows"), i18n$t("modified"))
  }
  
  else if (table_name == "plugins_vocabulary_concepts_with_counts"){
    result <- c(i18n$t("concept_id"), i18n$t("concept_name"), i18n$t("concept_display_name"),
      i18n$t("domain_id"), i18n$t("concept_class_id"), i18n$t("standard_concept"), i18n$t("concept_code"),
      i18n$t("num_patients"), i18n$t("num_rows"), i18n$t("action"))
  }
  
  else if (table_name == "plugins_vocabulary_mapped_concepts_with_counts"){
    result <- c(i18n$t("id"), i18n$t("concept_id"), i18n$t("relationship_id"), i18n$t("mapped_concept_id"),
      i18n$t("mapped_concept_name"), i18n$t("mapped_concept_display_name"), i18n$t("domain_id"),
      i18n$t("num_patients"), i18n$t("num_rows"), i18n$t("action"))
  }
  
  else if (table_name == "mapping_vocabulary_concepts"){
    result <- c(i18n$t("id"), i18n$t("concept_id"), i18n$t("concept_name"), i18n$t("domain_id"), 
      i18n$t("vocabulary_id"), i18n$t("concept_class_id"), i18n$t("standard_concept"), i18n$t("concept_code"),
      i18n$t("valid_start_date"), i18n$t("valid_end_date"), i18n$t("invalid_reason"))
  }
  
  else if (table_name == "mapping_vocabulary_concepts_with_counts"){
    result <- c(i18n$t("id"), i18n$t("concept_id"), i18n$t("concept_name"), i18n$t("domain_id"), 
      i18n$t("vocabulary_id"), i18n$t("concept_class_id"), i18n$t("standard_concept"), i18n$t("concept_code"),
      i18n$t("valid_start_date"), i18n$t("valid_end_date"), i18n$t("invalid_reason"), i18n$t("num_rows"))
  }
  
  else if (table_name == "mapping_vocabulary_concepts_with_counts_and_datasets"){
    result <- c(i18n$t("id"), i18n$t("concept_id"), i18n$t("concept_name"), i18n$t("domain_id"), 
      i18n$t("vocabulary_id"), i18n$t("concept_class_id"), i18n$t("standard_concept"), i18n$t("concept_code"),
      i18n$t("valid_start_date"), i18n$t("valid_end_date"), i18n$t("invalid_reason"), i18n$t("num_datasets"), i18n$t("num_rows"))
  }
  
  else if (table_name == "dataset_vocabulary_concepts_mapping"){
    result <- c(i18n$t("id"), i18n$t("vocabulary_id_1"), i18n$t("concept_id_1"), i18n$t("relationship_id"), 
      i18n$t("vocabulary_id_2"), i18n$t("concept_id_2"))
  }
  
  else if (table_name == "dataset_vocabulary_concepts_mapping_evals"){
    result <- c(i18n$t("id"), i18n$t("vocabulary_id_1"), i18n$t("concept_id_1"), i18n$t("concept_name_1"), 
      i18n$t("relationship_id"), i18n$t("vocabulary_id_2"), i18n$t("concept_id_2"), i18n$t("concept_name_2"),
      i18n$t("comment"), i18n$t("creator"), i18n$t("datetime"), i18n$t("positive_evals_short"), i18n$t("negative_evals_short"), 
      i18n$t("action"), i18n$t("user_evaluation_id"), i18n$t("modified"))
  }
  
  else if (table_name == "plugins"){
    result <- c(i18n$t("id"), i18n$t("name"), i18n$t("tab_type"), 
      i18n$t("created_on"), i18n$t("updated_on"), i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  }
  
  else if (table_name == "users"){
    result <- c(i18n$t("id"), i18n$t("username"), i18n$t("firstname"), i18n$t("lastname"),
      i18n$t("password"), i18n$t("user_access"), i18n$t("user_status"), i18n$t("datetime"), 
      i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  }
  
  else if (table_name %in% c("users_accesses", "users_statuses")){
    result <- c(i18n$t("id"), i18n$t("name"), i18n$t("description"), 
      i18n$t("datetime"), i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  }
  
  else if (table_name %in% c("patient_lvl_tabs", "aggregated_tabs")){
    result <- c(i18n$t("id"), i18n$t("name"), i18n$t("description"), i18n$t("tab_group"),
      i18n$t("parent_tab"), i18n$t("display_order"), i18n$t("creator"), i18n$t("datetime"), 
      i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  }
  
  else if (table_name %in% c("patient_lvl_tabs_groups", "aggregated_tabs_groups")){
    result <- c(i18n$t("id"), i18n$t("name"), i18n$t("description"),
      i18n$t("creator"), i18n$t("datetime"), 
      i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  }
  
  else if (table_name == "log"){
    result <- c(i18n$t("id"), i18n$t("category"), i18n$t("name"), i18n$t("value"), i18n$t("user"), i18n$t("datetime"))
  }
  
  else if (table_name == "scripts"){
    result <- c(i18n$t("id"), i18n$t("name"), i18n$t("created_on"), i18n$t("updated_on"), i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  }
  
  else if (table_name == "local_scripts"){
    result <- c(i18n$t("id"), i18n$t("name"), i18n$t("unique_id"), i18n$t("description"), i18n$t("category"), i18n$t("author"), i18n$t("version"),
      i18n$t("created_on"), i18n$t("updated_on"))
  }
  
  else if (table_name == "remote_git_scripts"){
    result <- c(i18n$t("name"), i18n$t("unique_id"), i18n$t("description"), i18n$t("category"), i18n$t("author"), i18n$t("version"),
      i18n$t("images"), i18n$t("created_on"), i18n$t("updated_on"), i18n$t("action"))
  }
  
  else if (table_name == "local_studies"){
    result <- c(i18n$t("id"), i18n$t("name"), i18n$t("unique_id"), i18n$t("description"), i18n$t("category"), i18n$t("author"), i18n$t("version"),
      i18n$t("created_on"), i18n$t("updated_on"))
  }
  
  else if (table_name == "remote_git_studies"){
    result <- c(i18n$t("name"), i18n$t("unique_id"), i18n$t("description"), i18n$t("category"), i18n$t("author"), i18n$t("version"),
      i18n$t("images"), i18n$t("created_on"), i18n$t("updated_on"), i18n$t("action"))
  }
  
  else if (table_name == "local_datasets_or_vocabs"){
    result <- c(i18n$t("id"), i18n$t("name"), i18n$t("unique_id"), i18n$t("description"), i18n$t("category"), i18n$t("author"), i18n$t("version"),
      i18n$t("created_on"), i18n$t("updated_on"))
  }
  
  else if (table_name == "remote_git_datasets_or_vocab"){
    result <- c(i18n$t("name"), i18n$t("unique_id"), i18n$t("description"), i18n$t("category"), i18n$t("author"), i18n$t("version"),
      i18n$t("images"), i18n$t("created_on"), i18n$t("updated_on"), i18n$t("action"))
  }
  
  else if (table_name == "perf_monitoring"){
    result <- c(i18n$t("elapsed_time"), i18n$t("task"), i18n$t("datetime_start"), i18n$t("datetime_stop"))
  }
  
  else if (table_name == "git_repos"){
    result <- c(i18n$t("id"), i18n$t("unique_id"), i18n$t("name"), i18n$t("api_key"), i18n$t("repo_url_address"), i18n$t("raw_files_url_address"),
      i18n$t("creator"), i18n$t("datetime"), i18n$t("deleted"), i18n$t("modified"), i18n$t("action"))
  }
  
  else if (table_name == "edit_git_repo_category_elements"){
    result <- c(i18n$t("id"), i18n$t("unique_id"), i18n$t("name"), i18n$t("category"), i18n$t("version"), 
      i18n$t("created_on"), i18n$t("updated_on"), i18n$t("action"))
  }
  
  result
}