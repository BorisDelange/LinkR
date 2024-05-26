#' @noRd
create_plugin_files <- function(id, r, plugin_id){
  
  # Get plugin folder
  sql <- glue::glue_sql("SELECT value FROM options WHERE category = 'plugin' AND name = 'unique_id' AND link_id = {plugin_id}", .con = r$db)
  plugin_unique_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
  plugin_folder <- paste0(r$app_folder, "/plugins/", plugin_unique_id)
  
  # Create folder if doesn't exist
  if (!dir.exists(plugin_folder)) dir.create(plugin_folder)
  
  # Retro-compatibility ----
  # Convert UI / server / translations entries from database to files
  # With versions < 0.3, we had only three files : ui.R, server.R and translations.csv
  # The category in code table were different
  
  sql <- glue::glue_sql("SELECT * FROM code WHERE category IN ('plugin_ui', 'plugin_server', 'plugin_translations') AND link_id = {plugin_id}", .con = r$db)
  old_plugin_files <- DBI::dbGetQuery(r$db, sql)
  
  if (nrow(old_plugin_files) > 0){
    for (filename in c("server.R", "ui.R", "translations.csv")){
      file_path <- paste0(plugin_folder, "/", filename)
      if (!file.exists(file_path)){
        
        file_code <- old_plugin_files %>% dplyr::filter(category == paste0("plugin_", sub("\\.[^.]*$", "", filename))) %>% dplyr::pull(code)
        
        # Create file if doesn't exist
        writeLines(file_code, file_path)
      }
    }
  }
  
  # List files in the plugin folder
  files_list <- list.files(path = plugin_folder, pattern = "(?i)*.\\.(r|py|csv|css|js)$")
  
  # Get files from database
  sql <- glue::glue_sql("SELECT * FROM options WHERE category = 'plugin_code' AND link_id = {plugin_id}", .con = r$db)
  plugin_files_db <- DBI::dbGetQuery(r$db, sql)
  db_list <- plugin_files_db$value
  
  # For plugins page, get files_list
  if (id == "plugins") plugins_page_files_list <- r$edit_plugin_code_files_list %>% dplyr::filter(plugin_id == !!plugin_id) %>% dplyr::pull(filename)
  
  # Synchronize files with database ----
  # Add files in database if don't exist
  
  if (length(files_list) > 0){
    for (i in 1:length(files_list)){
      file_name <- files_list[i]
      
      # For plugins page, update code_files_list
      
      if (id == "plugins"){
        if (file_name %not_in% plugins_page_files_list){
          
          # Get option id from db
          options_id <- plugin_files_db %>% dplyr::filter(link_id == plugin_id, value == file_name) %>% dplyr::pull(id)
          if (length(options_id) == 0) options_id <- NA_integer_
          
          r$edit_plugin_code_files_list <-
            r$edit_plugin_code_files_list %>% dplyr::bind_rows(tibble::tibble(id = options_id, plugin_id = !!plugin_id, filename = file_name))
        }
      }
      
      # If db entry doesn't exist, create new entry
      if (file_name %not_in% db_list){
        
        # Options table
        
        options_new_row_id <- as.integer(get_last_row(r$db, "options") + 1)
        new_options <- tibble::tibble(
          id = options_new_row_id, category = "plugin_code", link_id = {plugin_id}, name = "filename",
          value = {file_name}, value_num = NA_real_, creator_id = r$user_id, datetime = now(), deleted = FALSE
        )
        DBI::dbAppendTable(r$db, "options", new_options)
        
        # Code table 
        
        file_code <- readLines(paste0(plugin_folder, "/", file_name), warn = FALSE) %>% toString()
        new_code <- tibble::tibble(
          id = as.integer(get_last_row(r$db, "code") + 1), category = "plugin", link_id = {options_new_row_id}, code = file_code,
          creator_id = r$user_id, datetime = now(), deleted = FALSE
        )
        DBI::dbAppendTable(r$db, "code", new_code)
        
        # For plugins page, update code_files_list
        
        if (id == "plugins") r$edit_plugin_code_files_list <-
          r$edit_plugin_code_files_list %>%
          dplyr::mutate(id = dplyr::case_when(plugin_id == !!plugin_id & filename == file_name ~ options_new_row_id, TRUE ~ id))
      }
    }
  }
  
  # Synchronize database with files ----
  # Create files if don't exist.
  
  if (nrow(plugin_files_db) > 0){
    for (i in 1:nrow(plugin_files_db)){
      file <- plugin_files_db[i, ]
      file_name <- file$value
      
      if (!file.exists(paste0(plugin_folder, "/", file_name))){
        # Get code
        sql <- glue::glue_sql("SELECT * FROM code WHERE category = 'plugin' AND link_id = {file$id}", .con = r$db)
        row <- DBI::dbGetQuery(r$db, sql)
        
        # Create file
        writeLines(row$code, paste0(plugin_folder, "/", file_name))
        
        # For plugins page, update code_files_list
        
        if (id == "plugins") r$edit_plugin_code_files_list <-
          r$edit_plugin_code_files_list %>% dplyr::bind_rows(tibble::tibble(id = file$id, plugin_id = !!plugin_id, filename = file_name))
      }
    }
  }
}