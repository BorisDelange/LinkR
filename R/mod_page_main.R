#' page_main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_page_main_ui <- function(id = character(), language = "en", languages = tibble::tibble(), i18n = character(), users_accesses_toggles_options = tibble::tibble()){
  ns <- NS(id)
  result <- ""
  
  # --- --- --- --
  # Home page ----
  # --- --- --- --
  
  if (grepl("^home", id)){
    sapply(c("home", "home_get_started", "home_tutorials", "home_resources", "home_dev"), function(page){
      if (id == page) mod_home_ui(id = page, i18n = i18n) ->> result
    }) 
  }
  
  # --- --- --- --- --- --- -
  # My studies & subsets ----
  # --- --- --- --- --- --- -
  
  if (id == "my_studies") mod_my_studies_ui(id = "my_studies", i18n = i18n) -> result
  if (id == "my_subsets") mod_my_subsets_ui(id = "my_subsets", i18n = i18n) -> result
  
  # --- --- --- -
  # Messages ----
  # --- --- --- -
  
  if (id == "messages") mod_messages_ui(id = "messages", i18n = i18n) -> result
  
  # --- --- --- --- -
  # Vocabularies ----
  # --- --- --- --- -
  
  if (id == "vocabularies") mod_vocabularies_ui(id = "vocabularies", i18n = i18n) -> result
 
  # --- --- -- -
  # Scripts ----
  # --- --- -- -
  
  if (id == "scripts") mod_scripts_ui(id = "scripts", i18n = i18n, language = language, languages = languages) -> result
 
  # --- --- --- --- --- --- --- --- --
  # Patient-lvl & aggregated data ----
  # --- --- --- --- --- --- --- --- --
  
  if (id == "patient_level_data") mod_data_ui(id = "patient_level_data", i18n = i18n) -> result
  if (id == "aggregated_data") mod_data_ui(id = "aggregated_data", i18n = i18n) -> result

  # --- --- -- -
  # Plugins ----
  # --- --- -- -
  
  if (id %in% c("plugins_patient_lvl", "plugins_aggregated")) mod_plugins_ui(id = id, i18n = i18n, language = language, languages = languages) -> result
  
  # --- --- --- -
  # Settings ----
  # --- --- --- -
  
  if (grepl("^settings", id)){
    
    if (id == "settings_general_settings") mod_settings_general_ui(id = "settings_general_settings", i18n = i18n) -> result
    if (id == "settings_app_db") mod_settings_app_database_ui(id = "settings_app_db", i18n = i18n) -> result
    if (id == "settings_git") mod_settings_git_ui(id = "settings_git", i18n = i18n) -> result
    if (id == "settings_users") mod_settings_users_ui(id = "settings_users", i18n = i18n, users_accesses_toggles_options = users_accesses_toggles_options) -> result
    if (id == "settings_dev") mod_settings_dev_ui(id = "settings_dev", i18n = i18n) -> result
    
    # Subpages of Settings / data management
    sapply(c("data_sources", "datasets", "studies", "subsets", "vocabularies"), function(page_settings){
      if (id == paste0("settings_", page_settings)) mod_settings_data_management_ui(id = paste0("settings_", page_settings), i18n = i18n, language = language, languages = languages) ->> result
    })
    
    if (id == "settings_log") mod_settings_log_ui(id = "settings_log", i18n = i18n) -> result
  }

  result
}
