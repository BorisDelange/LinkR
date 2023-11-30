#' Get singular form of a word
#' 
#' @description Returns the singular form of a word
#' @param word Original word, in its plural form (character)
#' @param language language used for conversion (character)
#' @examples 
#' get_singular(word = "studies", language = "en")
get_singular <- function(word = character(), language = "en"){
  
  # IDs of settings pages are prefixed with "settings_", in that case, remove the prefix
  if (grepl("settings_", word)) word <- substr(word, nchar("settings_") + 1, nchar(word))
  switch(word, 
    "data_sources" = "data_source",
    "datasets" = "dataset",
    "studies" = "study",
    "subsets" = "subset",
    "vocabulary" = "vocabulary",
    "vocabularies" = "vocabulary",
    "patient_lvl_tabs" = "patient_lvl_tab",
    "patient_lvl_parent_tabs" = "patient_lvl_parent_tab",
    "patient_lvl_tabs_groups" = "patient_lvl_tab_group",
    "patient_lvl_widgets" = "patient_lvl_widget",
    "aggregated_tabs" = "aggregated_tab",
    "aggregated_parent_tabs" = "aggregated_parent_tab",
    "aggregated_tabs_groups" = "aggregated_tab_group",
    "aggregated_widgets" = "aggregated_widget",
    "tabs" = "tab",
    "parent_tabs" = "parent_tab",
    "tabs_groups" = "tab_group",
    "widgets" = "widget",
    "tabs_patient_lvl_tabs_groups_options" = "settings_tabs_patient_lvl_tab_group_options",
    "tabs_aggregated_tabs_groups_options" = "settings_tabs_aggregated_tab_group_options",
    "tab_types" = "tab_type",
    "plugins" = "plugin",
    "scripts" = "script",
    "git_repos" = "git_repo",
    "users" = "user",
    "users_accesses" = "user_access",
    "users_statuses" = "user_status") -> result
  result  
}

#' Get plural form of a word
#' 
#' @description Returns the plural form of a word
#' @param word Original word, in its singular form (character)
#' @param language language used for conversion (character)
#' @examples 
#' get_plural(word = "study", language = "en")
get_plural <- function(word = character(), language = "en"){
  # IDs of settings pages are prefixed with "settings_", in that case, remove the prefix
  if (grepl("settings_", word)) word <- substr(word, nchar("settings_") + 1, nchar(word))
  switch(word, 
    "data_source" = "data_sources",
    "data_sources" = "data_sources",
    "dataset" = "datasets",
    "datasets" = "datasets",
    "study" = "studies",
    "studies" = "studies",
    "subset" = "subsets",
    "subsets" = "subsets",
    "vocabulary" = "vocabularies",
    "vocabularies" = "vocabularies",
    "patient_lvl_tab" = "patient_lvl_tabs",
    "patient_lvl_parent_tab" = "patient_lvl_parent_tabs",
    "patient_lvl_tab_group" = "patient_lvl_tabs_groups",
    "patient_lvl_widget" = "patient_lvl_widgets",
    "aggregated_tab" = "aggregated_tabs",
    "aggregated_parent_tab" = "aggregated_parent_tabs",
    "aggregated_tab_group" = "aggregated_tabs_groups",
    "aggregated_widget" = "aggregated_widgets",
    "tab" = "tabs",
    "parent_tab" = "parent_tabs",
    "tab_group" = "tabs_groups",
    "widget" = "widgets",
    "tab_type" = "tab_types",
    "plugin" = "plugins",
    "plugins" = "plugins",
    "script" = "scripts",
    "scripts" = "scripts",
    "git_repos" = "git_repo",
    "user" = "users",
    "users" = "users",
    "user_access" = "users_accesses",
    "users_accesses" = "users_accesses",
    "user_status" = "users_statuses",
    "users_statuses" = "users_statuses") -> result
  result
}
