#' Run the Shiny application
#'
#' @description 
#' Runs the LinkR Shiny Application.\cr
#' Use language argument to choose language to use ("en" or "fr" available).\cr
#' Use app_folder argument to choose a folder where the files of the application will be saved. By default, 
#' a 'linkr' folder will be created in the folder returned by path.expand("~").\cr
#' @param language Default language to use in the App (character)
#' @param app_folder Location of the application folder (character).
#' @param authentication Requires an authentication to access the app? (logical)
#' @param username Username used for connection. Default username is "admin". (character)
#' @param local Run the app in local mode, do not load files on the internet (logical)
#' @param debug Debug mode : steps and errors will by displayed in the console (logical)
#' @param log_file Create a log file to see app log directly in the app (logical)
#' @param port Port used by shiny app (integer)
#' @param loading_options List specifying the initial page, project, and subset to load at startup (a list with `page`, `project`, and `subset` keys)
#' @examples 
#' \dontrun{
#' linkr(language = "en", app_folder = "my_app_folder/")
#' }
#' @export
#' @importFrom shiny shinyApp
#' @importFrom magrittr %>%

linkr <- function(
  language = "en",
  app_folder = character(),
  authentication = FALSE,
  username = "admin",
  local = FALSE,
  debug = FALSE,
  log_file = FALSE,
  port = 3838,
  loading_options = list()
) {
  
  # Loading options
  # - loading_options$page: load a specific page
  # - loading_options$project_id: load a project
  # - loading_options$load_data_page: load a data page ("patient_lvl" or "aggregated")
  # - loading_options$subset_id: load a subset_id the first time a project is loaded
  # - loading_options$person_id: load a person_id the first time a subset is loaded
  
  # Check version of shiny.fluent (has to be inferior to 0.4.0)
  if (packageVersion("shiny.fluent") >= "0.4.0") stop("Invalid shiny.fluent version: version 0.3.0 is required. Install it with remotes::install_github('Appsilon/shiny.fluent', ref = 'dd1c956').")
  
  # Set umask 002 for files creation
  Sys.umask("002")
  
  # Maximum size for uploaded data (4096 MB)
  # Used to restore database and import vocabularies
  # shiny.launch.browser to automatically open browser
  
  if (debug) cat(paste0("[", now(), "] [INFO] - [page_id = linkr] init LinkR - v0.3.1.9008"))
  options(shiny.maxRequestSize = 4096*1024^2, shiny.launch.browser = TRUE, shiny.port = port)
  
  if (!is.logical(debug)) stop("'debug' argument is not of logical type")
  if (!is.logical(local)) stop("'local' argument is not of logical type")
  if (!is.logical(authentication)) stop("'authentication' argument is not of logical type")
  
  if (!authentication){
    error_username <- TRUE
    if (length(username) > 0) if (!is.na(username) & username != "") error_username <- FALSE
    if (error_username) stop("'username' argument is invalid")
  }
  
  # Create app folder if it doesn't exist
  if (debug) cat(paste0("\n[", now(), "] [INFO] - [page_id = linkr] create app_folder"))
  if (length(app_folder) == 0) app_folder <- paste0(path.expand("~"), "/linkr")
  
  if (!dir.exists(app_folder)){
    tryCatch(
      dir.create(app_folder),
      error = function(e) print(e)
    )
  }
  
  # Create app sub-dirs
  if (debug) cat(paste0("\n[", now(), "] [INFO] - [page_id = linkr] create app sub-dirs"))
  sub_dirs <- c(
    "app_database",
    "data_cleaning",
    "datasets",
    "datasets_files",
    "log",
    "messages",
    "plugins",
    "projects",
    "projects_files",
    "subsets",
    "temp_files",
    "translations"
  )
  for (sub_dir in sub_dirs) if (!dir.exists(paste0(app_folder, "/", sub_dir))) dir.create(paste0(app_folder, "/", sub_dir))
  
  # Load translations
  if (debug) cat(paste0("\n[", now(), "] [INFO] - [page_id = linkr] load translations"))
  
  languages <- c("en", "fr")
  if (language %not_in% languages){
    cat("\n")
    stop(paste0(paste("Language is not valid. Valid languages are", paste(languages, collapse = ", ")), "."))
  }
  
  translations_path <- "inst/translations"
  if (!dir.exists(translations_path)) translations_path <- paste0(find.package("linkr"), "/translations")
  if (!dir.exists(translations_path)) print("Translations path not found")
  
  i18n <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = translations_path))
  i18n$set_translation_language(language)
  
  languages <- tibble::tribble(
    ~code, ~language, 
    "en", i18n$t("english"),
    "fr", i18n$t("french")#,
    # "it", i18n$t("italian"),
    # "es", i18n$t("spanish")
  )
  
  options(digits.secs = 0)
  
  # Toggles for users accesses
  
  users_accesses_toggles_options <- tibble::tribble(
    ~name, ~toggles,
    "users", c(
      "users_management",
      "users_statuses_management",
      "users_accesses_management"
    ),
    "projects", c(
      "projects_see_all_data",
      "projects_management",
      "projects_import",
      "projects_content_management",
      'projects_widgets_settings',
      "projects_scripts",
      "projects_widgets_console",
      "projects_dataset",
      "projects_data_cleaning",
      "projects_subsets_management",
      "projects_share"
    ),
    "plugins", c(
      "plugins_see_all_data",
      "plugins_management",
      "plugins_import",
      "plugins_edit_code",
      "plugins_share"
    ),
    "datasets", c(
      "datasets_see_all_data",
      "datasets_management",
      "datasets_import",
      "datasets_edit_code",
      "datasets_share"
    ),
    "vocabularies", c(
      "vocabularies_management",
      "vocabularies_import",
      "concepts_reload_dataset_concepts"
    ),
    "data_cleaning", c(
      "data_cleaning_see_all_data",
      "data_cleaning_management",
      "data_cleaning_import",
      "data_cleaning_edit_code",
      "data_cleaning_share"
    ),
    "console", c(
      "console_execute_code"
    ),
    "git_repos", c(
      "git_repos_management",
      "git_repos_remote_git_repo_management",
      "git_repos_install_remote_git_element"
    ),
    "app_db", c(
      "app_db_connection_settings",
      "app_db_db_request",
      "app_db_save_restore"
    ),
    "log", c(
      "log_user_log"
    )
  )
  
  pages <- c(
    "/",
    "app_db",
    "concepts",
    "console",
    "data",
    "datasets",
    "data_cleaning",
    "git_repos",
    "log",
    "login",
    "project_files",
    "plugins",
    "projects",
    "subsets",
    "tasks",
    "user_settings",
    "users",
    "vocabularies"
  )
  
  # Col types of database tables, to import and restore database
  
  db_col_types <- tibble::tribble(
    ~table, ~col_types, ~db, ~col_names,
    
    # Main tables
    
    "users", "icccciicl", "main", c("id", "username", "firstname", "lastname", "password", "user_access_id", "user_status_id", "datetime", "deleted"),
    "users_accesses", "icccl", "main", c("id", "name", "description", "datetime", "deleted"),
    "users_statuses", "icccl", "main", c("id", "name", "description", "datetime", "deleted"),
    "datasets", "iciiccl", "main", c("id", "name", "data_source_id", "creator_id", "creation_datetime", "update_datetime", "deleted"),
    "studies", "iciiiiccl", "main", c("id", "name", "dataset_id", "patient_lvl_tab_group_id", "aggregated_tab_group_id", "creator_id", "creation_datetime", "update_datetime", "deleted"),
    "plugins", "iciccl", "main", c("id", "name", "tab_type_id", "creation_datetime", "update_datetime", "deleted"),
    "scripts", "icccl", "main", c("id", "name", "creation_datetime", "update_datetime", "deleted"),
    "tabs_groups", "icccicl", "main", c("id", "category", "name", "description", "creator_id", "datetime", "deleted"),
    "tabs", "iccciiiicl", "main", c("id", "category", "name", "description", "tab_group_id", "parent_tab_id", "display_order", "creator_id", "datetime", "deleted"),
    "widgets", "icciiiicl", "main", c("id", "category", "name", "tab_id", "plugin_id", "display_order", "creator_id", "datetime", "deleted"),
    "code", "icicicl", "main", c("id", "category", "link_id", "code", "creator_id", "datetime", "deleted"),
    "options", "iciccnicl", "main", c("id", "category", "link_id", "name", "value", "value_num", "creator_id", "datetime", "deleted"),
    "messages", "iiicccicl", "main", c("id", "conversation_id", "study_id", "category", "message", "filepath", "creator_id", "datetime", "deleted"),
    "conversations", "iccl", "main", c("id", "name", "datetime", "deleted"),
    "user_deleted_conversations", "iiic", "main", c("id", "conversation_id", "user_id", "datetime"),
    "inbox_messages", "iiilcl", "main", c("id", "message_id", "receiver_id", "read", "datetime", "deleted"),
    "log", "icccic", "main", c("id", "category", "name", "value", "creator_id", "datetime"),
    "git_repos", "icccccicl", "main", c("id", "unique_id", "name", "api_key", "repo_url_address", "raw_files_url_address", "creator_id", "datetime", "deleted"),
    
    # Public tables
    
    "persons_options", "iiiiiiciccnicl", "public", c("id", "dataset_id", "study_id", "subset_id", "person_id", "visit_detail_id", "category", "link_id", "name", "value", "value_num", "creator_id", "datetime", "deleted"),
    "widgets_options", "iiiicccnicl", "public", c("id", "widget_id", "person_id", "link_id", "category", "name", "value", "value_num", "creator_id", "datetime", "deleted"),
    "subsets", "icciicl", "public", c("id", "name", "description", "study_id", "creator_id", "datetime", "deleted"),
    "options", "iciccnicl", "public", c("id", "category", "link_id", "name", "value", "value_num", "creator_id", "datetime", "deleted"),
    "code", "icicicl", "public", c("id", "category", "link_id", "code", "creator_id", "datetime", "deleted"),
    "subset_persons", "iiiiiicl", "public", c("id", "subset_id", "person_id", "visit_occurrence_id", "visit_detail_id", "creator_id", "datetime", "deleted"),
    "concept", "iiccccccccc", "public", c("id", "concept_id", "concept_name", "domain_id", "vocabulary_id", "concept_class_id", "standard_concept", "concept_code", "valid_start_date", "valid_end_date", "invalid_reason"),
    "concept_dataset", "iiciiii", "public", c("id", "concept_id", "vocabulary_id", "dataset_id", "count_persons_rows", "count_concepts_rows", "count_secondary_concepts_rows"),
    "concept_user", "iiiccc", "public", c("id", "user_id", "concept_id", "concept_name", "concept_display_name", "vocabulary_id"),
    "vocabulary", "icccciciiccl", "public", c("id", "vocabulary_id", "vocabulary_name", "vocabulary_reference", "vocabulary_version", "vocabulary_concept_id", "data_source_id", "display_order", "creator_id", "creation_datetime", "update_datetime", "deleted"),
    "domain", "icci", "public", c("id", "domain_id", "domain_name", "domain_concept_id"),
    "concept_class", "icci", "public", c("id", "concept_class_id", "concept_class_name", "concept_class_concept_id"),
    "concept_relationship", "iiicccc", "public", c("id", "concept_id_1", "concept_id_2", "relationship_id", "valid_start_date", "valid_end_date", "invalid_reason"),
    "concept_relationship_user", "iicic", "public", c("id", "concept_relationship_id", "comment", "creator_id", "datetime"),
    "concept_relationship_evals", "iiicc", "public", c("id", "concept_relationship_id", "creator_id", "evaluation_id", "datetime"),
    "relationship", "iccccci", "public", c("id", "relationship_id", "relationship_name", "is_hierarchical", "defines_ancestry", "reverse_relationship_id", "relationship_concept_id"),
    "concept_synonym", "iici", "public", c("id", "concept_id", "concept_synonym_name", "language_concept_id"),
    "concept_ancestor", "iiiii", "public", c("id", "ancestor_concept_id", "descendant_concept_id", "min_levels_of_separation", "max_levels_of_separation"),
    "drug_strength", "iiinininiiccc", "public", c("id", "drug_concept_id", "ingredient_concept_id", "amount_value", "amount_unit_concept_id", "numerator_value", "numerator_unit_concept_id", "denominator_value", "denominator_unit_concept_id", "box_size", "valid_start_date", "valid_end_date", "invalid_reason"),
    "widgets_concepts", "iiicccilicl", "public", c("id", "widget_id", "concept_id", "concept_name", "concept_display_name", "domain_id", "mapped_to_concept_id", "merge_mapped_concepts", "creator_id", "datetime", "deleted")
  )
  
  # Dropdowns options
  
  dropdowns <- list()
  
  dropdowns$concept <- tibble::tribble(
    ~key, ~text,
    0, "concept_id",
    1, "concept_name",
    2, "domain_id",
    3, "concept_class_id",
    4, "standard_concept",
    5, "concept_code",
    6, "valid_start_date",
    7, "valid_end_date",
    8, "invalid_reason"
  )
  
  dropdowns$concept_with_counts <- tibble::tribble(
    ~key, ~text,
    0, "concept_id",
    1, "concept_name",
    2, "concept_display_name",
    3, "domain_id",
    4, "vocabulary_id",
    5, "concept_class_id",
    6, "standard_concept",
    7, "concept_code",
    8, "valid_start_date",
    9, "valid_end_date",
    10, "invalid_reason",
    11, "count_persons_rows",
    12, "count_concepts_rows"
  )
  
  # Ace editor auto completion
  
  auto_complete_list <- list(
    dplyr = getNamespaceExports("dplyr"),
    DBI = getNamespaceExports("DBI"),
    ggplot2 = getNamespaceExports("ggplot2"),
    glue = getNamespaceExports("glue"),
    plotly = getNamespaceExports("plotly"),
    readr = getNamespaceExports("readr"),
    tidyr = getNamespaceExports("tidyr"),
    vroom = getNamespaceExports("vroom"),
    `OMOP data` = paste0("d$", c(
      "person", "visit_occurrence", "visit_detail", "death",
      "measurement", "observation", "procedure_occurrence", "condition_occurrence", "drug_exposure", "device_exposure",
      "note", "note_nlp", "specimen", "location", "care_site", "provider", "drug_era", "dose_era",
      "condition_era", "payer_plan_period", "cost", "observation_period",
      "data_subset",
      paste0("data_subset$", c(
        "person", "visit_occurrence", "visit_detail", "death",
        "measurement", "observation", "procedure_occurrence", "condition_occurrence", "drug_exposure", "device_exposure",
        "note", "note_nlp", "specimen", "drug_era", "dose_era",
        "condition_era", "payer_plan_period", "cost", "observation_period"
      )),
      "data_person",
      paste0("data_person$", c(
        "death", "measurement", "observation", "procedure_occurrence", "condition_occurrence", "drug_exposure", "device_exposure",
        "note", "note_nlp", "specimen", "drug_era", "dose_era",
        "condition_era", "payer_plan_period", "cost"
      )),
      "data_visit_detail",
      paste0("data_visit_detail$", c(
        "measurement", "observation", "procedure_occurrence", "condition_occurrence", "drug_exposure", "device_exposure",
        "note", "note_nlp", "payer_plan_period", "cost"
      ))
    ))
  )
  
  # Load UI & server
  
  if (debug) cat(paste0("\n[", now(), "] [INFO] - [page_id = linkr] load UI & server"))
  shinyApp(
    ui = app_ui(pages, language, languages, i18n, users_accesses_toggles_options, db_col_types, dropdowns, auto_complete_list, debug),
    server = app_server(pages, language, languages, i18n, app_folder, authentication, username, debug, log_file, local, users_accesses_toggles_options, db_col_types, dropdowns, auto_complete_list, loading_options),
    options = options
  )
}
