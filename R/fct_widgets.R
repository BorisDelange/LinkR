#' @noRd
create_authors_ui <- function(authors){
  
  personas <- list()
  authors <- authors %>% strsplit(split = "[,;]") %>% unlist() %>% trimws()
  
  if (length(authors) > 0) for (author in authors) personas <- rlist::list.append(personas, list(personaName = author))
  
  div(
    shiny.fluent::Facepile(personas = personas),
    style = "height: 35px; display: flex; align-items: center;"
  )
}


#' @noRd
create_element_files <- function(sql_category, con, single_id, element_id, element_wide){
  
  # Get variables from other environments
  for (obj_name in c("id", "r", "m")) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  # Element unique_id
  sql <- glue::glue_sql("SELECT value FROM options WHERE category = {sql_category} AND name = 'unique_id' AND link_id = {element_id}", .con = con)
  element_unique_id <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
  
  # Element dir
  element_dir <- paste0(r$app_folder, "/", id, "/", element_unique_id)
  
  # Get element options
  sql <- glue::glue_sql("SELECT * FROM options WHERE category = {sql_category} AND link_id = {element_id}", .con = con)
  element_options <- DBI::dbGetQuery(r$db, sql)
  
  # Element name
  element_name <- element_options %>% dplyr::filter(name == "name_en") %>% dplyr::pull(value) %>% remove_special_chars()
  
  # Create a temp dir
  
  temp_zip_dir <- file.path(r$app_folder, "temp_files", element_unique_id)
  if (dir.exists(temp_zip_dir)) unlink(temp_zip_dir, recursive = TRUE)
  dir.create(temp_zip_dir)
  
  temp_element_dir <- file.path(temp_zip_dir, element_name)
  dir.create(temp_element_dir)
  
  # Copy only specific types of files (excluding data or other irrelevant files)
  
  extensions <- c("py", "r", "jpg", "jpeg", "png", "svg", "md")
  regex <- paste0("\\.(", paste(extensions, collapse = "|"), ")$|^translations\\.csv$", collapse = "")
  
  files_list <- list.files(element_dir, full.names = TRUE, ignore.case = TRUE, pattern = regex)
  file.copy(files_list, temp_element_dir, overwrite = TRUE)
  
  # Create XML and README files
  create_element_xml(id = id, element_id = element_id, single_id = single_id, element_options = element_options, element_dir = temp_element_dir)
  description <- element_options %>% dplyr::filter(name == "description_en") %>% dplyr::pull(value)
  create_element_readme(description = description, element_dir = temp_element_dir)
  
  if (id == "projects"){
  
    # Export CSV files
    
    # Tables :
    # - tabs_groups
    # - tabs
    # - widgets
    # - widgets_concepts
    # - widgets_options
    # - plugins
    # - subsets
    # - options
    
    corresponding_ids <- list()
    data <- list()
    
    # Get data
    
    ### tabs_groups
    sql <- glue::glue_sql("SELECT * FROM tabs_groups WHERE id IN ({c(element_wide$patient_lvl_tab_group_id, element_wide$aggregated_tab_group_id)*})", .con = r$db)
    data$tabs_groups <- DBI::dbGetQuery(r$db, sql) %>% dplyr::mutate(creator_id = NA_integer_)
    corresponding_ids$tabs_groups <- tibble::tibble(old_id = as.integer(data$tabs_groups$id), new_id = c(1L, 2L))
    
    ### tabs
    sql <- glue::glue_sql("SELECT * FROM tabs WHERE tab_group_id IN ({c(element_wide$patient_lvl_tab_group_id, element_wide$aggregated_tab_group_id)*})", .con = r$db)
    data$tabs <- DBI::dbGetQuery(r$db, sql) %>% dplyr::mutate(creator_id = NA_integer_)
    if (nrow(data$tabs) > 0) corresponding_ids$tabs <- data$tabs %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = 1:dplyr::n())
    
    ### widgets
    sql <- glue::glue_sql("SELECT * FROM widgets WHERE tab_id IN ({data$tabs$id*}) AND deleted IS FALSE", .con = r$db)
    data$widgets <- DBI::dbGetQuery(r$db, sql) %>% dplyr::mutate(creator_id = NA_integer_)
    if (nrow(data$widgets) > 0) corresponding_ids$widgets <- data$widgets %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = 1:dplyr::n())
    
    ### widgets_concepts
    sql <- glue::glue_sql("SELECT * FROM widgets_concepts WHERE widget_id IN ({data$widgets$id*})", .con = m$db)
    data$widgets_concepts <- DBI::dbGetQuery(m$db, sql) %>% dplyr::mutate(creator_id = NA_integer_)
    if (nrow(data$widgets_concept) > 0) corresponding_ids$widgets_concepts <- data$widgets_concepts %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = 1:dplyr::n())
    
    ### widgets_options
    sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id IN ({data$widgets$id*})", .con = m$db)
    data$widgets_options <- DBI::dbGetQuery(m$db, sql) %>% dplyr::mutate(creator_id = NA_integer_, person_id = NA_integer_)
    if (nrow(data$widgets_options) > 0) corresponding_ids$widgets_options <- data$widgets_options %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = 1:dplyr::n())
    
    ### plugins
    sql <- glue::glue_sql("SELECT * FROM plugins WHERE id IN ({data$widgets %>% dplyr::distinct(plugin_id) %>% dplyr::pull()*}) AND deleted IS FALSE", .con = r$db)
    data$plugins <- DBI::dbGetQuery(r$db, sql)
    if (nrow(data$plugins) > 0) corresponding_ids$plugins <- data$plugins %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = 1:dplyr::n())
    
    ### subsets
    sql <- glue::glue_sql("SELECT * FROM subsets WHERE study_id = {element_id} AND deleted IS FALSE", .con = m$db)
    data$subsets <- DBI::dbGetQuery(m$db, sql)
    if (nrow(data$subsets) > 0) corresponding_ids$subsets <- data$subsets %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = 1:dplyr::n())
    
    ### options (for plugins and subsets)
    sql <- glue::glue_sql("SELECT * FROM options WHERE category IN ('plugin') AND link_id IN ({data$plugins %>% dplyr::distinct(id) %>% dplyr::pull()*})", .con = r$db)
    data$plugins_options <- DBI::dbGetQuery(r$db, sql)
    if (nrow(data$plugins_options) > 0) corresponding_ids$plugins_options <- data$plugins_options %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = 1:dplyr::n())
    
    sql <- glue::glue_sql("SELECT * FROM options WHERE category IN ('subset') AND link_id IN ({data$subsets %>% dplyr::distinct(id) %>% dplyr::pull()*})", .con = m$db)
    data$subsets_options <- DBI::dbGetQuery(m$db, sql)
    
    # Copy plugins and subsets files (before chaging IDs)
    
    for (element_type in c("plugins", "subsets")) {
      
      # Get singular form
      element_single <- substr(element_type, 1, nchar(element_type)-1)
      
      element_dir <- paste0(temp_element_dir, "/", element_type)
      dir.create(element_dir)
      
      if (nrow(data[[element_type]]) > 0){
        for (i in 1:nrow(data[[element_type]])){
          element <- data[[element_type]][i, ]
          element_type_unique_id <- data[[paste0(element_type, "_options")]] %>% dplyr::filter(category == element_single & name == "unique_id" & link_id == element$id) %>% dplyr::pull(value)
          
          element_name <- r[[paste0(element_type, "_long")]] %>% dplyr::filter(id == element$id, name == "name_en") %>% dplyr::pull(value) %>% remove_special_chars()
          
          type_dir <- paste0(r$app_folder, "/", element_type, "/", element_type_unique_id)
          project_type_dir <- paste0(element_dir, "/", element_name)
          list_of_files <- list.files(type_dir, ignore.case = TRUE, pattern = regex)
          
          dir.create(project_type_dir)
          file.copy(paste0(type_dir, "/", list_of_files), paste0(project_type_dir, "/", list_of_files))
          
          # Create XML and README files
          if (element_type == "plugins") con <- r$db else con <- m$db
          sql <- glue::glue_sql("SELECT * FROM options WHERE category = {element_single} AND link_id = {element$id}", .con = con)
          element_options <- DBI::dbGetQuery(con, sql)
          
          create_element_xml(id = element_type, element_id = element$id, single_id = element_single, element_options = element_options, element_dir = project_type_dir)
          description <- element_options %>% dplyr::filter(name == "description_en") %>% dplyr::pull(value)
          create_element_readme(description = description, element_dir = project_type_dir)
        }
      }
    }
    
    # Copy project scripts files
    
    target_project_files_dir <- paste0(temp_element_dir, "/projects_files")
    dir.create(target_project_files_dir)
    
    source_project_files_dir <- paste0(r$app_folder, "/projects_files/", element_unique_id)
    list_of_files <- list.files(source_project_files_dir, ignore.case = TRUE, pattern = regex)
    
    if (length(list_of_files) > 0) file.copy(paste0(source_project_files_dir, "/", list_of_files), paste0(target_project_files_dir, "/", list_of_files))
    
    # Change IDs
    
    ### tabs_groups
    data$tabs_groups <- data$tabs_groups %>% dplyr::mutate(id = c(1L, 2L), .before = "category")
    
    ### tabs
    if (nrow(data$tabs) > 0) data$tabs <- 
      data$tabs %>%
      dplyr::left_join(corresponding_ids$tabs %>% dplyr::select(id = old_id, new_id), by = "id") %>%
      dplyr::left_join(corresponding_ids$tabs %>% dplyr::select(parent_tab_id = old_id, new_parent_tab_id = new_id), by = "parent_tab_id") %>%
      dplyr::left_join(corresponding_ids$tabs_groups %>% dplyr::select(tab_group_id = old_id, new_tab_group_id = new_id), by = "tab_group_id") %>%
      dplyr::select(-id, -parent_tab_id, -tab_group_id) %>%
      dplyr::rename(id = new_id, parent_tab_id = new_parent_tab_id, tab_group_id = new_tab_group_id) %>%
      dplyr::relocate(id, .before = "category") %>%
      dplyr::relocate(parent_tab_id, .before = "display_order") %>%
      dplyr::relocate(tab_group_id, .before = "parent_tab_id")
    
    ### widgets
    if (nrow(data$widgets) > 0) data$widgets <- 
      data$widgets %>%
      dplyr::left_join(corresponding_ids$widgets %>% dplyr::select(id = old_id, new_id = new_id), by = "id") %>%
      dplyr::left_join(corresponding_ids$tabs %>% dplyr::select(tab_id = old_id, new_tab_id = new_id), by = "tab_id") %>%
      dplyr::left_join(corresponding_ids$plugins %>% dplyr::select(plugin_id = old_id, new_plugin_id = new_id), by = "plugin_id") %>%
      dplyr::select(-id, -tab_id, -plugin_id) %>%
      dplyr::rename(id = new_id, tab_id = new_tab_id, plugin_id = new_plugin_id) %>%
      dplyr::mutate(plugin_id = dplyr::case_when(is.na(plugin_id) ~ 0, TRUE ~ plugin_id)) %>%
      dplyr::relocate(id, .before = "category") %>%
      dplyr::relocate(tab_id, .after = "name") %>%
      dplyr::relocate(plugin_id, .before = "display_order")
    
    ### widgets_concepts
    if (nrow(data$widgets_concepts) > 0) data$widgets_concepts <- 
      data$widgets_concepts %>%
      dplyr::left_join(corresponding_ids$widgets_concepts %>% dplyr::select(id = old_id, new_id = new_id), by = "id") %>%
      dplyr::left_join(corresponding_ids$widgets %>% dplyr::select(widget_id = old_id, new_widget_id = new_id), by = "widget_id") %>%
      dplyr::select(-id, -widget_id) %>%
      dplyr::rename(id = new_id, widget_id = new_widget_id) %>%
      dplyr::relocate(id, .before = "concept_id") %>%
      dplyr::relocate(widget_id, .after = "id")
    
    ### widgets_options
    if (nrow(data$widgets_options) > 0) data$widgets_options <- 
      data$widgets_options %>%
      dplyr::left_join(corresponding_ids$widgets_options %>% dplyr::select(id = old_id, new_id = new_id), by = "id") %>%
      dplyr::left_join(corresponding_ids$widgets_options %>% dplyr::select(link_id = old_id, new_link_id = new_id), by = "link_id") %>%
      dplyr::left_join(corresponding_ids$widgets %>% dplyr::select(widget_id = old_id, new_widget_id = new_id), by = "widget_id") %>%
      dplyr::select(-id, -widget_id, -link_id) %>%
      dplyr::rename(id = new_id, widget_id = new_widget_id, link_id = new_link_id) %>%
      dplyr::relocate(id, .before = "person_id") %>%
      dplyr::relocate(widget_id, .after = "id") %>%
      dplyr::relocate(link_id, .after = "person_id") %>%
      dplyr::mutate(value_num = dplyr::if_else(category == "general_settings" & name == "selected_file_id", NA_integer_, value_num))
    
    ### plugins
    if (nrow(data$plugins) > 0) data$plugins <-
      data$plugins %>%
      dplyr::left_join(corresponding_ids$plugins %>% dplyr::select(id = old_id, new_id), by = "id") %>%
      dplyr::select(-id) %>%
      dplyr::rename(id = new_id) %>%
      dplyr::relocate(id, .before = "name")
    
    ### subsets
    if (nrow(data$subsets) > 0) data$subsets <-
      data$subsets %>%
      dplyr::left_join(corresponding_ids$subsets %>% dplyr::select(id = old_id, new_id), by = "id") %>%
      dplyr::select(-id) %>%
      dplyr::rename(id = new_id) %>%
      dplyr::relocate(id, .before = "name")
    
    ### options
    for (element_type in c("plugins", "subsets")) {
      if (nrow(data[[paste0(element_type, "_options")]]) > 0) data[[paste0(element_type, "_options")]] <-
        data[[paste0(element_type, "_options")]] %>%
        dplyr::left_join(corresponding_ids[[element_type]] %>% dplyr::select(link_id = old_id, new_link_id = new_id), by = "link_id") %>%
        dplyr::select(-link_id) %>% dplyr::rename(link_id = new_link_id) %>% dplyr::relocate(link_id, .before = "name")
    }
    
    data$options <-
      data$plugins_options %>%
      dplyr::bind_rows(data$subsets_options) %>%
      dplyr::mutate(id = 1:dplyr::n())
    
    # Create CSV files
    dir.create(paste0(temp_element_dir, "/app_db"))
    for (csv_file in c("tabs_groups", "tabs", "widgets", "widgets_concepts", "widgets_options", "plugins", "subsets", "options")) readr::write_csv(data[[csv_file]], paste0(temp_element_dir, "/app_db/", csv_file, ".csv"))
    
    # Create widgets folder with widgets scripts
    
    widgets_dir <- file.path(temp_element_dir, "widgets")
    patient_lvl_data_dir <- file.path(widgets_dir, "patient_lvl_data")
    aggregated_data_dir <- file.path(widgets_dir, "aggregated_data")
    
    dir.create(widgets_dir)
    dir.create(patient_lvl_data_dir)
    dir.create(aggregated_data_dir)
    
    for (category in c("patient_lvl", "aggregated")){
      
      tabs <- data$tabs %>% dplyr::filter(category == !!category)
      if (nrow(tabs) > 0){
        for (i in 1:nrow(tabs)){
          tab <- tabs[i, ]
          
          tab_name <- tab$name %>% remove_special_chars()
          tab_path <- file.path(widgets_dir, paste0(category, "_data"), tab_name)
          dir.create(tab_path)
          
          widgets <- data$widgets %>% dplyr::filter(tab_id == tab$id)
          
          if (nrow(widgets) > 0){
            for (i in 1:nrow(widgets)){
              widget <- widgets[i, ]
              
              widget_name <- widget$name %>% remove_special_chars()
              widget_path <- file.path(tab_path, widget_name)
              dir.create(widget_path)
              
              scripts <-
                data$widgets_options %>%
                dplyr::filter(widget_id == widget$id, category == "settings_files", name == "file_name") %>%
                dplyr::select(id, name = value) %>%
                dplyr::left_join(
                  data$widgets_options %>%
                    dplyr::filter(widget_id == widget$id, category == "figure_settings", name == "code") %>%
                    dplyr::select(id = link_id, code = value),
                  by = "id"
                )
              
              if (nrow(scripts) > 0){
                for (i in 1:nrow(scripts)){
                  script <- scripts[i, ]
                  
                  script_path <- file.path(widget_path, paste0(remove_special_chars(script$name), ".R"))
                  if (!is.na(script$code)) writeLines(script$code, script_path)
                  else file.create(script_path)
                }
              }
            } 
          }
        }
      }
    }
  }
  
  return(temp_zip_dir)
}

#' @noRd
create_element_readme <- function(description, element_dir){
  writeLines(description, paste0(element_dir, "/README.md"))
}

#' @noRd
create_element_scripts <- function(id, element_dir, code = ""){
  
  # Get variables from other environments
  language <- get("language", envir = parent.frame())
  
  if (!dir.exists(element_dir)) dir.create(element_dir)
  
  if (id == "datasets"){
    
    code_language <- ""
    if (language != "fr") code_language <- paste0(language, "/")
    
    code <- paste0(
      "# See documentation here: https://linkr.interhop.org/", code_language, "docs/import_data/\n\n",
      "# To import only specific tables, add tables_to_load argument:\n",
      "# tables_to_load = c(\"person\", \"visit_occurrence\", \"visit_detail\", \"measurement\")\n\n",
      "",
      "# 1) Import data from a folder\n\n",
      "",
      "# Specify a folder: all CSV and Parquet files will be automatically loaded.\n\n",
      "",
      "# import_dataset(omop_version = \"5.4\", data_folder = \"/my_data_folder\")\n\n",
      "",
      "# 2) Import data from a database connection\n\n",
      "",
      "# con <- DBI::dbConnect(\n",
      "    # RPostgres::Postgres(),\n",
      "    # host = \"localhost\",\n",
      "    # port = 5432,\n",
      "    # dbname = \"mimic-iv\",\n",
      "    # user = \"postgres\",\n",
      "    # password = \"postgres\"\n",
      "# )\n\n",
      "",
      "# import_dataset(omop_version = \"5.4\", con = con)"
    )
    
    writeLines(code, paste0(element_dir, "/main.R"))
  }
  else if (id == "subsets"){
    
    code <- paste0("add_patients_to_subset(patients = d$visit_detail %>% dplyr::select(person_id, visit_occurrence_id, visit_detail_id))")
    
    writeLines(code, paste0(element_dir, "/main.R"))
  }
  else if (id == "plugins"){
    
    ui_code <- ""
    writeLines(ui_code, paste0(element_dir, "/ui.R"))
    
    server_code <- ""
    writeLines(server_code, paste0(element_dir, "/server.R"))
    
    translations_code <- "base,en,fr\n"
    writeLines(translations_code, paste0(element_dir, "/translations.csv"))
  }
}

#' @noRd
create_element_ui <- function(page_id, element_id, single_id, element_name, users_ui, widget_buttons, onclick, short_description, is_selected_element){
  
  # Get variables from other environments
  id <- get("id", envir = parent.frame())
  ns <- NS(id)
  
  selected_element_css <- ""
  if (is_selected_element) selected_element_css <- paste0("selected_", single_id, "_widget")
  
  element_icon <- switch(
    single_id,
    "project" = "fa-folder-open",
    "dataset" = "fa-database",
    "data_cleaning" = "fa-broom",
    "plugin" = "fa-puzzle-piece",
    "subset" = "fa-layer-group",
    "vocabulary" = "fa-language"
  )
  
  max_length <- 75
  if (nchar(short_description) > max_length) {
    short_description_limited <- paste0(substr(short_description, 1, max_length - 3), "...")
    short_description_limited <- create_hover_card(ui = short_description_limited, text = short_description)
  } else {
    short_description_limited <- short_description
  }
  
  max_length <- 25
  if (nchar(element_name) > max_length) {
    element_name_limited <- paste0(substr(element_name, 1, max_length - 3), "...")
    element_name_limited <- create_hover_card(ui = element_name_limited, text = element_name)
  } else {
    element_name_limited <- element_name
  }
  
  div_content <- 
    div(
      id = ns(paste0(single_id, "_widget_", element_id)),
      class = paste0(selected_element_css, " element_widget ", single_id, "_widget"),
      div(
        class = paste0("element_widget_icon ", single_id, "_widget_icon"),
        tags$i(class = paste0("fas ", element_icon))
      ),
      div(
        class = "element_widget_content",
        tags$h1(element_name_limited),
        div(
          class = "element_widget_description",
          short_description_limited
        )
      ),
      widget_buttons
    )
  
  onclick <- paste0(onclick, "Shiny.setInputValue('", page_id, "-selected_element_type', '');")
  
  div(
    onclick = onclick,
    div_content
  )
}

#' @noRd
create_elements_ui <- function(page_id, id, elements, selected_element = NA_integer_, r, language, i18n){
  
  single_id <- switch(
    id, 
    "data_cleaning" = "data_cleaning",
    "datasets" = "dataset",
    "projects" = "project",
    "plugins" = "plugin", 
    "subsets" = "subset",
    "vocabularies" = "vocabulary"
  )
  
  ns <- NS(page_id)
  
  elements_ui <- tagList()
  
  elements_ids <- elements %>% dplyr::filter(name == paste0("name_", language)) %>% dplyr::arrange(value) %>% dplyr::pull(id)
  
  for (i in unique(elements_ids)){
    row <- elements %>% dplyr::filter(id == i)
    
    users_ui <- create_authors_ui(row %>% dplyr::filter(name == "author") %>% dplyr::pull(value))
    
    if (id == "vocabularies") element_name <- r$vocabularies_wide %>% dplyr::filter(id == i) %>% dplyr::pull(vocabulary_id)
    else element_name <- row %>% dplyr::filter(name == paste0("name_", language)) %>% dplyr::pull(value)
    
    # For plugins widgets, we add some content on the bottom
    
    widget_buttons <- tagList()
    short_description <- ""
    
    if (page_id == "home") onclick <- paste0("
      Shiny.setInputValue('", page_id, "-selected_", single_id, "', ", row$id, ");
      Shiny.setInputValue('", page_id, "-selected_", single_id, "_trigger', Math.random());
    ")
    else onclick <- paste0("
      Shiny.setInputValue('", page_id, "-selected_element', ", row$id, ");
      Shiny.setInputValue('", page_id, "-selected_element_trigger', Math.random());
    ")
    
    if (id %in% c("data_cleaning", "datasets", "plugins", "projects", "subsets")) short_description <- row %>% dplyr::filter(name == paste0("short_description_", language)) %>% dplyr::pull(value)
    if (length(short_description) == 0) short_description <- ""
    
    if (id == "plugins"){
      plugin_type <- row %>% dplyr::slice(1) %>% dplyr::pull(tab_type_id)
      if (page_id == "data") buttons <- c("tab_type", "info")
      else buttons <- "tab_type"
      widget_buttons <- get_plugin_buttons(page_id, buttons, plugin_type, row$id)
    }
    
    # Test if this element is the currently selected element, to add a green border
    if (!is.na(selected_element) & selected_element == i) is_selected_element <- TRUE
    else is_selected_element <- FALSE
    
    elements_ui <- tagList(
      elements_ui,
      create_element_ui(page_id, i, single_id, element_name, users_ui, widget_buttons, onclick, short_description, is_selected_element)
    )
  }
  
  if (page_id != "home") elements_ui <- div(elements_ui, class = paste0(id, "_container"))
  
  elements_ui
}

#' @noRd
create_element_xml <- function(id, element_id, single_id, element_options, element_dir){
  
  # Get variables from other environments
  r <- get("r", envir = parent.frame())
  
  xml <- XML::newXMLDoc()
  
  xml_root <- id
  if (id == "data_cleaning") xml_root <- "data_cleaning_scripts"
  
  elements_node <- XML::newXMLNode(xml_root, doc = xml)
  element_node <- XML::newXMLNode(single_id, parent = elements_node)
  
  XML::newXMLNode("app_version", r$app_version, parent = element_node)
  
  for(name in c("unique_id", "version", paste0("name_", r$languages$code), paste0("category_", r$languages$code))) XML::newXMLNode(
    name,
    element_options %>% dplyr::filter(name == !!name) %>% dplyr::pull(value),
    parent = element_node
  )
  
  for(name in c(paste0("description_", r$languages$code), paste0("short_description_", r$languages$code))) XML::newXMLNode(
    name,
    element_options %>% dplyr::filter(name == !!name) %>% dplyr::pull(value),
    parent = element_node
  )
  
  author_value <- element_options %>% dplyr::filter(name == "author") %>% dplyr::pull(value)
  authors_node <- XML::newXMLNode("authors", parent = element_node)
  authors <- strsplit(author_value, "[;,]")[[1]] %>% trimws()
  for (author in authors) {
    XML::newXMLNode("author", author, parent = authors_node)
  }
  
  element <- r[[paste0(id, "_wide")]] %>% dplyr::filter(id == element_id)
  
  # Datetimes not implemented for subsets
  if (id != "subsets") for (name in c("creation_datetime", "update_datetime")) XML::newXMLNode(name, element %>% dplyr::pull(get(!!name)), parent = element_node)
  
  # Specific nodes
  if (id == "plugins")  XML::newXMLNode("type", element$tab_type_id, parent = element_node)
  else if (id == "datasets") XML::newXMLNode("omop_version", element_options %>% dplyr::filter(name == "omop_version") %>% dplyr::pull(value), parent = element_node)
  
  # Create XML file
  XML::saveXML(xml, file = paste0(element_dir, paste0("/", single_id, ".xml")))
}

#' @noRd
create_options_tibble <- function(element_id, element_name, sql_category, user_id, username, last_options_id){
  
  # Get variables from other environments
  r <- get("r", envir = parent.frame())
  languages <- r$languages
  user_id <- r$user_id
  
  tibble::tribble(
    ~name, ~value, ~value_num,
    "users_allowed_read_group", "people_picker", 1,
    "user_allowed_read", "", user_id,
    "version", "0.0.1.9000", NA_integer_,
    "unique_id", paste0(sample(c(0:9, letters[1:6]), 64, TRUE), collapse = ''), NA_integer_,
    "author", username, NA_integer_,
    "downloaded_from", "", NA_integer_,
    "downloaded_from_url", "", NA_integer_
  ) %>%
    dplyr::bind_rows(
      languages %>%
        tidyr::crossing(name = c("description", "category", "name")) %>%
        dplyr::mutate(
          name = paste0(name, "_", code),
          value = ifelse(grepl("name_", name), element_name, ""),
          value_num = NA_integer_
        ) %>%
        dplyr::select(-code, -language)
    ) %>%
    dplyr::mutate(id = last_options_id + dplyr::row_number(), category = sql_category, link_id = element_id, .before = "name") %>%
    dplyr::mutate(creator_id = user_id, datetime = now(), deleted = FALSE)
}

#' @noRd
create_rmarkdown_file <- function(code, interpret_code = TRUE){
  
  # Get variables from other environments
  r <- get("r", envir = parent.frame())
  
  code <- gsub("\r", "\n", code)
  
  # Create temp dir
  dir <- paste0(r$app_folder, "/temp_files/", r$user_id, "/markdowns")
  output_file <- paste0(dir, "/", paste0(sample(c(0:9, letters[1:6]), 8, TRUE), collapse = ''), "_", now() %>% stringr::str_replace_all(":", "_") %>% stringr::str_replace_all(" ", "_"), ".Md")
  if (!dir.exists(dir)) dir.create(dir)
  
  # Create the markdown file
  if (interpret_code) knitr::knit(text = code, output = output_file, quiet = TRUE)
  else writeLines(code, con = output_file)
  
  return(output_file)
}

#' @noRd
delete_project <- function(project_id){
  
  # Get variables from other environments
  for (obj_name in c("r", "m")) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  # Delete project files
  
  project_unique_id <- r$projects_long %>% dplyr::filter(id == project_id, name == "unique_id") %>% dplyr::pull(value)
  if (length(project_unique_id) > 0) unlink(file.path(r$app_folder, "projects_files", project_unique_id), recursive = TRUE)
  
  # Delete subsets files
  
  sql <- glue::glue_sql("SELECT id FROM subsets WHERE study_id = {project_id}", .con = m$db)
  subsets_ids <- DBI::dbGetQuery(m$db, sql) %>% dplyr::pull()
  sql <- glue::glue_sql("SELECT value FROM options WHERE category = 'subset' AND name = 'unique_id' AND link_id IN ({subsets_ids*})", .con = m$db)
  subsets_unique_ids <- DBI::dbGetQuery(m$db, sql) %>% dplyr::pull()
  for (subset_unique_id in subsets_unique_ids) unlink(file.path(r$app_folder, "subsets", subset_unique_id), recursive = TRUE)
  
  # Delete data in DB
  
  sql <- glue::glue_sql("SELECT patient_lvl_tab_group_id, aggregated_tab_group_id FROM studies WHERE id = {project_id}", .con = r$db)
  tabs_groups_ids <- DBI::dbGetQuery(r$db, sql)
  tabs_groups_ids <- c(tabs_groups_ids$patient_lvl_tab_group_id, tabs_groups_ids$aggregated_tab_group_id)
  
  sql <- glue::glue_sql("SELECT * FROM tabs WHERE tab_group_id IN ({tabs_groups_ids*})", .con = r$db)
  tabs_ids <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(id)
  
  sql <- glue::glue_sql("SELECT * FROM widgets WHERE tab_id IN ({tabs_ids*})", .con = r$db)
  widgets_ids <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull(id)
  
  sql <- glue::glue_sql("SELECT * FROM subsets WHERE study_id = {project_id}", .con = m$db)
  subsets_ids <- DBI::dbGetQuery(m$db, sql) %>% dplyr::pull(id)
  
  sql <- glue::glue_sql("DELETE FROM subsets WHERE study_id = {project_id}", .con = m$db)
  sql_send_statement(m$db, sql)
  
  sql <- glue::glue_sql("DELETE FROM options WHERE category = 'subset' AND link_id IN ({subsets_ids*})", .con = m$db)
  sql_send_statement(m$db, sql)
  
  sql <- glue::glue_sql("DELETE FROM widgets_options WHERE widget_id IN ({widgets_ids*})", .con = m$db)
  sql_send_statement(m$db, sql)
  
  sql <- glue::glue_sql("DELETE FROM widgets_concepts WHERE widget_id IN ({widgets_ids*})", .con = m$db)
  sql_send_statement(m$db, sql)
  
  sql <- glue::glue_sql("DELETE FROM persons_options WHERE study_id = {project_id}", .con = m$db)
  sql_send_statement(m$db, sql)
  
  sql <- glue::glue_sql("DELETE FROM widgets WHERE tab_id IN ({tabs_ids*})", .con = r$db)
  sql_send_statement(r$db, sql)
  
  sql <- glue::glue_sql("DELETE FROM tabs WHERE tab_group_id IN ({tabs_groups_ids*})", .con = r$db)
  sql_send_statement(r$db, sql)
}

#' @noRd
get_plugin_buttons <- function(id, buttons, plugin_type, plugin_id){
  
  # Get variables from other environments
  r <- get("r", envir = parent.frame())
  i18n <- r$i18n
  
  buttons_ui <- tagList()
  
  if ("tab_type" %in% buttons){
    if (plugin_type == 1) {
      plugin_type_icon <- div(shiny.fluent::FontIcon(iconName = "Contact"), class = "small_icon_button")
      plugin_type_text <- i18n$t("patient_lvl_plugin")
    }
    else if (plugin_type == 2) {
      plugin_type_icon <- div(shiny.fluent::FontIcon(iconName = "Group"), class = "small_icon_button")
      plugin_type_text <- i18n$t("aggregated_plugin")
    }
    else if (plugin_type %in% c(12, 21)){
      plugin_type_icon <- div(
        div(shiny.fluent::FontIcon(iconName = "Contact"), class = "small_icon_button"),
        div(shiny.fluent::FontIcon(iconName = "Group"), class = "small_icon_button"),
        style = "display: flex; gap: 5px;"
      )
      plugin_type_text <- i18n$t("patient_lvl_or_aggregated_plugin")
    }
    
    plugin_type_icon <- create_hover_card(ui = plugin_type_icon, text = plugin_type_text)
  
    buttons_ui <- tagList(
      buttons_ui,
      div(
        plugin_type_icon,
        class = "plugin_widget_bottom_button"
      )
    )
  }
  
  if ("info" %in% buttons){
    buttons_ui <- tagList(
      buttons_ui,
      div(
        class = "plugin_widget_top_button",
        tags$a(
          href = "#",
          onclick = paste0("
            event.preventDefault();
            event.stopPropagation();
            Shiny.setInputValue('", id, "-show_plugin_description_trigger', Math.random());
            Shiny.setInputValue('", id, "-show_plugin_description', ", plugin_id, ");
          "),
          tags$i(class = "fa-solid fa-circle-info")
        )
      )
    )
  }
  
  buttons_ui
}

#' @noRd
import_element <- function(con, sql_table, sql_category, single_id, element, element_type, temp_dir, user_accesses){
  
  # Get variables from other environments
  for (obj_name in c("id", "r", "m", "input", "output", "user_accesses")) assign(obj_name, get(obj_name, envir = parent.frame()))
  ns <- NS(id)
  i18n <- r$i18n
  language <- r$language
  
  # Delete old files and copy new files
  
  element_folder <- paste0(r$app_folder, "/", element_type, "/", element$unique_id)
  unlink(element_folder, recursive = TRUE)
  dir.create(element_folder)
  
  files_list <- list.files(temp_dir) %>% setdiff(c("app_db", "plugins", "projects_files", "subsets"))
  file.copy(paste0(temp_dir, "/", files_list), paste0(element_folder, "/", files_list))
  
  # Reassign new id
  element <- element %>% dplyr::mutate(id = get_last_row(con, sql_table) + 1)
  
  # Delete old entries in database
  
  element_id <- DBI::dbGetQuery(con, glue::glue_sql("SELECT link_id FROM options WHERE category = {sql_category} AND name = 'unique_id' AND value = {element$unique_id}", .con = con)) %>% dplyr::pull()
  
  if (length(element_id) > 0){
    
    # Keep old element id
    element <- element %>% dplyr::mutate(id = element_id)
    
    # Delete element in db
    sql_send_statement(con, glue::glue_sql("DELETE FROM {`sql_table`} WHERE id = {element_id}", .con = con))
    
    # For projects, delete all rows in associated tables
    if (sql_category == "study") delete_project(element_id)
    
    # Delete options in db
    sql_send_statement(con, glue::glue_sql("DELETE FROM options WHERE category = {sql_category} AND link_id = {element_id}", .con = con))
  }
  
  # Import data in database
  
  # Create columns if don't exist (one column for option and for language)
  prefixes <- c("short_description", "description", "name", "category")
  new_cols <- outer(prefixes, r$languages$code, paste, sep = "_") %>% as.vector()
  for(col in new_cols) if(!col %in% colnames(element)) element <- element %>% dplyr::mutate(!!col := "")
  
  element <- element %>% dplyr::mutate(name = get(paste0("name_", language)))
  
  # Element table
  
  if (element_type == "plugins") new_data <-
    element %>% 
    dplyr::transmute(id, name, tab_type_id = type, creation_datetime, update_datetime, deleted = FALSE)
  
  else if (element_type == "datasets") new_data <-
    element %>% 
    dplyr::transmute(id, name, data_source_id = NA_integer_, creation_datetime, update_datetime, deleted = FALSE)
  
  else if (element_type == "projects") new_data <-
    element %>%
    dplyr::transmute(
      id, name, dataset_id = NA_integer_, patient_lvl_tab_group_id = NA_integer_, aggregated_tab_group_id = NA_integer_,
      creator_id = r$user_id, creation_datetime, update_datetime, deleted = FALSE
    )
  
  DBI::dbAppendTable(con, sql_table, new_data)
  
  # Options table
  
  new_options <- tibble::tribble(
    ~name, ~value, ~value_num,
    "users_allowed_read_group", "everybody", 1,
    "user_allowed_read", "", r$user_id,
    "version", element$version, NA_real_,
    "unique_id", element$unique_id, NA_real_,
    "author", element$authors, NA_real_,
    "downloaded_from", "", NA_real_,
    "downloaded_from_url", "", NA_real_
  ) %>%
    dplyr::bind_rows(
      element %>%
        dplyr::select(dplyr::starts_with(c("category", "name", "description", "short_description")), -name) %>%
        tidyr::pivot_longer(dplyr::starts_with(c("category", "name", "description", "short_description")), names_to = "name", values_to = "value") %>%
        dplyr::mutate(value_num = NA_real_)
    )
  
  if (element_type == "datasets") new_options <- 
    new_options %>% 
    dplyr::bind_rows(tibble::tibble(name = "omop_version", value = element$omop_version, value_num = NA_integer_))
  
  new_options <- 
    new_options %>%
    dplyr::mutate(id = get_last_row(con, "options") + dplyr::row_number(), category = sql_category, link_id = new_data$id, .before = "name") %>%
    dplyr::mutate(creator_id = r$user_id, datetime = now(), deleted = FALSE)
  
  DBI::dbAppendTable(con, "options", new_options)
  
  # Import project
  
  if (element_type == "projects"){
    
    update_plugins <- TRUE
    if (length(input$import_project_plugins) > 0) update_plugins <- input$import_project_plugins
    
    import_project(temp_dir = temp_dir, update_plugins = update_plugins, project_id = element$id, unique_id = element$unique_id)
  }
  
  # Reload elements var and widgets
  shinyjs::runjs(paste0("Shiny.setInputValue('", element_type, "-reload_elements_var', Math.random());"))
  
  # For project import, reload plugins var
  if (element_type == "projects") reload_elements_var(page_id = id, id = "plugins", con = r$db, long_var_filtered = "filtered_plugins_long")
  
  show_message_bar(paste0("success_importing_", single_id), "success")
  
  # Update selected element UI
  if (id == "git_repos") shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-selected_element_trigger', Math.random());"))
}

#' @noRd
import_project <- function(temp_dir, update_plugins, project_id, unique_id){
  
  # Get variables from other environments
  for (obj_name in c("r", "m", "user_accesses")) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  # Tables :
  # - tabs_groups
  # - tabs
  # - widgets
  # - widgets_concepts
  # - widgets_options
  # - plugins
  # - subsets
  # - options
  
  # Load CSV files
  
  csv_folder <- paste0(temp_dir, "/app_db")
  
  csv_files <- list.files(csv_folder)
  
  data <- list()
  last_row <- list()
  
  if (length(csv_files) > 0){
    for (csv_file in csv_files){
      
      table <- sub("\\.csv$", "", csv_file, ignore.case = TRUE)
      
      file_path <- paste0(csv_folder, "/", csv_file)
      
      if (table %in% c("options", "plugins", "tabs", "tabs_groups", "widgets")){
        db <- "main"
        con <- r$db
      }
      else {
        db <- "public"
        con <- m$db
      }
      
      last_row[[table]] <- get_last_row(con, table)
      
      col_types <- get_app_db_col_types() %>% dplyr::filter(db == !!db, table == !!table) %>% dplyr::pull(col_types)
      data[[table]] <- vroom::vroom(file_path, col_types = col_types, progress = FALSE)
      
      if (table %not_in% c("options", "plugins")) data[[table]] <- data[[table]] %>% dplyr::mutate(id = id + last_row[[table]])
    }
  }
  
  # Add data in database
  
  ## plugins
  
  ### Reload r$plugins_wide & r$plugins_long
  reload_elements_var(page_id = "plugins", id = "plugins", con = r$db, long_var_filtered = "filtered_plugins_long")
  
  data$plugins <- data$plugins %>% dplyr::mutate(new_id = id + last_row$plugins)
  
  local_plugins <- 
    r$plugins_wide %>%
    dplyr::left_join(r$plugins_long %>% dplyr::filter(name == "unique_id") %>% dplyr::select(id, unique_id = value), by = "id") %>%
    dplyr::left_join(r$plugins_long %>% dplyr::filter(name == "version") %>% dplyr::select(id, local_version = value), by = "id")
  
  imported_plugins <-
    data$plugins %>% dplyr::select(id, tab_type_id) %>%
    dplyr::left_join(data$options %>% dplyr::filter(category == "plugin" & name == "unique_id") %>% dplyr::select(id = link_id, unique_id = value), by = "id") %>%
    dplyr::left_join(data$options %>% dplyr::filter(category == "plugin" & name == "version") %>% dplyr::select(id = link_id, imported_version = value), by = "id")
  
  new_plugins <- imported_plugins %>% dplyr::filter(unique_id %not_in% local_plugins$unique_id)
  
  more_recent_plugins <-
    imported_plugins %>%
    dplyr::filter(unique_id %in% local_plugins$unique_id) %>%
    dplyr::left_join(local_plugins %>% dplyr::select(unique_id, local_version), by = "unique_id") %>%
    dplyr::mutate_at(c("imported_version", "local_version"), package_version) %>%
    dplyr::filter(imported_version > local_version)
  
  # Change ID
  # If already exists locally, keep local ID
  data$plugins <-
    data$plugins %>%
    dplyr::left_join(imported_plugins %>% dplyr::select(id, unique_id), by = "id") %>%
    dplyr::left_join(local_plugins %>% dplyr::select(local_id = id, unique_id), by = "unique_id") %>%
    dplyr::mutate_at(c("local_id", "new_id"), as.integer) %>%
    dplyr::mutate(new_id = dplyr::case_when(
      !is.na(local_id) ~ local_id,
      TRUE ~ new_id
    )) %>%
    dplyr::select(-unique_id, -local_id)
  
  ## projects
  patient_lvl_tab_group_id <- data$tabs_groups %>% dplyr::filter(category == "patient_lvl") %>% dplyr::pull(id)
  aggregated_tab_group_id <- data$tabs_groups %>% dplyr::filter(category == "aggregated") %>% dplyr::pull(id)
  sql <- glue::glue_sql("UPDATE studies SET patient_lvl_tab_group_id = {patient_lvl_tab_group_id}, aggregated_tab_group_id = {aggregated_tab_group_id} WHERE id = {project_id}", .con = r$db)
  sql_send_statement(r$db, sql)
  
  ## subsets
  data$subsets <- data$subsets %>% dplyr::mutate(study_id = project_id)
  
  ## tabs
  data$tabs <- data$tabs %>% dplyr::mutate(
    tab_group_id = tab_group_id + last_row$tabs_groups,
    parent_tab_id = parent_tab_id + last_row$tabs
  )
  
  ## widgets
  data$widgets <-
    data$widgets %>%
    dplyr::mutate(tab_id = tab_id + last_row$tabs) %>%
    dplyr::left_join(data$plugins %>% dplyr::select(plugin_id = id, new_plugin_id = new_id), by = "plugin_id") %>%
    dplyr::select(-plugin_id) %>%
    dplyr::rename(plugin_id = new_plugin_id) %>%
    dplyr::relocate(plugin_id, .after = "tab_id")
  
  ## widgets_concepts
  data$widgets_concepts <- data$widgets_concepts %>% dplyr::mutate(widget_id = widget_id + last_row$widgets)
  
  ## widgets_options
  data$widgets_options <- 
    data$widgets_options %>%
    dplyr::mutate(widget_id = widget_id + last_row$widgets, link_id = link_id + last_row$widgets_options)
  
  ## options
  
  plugins_options <- data$options %>% dplyr::filter(category == "plugin")
  subsets_options <- data$options %>% dplyr::filter(category == "subset")
  
  if (nrow(data$plugins) > 0){
    plugins_options <-
      plugins_options %>%
      dplyr::left_join(
        data$plugins %>% dplyr::transmute(link_id = id, new_link_id = new_id, category = "plugin"),
        by = c("link_id", "category")
      ) %>%
      dplyr::select(-link_id) %>%
      dplyr::rename(link_id = new_link_id) %>%
      dplyr::relocate(link_id, .after = "category")
  }
  
  if (nrow(data$subsets) > 0){
    subsets_options <-
      subsets_options %>%
      dplyr::mutate(link_id = link_id + last_row$subsets)
  }
  
  data$options <- dplyr::bind_rows(plugins_options, subsets_options)
  
  # Delete data for more recent plugins
  if (update_plugins){
    
    # Get local IDs
    ids_to_del <-
      local_plugins %>% 
      dplyr::inner_join(more_recent_plugins %>% dplyr::select(unique_id), by = "unique_id") %>%
      dplyr::pull(id)
    
    # Delete rows in db
    if (length(ids_to_del) > 0){
      
      sql <- glue::glue_sql("DELETE FROM plugins WHERE id IN ({ids_to_del*})", .con = r$db)
      sql_send_statement(r$db, sql)
      
      sql <- glue::glue_sql("DELETE FROM options WHERE category = 'plugin' AND link_id IN ({ids_to_del*})", .con = r$db)
      sql_send_statement(r$db, sql)
    }
    
    # Delete files
    for (plugin_unique_id in more_recent_plugins$unique_id){
      plugin_folder <- file.path(r$app_folder, "plugins", plugin_unique_id)
      unlink(plugin_folder, recursive = TRUE)
    }
  }
  
  # Filter plugins to add
  if (update_plugins) data$plugins <- data$plugins %>% dplyr::filter(id %in% new_plugins$id | id %in% more_recent_plugins$id)
  if (!update_plugins) data$plugins <- data$plugins %>% dplyr::filter(id %in% new_plugins$id)
  
  data$plugins <- data$plugins %>% dplyr::mutate(id = new_id) %>% dplyr::select(-new_id)
  
  # Filter options on plugins to import
  data$options <- data$options %>% dplyr::filter(category == "subset" | (category == "plugin" & link_id %in% data$plugins$id))
  
  # Copy data in app database
  for (table in c("tabs_groups", "tabs", "widgets", "widgets_concepts", "widgets_options", "plugins", "subsets")){
    
    if (table %in% c("widgets_concepts", "widgets_options", "subsets")) con <- m$db
    else con <- r$db
    
    DBI::dbAppendTable(con, table, data[[table]])
    
    if (table == "plugins"){
      
      last_options_row <- get_last_row(con, "options")
      data$options <- data$options %>% dplyr::mutate(id = last_options_row + dplyr::row_number())
      DBI::dbAppendTable(con, "options", data$options %>% dplyr::filter(category == "plugin"))
    }
    else if (table == "subsets"){
      
      last_options_row <- get_last_row(con, "options")
      data$options <- data$options %>% dplyr::mutate(id = last_options_row + dplyr::row_number())
      DBI::dbAppendTable(con, "options", data$options %>% dplyr::filter(category == "subset"))
    }
  }
  
  # Copy plugins and subsets files
  
  for (element_type in c("plugins", "subsets")) {
    
    element_single <- substr(element_type, 1, nchar(element_type)-1)
    
    if (nrow(data[[element_type]]) > 0){
      items <-
        data[[element_type]] %>%
        dplyr::select(id) %>%
        dplyr::left_join(
          data$options %>%
            dplyr::filter(category == element_single & name == "unique_id") %>%
            dplyr::select(id = link_id, unique_id = value),
          by = "id"
        ) %>%
        dplyr::left_join(
          data$options %>%
            dplyr::filter(category == element_single & name == "name_en") %>%
            dplyr::select(id = link_id, name = value),
          by = "id"
        ) %>%
        dplyr::mutate_at("name", remove_special_chars)
      
      name_to_unique_id <- items %>% dplyr::select(name, unique_id) %>% dplyr::distinct()
      
      source_folder <- file.path(temp_dir, element_type)
      target_folder <- file.path(r$app_folder, element_type)
      
      for (name in name_to_unique_id$name) {
        source_path <- file.path(source_folder, name)
        
        new_name <- name_to_unique_id %>% dplyr::filter(name == !!name) %>% dplyr::pull(unique_id)
        
        target_path <- file.path(target_folder, new_name)
        if (!dir.exists(target_path)) dir.create(target_path, recursive = TRUE)
        
        files_list <- list.files(source_path, full.names = TRUE)
        file.copy(files_list, target_path, overwrite = TRUE)
      }
    }
  }
  
  # Copy projects scripts files
  
  projects_files_source_folder <- paste0(temp_dir, "/projects_files")
  files_list <- list.files(projects_files_source_folder, full.names = TRUE)
  
  projects_files_target_folder <- file.path(r$app_folder, "projects_files", unique_id)
  if (!dir.exists(projects_files_target_folder)) dir.create(projects_files_target_folder)
  
  file.copy(files_list, projects_files_target_folder, overwrite = TRUE)
}

#' @noRd
reload_elements_var <- function(page_id, id, con, long_var_filtered){
  
  # Get variables from other environments
  for (obj_name in c("r", "m", "user_accesses")) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  sql_category <- switch(
    id,
    "data_cleaning" = "data_cleaning",
    "datasets" = "dataset",
    "projects" = "study",
    "plugins" = "plugin", 
    "subsets" = "subset",
    "vocabularies" = "vocabulary"
  )
  
  sql_table <- switch(
    id, 
    "data_cleaning" = "scripts",
    "datasets" = "datasets",
    "projects" = "studies",
    "plugins" = "plugins", 
    "subsets" = "subsets",
    "vocabularies" = "vocabulary"
  )
  
  long_var <- paste0(id, "_long")
  wide_var <- paste0(id, "_wide")
  
  # See all elements ?
  if (paste0(id, "_see_all_data") %in% user_accesses) sql_join <- "LEFT JOIN" else sql_join <- "INNER JOIN"
  
  if (sql_table == "plugins"){
    sql <- glue::glue_sql(paste0("WITH {paste0('selected_', id)} AS (
      SELECT DISTINCT d.id
      FROM {sql_table} d
      LEFT JOIN options AS r ON d.id = r.link_id AND r.category = {sql_category} AND r.name = 'users_allowed_read_group'
      LEFT JOIN options AS u ON d.id = u.link_id AND u.category = {sql_category} AND u.name = 'user_allowed_read'
      WHERE r.value = 'everybody' OR (r.value = 'people_picker' AND u.value_num = {r$user_id})
    )
    SELECT d.id, d.update_datetime, d.tab_type_id, o.name, o.value, o.value_num
      FROM {sql_table} d
      ", sql_join, " {paste0('selected_', id)} ON d.id = {paste0('selected_', id)}.id
      LEFT JOIN options o ON o.category = {sql_category} AND d.id = o.link_id"), .con = con)
  }
  
  else if (sql_table == "subsets"){
    if (length(m$selected_study) == 0) return()
    sql <- glue::glue_sql(paste0("
      SELECT s.id, o.name, o.value, o.value_num
      FROM subsets s
      LEFT JOIN options o ON o.category = 'subset' AND s.id = o.link_id
      WHERE study_id = {m$selected_study}"), .con = con)
  }
  
  else {
    sql <- glue::glue_sql(paste0("WITH {paste0('selected_', id)} AS (
      SELECT DISTINCT d.id
      FROM {sql_table} d
      LEFT JOIN options AS r ON d.id = r.link_id AND r.category = {sql_category} AND r.name = 'users_allowed_read_group'
      LEFT JOIN options AS u ON d.id = u.link_id AND u.category = {sql_category} AND u.name = 'user_allowed_read'
      WHERE r.value = 'everybody' OR (r.value = 'people_picker' AND u.value_num = {r$user_id})
    )
    SELECT d.id, d.update_datetime, o.name, o.value, o.value_num
      FROM {sql_table} d
      ", sql_join, " {paste0('selected_', id)} ON d.id = {paste0('selected_', id)}.id
      LEFT JOIN options o ON o.category = {sql_category} AND d.id = o.link_id"), .con = con)
  }
  
  r[[long_var]] <- DBI::dbGetQuery(con, sql) %>% tibble::as_tibble()
  
  if (page_id == "data"){
    if (r$data_page == "patient_lvl") tab_type_id <- 1 else tab_type_id <- 2
    r[[long_var_filtered]] <- r[[long_var]] %>% dplyr::filter(grepl(!!tab_type_id, tab_type_id))
  }
  else r[[long_var_filtered]] <- r[[long_var]]
  
  sql <- glue::glue_sql("SELECT * FROM {`sql_table`} WHERE id IN ({unique(r[[long_var]]$id)*})", .con = con)
  r[[wide_var]] <- DBI::dbGetQuery(con, sql) %>% tibble::as_tibble()
  
  # Reload home widgets
  r[[paste0("reload_home_", id)]] <- now()
  
  # Reload project subsets
  if (sql_table == "subsets" && !is.na(m$selected_study)) m$subsets <- r$subsets_wide %>% dplyr::filter(study_id == m$selected_study)
}
