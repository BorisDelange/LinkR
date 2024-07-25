#' @noRd
reload_elements_var <- function(page_id, con, r, m, long_var_filtered){
  
  if (page_id == "data") id <- "plugins"
  else id <- page_id
  
  sql_category <- switch(id, 
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
  
  if (sql_table == "plugins"){
    sql <- glue::glue_sql("WITH {paste0('selected_', id)} AS (
      SELECT DISTINCT d.id
      FROM {sql_table} d
      LEFT JOIN options AS r ON d.id = r.link_id AND r.category = {sql_category} AND r.name = 'users_allowed_read_group'
      LEFT JOIN options AS u ON d.id = u.link_id AND u.category = {sql_category} AND u.name = 'user_allowed_read'
      WHERE r.value = 'everybody' OR (r.value = 'people_picker' AND u.value_num = {r$user_id})
    )
    SELECT d.id, d.update_datetime, d.tab_type_id, o.name, o.value, o.value_num
      FROM {sql_table} d
      INNER JOIN {paste0('selected_', id)} ON d.id = {paste0('selected_', id)}.id
      LEFT JOIN options o ON o.category = {sql_category} AND d.id = o.link_id", .con = con)
  }
  
  else if (sql_table == "subsets"){
    sql <- glue::glue_sql("WITH {paste0('selected_', id)} AS (
      SELECT DISTINCT d.id
      FROM {sql_table} d
      LEFT JOIN options AS r ON d.id = r.link_id AND r.category = {sql_category} AND r.name = 'users_allowed_read_group'
      LEFT JOIN options AS u ON d.id = u.link_id AND u.category = {sql_category} AND u.name = 'user_allowed_read'
      WHERE 
        d.study_id = {m$selected_study} AND
        (r.value = 'everybody' OR (r.value = 'people_picker' AND u.value_num = {r$user_id}))
    )
    SELECT d.id, o.name, o.value, o.value_num
      FROM {sql_table} d
      INNER JOIN {paste0('selected_', id)} ON d.id = {paste0('selected_', id)}.id
      LEFT JOIN options o ON o.category = {sql_category} AND d.id = o.link_id", .con = con)
  }
  
  else {
    sql <- glue::glue_sql("WITH {paste0('selected_', id)} AS (
      SELECT DISTINCT d.id
      FROM {sql_table} d
      LEFT JOIN options AS r ON d.id = r.link_id AND r.category = {sql_category} AND r.name = 'users_allowed_read_group'
      LEFT JOIN options AS u ON d.id = u.link_id AND u.category = {sql_category} AND u.name = 'user_allowed_read'
      WHERE r.value = 'everybody' OR (r.value = 'people_picker' AND u.value_num = {r$user_id})
    )
    SELECT d.id, d.update_datetime, o.name, o.value, o.value_num
      FROM {sql_table} d
      INNER JOIN {paste0('selected_', id)} ON d.id = {paste0('selected_', id)}.id
      LEFT JOIN options o ON o.category = {sql_category} AND d.id = o.link_id", .con = con)
  }
  
  r[[long_var]] <- DBI::dbGetQuery(con, sql) %>% tibble::as_tibble()
  
  if (page_id == "data"){
    if (r$data_page == "patient_lvl") tab_type_id <- 1 else tab_type_id <- 2
    r[[long_var_filtered]] <- r[[long_var]] %>% dplyr::filter(tab_type_id == !!tab_type_id)
  }
  else r[[long_var_filtered]] <- r[[long_var]]
  
  sql <- glue::glue_sql("SELECT * FROM {sql_table} WHERE id IN ({unique(r[[long_var]]$id)*})", .con = con)
  r[[wide_var]] <- DBI::dbGetQuery(con, sql) %>% tibble::as_tibble()
}

#' @noRd
create_elements_ui <- function(page_id, elements, r, language, i18n){
  
  if (page_id == "data") id <- "plugins"
  else id <- page_id
  
  single_id <- switch(id, 
    "data_cleaning" = "data_cleaning",
    "datasets" = "dataset",
    "projects" = "project",
    "plugins" = "plugin", 
    "subsets" = "subset",
    "vocabularies" = "vocabulary"
  )
  
  ns <- NS(id)
  
  elements_ui <- tagList()
  
  for (i in unique(elements$id)){
    row <- elements %>% dplyr::filter(id == i)
    
    users_ui <- create_authors_ui(row %>% dplyr::filter(name == "author") %>% dplyr::pull(value))
    
    element_name <- row %>% dplyr::filter(name == paste0("name_", language)) %>% dplyr::pull(value)
    
    max_length <- 45
    if (nchar(element_name) > max_length) element_name <- paste0(substr(element_name, 1, max_length - 3), "...")
    
    # For plugins widgets, we add some content on the bottom
    
    widget_buttons <- tagList()
    short_description <- ""
    onclick <- paste0("
      Shiny.setInputValue('", page_id, "-selected_element', ", row$id, ");
      Shiny.setInputValue('", page_id, "-selected_element_trigger', Math.random());
    ")
    
    if (id %in% c("data_cleaning", "datasets", "plugins", "projects")) short_description <- row %>% dplyr::filter(name == paste0("short_description_", language)) %>% dplyr::pull(value)
    
    if (id == "plugins"){
      plugin_type <- row %>% dplyr::slice(1) %>% dplyr::pull(tab_type_id)
      widget_buttons <- get_plugin_buttons(plugin_type, i18n)
    }
    
    else if (id == "projects"){
      widget_buttons <-
        div(
          div(
            create_hover_card(
              ui = shiny.fluent::IconButton.shinyInput(ns("project_settings"), iconProps = list(iconName = "Settings"), href = shiny.router::route_link("projects")),
              text = i18n$t("set_up_project")),
            class = "small_icon_button",
            onclick = paste0(onclick, "
              Shiny.setInputValue('", page_id, "-selected_element_type', 'project_options');
              event.stopPropagation();
            ")
          ),
          div(
            create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("load_project"), iconProps = list(iconName = "Play")), text = i18n$t("load_project")),
            class = "small_icon_button"
          ),
          class = "project_widget_buttons"
        )
    }
    
    elements_ui <- tagList(
      create_element_ui(page_id, single_id, element_name, users_ui, widget_buttons, onclick, short_description),
      elements_ui
    )
  }
  
  div(elements_ui, class = paste0(id, "_container"))
}

compare_git_elements_datetimes <- function(action, i18n, local_element, git_element){
  
  # First, we compare update datetimes: is the element on remote git repo is newer than our version?
  diff_time <- difftime(local_element$update_datetime, git_element$update_datetime, unit = "mins") %>% as.integer()
  
  # No action
  # The two versions are the same or, for push, git version is newer, and for pull, local version is newer
  if (diff_time == 0) diff_time_text <- strong(tolower(i18n$t("element_git_and_local_versions_are_the_same")))
  else if (diff_time < 0 & action == "push") diff_time_text <- strong(tolower(i18n$t("element_git_version_more_recent")))
  else if (diff_time > 0 & action == "pull") diff_time_text <- strong(tolower(i18n$t("element_local_version_more_recent")))
  
  # Action
  # Then, we compare the update datetime from now (update XX time ago...)
  else {
    diff_time_now <- difftime(now(), git_element$update_datetime, unit = "mins") %>% as.integer()
    
    if (diff_time_now < 60) {
      if (diff_time_now == 1) diff_time_unit_text <- i18n$t("minute")
      else diff_time_unit_text <- i18n$t("minutes")
      diff_time_text <- tagList(strong(diff_time_now, " ", tolower(diff_time_unit_text)), " ", tolower(i18n$t("updated_x_ago")))
    }
    else if (diff_time_now < 1440) {
      diff_time_now <- difftime(now(), git_element$update_datetime, unit = "hours") %>% as.integer()
      
      if (diff_time_now == 1) diff_time_unit_text <- i18n$t("hour")
      else diff_time_unit_text <- i18n$t("hours")
      diff_time_text <- tagList(strong(diff_time_now, " ", tolower(diff_time_unit_text)), " ", tolower(i18n$t("updated_x_ago")))
    }
    else {
      diff_time_now <- difftime(now(), git_element$update_datetime, unit = "days") %>% as.integer()
      
      if (diff_time_now == 1) diff_time_unit_text <- i18n$t("day")
      else diff_time_unit_text <- i18n$t("days")
      diff_time_text <- tagList(strong(diff_time_now, " ", tolower(diff_time_unit_text)), " ", tolower(i18n$t("updated_x_ago")))
    }
  }
  
  return(list(diff_time, diff_time_text))
}

#' @noRd
get_plugin_buttons <- function(plugin_type, i18n){
  
  if (plugin_type == 1) {
    plugin_type_icon <- div(shiny.fluent::FontIcon(iconName = "Contact"), class = "small_icon_button")
    plugin_type_text <- i18n$t("patient_lvl_plugin")
  }
  else {
    plugin_type_icon <- div(shiny.fluent::FontIcon(iconName = "Group"), class = "small_icon_button")
    plugin_type_text <- i18n$t("aggregated_plugin")
  }
  
  plugin_type_icon <- create_hover_card(ui = plugin_type_icon, text = plugin_type_text)
  
  div(
    div(
      # div("R", class = "prog_label r_label"),
      # div("Python", class = "prog_label python_label"),
      # class = "plugin_widget_labels"
    ),
    plugin_type_icon,
    class = "plugin_widget_bottom"
  )
}

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
create_element_ui <- function(page_id, single_id, element_name, users_ui, widget_buttons, onclick, short_description){
  
  div(
    onclick = paste0(onclick, "Shiny.setInputValue('", page_id, "-selected_element_type', '');"),
    div(
      class = paste0(single_id, "_widget"),
      div(
        tags$h1(element_name),
        users_ui,
        div(short_description)
      ),
      widget_buttons
    )
  )
}

#' @noRd
create_element_files <- function(id, r, element_id, single_id, sql_category, element_options, element_dir){
  
  if (!dir.exists(element_dir)) dir.create(element_dir)
  
  if (id %in% c("data_cleaning", "datasets")){
    # Get dataset code
    
    sql <- glue::glue_sql("SELECT code FROM code WHERE category = {sql_category} AND link_id = {element_id}", .con = r$db)
    code <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
    
    writeLines(code, paste0(element_dir, "/code.R"))
  }
  
  # Create XML file
  create_element_xml(id, r, element_id, single_id, element_options, element_dir)
}

#' @noRd
create_element_xml <- function(id, r, element_id, single_id, element_options, element_dir){
  
  xml <- XML::newXMLDoc()
  
  xml_root <- id
  if (id == "data_cleaning") xml_root <- "data_cleaning_scripts"
  
  elements_node <- XML::newXMLNode(xml_root, doc = xml)
  element_node <- XML::newXMLNode(single_id, parent = elements_node)
  
  XML::newXMLNode("app_version", r$app_version, parent = element_node)
  
  for(name in c("unique_id", "version", "author", paste0("name_", r$languages$code), paste0("category_", r$languages$code))){
    XML::newXMLNode(name, element_options %>% dplyr::filter(name == !!name) %>% dplyr::pull(value), parent = element_node)
  }
  for(name in c(paste0("description_", r$languages$code), paste0("short_description_", r$languages$code))) XML::newXMLNode(
    name,
    element_options %>% dplyr::filter(name == !!name) %>% dplyr::pull(value),
    parent = element_node
  )
  
  element <- r[[paste0(id, "_wide")]] %>% dplyr::filter(id == element_id)
  
  for (name in c("creation_datetime", "update_datetime")) XML::newXMLNode(name, element %>% dplyr::pull(get(!!name)), parent = element_node)
  
  # Specific nodes
  if (id == "plugins")  XML::newXMLNode("type", element$tab_type_id, parent = element_node)
  else if (id == "datasets") XML::newXMLNode("omop_version", element_options %>% dplyr::filter(name == "omop_version") %>% dplyr::pull(value), parent = element_node)
  
  # Create XML file
  XML::saveXML(xml, file = paste0(element_dir, paste0("/", single_id, ".xml")))
}

#' @noRd
create_options_tibble <- function(element_id, element_name, sql_category, user_id, username, languages, last_options_id){
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
create_project_files <- function(id, r, m, single_id, element_id, element_wide, element_options, element_dir){
  
  if (!dir.exists(element_dir)) dir.create(element_dir)
  
  # Create a temp dir
  
  temp_dir <- paste0(r$app_folder, "/temp_files/", r$user_id, "/projects/", now() %>% stringr::str_replace_all(":| |-", ""), paste0(sample(c(0:9, letters[1:6]), 24, TRUE), collapse = ""))
  dir.create(temp_dir, recursive = TRUE)
  
  temp_dir_copy <- paste0(temp_dir, "/", element_options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
  if (!dir.exists(temp_dir_copy)) dir.create(temp_dir_copy, recursive = TRUE)
  
  # Export CSV files
  
  # Tables :
  # - tabs_groups
  # - tabs
  # - widgets
  # - widgets_concepts
  # - widgets_options
  # - plugins
  # - code
  # - options
  
  corresponding_ids <- list()
  for (table in c("plugins", "options", "code")) corresponding_ids[[table]] <- tibble::tibble(old_id = integer(), new_id = integer())
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
  if (nrow(data$plugins) > 0){
    
    # Create plugins files
    for (plugin_id in data$plugins$id) create_plugin_files(id = id, r = r, plugin_id = plugin_id)
    
    if (nrow(corresponding_ids$plugins) > 0) last_row <- max(corresponding_ids$plugins$new_id)
    else last_row <- 0L
    corresponding_ids$plugins <- corresponding_ids$plugins %>% dplyr::bind_rows(
      data$plugins %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = last_row + 1:dplyr::n()))
  }
  
  ### options (for plugins)
  sql <- glue::glue_sql("SELECT * FROM options WHERE category IN ('plugin', 'plugin_code') AND link_id IN ({data$plugins %>% dplyr::distinct(id) %>% dplyr::pull()*})", .con = r$db)
  data$options <- DBI::dbGetQuery(r$db, sql)
  if (nrow(data$options) > 0){
    if (nrow(corresponding_ids$options) > 0) last_row <- max(corresponding_ids$options$new_id)
    else last_row <- 0L
    corresponding_ids$options <- corresponding_ids$options %>% dplyr::bind_rows(
      data$options %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = last_row + 1:dplyr::n()))
  }
  
  ### code (for plugins)
  sql <- glue::glue_sql("SELECT * FROM code WHERE category = 'plugin' AND link_id IN ({data$options %>% dplyr::filter(category == 'plugin_code') %>% dplyr::distinct(id) %>% dplyr::pull()*})", .con = r$db)
  
  data$code <- DBI::dbGetQuery(r$db, sql)
  if (nrow(data$code) > 0){
    if (nrow(corresponding_ids$code) > 0) last_row <- max(corresponding_ids$code$new_id)
    else last_row <- 0L
    corresponding_ids$code <- corresponding_ids$code %>% dplyr::bind_rows(
      data$code %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = last_row + 1:dplyr::n()))
  }
  
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
    dplyr::relocate(link_id, .after = "person_id")

  ### plugins
  if (nrow(data$plugins) > 0) data$plugins <- 
    data$plugins %>%
    dplyr::left_join(corresponding_ids$plugins %>% dplyr::select(id = old_id, new_id), by = "id") %>%
    dplyr::select(-id) %>%
    dplyr::rename(id = new_id) %>%
    dplyr::relocate(id, .before = "name")

  ### options
  if (nrow(data$options) > 0) data$options <- 
    data$options %>%
    dplyr::left_join(corresponding_ids$options %>% dplyr::select(id = old_id, new_id), by = "id") %>%
    dplyr::left_join(corresponding_ids$plugins %>% dplyr::select(link_id = old_id, new_link_id = new_id), by = "link_id") %>%
    dplyr::select(-id, -link_id) %>%
    dplyr::rename(id = new_id, link_id = new_link_id) %>%
    dplyr::relocate(id, .before = "category") %>%
    dplyr::relocate(link_id, .before = "name")

  ### code
  if (nrow(data$code) > 0) data$code <- 
    data$code %>%
    dplyr::left_join(corresponding_ids$code %>% dplyr::select(id = old_id, new_id), by = "id") %>%
    dplyr::left_join(corresponding_ids$options %>% dplyr::select(link_id = old_id, new_link_id = new_id), by = "link_id") %>%
    dplyr::select(-id, -link_id) %>%
    dplyr::rename(id = new_id, link_id = new_link_id) %>%
    dplyr::relocate(id, .before = "category") %>%
    dplyr::relocate(link_id, .before = "code")

  # Create CSV files
  dir.create(paste0(temp_dir_copy, "/app_db"))
  for (csv_file in c(
    "tabs_groups", "tabs", "widgets", "widgets_concepts", "widgets_options",
    "plugins", "options", "code"
    )
  ) readr::write_csv(data[[csv_file]], paste0(temp_dir_copy, "/app_db/", csv_file, ".csv"))

  # Copy plugins folders
  # dir.create(paste0(temp_dir_copy, "/plugins"))
  # 
  # if (nrow(data$plugins) > 0){
  #   for (i in 1:nrow(data$plugins)){
  #     plugin <- data$plugins[i, ]
  #     unique_id <- data$options %>% dplyr::filter(category == "plugin" & name == "unique_id" & link_id == plugin$id) %>% dplyr::pull(value)
  #     
  #     plugin_dir <- paste0(r$app_folder, "/plugins/", unique_id)
  #     
  #     list_of_files <- list.files(plugin_dir)
  #     dir.create(paste0(temp_dir_copy, "/plugins/", unique_id))
  #     file.copy(paste0(plugin_dir, "/", list_of_files), paste0(temp_dir_copy, "/plugins/", unique_id, "/", list_of_files))
  #   }
  # }
  
  # Create XML file
  create_element_xml(id, r, element_id, single_id, element_options, element_dir)
  
  # Copy XML file in temp dir
  file.copy(paste0(element_dir, "/project.xml"), paste0(temp_dir_copy, "/project.xml"))
  
  # Write README.md
  readme <- element_options %>% dplyr::filter(name == "description_en") %>% dplyr::pull(value)
  writeLines(readme, paste0(element_dir, "/README.md"))
  writeLines(readme, paste0(temp_dir_copy, "/README.md"))
  
  return(temp_dir_copy)
}

#' @noRd
create_rmarkdown_file <- function(r, code, interpret_code = TRUE){
  
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