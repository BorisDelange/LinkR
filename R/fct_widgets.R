#' @noRd
reload_elements_var <- function(page_id, con, r, m, long_var_filtered){
  
  if (page_id == "data") id <- "plugins"
  else id <- page_id
  
  sql_category <- switch(id, 
    "data_cleaning" = "script",
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
    
    users_ui <- create_users_ui(row, r$users)
    
    element_name <- row %>% dplyr::filter(name == paste0("name_", language)) %>% dplyr::pull(value)
    
    max_length <- 45
    if (nchar(element_name) > max_length) element_name <- paste0(substr(element_name, 1, max_length - 3), "...")
    
    # For plugins widgets, we add some content on the bottom
    
    widget_buttons <- tagList()
    onclick <- paste0("
      Shiny.setInputValue('", page_id, "-selected_element', ", row$id, ");
      Shiny.setInputValue('", page_id, "-selected_element_trigger', Math.random());
    ")
    
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
      create_element_ui(page_id, single_id, element_name, users_ui, widget_buttons, onclick),
      elements_ui
    )
  }
  
  div(elements_ui, class = paste0(id, "_container"))
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
create_users_ui <- function(row, users){
  
  personas <- list()
  element_users <- row %>% dplyr::filter(name == "user_allowed_read") %>% dplyr::distinct(value_num) %>% dplyr::pull(value_num)
  for (j in element_users){
    user <- users %>% dplyr::filter(id == j)
    personas <- rlist::list.append(personas, list(personaName = user$name))
  }
  
  shiny.fluent::Facepile(personas = personas)
}

#' @noRd
create_element_ui <- function(page_id, single_id, element_name, users_ui, widget_buttons, onclick){
  
  div(
    onclick = paste0(onclick, "Shiny.setInputValue('", page_id, "-selected_element_type', '');"),
    div(
      class = paste0(single_id, "_widget"),
      div(
        tags$h1(element_name),
        users_ui,
        div(paste0("Short description of my ", single_id))
      ),
      widget_buttons
    )
  )
}

#' @noRd
create_element_files <- function(id, r, element_id, single_id, element_options, element_dir){
  
  if (!dir.exists(element_dir)) dir.create(element_dir)
  
  if (id == "datasets"){
    # Get dataset code
    
    sql <- glue::glue_sql("SELECT code FROM code WHERE category = {single_id} AND link_id = {element_id}", .con = r$db)
    code <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
    
    writeLines(code, paste0(element_dir, "/code.R"))
  }
  
  # Create XML file
  create_element_xml(id, r, element_id, single_id, element_options, element_dir)
}

#' @noRd
create_element_xml <- function(id, r, element_id, single_id, element_options, element_dir){
  
  xml <- XML::newXMLDoc()
  
  elements_node <- XML::newXMLNode(id, doc = xml)
  element_node <- XML::newXMLNode(single_id, parent = elements_node)
  
  XML::newXMLNode("app_version", r$app_version, parent = element_node)
  
  for(name in c("unique_id", "version", "author", paste0("name_", r$languages$code), paste0("category_", r$languages$code))){
    XML::newXMLNode(name, element_options %>% dplyr::filter(name == !!name) %>% dplyr::pull(value), parent = element_node)
  }
  for(name in c(paste0("description_", r$languages$code))) XML::newXMLNode(
    name,
    element_options %>% dplyr::filter(name == !!name) %>% dplyr::pull(value) %>% stringr::str_replace_all("''", "'"),
    parent = element_node
  )
  
  element <- r[[paste0(id, "_wide")]] %>% dplyr::filter(id == element_id)
  
  for (name in c("creation_datetime", "update_datetime")) XML::newXMLNode(name, element %>% dplyr::pull(get(!!name)), parent = element_node)
  
  # Specific nodes
  if (id == "plugins")  XML::newXMLNode("type", element$tab_type_id, parent = element_node)
  
  # Create XML file
  XML::saveXML(xml, file = paste0(element_dir, paste0("/", single_id, ".xml")))
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
create_rmarkdown_file <- function(r, code){
  
  code <- gsub("\r", "\n", code)
  
  # Create temp dir
  dir <- paste0(r$app_folder, "/temp_files/", r$user_id, "/markdowns")
  output_file <- paste0(dir, "/", paste0(sample(c(0:9, letters[1:6]), 8, TRUE), collapse = ''), "_", now() %>% stringr::str_replace_all(":", "_") %>% stringr::str_replace_all(" ", "_"), ".Md")
  if (!dir.exists(dir)) dir.create(dir)
  
  # Create the markdown file
  knitr::knit(text = code, output = output_file, quiet = TRUE)
  
  return(output_file)
}