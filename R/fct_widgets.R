#' @noRd
reload_elements_var <- function(page_id, con, r, m, long_var_filtered){
  
  if (page_id == "data") id <- "plugins"
  else id <- page_id
  
  sql_category <- switch(
    id, 
    "data_cleaning" = "script", "datasets" = "dataset", "projects" = "study", "plugins" = "plugin", 
    "subsets" = "subset", "vocabularies" = "vocabulary")
  
  sql_table <- switch(
    id, 
    "data_cleaning" = "scripts", "datasets" = "datasets", "projects" = "studies", "plugins" = "plugins", 
    "subsets" = "subsets", "vocabularies" = "vocabulary")
  
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
  
  single_id <- switch(
    id, 
    "data_cleaning" = "data_cleaning", "datasets" = "dataset", "projects" = "project", "plugins" = "plugin", 
    "subsets" = "subset", "vocabularies" = "vocabulary")
  
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
      Shiny.setInputValue('", page_id, "-selected_element', ", i, ");
      Shiny.setInputValue('", page_id, "-selected_element_trigger', Math.random());
    ")
    
    if (id == "plugins"){
      
      plugin_type <- row %>% dplyr::slice(1) %>% dplyr::pull(tab_type_id)
      get_plugin_buttons(plugin_type, i18n)
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
  
  widget_buttons <-
    div(
      div(
        div("R", class = "prog_label r_label"),
        div("Python", class = "prog_label python_label"),
        class = "plugin_widget_labels"
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
  
  users_ui <- shiny.fluent::Facepile(personas = personas)
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