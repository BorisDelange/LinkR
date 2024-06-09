reload_elements_var <- function(id, con, r){
  
  sql_category <- switch(
    id, 
    "data_cleaning" = "script", "datasets" = "dataset", "projects" = "study", "plugins" = "plugin", 
    "subsets" = "subset", "vocabularies" = "vocabulary")
  
  sql_table <- switch(
    id, 
    "data_cleaning" = "scripts", "datasets" = "datasets", "projects" = "studies", "plugins" = "plugins", 
    "subsets" = "subsets", "vocabularies" = "vocabulary")
  
  long_var <- paste0(id, "_long")
  long_var_filtered <- paste0("filtered_", id, "_long")
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
  r[[long_var_filtered]] <- r[[long_var]]
  
  sql <- glue::glue_sql("SELECT * FROM {sql_table} WHERE id IN ({unique(r[[long_var]]$id)*})", .con = con)
  r[[wide_var]] <- DBI::dbGetQuery(con, sql) %>% tibble::as_tibble()
}