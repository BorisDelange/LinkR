#' @noRd
mod_home_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  
  if (id == "home") page <- "home" else page <- substr(id, 6, nchar(id))
  
  div(
    class = "main",
    style = "margin-top: 20px;",
    div(tags$a(i18n$t("projects"), href = shiny.router::route_link("projects"), class = "home_link"), style = "margin-bottom: 10px;"),
    uiOutput(ns("projects"))#,
    # shiny.fluent::PrimaryButton.shinyInput(ns("show_all_projects"), i18n$t("show_all_projects"), href = shiny.router::route_link("projects"))
  )
}

#' @noRd 
mod_home_server <- function(id, r, d, m, language, i18n, debug){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (debug) cat(paste0("\n", now(), " - mod_home - ", id, " - start"))
    
    # --- --- --- -
    # Projects ----
    # --- --- --- -

    r$reload_projects_data <- now()

    ## Reload projects data ----
    observeEvent(r$reload_projects_data, {
      if (debug) cat(paste0("\n", now(), " - mod_home - ", id, " - observer r$reload_projects_data"))

      sql <- glue::glue_sql("WITH selected_studies AS (
        SELECT DISTINCT s.id
        FROM studies s
        LEFT JOIN options AS r ON s.id = r.link_id AND r.category = 'study' AND r.name = 'users_allowed_read_group'
        LEFT JOIN options AS u ON s.id = u.link_id AND u.category = 'study' AND u.name = 'user_allowed_read'
        WHERE (r.value = 'people_picker' AND u.value_num = {r$user_id}) OR r.value = 'everybody'
      )
      SELECT s.id AS study_id, s.update_datetime, o.*
        FROM studies s
        INNER JOIN selected_studies ON s.id = selected_studies.id
        LEFT JOIN options o ON o.category = 'study' AND s.id = o.link_id", .con = r$db)
      r$projects_long <- DBI::dbGetQuery(r$db, sql)
      r$filtered_projects_long <- r$projects_long

      sql <- glue::glue_sql("SELECT id, CONCAT(firstname, ' ', lastname) AS name, CONCAT(SUBSTRING(firstname, 1, 1), SUBSTRING(lastname, 1, 1)) AS initials FROM users", .con = r$db)
      r$projects_users <- DBI::dbGetQuery(r$db, sql)
      
      sql <- glue::glue_sql("SELECT * FROM studies WHERE id IN ({unique(r$projects_long$study_id)*})", .con = r$db)
      r$projects_wide <- DBI::dbGetQuery(r$db, sql)

      r$reload_projects_list <- now()
    })

    ## Reload projects widgets -----
    observeEvent(r$reload_projects_list, {
      if (debug) cat(paste0("\n", now(), " - mod_home - ", id, " - observer r$reload_projects_list"))
      
      projects_ids <- r$filtered_projects_long %>% dplyr::distinct(study_id) %>% dplyr::slice(1:3) %>% dplyr::pull()
      projects <- r$filtered_projects_long %>% dplyr::filter(study_id %in% projects_ids)
      projects_ui <- load_projects_ui(id, projects, r$projects_users, language, i18n)
      output$projects <- renderUI(projects_ui)

      # Unlock reactivity
      shinyjs::show("projects")
    })

    ## A project is selected ----
    
    observeEvent(input$selected_project, {
      if (debug) cat(paste0("\n", now(), " - mod_home - observer input$selected_project"))

      # Load data & concept pages if not already loaded
      # If not already loaded, project is loaded after data pages server side is loaded
      # Else, project is loaded directly
      # Delay to change page before executing server

      if (length(r$loaded_pages$data) == 0){
        r$load_page <- "data"
        r$data_page <- "patient_lvl"
      }
      else r$load_project_trigger <- now()
    })

    # Trigger to load a project
    observeEvent(r$load_project_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_home - observer r$load_project_trigger"))

      req(length(input$selected_project) > 0)

      # shiny.router::change_page("patient_level_data")
      shinyjs::delay(500, m$selected_study <- input$selected_project)
    })
    
    # Go to project settings
    observeEvent(input$selected_project_settings_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_home - observer input$selected_project_settings_trigger"))
      
      r$selected_project_settings <- input$selected_project_settings
      r$selected_project_settings_trigger <- now()
    })
  })
}
