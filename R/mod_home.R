#' @noRd
mod_home_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  
  if (id == "home") page <- "home" else page <- substr(id, 6, nchar(id))
  
  div(class = "main",
    render_settings_default_elements(ns = ns),
    div(
      div(shiny.fluent::Breadcrumb(items = list(list(key = page, text = i18n$t("projects")))), style = "margin-left:10px"),
      div(shiny.fluent::SearchBox.shinyInput(ns("search_project")), style = "width:320px; margin:10px 0 0 10px;"),
      uiOutput(ns("projects"))
    )
  )
}

#' @noRd 
mod_home_server <- function(id, r, d, m, language, i18n, debug){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (debug) cat(paste0("\n", now(), " - mod_home - ", id, " - start"))
    
    # --- --- --- --- --
    # Show projects ----
    # --- --- --- --- --
    
    r$reload_projects_data <- now()
    
    # Reload projects data
    observeEvent(r$reload_projects_data, {
      if (debug) cat(paste0("\n", now(), " - mod_home - ", id, " - observer r$reload_projects_data"))
      
      sql <- glue::glue_sql("WITH selected_studies AS (
        SELECT DISTINCT s.id
        FROM studies s
        LEFT JOIN options AS r ON s.id = r.link_id AND r.category = 'study' AND r.name = 'users_allowed_read_group'
        LEFT JOIN options AS u ON s.id = u.link_id AND u.category = 'study' AND u.name = 'user_allowed_read'
        WHERE r.value = 'people_picker' AND u.value_num = {r$user_id} AND s.deleted IS FALSE
      )
      SELECT s.id AS study_id, s.update_datetime, o.*
        FROM studies s
        INNER JOIN selected_studies ON s.id = selected_studies.id
        LEFT JOIN options o ON o.category = 'study' AND s.id = o.link_id", .con = r$db)
      r$projects_long <- DBI::dbGetQuery(r$db, sql)
      r$filtered_projects_long <- r$projects_long
      
      sql <- glue::glue_sql("SELECT id, CONCAT(firstname, ' ', lastname) AS name, CONCAT(SUBSTRING(firstname, 1, 1), SUBSTRING(lastname, 1, 1)) AS initials FROM users", .con = r$db)
      r$projects_users <- DBI::dbGetQuery(r$db, sql)
      
      # Load r$studies (to work with old version)
      sql <- glue::glue_sql("SELECT * FROM studies WHERE id IN ({unique(r$projects_long$study_id)*})", .con = r$db)
      r$projects_wide <- DBI::dbGetQuery(r$db, sql)
      r$studies <- r$projects_wide
      
      r$reload_projects_list <- now()
    })
    
    # Search a project
    observeEvent(input$search_project, {
      if (debug) cat(paste0("\n", now(), " - mod_home - ", id, " - observer input$search_project"))
      
      if (input$search_project == "") r$filtered_projects_long <- r$projects_long
      else {
        filtered_ids <- r$projects_long %>% dplyr::filter(name == paste0("name_", language) & grepl(tolower(input$search_project), tolower(value)))
        r$filtered_projects_long <- r$projects_long %>% dplyr::filter(study_id %in% filtered_ids)
      }
      
      r$reload_projects_list <- now()
    })
    
    # Reload projects list
    observeEvent(r$reload_projects_list, {
      if (debug) cat(paste0("\n", now(), " - mod_home - ", id, " - observer r$reload_projects_list"))
      
      # Filter projects with search box
      
      projects <- r$filtered_projects_long
      
      projects_ui <- tagList()
      
      for (i in unique(projects$study_id)){
        row <- projects %>% dplyr::filter(study_id == i)
        
        personas <- list()
        study_users <- row %>% dplyr::filter(name == "user_allowed_read") %>% dplyr::distinct(value_num) %>% dplyr::pull(value_num)
        for (j in study_users){
          user <- r$projects_users %>% dplyr::filter(id == j)
          personas <- rlist::list.append(personas, list(personaName = user$name))
        }
        
        users_ui <- shiny.fluent::Facepile(personas = personas)
        
        project_name <- row %>% dplyr::filter(name == paste0("name_", language)) %>% dplyr::pull(value)
        
        max_length <- 45
        if (nchar(project_name) > max_length) project_name <- paste0(substr(project_name, 1, max_length - 3), "...")
        
        study_description <- "Short description of my study"
        if (i == 10) study_description <- "Etude du pronostic des patients avec pneumopathies graves admis en réanimation"
        else if (i == 17) study_description <- "Dashboard d'indicateurs de qualité des soins en réanimation"
        
        projects_ui <- tagList(
          tags$a(
            href = shiny.router::route_link("patient_level_data"),
            onClick = paste0("Shiny.setInputValue('", id, "-selected_project', ", i, ", {priority: 'event'});"),
            div(
              class = "project_card",
              div(
                tags$h1(project_name),
                users_ui,
                study_description
              )
            ),
            class = "no-hover-effect"
          ),
          projects_ui
        )
      }
      
      projects_ui <- div(projects_ui, class = "projects_container")
      
      output$projects <- renderUI(projects_ui)
      
      # Unlock reactivity
      shinyjs::show("projects")
    })
    
    # A project is selected
    observeEvent(input$selected_project, {
      if (debug) cat(paste0("\n", now(), " - mod_home - observer input$selected_project"))
      
      # Load data & cencept pages if not already loaded
      # If not already loaded, project is loaded after data pages server side is loaded
      # Else, project is loaded directly
      if (length(r$loaded_pages$patient_level_data) == 0) r$load_page <- "patient_level_data"
      else r$load_project_trigger <- now()
    })
    
    # Trigger to load a project
    observeEvent(r$load_project_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_home - observer r$load_project_trigger"))
      
      req(length(input$selected_project) > 0)
      
      m$selected_study <- input$selected_project
    })
    
    if (debug) cat(paste0("\n", now(), " - mod_home - ", id, " - end"))
  })
}
