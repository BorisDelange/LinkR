#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  
  if (id == "home") page <- "home" else page <- substr(id, 6, nchar(id))
  
  div(class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    div(
      shiny.fluent::Breadcrumb(items = list(list(key = page, text = i18n$t("projects")))), br(),
      # shiny.fluent::Stack(
        # horizontal = TRUE, tokens = list(childrenGap = 18),
        div(shiny.fluent::SearchBox.shinyInput(ns("search_project")), style = "width:370px; margin-left:10px;"),
        # div(shiny.fluent::PrimaryButton.shinyInput(ns("add_project"), text = i18n$t("create_project"))),
      # ), 
      br(),
      uiOutput(ns("projects")),
      style = "margin-left:20px;"
    ),
    br()#,
    # tags$script(HTML(paste0(
    #   "var timeoutId;",
    #   "document.getElementById('", id, "-bd').addEventListener('mouseover', function(e) {",
    #   "  clearTimeout(timeoutId);",
    #   "  var tooltip = document.getElementById('", id, "-bd-tooltip');",
    #   "  timeoutId = setTimeout(function() {",
    #   "    tooltip.style.visibility = 'visible';",
    #   "    tooltip.style.left = (e.pageX - 15) + 'px';",
    #   "    tooltip.style.top = (e.pageY + 15) + 'px';",
    #   "  }, 500)",
    #   "});",
    #   "document.getElementById('", id, "-bd').addEventListener('mouseout', function() {",
    #   "  document.getElementById('", id, "-bd-tooltip').style.visibility = 'hidden';",
    #   "  clearTimeout(timeoutId);",
    #   "});"
    # )))
  )
}

#' home Server Functions
#'
#' @noRd 
mod_home_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), 
    language = "en", i18n = character(), perf_monitoring = FALSE, debug = FALSE, show_home_page = TRUE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if(perf_monitoring) monitor_perf(r = r, action = "start")
    if (debug) cat(paste0("\n", now(), " - mod_home - ", id, " - start"))
    
    if (id == "home") page <- "home" else page <- substr(id, 6, nchar(id))
    
    observeEvent(input$current_tab, {
      
      if (debug) cat(paste0("\n", now(), " - mod_home - ", id, " - observer input$current_tab"))
      
      sapply(cards %>% setdiff(., input$current_tab), shinyjs::hide)
      shinyjs::show(input$current_tab)
    })
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    observeEvent(input$help, {
      if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r[[paste0("help_home_", id, "_open_panel")]] <- TRUE
      else if (id == "home" & shiny.router::get_page() == "/") r$help_home_home_open_panel <- TRUE
    })
    observeEvent(input$hide_panel, r[[paste0("help_home_", id, "_open_panel")]] <- FALSE)
    
    # observeEvent(shiny.router::get_page(), {
    #   if (debug) cat(paste0("\n", now(), " - mod_home - ", id, " - observer shiny_router::change_page"))
    # 
    #   # Close help pages when page changes
    #   r[[paste0("help_home_", id, "_open_panel")]] <- FALSE
    #   r[[paste0("help_home_", id, "_open_modal")]] <- FALSE
    # })
    
    r[[paste0("help_home_", id, "_open_panel_light_dismiss")]] <- TRUE
    observeEvent(input$show_modal, r[[paste0("help_home_", id, "_open_modal")]] <- TRUE)
    observeEvent(input$hide_modal, {
      r[[paste0("help_home_", id, "_open_modal")]] <- FALSE
      r[[paste0("help_home_", id, "_open_panel_light_dismiss")]] <- TRUE
    })
    
    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_home_", id, "_page_", i)]] <- now())
    })
    
    help_home(output = output, r = r, id = id, language = language, i18n = i18n, ns = ns)
    
    # --- --- --- --- --
    # Show projects ----
    # --- --- --- --- --
    
    r$reload_projects_data <- now()
    
    observeEvent(r$reload_projects_data, {
      if (debug) cat(paste0("\n", now(), " - mod_home - ", id, " - observer r$reload_projects_data"))
      
      sql <- glue::glue_sql("WITH selected_studies AS (
        SELECT DISTINCT s.id
        FROM studies s
        LEFT JOIN options AS r ON s.id = r.link_id AND r.category = 'study' AND r.name = 'users_allowed_read_group'
        LEFT JOIN options AS u ON s.id = u.link_id AND u.category = 'study' AND u.name = 'user_allowed_read'
        --WHERE (r.value = 'everybody' OR (r.value = 'people_picker' AND u.value_num = {r$user_id}))
        WHERE r.value = 'people_picker' AND u.value_num = {r$user_id}
          AND s.deleted IS FALSE
      )
      SELECT s.id AS study_id, o.*
        FROM studies s
        INNER JOIN selected_studies ON s.id = selected_studies.id
        LEFT JOIN options o ON o.category = 'study' AND s.id = o.link_id", .con = r$db)
      r$projects <- DBI::dbGetQuery(r$db, sql)
      r$filtered_projects <- r$projects
      
      sql <- glue::glue_sql("SELECT id, CONCAT(firstname, ' ', lastname) AS name, CONCAT(SUBSTRING(firstname, 1, 1), SUBSTRING(lastname, 1, 1)) AS initials FROM users", .con = r$db)
      r$projects_users <- DBI::dbGetQuery(r$db, sql)
      
      # Load r$studies (to work with old version)
      sql <- glue::glue_sql("SELECT * FROM studies WHERE id IN ({unique(r$projects$study_id)*})", .con = r$db)
      r$studies <- DBI::dbGetQuery(r$db, sql)
      
      r$reload_projects_list <- now()
    })
    
    observeEvent(input$search_project, {
      if (debug) cat(paste0("\n", now(), " - mod_home - ", id, " - observer input$search_project"))
      
      if (input$search_project == "") r$filtered_projects <- r$projects
      else {
        filtered_ids <- r$projects %>% dplyr::filter(name == paste0("name_", language) & grepl(tolower(input$search_project), tolower(value)))
        r$filtered_projects <- r$projects %>% dplyr::filter(study_id %in% filtered_ids)
      }
      
      r$reload_projects_list <- now()
    })
    
    observeEvent(r$reload_projects_list, {
      if (debug) cat(paste0("\n", now(), " - mod_home - ", id, " - observer r$reload_projects_list"))
      
      # Filter projects with search box
      
      projects <- r$filtered_projects
      
      projects_ui <- tagList()
      
      for (i in unique(projects$study_id)){
        row <- projects %>% dplyr::filter(study_id == i)
        
        users_ui <- tagList()
        study_users <- row %>% dplyr::filter(name == "user_allowed_read") %>% dplyr::distinct(value_num) %>% dplyr::pull(value_num)
        for (j in study_users){
          user <- r$projects_users %>% dplyr::filter(id == j)
          users_ui <- tagList(users_ui, 
            div(
              id = ns(paste0("study_", i, "_user_", j)),
              user$initials,
              tags$span(
                id = ns("bd-tooltip"),
                style = "visibility:hidden; white-space:nowrap; position:fixed; background-color:white; color:black; padding:5px; border-radius:5px; box-shadow:0 5px 15px rgba(0,0,0,0.3); font-size:12px; font-weight:normal; z-index:1;",
                user$name
              ),
              style = "position:relative; width:25px; height:25px; border-radius:50%; color:white; background-color:#95a5a6; font-size:12px; font-weight:bold; display:flex; justify-content:center; align-items:center;"
            )
          )
        }
        
        project_name <- row %>% dplyr::filter(name == paste0("name_", language)) %>% dplyr::pull(value)
        max_length <- 45
        if (nchar(project_name) > max_length) project_name <- paste0(substr(project_name, 1, max_length - 3), "...")
        
        projects_ui <- tagList(
          tags$a(
            href = shiny.router::route_link("patient_level_data"),
            onClick = paste0("Shiny.setInputValue('", id, "-selected_project', ", i, ", {priority: 'event'});"),
            div(
              span(project_name, style = "font-weight:bold; font-size:16px;"),
              div(
                users_ui,
                style = "display:flex; gap:5px; margin-top:10px;"
              ),
              div(
                "Short description of my study",
                style = "margin-top:10px;"
              ),
              style = "width:350px; height:165px; box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19); margin:10px; padding:10px;"
            ),
            class = "no-hover-effect"
          ),
          projects_ui
        )
      }
      
      projects_ui <- div(projects_ui, style = "display:flex; flex-wrap:wrap; justify-content:flex-start; gap:15px;")
      
      output$projects <- renderUI(projects_ui)
    })
    
    observeEvent(input$selected_project, {
      if (debug) cat(paste0("\n", now(), " - mod_home - ", id, " - observer input$selected_project"))
      
      m$selected_study <- input$selected_project
    })
    
    if (debug) cat(paste0("\n", now(), " - mod_home - ", id, " - end"))
  })
}
