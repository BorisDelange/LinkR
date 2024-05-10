#' @noRd
load_projects_ui <- function(id, projects, projects_users, language, i18n){
  
  ns <- NS(id)
  
  projects_ui <- tagList()
  
  for (i in unique(projects$study_id)){
    
    row <- projects %>% dplyr::filter(study_id == i)
    
    personas <- list()
    study_users <- row %>% dplyr::filter(name == "user_allowed_read") %>% dplyr::distinct(value_num) %>% dplyr::pull(value_num)
    for (j in study_users){
      user <- projects_users %>% dplyr::filter(id == j)
      personas <- rlist::list.append(personas, list(personaName = user$name))
    }
    
    users_ui <- shiny.fluent::Facepile(personas = personas)
    
    project_name <- row %>% dplyr::filter(name == paste0("name_", language)) %>% dplyr::pull(value)
    
    max_length <- 45
    if (nchar(project_name) > max_length) project_name <- paste0(substr(project_name, 1, max_length - 3), "...")
    
    study_description <- "Short description of my study"
    
    projects_ui <- tagList(
      tags$a(
        href = shiny.router::route_link("data"),
        onClick = paste0("Shiny.setInputValue('", id, "-selected_project', ", i, ");"),
        div(
          class = "project_widget",
          div(
            tags$h1(project_name),
            users_ui,
            study_description
          ),
          div(
            div(
              create_hover_card(
                ui = shiny.fluent::IconButton.shinyInput(ns("project_settings"), iconProps = list(iconName = "Settings"), href = shiny.router::route_link("projects")),
                text = i18n$t("set_up_project")),
              class = "small_icon_button",
              onclick = paste0("
                Shiny.setInputValue('", id, "-selected_project_settings_trigger', Math.random());
                Shiny.setInputValue('", id, "-selected_project_settings', ", i, ");
                event.stopPropagation();
              ")
            ),
            div(
              create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("load_project"), iconProps = list(iconName = "Play")), text = i18n$t("load_project")),
              class = "small_icon_button"
            ),
            class = "project_widget_buttons"
          )
        ),
        class = "no-hover-effect"
      ),
      projects_ui
    )
  }
  
  div(projects_ui, class = "projects_container")
}
