#' page_header UI Function
#'
#' @description A shiny Module.
#'
#' @param id ID of current page (character)
#' @param language Language used (character)
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
mod_page_header_ui <- function(id = character(), i18n = character()){
  
  ns <- NS(id)
  
  header_command_bar_style <- ""
  
  if (id %in% c("home", "log", "plugins", "projects")) header_command_bar_style <- "margin-left: 10px;"
  
  command_bar_1 <- div(
    id = ns("command_bar_1_div"),
    shiny.fluent::CommandBar(
      items = list(
        # Configure
        shiny.fluent::CommandBarItem(text = "", "Settings",
          subMenuProps = list(items = list(
            list(text = i18n$t("projects"), iconProps = list(iconName = "CustomList"), href = shiny.router::route_link("projects")),
            list(text = i18n$t("datasets"), iconProps = list(iconName = "OfflineStorage"), href = shiny.router::route_link("datasets")),
            list(text = i18n$t("vocabularies"), iconProps = list(iconName = "AllApps"), href = shiny.router::route_link("vocabularies"))
          )),
          title = i18n$t("configure")
        ),
        # Develop
        shiny.fluent::CommandBarItem("", "CodeEdit",
          subMenuProps = list(items = list(
            list(text = i18n$t("console"), iconProps = list(iconName = "Embed"), href = shiny.router::route_link("console")),
            list(text = i18n$t("plugins"), iconProps = list(iconName = "Code"), href = shiny.router::route_link("plugins")),
            list(text = i18n$t("data_cleaning"), iconProps = list(iconName = "CodeEdit"), href = shiny.router::route_link("data_cleaning"))
          )),
          title = i18n$t("develop")
        ),
        # Discover
        shiny.fluent::CommandBarItem("", "World", title = i18n$t("explore"), href = shiny.router::route_link("git_repos"))
      )
    ),
    # id = ns("header_command_bar"),
    class = "header_command_bar",
    style = header_command_bar_style
  )
  
  settings_div <- div(
    shiny.fluent::CommandBar(
      items = list(
        shiny.fluent::CommandBarItem(
          "", "Settings",
          subMenuProps = list(items = list(
            list(text = i18n$t("users"), iconProps = list(iconName = "Contact"), href = shiny.router::route_link("users")),
            list(text = i18n$t("app_db"), iconProps = list(iconName = "OfflineStorage"), href = shiny.router::route_link("app_db")),
            list(text = i18n$t("log"), iconProps = list(iconName = "KnowledgeArticle"), href = shiny.router::route_link("log"))
          )),
          title = i18n$t("app_settings")
        )
      )
    ),
    class = "header_command_bar"
  )
  
  command_bar_2 <- tagList(
    tags$a(
      id = ns("command_bar_2_a"),
      href = shiny.router::route_link("projects"),
      div(
        shinyjs::hidden(
          div(
            id = ns("selected_project_div"),
            uiOutput(ns("selected_project")), 
            class = "selected_project"
          )
        ), 
        class = "selected_project_container"
      ),
      style = "z-index:200; text-decoration:none; margin-left:-28px;",
    ),
    div(
      id = ns("command_bar_2_div"),
      shiny.fluent::CommandBar(
        items = list(
          # Explore
          shiny.fluent::CommandBarItem(
            "", "BIDashboard",
            subMenuProps = list(items = list(
              list(text = i18n$t("concepts"), iconProps = list(iconName = "AllApps"), href = shiny.router::route_link("concepts")),
              list(text = i18n$t("patient_lvl_data"), iconProps = list(iconName = "Contact"), href = shiny.router::route_link("data?type=patient_lvl")),
              list(text = i18n$t("aggregated_data"), iconProps = list(iconName = "People"), href = shiny.router::route_link("data?type=aggregated"))
            )),
            title = i18n$t("explore_data")
          ),
          # Configure
          shiny.fluent::CommandBarItem(
            "", "Settings",
            subMenuProps = list(items = list(
              list(text = i18n$t("subsets"), iconProps = list(iconName = "People"), href = shiny.router::route_link("subsets"))
            )),
            title = i18n$t("configure_project")
          ),
          # Messages
          shiny.fluent::CommandBarItem("", "Chat", href = shiny.router::route_link("project_messages"), title = i18n$t("messages")),
          # Console
          shiny.fluent::CommandBarItem("", "Code", href = shiny.router::route_link("project_console"), title = i18n$t("console")),
          # Tasks
          shiny.fluent::CommandBarItem("", "CheckList", href = shiny.router::route_link("tasks"), title = i18n$t("tasks"))
        )
      ),
      class = "header_command_bar",
      style = "display:inline-block; margin-left:8px;"
    )
  )
  
  if (id %in% c("app_db", "home", "console", "datasets", "data_cleaning", "git_repos",
    "log", "plugins", "projects", "users", "vocabularies")) command_bar_2 <- shinyjs::hidden(command_bar_2)
  else command_bar_1 <- shinyjs::hidden(command_bar_1)
  
  tagList(
    div(
      class = "header",
      div(
        class = "header_left_bar",
        div(
          tags$img(src = "www/logo.png"),
          class = "logo",
          onclick = paste0("window.location.href='", shiny.router::route_link("/"), "';")
        ),
        div(
          command_bar_1,
          command_bar_2,
          uiOutput(ns("current_page"), class = "current_page"),
          div(class = "message_bars", uiOutput(ns("message_bar"))),
          style = "display:flex;"
        )
      ),
      div(class = "header_right_bar",
        #   uiOutput(ns("user")),
        #   div(uiOutput(ns("username")), style = "font-weight:bold; padding: 12px 10px 0px 0px;"),
        # )
        settings_div
      )
    )
  )
}

#' @noRd 
mod_page_header_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), language = "en", i18n = character(), debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # output$username <- renderUI(r$username)
    output$user <- renderUI(
      tags$a(
        div(
          r$user$initials,
          style = "position:relative; width:25px; height:25px; border-radius:50%; color:white; background-color:#95a5a6; font-size:12px; font-weight:bold; display:flex; justify-content:center; align-items:center;"
        ),
        class = "no-hover-effect"
      )
    )
    
    # Show current page
    
    if (id == "data"){
      observeEvent(shiny.router::get_page(), {
        if (debug) cat(paste0("\n", now(), " - mod_page_header (", id, ") - observer shiny.router::get_page()"))
        output$current_page <- renderUI(i18n$t(paste0(r$data_page, "_data")))
      })
    }
    else if (id == "project_messages") output$current_page <- renderUI(i18n$t("messages"))
    else output$current_page <- renderUI(i18n$t(id))
    
    # Selected project
    
    if (id %in% c("concepts", "data", "projects", "project_messages", "project_console", "subsets", "tasks")){
      
      observeEvent(m$selected_study, {
        if (debug) cat(paste0("\n", now(), " - mod_page_header (", id, ") - observer m$selected_study"))
        
        project_name <- r$projects_long %>% dplyr::filter(id == m$selected_study, name == paste0("name_", language)) %>% dplyr::pull(value)
        project_name_short <- project_name
        max_length <- 27
        if (nchar(project_name_short) > max_length) project_name_short <- paste0(substr(project_name_short, 1, max_length - 3), "...")
        output$selected_project <- renderUI(create_hover_card(ui = project_name_short, text = project_name))
      })
    }
  })
}