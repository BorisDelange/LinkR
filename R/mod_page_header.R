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
mod_page_header_ui <- function(id, language, i18n){
  
  ns <- NS(id)
  
  command_bar_1 <- div(
    id = ns("command_bar_1_div"),
    shiny.fluent::CommandBar(
      items = list(
        
        # Main pages
        shiny.fluent::CommandBarItem(
          "", "OpenFolderHorizontal", title = i18n$t("projects"), href = shiny.router::route_link("projects"),
          onClick = htmlwidgets::JS(paste0("item => {Shiny.setInputValue('", id, "-show_home', Math.random());}"))
        ),
        shiny.fluent::CommandBarItem("", "Code", title = i18n$t("console"), href = shiny.router::route_link("console")),
        shiny.fluent::CommandBarItem("", "World", title = i18n$t("content_catalog"), href = shiny.router::route_link("git_repos")),
        
        # Other pages
        shiny.fluent::CommandBarItem(
          text = "", "More",
          subMenuProps = list(items = list(
            list(
              text = i18n$t("plugins"), iconProps = list(iconName = "Puzzle"), href = shiny.router::route_link("plugins"),
              onClick = htmlwidgets::JS(paste0("item => {Shiny.setInputValue('", id, "-show_home', Math.random());}"))
            ),
            list(
              text = i18n$t("datasets"), iconProps = list(iconName = "OfflineStorage"), href = shiny.router::route_link("datasets"),
              onClick = htmlwidgets::JS(paste0("item => {Shiny.setInputValue('", id, "-show_home', Math.random());}"))
            ),
            list(
              text = i18n$t("vocabularies"), iconProps = list(iconName = "LocaleLanguage"), href = shiny.router::route_link("vocabularies"),
              onClick = htmlwidgets::JS(paste0("item => {Shiny.setInputValue('", id, "-show_home', Math.random());}"))
            ),
            list(
              text = i18n$t("data_cleaning"), iconProps = list(iconName = "Broom"), href = shiny.router::route_link("data_cleaning"),
              onClick = htmlwidgets::JS(paste0("item => {Shiny.setInputValue('", id, "-show_home', Math.random());}"))
            )
          )),
          title = i18n$t("other_pages")
        )
      )
    ),
    class = "header_command_bar header_command_bar_1"
  )
  
  command_bar_2 <- tagList(
    tags$a(
      id = ns("command_bar_2_link"),
      href = shiny.router::route_link("projects"),
      div(
        div(
          id = ns("selected_project_div"),
          uiOutput(ns("selected_project")), 
          class = "selected_project"
        ), 
        class = "selected_project_container"
      ),
      style = "z-index:200; text-decoration:none;",
    ),
    div(
      id = ns("command_bar_2_div"),
      shiny.fluent::CommandBar(
        items = list(
          shiny.fluent::CommandBarItem("", "Contact", href = shiny.router::route_link("data?type=patient_lvl"), title = i18n$t("patient_lvl_data")),
          shiny.fluent::CommandBarItem("", "People", href = shiny.router::route_link("data?type=aggregated"), title = i18n$t("aggregated_data")),
          shiny.fluent::CommandBarItem("", "AllApps", href = shiny.router::route_link("concepts"), title = i18n$t("concepts")),
          shiny.fluent::CommandBarItem(
            "", "More",
            subMenuProps = list(items = list(
              list(
                text = i18n$t("subsets"), iconProps = list(iconName = "MapLayers"), href = shiny.router::route_link("subsets"),
                onClick = htmlwidgets::JS(paste0("item => {Shiny.setInputValue('", id, "-show_home', Math.random());}"))
              ),
              list(text = i18n$t("project_files"), iconProps = list(iconName = "Documentation"), href = shiny.router::route_link("project_files"))#,
              # list(text = i18n$t("tasks"), iconProps = list(iconName = "CheckList"), href = shiny.router::route_link("tasks"))
            )),
            title = i18n$t("other_pages")
          )
        )
      ),
      class = "header_command_bar",
      style = "display:inline-block; margin-left:8px;"
    )
  )
  
  if (id %in% c(
    "app_db", "home", "console", "datasets", "data_cleaning", "git_repos",
    "log", "plugins", "projects", "user_settings", "users", "vocabularies")) command_bar_2 <- shinyjs::hidden(command_bar_2)
  
  tagList(
    div(
      class = "header",
      div(
        class = "header_left_bar",
        div(
          tags$img(src = "www/images/logo.png"),
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
      div(
        class = "header_right_bar",
        div(
          shiny.fluent::CommandBar(
            items = list(
              shiny.fluent::CommandBarItem("", "Help", onClick = htmlwidgets::JS(paste0("item => { Shiny.setInputValue('", id, "-open_help_modal', Math.random()); }")), title = i18n$t("help")),
              shiny.fluent::CommandBarItem(
                "", "Settings",
                subMenuProps = list(items = list(
                  list(text = i18n$t("user_settings"), iconProps = list(iconName = "Settings"), href = shiny.router::route_link("user_settings")),
                  list(text = i18n$t("users"), iconProps = list(iconName = "People"), href = shiny.router::route_link("users")),
                  list(text = i18n$t("app_db"), iconProps = list(iconName = "OfflineStorage"), href = shiny.router::route_link("app_db")),
                  list(text = i18n$t("log"), iconProps = list(iconName = "KnowledgeArticle"), href = shiny.router::route_link("log"))
                )),
                title = i18n$t("app_settings")
              )
            )
          ),
          class = "header_command_bar"
        )
      ),
      shinyjs::hidden(
        div(
          id = ns("help_modal"),
          div(
            div(
              tags$h1(i18n$t("help")),
              shiny.fluent::IconButton.shinyInput(ns("close_help_modal"), iconProps = list(iconName = "ChromeClose")),
              class = "help_modal_head small_close_button"
            ),
            htmlTemplate(system.file("html_pages", paste0(language, "_help.html"), package = "linkr")),
            class = "help_modal_content"
          ),
          class = "help_modal"
        )
      )
    )
  )
}

#' @noRd 
mod_page_header_server <- function(id, r, d, m, language, i18n, debug){
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
    else if (id == "git_repos") output$current_page <- renderUI(i18n$t("content_catalog"))
    else output$current_page <- renderUI(i18n$t(id))
    
    # Selected project
    
    if (id %in% c("concepts", "data", "projects", "project_files", "subsets", "tasks")){
      
      observeEvent(m$selected_study, {
        if (debug) cat(paste0("\n", now(), " - mod_page_header (", id, ") - observer m$selected_study"))
        
        project_name <- r$projects_long %>% dplyr::filter(id == m$selected_study, name == paste0("name_", language)) %>% dplyr::pull(value)
        project_name_short <- project_name
        
        max_length <- 27
        if (nchar(project_name_short) > max_length) project_name_short <- paste0(substr(project_name_short, 1, max_length - 3), "...")
        
        output$selected_project <- renderUI(create_hover_card(ui = project_name_short, text = project_name))
      })
    }
    
    observeEvent(input$open_help_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_page_header (", id, ") - observer input$open_help_modal"))
      shinyjs::show("help_modal")
    })
    
    observeEvent(input$close_help_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_page_header (", id, ") - observer input$close_help_modal"))
      shinyjs::hide("help_modal")
    })
  })
}