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

  # result <- div()
  
  # print(i18n$t("scripts_and_plugins"))
  
  div(class = "header",
    div(htmltools::img(src = "www/logo.png", style = "height:25px;"), class = "logo"),
    div(class = "title", shiny.fluent::Text(variant = "xLarge", "LinkR")),
    div(
      tags$a(
        tags$img(src = "www/sidebar.png", style = "width:16px; height:auto; display:block; margin-top:8px; margin-left:8px;"), 
        href = "javascript:void(0);", 
        onclick = paste0("event.preventDefault(); Shiny.setInputValue('", id, "-show_hide_sidenav', Math.random()); return false;"),
        style = "display: block; width:100%; height:100%;"
      ), 
      class = "link_image_container"),
    div(class = "header_left_bar", 
      shiny.fluent::CommandBar(
        items = list(
          shiny.fluent::CommandBarItem(i18n$t("home"), "Home", href = shiny.router::route_link("/")),
          shiny.fluent::CommandBarItem(i18n$t("data"), "OfflineStorage",
            subMenuProps = list(items = list(
              list(text = i18n$t("access_to_data"), iconProps = list(iconName = "BIDashboard"), href = shiny.router::route_link("data")),
              list(text = i18n$t("data_cleaning"), iconProps = list(iconName = "CodeEdit"), href = shiny.router::route_link("scripts")),
              list(text = i18n$t("my_studies"), iconProps = list(iconName = "CustomList"), href = shiny.router::route_link("my_studies")),
              list(text = i18n$t("my_subsets"), iconProps = list(iconName = "People"), href = shiny.router::route_link("my_subsets"))
          ))),
          shiny.fluent::CommandBarItem(i18n$t("messages"), "Chat", href = shiny.router::route_link("messages")),
          shiny.fluent::CommandBarItem(i18n$t("vocabularies"), "AllApps", href = shiny.router::route_link("vocabularies")),
          shiny.fluent::CommandBarItem(i18n$t("plugins"), "Code", subMenuProps = list(items = list(
            list(text = i18n$t("patient_lvl_data"), iconProps = list(iconName = "Contact"), href = shiny.router::route_link("plugins_patient_lvl")),
            list(text = i18n$t("aggregated_data"), iconProps = list(iconName = "Group"), href = shiny.router::route_link("plugins_aggregated"))
          )))
        )
      )
    ),
    div(class = "header_right_bar",
      shiny.fluent::Stack(horizontal = TRUE, tokens = (childrenGap = 0),
        shiny.fluent::CommandBar(),
        div(uiOutput(ns("username")), style = "font-weight:bold; padding: 12px 10px 0px 0px;"),
        shiny.fluent::CommandBarButton.shinyInput(ns("help"), iconProps = list("iconName" = "Help")),
        shiny.fluent::CommandBarButton.shinyInput("settings", iconProps = list("iconName" = "Settings"), href = shiny.router::route_link("settings/general_settings")),
        shiny.fluent::CommandBarButton.shinyInput(".shinymanager_logout", iconProps = list("iconName" = "PowerButton"))
      )
    )
  )
}

#' page_header Server Functions
#'
#' @noRd 
mod_page_header_server <- function(id = character(), r = shiny::reactiveValues(), language = "en", i18n = character(),
  perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Show / hide sidenav
    
    r$show_hide_sidenav <- "hide"
  
    observeEvent(input$show_hide_sidenav, {
      if (debug) cat(paste0("\n", now(), " - mod_page_header - observer input$show_hide_sidenav"))
      
      if (r$show_hide_sidenav == "hide"){
        r$show_hide_sidenav <- "show"
        shinyjs::runjs(paste0("$('.extended_sidenav').css('display', 'none');"))
        shinyjs::runjs(paste0("$('.reduced_sidenav').css('display', 'block');"))
        shinyjs::runjs(paste0("$('.grid-container').css('grid-template-areas', '\"header header header\" \"sidenav main main\" \"footer footer footer\"');"))
        shinyjs::runjs(paste0("$('.main').css('left', '20px');"))
      }
      else {
        r$show_hide_sidenav <- "hide"
        shinyjs::runjs(paste0("$('.extended_sidenav').css('display', 'block');"))
        shinyjs::runjs(paste0("$('.reduced_sidenav').css('display', 'none');"))
        shinyjs::runjs(paste0("$('.grid-container').css('grid-template-areas', '\"header header header\" \"sidenav sidenav main\" \"footer footer footer\"');"))
        shinyjs::runjs(paste0("$('.main').css('left', '0px');"))
      }
    })
    
    output$username <- renderUI(r$username)
  })
}