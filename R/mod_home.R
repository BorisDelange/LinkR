#' @noRd
mod_home_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  
  if (id == "home") page <- "home" else page <- substr(id, 6, nchar(id))
  
  div(
    class = "main",
    div(
      class = "home_container",
      div(
        class = "home_row",
        div(
          div(
            h1(i18n$t("projects")),
            p(i18n$t("projects_widget_text")),
            div(
              shiny.fluent::PrimaryButton.shinyInput(
                ns("go_project_page"), i18n$t("show_projects"), 
                href = shiny.router::route_link("projects"), iconProps = list(iconName = "Play")),
              class = "home_widget_button"
            )
          ),
          class = "home_widget"
        ),
        div(
          div(
            h1(i18n$t("datasets")),
            p(i18n$t("datasets_widget_text")),
            div(
              shiny.fluent::DefaultButton.shinyInput(
                ns("go_datasets_page"), i18n$t("manage_datasets"), 
                href = shiny.router::route_link("datasets"), iconProps = list(iconName = "Play")),
              class = "home_widget_button"
            )
          ),
          class = "home_widget"
        ),
        div(
          div(
            h1(i18n$t("vocabularies")),
            p(i18n$t("vocabularies_widget_text")),
            div(
              shiny.fluent::DefaultButton.shinyInput(
                ns("go_vocabularies_page"), i18n$t("manage_vocabularies"), 
                href = shiny.router::route_link("vocabularies"), iconProps = list(iconName = "Play")),
              class = "home_widget_button"
            )
          ),
          class = "home_widget"
        )
      ),
      div(
        class = "home_row",
        div(
          div(
            h1(i18n$t("console")),
            p(i18n$t("console_widget_text")),
            div(
              shiny.fluent::DefaultButton.shinyInput(
                ns("go_console_page"), i18n$t("open_console"), 
                href = shiny.router::route_link("console"), iconProps = list(iconName = "Play")),
              class = "home_widget_button"
            )
          ),
          class = "home_widget"
        ),
        div(
          div(
            h1(i18n$t("plugins")),
            p(i18n$t("plugins_widget_text")),
            div(
              shiny.fluent::DefaultButton.shinyInput(
                ns("go_dplugins_page"), i18n$t("show_plugins"), 
                href = shiny.router::route_link("plugins"), iconProps = list(iconName = "Play")),
              class = "home_widget_button"
            )
          ),
          class = "home_widget"
        ),
        div(
          div(
            h1(i18n$t("data_cleaning")),
            p(i18n$t("data_cleaning_widget_text")),
            div(
              shiny.fluent::DefaultButton.shinyInput(
                ns("go_data_cleaning_page"), i18n$t("manage_data_cleaning_scripts"), 
                href = shiny.router::route_link("data_cleaning"), iconProps = list(iconName = "Play")),
              class = "home_widget_button"
            )
          ),
          class = "home_widget"
        )
      ),
      div(
        class = "home_row",
        div(
          div(
            h1(i18n$t("explore")),
            p(i18n$t("explore_widget_text")),
            div(
              shiny.fluent::DefaultButton.shinyInput(
                ns("go_explore_page"), i18n$t("show_catalog"), 
                href = shiny.router::route_link("explore"), iconProps = list(iconName = "Play")),
              class = "home_widget_button"
            )
          ),
          class = "home_widget"
        ),
        div(
          div(
            h1(i18n$t("help")),
            p(i18n$t("help_widget_text")),
            div(
              shiny.fluent::DefaultButton.shinyInput(
                ns("go_help_page"), i18n$t("open_help"), 
                href = shiny.router::route_link(""), iconProps = list(iconName = "Play")),
              class = "home_widget_button"
            )
          ),
          class = "home_widget"
        ),
        div(
          div(
            h1(i18n$t("settings")),
            p(i18n$t("settings_widget_text")),
            div(
              shiny.fluent::DefaultButton.shinyInput(
                ns("go_data_settings_page"), i18n$t("open_settings"),
                href = shiny.router::route_link(""), iconProps = list(iconName = "Play")),
              class = "home_widget_button"
            )
          ),
          class = "home_widget"
        )
      )
    )
    # style = "margin-top: 20px;",
    # div(tags$a(i18n$t("projects"), href = shiny.router::route_link("projects"), class = "home_link"), style = "margin-bottom: 10px;"),
    # uiOutput(ns("projects"))#,
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

    
  })
}
