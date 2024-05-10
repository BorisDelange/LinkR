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
            h1("Projets"),
            p("Les projets permettent de travailler de façon collaborative sur des données de santé au format OMOP,
              à l'aide d'une interface graphique modulable."),
            div(
              shiny.fluent::PrimaryButton.shinyInput(
                ns("go_project_page"), "Afficher les projets", 
                href = shiny.router::route_link("projects"), iconProps = list(iconName = "Play")),
              class = "home_widget_button"
            )
          ),
          class = "home_widget"
        ),
        div(
          div(
            h1("Datasets"),
            p("..."),
            div(
              shiny.fluent::DefaultButton.shinyInput(
                ns("go_datasets_page"), "Afficher les datasets", 
                href = shiny.router::route_link("datasets"), iconProps = list(iconName = "Play")),
              class = "home_widget_button"
            )
          ),
          class = "home_widget"
        ),
        div(
          div(
            h1("Terminologies"),
            p("..."),
            div(
              shiny.fluent::DefaultButton.shinyInput(
                ns("go_vocabularies_page"), "Afficher les terminologies", 
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
            h1("Console"),
            p("..."),
            div(
              shiny.fluent::DefaultButton.shinyInput(
                ns("go_console_page"), "Ouvrir la console", 
                href = shiny.router::route_link("console"), iconProps = list(iconName = "Play")),
              class = "home_widget_button"
            )
          ),
          class = "home_widget"
        ),
        div(
          div(
            h1("Plugins"),
            p("..."),
            div(
              shiny.fluent::DefaultButton.shinyInput(
                ns("go_dplugins_page"), "Afficher les plugins", 
                href = shiny.router::route_link("plugins"), iconProps = list(iconName = "Play")),
              class = "home_widget_button"
            )
          ),
          class = "home_widget"
        ),
        div(
          div(
            h1("Data cleaning"),
            p("..."),
            div(
              shiny.fluent::DefaultButton.shinyInput(
                ns("go_data_cleaning_page"), "Afficher les scripts de data cleaning", 
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
            h1("Explorer"),
            p("..."),
            div(
              shiny.fluent::DefaultButton.shinyInput(
                ns("go_explore_page"), "Afficher le catalogue", 
                href = shiny.router::route_link("explore"), iconProps = list(iconName = "Play")),
              class = "home_widget_button"
            )
          ),
          class = "home_widget"
        ),
        div(
          div(
            h1("Aide"),
            p("..."),
            div(
              shiny.fluent::DefaultButton.shinyInput(
                ns("go_help_page"), "Ouvrir l'aide", 
                href = shiny.router::route_link(""), iconProps = list(iconName = "Play")),
              class = "home_widget_button"
            )
          ),
          class = "home_widget"
        ),
        div(
          div(
            h1("Paramètres"),
            p("..."),
            div(
              shiny.fluent::DefaultButton.shinyInput(
                ns("go_data_settings_page"), "Ouvrir les paramètres", 
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
