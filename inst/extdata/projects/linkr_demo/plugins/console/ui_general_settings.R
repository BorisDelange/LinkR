# UI - General settings
#
# Include the UI components for the general settings in this section.
# If you add any fields here, ensure that you also update server_general_settings.R and ui_general_settings.R accordingly.

div(
    div(
        tags$strong(i18np$t("display")), br(),
        div(
            shiny.fluent::Toggle.shinyInput(
                ns("show_settings_file_%widget_id%"), value = toggle_values$show_settings_file,
                onClick = htmlwidgets::JS(paste0("item => {Shiny.setInputValue('", id, "-save_general_settings_%widget_id%', Math.random());}"))
            ),
            tags$label(i18np$t("show_settings_file"), `for` = ns("show_settings_file_%widget_id%"), style = "margin-left: 5px;"),
            style = "display: flex; margin-top: 8px;" 
        ),
        div(
            shiny.fluent::Toggle.shinyInput(
                ns("figure_and_settings_side_by_side_%widget_id%"), value = toggle_values$figure_and_settings_side_by_side,
                onClick = htmlwidgets::JS(paste0("item => {Shiny.setInputValue('", id, "-save_general_settings_%widget_id%', Math.random());}"))
            ),
            tags$label(i18np$t("figure_and_settings_side_by_side"), `for` = ns("figure_and_settings_side_by_side_%widget_id%"), style = "margin-left: 5px;"),
            style = "display: flex; margin-top: 5px;" 
        )
    ),
    div(
        tags$strong(i18np$t("code_execution")), br(),
        div(
            shiny.fluent::Toggle.shinyInput(
                ns("run_code_at_settings_file_load_%widget_id%"), value = toggle_values$run_code_at_settings_file_load,
                onClick = htmlwidgets::JS(paste0("item => {Shiny.setInputValue('", id, "-save_general_settings_%widget_id%', Math.random());}"))
            ),
            tags$label(i18np$t("run_code_at_settings_file_load"), `for` = ns("run_code_at_settings_file_load_%widget_id%"), style = "margin-left: 5px;"),
            style = "display: flex; margin-top: 8px;" 
        ),
        div(
            shiny.fluent::Toggle.shinyInput(
                ns("run_code_on_data_update_%widget_id%"), value = toggle_values$run_code_on_data_update,
                onClick = htmlwidgets::JS(paste0("item => {Shiny.setInputValue('", id, "-save_general_settings_%widget_id%', Math.random());}"))
            ),
            tags$label(i18np$t("run_code_on_data_update"), `for` = ns("run_code_on_data_update_%widget_id%"), style = "margin-left: 5px;"),
            style = "display: flex; margin-top: 5px;" 
        ),
        style = "margin-top: 10px;"
    ),
    style = "padding: 10px;"
)
