# UI - Settings files
#
# This code corresponds to the page that appears when you click on the file icon at the top of the widget.
# The UI allows you to select, add, and delete settings files.

tagList(

    # Add settings file modal
    shinyjs::hidden(
      div(
        id = ns("add_settings_file_modal_%widget_id%"),
        div(
            div(
                tags$h1(i18np$t("create_settings_file"), style = "font-size: 14px;"),
                shiny.fluent::IconButton.shinyInput(ns("close_add_settings_file_modal_%widget_id%"), iconProps = list(iconName = "ChromeClose")),
                style = "display: flex; justify-content: space-between;",
                class = "small_close_button"
            ),
            div(shiny.fluent::TextField.shinyInput(ns("settings_file_name_%widget_id%"), label = i18np$t("file_name")), style = "width: 200px;"),
            div(
                shiny.fluent::PrimaryButton.shinyInput(ns("add_settings_file_%widget_id%"), i18np$t("add")),
                style = "position: absolute; right: 10px; bottom: 8px;"
            ),
            style = "background: #fff; padding: 5px 10px 10px 15px; position: relative; width: 400px; height: 120px;"
        ),
        style = "display: flex; align-items: center; justify-content: center; position: absolute; left: 0; top: 0;  width: 100%; height: 100%; background-color: rgba(0,0,0,0.2); z-index: 1000;"
      )
    ),
    
    # Delete settings file modal
    shinyjs::hidden(
      div(
        id = ns("delete_settings_file_modal_%widget_id%"),
        div(
            tags$h1(i18np$t("delete_settings_file_title"), style = "font-size: 14px;"),
            tags$p(i18np$t("delete_settings_file_text")),
            div(
                shiny.fluent::DefaultButton.shinyInput(ns("close_file_deletion_modal_%widget_id%"), i18np$t("dont_delete")),
                div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_file_deletion_%widget_id%"), i18np$t("delete")), class = "delete_button"),
                style = "position: absolute; right: 10px; bottom: 8px; display: flex; gap: 5px;"
            ),
            style = "background: #fff; padding: 5px 10px 10px 15px; position: relative; width: 400px; height: 120px;"
        ),
        style = "display: flex; align-items: center; justify-content: center; position: absolute; left: 0; top: 0;  width: 100%; height: 100%; background-color: rgba(0,0,0,0.2); z-index: 1000;"
      )
    ),
    
    # Sidenav
    div(
        id = ns("settings_files_sidenav_%widget_id%"),
        shiny.fluent::IconButton.shinyInput(ns("create_settings_file_%widget_id%"), iconProps = list(iconName = "Add"), title = i18np$t("create_settings_file"), style = "margin: 0"),
        class = "widget_icon",
        style = "border-right: solid grey 0.5px; padding-left: 5px;"
    ),
    
    # Dropdown and delete button
    div(
        div(shiny.fluent::Dropdown.shinyInput(ns("settings_file_%widget_id%"), label = i18np$t("file"), options = dropdown_options, value = selected_file), style = "width: 200px"),
        shinyjs::hidden(
            div(
                id = ns("delete_settings_file_div_%widget_id%"),
                shiny.fluent::IconButton.shinyInput(ns("delete_settings_file_%widget_id%"), iconProps = list(iconName = "Delete")),
                style = "margin-top: 26px;", class = "widget_icon"
            )
        ),
        style = "display: flex; gap: 5px; margin: 5px 10px;"
    )
)
