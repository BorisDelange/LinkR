create_widget <- function(ns, widget_id, ui_code, show_edit_buttons = FALSE){
  edit_buttons <- div(
    id = ns(paste0("widget_settings_remove_buttons_", widget_id)),
    div(
      div(
        shiny.fluent::IconButton.shinyInput(ns(paste0("widget_settings_", widget_id)), iconProps = list(iconName = "Settings")),
        class = "small_icon_button"
      ),
      div(
        shiny.fluent::IconButton.shinyInput(ns(paste0("remove_widget_", widget_id)), iconProps = list(iconName = "Delete")),
        class = "small_icon_button"
      ),
      style = "display: flex; gap: 2px;"
    ),
    class = "widget_buttons"
  )
  if (!show_edit_buttons) edit_buttons <- shinyjs::hidden(edit_buttons)
  
  div(
    ui_code,
    edit_buttons,
    class = "widget"
  )
}