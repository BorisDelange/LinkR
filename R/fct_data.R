#' @noRd
create_gridstack_instance <- function(id, tab_id){
  ns <- NS(id)
  
  shinyjs::delay(200, 
    shinyjs::runjs(paste0("
      if (!window.gridStackInstances['", tab_id, "']) {
        import('https://esm.sh/gridstack').then((module) => {
          const GridStack = module.GridStack;
          window.gridStackInstances['", tab_id, "'] = GridStack.init({
            cellHeight: 100,
            staticGrid: true,
            resizable: { handles: 'se, ne, nw, sw' },
            margin: 10
          }, '#", ns(paste0("gridstack_", tab_id)), "');
        });
      }
    "))
  )
}

#' @noRd
create_widget <- function(id, widget_id, ui_code, show_edit_buttons = FALSE){
  ns <- NS(id)
  
  if (id == "data") prefix <- "data"
  else if (id == "plugins") prefix <- "plugins"
  
  edit_buttons <- div(
    id = ns(paste0(prefix, "_widget_settings_buttons_", widget_id)),
    div(
      div(
        shiny.fluent::IconButton.shinyInput(ns(paste0(prefix, "_widget_settings_", widget_id)), iconProps = list(iconName = "Settings")),
        class = "small_icon_button"
      ),
      div(
        shiny.fluent::IconButton.shinyInput(ns(paste0(prefix, "_widget_remove_", widget_id)), iconProps = list(iconName = "Delete")),
        class = "small_icon_button"
      ),
      style = "display: flex; gap: 2px;"
    ),
    class = "widget_buttons"
  )
  if (!show_edit_buttons) edit_buttons <- shinyjs::hidden(edit_buttons)
  
  div(
    id = ns(paste0(prefix, "_gridstack_item_", widget_id)),
    class = "grid-stack-item",
    div(
      class = "grid-stack-item-content",
      div(
        ui_code,
        edit_buttons,
        class = "widget"
      )
    )
  )
}

#' @noRd
add_widget_to_gridstack <- function(id, tab_id, ui_output, previous_widget_id = NA_integer_){
  ns <- NS(id)
  
  if (id == "plugins") delete_previous_widget <- paste0(
    "var previous_widget = grid.el.querySelector('#", ns(paste0("plugins_gridstack_item_", previous_widget_id)), "');
    if (previous_widget) grid.removeWidget(previous_widget);"
  )
  else delete_previous_widget <- ""
  
  shinyjs::delay(200, shinyjs::runjs(paste0("
    var grid = window.gridStackInstances['", tab_id, "'];
    
    if (grid) {
      // Remove previous widget
      ", delete_previous_widget, "
      
      // Add new widget
      grid.addWidget(`", ui_output, "`, {w: 6, h: 4});
      
      // Load react components
      $(document).on('shiny:idle', function(event) {
        document.querySelectorAll('.react-container').forEach(container => {
          const reactDataScript = container.querySelector('.react-data');
          if (reactDataScript) {
            jsmodule['@/shiny.react'].findAndRenderReactData();
          }
        });
      });
      
      // Rebind Shiny components
      setTimeout(function() {
        Shiny.unbindAll();
        Shiny.bindAll();
      }, 100);
    }
  ")))
}

#' @noRd
process_widget_code <- function(code, tab_id, widget_id, study_id, patient_id) {
  code <- gsub("%tab_id%", as.character(tab_id), code, fixed = TRUE)
  code <- gsub("%widget_id%", as.character(widget_id), code, fixed = TRUE)
  code <- gsub("%req%", "req(m[[session_code]] == session_num)\nreq(m$selected_study == %study_id%)", code, fixed = TRUE)
  code <- gsub("%study_id%", as.character(study_id), code, fixed = TRUE)
  code <- gsub("%patient_id%", as.character(patient_id), code, fixed = TRUE)
  gsub("\r", "\n", code, fixed = TRUE)
  code
}