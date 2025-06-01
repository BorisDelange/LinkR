#' @import shiny
#' @noRd
app_ui <- function() {
  
  variables_list <- get("variables_list", envir = parent.frame())
  for (obj_name in variables_list) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  languages <- get_languages()
  
  pages_variables_list <- c("page", "language", "languages", "i18n")
  
  do.call(
    shiny.router::router_ui,
    lapply(get_pages(), function(page){
      if ("event" %in% log_level) cat(paste0("\n[", now(), "] [EVENT] [page_id = ui] make_router ", page))
      
      if (authentication && page == "login") page_url <- "/"
      else if (!authentication && page == "home") page_url <- "/"
      else page_url <- page
      
      if (page == "login") shiny.router::route(
        page_url,
        div(
          class = "page_container",
          div(
            mod_login_ui("login"),
            class = "main_container"
          )
        )
      )
      
      else shiny.router::route(
        page_url,
        div(
          class = "page_container",
          mod_page_header_ui(page),
          div(
            mod_page_sidenav_ui(page),
            do.call(paste0("mod_", page, "_ui"), list(page)),
            class = "main_container"
          ),
          mod_page_footer_ui(page)
        )
      )
    })
  ) -> page
  
  # Make app/www accessible via /www URL
  addResourcePath("www", system.file("www", package = "linkr"))
  
  # Make node_modules accessible via /node_modules URL
  addResourcePath("node_modules", system.file("node_modules", package = "linkr"))
  
  tagList(
    tags$head(
      
      # Title
      tags$title("LinkR"),
      
      # Favicon
      tags$link(rel = "shortcut icon", href = "www/img/favicon.png"),
      
      # CSS files
      tags$link(rel = "stylesheet", href = "www/css/style.css", type = "text/css"),
      
      # Add jquery
      tags$link(rel = "stylesheet", href = "www/css/jquery-ui-1.12.1.css", type = "text/css"),
      tags$script(src = "www/js/jquery-ui-1.12.1.min.js"),
      
      # Add gridstacks.js
      tags$link(rel = "stylesheet", href = "node_modules/gridstack/dist/gridstack.min.css"),
      tags$script(src = "node_modules/gridstack/dist/gridstack-all.js"),
      
      tags$script(HTML("
        document.addEventListener('DOMContentLoaded', function() {
          window.gridStackInstances = {};
        });
      ")),
      
      # Add resizePanels
      tags$link(rel = "stylesheet", href = "www/css/resizePanels.css"),
      tags$script(src = "www/js/resizePanels.js"),
      
      # Add sortable.js and sortableTabs.js
      tags$script(src = "www/js/sortable.min.js"),
      tags$script(src = "www/js/sortableTabs.js"),
      
      # Add fontawesome icons
      tags$link(rel = "stylesheet", href = "www/css/font-awesome-5.15.3-all.min.css", type = "text/css"),
      
      # Add fluent icons
      tags$link(rel = "stylesheet", href = "www/css/fluent-icons.css"),
      
      # Add customised fluent UI
      tags$link(rel = "stylesheet", href = "www/css/fluent-style.css", type = "text/css"),
      
      # Shinyjs is used to show and hide message bars
      shinyjs::useShinyjs(),
      
      # Shinybusy is used to add a busy bar on top of the page, when there are loading times
      shinybusy::add_busy_bar(timeout = 1000, color = "#0D98FF", height = "5px")
    ),
    shiny.fluent::fluentPage(page)
  )
}