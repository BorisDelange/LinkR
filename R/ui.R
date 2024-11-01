#' @import shiny
#' @noRd
app_ui <- function(
    pages, language, languages, i18n, users_accesses_toggles_options, 
    db_col_types, dropdowns, auto_complete_list, debug) {
  
  do.call(
    shiny.router::router_ui,
    lapply(pages, function(page_url){
      if (debug) cat(paste0("\n", now(), " - ui - make_router - ", page_url))
      
      if (page_url == "/") page <- "home" else page <- page_url
      
      code_hotkeys <- list(
        save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
        run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
        run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
        comment = list(win = "CTRL-SHIFT-C", mac = "CTRL-SHIFT-C|CMD-SHIFT-C")
      )
      
      args <- list(page, language, languages, i18n)
      
      if (page == "users") args <- list(page, language, languages, i18n, users_accesses_toggles_options)
      else if (page == "app_db") args <- list(page, language, languages, i18n, code_hotkeys, db_col_types)
      else if (page %in% c("console", "data_cleaning", "datasets", "subsets")) args <- list(page, language, languages, i18n, code_hotkeys, auto_complete_list)
      else if (page == "vocabularies") args <- list(page, language, languages, i18n, code_hotkeys, dropdowns)
      else if (page == "concepts") args <- list(page, language, languages, i18n, dropdowns)
      
      if (page == "login") shiny.router::route(
        page_url,
        div(
          class = "page_container",
          div(
            mod_login_ui("login", i18n),
            class = "main_container"
          )
        )
      )
      
      else shiny.router::route(
        page_url,
        div(
          class = "page_container",
          mod_page_header_ui(id = page, i18n = i18n),
          div(
            mod_page_sidenav_ui(id = page, language = language, i18n = i18n),
            do.call(paste0("mod_", page, "_ui"), args),
            class = "main_container"
          ),
          mod_page_footer_ui(i18n = i18n)
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
      tags$link(rel = "shortcut icon", href = "www/images/favicon.png"),
      
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