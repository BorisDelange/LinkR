#' @import shiny
#' @noRd
app_ui <- function(pages, language, languages, i18n, users_accesses_toggles_options, db_col_types, dropdowns, debug) {
  
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
      else if (page %in% c("data_cleaning", "datasets")) args <- list(page, language, languages, i18n, code_hotkeys)
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
            mod_page_sidenav_ui(id = page, i18n = i18n),
            do.call(paste0("mod_", page, "_ui"), args),
            class = "main_container"
          ),
          mod_page_footer_ui(i18n = i18n)
        )
      )
    })
  ) -> page
  
  golem::add_resource_path("www", system.file("app/www", package = "linkr"))
  
  tagList(
    golem_add_external_resources(),
    shiny.fluent::fluentPage(page)
  )
}

#' @noRd
golem_add_external_resources <- function(){
  
  golem::add_resource_path("www", app_sys("app/www"))
 
  tags$head(
    golem::favicon(ext = "png"),
    golem::bundle_resources(path = app_sys("app/www"), app_title = "LinkR"),
    tags$link(href = "style.css", rel = "stylesheet", type = "text/css"),
    tags$link(href = "fluent_style.css", rel = "stylesheet", type = "text/css"),

    # Add jquery
    tags$link(rel = "stylesheet", href = "https://code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"),
    tags$script(src = "https://code.jquery.com/ui/1.12.1/jquery-ui.js"),

    # Add gridstacks.js
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/gridstack.js/10.1.0/gridstack.min.css"),

    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        window.gridStackInstances = {};
      });
    ")),

    # Script to make an input when a text is entered in the person ComboBox.shinyInput
    tags$script("
      $(document).on('keyup', '#data-person-input', function() {
        Shiny.setInputValue('data-person_trigger', $(this).val());
      });
    "),

    # Add fontawesome icons
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),

    # Shinyjs is used to show and hide message bars
    shinyjs::useShinyjs(),

    # Shinybusy is used to add a busy bar on top of the page, when there are loading times
    shinybusy::add_busy_bar(timeout = 1000, color = "#0D98FF", height = "3px")
  )
}