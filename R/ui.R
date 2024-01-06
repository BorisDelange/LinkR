#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#' @param css CSS file location (character)
#' @param language Default language to use in the App (character)
#' @param i18n shiny.i18n object for translations
#' @param users_accesses_toggles_options A tibble containing users accesses, to add in database if no internet access (tibble)
#' @param debug Debug mode : steps and errors will by displayed in the console (logical)
#' @import shiny
#' @noRd

app_ui <- function(request, css, language, languages, i18n = character(), users_accesses_toggles_options = tibble::tibble(), debug = FALSE) {
  
  pages <- c(
    "/", 
    "home/get_started", 
    "home/tutorials", 
    "home/resources",
    "my_studies", 
    "my_subsets",
    "messages",
    "vocabularies", 
    "data", 
    "scripts", 
    "patient_level_data", 
    "aggregated_data",
    "plugins",
    "plugins_patient_lvl",
    "plugins_aggregated",
    "settings/general_settings",
    "settings/app_db",
    "settings/git",
    "settings/users", 
    "settings/dev", 
    "settings/data_sources",
    "settings/datasets", 
    "settings/vocabularies",
    "settings/log")
  
  do.call(shiny.router::router_ui,
    lapply(pages, function(page_url){
      if (debug) cat(paste0("\n", Sys.time(), " - ui - make_router - ", page_url))
      if (page_url == "/") page <- "home" else page <- page_url
      shiny.router::route(page_url, make_layout(language = language, languages = languages, page = page, i18n = i18n, users_accesses_toggles_options = users_accesses_toggles_options))
    })
  ) -> page
  
  # Secure page with ShinyManager
  shinymanager::secure_app(
    tagList(
      golem_add_external_resources(css),
      shiny.fluent::fluentPage(page)
    ),
    enable_admin = FALSE, language = tolower(language), fab_position = "none"
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @param css CSS file location (character)
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd

golem_add_external_resources <- function(css){
  
  add_resource_path('www', app_sys('app/www'))
  
  # Marker is used to highlight some text
  # if (require("marker")) marker_div <- marker::useMarker() else marker_div <- ""
 
  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'LinkR'
    ),
    # Link to CSS file
    tags$link(href = css, rel = "stylesheet", type = "text/css"),
    
    # Add highlight.js
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/styles/tomorrow-night-blue.min.css"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.3.1/highlight.min.js"),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        var observer = new MutationObserver(function(mutations) {
          mutations.forEach(function(mutation) {
            if (mutation.addedNodes.length > 0) {
              document.querySelectorAll('.code_highlight:not(.hljs)').forEach((block) => {
                hljs.highlightElement(block);
                block.classList.add('hljs'); // Ajoute une classe pour marquer que le highlight a été appliqué
              });
            }
          });
        });

        var target = document.querySelector('body'); // Observez tout le body pour les changements
        observer.observe(target, { childList: true, subtree: true });
      });
    ")),
    
    # Script for make an input when a text is entered in the person ComboBox.shinyInput
    tags$script("
      $(document).on('keyup', '#patient_level_data-person-input', function() {
        Shiny.setInputValue('patient_level_data-person_trigger', $(this).val());
      });
    "),
    
    # Add fontawesome icons
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css",
      integrity = "sha384-cmES2APjF1Ar8dUWeaROssI2FTx2MFqjMq9u2p89A/QD5/dZqk3bxDi1y5w2AWiS",
      crossorigin = "anonymous"),
    
    # Shinyjs is used to show and hide message bars
    shinyjs::useShinyjs(),
    
    # marker_div,
    
    # Shinybusy is used to add a busy bar on top of the page, when there are loading times
    shinybusy::add_busy_bar(timeout = 1000, color = "#0D98FF", height = "3px"),
    
    # A function to make info button works, on the header
    tags$script(
      "$(function() {
          $('.ms-Button--commandBar').on('click', function() {
            Shiny.setInputValue('header_active_page', $(this).attr('id'));
          })
        })"
    )
  )
}
