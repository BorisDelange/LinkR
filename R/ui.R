#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#' @param css CSS file location (character)
#' @param page Pages created with shiny.router
#' @param users_accesses_toggles_options A tibble containing users accesses, to add in database if no internet access (tibble)
#' @param language Default language to use in the App (character)
#' @param debug Debug mode : steps and errors will by displayed in the console (logical)
#' @import shiny
#' @noRd

app_ui <- function(request, css, page, users_accesses_toggles_options, language, debug = FALSE) {
  
  # Secure page with ShinyManager
  shinymanager::secure_app(
    tagList(
      golem_add_external_resources(css),
      shiny.fluent::fluentPage(page$ui)
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
    # tags$meta(charset = "UTF-8"),
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'LinkR'
    ),
    # Link to CSS file
    htmltools::tags$link(href = css, rel = "stylesheet", type = "text/css"),
    
    # Add highlight.js
    # tags$head(
    #   tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.8.0/styles/a11y-light.min.css"),
    #   tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.8.0/highlight.min.js")
    # ),
    # tags$script(
    #   HTML("
    #   document.addEventListener('DOMContentLoaded', function() {
    #     document.querySelectorAll('pre code').forEach(function(block) {
    #       hljs.highlightBlock(block);
    #     });
    #   });
    # ")
    # ),
    
    # Add fontawesome icons
    htmltools::tags$link(
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