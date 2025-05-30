#' @noRd
mod_home_ui <- function(id){
  
  pages_variables_list <- get("pages_variables_list", envir = parent.frame())
  for (obj_name in pages_variables_list) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  ns <- NS(id)
  
  if (id == "home") page <- "home" else page <- substr(id, 6, nchar(id))
  
  div(
    class = "main",
    div(
      class = "home-container",
      div(
        class = "home-section",
        tags$h1(tags$a(href = shiny.router::route_link("projects"), i18n$t("projects")), class = "home-section-title"),
        uiOutput(ns("projects"))
      ),
      div(
        class = "home-section",
        tags$h1(tags$a(href = shiny.router::route_link("datasets"), i18n$t("datasets")), class = "home-section-title"),
        uiOutput(ns("datasets"))
      ),
      div(
        class = "home-section",
        tags$h1(tags$a(href = shiny.router::route_link("plugins"), i18n$t("plugins")), class = "home-section-title"),
        uiOutput(ns("plugins"))
      ),
      htmlTemplate(system.file("html_pages", paste0(language, "_home.html"), package = "linkr"))
    )
  )
}

#' @noRd 
mod_home_server <- function(id){
  
  pages_variables_list <- get("pages_variables_list", envir = parent.frame())
  for (obj_name in pages_variables_list) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    sapply(c("projects", "datasets", "plugins"), function(page){
      
      single_id <- switch(
        page, 
        "data_cleaning" = "data_cleaning", 
        "datasets" = "dataset",
        "projects" = "project", 
        "plugins" = "plugin", 
        "subsets" = "subset", 
        "vocabularies" = "vocabulary"
      )
        
      observeEvent(r[[paste0("reload_home_", page)]], try_catch(paste0("r$reload_home_", page), {
        
        long_var_filtered <- paste0("filtered_", page, "_long")
        
        reload_elements_var(page_id = "home", id = page, con = r$db, long_var_filtered = long_var_filtered)
        
        buttons <- tagList(
          tags$button(class = "home-nav-button prev hidden", onclick = "scrollContainer(this, 'left')", tags$i(class = "fas fa-chevron-left")),
          tags$button(class = "home-nav-button next hidden", onclick = "scrollContainer(this, 'right')", tags$i(class = "fas fa-chevron-right")),
        )
        
        if (nrow(r[[long_var_filtered]]) > 0) elements_ui <- create_elements_ui(page_id = "home", id = page, elements = r[[long_var_filtered]], r = r, language = language, i18n = i18n)
        else {
          elements_ui <- div(
            shiny.fluent::MessageBar(
              tagList(
                i18n$t(paste0("no_", single_id, "_to_display")), ".",
                tags$a(
                  href = shiny.router::route_link(paste0(page, "?create_element")),
                  i18n$t(paste0("create_", single_id))
                ), "."
              ),
              messageBarType = 5
            ),
            class = "message_bar"
          )
          buttons <- shinyjs::hidden(buttons)
        }
        
        elements_ui <- div(
          class = "home-widget-container",
          buttons,
          elements_ui
        )
        
        output[[page]] <- renderUI(elements_ui)
      }))
      
      r[[paste0("reload_home_", page)]] <- now()
      
      observeEvent(input[[paste0("selected_", single_id, "_trigger")]], try_catch(paste0("r$selected_", single_id, "_trigger"), {
        
        shiny.router::change_page(page)
        shinyjs::runjs(paste0("Shiny.setInputValue('", page, "-selected_element', ", input[[paste0("selected_", single_id)]], ");"))
        shinyjs::delay(500, shinyjs::runjs(paste0("Shiny.setInputValue('", page, "-selected_element_trigger', Math.random());")))
      }))
    })
    
    shinyjs::delay(10, shinyjs::runjs("
      document.querySelectorAll('.home-section').forEach(section => {
        updateNavButtons(section);
      });
    "))
  })
}
