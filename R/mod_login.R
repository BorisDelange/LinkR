#' @noRd 
mod_login_ui <- function(id, i18n){
  ns <- NS(id)
  
  div(
    div(
      div(
        tags$img(src = "www/logo.png", style = "width: 28px; margin-bottom: 15px;"),
        style = "width: 100%; display: flex; align-items: center; justify-content: center;"
      ),
      div(shiny.fluent::TextField.shinyInput(ns("username"), label = i18n$t("username")), style = "width: 200px;"),
      div(shiny.fluent::TextField.shinyInput(ns("password"), label = i18n$t("password"), type = "password", canRevealPassword = TRUE), style = "width: 200px;"),
      div(
        shiny.fluent::PrimaryButton.shinyInput(ns("login"), i18n$t("submit_login")),
        style = "width: 100%; display: flex; align-items: center; justify-content: center; margin-top: 15px;"
      ),
      style = "width: 200px; padding: 15px 10px;",
      class = "widget"
    ),
    style = "width: 100%; display: flex; align-items: center; justify-content: center;"
  )
}

#' @noRd 
mod_login_server <- function(id, r, i18n, debug){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$login, {
      if (debug) cat(paste0("\n", now(), " - mod_login - observer input$login"))
      
      req(length(r$user_id) == 0)
      
      # Check if login and username are not empty
      empty <- list()
      
      for (field in c("username", "password")){
        empty[[field]] <- TRUE
        name <- input[[field]]
        if (length(name) > 0) if (!is.na(name) & name != "") empty[[field]] <- FALSE
        if (empty[[field]]) shiny.fluent::updateTextField.shinyInput(session, field, errorMessage = i18n$t("provide_valid_value"))
        else shiny.fluent::updateTextField.shinyInput(session, field, errorMessage = NULL)
      }
      
      req(!empty$username, !empty$password)
      
      # Check login and password
      sql <- glue::glue_sql("SELECT id, password FROM users WHERE username = {input$username}", .con = r$db)
      result <- DBI::dbGetQuery(r$db, sql)
      
      if (nrow(result) > 0 && bcrypt::checkpw(input$password, result$password)){
        r$user_id <- result$id
        for (field in c("username", "password")) shiny.fluent::updateTextField.shinyInput(session, field, disabled = TRUE)
        shiny.fluent::updatePrimaryButton.shinyInput(session, "login", disabled = TRUE)
        shiny.router::change_page("/")
      }
      else shiny.fluent::updateTextField.shinyInput(session, "password", errorMessage = i18n$t("invalid_login_or_password"))
    })
  })
}