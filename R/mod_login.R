#' @noRd 
mod_login_ui <- function(id){
  
  pages_variables_list <- get("pages_variables_list", envir = parent.frame())
  for (obj_name in pages_variables_list) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  ns <- NS(id)
  
  div(
    div(
      div(
        tags$img(src = "www/img/logo.png", style = "width: 34px; margin-bottom: 15px;"),
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
    style = "width: 100%; display: flex; align-items: center; justify-content: center; margin-top: -100px;",
    
    # Trigger login button when user presses Enter in username or password fields
    tags$script(HTML(sprintf("
      $(document).ready(function() {
        setTimeout(function() {
          $('#%s-username, #%s-password').on('keypress', function(e) {
            if (e.which == 13) {
              Shiny.setInputValue('%s-username', $('#%s-username').val());
              Shiny.setInputValue('%s-password', $('#%s-password').val());
              
              setTimeout(function() {
                $('#%s-login').click();
              }, 50);
              return false;
            }
          });
        }, 500);
      });
    ", id, id, id, id, id, id, id)))
  )
}

#' @noRd 
mod_login_server <- function(id){
  
  pages_variables_list <- get("pages_variables_list", envir = parent.frame())
  for (obj_name in pages_variables_list) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$login, try_catch("input$login", {
      
      if (length(r$user_id) > 0) return()
      
      # Check if login and username are not empty
      empty <- list()
      
      for (field in c("username", "password")){
        empty[[field]] <- TRUE
        name <- input[[field]]
        if (length(name) > 0) if (!is.na(name) & name != "") empty[[field]] <- FALSE
        if (empty[[field]]) shiny.fluent::updateTextField.shinyInput(session, field, errorMessage = i18n$t("provide_valid_value"))
        else shiny.fluent::updateTextField.shinyInput(session, field, errorMessage = NULL)
      }
      
      if (empty$username || empty$password) return()
      
      # Check login and password
      sql <- glue::glue_sql("SELECT id, password FROM users WHERE username = {input$username}", .con = r$db)
      result <- DBI::dbGetQuery(r$db, sql)
      
      error_login_or_password <- TRUE
      
      if (nrow(result) > 0){
        
        password_check <- tryCatch({
          bcrypt::checkpw(input$password, result$password)
        }, error = function(e) FALSE)
        
        if (password_check){
          error_login_or_password <- FALSE
          
          r$user_id <- result$id
          
          for (field in c("username", "password")) shiny.fluent::updateTextField.shinyInput(session, field, disabled = TRUE)
          shiny.fluent::updatePrimaryButton.shinyInput(session, "login", disabled = TRUE)
          
          r$load_page <- "home"
          shiny.router::change_page("/")
        }
      }
      
      if (error_login_or_password) shiny.fluent::updateTextField.shinyInput(session, "password", errorMessage = i18n$t("invalid_login_or_password"))
    }))
  })
}