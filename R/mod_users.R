#' @noRd
mod_users_ui <- function(id, language, languages, i18n, users_accesses_toggles_options){
  ns <- NS(id)
  
  # Modals ----
  modals <- list()
  
  for (modal in c("user", "user_status", "user_access")){
    
    if (modal != "user") modals[[paste0("create_", modal, "_modal")]] <-
      shinyjs::hidden(
        div(
          id = ns(paste0("create_", modal, "_modal")),
          div(
            div(
              tags$h1(i18n$t(paste0("create_", modal))),
              shiny.fluent::IconButton.shinyInput(ns(paste0("close_create_", modal, "_modal")), iconProps = list(iconName = "ChromeClose")),
              class = "create_element_modal_head small_close_button"
            ),
            div(
              div(
                div(shiny.fluent::TextField.shinyInput(ns(paste0("create_", modal, "_name")), label = i18n$t("name")), style = "width: 200px;"),
                div(shiny.fluent::TextField.shinyInput(ns(paste0("create_", modal, "_description")), label = i18n$t("description")), style = "width: 100%;"),
              ),
              div(
                shiny.fluent::PrimaryButton.shinyInput(ns(paste0("add_", modal)), i18n$t("add")),
                class = "create_element_modal_buttons"
              ),
            ),
            class = "create_user_status_modal_content"
          ),
          class = "create_element_modal"
        )
      )
    
    modals[[paste0("delete_", modal, "_modal")]] <-
      shinyjs::hidden(
        div(
          id = ns(paste0("delete_", modal, "_modal")),
          div(
            tags$h1(i18n$t(paste0("delete_", modal, "_title"))), tags$p(i18n$t(paste0("delete_", modal, "_text"))),
            div(
              shiny.fluent::DefaultButton.shinyInput(ns(paste0("close_", modal, "_deletion_modal")), i18n$t("dont_delete")),
              div(shiny.fluent::PrimaryButton.shinyInput(ns(paste0("confirm_", modal, "_deletion")), i18n$t("delete")), class = "delete_button"),
              class = "delete_modal_buttons"
            ),
            class = "delete_modal_content"
          ),
          class = "delete_modal"
        )
      )
  }
  
  # Users accesses toggles ----
  
  users_accesses_toggles_options_result <- tagList()
  
  for (i in 1:nrow(users_accesses_toggles_options)){
    
    sub_results <- tagList()
    
    if (users_accesses_toggles_options[[i, "toggles"]] != ""){
      for (toggle in users_accesses_toggles_options[[i, "toggles"]][[1]]){
        sub_results <- tagList(
          sub_results,
          div(
            shiny.fluent::Toggle.shinyInput(ns(paste0("toggle_", toggle)), label = i18n$t(paste0(toggle, "_user_access_description"))),
            style = "margin-left: 40px;"
          )
        )
      }
    }
    
    sub_results <- div(sub_results, style = "margin: 5px 0 15px 0;")
    label <- users_accesses_toggles_options[[i, "name"]]
    
    users_accesses_toggles_options_result <- tagList(
      users_accesses_toggles_options_result,
      div(
        shiny.fluent::Toggle.shinyInput(ns(paste0("toggle_", label)), label = i18n$t(label)),
        div(id = ns(paste0("sub_results_", label, "_div")), sub_results)
      )
    )
  }
  
  
  div(
    class = "main",
    div(
      id = ns("users_div"),
      div(
        div(
          # Users ----
          
          div(
            h1(i18n$t("users")),
            shinyjs::hidden(
              div(
                id = ns("edit_user_icons"),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("delete_user"), iconProps = list(iconName = "Delete")), text = i18n$t("delete_user")),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("cancel_user_updates"), iconProps = list(iconName = "Cancel")), text = i18n$t("cancel_updates")),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("save_user_updates"), iconProps = list(iconName = "Accept")), text = i18n$t("save_updates")),
                class = "small_icon_button",
                style = "display: flex; margin-top: 5px;"
              )
            ),
            shinyjs::hidden(
              div(
                id = ns("add_user_icon"),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("create_user"), iconProps = list(iconName = "Add")), text = i18n$t("create_user")),
                class = "small_icon_button",
                style = "margin-top: 5px;"
              )
            ),
            style = "display: flex; justify-content: space-between;"
          ),
          shinyjs::hidden(DT::DTOutput(ns("users_dt"))),
          div(
            id = ns("users_management_forbidden_access"),
            shiny.fluent::MessageBar(i18n$t("unauthorized_access_area"), messageBarType = 5),
            style = "display: inline-block; margin: 5px;"
          ),
          shinyjs::hidden(
            div(
              id = ns("user_edition_div"),
              div(
                div(shiny.fluent::TextField.shinyInput(ns("edit_user_username"), label = i18n$t("login")), style = "width: 200px;"),
                div(shiny.fluent::DefaultButton.shinyInput(ns("edit_user_password"), i18n$t("change_password"), style = "width: 200px; margin-top: 26px;")),
                style = "display: flex; gap: 10px;"
              ),
              div(
                div(shiny.fluent::TextField.shinyInput(ns("edit_user_firstname"), label = i18n$t("firstname")), style = "width: 200px;"),
                div(shiny.fluent::TextField.shinyInput(ns("edit_user_lastname"), label = i18n$t("lastname")), style = "width: 200px;"),
                style = "display: flex; gap: 10px;"
              ),
              div(shiny.fluent::Dropdown.shinyInput(ns("edit_user_status"), label = i18n$t("user_status")), style = "width: 200px;"),
              div(shiny.fluent::Dropdown.shinyInput(ns("edit_user_access"), label = i18n$t("user_access")), style = "width: 200px;")
            )
          ),
          class = "widget", style = "height: 50%;"
        ),
        
        # Users statuses----
        
        div(
          div(
            h1(i18n$t("users_statuses")),
            shinyjs::hidden(
              div(
                id = ns("edit_user_status_icons"),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("delete_user_status"), iconProps = list(iconName = "Delete")), text = i18n$t("delete_user_status")),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("cancel_user_status_updates"), iconProps = list(iconName = "Cancel")), text = i18n$t("cancel_updates")),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("save_user_status_updates"), iconProps = list(iconName = "Accept")), text = i18n$t("save_updates")),
                class = "small_icon_button",
                style = "display: flex; margin-top: 5px;"
              )
            ),
            shinyjs::hidden(
              div(
                id = ns("add_user_status_icon"),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("create_user_status"), iconProps = list(iconName = "Add")), text = i18n$t("create_user_status")),
                class = "small_icon_button",
                style = "margin-top: 5px;"
              )
            ),
            style = "display: flex; justify-content: space-between;"
          ),
          shinyjs::hidden(DT::DTOutput(ns("users_statuses_dt"))),
          div(
            id = ns("users_statuses_management_forbidden_access"),
            shiny.fluent::MessageBar(i18n$t("unauthorized_access_area"), messageBarType = 5),
            style = "display: inline-block; margin: 5px;"
          ),
          shinyjs::hidden(
            div(
              id = ns("user_status_edition_div"),
              div(shiny.fluent::TextField.shinyInput(ns("edit_user_status_name"), label = i18n$t("name")), style = "width: 200px;"),
              div(shiny.fluent::TextField.shinyInput(ns("edit_user_status_description"), label = i18n$t("description")), style = "width: 100%;"),
            )
          ),
          class = "widget", style = "height: 50%;"
        ),
        class = "users_left"
      ),
      
      # Users accesses ----
      
      div(
        div(
          div(
            h1(i18n$t("users_accesses")),
            shinyjs::hidden(
              div(
                id = ns("edit_user_access_icons"),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("delete_user_access"), iconProps = list(iconName = "Delete")), text = i18n$t("delete_user_access")),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("cancel_user_access_updates"), iconProps = list(iconName = "Cancel")), text = i18n$t("cancel_updates")),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("save_user_access_updates"), iconProps = list(iconName = "Accept")), text = i18n$t("save_updates")),
                class = "small_icon_button",
                style = "display: flex; margin-top: 5px;"
              )
            ),
            shinyjs::hidden(
              div(
                id = ns("add_user_access_icon"),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("create_user_access"), iconProps = list(iconName = "Add")), text = i18n$t("create_user_access")),
                class = "small_icon_button",
                style = "margin-top: 5px;"
              )
            ),
            style = "display: flex; justify-content: space-between;"
          ),
          shinyjs::hidden(DT::DTOutput(ns("users_accesses_dt"))),
          div(
            id = ns("users_accesses_management_forbidden_access"),
            shiny.fluent::MessageBar(i18n$t("unauthorized_access_area"), messageBarType = 5),
            style = "display: inline-block; margin: 5px;"
          ),
          shinyjs::hidden(
            div(
              id = ns("user_access_edition_div"),
              div(shiny.fluent::TextField.shinyInput(ns("edit_user_access_name"), label = i18n$t("name")), style = "width: 200px;"),
              div(shiny.fluent::TextField.shinyInput(ns("edit_user_access_description"), label = i18n$t("description")), style = "width: 100%;"),
              tags$hr(style = "height: 1px; border: none; background-color: #ccc; margin: 15px 0;"),
              users_accesses_toggles_options_result
            )
          ),
          class = "widget", style = "height: calc(100% - 25px); padding-top: 1px; overflow: auto;"
        ),
        class = "users_right"
      ),
      class = "users_container"
    ),
    
    # Modals ----
    
    # Create a user modal
    shinyjs::hidden(
      div(
        id = ns("create_user_modal"),
        div(
          div(
            tags$h1(i18n$t("create_user")),
            shiny.fluent::IconButton.shinyInput(ns("close_create_user_modal"), iconProps = list(iconName = "ChromeClose")),
            class = "create_element_modal_head small_close_button"
          ),
          div(
            div(
              div(
                div(shiny.fluent::TextField.shinyInput(ns("create_user_username"), label = i18n$t("username")), style = "width: 200px;"),
                div(shiny.fluent::TextField.shinyInput(ns("create_user_password"), label = i18n$t("password"), type = "password", canRevealPassword = TRUE), style = "width: 200px;"),
                style = "display: flex; gap: 10px;"
              ),
              div(
                div(shiny.fluent::TextField.shinyInput(ns("create_user_firstname"), label = i18n$t("firstname")), style = "width: 200px;"),
                div(shiny.fluent::TextField.shinyInput(ns("create_user_lastname"), label = i18n$t("lastname")), style = "width: 200px;"),
                style = "display: flex; gap: 10px;"
              ),
              div(shiny.fluent::Dropdown.shinyInput(ns("create_user_user_status"), label = i18n$t("user_status")), style = "width: 200px;"),
              div(shiny.fluent::Dropdown.shinyInput(ns("create_user_user_access"), label = i18n$t("user_access")), style = "width: 200px;")
            ),
            div(
              shiny.fluent::PrimaryButton.shinyInput(ns("add_user"), i18n$t("add")),
              class = "create_element_modal_buttons"
            ),
          ),
          class = "create_user_modal_content"
        ),
        class = "create_element_modal"
      )
    ),
    
    # Update password modal
    
    shinyjs::hidden(
      div(
        id = ns("edit_user_password_modal"),
        div(
          div(
            tags$h1(i18n$t("change_password")),
            shiny.fluent::IconButton.shinyInput(ns("close_edit_user_password_modal"), iconProps = list(iconName = "ChromeClose")),
            class = "change_password_modal_head small_close_button"
          ),
          div(
            div(shiny.fluent::TextField.shinyInput(ns("new_password_1"), label = i18n$t("new_password"), type = "password", canRevealPassword = TRUE), style = "width: 200px;"),
            div(shiny.fluent::TextField.shinyInput(ns("new_password_2"), label = i18n$t("repeat_new_password"), type = "password", canRevealPassword = TRUE), style = "width: 200px;"),
            div(
              shiny.fluent::PrimaryButton.shinyInput(ns("confirm_password_update"), i18n$t("confirm")),
              class = "change_password_modal_buttons"
            ),
          ),
          class = "change_password_modal_content"
        ),
        class = "change_password_modal"
      )
    ),
    
    # Delete a user modal
    modals$delete_user_modal,
    
    # Create a user status moda
    modals$create_user_status_modal,
    
    # Delete a user status modal
    modals$delete_user_status_modal,
    
    # Create a user access modal
    modals$create_user_access_modal,
    
    # Delete a user access modal
    modals$delete_user_access_modal
  )
}

#' @noRd 
mod_users_server <- function(id, r, d, m, language, i18n, debug, users_accesses_toggles_options, user_accesses){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Current user accesses ----
    
    for (type in c("users", "users_accesses", "users_statuses")){
      single <- switch(type, "users" = "user", "users_accesses" = "user_access", "users_statuses" = "user_status")
      
      if (paste0(type, "_management") %in% user_accesses){
        sapply(c(paste0("add_", single, "_icon"), paste0(type, "_dt")), shinyjs::show)
        shinyjs::hide(paste0(type, "_management_forbidden_access"))
      }
    }
    
    # |----------- USERS --------------------------- -----
    
    # Reload users var ----
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_users', Math.random());"))
    
    observeEvent(input$reload_users, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$reload_users"))
      
      sql <- glue::glue_sql(paste0(
        "SELECT u.id, u.username, u.firstname, u.lastname, (u.firstname || ' ' || u.lastname) AS name, (SUBSTR(u.firstname, 1, 1) || SUBSTR(u.lastname, 1, 1)) AS initials, ",
        "s.id AS user_status_id, s.name AS user_status, a.id AS user_access_id, a.name AS user_access ",
        "FROM users u ",
        "LEFT JOIN users_statuses s ON u.user_status_id = s.id ",
        "LEFT JOIN users_accesses a ON u.user_access_id = a.id"), .con = r$db)

      r$users <- DBI::dbGetQuery(r$db, sql) %>% tibble::as_tibble()
      
      users <- r$users %>% 
        dplyr::select(id, firstname, lastname, user_status, user_access) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          action = as.character(
            div(
              shiny.fluent::IconButton.shinyInput(
                ns(paste0("edit_user_", id)), iconProps = list(iconName = "Edit"),
                onClick = htmlwidgets::JS(paste0(
                  "item => {",
                     "Shiny.setInputValue('users-edit_user_id', ", id, ");",
                     "Shiny.setInputValue('users-edit_user_trigger', Math.random());",
                  "}"
                ))
              ),
              class = "small_icon_button small_icon_button_dt"
            )
          )
        )
      
      col_names <- c(i18n$t("id"), i18n$t("firstname"), i18n$t("lastname"), i18n$t("user_status"), i18n$t("user_access"), i18n$t("action"))

      sortable_cols <- c("firstname", "lastname", "user_status", "user_access")
      searchable_cols <- c("firstname", "lastname", "user_status", "user_access")
      factorize_cols <- c("user_status", "user_access")
      column_widths <- c("id" = "50px", "action" = "50px")
      centered_cols <- "action"

      render_datatable(
        output = output, ns = ns, i18n = i18n, data = users, selection = "none",
        output_name = "users_dt", datatable_dom = "<'top't><'bottom'p>", sortable_cols = sortable_cols, centered_cols = centered_cols,
        col_names = col_names, searchable_cols = searchable_cols, factorize_cols = factorize_cols, filter = TRUE, column_widths = column_widths
      )
    })
    
    # Create user ----
    
    ## Open modal
    observeEvent(input$create_user, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$create_user"))
      shinyjs::show("create_user_modal")
    })
    
    ## Close modal
    observeEvent(input$close_create_user_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$close_create_user_modal"))
      shinyjs::hide("create_user_modal")
    })
    
    ## Creation confirmed
    
    observeEvent(input$add_user, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$add_user"))
      
      req("users_management" %in% user_accesses)
      
      # Check if textfields are not empty
      empty <- list()
      
      for (field in c("username", "password", "firstname", "lastname")){
        empty[[field]] <- TRUE
        name <- input[[paste0("create_user_", field)]]
        if (length(name) > 0) if (!is.na(name) & name != "") empty[[field]] <- FALSE
        if (empty[[field]]) shiny.fluent::updateTextField.shinyInput(session, paste0("create_user_", field), errorMessage = i18n$t("provide_valid_value"))
        else shiny.fluent::updateTextField.shinyInput(session, paste0("create_user_", field), errorMessage = NULL)
      }
      
      req(!empty$username, !empty$password, !empty$firstname, !empty$lastname)
      
      # Check if username is not already used
      sql <- glue::glue_sql("SELECT id FROM users WHERE LOWER(username) = {tolower(input$create_user_username)}", .con = r$db)
      username_already_used <- nrow(DBI::dbGetQuery(r$db, sql) > 0)

      if (username_already_used) shiny.fluent::updateTextField.shinyInput(session, "create_user_username", errorMessage = i18n$t("username_already_used"))
      req(!username_already_used)
      shiny.fluent::updateTextField.shinyInput(session, "create_user_username", errorMessage = NULL)
      
      # Check if firstname + lastname are not already used
      sql <- glue::glue_sql("SELECT id FROM users WHERE LOWER(firstname) = {tolower(input$create_user_firstname)} AND LOWER(lastname) = {tolower(input$create_user_lastname)}", .con = r$db)
      name_already_used <- nrow(DBI::dbGetQuery(r$db, sql) > 0)

      if (name_already_used) for (field in c("firstname", "lastname")) shiny.fluent::updateTextField.shinyInput(session, paste0("create_user_", field), errorMessage = i18n$t("firstname_and_lastname_already_used"))
      req(!name_already_used)
      for (field in c("firstname", "lastname")) shiny.fluent::updateTextField.shinyInput(session, paste0("create_user_", field), errorMessage = NULL)
      
      # Check if dropdowns are not empty
      empty <- list()
      
      for (field in c("user_status", "user_access")){
        if (field == "user_access") dropdown_options <- r$users_accesses
        else if (field == "user_status") dropdown_options <- r$users_statuses
        dropdown_options <- dropdown_options %>% convert_tibble_to_list(key_col = "id", text_col = "name")
        
        if (length(input[[paste0("create_user_", field)]]) == 0) shiny.fluent::updateDropdown.shinyInput(session, paste0("create_user_", field), errorMessage = i18n$t("provide_valid_value"), options = dropdown_options)
        else shiny.fluent::updateDropdown.shinyInput(session, paste0("create_user_", field), errorMessage = NULL, options = dropdown_options)
        req(length(input[[paste0("create_user_", field)]]) > 0)
      }
      
      # Add new data in db
      
      new_data <- tibble::tibble(
        id = get_last_row(r$db, "users") + 1, username = input$create_user_username,
        firstname = input$create_user_firstname, lastname = input$create_user_lastname, password = bcrypt::hashpw(input$create_user_password),
        user_access_id = input$create_user_user_access, user_status_id = input$create_user_user_status, datetime = now(), deleted = FALSE
      )
      DBI::dbAppendTable(r$db, "users", new_data)
      
      # Reload textfields
      for (field in c("username", "password", "firstname", "lastname")) shiny.fluent::updateTextField.shinyInput(session, paste0("create_user_", field), value = "")
      
      # Reload users var and DT
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_users', Math.random());"))
      
      # Notify user
      show_message_bar(id, output, "user_added", "success", i18n = i18n, ns = ns)
      
      shinyjs::hide("create_user_modal")
    })
    
    # Edit user ----
    
    observeEvent(input$edit_user_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$edit_user_trigger"))
      
      req("users_management" %in% user_accesses)
      
      # Update fields
      user <- r$users %>% dplyr::filter(id == input$edit_user_id)
      
      for (field in c("username", "firstname", "lastname")) shiny.fluent::updateTextField.shinyInput(session, paste0("edit_user_", field), value = user[[field]])
      for (field in c("user_access", "user_status")){
        if (field == "user_access") dropdown_options <- r$users_accesses
        else if (field == "user_status") dropdown_options <- r$users_statuses
        dropdown_options <- dropdown_options %>% convert_tibble_to_list(key_col = "id", text_col = "name")
        shiny.fluent::updateDropdown.shinyInput(session, paste0("edit_", field), options = dropdown_options, value = user[[paste0(field, "_id")]])
      }
      
      sapply(c("users_dt", "add_user_icon"), shinyjs::hide)
      sapply(c("user_edition_div", "edit_user_icons"), shinyjs::show)
    })
    
    observeEvent(input$edit_user_password, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$edit_user_password"))
      
      shinyjs::show("edit_user_password_modal")
    })
    
    observeEvent(input$close_edit_user_password_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$close_edit_user_password_modal"))
      
      shinyjs::hide("edit_user_password_modal")
    })
    
    observeEvent(input$confirm_password_update, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$confirm_password_update"))
      
      empty_fields <- list()
      for (field in c("new_password_1", "new_password_2")) {
        empty_fields[[field]] <- TRUE
        field_value <- input[[field]]
        if (length(field_value) > 0) if (!is.na(field_value) & field_value != "") empty_fields[[field]] <- FALSE
        if (empty_fields[[field]]) shiny.fluent::updateTextField.shinyInput(session, field, errorMessage = i18n$t("provide_valid_value"))
        else shiny.fluent::updateTextField.shinyInput(session, field, errorMessage = NULL)
      }
      req(!TRUE %in% empty_fields)
      
      passwords_match <- input$new_password_1 == input$new_password_2
      if (!passwords_match) {
        shiny.fluent::updateTextField.shinyInput(session, "new_password_2", errorMessage = i18n$t("passwords_dont_match"))
        req(FALSE)
      }
      
      hashed_password <- bcrypt::hashpw(input$new_password_1)
      
      sql <- glue::glue_sql("UPDATE users SET password = {hashed_password} WHERE id = {input$edit_user_id}", .con = r$db)
      DBI::dbExecute(r$db, sql)
      
      for (field in c("new_password_1", "new_password_2")) shiny.fluent::updateTextField.shinyInput(session, field, value = "")
      
      shinyjs::hide("edit_user_password_modal")
      
      show_message_bar(id, output, "password_successfully_updated", "success", i18n = i18n, ns = ns)
    })
    
    # Save updates
    
    observeEvent(input$save_user_updates, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$save_user_updates"))
      
      req("users_management" %in% user_accesses)
      
      # Check if textfields are not empty
      empty <- list()
      
      for (field in c("username", "firstname", "lastname")){
        empty[[field]] <- TRUE
        name <- input[[paste0("edit_user_", field)]]
        if (length(name) > 0) if (!is.na(name) & name != "") empty[[field]] <- FALSE
        if (empty[[field]]) shiny.fluent::updateTextField.shinyInput(session, paste0("edit_user_", field), errorMessage = i18n$t("provide_valid_value"))
        else shiny.fluent::updateTextField.shinyInput(session, paste0("edit_user_", field), errorMessage = NULL)
      }
      
      req(!empty$username, !empty$firstname, !empty$lastname)
      
      # Check if dropdowns are not empty (when a user status or access is deleted)
      empty <- list()
      
      for (field in c("user_access", "user_status")){
        empty <- TRUE
        
        if (field == "user_access") data <- r$users_accesses
        else if (field == "user_status") data <- r$users_statuses
        dropdown_options <- data %>% convert_tibble_to_list(key_col = "id", text_col = "name")
        
        if (length(input[[paste0("edit_", field)]]) > 0) if (input[[paste0("edit_", field)]] %in% data$id) empty <- FALSE
          
        if (empty) shiny.fluent::updateDropdown.shinyInput(session, paste0("edit_", field), errorMessage = i18n$t("provide_valid_value"), options = dropdown_options)
        else shiny.fluent::updateDropdown.shinyInput(session, paste0("edit_", field), errorMessage = NULL, options = dropdown_options)
        req(!empty)
      }
      
      # Check if username is not already used
      sql <- glue::glue_sql("SELECT id FROM users WHERE LOWER(username) = {tolower(input$edit_user_username)} AND id != {input$edit_user_id}", .con = r$db)
      username_already_used <- nrow(DBI::dbGetQuery(r$db, sql) > 0)
      
      if (username_already_used) shiny.fluent::updateTextField.shinyInput(session, "edit_user_username", errorMessage = i18n$t("username_already_used"))
      req(!username_already_used)
      shiny.fluent::updateTextField.shinyInput(session, "edit_user_username", errorMessage = NULL)
      
      # Check if firstname + lastname are not already used
      sql <- glue::glue_sql("SELECT id FROM users WHERE LOWER(firstname) = {tolower(input$edit_user_firstname)} AND LOWER(lastname) = {tolower(input$edit_user_lastname)} AND id != {input$edit_user_id}", .con = r$db)
      name_already_used <- nrow(DBI::dbGetQuery(r$db, sql) > 0)
      
      if (name_already_used) for (field in c("firstname", "lastname")) shiny.fluent::updateTextField.shinyInput(session, paste0("edit_user_", field), errorMessage = i18n$t("firstname_and_lastname_already_used"))
      req(!name_already_used)
      for (field in c("firstname", "lastname")) shiny.fluent::updateTextField.shinyInput(session, paste0("edit_user_", field), errorMessage = NULL)
      
      # Save updates in db
      sql <- glue::glue_sql(paste0(
        "UPDATE users SET username = {input$edit_user_username}, firstname = {input$edit_user_firstname}, ",
        "lastname = {input$edit_user_lastname}, user_status_id = {input$edit_user_status}, user_access_id = {input$edit_user_access} ",
        "WHERE id = {input$edit_user_id}"
      ), .con = r$db)
      sql_send_statement(r$db, sql)
      
      # Reload users var and DT
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_users', Math.random());"))
      
      # Notify user
      show_message_bar(id, output, "modif_saved", "success", i18n = i18n, ns = ns)
      
      sapply(c("user_edition_div", "edit_user_icons"), shinyjs::hide)
      sapply(c("users_dt", "add_user_icon"), shinyjs::show)
    })
    
    # Cancel updates
    
    observeEvent(input$cancel_user_updates, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$cancel_user_updates"))
      
      sapply(c("user_edition_div", "edit_user_icons"), shinyjs::hide)
      sapply(c("users_dt", "add_user_icon"), shinyjs::show)
    })
    
    # Delete user ----
    
    ## Open modal
    observeEvent(input$delete_user, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$delete_user"))
      shinyjs::show("delete_user_modal")
    })
    
    ## Close modal
    observeEvent(input$close_user_deletion_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$close_user_deletion_modal"))
      
      shinyjs::hide("delete_user_modal")
    })
    
    ## Deletion confirmed
    observeEvent(input$confirm_user_deletion, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$confirm_user_deletion"))
      
      req("users_management" %in% user_accesses)
      
      # Delete user from db
      sql <- glue::glue_sql("DELETE FROM users WHERE id = {input$edit_user_id}", .con = r$db)
      sql_send_statement(r$db, sql)
      
      # Reload users var and DT
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_users', Math.random());"))
      
      # Notify user
      show_message_bar(id, output, "user_deleted", "warning", i18n = i18n, ns = ns)
      
      sapply(c("user_edition_div", "edit_user_icons", "delete_user_modal"), shinyjs::hide)
      sapply(c("users_dt", "add_user_icon"), shinyjs::show)
    })
    
    # |----------- USERS STATUSES ---------- -----
    
    # Reload user statuses var ----
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_users_statuses', Math.random());"))
    
    observeEvent(input$reload_users_statuses, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$reload_users_statuses"))
      reload_user_attribute("user_status")
    })
    
    # Create user status ----
    
    ## Open modal
    observeEvent(input$create_user_status, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$create_user_status"))
      shinyjs::show("create_user_status_modal")
    })
    
    ## Close modal
    observeEvent(input$close_create_user_status_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$close_create_user_status_modal"))
      shinyjs::hide("create_user_status_modal")
    })
    
    ## Creation confirmed
    
    observeEvent(input$add_user_status, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$add_user_status"))
      
      req("users_statuses_management" %in% user_accesses)
      add_user_attribute("user_status")
    })
    
    # Edit user status ----
    
    observeEvent(input$edit_user_status_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$edit_user_status_trigger"))
      
      # Update fields
      user_status <- r$users_statuses %>% dplyr::filter(id == input$edit_user_status_id)
      
      for (field in c("name", "description")) shiny.fluent::updateTextField.shinyInput(session, paste0("edit_user_status_", field), value = user_status[[field]])
      
      sapply(c("users_statuses_dt", "add_user_status_icon"), shinyjs::hide)
      sapply(c("user_status_edition_div", "edit_user_status_icons"), shinyjs::show)
    })
    
    # Save updates
    
    observeEvent(input$save_user_status_updates, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$save_user_status_updates"))
      
      req("users_statuses_management" %in% user_accesses)
      save_user_attribute_updates("user_status")
    })
    
    # Cancel updates
    
    observeEvent(input$cancel_user_status_updates, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$cancel_user_status_updates"))
      
      sapply(c("user_status_edition_div", "edit_user_status_icons"), shinyjs::hide)
      sapply(c("users_statuses_dt", "add_user_status_icon"), shinyjs::show)
    })
    
    # Delete user status ----
    
    ## Open modal
    observeEvent(input$delete_user_status, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$delete_user_status"))
      shinyjs::show("delete_user_status_modal")
    })
    
    ## Close modal
    observeEvent(input$close_user_status_deletion_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$close_user_status_deletion_modal"))
      
      shinyjs::hide("delete_user_status_modal")
    })
    
    ## Deletion confirmed
    observeEvent(input$confirm_user_status_deletion, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$confirm_user_status_deletion"))
      
      req("users_statuses_management" %in% user_accesses)
      delete_user_attribute("user_status")
    })
    
    # |----------- USERS ACCESSES --------- -----
    
    # Reload user accesses var ----
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_users_accesses', Math.random());"))
    
    observeEvent(input$reload_users_accesses, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$reload_users_accesses"))
      
      reload_user_attribute("user_access")
    })
    
    # Create user access ----
    
    ## Open modal
    observeEvent(input$create_user_access, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$create_user_access"))
      shinyjs::show("create_user_access_modal")
    })
    
    ## Close modal
    observeEvent(input$close_create_user_access_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$close_create_user_access_modal"))
      shinyjs::hide("create_user_access_modal")
    })
    
    ## Creation confirmed
    
    observeEvent(input$add_user_access, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$add_user_access"))
      
      req("users_accesses_management" %in% user_accesses)
      add_user_attribute("user_access")
    })
    
    # Edit user access ----
    
    observeEvent(input$edit_user_access_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$edit_user_access_trigger"))
      
      # Update fields
      user_access <- r$users_accesses %>% dplyr::filter(id == input$edit_user_access_id)
      
      for (field in c("name", "description")) shiny.fluent::updateTextField.shinyInput(session, paste0("edit_user_access_", field), value = user_access[[field]])
      
      sapply(c("users_accesses_dt", "add_user_access_icon"), shinyjs::hide)
      sapply(c("user_access_edition_div", "edit_user_access_icons"), shinyjs::show)
      
      # Update toggles
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_user_accesses_toggles', Math.random());"))
    })
    
    # Save updates
    
    observeEvent(input$save_user_access_updates, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$save_user_access_updates"))
      
      req("users_accesses_management" %in% user_accesses)
      save_user_attribute_updates("user_access")
    })
    
    # Cancel updates
    
    observeEvent(input$cancel_user_access_updates, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$cancel_user_access_updates"))
      
      sapply(c("user_access_edition_div", "edit_user_access_icons"), shinyjs::hide)
      sapply(c("users_accesses_dt", "add_user_access_icon"), shinyjs::show)
    })
    
    # Show / hide toggles
    
    sapply(1:nrow(users_accesses_toggles_options), function(i){
      
      # Show or hide toggles
      label <- users_accesses_toggles_options[[i, "name"]]
      
      observeEvent(input[[paste0("toggle_", label)]], {
        if (debug) cat(paste0("\n", now(), " - mod_users - observer input$toggle_", label))
        if (input[[paste0("toggle_", label)]]) shinyjs::show(paste0("sub_results_", label, "_div"))
        else shinyjs::hide(paste0("sub_results_", label, "_div"))
      })
    })
    
    # Update toggles
    
    observeEvent(input$update_user_accesses_toggles, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$update_user_accesses_toggles"))
      
      sql <- glue::glue_sql("SELECT name, value_num FROM options WHERE category = 'users_accesses' AND link_id = {input$edit_user_access_id} AND value_num = 1", .con = r$db)
      current_data <- DBI::dbGetQuery(r$db, sql)
      
      for (i in 1:nrow(users_accesses_toggles_options)){
        
        user_access_name <- users_accesses_toggles_options[[i, "name"]]
        value <- FALSE
        if (user_access_name %in% current_data$name) value <- TRUE
        shiny.fluent::updateToggle.shinyInput(session, paste0("toggle_", user_access_name), value = value)
        
        if (users_accesses_toggles_options[[i, "toggles"]] != ""){
          for (toggle in users_accesses_toggles_options[[i, "toggles"]][[1]]){
            value <- FALSE
            if (toggle %in% current_data$name) value <- TRUE
            shiny.fluent::updateToggle.shinyInput(session, paste0("toggle_", toggle), value = value)
          }
        }
      }
    })
    
    # Delete user access ----
    
    ## Open modal
    observeEvent(input$delete_user_access, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$delete_user_access"))
      shinyjs::show("delete_user_access_modal")
    })
    
    ## Close modal
    observeEvent(input$close_user_access_deletion_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$close_user_access_deletion_modal"))
      
      shinyjs::hide("delete_user_access_modal")
    })
    
    ## Deletion confirmed
    observeEvent(input$confirm_user_access_deletion, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$confirm_user_access_deletion"))
      
      req("users_accesses_management" %in% user_accesses)
      delete_user_attribute("user_access")
    })
    
    # |----------- MODAL FUNCTIONS -------- -----
    
    add_user_attribute <- function(type){
      
      table <- switch(type, "user_access" = "users_accesses", "user_status" = "users_statuses")
      
      # Check if textfields are not empty
      empty_name <- TRUE
      name <- input[[paste0("create_", type, "_name")]]
      if (length(name) > 0) if (!is.na(name) & name != "") empty_name <- FALSE
      if (empty_name) shiny.fluent::updateTextField.shinyInput(session, paste0("create_", type, "_name"), errorMessage = i18n$t("provide_valid_value"))
      else shiny.fluent::updateTextField.shinyInput(session, paste0("create_", type, "_name"), errorMessage = NULL)
      
      req(!empty_name)
      
      # Check if name is not already used
      sql <- glue::glue_sql("SELECT id FROM {`table`} WHERE LOWER(name) = {tolower(input[[paste0('create_', type, '_name')]])}", .con = r$db)
      name_already_used <- nrow(DBI::dbGetQuery(r$db, sql) > 0)
      
      if (name_already_used) shiny.fluent::updateTextField.shinyInput(session, paste0("create_", type, "_name"), errorMessage = i18n$t("name_already_used"))
      req(!name_already_used)
      shiny.fluent::updateTextField.shinyInput(session, paste0("create_", type, "_name"), errorMessage = NULL)
      
      description <- ""
      if (length(input[[paste0("create_", type, "_description")]]) > 0) description <- input[[paste0("create_", type, "_description")]]
      
      # Add new data in db
      
      new_data <- tibble::tibble(
        id = get_last_row(r$db, "users_statuses") + 1, name = input[[paste0("create_", type, "_name")]], description = description,
        datetime = now(), deleted = FALSE
      )
      DBI::dbAppendTable(r$db, table, new_data)
      
      # Reload textfields
      for (field in c("name", "description")) shiny.fluent::updateTextField.shinyInput(session, paste0("create_", type, "_", field), value = "")
      
      # Reload users var and DT
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_", table, "', Math.random());"))
      
      # Notify user
      show_message_bar(id, output, paste0(type, "_added"), "success", i18n = i18n, ns = ns)
      
      shinyjs::hide(paste0("create_", type, "_modal"))
    }
    
    delete_user_attribute <- function(type){
      
      table <- switch(type, "user_access" = "users_accesses", "user_status" = "users_statuses")
      
      # Delete user from db
      sql <- glue::glue_sql("DELETE FROM {`table`} WHERE id = {input[[paste0('edit_', type, '_id')]]}", .con = r$db)
      sql_send_statement(r$db, sql)
      
      # Reload users var and DT
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_", table, "', Math.random());"))
      
      # Notify user
      show_message_bar(id, output, paste0(type, "_deleted"), "warning", i18n = i18n, ns = ns)
      
      sapply(c(paste0(type, "_edition_div"), paste0("edit_", type, "_icons"), paste0("delete_", type, "_modal")), shinyjs::hide)
      sapply(c(paste0(table, "_dt"), paste0("add_", type, "_icon")), shinyjs::show)
    }
    
    reload_user_attribute <- function(type){
      
      table <- switch(type, "user_access" = "users_accesses", "user_status" = "users_statuses")
      
      sql <- glue::glue_sql("SELECT * FROM {`table`}", .con = r$db)
      r[[table]] <- DBI::dbGetQuery(r$db, sql)
      
      # Update dropdowns
      
      dropdown_options <- convert_tibble_to_list(r[[table]], key_col = "id", text_col = "name")
      value <- NULL
      if (length(input[[paste0("edit_", type)]]) > 0) value <- input[[paste0("edit_", type)]]
      
      shiny.fluent::updateDropdown.shinyInput(session, paste0("edit_", type), options = dropdown_options, value = value)
      shiny.fluent::updateDropdown.shinyInput(session, paste0("create_user_", type), options = dropdown_options)
      
      # Update DT
      
      users_statuses <- 
        r[[table]] %>% 
        dplyr::select(id, name, description) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          action = as.character(
            div(
              shiny.fluent::IconButton.shinyInput(
                ns(paste0("edit_", type, "_", id)), iconProps = list(iconName = "Edit"),
                onClick = htmlwidgets::JS(paste0(
                  "item => {",
                  "Shiny.setInputValue('users-edit_", type, "_id', ", id, ");",
                  "Shiny.setInputValue('users-edit_", type, "_trigger', Math.random());",
                  "}"
                ))
              ),
              class = "small_icon_button small_icon_button_dt"
            )
          )
        )
      
      col_names <- c(i18n$t("id"), i18n$t("name"), i18n$t("description"), i18n$t("action"))
      
      sortable_cols <- c("id", "name")
      searchable_cols <- "name"
      column_widths <- c("id" = "50px", "name" = "100px", "action" = "50px")
      centered_cols <- "action"
      
      render_datatable(
        output = output, ns = ns, i18n = i18n, data = users_statuses, selection = "none",
        output_name = paste0(table, "_dt"), datatable_dom = "<'top't><'bottom'p>", sortable_cols = sortable_cols, centered_cols = centered_cols,
        col_names = col_names, searchable_cols = searchable_cols, filter = TRUE, column_widths = column_widths
      )
    }
    
    save_user_attribute_updates <- function(type){
      
      table <- switch(type, "user_access" = "users_accesses", "user_status" = "users_statuses")
      
      empty_name <- TRUE
      name <- input[[paste0("edit_", type, "_name")]]
      if (length(name) > 0) if (!is.na(name) & name != "") empty_name <- FALSE
      if (empty_name) shiny.fluent::updateTextField.shinyInput(session, paste0("edit_", type, "_name"), errorMessage = i18n$t("provide_valid_value"))
      else shiny.fluent::updateTextField.shinyInput(session, paste0("edit_", type, "_name"), errorMessage = NULL)
      
      req(!empty_name)
      
      # Check if name is not already used
      sql <- glue::glue_sql("SELECT id FROM {`table`} WHERE LOWER(name) = {tolower(input[[paste0('edit_', type, '_name')]])} AND id != {input[[paste0('edit_', type, '_id')]]}", .con = r$db)
      name_already_used <- nrow(DBI::dbGetQuery(r$db, sql) > 0)
      
      if (name_already_used) shiny.fluent::updateTextField.shinyInput(session, paste0("edit_", type, "_name"), errorMessage = i18n$t("name_already_used"))
      req(!name_already_used)
      shiny.fluent::updateTextField.shinyInput(session, paste0("edit_", type, "_name"), errorMessage = NULL)
      
      description <- ""
      if (length(input[[paste0("edit_", type, "_description")]]) > 0) description <- input[[paste0("edit_", type, "_description")]]
      
      # Save updates in db
      sql <- glue::glue_sql(paste0(
        "UPDATE {`table`} SET name = {input[[paste0('edit_', type, '_name')]]}, description = {description} ",
        "WHERE id = {input[[paste0('edit_', type, '_id')]]}"
      ), .con = r$db)
      sql_send_statement(r$db, sql)
      
      # Add user accesses in db
      if (type == "user_access"){
        
        # Delete old rows
        sql <- glue::glue_sql("DELETE FROM options WHERE category = 'users_accesses' AND link_id = {input$edit_user_access_id}", .con = r$db)
        sql_send_statement(r$db, sql)
        
        # Add new ones
        new_data <- tibble::tibble(
          category = character(), link_id = integer(), name = character(), value = character(),
          value_num = numeric(), creator_id = integer(), datetime = character(), deleted = logical()
        )
        
        # Loop over all toggles
        
        for (i in 1:nrow(users_accesses_toggles_options)){
          
          user_access_name <- users_accesses_toggles_options[[i, "name"]]
          
          new_data <- new_data %>% dplyr::bind_rows(
            tibble::tibble(
              category = "users_accesses", 
              link_id = input$edit_user_access_id, 
              name = user_access_name,
              value = "",
              value_num = as.numeric(input[[paste0("toggle_", user_access_name)]]),
              creator_id = r$user_id,
              datetime = now(),
              deleted = FALSE
            )
          )
          
          if (users_accesses_toggles_options[[i, "toggles"]] != ""){
            
            for (toggle in users_accesses_toggles_options[[i, "toggles"]][[1]]){
              
              value_num <- as.numeric(input[[paste0("toggle_", toggle)]])
              
              if (input[[paste0("toggle_", user_access_name)]] == 0) value_num <- 0
              
              new_data <- new_data %>% dplyr::bind_rows(
                tibble::tibble(
                  category = "users_accesses", 
                  link_id = input$edit_user_access_id, 
                  name = toggle,
                  value = "",
                  value_num = value_num,
                  creator_id = r$user_id,
                  datetime = now(),
                  deleted = FALSE
                )
              )
            }
          }
        }
        
        new_data <- new_data %>% dplyr::mutate(id = get_last_row(r$db, "options") + 1 + dplyr::row_number(), .before = "category")
        
        DBI::dbAppendTable(r$db, "options", new_data)
      }
      
      # Reload users var and DT
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_", table, "', Math.random());"))
      
      # Notify user
      show_message_bar(id, output, "modif_saved", "success", i18n = i18n, ns = ns)
      
      sapply(c(paste0(type, "_edition_div"), paste0("edit_", type, "_icons")), shinyjs::hide)
      sapply(c(paste0(table, "_dt"), paste0("add_", type, "_icon")), shinyjs::show)
    }
  })
}
