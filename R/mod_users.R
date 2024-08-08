#' @noRd
mod_users_ui <- function(id, language, languages, i18n, users_accesses_toggles_options){
  ns <- NS(id)
  
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
            div(
              id = ns("add_user_icon"),
              create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("create_user"), iconProps = list(iconName = "Add")), text = i18n$t("create_user")),
              class = "small_icon_button",
              style = "margin-top: 5px;"
            ),
            style = "display: flex; justify-content: space-between;"
          ),
          DT::DTOutput(ns("users_dt")),
          shinyjs::hidden(
            div(
              id = ns("user_edition_div"),
              div(shiny.fluent::TextField.shinyInput(ns("edit_user_username"), label = i18n$t("login")), style = "width: 200px;"),
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
            div(
              id = ns("add_user_status_icon"),
              create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("create_user_status"), iconProps = list(iconName = "Add")), text = i18n$t("create_user_status")),
              class = "small_icon_button",
              style = "margin-top: 5px;"
            ),
            style = "display: flex; justify-content: space-between;"
          ),
          DT::DTOutput(ns("users_statuses_dt")),
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
          h1(i18n$t("users_accesses")),
          class = "widget", style = "height: calc(100% - 25px); padding-top: 1px;"
        ),
        class = "users_right"
      ),
      class = "users_container"
    ),
    
    # Create a user modal ----
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
              div(shiny.fluent::TextField.shinyInput(ns("create_user_username"), label = i18n$t("username")), style = "width: 200px;"),
              div(shiny.fluent::TextField.shinyInput(ns("create_user_password"), label = i18n$t("password"), type = "password", canRevealPassword = TRUE), style = "width: 200px;"),
              div(
                div(shiny.fluent::TextField.shinyInput(ns("create_user_firstname"), label = i18n$t("firstname")), style = "width: 200px;"),
                div(shiny.fluent::TextField.shinyInput(ns("create_user_lastname"), label = i18n$t("lastname")), style = "width: 200px;"),
                style = "display: flex; gap: 10px;"
              ),
              div(shiny.fluent::Dropdown.shinyInput(ns("create_user_status"), label = i18n$t("user_status")), style = "width: 200px;"),
              div(shiny.fluent::Dropdown.shinyInput(ns("create_user_access"), label = i18n$t("user_access")), style = "width: 200px;")
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
    
    # Delete a user modal ----
    shinyjs::hidden(
      div(
        id = ns("delete_user_modal"),
        div(
          tags$h1(i18n$t("delete_user_title")), tags$p(i18n$t("delete_user_text")),
          div(
            shiny.fluent::DefaultButton.shinyInput(ns("close_user_deletion_modal"), i18n$t("dont_delete")),
            div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_user_deletion"), i18n$t("delete")), class = "delete_button"),
            class = "delete_modal_buttons"
          ),
          class = "delete_modal_content"
        ),
        class = "delete_modal"
      )
    ),
    
    # Create a user status modal ----
    shinyjs::hidden(
      div(
        id = ns("create_user_status_modal"),
        div(
          div(
            tags$h1(i18n$t("create_user_status")),
            shiny.fluent::IconButton.shinyInput(ns("close_create_user_status_modal"), iconProps = list(iconName = "ChromeClose")),
            class = "create_element_modal_head small_close_button"
          ),
          div(
            div(
              div(shiny.fluent::TextField.shinyInput(ns("create_user_status_name"), label = i18n$t("name")), style = "width: 200px;"),
              div(shiny.fluent::TextField.shinyInput(ns("create_user_status_description"), label = i18n$t("description")), style = "width: 100%;"),
            ),
            div(
              shiny.fluent::PrimaryButton.shinyInput(ns("add_user_status"), i18n$t("add")),
              class = "create_element_modal_buttons"
            ),
          ),
          class = "create_user_status_modal_content"
        ),
        class = "create_element_modal"
      )
    ),
    
    # Delete a user status modal ----
    # shinyjs::hidden(
    #   div(
    #     id = ns("delete_user_status_modal"),
    #     div(
    #       tags$h1(i18n$t("delete_user_status_title")), tags$p(i18n$t("delete_user_status_text")),
    #       div(
    #         shiny.fluent::DefaultButton.shinyInput(ns("close_user_status_deletion_modal"), i18n$t("dont_delete")),
    #         div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_user_status_deletion"), i18n$t("delete")), class = "delete_button"),
    #         class = "delete_modal_buttons"
    #       ),
    #       class = "delete_modal_content"
    #     ),
    #     class = "delete_modal"
    #   )
    # ),
  )
}

#' @noRd 
mod_users_server <- function(id, r, d, m, language, i18n, debug, users_accesses_toggles_options){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
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
      
      for (field in c("user_access", "user_status")){
        if (field == "user_access") dropdown_options <- r$users_accesses
        else if (field == "user_status") dropdown_options <- r$users_statuses
        dropdown_options <- dropdown_options %>% convert_tibble_to_list(key_col = "id", text_col = "name")
        
        if (length(input[[paste0("create_", field)]]) == 0) shiny.fluent::updateDropdown.shinyInput(session, paste0("create_", field), errorMessage = i18n$t("provide_valid_value"), options = dropdown_options)
        else shiny.fluent::updateDropdown.shinyInput(session, paste0("create_", field), errorMessage = NULL, options = dropdown_options)
        req(length(input[[paste0("create_", field)]]) > 0)
      }
      
      # Add new data in db
      # Hash password with bcrypt
      encrypted_password <- bcrypt::hashpw(input$create_user_password)
      
      new_data <- tibble::tibble(
        id = get_last_row(r$db, "users") + 1, username = input$create_user_username,
        firstname = input$create_user_firstname, lastname = input$create_user_lastname, password = encrypted_password,
        user_access_id = input$create_user_access, user_status_id = input$create_user_status, datetime = now(), deleted = FALSE
      )
      DBI::dbAppendTable(r$db, "users", new_data)
      
      # Reload textfields
      for (field in c("username", "password", "firstname", "lastname")) shiny.fluent::updateTextField.shinyInput(session, paste0("create_user_", field), value = "")
      
      # Reload users var and DT
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_users', Math.random());"))
      
      # Notify user
      show_message_bar(output, "user_added", "success", i18n = i18n, ns = ns)
      
      shinyjs::hide("create_user_modal")
    })
    
    # Edit user ----
    
    observeEvent(input$edit_user_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$edit_user_trigger"))
      
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
    
    # Save updates
    
    observeEvent(input$save_user_updates, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$save_user_updates"))
      
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
      show_message_bar(output, "modif_saved", "success", i18n = i18n, ns = ns)
      
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
      
      # Delete user from db
      sql <- glue::glue_sql("DELETE FROM users WHERE id = {input$edit_user_id}", .con = r$db)
      sql_send_statement(r$db, sql)
      
      # Reload users var and DT
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_users', Math.random());"))
      
      # Notify user
      show_message_bar(output, "user_deleted", "warning", i18n = i18n, ns = ns)
      
      sapply(c("user_edition_div", "edit_user_icons", "delete_user_modal"), shinyjs::hide)
      sapply(c("users_dt", "add_user_icon"), shinyjs::show)
    })
    
    # |----------- USERS STATUSES ---------- -----
    
    # Reload user statuses var ----
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_users_statuses', Math.random());"))
    
    observeEvent(input$reload_users_statuses, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$reload_users_statuses"))
      
      sql <- glue::glue_sql("SELECT * FROM users_statuses", .con = r$db)
      r$users_statuses <- DBI::dbGetQuery(r$db, sql)
      
      # Update dropdowns
      
      dropdown_options <- convert_tibble_to_list(r$users_statuses, key_col = "id", text_col = "name")
      value <- NULL
      if (length(input$edit_user_status) > 0) value <- input$edit_user_status
      
      shiny.fluent::updateDropdown.shinyInput(session, "edit_user_status", options = dropdown_options, value = value)
      shiny.fluent::updateDropdown.shinyInput(session, "create_user_status", options = dropdown_options)
      
      # Update DT
      
      users_statuses <- 
        r$users_statuses %>% 
        dplyr::select(id, name, description) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          action = as.character(
            div(
              shiny.fluent::IconButton.shinyInput(
                ns(paste0("edit_user_status_", id)), iconProps = list(iconName = "Edit"),
                onClick = htmlwidgets::JS(paste0(
                  "item => {",
                  "Shiny.setInputValue('users-edit_user_status_id', ", id, ");",
                  "Shiny.setInputValue('users-edit_user_status_trigger', Math.random());",
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
        output_name = "users_statuses_dt", datatable_dom = "<'top't><'bottom'p>", sortable_cols = sortable_cols, centered_cols = centered_cols,
        col_names = col_names, searchable_cols = searchable_cols, filter = TRUE, column_widths = column_widths
      )
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
      
      # Check if textfields are not empty
      empty_name <- TRUE
      name <- input$create_user_status_name
      if (length(name) > 0) if (!is.na(name) & name != "") empty_name <- FALSE
      if (empty_name) shiny.fluent::updateTextField.shinyInput(session, "create_user_status_name", errorMessage = i18n$t("provide_valid_value"))
      else shiny.fluent::updateTextField.shinyInput(session, "create_user_status_name", errorMessage = NULL)
      
      req(!empty_name)
      
      # Check if name is not already used
      sql <- glue::glue_sql("SELECT id FROM users_statuses WHERE LOWER(name) = {tolower(input$create_user_status_name)}", .con = r$db)
      name_already_used <- nrow(DBI::dbGetQuery(r$db, sql) > 0)
      
      if (name_already_used) shiny.fluent::updateTextField.shinyInput(session, "create_user_status_name", errorMessage = i18n$t("name_already_used"))
      req(!name_already_used)
      shiny.fluent::updateTextField.shinyInput(session, "create_user_status_name", errorMessage = NULL)
      
      description <- ""
      if (length(input$create_user_status_description) > 0) description <- input$create_user_status_description
      
      # Add new data in db
      
      new_data <- tibble::tibble(
        id = get_last_row(r$db, "users_statuses") + 1, name = input$create_user_status_name, description = description,
        datetime = now(), deleted = FALSE
      )
      DBI::dbAppendTable(r$db, "users_statuses", new_data)
      
      # Reload textfields
      for (field in c("name", "description")) shiny.fluent::updateTextField.shinyInput(session, paste0("create_user_status_", field), value = "")

      # Reload users var and DT
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_users_statuses', Math.random());"))

      # Notify user
      show_message_bar(output, "user_status_added", "success", i18n = i18n, ns = ns)
      
      shinyjs::hide("create_user_status_modal")
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
      
      # Check if textfield is not empty
      
      empty_name <- TRUE
      name <- input$edit_user_status_name
      if (length(name) > 0) if (!is.na(name) & name != "") empty_name <- FALSE
      if (empty_name) shiny.fluent::updateTextField.shinyInput(session, "edit_user_status_name", errorMessage = i18n$t("provide_valid_value"))
      else shiny.fluent::updateTextField.shinyInput(session, "edit_user_status_name", errorMessage = NULL)
      
      req(!empty_name)
      
      # Check if name is not already used
      sql <- glue::glue_sql("SELECT id FROM users_statuses WHERE LOWER(name) = {tolower(input$edit_user_status_name)} AND id != {input$edit_user_status_id}", .con = r$db)
      name_already_used <- nrow(DBI::dbGetQuery(r$db, sql) > 0)
      
      if (name_already_used) shiny.fluent::updateTextField.shinyInput(session, "edit_user_status_name", errorMessage = i18n$t("name_already_used"))
      req(!name_already_used)
      shiny.fluent::updateTextField.shinyInput(session, "edit_user_status_name", errorMessage = NULL)
      
      description <- ""
      if (length(input$edit_user_status_description) > 0) description <- input$edit_user_status_description
      
      # Save updates in db
      sql <- glue::glue_sql(paste0(
        "UPDATE users_statuses SET name = {input$edit_user_status_name}, description = {description} ",
        "WHERE id = {input$edit_user_status_id}"
      ), .con = r$db)
      sql_send_statement(r$db, sql)
      
      # Reload users var and DT
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_users_statuses', Math.random());"))
      
      # Notify user
      show_message_bar(output, "modif_saved", "success", i18n = i18n, ns = ns)
      
      sapply(c("user_status_edition_div", "edit_user_status_icons"), shinyjs::hide)
      sapply(c("users_statuses_dt", "add_user_status_icon"), shinyjs::show)
    })
    
    # Cancel updates
    
    observeEvent(input$cancel_user_status_updates, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$cancel_user_status_updates"))
      
      sapply(c("user_status_edition_div", "edit_user_status_icons"), shinyjs::hide)
      sapply(c("users_statuses_dt", "add_user_status_icon"), shinyjs::show)
    })
    
    # |----------- USERS ACCESSES --------- -----
    
    # Reload user accesses var ----
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_users_accesses', Math.random());"))
    
    observeEvent(input$reload_users_accesses, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$reload_users_accesses"))
      
      sql <- glue::glue_sql("SELECT * FROM users_accesses", .con = r$db)
      r$users_accesses <- DBI::dbGetQuery(r$db, sql)
      
      dropdown_options <- convert_tibble_to_list(r$users_accesses, key_col = "id", text_col = "name")
      value <- NULL
      if (length(input$edit_user_access) > 0) value <- input$edit_user_access
      
      shiny.fluent::updateDropdown.shinyInput(session, "edit_user_access", options = dropdown_options, value = value)
      shiny.fluent::updateDropdown.shinyInput(session, "create_user_access", options = dropdown_options)
    })
  })
}
