#' @noRd
mod_users_ui <- function(id, language, languages, i18n, users_accesses_toggles_options){
  ns <- NS(id)
  
  div(
    class = "main",
    div(
      id = ns("users_div"),
      div(
        div(
          h1(i18n$t("users")),
          DT::DTOutput(ns("users_dt")),
          class = "widget", style = "height: 50%;"
        ),
        div(
          h1(i18n$t("users_statuses")),
          class = "widget", style = "height: 50%;"
        ),
        class = "users_left"
      ),
      div(
        div(
          h1(i18n$t("users_accesses")),
          class = "widget", style = "height: calc(100% - 25px); padding-top: 1px;"
        ),
        class = "users_right"
      ),
      class = "users_container"
    )
  )
}

#' @noRd 
mod_users_server <- function(id, r, d, m, language, i18n, debug, users_accesses_toggles_options){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Reload users var ----
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_users', Math.random());"))
    
    observeEvent(input$reload_users, {
      if (debug) cat(paste0("\n", now(), " - mod_users - observer input$reload_users"))
      
      sql <- glue::glue_sql(paste0(
        "SELECT u.id, u.firstname, u.lastname, (u.firstname || ' ' || u.lastname) AS name, (SUBSTR(u.firstname, 1, 1) || SUBSTR(u.lastname, 1, 1)) AS initials, ",
        "s.name AS user_status, a.name AS user_access ",
        "FROM users u ",
        "LEFT JOIN users_statuses s ON u.user_status_id = s.id ",
        "LEFT JOIN users_accesses a ON u.user_access_id = a.id"), .con = r$db)

      r$users <- DBI::dbGetQuery(r$db, sql) %>% tibble::as_tibble()
      users <- r$users %>% dplyr::select(-name, -initials)
      
      col_names <- c(i18n$t("id"), i18n$t("firstname"), i18n$t("lastname"), i18n$t("user_status"), i18n$t("user_access"))

      sortable_cols <- ""
      searchable_cols <- ""
      factorize_cols <- ""
      hidden_cols <- ""
      editable_cols <- ""
      column_widths <- c("id" = "50px")

      render_datatable(
        output = output, ns = ns, i18n = i18n, data = users, hidden_cols = hidden_cols, editable_cols = editable_cols,
        output_name = "users_dt", datatable_dom = "<'top't><'bottom'p>", sortable_cols = sortable_cols,
        col_names = col_names, searchable_cols = searchable_cols, factorize_cols = factorize_cols, filter = TRUE, column_widths = column_widths
      )
    })
  })
}
