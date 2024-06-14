#' @noRd 
mod_git_repos_ui <- function(id = character(), language = "en", languages = tibble::tibble(), i18n = character()){
  ns <- NS(id)
  
  pivot_item_js <- paste0("
    Shiny.setInputValue('", id, "-current_tab', this.id);
    Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());"
  )
  
  div(
    class = "main",
    
    # All git repos ----
    
    div(
      id = ns("all_git_repos"),
      div(
        div(
          id = ns("explore_map_and_ui_div"),
          leaflet::leafletOutput(ns("explore_map"), height = "100%"),
          shinyjs::hidden(
            div(
              id = ns("explore_ui")
            )
          ),
          class = "map_widget", style = "height: 80%;"
        ),
        div(
          id = ns("git_infos_div"),
          uiOutput(ns("git_infos")),
          class = "widget", style = "height: 20%;"
        ),
        class = "git_repos_left"
      ),
      div(
        div(
          uiOutput(ns("map_git_readme")),
          shinyjs::hidden(uiOutput(ns("list_git_readme"))),
          class = "widget", style = "height: calc(100% - 35px); padding: 1px 20px 10px 20px; overflow: auto;"
        ),
        class = "git_repos_right"
      ),
      class = "git_repos_container"
    ),
    
    # Git repo details ----
    
    shinyjs::hidden(
      div(
        id = ns("one_git_repo"),
        div(
          uiOutput(ns("breadcrumb")),
          div(
            id = ns("plugin_pivot"),
            tags$button(id = ns("summary"), i18n$t("summary"), class = "pivot_item selected_pivot_item", onclick = pivot_item_js),
            tags$button(id = ns("projects"), i18n$t("projects"), class = "pivot_item", onclick = pivot_item_js),
            tags$button(id = ns("plugins"), i18n$t("plugins"), class = "pivot_item", onclick = pivot_item_js),
            tags$button(id = ns("data_cleaning_scripts"), i18n$t("data_cleaning_scripts"), class = "pivot_item", onclick = pivot_item_js),
            tags$button(id = ns("datasets"), i18n$t("datasets"), class = "pivot_item", onclick = pivot_item_js),
            tags$button(id = ns("vocabularies"), i18n$t("vocabularies"), class = "pivot_item", onclick = pivot_item_js),
            class = "pivot"
          ),
          style = "display: flex; justify-content: space-between; z-index: 100;"
        ),
        
        ## Summary ----
        div(
          id = ns("summary_div"),
          div(
            div(
              h1(i18n$t("informations")),
              class = "widget", style = "height: 50%;"
            ),
            div(
              "",
              class = "widget", style = "height: 50%;"
            ),
            class = "git_repos_summary_left"
          ),
          div(
            div(
              h1(i18n$t("description")),
              class = "widget", style = "height: calc(100% - 25px); padding-top: 1px;"
            ),
            class = "git_repos_summary_right"
          ),
          class = "git_repos_summary_container"
        ),
        
        ## Projects ----
        shinyjs::hidden(
          div(
            id = ns("projects_div"),
            style = "height: 100%;"
          )
        ),
        style = "height: 100%; display: flex; flex-direction: column;"
      )
    )
  )
}

#' @noRd 
mod_git_repos_server <- function(id, r, d, m, language, i18n, debug){
  
  # |-------------------------------- -----
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (debug) cat(paste0("\n", now(), " - mod_git_repos - start"))
    
    # No internet access ----
    
    if (!r$has_internet) {
      shinyjs::show("explore_ui")
      shinyjs::hide("explore_map")
      
      output$explore_ui <- renderUI(
        div(
          shiny.fluent::MessageBar(i18n$t("no_internet_access"), messageBarType = 5), 
          style = "display: inline-block; margin: 5px 0 0 10px;"
        )
      )
    }
    
    # Internet access ----
    
    if (r$has_internet){
      
      ## Download git repos ----

      git_repos <- tibble::tibble()

      filename <- "https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/git_repos/git_repos.csv"
      filename_local <- paste0(r$app_folder, "/temp_files/", r$user_id, "/git_repos/git_repos.csv")

      # Get csv file from remote git
      tryCatch(download.file(filename, filename_local, quiet = TRUE),
        error = function(e){
          show_message_bar(output, "error_loading_git_repos_csv", "severeWarning", i18n = i18n, ns = ns)
          cat(paste0("\n", now(), " - mod_git_repos - error loading remote git csv file - error = ", toString(e)))
      })

      tryCatch(git_repos <- vroom::vroom(filename_local, col_types = "cnnccccl", progress = FALSE),
        error = function(e){
          show_message_bar(output, "error_loading_git_repos_csv", "severeWarning", i18n = i18n, ns = ns)
          cat(paste0("\n", now(), " - mod_git_repos - error loading remote git csv file - error = ", toString(e)))
        })
      
      new_cols <- outer("name", r$languages$code, paste, sep = "_") %>% as.vector()
      for(col in new_cols) if(!col %in% colnames(git_repos)) git_repos <- git_repos %>% dplyr::mutate(!!col := "")

      git_repos <- git_repos %>% dplyr::mutate(name = ifelse(!is.na(get(paste0("name_", language))), get(paste0("name_", language)), name_en))
      
      # Display map ----
      output$explore_map <- leaflet::renderLeaflet({

        if (debug) cat(paste0("\n", now(), " - mod_git_repos - output$explore_map"))

        leaflet::leaflet(git_repos) %>%
          leaflet::addTiles() %>%
          leaflet::addProviderTiles("CartoDB.Voyager") %>%
          leaflet::addMarkers(
            lng = ~lng, lat = ~lat,
            clusterOptions = leaflet::markerClusterOptions(),
            popup = ~paste("<strong>", name, "</strong>")
          )
      })
      
      # Display list or map ----
      
      observeEvent(input$show_list, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$show_list"))
        
        sapply(c("show_list_div", "explore_map", "map_git_readme"), shinyjs::hide)
        sapply(c("explore_ui", "list_git_readme"), shinyjs::show)
        shinyjs::delay(50, shinyjs::show("show_map_div"))
      })
      
      observeEvent(input$show_map, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$show_map"))
        
        sapply(c("show_map_div", "explore_ui", "list_git_readme"), shinyjs::hide)
        sapply(c("map_git_readme", "explore_map"), shinyjs::show)
        shinyjs::delay(50, shinyjs::show("show_list_div"))
        
        # Prevent a bug with display with leaflet map
        shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
      })
      
      # Click on the map ----
      observeEvent(input$explore_map_marker_click, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$explore_map_marker_click"))
        
        #   # Update API key field
        #   shiny.fluent::updateTextField.shinyInput(session = session, "api_key_git_repo_with_map", errorMessage = NULL, value = "")
        
        git_repo <- git_repos %>% dplyr::filter(
          sprintf("%.6f", as.numeric(lat)) == sprintf("%.6f", as.numeric(input$explore_map_marker_click$lat)),
          sprintf("%.6f", as.numeric(lng)) == sprintf("%.6f", as.numeric(input$explore_map_marker_click$lng))
        )
        
        readme <- div(
          shiny.fluent::MessageBar(i18n$t("error_loading_git_readme"), messageBarType = 5), 
          style = "display: inline-block; margin-top: 10px;"
        )
        
        # Get README on git repo
        tryCatch({
          dir <- paste0(r$app_folder, "/temp_files/", r$user_id, "/git_repos")
          new_file <- paste0(dir, "/", paste0(sample(c(0:9, letters[1:6]), 8, TRUE), collapse = ''), "_README.md")
          if (!dir.exists(dir)) dir.create(dir)
          
          readme_url <- git_repo$raw_files_url_address
          nchar_readme_url <- nchar(readme_url)
          if (substr(readme_url, nchar_readme_url, nchar_readme_url) != "/") readme_url <- paste0(readme_url, "/")
          
          filename_remote <- paste0(readme_url, "README.md")
          
          download.file(filename_remote, new_file, quiet = TRUE)
          
          con <- textConnection(new_file)
          readme <- div(readLines(con, warn = FALSE) %>% includeMarkdown() %>% withMathJax())
          close(con)
        },
        error = function(e) cat(paste0("\n", now(), " - mod_git_repos - error loading remote git readme - error = ", toString(e))))
        
        # Render UI
        output$map_git_readme <- renderUI({
          if (debug) cat(paste0("\n", now(), " - mod_settings_git - output$map_git_readme"))
          readme
        })
        
        # Get git repo informations
        
        git_infos <- div(
          shiny.fluent::MessageBar(i18n$t("error_loading_git_infos"), messageBarType = 5), 
          style = "display: inline-block; margin-top: 10px;"
        )
        
        r$git_repo <- git_repo
        
        if (nrow(git_repo) > 0){
          git_infos <- tagList(
            tags$h1(git_repo[[paste0("name_", language)]]),
            div(
              strong(i18n$t("url_address")), tags$span(":", style = "margin: 0 5px;"),
              shiny.fluent::Link(href = git_repo$repo_url_address, git_repo$repo_url_address, target = "_blank"),
              style = "display: flex;"
            ),
            div(
              shiny.fluent::PrimaryButton.shinyInput(ns("show_content"), i18n$t("show_content")),
              class = "create_element_modal_buttons",
              style = "margin-left: 10px;"
            )
          )
        }
        
        output$git_infos <- renderUI(
          git_infos
        )
      #   
      #   # Disable add button is git repo already in db
      #   if (r$git_repos %>% dplyr::inner_join(git_repo %>% dplyr::select(unique_id), by = "unique_id") %>% nrow() > 0){
      #     shiny.fluent::updateActionButton.shinyInput(session = session, "add_git_repo_with_map", disabled = TRUE)
      #     shinyjs::hide("api_key_git_repo_with_map_div")
      #   }
      #   else {
      #     shiny.fluent::updateActionButton.shinyInput(session = session, "add_git_repo_with_map", disabled = FALSE)
      #     
      #     # If an API key is needed
      #     if (git_repo$api_key_required) shinyjs::show("api_key_git_repo_with_map_div") else shinyjs::hide("api_key_git_repo_with_map_div")
      #   }
      #   
      #   r$show_git_repo_description_trigger <- now()
      #   r$show_git_repo_description_type <- "map"
      #   r$show_git_repo_description_url <- git_repo$raw_files_url_address
      #   
      #   if (substr(r$show_git_repo_description_url, nchar(r$show_git_repo_description_url), 
      #              nchar(r$show_git_repo_description_url)) != "/") r$show_git_repo_description_url <- paste0(r$show_git_repo_description_url, "/")
      })
      # 
      # ## Add with map
      # observeEvent(input$add_git_repo_with_map, {
      #   if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$add_git_repo_with_map"))
      #   
      #   req(length(input$git_repos_map_marker_click) > 0)
      #   git_repo <- git_repos %>% dplyr::filter(
      #     sprintf("%.6f", as.numeric(lat)) == sprintf("%.6f", as.numeric(input$git_repos_map_marker_click$lat)),
      #     sprintf("%.6f", as.numeric(lng)) == sprintf("%.6f", as.numeric(input$git_repos_map_marker_click$lng))
      #   )
      #   
      #   new_data <- list()
      #   if (paste0("name_", language) %in% colnames(git_repo)) new_data$name <- git_repo[[paste0("name_", language)]]
      #   else new_data$name <- git_repo$name_en
      #   new_data$api_key <- coalesce2(type = "char", x = input$api_key_git_repo_with_map)
      #   new_data$repo_url_address <- git_repo$repo_url_address
      #   new_data$raw_files_url_address <- git_repo$raw_files_url_address
      #   new_data$unique_id <- git_repo$unique_id
      #   
      #   # Check if git repo is not already in database
      #   sql <- glue::glue_sql("SELECT * FROM git_repos WHERE unique_id = {new_data$unique_id} AND deleted IS FALSE", .con = r$db)
      #   check_unique_id <- DBI::dbGetQuery(r$db, sql) %>% nrow() == 0
      #   if (!check_unique_id) show_message_bar(output, "git_repo_already_in_db", "severeWarning", i18n = i18n, ns = ns)
      #   req(check_unique_id)
      #   
      #   # Check if name is not already taken
      #   sql <- glue::glue_sql("SELECT * FROM git_repos WHERE name = {new_data$name} AND deleted IS FALSE", .con = r$db)
      #   check_unique_name <- DBI::dbGetQuery(r$db, sql) %>% nrow() == 0
      #   if (!check_unique_name) show_message_bar(output, "name_already_used", "severeWarning", i18n = i18n, ns = ns)
      #   req(check_unique_name)
      #   
      #   if (git_repo$api_key_required){
      #     if (is.na(input$api_key_git_repo_with_map) | input$api_key_git_repo_with_map == "") shiny.fluent::updateTextField.shinyInput(session, "api_key_git_repo_with_map", errorMessage = i18n$t("provide_valid_api_key"))
      #     else shiny.fluent::updateTextField.shinyInput(session, "api_key_git_repo_with_map", errorMessage = NULL)
      #     req(!is.na(input$api_key_git_repo_with_map) & input$api_key_git_repo_with_map != "")
      #   }
      #   
      #   add_settings_new_data(session = session, output = output, r = r, m = m, i18n = i18n, id = id, data = new_data, table = "git_repos")
      #   
      #   # Update & hide API key field
      #   shiny.fluent::updateTextField.shinyInput(session = session, "api_key_git_repo_with_map", errorMessage = NULL, value = "")
      #   shinyjs::hide("api_key_git_repo_with_map_div")
      #   
      #   # Disable add button
      #   shiny.fluent::updateActionButton.shinyInput(session = session, "add_git_repo_with_map", disabled = TRUE)
      #   
      #   # Reload datatable
      #   r$git_repos_temp <- r$git_repos %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      # })
      
      # A repo is selected ----
      observeEvent(input$show_content, {
        if (debug) cat(paste0("\n", now(), " - mog_git_repos - observer input$show_content"))
        
        shinyjs::hide("all_git_repos")
        shinyjs::show("one_git_repo")
      })
    }
    
  })
}