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
              id = ns("explore_ui"),
              DT::DTOutput(ns("git_repos_dt")),
              style = "padding: 0 10px;"
            )
          ),
          class = "map_widget", style = "height: 80%;"
        ),
        div(
          id = ns("git_infos_div"),
          uiOutput(ns("map_git_infos")),
          uiOutput(ns("list_git_infos")),
          class = "widget", style = "height: 20%;"
        ),
        class = "git_repos_left"
      ),
      div(
        div(
          uiOutput(ns("map_git_readme")),
          shinyjs::hidden(uiOutput(ns("list_git_readme"))),
          class = "widget markdown_widget"
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
            tags$button(id = ns("data_cleaning_scripts"), i18n$t("data_cleaning"), class = "pivot_item", onclick = pivot_item_js),
            tags$button(id = ns("datasets"), i18n$t("datasets"), class = "pivot_item", onclick = pivot_item_js),
            # tags$button(id = ns("vocabularies"), i18n$t("vocabularies"), class = "pivot_item", onclick = pivot_item_js),
            class = "pivot"
          ),
          style = "display: flex; justify-content: space-between; z-index: 100;"
        ),
        
        ## Summary ----
        div(
          id = ns("summary_div"),
          div(
            div(
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
              uiOutput(ns("summary_git_readme")),
              class = "widget markdown_widget",
            ),
            class = "git_repos_summary_right"
          ),
          class = "git_repos_summary_container"
        ),
        
        ## Widgets ----
        shinyjs::hidden(
          div(
            id = ns("widgets_div"),
            div(shiny.fluent::SearchBox.shinyInput(ns("search_element")), style = "width:320px; margin:10px 0 0 10px;"),
            uiOutput(ns("elements")),
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
    
    # Prevent bug leaflet ----
    observeEvent(shiny.router::get_page(), {
      req(shiny.router::get_page() == id)
      if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer shiny.router::get_page()"))
      
      # Prevent a bug with leaflet map display
      shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
    })
    
    # Initiate vars ----
    
    # all_divs <- c("summary", "projects", "plugins", "data_cleaning_scripts", "datasets", "vocabularies")
    all_divs <- c("summary", "projects", "plugins", "data_cleaning_scripts", "datasets")
    
    r$loaded_git_repos <- tibble::tibble(unique_id = character(), datetime = character())
    
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
      
      # Display list or map ----
      
      observeEvent(input$show_list, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$show_list"))
        
        sapply(c("show_list_div", "explore_map", "map_git_infos", "map_git_readme"), shinyjs::hide)
        sapply(c("explore_ui", "list_git_infos", "list_git_readme"), shinyjs::show)
        shinyjs::delay(50, shinyjs::show("show_map_div"))
      })
      
      observeEvent(input$show_map, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$show_map"))
        
        sapply(c("show_map_div", "explore_ui", "list_git_infos", "list_git_readme"), shinyjs::hide)
        sapply(c("map_git_infos", "map_git_readme", "explore_map"), shinyjs::show)
        shinyjs::delay(50, shinyjs::show("show_list_div"))
        
        # Prevent a bug with leaflet map display
        shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
      })
      
      # Reload list ----
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_git_repos', Math.random());"))
      
      observeEvent(input$reload_git_repos, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$reload_git_repos"))
        
        sql <- glue::glue_sql("WITH selected_git_repos AS (
          SELECT DISTINCT g.id
          FROM git_repos g
          LEFT JOIN options AS r ON g.id = r.link_id AND r.category = 'git_repo' AND r.name = 'users_allowed_read_group'
          LEFT JOIN options AS u ON g.id = u.link_id AND u.category = 'git_repo' AND u.name = 'user_allowed_read'
          WHERE r.value = 'everybody' OR (r.value = 'people_picker' AND u.value_num = {r$user_id})
        )
        SELECT g.id, g.unique_id, g.repo_url_address, o.name, o.value, o.value_num
          FROM git_repos g
          INNER JOIN selected_git_repos s ON g.id = s.id
          LEFT JOIN options o ON o.category = 'git_repo' AND g.id = o.link_id", .con = r$db)
        
        r$git_repos_long <- DBI::dbGetQuery(r$db, sql) %>% tibble::as_tibble()
        
        sql <- glue::glue_sql("SELECT * FROM git_repos WHERE id IN ({unique(r$git_repos_long$id)*})", .con = r$db)
        r$git_repos_wide <- DBI::dbGetQuery(r$db, sql) %>% tibble::as_tibble()
        
        # Render datatable
        render_datatable(
          output = output, ns = ns, i18n = i18n, data = r$git_repos_wide %>% dplyr::select(name),
          output_name = "git_repos_dt", col_names = i18n$t("name"), datatable_dom = "<'top't><'bottom'p>",
          sortable_cols = "name", searchable_cols = "name", filter = TRUE
        )
      })
      
      # A row is selected ----
      
      observeEvent(input$git_repos_dt_rows_selected, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$git_repos_dt_rows_selected"))
        
        git_repo_id <- r$git_repos_wide[input$git_repos_dt_rows_selected, ] %>% dplyr::pull(id)
        git_repo <- r$git_repos_wide %>% dplyr::filter(id == git_repo_id)
        r$list_git_repo <- git_repo
        
        output$list_git_infos <- renderUI(get_git_infos(git_repo, "list"))
        
        readme <- renderUI(get_git_readme(r, git_repo))
        output$list_git_readme <- renderUI(readme)
        output$summary_git_readme <- renderUI(readme)
      })
      
      # Download git repos ----

      git_repos <- tibble::tibble()

      filename <- "https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/git_repos/git_repos.csv"
      filename_local <- paste0(r$app_folder, "/temp_files/", r$user_id, "/git_repos/git_repos.csv")

      # Get csv file from remote git
      tryCatch(download.file(filename, filename_local, quiet = TRUE),
        error = function(e){
          show_message_bar(output, "error_loading_git_repos_csv", "warning", i18n = i18n, ns = ns)
          cat(paste0("\n", now(), " - mod_git_repos - error loading remote git csv file - error = ", toString(e)))
      })

      tryCatch(git_repos <- vroom::vroom(filename_local, col_types = "cnnccccl", progress = FALSE),
        error = function(e){
          show_message_bar(output, "error_loading_git_repos_csv", "warning", i18n = i18n, ns = ns)
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
      
      # Click on the map ----
      observeEvent(input$explore_map_marker_click, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$explore_map_marker_click"))
        
        #   # Update API key field
        #   shiny.fluent::updateTextField.shinyInput(session = session, "api_key_git_repo_with_map", errorMessage = NULL, value = "")
        
        git_repo <- git_repos %>% dplyr::filter(
          sprintf("%.6f", as.numeric(lat)) == sprintf("%.6f", as.numeric(input$explore_map_marker_click$lat)),
          sprintf("%.6f", as.numeric(lng)) == sprintf("%.6f", as.numeric(input$explore_map_marker_click$lng))
        )
        
        r$map_git_repo <- git_repo
        
        readme <- get_git_readme(r, git_repo)
        output$map_git_readme <- renderUI(readme)
        output$summary_git_readme <- renderUI(readme)
        
        # Get git repo informations
        
        output$map_git_infos <- renderUI(get_git_infos(git_repo, "map"))
        
        r$git_repo <- git_repo
        
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
      
      observeEvent(input$show_content_list, {
        if (debug) cat(paste0("\n", now(), " - mog_git_repos - observer input$show_content_list"))
        
        r$git_repo <- r$list_git_repo
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_content_trigger', Math.random());"))
      })
      
      observeEvent(input$show_content_map, {
        if (debug) cat(paste0("\n", now(), " - mog_git_repos - observer input$show_content_map"))
        
        r$git_repo <- r$map_git_repo
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_content_trigger', Math.random());"))
      })
      
      observeEvent(input$show_content_trigger, {
        if (debug) cat(paste0("\n", now(), " - mog_git_repos - observer input$show_content_trigger"))
        
        sapply(c("one_repo_reduced_sidenav", "one_git_repo"), shinyjs::show)
        sapply(c("all_repos_reduced_sidenav", "all_git_repos"), shinyjs::hide)
        
        output$breadcrumb <- renderUI(
          shiny.fluent::Breadcrumb(items = list(
            list(key = "main", text = i18n$t("git_repos"), href = shiny.router::route_link("git_repos"), 
              onClick = htmlwidgets::JS(paste0("item => { Shiny.setInputValue('", id, "-show_home', Math.random()); }"))),
            list(key = "main", text = r$git_repo$name))
          )
        )
        
        # Clone git repo if not already loaded
        
        local_path <- ""
        tryCatch(local_path <- load_git_repo(id, r, r$git_repo),
          error = function(e){
            show_message_bar(output, "error_downloading_git_repo", "warning", i18n = i18n, ns = ns)
            cat(paste0("\n", now(), " - mod_git_repos - error downloading git repo - error = ", toString(e)))
          }
        )
        
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-git_repo_local_path', '", local_path, "');"))
      })
      
      # Repo current tab ----
      
      observeEvent(input$current_tab_trigger, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$current_tab_trigger"))
        
        current_tab <- gsub(paste0(id, "-"), "", input$current_tab, fixed = FALSE)
        
        # Show or hide pages depending on selected tab
        if (current_tab == "summary"){
          shinyjs::show("summary_div")
          shinyjs::hide("widgets_div")
        }
        else {
          shinyjs::show("widgets_div")
          shinyjs::hide("summary_div")
        }
        
        shinyjs::show("one_repo_reduced_sidenav")
        shinyjs::hide("all_repos_reduced_sidenav")
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_hide_sidenav', 'hide');"))
        
        # Change selected tab
        sapply(all_divs, function(button_id) shinyjs::removeClass(class = "selected_pivot_item", selector = paste0("#", id, "-", button_id)))
        shinyjs::addClass(class = "selected_pivot_item", selector = paste0("#", id, "-", current_tab))
        
        # Reload widgets UI
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_elements_list', Math.random());"))
      })
      
      # Reload widgets ----
      
      observeEvent(input$reload_elements_list, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$reload_elements_list"))
        
        current_tab <- gsub(paste0(id, "-"), "", input$current_tab, fixed = FALSE)
        
        single_id <- switch(current_tab, 
          "data_cleaning" = "data_cleaning", 
          "datasets" = "dataset",
          "projects" = "project", 
          "plugins" = "plugin", 
          "subsets" = "subset"#, 
          # "vocabularies" = "vocabulary"
        )
        
        elements <- tibble::tibble()
        
        # Get elements from XML file
        
        xml_file_path <- paste0(input$git_repo_local_path, "/", current_tab, "/", current_tab, ".xml")

        error_loading_xml_file <- TRUE
        
        tryCatch({
          elements <-
            xml2::read_xml(xml_file_path) %>%
            XML::xmlParse() %>%
            XML::xmlToDataFrame(nodes = XML::getNodeSet(., paste0("//", single_id)), stringsAsFactors = FALSE) %>%
            tibble::as_tibble()
          
          error_loading_xml_file <- FALSE
        }, error = function(e) cat(paste0("\n", now(), " - mod_git_repos - error downloading ", current_tab, " readme - error = ", toString(e))))
        
        if (error_loading_xml_file) elements_ui <- div(
          shiny.fluent::MessageBar(i18n$t("error_loading_category_xml_file"), messageBarType = 5), 
          style = "display: inline-block; margin: 10px 0 0 10px;"
        )
        else if (nrow(elements) == 0) elements_ui <- div(
          shiny.fluent::MessageBar(i18n$t("no_elements_to_display"), messageBarType = 5), 
          style = "display: inline-block; margin: 10px 0 0 10px;"
        )
        
        else {
          elements_ui <- tagList()
          
          for (i in 1:nrow(elements)){
            row <- elements[i, ]
            
            element_name <- row[[paste0("name_", language)]]
            
            users_ui <- ""
  
            max_length <- 45
            if (nchar(element_name) > max_length) element_name <- paste0(substr(element_name, 1, max_length - 3), "...")
              
            widget_buttons <- tagList()
            
            # For plugins widgets, we add some content on the bottom
            
            if (current_tab == "plugins"){
              plugin_type <- row %>% dplyr::slice(1) %>% dplyr::pull(type)
              widget_buttons <- get_plugin_buttons(plugin_type, i18n)
            }
            
            onclick <- paste0("
              Shiny.setInputValue('", id, "-selected_element', '", row$unique_id, "');
              Shiny.setInputValue('", id, "-selected_element_trigger', Math.random());
            ")
  
            elements_ui <- tagList(
              create_element_ui(page_id = id, single_id, element_name, users_ui, widget_buttons, onclick),
              elements_ui
            )
          }
        }

        elements_ui <- div(elements_ui, class = paste0(current_tab, "_container"))
        output$elements <- renderUI(elements_ui)
      })
      
      # Go to all repos page ----
      
      observeEvent(input$show_home, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$show_home"))
        
        sapply(c("all_repos_reduced_sidenav", "all_git_repos"), shinyjs::show)
        sapply(c("one_repo_reduced_sidenav", "one_git_repo"), shinyjs::hide)
        
        # Prevent a bug with leaflet map display
        shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
      })
    }
    
    # |-------------------------------- -----
    
    ## Module functions ----
    
    get_git_infos <- function(git_repo, git_source){
      
      if (nrow(git_repo) > 0) tagList(
        tags$h1(git_repo$name),
        div(
          strong(i18n$t("url_address")), tags$span(":", style = "margin: 0 5px;"),
          shiny.fluent::Link(href = git_repo$repo_url_address, git_repo$repo_url_address, target = "_blank"),
          style = "display: flex;"
        ),
        div(
          shiny.fluent::PrimaryButton.shinyInput(ns(paste0("show_content_", git_source)), i18n$t("show_content")),
          class = "create_element_modal_buttons",
          style = "margin-left: 10px;"
        )) else div(
        shiny.fluent::MessageBar(i18n$t("error_loading_git_infos"), messageBarType = 5), 
        style = "display: inline-block; margin-top: 10px;"
      )
    }
    
    get_git_readme <- function(r, git_repo){
      
      readme <- div(
        shiny.fluent::MessageBar(i18n$t("error_loading_git_readme"), messageBarType = 5), 
        style = "display: inline-block; margin-top: 10px;"
      )
      
      # Get README on git repo
      
      if (nrow(git_repo) > 0){
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
      } 
      
      readme
    }
    
  })
}