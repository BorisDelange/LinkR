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
        
        ## Map or list
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
        
        ## Selected git infos
        div(
          id = ns("git_infos_div"),
          div(
            id = ns("map_git_infos_div"),
            uiOutput(ns("map_git_infos_title")),
            uiOutput(ns("map_git_infos_content"))
          ),
          shinyjs::hidden(
            div(
              id = ns("list_git_infos_div"),
              div(
                uiOutput(ns("list_git_infos_title")),
                div(
                  shinyjs::hidden(
                    div(
                      id = ns("list_git_infos_buttons"),
                      create_hover_card(
                        ui = shinyjs::hidden(
                          div(
                            id = ns("delete_git_repo_button"),
                            shiny.fluent::IconButton.shinyInput(ns("delete_git_repo"), iconProps = list(iconName = "Delete"))
                          )
                        ), 
                        text = i18n$t("delete")
                      ),
                      div(
                        id = ns("edit_git_repo_button"),
                        create_hover_card(
                          ui = shinyjs::hidden(
                            div(
                              id = ns("edit_git_repo_button"),
                              shiny.fluent::IconButton.shinyInput(ns("edit_git_repo"), iconProps = list(iconName = "Edit"))
                            )
                          ),
                          text = i18n$t("edit")
                        )
                      ),
                      shinyjs::hidden(
                        div(
                          id = ns("save_git_repo_edition_button"),
                          create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("save_git_repo_edition"), iconProps = list(iconName = "Accept")), text = i18n$t("save")),
                        )
                      ),
                      style = "display: flex; margin-top: 5px;",
                      class = "small_icon_button"
                    )
                  )
                ),
                style = "display: flex; justify-content: space-between;"
              ),
              uiOutput(ns("list_git_infos_content")),
              shinyjs::hidden(
                div(
                  id = ns("edit_git_repo_div"),
                  div(shiny.fluent::TextField.shinyInput(ns("git_repo_name"), label = i18n$t("name")), style = "width: 200px;"),
                  div(shiny.fluent::TextField.shinyInput(ns("git_repo_url_address"), label = i18n$t("url_address")), style = "width: 400px;"),
                  div(shiny.fluent::TextField.shinyInput(ns("git_repo_raw_files_url_address"), label = i18n$t("repo_url_address")), style = "width: 400px;")
                )
              )
            )
          ),
          class = "widget", style = "height: 300px;"
        ),
        
        # Readme ace editor
        shinyjs::hidden(
          div(
            id = ns("edit_readme_div"),
            div(
              h1(i18n$t("edit_readme")),
              div(
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("generate_readme"), iconProps = list(iconName = "Generate")), text = i18n$t("generate_readme")),
                create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("run_readme_code"), iconProps = list(iconName = "Play")), text = i18n$t("run_code")),
                style = "display: flex; margin-top: 5px;"
              ),
              class = "small_icon_button",
              style = "display: flex; justify-content: space-between;"
            ),
            div(
              shinyAce::aceEditor(
                ns("readme_code"), mode = "markdown",
                hotkeys = list(
                  save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
                  run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER")
                ),
                autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, fontSize = 11, showPrintMargin = FALSE
              ),
              style = "width: 100%; height: calc(100% - 45px); display: flex; flex-direction: column;"
            ),
            class = "widget", style = "height: 100%;"
          )
        ),
        class = "git_repos_left"
      ),
      div(
        div(
          div(
            shinyjs::hidden(
              div(
                id = ns("edit_readme_buttons"),
                shinyjs::hidden(
                  div(
                    id = ns("edit_readme_button"),
                    create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("edit_readme"), iconProps = list(iconName = "Edit")), text = i18n$t("edit_readme"))
                  )
                ),
                shinyjs::hidden(
                  div(
                    id = ns ("save_and_cancel_readme_buttons"),
                    div(
                      id = ns("cancel_readme_button"),
                      create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("cancel_readme"), iconProps = list(iconName = "Cancel")), text = i18n$t("cancel_readme_updates"))
                    ),
                    div(
                      id = ns("save_readme_button"),
                      create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("save_readme"), iconProps = list(iconName = "Accept")), text = i18n$t("save_readme")),
                    ),
                    style = "display: flex;"
                  )
                ),
                style = "margin-top: 5px;"
              )
            ),
            class = "small_icon_button",
            style = "position: absolute; top: 0; right: 10px;"
          ),
          uiOutput(ns("map_git_readme")),
          shinyjs::hidden(uiOutput(ns("list_git_readme"))),
          class = "widget markdown",
          style = "height: calc(100% - 20px);"
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
          div(uiOutput(ns("breadcrumb")), class = "breadcrumb"),
          div(
            id = ns("plugin_pivot"),
            tags$button(id = ns("projects"), i18n$t("projects"), class = "pivot_item", onclick = pivot_item_js),
            tags$button(id = ns("plugins"), i18n$t("plugins"), class = "pivot_item", onclick = pivot_item_js),
            tags$button(id = ns("data_cleaning_scripts"), i18n$t("data_cleaning"), class = "pivot_item", onclick = pivot_item_js),
            tags$button(id = ns("datasets"), i18n$t("datasets"), class = "pivot_item", onclick = pivot_item_js),
            class = "pivot"
          ),
          style = "display: flex; justify-content: space-between; z-index: 100;"
        ),
        
        ## Widgets ----
        # shinyjs::hidden(
        div(
          id = ns("widgets_div"),
          div(shiny.fluent::SearchBox.shinyInput(ns("search_element")), style = "width: 280px; margin: 10px 0 15px 10px;"),
          div(uiOutput(ns("elements")), style = "margin: 10px 0 0 10px;"),
          style = "height: 100%;"
        ),
        # ),
        
        ## Selected element details ----
        shinyjs::hidden(
          div(
            id = ns("element_details_div"),
            div(
              div(
                uiOutput(ns("element_details_title")),
                div(uiOutput(ns("element_details_ui"))),
                div(
                  uiOutput(ns("synchronize_git_buttons")),
                  class = "git_element_details_git_buttons"
                ),
                class = "widget", style = "height: 50%; padding-top: 1px;"
              ),
              class = "git_element_details_left"
            ),
            div(
              div(
                tags$h1(i18n$t("description")),
                uiOutput(ns("element_details_description")),
                class = "widget markdown",
                style = "height: calc(100% - 20px);"
              ),
              class = "git_element_details_right"
            ),
            class = "git_element_details_container"
          )
        ),
        style = "height: 100%; display: flex; flex-direction: column;"
      )
    ),
    
    # Create a git repo modal ----
    shinyjs::hidden(
      div(
        id = ns("create_git_repo_modal"),
        div(
          div(
            tags$h1(i18n$t("create_git_repo")),
            shiny.fluent::IconButton.shinyInput(ns("close_create_git_repo_modal"), iconProps = list(iconName = "ChromeClose")),
            class = "create_git_repo_modal_head small_close_button"
          ),
          div(
            div(shiny.fluent::TextField.shinyInput(ns("git_repo_creation_name"), label = i18n$t("name")), style = "width: 200px;"),
            div(shiny.fluent::TextField.shinyInput(ns("git_repo_creation_url_address"), label = i18n$t("url_address")), style = "width: 400px;"),
            div(shiny.fluent::TextField.shinyInput(ns("git_repo_creation_raw_files_url_address"), label = i18n$t("raw_files_url_address")), style = "width: 400px;"),
            div(
              shiny.fluent::PrimaryButton.shinyInput(ns("add_git_repo"), i18n$t("add")),
              class = "create_git_repo_modal_buttons"
            ),
          ),
          class = "create_git_repo_modal_content"
        ),
        class = "create_git_repo_modal"
      )
    ),
    
    # Delete a git repo modal ----
    shinyjs::hidden(
      div(
        id = ns("delete_git_repo_modal"),
        div(
          tags$h1(i18n$t("delete_git_repo_title")), tags$p(i18n$t("delete_git_repo_text")),
          div(
            shiny.fluent::DefaultButton.shinyInput(ns("close_git_repo_deletion_modal"), i18n$t("dont_delete")),
            div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_git_repo_deletion"), i18n$t("delete")), class = "delete_button"),
            class = "delete_modal_buttons"
          ),
          class = "delete_modal_content"
        ),
        class = "delete_modal"
      )
    ),
    
    # Delete a git element modal ----
    shinyjs::hidden(
      div(
        id = ns("delete_git_element_modal"),
        div(
          tags$h1(i18n$t("delete_git_element_title")), tags$p(i18n$t("delete_git_element_text")),
          div(
            shiny.fluent::DefaultButton.shinyInput(ns("close_git_element_deletion_modal"), i18n$t("dont_delete")),
            div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_git_element_deletion"), i18n$t("delete")), class = "delete_button"),
            class = "delete_modal_buttons"
          ),
          class = "delete_modal_content"
        ),
        class = "delete_modal"
      )
    ),
    
    # Commit and push modal ----
    shinyjs::hidden(
      div(
        id = ns("push_git_modal"),
        div(
          tags$h1(i18n$t("push_git_title")),
          shiny.fluent::TextField.shinyInput(ns("push_git_api_key"), type = "password", canRevealPassword = TRUE, label = i18n$t("api_key")),
          shiny.fluent::TextField.shinyInput(ns("push_git_commit_message"), label = i18n$t("commit_message")),
          div(
            shiny.fluent::DefaultButton.shinyInput(ns("close_push_git_modal"), i18n$t("cancel")),
            shiny.fluent::PrimaryButton.shinyInput(ns("confirm_push_git_update"), i18n$t("update")),
            class = "push_git_modal_buttons"
          ),
          class = "push_git_modal_content"
        ),
        class = "push_git_modal"
      )
    ),
    
    # Import project plugins modal ----
    shinyjs::hidden(
      div(
        id = ns("update_project_plugins_modal"),
        div(
          tags$h1(i18n$t("update_project_plugins_title")), tags$p(i18n$t("update_project_plugins_text")),
          div(
            shiny.fluent::DefaultButton.shinyInput(ns("close_project_plugins_import_modal"), i18n$t("dont_update")),
            div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_project_plugins_import"), i18n$t("update"))),
            class = "import_modal_buttons"
          ),
          class = "import_modal_content"
        ),
        class = "import_modal"
      )
    )
  )
}

#' @noRd 
mod_git_repos_server <- function(id, r, d, m, language, i18n, debug, user_accesses){
  
  # |-------------------------------- -----
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Current user accesses ----
    
    if ("git_repos_management" %in% user_accesses) sapply(c("create_git_repo_button", "delete_git_repo_button", "edit_git_repo_button"), shinyjs::show)
    if ("git_repos_remote_git_repo_management" %in% user_accesses) sapply(c("edit_readme_buttons", "save_git_repo_button"), shinyjs::show)
    
    if (debug) cat(paste0("\n", now(), " - mod_git_repos - start"))
    
    # Prevent bug leaflet ----
    observeEvent(shiny.router::get_page(), {
      req(shiny.router::get_page() == id)
      if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer shiny.router::get_page()"))
      
      # Prevent a bug with leaflet map display
      shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
    })
    
    # Initiate vars ----
    
    all_divs <- c("summary", "projects", "plugins", "data_cleaning_scripts", "datasets")
    r$list_git_repo <- tibble::tibble()
    r$map_git_repo <- tibble::tibble()
    r$loaded_git_readme <- tibble::tibble(unique_id = character(), type = character(), local_url = character())
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-current_page', 'map');"))
    
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
    
    # |-------------------------------- -----
    
    if (r$has_internet){
      
      # Git repos list and map ----
      
      ## Download git repos ----
      
      git_repos <- tibble::tibble()
      
      filename <- "https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/git_repos/git_repos.csv"
      filename_local <- paste0(r$app_folder, "/temp_files/", r$user_id, "/git_repos/git_repos.csv")
      
      ## Get csv file from remote git
      tryCatch(download.file(filename, filename_local, quiet = TRUE),
        error = function(e){
          show_message_bar(id, output, "error_loading_git_repos_csv", "warning", i18n = i18n, ns = ns)
          cat(paste0("\n", now(), " - mod_git_repos - error loading remote git csv file - error = ", toString(e)))
        })
      
      tryCatch(git_repos <- vroom::vroom(filename_local, col_types = "cnnccccl", progress = FALSE),
        error = function(e){
          show_message_bar(id, output, "error_loading_git_repos_csv", "warning", i18n = i18n, ns = ns)
          cat(paste0("\n", now(), " - mod_git_repos - error loading remote git csv file - error = ", toString(e)))
        })
      
      new_cols <- outer("name", r$languages$code, paste, sep = "_") %>% as.vector()
      for(col in new_cols) if(!col %in% colnames(git_repos)) git_repos <- git_repos %>% dplyr::mutate(!!col := "")
      
      git_repos <- git_repos %>% dplyr::mutate(name = ifelse(!is.na(get(paste0("name_", language))), get(paste0("name_", language)), name_en))
      
      ## Display map
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
      
      ## Display list or map ----
      
      observeEvent(input$show_list, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$show_list"))
        
        sapply(c("show_list_div", "explore_map", "map_git_infos_div", "map_git_readme", "save_and_cancel_readme_buttons", "edit_readme_div"), shinyjs::hide)
        sapply(c("explore_ui", "list_git_infos_div", "list_git_readme", "explore_map_and_ui_div", "git_infos_div"), shinyjs::show)
        
        if (nrow(r$list_git_repo) > 0) shinyjs::show("edit_readme_button")
        else shinyjs::hide("edit_readme_button")
        
        shinyjs::delay(50, shinyjs::show("show_map_div"))
        
        # Save that we are on the list page
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-current_page', 'list');"))
      })
      
      observeEvent(input$show_map, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$show_map"))
        
        sapply(c("show_map_div", "explore_ui", "list_git_infos_div", "list_git_readme", "save_and_cancel_readme_buttons", "edit_readme_div"), shinyjs::hide)
        sapply(c("map_git_infos_div", "map_git_readme", "explore_map", "explore_map_and_ui_div", "git_infos_div"), shinyjs::show)
        
        # sapply(c("show_map_div", "explore_ui", "list_git_infos_div", "list_git_readme"), shinyjs::hide)
        # sapply(c("map_git_infos_div", "map_git_readme", "explore_map"), shinyjs::show)
        
        if (nrow(r$map_git_repo) > 0) shinyjs::show("edit_readme_button")
        else shinyjs::hide("edit_readme_button")
        
        shinyjs::delay(50, shinyjs::show("show_list_div"))
        
        # Prevent a bug with leaflet map display
        shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
        
        # Save that we are on the map page
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-current_page', 'map');"))
      })
      
      ## Reload list ----
      
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
      
      ## A row is selected ----
      
      observeEvent(input$git_repos_dt_rows_selected, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$git_repos_dt_rows_selected"))
        
        git_repo_id <- r$git_repos_wide[input$git_repos_dt_rows_selected, ] %>% dplyr::pull(id)
        git_repo <- r$git_repos_wide %>% dplyr::filter(id == git_repo_id)
        r$list_git_repo <- git_repo
        
        # Update textfields
        shiny.fluent::updateTextField.shinyInput(session, "git_repo_name", value = git_repo$name)
        shiny.fluent::updateTextField.shinyInput(session, "git_repo_url_address", value = git_repo$repo_url_address)
        shiny.fluent::updateTextField.shinyInput(session, "git_repo_raw_files_url_address", value = git_repo$raw_files_url_address)
        shiny.fluent::updateTextField.shinyInput(session, "push_git_api_key", value = "")
        
        # Update git infos readme
        readme <- renderUI(get_git_readme(r, git_repo, type = "list"))
        output$list_git_readme <- renderUI(readme)
        output$summary_git_readme <- renderUI(readme)
        
        # Show / hide divs
        sapply(c("save_git_repo_edition_button", "edit_git_repo_div"), shinyjs::hide)
        sapply(c("edit_git_repo_button", "list_git_infos_content", "show_content_list_div", "edit_readme_button"), shinyjs::show)
        
        # Update git infos UI
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_list_git_infos_ui', Math.random());"))
      })
      
      observeEvent(input$reload_list_git_infos_ui, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$reload_list_git_infos_ui"))
        
        # Update git infos UI
        git_repo <- r$list_git_repo
        shinyjs::show("list_git_infos_buttons")
        output$list_git_infos_title <- renderUI(tags$h1(git_repo$name))
        output$list_git_infos_content <- renderUI(get_git_infos(r, git_repo, "list"))
      })
      
      ## Click on the map ----
      observeEvent(input$explore_map_marker_click, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$explore_map_marker_click"))
        
        git_repo <- git_repos %>% dplyr::filter(
          sprintf("%.6f", as.numeric(lat)) == sprintf("%.6f", as.numeric(input$explore_map_marker_click$lat)),
          sprintf("%.6f", as.numeric(lng)) == sprintf("%.6f", as.numeric(input$explore_map_marker_click$lng))
        )
        
        shiny.fluent::updateTextField.shinyInput(session, "push_git_api_key", value = "")
        
        r$map_git_repo <- git_repo
        
        readme <- get_git_readme(r, git_repo, type = "map")
        output$map_git_readme <- renderUI(readme)
        output$summary_git_readme <- renderUI(readme)
        
        # Get git repo informations
        
        if (nrow(git_repo) > 0){
          
          output$map_git_infos_title <- renderUI(tags$h1(git_repo$name))
          output$map_git_infos_content <- renderUI(get_git_infos(r, git_repo, "map"))
        }
        else {
          
          output$map_git_infos_title <- renderUI(div())
          output$map_git_infos <- div(
            shiny.fluent::MessageBar(i18n$t("error_loading_git_infos"), messageBarType = 5), 
            style = "display: inline-block; margin-top: 10px;"
          )
        }
        
        r$git_repo <- git_repo
        
        # Show readme edit button
        shinyjs::show("edit_readme_button")
      })
      
      # |-------------------------------- -----
      
      # Git repos management ----
      
      ## Add a git repo ----
      
      ### Add from map
      
      observeEvent(input$add_to_saved_git_repos, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$add_to_saved_git_repos"))
        
        req(nrow(r$map_git_repo) > 0)
        
        # Disable add button
        shiny.fluent::updateActionButton.shinyInput(session, "add_to_saved_git_repos", disabled = TRUE)
        
        git_repo <- r$map_git_repo
        
        if (paste0("name_", language) %in% colnames(git_repo)) name <- git_repo[[paste0("name_", language)]]
        else name <- git_repo$name_en
        
        # Add new data to db
        
        git_repo_new_row <- get_last_row(r$db, "git_repos") + 1
        
        new_data <- git_repo %>% dplyr::transmute(
          id = git_repo_new_row, unique_id,
          name = !!name, api_key = NA_character_, repo_url_address, raw_files_url_address,
          creator_id = r$user_id, datetime = now(), deleted = FALSE
        )
        DBI::dbAppendTable(r$db, "git_repos", new_data)
        
        new_options <-
          tibble::tribble(
            ~name, ~value, ~value_num,
            "users_allowed_read_group", "everybody", 1,
            "user_allowed_read", "", r$user_id,
          ) %>%
          dplyr::mutate(id = get_last_row(r$db, "options") + dplyr::row_number(), category = "git_repo", link_id = git_repo_new_row, .before = "name") %>%
          dplyr::mutate(creator_id = r$user_id, datetime = now(), deleted = FALSE)
        DBI::dbAppendTable(r$db, "options", new_options)
        
        # Reload list
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_git_repos', Math.random());"))
      })
      
      ### Open modal
      observeEvent(input$create_git_repo, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$create_git_repo"))
        shinyjs::show("create_git_repo_modal")
      })
      
      ### Close modal
      observeEvent(input$close_create_git_repo_modal, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$close_create_git_repo_modal"))
        shinyjs::hide("create_git_repo_modal")
      })
      
      ### Add button clicked
      observeEvent(input$add_git_repo, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$add_git_repo"))
        
        req("git_repos_management" %in% user_accesses)
        
        # Check if a textfield is empty
        empty_fields <- list()
        
        for (field in c("git_repo_creation_name", "git_repo_creation_url_address", "git_repo_creation_raw_files_url_address")){
          empty_fields[[field]] <- TRUE
          field_name <- input[[field]]
          if (length(field_name) > 0) if (!is.na(field_name) & field_name != "") empty_fields[[field]] <- FALSE
          if (empty_fields[[field]]) shiny.fluent::updateTextField.shinyInput(session, field, errorMessage = i18n$t("provide_valid_value"))
          else shiny.fluent::updateTextField.shinyInput(session, field, errorMessage = NULL)
        }
        req(!TRUE %in% empty_fields)
        
        # Check if name is already used
        git_repo_name <- input$git_repo_creation_name
        
        sql <- glue::glue_sql("SELECT id FROM git_repos WHERE LOWER(name) = {tolower(git_repo_name)}", .con = r$db)
        name_already_used <- nrow(DBI::dbGetQuery(r$db, sql) > 0)
        
        if (name_already_used) shiny.fluent::updateTextField.shinyInput(session, "git_repo_creation_name", errorMessage = i18n$t("name_already_used"))
        req(!name_already_used)
        
        # Check if git repos url and raw files url address are not already in db
        sql <- glue::glue_sql("SELECT id FROM git_repos WHERE LOWER(repo_url_address) = {tolower(input$git_repo_creation_url_address)}", .con = r$db)
        url_already_used <- nrow(DBI::dbGetQuery(r$db, sql) > 0)
        
        if (url_already_used) shiny.fluent::updateTextField.shinyInput(session, "git_repo_creation_url_address", errorMessage = i18n$t("git_repo_url_address_already_used"))
        req(!url_already_used)
        
        sql <- glue::glue_sql("SELECT id FROM git_repos WHERE LOWER(raw_files_url_address) = {tolower(input$git_repo_creation_raw_files_url_address)}", .con = r$db)
        raw_files_url_already_used <- nrow(DBI::dbGetQuery(r$db, sql) > 0)
        
        if (raw_files_url_already_used) shiny.fluent::updateTextField.shinyInput(session, "git_repo_creation_raw_files_url_address", errorMessage = i18n$t("git_repo_url_address_already_used"))
        req(!raw_files_url_already_used)
        
        # Add git repo in db
        git_repo_new_row <- get_last_row(r$db, "git_repos") + 1
        
        new_data <- tibble::tibble(
          id = git_repo_new_row, unique_id = paste0(sample(c(0:9, letters[1:6]), 64, TRUE), collapse = ''),
          name = input$git_repo_creation_name, api_key = NA_character_,
          repo_url_address = input$git_repo_creation_url_address, raw_files_url_address = input$git_repo_creation_raw_files_url_address,
          creator_id = r$user_id, datetime = now(), deleted = FALSE
        )
        DBI::dbAppendTable(r$db, "git_repos", new_data)
        
        new_options <-
          tibble::tribble(
            ~name, ~value, ~value_num,
            "users_allowed_read_group", "everybody", 1,
            "user_allowed_read", "", r$user_id,
          ) %>%
          dplyr::mutate(id = get_last_row(r$db, "options") + dplyr::row_number(), category = "git_repo", link_id = git_repo_new_row, .before = "name") %>%
          dplyr::mutate(creator_id = r$user_id, datetime = now(), deleted = FALSE)
        DBI::dbAppendTable(r$db, "options", new_options)
        
        # Reload git repos list
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_git_repos', Math.random());"))
        
        # Notify user
        show_message_bar(id, output, "git_repo_added", "success", i18n = i18n, ns = ns)
        
        # Reset fields
        for (field in c("git_repo_creation_name", "git_repo_creation_url_address", "git_repo_creation_raw_files_url_address")){
          shiny.fluent::updateTextField.shinyInput(session, field, value = "") 
        }
        
        # Reset selected git tibble
        r$list_git_repo <- tibble::tibble()
        
        # Reset git infos fields
        output$list_git_infos_title <- renderUI("")
        output$list_git_infos_content <- renderUI("")
        output$list_git_readme <- renderUI("")
        sapply(c("list_git_infos_buttons", "show_content_list_div", "edit_readme_button"), shinyjs::hide)
        
        # Close modal
        shinyjs::hide("create_git_repo_modal")
      })
      
      ## Delete git repo ----
      
      ### Open modal
      observeEvent(input$delete_git_repo, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$delete_git_repo"))
        shinyjs::show("delete_git_repo_modal")
      })
      
      ### Close modal
      observeEvent(input$close_git_repo_deletion_modal, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$close_git_repo_deletion_modal"))
        shinyjs::hide("delete_git_repo_modal")
      })
      
      ### Deletion confirmed
      observeEvent(input$confirm_git_repo_deletion, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$confirm_git_repo_deletion"))
        
        req("git_repos_management" %in% user_accesses)
        
        sql <- glue::glue_sql("DELETE FROM git_repos WHERE id = {r$list_git_repo$id}", .con = r$db)
        sql_send_statement(r$db, sql)
        
        sql <- glue::glue_sql("DELETE FROM options WHERE category = 'git_repo' AND link_id = {r$list_git_repo$id}", .con = r$db)
        sql_send_statement(r$db, sql)
        
        # Reload git repos list
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_git_repos', Math.random());"))
        
        # Notify user
        show_message_bar(id, output, "git_repo_deleted", "warning", i18n = i18n, ns = ns)
        
        # Close modal
        shinyjs::hide("delete_git_repo_modal")
        
        # Reset selected git tibble
        r$list_git_repo <- tibble::tibble()
        
        # Reset git infos fields
        output$list_git_infos_title <- renderUI("")
        output$list_git_infos_content <- renderUI("")
        output$list_git_readme <- renderUI("")
        sapply(c("list_git_infos_buttons", "show_content_list_div", "edit_readme_button"), shinyjs::hide)
      })
      
      ## Edit git repo ----
      
      observeEvent(input$edit_git_repo, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$edit_git_repo"))
        
        req("git_repos_management" %in% user_accesses)
        sapply(c("edit_git_repo_button", "list_git_infos_content"), shinyjs::hide)
        sapply(c("save_git_repo_edition_button", "edit_git_repo_div"), shinyjs::show)
      })
      
      ## Save git repo edition ----
      
      observeEvent(input$save_git_repo_edition, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$save_git_repo_edition"))
        
        req("git_repos_management" %in% user_accesses)
        
        # Check if a textfield is empty
        empty_fields <- list()
        
        for (field in c("git_repo_name", "git_repo_url_address", "git_repo_raw_files_url_address")){
          empty_fields[[field]] <- TRUE
          field_name <- input[[field]]
          if (length(field_name) > 0) if (!is.na(field_name) & field_name != "") empty_fields[[field]] <- FALSE
          if (empty_fields[[field]]) shiny.fluent::updateTextField.shinyInput(session, field, errorMessage = i18n$t("provide_valid_value"))
          else shiny.fluent::updateTextField.shinyInput(session, field, errorMessage = NULL)
        }
        req(!TRUE %in% empty_fields)
        
        # Check if name is already used
        git_repo_name <- input$git_repo_name
        git_repo_id <- r$list_git_repo$id
        
        sql <- glue::glue_sql("SELECT name FROM git_repos WHERE LOWER(name) = {tolower(git_repo_name)} AND id != {git_repo_id}", .con = r$db)
        name_already_used <- nrow(DBI::dbGetQuery(r$db, sql) > 0)
        
        if (name_already_used) shiny.fluent::updateTextField.shinyInput(session, "git_repo_name", errorMessage = i18n$t("name_already_used"))
        req(!name_already_used)
        
        # Update values on db
        sql <- glue::glue_sql("UPDATE git_repos SET name = {git_repo_name}, repo_url_address = {input$git_repo_url_address}, raw_files_url_address = {input$git_repo_raw_files_url_address} WHERE id = {git_repo_id}", .con = r$db)
        sql_send_statement(r$db, sql)
        
        # Reload list
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_git_repos', Math.random());"))
        
        # Reload selected repo UI
        r$list_git_repo <-
          r$list_git_repo %>%
          dplyr::mutate(name = git_repo_name, repo_url_address = input$git_repo_url_address, raw_files_url_address = input$git_repo_raw_files_url_address)
  
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_list_git_infos_ui', Math.random());"))
        
        sapply(c("save_git_repo_edition_button", "edit_git_repo_div"), shinyjs::hide)
        sapply(c("edit_git_repo_button", "list_git_infos_content"), shinyjs::show)
        
        # Notify user
        show_message_bar(id, output, "modif_saved", "success", i18n = i18n, ns = ns)
      })
      
      ## Edit readme ----
      
      ### Edit readme ----
      
      observeEvent(input$edit_readme, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$edit_readme"))
        
        req("git_repos_remote_git_repo_management" %in% user_accesses)
        
        # Update ace editor
        readme_url <-
          r$loaded_git_readme %>% 
          dplyr::filter(type == input$current_page & unique_id == r[[paste0(input$current_page, "_git_repo")]]$unique_id) %>%
          dplyr::pull(local_url)
        
        code <- ""
        if (file.exists(readme_url)) code <- readLines(readme_url, warn = FALSE) %>% paste(collapse = "\n")
        
        shinyAce::updateAceEditor(session, "readme_code", value = code)
        
        sapply(c("explore_map_and_ui_div", "git_infos_div", "edit_readme_button"), shinyjs::hide)
        sapply(c("save_and_cancel_readme_buttons", "edit_readme_div"), shinyjs::show)
      })
      
      ### Generate readme ----
      
      observeEvent(input$generate_readme, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$generate_readme"))
        
        req("git_repos_remote_git_repo_management" %in% user_accesses)
        
        git_repo <- r[[paste0(input$current_page, "_git_repo")]]
        
        # Load git repo
        loaded_git_repo <- tibble::tibble()
        
        tryCatch({
            loaded_git_repo <- load_git_repo(id, r, git_repo)
          }, error = function(e){
            show_message_bar(id, output, "error_loading_git_repo", "warning", i18n = i18n, ns = ns)
            cat(paste0("\n", now(), " - mod_git_repos - error downloading git repo - error = ", toString(e)))
          }
        )
        
        if (nrow(loaded_git_repo) > 0) local_path <- loaded_git_repo$local_path
        else local_path <- ""
        
        code <- ""
        
        automatic_code <- ""
        
        if (local_path != ""){
        
          for (category in c("projects", "plugins", "datasets", "data_cleaning_scripts")){
            
            icon <- switch(
              category,
              "projects" = "file-alt",
              "plugins" = "terminal",
              "data_cleaning_scripts" = "code",
              "datasets" = "database"
            )
            
            single_id <- switch(
              category, 
              "data_cleaning_scripts" = "data_cleaning_script",
              "datasets" = "dataset",
              "projects" = "project",
              "plugins" = "plugin", 
              "subsets" = "subset"
            )
            
            category_title <- category %>% stringr::str_to_title() %>% stringr::str_replace_all("_", " ")
            readme_category <- paste0("\n\n## <i class='fa fa-", icon, "' style='color: steelblue; margin-right: 5px;'></i> ", category_title)
            
            tryCatch({
              
              category_path <- file.path(local_path, category)
              subdirs <- list.dirs(category_path, full.names = TRUE, recursive = FALSE)
              
              category_elements <- tibble::tibble()
              
              for (subdir in subdirs) {
                
                xml_path <- file.path(subdir, paste0(single_id, ".xml"))
                
                if (file.exists(xml_path)) {
                  
                  element_data <-
                    xml2::read_xml(xml_path) %>%
                    XML::xmlParse() %>%
                    XML::xmlToDataFrame(stringsAsFactors = FALSE) %>%
                    tibble::as_tibble()
                  
                  category_elements <- dplyr::bind_rows(category_elements, element_data)
                }
              }
              
              if (nrow(category_elements) > 0) {
                
                category_elements <- category_elements %>% dplyr::arrange(name_en)
                
                for (i in 1:nrow(category_elements)) {
                  category_element <- category_elements[i, ]
                  description <- category_element$description_en
                  if (description != "") description <- paste0("\n", description)
                  
                  readme_category <- paste0(
                    readme_category, "\n\n",
                    "<details style = 'border: solid 1px #c0c0c0; padding: 5px 10px; margin: 5px 0;'>\n",
                    "<summary><span style = 'font-size:13px;'>", category_element$name_en, "</summary>\n",
                    description,
                    "</details>"
                  )
                }
              }
              
              if (nrow(category_elements) == 0) {
                readme_category <- paste0(
                  readme_category, 
                  "\n\nNo ", 
                  stringr::str_replace_all(category, "_", " "),
                  " available."
                )
              }
              
              automatic_code <- paste0(automatic_code, readme_category)
              
            }, error = function(e){
              show_message_bar(id, output, "error_generate_readme", "warning", i18n = i18n, ns = ns)
              cat(paste0("\n", now(), " - mod_git_repos - error generating readme - category = ", category, " - error = ", toString(e)))
            })
          }
        }
        
        # Add span to identity automatically generated code
        # automatic_code <- paste0(
        #   "\n\n<span id='generated_code_start'></span>",
        #   automatic_code,
        #   "\n\n<span id='generated_code_end'></span>"
        # )
        
        current_content <- input$readme_code
        
        extract_section <- function(content, start_marker, end_marker) {
          pattern <- paste0(start_marker, "(.*?)", end_marker)
          matches <- regmatches(content, regexec(pattern, content, dotall = TRUE))[[1]]
          if (length(matches) > 1) {
            return(matches[2])
          }
          return("")
        }
        
        get_content_before <- function(content, marker) {
          parts <- strsplit(content, marker)[[1]]
          if (length(parts) > 0) return(parts[1])
          return("")
        }
        
        get_content_after <- function(content, marker) {
          parts <- strsplit(content, marker)[[1]]
          if (length(parts) > 1) return(parts[2])
          return("")
        }
        
        content_before <- get_content_before(current_content, "<span id='generated_code_start'></span>")
        content_after <- get_content_after(current_content, "<span id='generated_code_end'></span>")
        
        code <- paste0(
          if (nchar(trimws(content_before)) == 0) paste0("# ", git_repo$name) else trimws(content_before),
          "\n\n<span id='generated_code_start'></span>",
          automatic_code,
          "\n\n<span id='generated_code_end'></span>",
          if (nchar(trimws(content_after)) > 0) paste0("\n\n", trimws(content_after)) else ""
        )

        shinyAce::updateAceEditor(session, "readme_code", value = code)
        
        # Run code
        output_file <- create_rmarkdown_file(r, code, interpret_code = FALSE)
        output[[paste0(input$current_page, "_git_readme")]] <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(output_file))))
      })
      
      ### Run readme code ----
      
      observeEvent(input$run_readme_code, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$run_readme_code"))
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_readme_code_trigger', Math.random());"))
      })
      
      observeEvent(input$readme_code_run_all, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$readme_code_run_all"))
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-run_readme_code_trigger', Math.random());"))
      })
      
      observeEvent(input$run_readme_code_trigger, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$run_readme_code_trigger"))

        output_file <- create_rmarkdown_file(r, input$readme_code, interpret_code = FALSE)
        output[[paste0(input$current_page, "_git_readme")]] <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(output_file))))
      })
      
      ### Save readme updates ----
      
      observeEvent(input$readme_code_save, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$readme_code_save"))
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_readme_trigger', Math.random());"))
      })
      
      observeEvent(input$save_readme, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$save_readme"))

        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-save_readme_trigger', Math.random());"))

        sapply(c("save_and_cancel_readme_buttons", "edit_readme_div"), shinyjs::hide)
        sapply(c("explore_map_and_ui_div", "git_infos_div", "edit_readme_button"), shinyjs::show)
        
        # Prevent a bug with leaflet map display
        shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
      })
      
      observeEvent(input$save_readme_trigger, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$save_readme_trigger"))
        
        req("git_repos_remote_git_repo_management" %in% user_accesses)
        
        git_repo <- r[[paste0(input$current_page, "_git_repo")]]
        
        # Load git repo
        loaded_git_repo <- tibble::tibble()
        
        tryCatch({
            loaded_git_repo <- load_git_repo(id, r, git_repo)
          }, error = function(e){
            show_message_bar(id, output, "error_loading_git_repo", "warning", i18n = i18n, ns = ns)
            cat(paste0("\n", now(), " - mod_git_repos - error downloading git repo - error = ", toString(e)))
          }
        )
        
        # Update README.md
        if (nrow(loaded_git_repo) > 0){
          file_path <- paste0(loaded_git_repo$local_path, "/README.md")
          writeLines(input$readme_code, file_path)
          
          copy_file_path <-
            r$loaded_git_readme %>% 
            dplyr::filter(type == input$current_page & unique_id == git_repo$unique_id) %>%
            dplyr::pull(local_url)
          writeLines(input$readme_code, copy_file_path)
          
          # Open commit modal
          shinyjs::show("push_git_modal")
        }
       
        # Reload markdown
        if (input$readme_code == "") output$readme_ui <- renderUI(div(shiny.fluent::MessageBar(i18n$t("no_description_available"), messageBarType = 5) ,style = "display: inline-block;"))
        else {
          output_file <- create_rmarkdown_file(r, input$readme_code, interpret_code = FALSE)
          output$readme_ui <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(output_file))))
        }
      })
      
      ### Close git push modal
      observeEvent(input$close_push_git_modal, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$close_push_git_modal"))
        shinyjs::hide("push_git_modal")
      })
      
      ### Confirm readme update on remote git
      observeEvent(input$confirm_push_git_update, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$confirm_push_git_update"))
        
        tryCatch({
          git_repo <- r[[paste0(input$current_page, "_git_repo")]]
          repo <- r$loaded_git_repos_objects[[git_repo$unique_id]]
          
          username <- r$users %>% dplyr::filter(id == r$user_id) %>% dplyr::pull(name)
          git2r::config(repo, user.name = username)
          git2r::add(repo, ".")
          
          if (length(git2r::status(repo, unstaged = FALSE, untracked = FALSE, ignored = FALSE)$staged) > 0){

            commit_message <- input$push_git_commit_message
            if (commit_message == "") commit_message <- paste0("Update README from LinkR")
            git2r::commit(repo, message = commit_message)

            # Create main branch if doesn't exist
            git_branches <- names(git2r::branches(repo))
            if (!"main" %in% git_branches){
              git2r::branch_create(commit = git2r::last_commit(repo), name = "main")
              git2r::checkout(repo, "main")
            }

            credentials <- git2r::cred_user_pass("linkr_user", input$push_git_api_key)

            git2r::push(repo, "origin", "refs/heads/main", credentials = credentials)
            
            # Reset commit message (we keep API key, easier when we push files frequently)
            shiny.fluent::updateTextField.shinyInput(session, "push_git_commit_message", value = "")

            # Notify user
            show_message_bar(id, output, "success_update_remote_git_repo", "success", i18n = i18n, ns = ns)
          }
        }, error = function(e){
          show_message_bar(id, output, message = "error_update_remote_git_repo", type = "severeWarning", i18n = i18n, ns = ns)
          cat(paste0("\n", now(), " - mod_git_repos - update remot git error - ", toString(e)))
        })
        
        # Close git push modal
        shinyjs::hide("push_git_modal")
      })
      
      ### Cancel readme updates ----
      
      observeEvent(input$cancel_readme, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$cancel_readme"))

        sapply(c("save_and_cancel_readme_buttons", "edit_readme_div"), shinyjs::hide)
        sapply(c("explore_map_and_ui_div", "git_infos_div", "edit_readme_button"), shinyjs::show)
        
        # Prevent a bug with leaflet map display
        shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")

        # Reset readme editor with last value
        readme_url <-
          r$loaded_git_readme %>% 
          dplyr::filter(type == input$current_page & unique_id == r[[paste0(input$current_page, "_git_repo")]]$unique_id) %>%
          dplyr::pull(local_url)
        
        code <- ""
        if (file.exists(readme_url)) code <- readLines(readme_url, warn = FALSE) %>% paste(collapse = "\n")
        
        shinyAce::updateAceEditor(session, "readme_code", value = code)
        
        if (code == "") output$readme_ui <- renderUI(div(shiny.fluent::MessageBar(i18n$t("no_description_available"), messageBarType = 5) ,style = "display: inline-block;"))
        else {
          output_file <- create_rmarkdown_file(r, code, interpret_code = FALSE)
          output$readme_ui <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(output_file))))
        }
      })
      
      # |-------------------------------- -----
      
      # A repo is selected ----
      
      ## Selected from DT
      observeEvent(input$show_content_list, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$show_content_list"))
        
        r$git_repo <- r$list_git_repo
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_content_trigger', Math.random());"))
      })
      
      ## Selected from the map
      observeEvent(input$show_content_map, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$show_content_map"))
        
        r$git_repo <- r$map_git_repo
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_content_trigger', Math.random());"))
      })
      
      ## Sidenav reload git button
      observeEvent(input$reload_git_repo, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$reload_git_repo"))
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_content_trigger', Math.random());"))
      })
      
      observeEvent(input$show_content_trigger, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$show_content_trigger"))
        
        sapply(c("one_repo_reduced_sidenav", "one_git_repo"), shinyjs::show)
        sapply(c("all_repos_reduced_sidenav", "all_git_repos"), shinyjs::hide)
        
        output$breadcrumb <- renderUI(create_breadcrumb(c(1, 2), i18n))
        
        # Clone git repo if not already loaded
        
        loaded_git_repo <- tibble::tibble()
        
        tryCatch({
            loaded_git_repo <- load_git_repo(id, r, r$git_repo)
          }, error = function(e){
            show_message_bar(id, output, "error_loading_git_repo", "warning", i18n = i18n, ns = ns)
            cat(paste0("\n", now(), " - mod_git_repos - error downloading git repo - error = ", toString(e)))
          }
        )
        
        if (nrow(loaded_git_repo) > 0) local_path <- loaded_git_repo$local_path
        else local_path <- ""
        
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-git_repo_local_path', '", local_path, "');"))
        
        # Return to summary tab (in case we have clicked on reload_git_repo)
        shinyjs::runjs(paste0("
          Shiny.setInputValue('", id, "-current_tab', '", id, "-projects');
          Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());"
        ))
      })
      
      ## Repo current tab ----
      
      observeEvent(input$current_tab_trigger, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$current_tab_trigger"))
        
        # Hide element details
        shinyjs::hide("element_details_div")
        
        current_tab <- gsub(paste0(id, "-"), "", input$current_tab, fixed = FALSE)
        shinyjs::show("widgets_div")
        
        shinyjs::show("one_repo_reduced_sidenav")
        shinyjs::hide("all_repos_reduced_sidenav")
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_hide_sidenav', 'hide');"))
        
        # Change selected tab
        sapply(all_divs, function(button_id) shinyjs::removeClass(class = "selected_pivot_item", selector = paste0("#", id, "-", button_id)))
        shinyjs::addClass(class = "selected_pivot_item", selector = paste0("#", id, "-", current_tab))
        
        # Update breadcrumb
        output$breadcrumb <- renderUI(create_breadcrumb(c(1, 2, 3), i18n, current_tab))
        
        # Reload widgets UI
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_elements_list', Math.random());"))
      })
      
      ## Reload widgets ----
      
      observeEvent(input$reload_elements_list, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$reload_elements_list"))
        
        current_tab <- gsub(paste0(id, "-"), "", input$current_tab, fixed = FALSE)
        
        single_id <- switch(
          current_tab, 
          "data_cleaning_scripts" = "data_cleaning_script", 
          "datasets" = "dataset",
          "projects" = "project", 
          "plugins" = "plugin", 
          "subsets" = "subset"
        ) 
        
        elements <- tibble::tibble()
        error_loading_xml_file <- TRUE
        
        tryCatch({  
          category_dir <- file.path(input$git_repo_local_path, current_tab)
          elements <- get_elements_from_xml(category_dir = category_dir, single_id = single_id)
          error_loading_xml_file <- FALSE
        }, error = function(e) cat(paste0("\n", now(), " - mod_git_repos - error downloading xml file : ", toString(e))))
        
        r$loaded_git_repo_elements <- elements
        
        if (error_loading_xml_file) elements_ui <- div(
          shiny.fluent::MessageBar(i18n$t("error_loading_category_xml_file"), messageBarType = 5), 
          style = "display: inline-block;"
        )
        else if (nrow(elements) == 0) elements_ui <- div(
          shiny.fluent::MessageBar(i18n$t("no_elements_to_display"), messageBarType = 5), 
          style = "display: inline-block;"
        )
        
        else {
          elements_ui <- tagList()
          
          for (i in 1:nrow(elements)){
            row <- elements[i, ]
            
            if (paste0("name_", language) %in% colnames(row)) element_name <- row[[paste0("name_", language)]]
            else element_name <- row$name_en
            
            users_ui <- create_authors_ui(row$authors)
            
            max_length <- 45
            if (nchar(element_name) > max_length) element_name <- paste0(substr(element_name, 1, max_length - 3), "...")
              
            widget_buttons <- tagList()
            
            # For plugins widgets, we add some content on the bottom
            
            if (current_tab == "plugins"){
              plugin_type <- row %>% dplyr::slice(1) %>% dplyr::pull(type)
              widget_buttons <- get_plugin_buttons(id, "tab_type", plugin_type, row$id, i18n)
            }
            
            onclick <- paste0("
              Shiny.setInputValue('", id, "-selected_element', '", row$unique_id, "');
              Shiny.setInputValue('", id, "-selected_element_trigger', Math.random());
            ")
            
            short_description <- row[[paste0("short_description_", language)]]
            
            elements_ui <- tagList(
              create_element_ui(ns, page_id = id, i, single_id, element_name, users_ui, widget_buttons, onclick, short_description, FALSE),
              elements_ui
            )
          }
        }

        elements_ui <- div(elements_ui, class = paste0(current_tab, "_container"))
        output$elements <- renderUI(elements_ui)
      })
      
      ## Return to all repos page ----
      
      observeEvent(input$show_home, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$show_home"))
        
        sapply(c("all_repos_reduced_sidenav", "all_git_repos"), shinyjs::show)
        sapply(c("one_repo_reduced_sidenav", "one_git_repo"), shinyjs::hide)
        
        # Prevent a bug with leaflet map display
        shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
      })
      
      ## An element is selected ----
      
      observeEvent(input$selected_element_trigger, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$selected_element_trigger"))
        
        git_element <- r$loaded_git_repo_elements %>% dplyr::filter(unique_id == input$selected_element)
        
        current_tab <- gsub(paste0(id, "-"), "", input$current_tab, fixed = FALSE)
        current_tab_single <- switch(
          current_tab, 
          "data_cleaning_scripts" = "data_cleaning_script", 
          "datasets" = "dataset",
          "projects" = "study", 
          "plugins" = "plugin"
        )
        
        if (current_tab == "data_cleaning_scripts") sql_table <- "scripts"
        else if (current_tab == "projects") sql_table <- "studies"
        else sql_table <- current_tab
        
        # Update breadcrumb
        if (paste0("name_", language) %in% colnames(git_element)) git_element_name <- git_element[[paste0("name_", language)]]
        else git_element_name <- git_element$name_en
        
        output$breadcrumb <- renderUI(create_breadcrumb(c(1, 2, 3, 4), i18n, current_tab, git_element_name))
        
        # Show element description
        if (paste0("description_", language) %in% colnames(git_element)) description <- git_element %>% dplyr::select(!!rlang::sym(paste0("description_", language))) %>% dplyr::pull()
        else description <- git_element$description_en
        
        if (description == "") output$element_details_description <- renderUI(div(shiny.fluent::MessageBar(i18n$t("no_description_available"), messageBarType = 5) ,style = "display: inline-block;"))
        else {
          description <- description %>% includeMarkdown()
          output$element_details_description <- renderUI(div(description))
        }
        
        # Update installation UI
        output$element_details_title <- renderUI(tags$h1(i18n$t(paste0("install_", current_tab_single))))
        
        ## Does the element is already installed locally?
        
        sql <- glue::glue_sql(paste0(
          "SELECT * FROM {sql_table} WHERE id = (",
            "SELECT link_id FROM options WHERE category = {current_tab_single} AND name = 'unique_id' AND value = {input$selected_element}",
          ")"), .con = r$db)
        local_element <- DBI::dbGetQuery(r$db, sql)
        
        if (nrow(local_element) > 0){
          
          sql <- glue::glue_sql("SELECT value FROM options WHERE category = {current_tab_single} AND link_id = {local_element$id} AND name = 'version'", .con = r$db)
          element_version <- DBI::dbGetQuery(r$db, sql) %>% dplyr::pull()
          local_version <- package_version(element_version)
          git_version <- package_version(git_element$version)
          
          git_element_ui <- div(
            tags$table(
              tags$tr(tags$td(strong(i18n$t("name")), style = "min-width: 80px;"), tags$td(git_element_name)),
              tags$tr(tags$td(strong(i18n$t("author_s"))), tags$td(git_element$authors)),
              tags$tr(tags$td(strong(i18n$t("created_on"))), tags$td(git_element$creation_datetime)),
              tags$tr(tags$td(strong(i18n$t("updated_on"))), tags$td(git_element$update_datetime)),
              tags$tr(tags$td(strong(i18n$t("local_version"))), tags$td(element_version)),
              tags$tr(tags$td(strong(i18n$t("remote_git_version"))), tags$td(git_element$version))
            ),
            class = "git-table"
          )
          
          # Update synchronize buttons
          if (local_version == git_version) install_button <- shiny.fluent::DefaultButton.shinyInput(ns("git_repo_element_already_installed"), i18n$t("local_version_up_to_date"), disabled = TRUE)
          else if (local_version > git_version) install_button <- shiny.fluent::DefaultButton.shinyInput(ns("git_repo_element_local_version_more_recent"), i18n$t("element_local_version_more_recent"), disabled = TRUE)
          else install_button <- shiny.fluent::PrimaryButton.shinyInput(ns("git_install_element"), i18n$t("update"))
        }
        else {
          git_element_ui <- div(
            shiny.fluent::MessageBar(i18n$t(paste0(current_tab_single, "_doesnt_exist_locally")), messageBarType = 5), 
            style = "display: inline-block;"
          )
          
          # Update synchronize buttons
          install_button <- shiny.fluent::PrimaryButton.shinyInput(ns("git_install_element"), i18n$t("install"))
        }
        
        # Install and delete are authorized for this user?
        if ("git_repos_install_remote_git_element" %not_in% user_accesses) install_button <- ""
        if ("git_repos_remote_git_repo_management" %in% user_accesses) delete_button <- div(shiny.fluent::PrimaryButton.shinyInput(ns("delete_element_from_git"), i18n$t("delete_element_from_remote_git")), class = "delete_button")
        else delete_button <- ""
        
        output$element_details_ui <- renderUI(git_element_ui)
        output$synchronize_git_buttons <- renderUI(
          div(
            delete_button,
            install_button,
            style = "display: flex; gap: 5px;"
          )
        )
        
        shinyjs::hide("widgets_div")
        shinyjs::show("element_details_div")
      })
      
      ## Edit description ----
      
      observeEvent(input$edit_description, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$edit_description"))
        
        sapply(c("edit_description_button", "summary_informations_div"), shinyjs::hide)
        sapply(c("save_and_cancel_description_buttons", "edit_description_div"), shinyjs::show)
      })
      
      ## Install an element ----
      
      observeEvent(input$git_install_element, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$git_install_element"))
        
        current_tab <- gsub(paste0(id, "-"), "", input$current_tab, fixed = FALSE)
        if (current_tab == "projects") shinyjs::show("update_project_plugins_modal")
        else shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-git_install_element_confirmed', Math.random());"))
      })
      
      observeEvent(input$confirm_project_plugins_import, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$confirm_project_plugins_import"))
        shinyjs::hide("update_project_plugins_modal")
        
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-import_project_plugins', true);"))
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-git_install_element_confirmed', Math.random());"))
      })
      
      observeEvent(input$close_project_plugins_import_modal, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$close_project_plugins_import_modal"))
        shinyjs::hide("update_project_plugins_modal")
        
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-import_project_plugins', false);"))
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-git_install_element_confirmed', Math.random());"))
      })
      
      observeEvent(input$git_install_element_confirmed, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$git_install_element_confirmed"))
        
        current_tab <- gsub(paste0(id, "-"), "", input$current_tab, fixed = FALSE)
        current_tab_single <- switch(
          current_tab, 
          "data_cleaning_scripts" = "data_cleaning_script", 
          "datasets" = "dataset",
          "projects" = "project", 
          "plugins" = "plugin"
        )
        
        sql_table <- switch(
          current_tab, 
          "data_cleaning_scripts" = "scripts", 
          "datasets" = "datasets",
          "projects" = "studies", 
          "plugins" = "plugins"
        )
        
        sql_category <- switch(
          current_tab, 
          "data_cleaning_script" = "data_cleaning",
          "datasets" = "dataset",
          "projects" = "study",
          "plugins" = "plugin"
        )
        
        single_id <- switch(
          current_tab, 
          "data_cleaning_scripts" = "data_cleaning_script",
          "datasets" = "dataset",
          "projects" = "project",
          "plugins" = "plugin"
        )
        
        tryCatch({
          
          git_element <- r$loaded_git_repo_elements %>% dplyr::filter(unique_id == input$selected_element)
          git_folder <- r$loaded_git_repos %>% dplyr::filter(unique_id == r$git_repo$unique_id) %>% dplyr::pull(local_path)
          element_name <- git_element %>% dplyr::pull(name_en) %>% remove_special_chars()
          git_element_folder <- file.path(git_folder, current_tab, element_name)
          
          import_element(
            id = id, input = input, output = output, r = r, m = m, con = r$db, sql_table = sql_table, sql_category = sql_category, single_id = single_id,
            unique_id = input$selected_element, element = git_element, element_type = current_tab, temp_dir = git_element_folder,
            user_accesses = user_accesses
          )
        },
        error = function(e){
          show_message_bar(id, output, paste0("error_install_remote_git_", current_tab_single), "warning", i18n = i18n, ns = ns)
          cat(paste0("\n", now(), " - mod_git_repos - error installing ", current_tab_single, " from git - error = ", toString(e)))
        })
      })
      
      ## Delete element from git ----
      
      ### Open modal
      observeEvent(input$delete_element_from_git, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$delete_element_from_git"))
        shinyjs::show("delete_git_element_modal")
      })
      
      ### Close modal
      observeEvent(input$close_git_element_deletion_modal, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$close_git_element_deletion_modal"))
        shinyjs::hide("delete_git_element_modal")
      })
      
      ### Deletion confirmed
      observeEvent(input$confirm_git_element_deletion, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$confirm_git_element_deletion"))
        
        tryCatch({
          current_tab <- gsub(paste0(id, "-"), "", input$current_tab, fixed = FALSE)
          git_element <- r$loaded_git_repo_elements %>% dplyr::filter(unique_id == input$selected_element)
          
          # Delete git element files
          git_folder <- r$loaded_git_repos %>% dplyr::filter(unique_id == r$git_repo$unique_id) %>% dplyr::pull(local_path)
          git_category_folder <- paste0(git_folder, "/", current_tab)
          for (file in list.files(git_category_folder, full.names = TRUE)) if (grepl(git_element$unique_id, file)) git_element_folder <- file
          
          unlink(git_element_folder, recursive = TRUE)
          
          # Return to elements page
          shinyjs::runjs(paste0(
            "Shiny.setInputValue('", id, "-current_tab', '", current_tab, "');",
            "Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());"
          ))
        },
        error = function(e){
          show_message_bar(id, output, "error_removing_git_element", "severeWarning", i18n = i18n, ns = ns)
          cat(paste0("\n", now(), " - mod_git_repos - error removing git element - error = ", toString(e)))
        })
        
        shinyjs::hide("delete_git_element_modal")
      })
      
      ## Commit and push with updates ----
      
      observeEvent(input$save_git_repo, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$save_git_repo"))
        shinyjs::show("push_git_modal")
      })
      
      ## Return to selected repo page ----
      
      observeEvent(input$show_git_repo, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$show_git_repo"))
        
        shinyjs::hide("element_details_div")
        shinyjs::show("widgets_div")
        
        current_tab <- gsub(paste0(id, "-"), "", input$current_tab, fixed = FALSE)
        
        output$breadcrumb <- renderUI(create_breadcrumb(c(1, 2, 3), i18n, current_tab))
      })
    }
    
    # |-------------------------------- -----
    
    ## Module functions ----
    
    get_git_infos <- function(r, git_repo, git_source){
      
      if (git_source == "map"){
        
        # Enable or disable add button
        sql <- glue::glue_sql("SELECT id FROM git_repos WHERE unique_id = {git_repo$unique_id}", .con = r$db)
        result <- DBI::dbGetQuery(r$db, sql)
        if (nrow(result) == 0) disabled <- FALSE
        else disabled <- TRUE
        add_git_repo_button <- shiny.fluent::PrimaryButton.shinyInput(ns("add_to_saved_git_repos"), i18n$t("add_to_saved_git_repos"), iconProps = list(iconName = "Add"), disabled = disabled)
      }
      else add_git_repo_button <- div()
      
      tagList(
        div(
          strong(i18n$t("url_address")), tags$span(":", style = "margin: 0 5px;"),
          shiny.fluent::Link(href = git_repo$repo_url_address, git_repo$repo_url_address, target = "_blank"),
          style = "display: flex;"
        ),
        div(
          id = ns(paste0("show_content_", git_source, "_div")),
          add_git_repo_button,
          shiny.fluent::PrimaryButton.shinyInput(ns(paste0("show_content_", git_source)), i18n$t("show_content"), iconProps = list(iconName = "Play")),
          class = "git_repo_infos_modal_buttons",
          style = "display: flex; gap: 5px;"
        )
      )
    }
    
    get_git_readme <- function(r, git_repo, type){
      
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
          
          # Save local url
          if (nrow(r$loaded_git_readme %>% dplyr::filter(unique_id == git_repo$unique_id & type == !!type)) == 0) r$loaded_git_readme <-
            r$loaded_git_readme %>%
            dplyr::bind_rows(tibble::tibble(unique_id = git_repo$unique_id, type = type, local_url = new_file))
          
          download.file(filename_remote, new_file, quiet = TRUE)
          
          con <- textConnection(new_file)
          readme <- div(readLines(con, warn = FALSE) %>% includeMarkdown())
          close(con)
        },
        error = function(e) cat(paste0("\n", now(), " - mod_git_repos - error loading remote git readme - error = ", toString(e))))
      } 
      
      readme
    }
    
    create_breadcrumb <- function(levels, i18n, current_tab = "", selected_element_name = ""){
      
      items <- list()
      
      if (1 %in% levels) items <- rlist::list.append(
        items,
        list(
          key = "main", text = i18n$t("git_repos"), href = shiny.router::route_link("git_repos"),
          onClick = htmlwidgets::JS(paste0("item => { Shiny.setInputValue('", id, "-show_home', Math.random()); }"))
        )
      )
      
      if (2 %in% levels) items <- rlist::list.append(
        items,
        list(
          key = "git_repo", text = r$git_repo$name,
          onClick = htmlwidgets::JS(paste0("item => { Shiny.setInputValue('", id, "-show_home', Math.random()); }"))
        )
      )
      
      if (3 %in% levels){
        if (4 %in% levels) onclick <- htmlwidgets::JS(paste0("item => { Shiny.setInputValue('", id, "-show_git_repo', Math.random()); }"))
        else onclick <- ""
        
        items <- rlist::list.append(
          items,
          list(
            key = current_tab, text = i18n$t(current_tab),
            onClick = onclick
          )
        )
      }
      
      if (4 %in% levels) items <- rlist::list.append(
        items,
        list(key = "element", text = selected_element_name)
      )
      
      shiny.fluent::Breadcrumb(items = items)
    }
    
  })
}
