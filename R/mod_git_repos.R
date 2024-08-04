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
                      create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("delete_git_repo"), iconProps = list(iconName = "Delete")), text = i18n$t("delete")),
                      div(
                        id = ns("edit_git_repo_button"),
                        create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("edit_git_repo"), iconProps = list(iconName = "Edit")), text = i18n$t("edit"))
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
            div(
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
            ),
            class = "small_icon_button",
            style = "position: absolute; top: 0; right: 10px;"
          ),
          uiOutput(ns("map_git_readme")),
          shinyjs::hidden(uiOutput(ns("list_git_readme"))),
          class = "widget markdown_widget markdown"
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
            tags$button(id = ns("data_cleaning"), i18n$t("data_cleaning"), class = "pivot_item", onclick = pivot_item_js),
            tags$button(id = ns("datasets"), i18n$t("datasets"), class = "pivot_item", onclick = pivot_item_js),
            class = "pivot"
          ),
          style = "display: flex; justify-content: space-between; z-index: 100;"
        ),
        
        ## Summary ----
        div(
          id = ns("summary_div"),
          div(
            div(
              div(
                shinyjs::hidden(
                  div(
                    id = ns("edit_description_button"),
                    create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("edit_description"), iconProps = list(iconName = "Edit")), text = i18n$t("edit_description"))
                  )
                ),
                shinyjs::hidden(
                  div(
                    id = ns ("save_and_cancel_description_buttons"),
                    div(
                      id = ns("cancel_description_button"),
                      create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("cancel_description"), iconProps = list(iconName = "Cancel")), text = i18n$t("cancel_description_updates"))
                    ),
                    div(
                      id = ns("save_description_button"),
                      create_hover_card(ui = shiny.fluent::IconButton.shinyInput(ns("save_description"), iconProps = list(iconName = "Accept")), text = i18n$t("save_description")),
                    ),
                    style = "display: flex;"
                  )
                ),
                style = "margin-top: 5px;"
              ),
              uiOutput(ns("summary_git_readme")),
              class = "widget markdown_widget",
            ),
            class = "git_repos_summary"
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
        
        ## Selected element details ----
        shinyjs::hidden(
          div(
            id = ns("element_details_div"),
            div(
              div(
                uiOutput(ns("element_details_title")),
                div(uiOutput(ns("element_details_ui")), style = "margin-top:10px;"),
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
                uiOutput(ns("element_details_description")),
                class = "widget markdown_widget markdown",
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
    
    # Commit and push readme modal ----
    shinyjs::hidden(
      div(
        id = ns("push_git_readme_modal"),
        div(
          tags$h1(i18n$t("push_git_readme_title")),
          shiny.fluent::TextField.shinyInput(ns("push_git_readme_api_key"), type = "password", canRevealPassword = TRUE, label = i18n$t("api_key")),
          shiny.fluent::TextField.shinyInput(ns("push_git_readme_commit_message"), label = i18n$t("commit_message")),
          div(
            shiny.fluent::DefaultButton.shinyInput(ns("close_push_git_readme_modal"), i18n$t("cancel")),
            shiny.fluent::PrimaryButton.shinyInput(ns("confirm_push_git_readme_update"), i18n$t("update")),
            class = "push_git_readme_modal_buttons"
          ),
          class = "push_git_readme_modal_content"
        ),
        class = "push_git_readme_modal"
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
    
    all_divs <- c("summary", "projects", "plugins", "data_cleaning", "datasets")
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
    
    # Internet access ----
    
    if (r$has_internet){
      
      # Display list or map ----
      
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
        
        # Update textfields
        shiny.fluent::updateTextField.shinyInput(session, "git_repo_name", value = git_repo$name)
        shiny.fluent::updateTextField.shinyInput(session, "git_repo_url_address", value = git_repo$repo_url_address)
        shiny.fluent::updateTextField.shinyInput(session, "git_repo_raw_files_url_address", value = git_repo$raw_files_url_address)
        shiny.fluent::updateTextField.shinyInput(session, "push_git_readme_api_key", value = "")
        
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
        output$list_git_infos_content <- renderUI(get_git_infos(git_repo, "list"))
      })
      
      # Add a git repo ----
      
      ## Open modal
      observeEvent(input$create_git_repo, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$create_git_repo"))
        shinyjs::show("create_git_repo_modal")
      })
      
      ## Close modal
      observeEvent(input$close_create_git_repo_modal, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$close_create_git_repo_modal"))
        shinyjs::hide("create_git_repo_modal")
      })
      
      ## Add button clicked
      observeEvent(input$add_git_repo, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$add_git_repo"))
        
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
        show_message_bar(output, "git_repo_added", "success", i18n = i18n, ns = ns)
        
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
      
      # Delete git repo ----
      
      ## Open modal
      observeEvent(input$delete_git_repo, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$delete_git_repo"))
        shinyjs::show("delete_git_repo_modal")
      })
      
      ## Close modal
      observeEvent(input$close_git_repo_deletion_modal, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$close_git_repo_deletion_modal"))
        shinyjs::hide("delete_git_repo_modal")
      })
      
      ## Deletion confirmed
      observeEvent(input$confirm_git_repo_deletion, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$confirm_git_repo_deletion"))
        
        sql <- glue::glue_sql("DELETE FROM git_repos WHERE id = {r$list_git_repo$id}", .con = r$db)
        sql_send_statement(r$db, sql)
        
        sql <- glue::glue_sql("DELETE FROM options WHERE category = 'git_repo' AND link_id = {r$list_git_repo$id}", .con = r$db)
        sql_send_statement(r$db, sql)
        
        # Reload git repos list
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_git_repos', Math.random());"))
        
        # Notify user
        show_message_bar(output, "git_repo_deleted", "warning", i18n = i18n, ns = ns)
        
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
      
      # Edit git repo ----
      
      observeEvent(input$edit_git_repo, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$edit_git_repo"))
        
        sapply(c("edit_git_repo_button", "list_git_infos_content"), shinyjs::hide)
        sapply(c("save_git_repo_edition_button", "edit_git_repo_div"), shinyjs::show)
      })
      
      # Save git repo edition ----
      
      observeEvent(input$save_git_repo_edition, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$save_git_repo_edition"))
        
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
        show_message_bar(output, "modif_saved", "success", i18n = i18n, ns = ns)
      })
      
      # Edit readme ----
      
      ## Edit readme ----
      
      observeEvent(input$edit_readme, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$edit_readme"))
        
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
      
      ## Generate readme ----
      
      observeEvent(input$generate_readme, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$generate_readme"))
        
        git_repo <- r[[paste0(input$current_page, "_git_repo")]]
        
        # Load git repo
        loaded_git_repo <- tibble::tibble()
        
        tryCatch({
            loaded_git_repo <- load_git_repo(id, r, git_repo)
          }, error = function(e){
            show_message_bar(output, "error_loading_git_repo", "warning", i18n = i18n, ns = ns)
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
              
              # Load XML
              xml_path <- paste0(local_path, "/", category, "/", category, ".xml")
              
              category_elements <-
                xml2::read_xml(xml_path) %>%
                XML::xmlParse() %>%
                XML::xmlToDataFrame(nodes = XML::getNodeSet(., paste0("//", single_id)), stringsAsFactors = FALSE) %>%
                tibble::as_tibble()
              
              if (nrow(category_elements) > 0){
                
                category_elements <- category_elements %>% dplyr::arrange(name_en)
                
                for (i in 1:nrow(category_elements)){
  
                  category_element <- category_elements[i, ]
  
                  # category_folder <- paste0(local_path "/", category, "/")
                  # category_folder <- paste0(category_folder, category_element$unique_id)
                  
                  description <- category_element$description_en #%>%
                  #   stringr::str_replace_all("%element_folder%", category_folder)
                  
                  readme_category <- paste0(
                    readme_category, "\n\n",
                    "<details style = 'border: solid 1px #c0c0c0; padding: 5px 10px; margin: 5px 0;'>\n",
                    "<summary><span style = 'font-size:13px;'>", category_element$name_en, "</summary>\n\n",
                    description, "\n\n",
                    "</details>"
                  )
                }
              }
              
              if (nrow(category_elements) == 0) readme_category <- paste0(readme_category, "\n\nNo ", stringr::str_replace_all(category, "_", " "), " available.")
              
              automatic_code <- paste0(automatic_code, readme_category)
              
            }, error = function(e){
              show_message_bar(output, "error_generate_readme", "warning", i18n = i18n, ns = ns)
              cat(paste0("\n", now(), " - mod_git_repos - error generating readme - category = ", category, " - error = ", toString(e)))
            })
          }
        }
        
        # Add span to identity automatically generated code
        automatic_code <- paste0(
          "\n\n<span id='generated_code_start'></span>",
          automatic_code,
          "\n\n<span id='generated_code_end'></span>"
        )
        
        code <- paste0("# ", git_repo$name, automatic_code)

        shinyAce::updateAceEditor(session, "readme_code", value = code)
        
        # Run code
        output_file <- create_rmarkdown_file(r, code, interpret_code = FALSE)
        output[[paste0(input$current_page, "_git_readme")]] <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(output_file))))
      })
      
      ## Run readme code ----
      
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
      
      ## Save readme updates ----
      
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
        
        git_repo <- r[[paste0(input$current_page, "_git_repo")]]
        
        # Load git repo
        loaded_git_repo <- tibble::tibble()
        
        tryCatch({
            loaded_git_repo <- load_git_repo(id, r, git_repo)
          }, error = function(e){
            show_message_bar(output, "error_loading_git_repo", "warning", i18n = i18n, ns = ns)
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
          shinyjs::show("push_git_readme_modal")
        }
       
        # Reload markdown
        output_file <- create_rmarkdown_file(r, input$readme_code, interpret_code = FALSE)
        output$readme_ui <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(output_file))))
      })
      
      ## Close git push modal
      observeEvent(input$close_push_git_readme_modal, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$close_push_git_readme_modal"))
        shinyjs::hide("push_git_readme_modal")
      })
      
      ## Confirm readme update on remote git
      observeEvent(input$confirm_push_git_readme_update, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$confirm_push_git_readme_update"))
        
        tryCatch({
          git_repo <- r[[paste0(input$current_page, "_git_repo")]]
          repo <- r$loaded_git_repos_objects[[git_repo$unique_id]]
          
          username <- r$users %>% dplyr::filter(id == r$user_id) %>% dplyr::pull(name)
          git2r::config(repo, user.name = username)
          git2r::add(repo, ".")
          
          if (length(git2r::status(repo, unstaged = FALSE, untracked = FALSE, ignored = FALSE)$staged) > 0){

            commit_message <- input$push_git_readme_commit_message
            if (commit_message == "") commit_message <- paste0("Update README from LinkR")
            git2r::commit(repo, message = commit_message)

            # Create main branch if doesn't exist
            git_branches <- names(git2r::branches(repo))
            if (!"main" %in% git_branches){
              git2r::branch_create(commit = git2r::last_commit(repo), name = "main")
              git2r::checkout(repo, "main")
            }

            credentials <- git2r::cred_user_pass("linkr_user", input$push_git_readme_api_key)

            git2r::push(repo, "origin", "refs/heads/main", credentials = credentials)
            
            # Reset commit message (we keep API key, easier when we push files frequently)
            shiny.fluent::updateTextField.shinyInput(session, "push_git_readme_commit_message", value = "")

            # Notify user
            show_message_bar(output, "success_update_remote_git_repo", "success", i18n = i18n, ns = ns)
          }
        }, error = function(e){
          show_message_bar(output, message = "error_update_remote_git_repo", type = "severeWarning", i18n = i18n, ns = ns)
          cat(paste0("\n", now(), " - mod_git_repos - update remot git error - ", toString(e)))
        })
        
        # Close git push modal
        shinyjs::hide("push_git_readme_modal")
      })
      
      ## Cancel readme updates ----
      
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
        
        output_file <- create_rmarkdown_file(r, code, interpret_code = FALSE)
        output$readme_ui <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(output_file))))
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
        
         git_repo <- git_repos %>% dplyr::filter(
            sprintf("%.6f", as.numeric(lat)) == sprintf("%.6f", as.numeric(input$explore_map_marker_click$lat)),
            sprintf("%.6f", as.numeric(lng)) == sprintf("%.6f", as.numeric(input$explore_map_marker_click$lng))
          )
         
          shiny.fluent::updateTextField.shinyInput(session, "push_git_readme_api_key", value = "")
          
          r$map_git_repo <- git_repo
          
          readme <- get_git_readme(r, git_repo, type = "map")
          output$map_git_readme <- renderUI(readme)
          output$summary_git_readme <- renderUI(readme)
          
          # Get git repo informations
          
          if (nrow(git_repo) > 0){
            
            output$map_git_infos_title <- renderUI(tags$h1(git_repo$name))
            output$map_git_infos_content <- renderUI(get_git_infos(git_repo, "map"))
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
      
      # A repo is selected ----
      
      # Selected from DT
      observeEvent(input$show_content_list, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$show_content_list"))
        
        r$git_repo <- r$list_git_repo
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_content_trigger', Math.random());"))
      })
      
      # Selected from the map
      observeEvent(input$show_content_map, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$show_content_map"))
        
        r$git_repo <- r$map_git_repo
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-show_content_trigger', Math.random());"))
      })
      
      # Sidenav reload git button
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
            show_message_bar(output, "error_loading_git_repo", "warning", i18n = i18n, ns = ns)
            cat(paste0("\n", now(), " - mod_git_repos - error downloading git repo - error = ", toString(e)))
          }
        )
        
        if (nrow(loaded_git_repo) > 0) local_path <- loaded_git_repo$local_path
        else local_path <- ""
        
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-git_repo_local_path', '", local_path, "');"))
        
        # Return to summary tab (in case we have clicked on reload_git_repo)
        shinyjs::runjs(paste0("
          Shiny.setInputValue('", id, "-current_tab', '", id, "-summary');
          Shiny.setInputValue('", id, "-current_tab_trigger', Math.random());"
        ))
      })
      
      # Repo current tab ----
      
      observeEvent(input$current_tab_trigger, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$current_tab_trigger"))
        
        # Hide element details
        shinyjs::hide("element_details_div")
        
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
        
        # Update breadcrumb
        output$breadcrumb <- renderUI(create_breadcrumb(c(1, 2, 3), i18n, current_tab))
        
        # Reload widgets UI
        shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_elements_list', Math.random());"))
      })
      
      # Reload widgets ----
      
      observeEvent(input$reload_elements_list, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$reload_elements_list"))
        
        current_tab <- gsub(paste0(id, "-"), "", input$current_tab, fixed = FALSE)
        req(current_tab != "summary")
        
        single_id <- switch(current_tab, 
          "data_cleaning" = "data_cleaning", 
          "datasets" = "dataset",
          "projects" = "project", 
          "plugins" = "plugin", 
          "subsets" = "subset"
        )
        
        elements <- tibble::tibble()
        r$loaded_git_repo_elements <- elements
        
        # Get elements from XML file
        
        xml_file_path <- paste0(input$git_repo_local_path, "/", current_tab, "/", current_tab, ".xml")
        
        error_loading_xml_file <- TRUE
        
        tryCatch({
          elements <-
            xml2::read_xml(xml_file_path) %>%
            XML::xmlParse() %>%
            XML::xmlToDataFrame(nodes = XML::getNodeSet(., paste0("//", single_id)), stringsAsFactors = FALSE) %>%
            tibble::as_tibble()
          
          r$loaded_git_repo_elements <- elements
          
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
            
            if (paste0("name_", language) %in% colnames(row)) element_name <- row[[paste0("name_", language)]]
            else element_name <- row$name_en
            
            users_ui <- create_authors_ui(row$author)
            
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
            
            short_description <- row[[paste0("short_description_", language)]]
            
            elements_ui <- tagList(
              create_element_ui(page_id = id, single_id, element_name, users_ui, widget_buttons, onclick, short_description),
              elements_ui
            )
          }
        }

        elements_ui <- div(elements_ui, class = paste0(current_tab, "_container"))
        output$elements <- renderUI(elements_ui)
      })
      
      # Return to all repos page ----
      
      observeEvent(input$show_home, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$show_home"))
        
        sapply(c("all_repos_reduced_sidenav", "all_git_repos"), shinyjs::show)
        sapply(c("one_repo_reduced_sidenav", "one_git_repo"), shinyjs::hide)
        
        # Prevent a bug with leaflet map display
        shinyjs::runjs("var event = new Event('resize'); window.dispatchEvent(event);")
      })
      
      # An element is selected ----
      
      observeEvent(input$selected_element_trigger, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$selected_element_trigger"))
        
        git_element <- r$loaded_git_repo_elements %>% dplyr::filter(unique_id == input$selected_element)
        
        current_tab <- gsub(paste0(id, "-"), "", input$current_tab, fixed = FALSE)
        current_tab_single <- switch(
          current_tab, 
          "data_cleaning" = "data_cleaning", 
          "datasets" = "dataset",
          "projects" = "study", 
          "plugins" = "plugin"
        )
        
        if (current_tab == "data_cleaning") sql_table <- "scripts"
        else if (current_tab == "projects") sql_table <- "studies"
        else sql_table <- current_tab
        
        # Update breadcrumb
        if (paste0("name_", language) %in% colnames(git_element)) git_element_name <- git_element[[paste0("name_", language)]]
        else git_element_name <- git_element$name_en
        
        output$breadcrumb <- renderUI(create_breadcrumb(c(1, 2, 3, 4), i18n, current_tab, git_element_name))
        
        # Show element description
        if (paste0("description_", language) %in% colnames(git_element)) description <-
          git_element %>% dplyr::select(!!rlang::sym(paste0("description_", language))) %>% dplyr::pull()
        else description <- git_element$description_en
        
        description <- description %>% includeMarkdown()
        output$element_details_description <- renderUI(div(description))
        
        # Update installation UI
        output$element_details_title <- renderUI(tags$h1(i18n$t(paste0("install_", current_tab_single))))
        
        ## Does the element is already installed locally?
        
        sql <- glue::glue_sql(paste0(
          "SELECT * FROM {sql_table} WHERE id = (",
            "SELECT link_id FROM options WHERE category = {current_tab_single} AND name = 'unique_id' AND value = {input$selected_element}",
          ")"), .con = r$db)
        local_element <- DBI::dbGetQuery(r$db, sql)
        
        if (nrow(local_element) > 0){
          
          datetimes_comparison <- compare_git_elements_datetimes("pull", i18n, local_element, git_element)
          diff_time <- datetimes_comparison[[1]]
          diff_time_text <- datetimes_comparison[[2]]
          
          git_element_ui <- div(
            tags$table(
              tags$tr(tags$td(strong(i18n$t("name")), style = "min-width: 80px;"), tags$td(git_element_name)),
              tags$tr(tags$td(strong(i18n$t("author_s"))), tags$td(git_element$author)),
              tags$tr(tags$td(strong(i18n$t("created_on"))), tags$td(git_element$creation_datetime)),
              tags$tr(
                tags$td(strong(i18n$t("updated_on"))),
                tags$td(git_element$update_datetime, " -", diff_time_text)
              )
            )
          )
          
          # Update synchronize buttons
          if (diff_time == 0) install_button <- shiny.fluent::DefaultButton.shinyInput(ns("git_repo_element_already_installed"), i18n$t("local_version_up_to_date"), disabled = TRUE)
          else if (diff_time > 0) install_button <- shiny.fluent::DefaultButton.shinyInput(ns("git_repo_element_local_version_more_recent"), i18n$t("element_local_version_more_recent"), disabled = TRUE)
          else install_button <- shiny.fluent::PrimaryButton.shinyInput(ns("git_install_element"), i18n$t("update"))
          
          synchronize_git_buttons <- div(
            install_button,
            style = "display: flex; gap: 5px;"
          )
        }
        else {
          git_element_ui <- div(
            shiny.fluent::MessageBar(i18n$t(paste0(current_tab_single, "_doesnt_exist_locally")), messageBarType = 5), 
            style = "display: inline-block;"
          )
          
          # Update synchronize buttons
          synchronize_git_buttons <- shiny.fluent::PrimaryButton.shinyInput(ns("git_install_element"), i18n$t("install"))
        }
        
        output$element_details_ui <- renderUI(git_element_ui)
        output$synchronize_git_buttons <- renderUI(synchronize_git_buttons)
        
        shinyjs::hide("widgets_div")
        shinyjs::show("element_details_div")
      })
      
      # Edit description ----
      
      observeEvent(input$edit_description, {
        if (debug) cat(paste0("\n", now(), " - mod_git_repos - observer input$edit_description"))
        
        sapply(c("edit_description_button", "summary_informations_div"), shinyjs::hide)
        sapply(c("save_and_cancel_description_buttons", "edit_description_div"), shinyjs::show)
      })
      
      # Install an element ----
      
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
          "data_cleaning" = "data_cleaning", 
          "datasets" = "dataset",
          "projects" = "project", 
          "plugins" = "plugin"
        )
        
        sql_table <- switch(
          current_tab, 
          "data_cleaning" = "scripts", 
          "datasets" = "datasets",
          "projects" = "studies", 
          "plugins" = "plugins"
        )
        
        sql_category <- switch(
          current_tab, 
          "data_cleaning" = "data_cleaning",
          "datasets" = "dataset",
          "projects" = "study",
          "plugins" = "plugin"
        )
        
        tryCatch({
          
          git_element <- r$loaded_git_repo_elements %>% dplyr::filter(unique_id == input$selected_element)
          
          # Delete local element files
          local_element_folder <- paste0(r$app_folder, "/", current_tab, "/", input$selected_element)
          unlink(local_element_folder, recursive = TRUE)
          
          # Copy files
          git_folder <- r$loaded_git_repos %>% dplyr::filter(unique_id == r$git_repo$unique_id) %>% dplyr::pull(local_path)
          git_category_folder <- paste0(git_folder, "/", current_tab)
          
          ## Get element folder
          file_names <- list.files(path = git_category_folder)
          for (file_name in file_names) if (grepl(input$selected_element, file_name)) git_element_folder <- paste0(git_category_folder, "/", file_name)
          
          ## Copy files in local folder
          
          if (!dir.exists(local_element_folder)) dir.create(local_element_folder)
          files_to_copy <- list.files(git_element_folder, full.names = TRUE)
          file.copy(files_to_copy, local_element_folder, overwrite = TRUE)
          
          if (current_tab == "projects"){
            
            # Copy app_db folder
            app_db_folder <- paste0(local_element_folder, "/app_db")
            if (!dir.exists(app_db_folder)) dir.create(app_db_folder)
            files_to_copy <- list.files(paste0(git_element_folder, "/app_db"), full.names = TRUE)
            file.copy(files_to_copy, app_db_folder, overwrite = TRUE)
          }
          
          # Delete local element from db

          sql <- glue::glue_sql(paste0(
            "SELECT * FROM {sql_table} WHERE id = (",
            "SELECT link_id FROM options WHERE category = {sql_category} AND name = 'unique_id' AND value = {input$selected_element}",
            ")"), .con = r$db)
          local_element <- DBI::dbGetQuery(r$db, sql)

          # If element exists, keep current id
          if (nrow(local_element) > 0){

            element_id <- local_element$id

            ## Delete rows in element table
            sql <- glue::glue_sql("DELETE FROM {sql_table} WHERE id = {local_element$id}", .con = r$db)
            sql_send_statement(r$db, sql)

            ## Delete rows in options table
            sql <- glue::glue_sql("DELETE FROM options WHERE category = {sql_category} AND link_id = {local_element$id}", .con = r$db)
            sql_send_statement(r$db, sql)

            ## Delete rows in code table
            sql <- glue::glue_sql("DELETE FROM code WHERE category = {sql_category} AND link_id = {local_element$id}", .con = r$db)
            sql_send_statement(r$db, sql)
            
            ## Delete project
            if (current_tab == "projects") delete_project(r, m, local_element$id)
          }

          # If element doesn't exist, create a new id
          if (nrow(local_element) == 0) element_id <- get_last_row(r$db, sql_table) + 1

          # Add local element db rows

          ## Add rows in element table

          element_name <- git_element$name_en
          if (paste0("name_", language) %in% colnames(git_element)) element_name <- git_element[[paste0("name_", language)]]

          if (sql_table == "datasets") new_data <- tibble::tibble(
            id = element_id, name = element_name, data_source_id = NA_integer_, creator_id = r$user_id,
            creation_datetime = git_element$creation_datetime, update_datetime = git_element$update_datetime, deleted = FALSE)

          else if (sql_table == "plugins") new_data <- tibble::tibble(
            id = element_id, name = element_name, tab_type_id = git_element$type,
            creation_datetime = git_element$creation_datetime, update_datetime = git_element$update_datetime, deleted = FALSE)

          else if (sql_table == "scripts") new_data <- tibble::tibble(
            id = element_id, name = element_name,
            creation_datetime = git_element$creation_datetime, update_datetime = git_element$update_datetime, deleted = FALSE)

          else if (sql_table == "studies") new_data <- tibble::tibble(
            id = element_id, name = element_name, dataset_id = NA_integer_, patient_lvl_tab_group_id = NA_integer_, aggregated_tab_group_id = NA_integer_,
            creator_id = r$user_id, creation_datetime = git_element$creation_datetime, update_datetime = git_element$update_datetime, deleted = FALSE)

          DBI::dbAppendTable(r$db, sql_table, new_data)

          ## Add rows in options table

          new_options <- tibble::tribble(
            ~name, ~value, ~value_num,
            "users_allowed_read_group", "everybody", 1,
            "user_allowed_read", "", r$user_id,
            "version", git_element$version, NA_integer_,
            "unique_id", git_element$unique_id, NA_integer_,
            "author", git_element$author, NA_integer_,
            "downloaded_from", element_name, NA_integer_,
            "downloaded_from_url", r$git_repo$repo_url_address, NA_integer_
          )

          for (language in r$languages$code){
            for (col in c("short_description", "description", "category", "name")){
              colname <- paste0(col, "_", language)
              if (colname %in% colnames(git_element)) value <- git_element[[colname]]
              else value <- ""
              new_options <- new_options %>% dplyr::bind_rows(tibble::tibble(name = colname, value = value, value_num = NA_integer_))
            }
          }

          if (current_tab == "datasets") new_options <- new_options %>% dplyr::bind_rows(tibble::tibble(name = "omop_version", value = "5.3", value_num = NA_real_))

          new_options <-
            new_options %>%
            dplyr::mutate(id = get_last_row(r$db, "options") + dplyr::row_number(), category = sql_category, link_id = element_id, .before = "name") %>%
            dplyr::mutate(creator_id = r$user_id, datetime = now(), deleted = FALSE)

          DBI::dbAppendTable(r$db, "options", new_options)

          ## Add rows in code table

          if (current_tab %in% c("datasets", "data_cleaning")){

            element_code <- readLines(paste0(local_element_folder, "/code.R"), warn = FALSE) %>% paste(collapse = "\n")

            new_code <- tibble::tibble(
              id = get_last_row(r$db, "code") + 1, category = sql_category, link_id = element_id, code = element_code,
              creator_id = r$user_id, datetime = now(), deleted = FALSE)

            DBI::dbAppendTable(r$db, "code", new_code)
          }

          ## Import project
          if (current_tab == "projects"){
            
            csv_folder <- paste0(local_element_folder, "/app_db")
            update_plugins <- TRUE
            if (length(input$import_project_plugins) > 0) update_plugins <- input$import_project_plugins
            print(update_plugins)
            import_project(r, m, csv_folder, update_plugins, element_id)
          }

          # Update selected element UI
          shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-selected_element_trigger', Math.random());"))
        },
        error = function(e){
          show_message_bar(output, paste0("error_install_remote_git_", current_tab_single), "warning", i18n = i18n, ns = ns)
          cat(paste0("\n", now(), " - mod_git_repos - error installing ", current_tab_single, " from git - error = ", toString(e)))
        })
      })
      
      # Return to selected repo page ----
      
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
    
    get_git_infos <- function(git_repo, git_source){
      
      tagList(
        div(
          strong(i18n$t("url_address")), tags$span(":", style = "margin: 0 5px;"),
          shiny.fluent::Link(href = git_repo$repo_url_address, git_repo$repo_url_address, target = "_blank"),
          style = "display: flex;"
        ),
        div(
          id = ns(paste0("show_content_", git_source, "_div")),
          shiny.fluent::PrimaryButton.shinyInput(ns(paste0("show_content_", git_source)), i18n$t("show_content")),
          class = "git_repo_infos_modal_buttons",
          style = "margin-left: 10px;"
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
          onClick = htmlwidgets::JS(paste0(
            "item => { ",
              "Shiny.setInputValue('", id, "-current_tab_trigger', Math.random()); ",
              "Shiny.setInputValue('", id, "-current_tab', 'summary'); ",
            "}")
          )
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