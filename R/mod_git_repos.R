#' @noRd 
mod_git_repos_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  
  # cards <- c("git_add_repo_card", "git_repos_management_card", "git_repo_options_card", "git_edit_repo_card")
  # 
  # forbidden_cards <- tagList()
  # for (card in cards) forbidden_cards <- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  # 
  # # --- --- --- -- -
  # # Pivot items ----
  # # --- --- --- -- -
  # 
  # div(class = "main",
  #   render_settings_default_elements(ns = ns),
  #   shiny.fluent::reactOutput(ns("help_panel")),
  #   shiny.fluent::reactOutput(ns("help_modal")),
  #   shiny.fluent::reactOutput(ns("git_repos_delete_confirm")),
  #   shiny.fluent::reactOutput(ns("edit_repo_delete_confirm")),
  #   shiny.fluent::Breadcrumb(items = list(
  #     list(key = "remote_git_repos", text = i18n$t("remote_git_repos"))
  #   ), maxDisplayedItems = 3),
  #   shiny.fluent::Pivot(
  #     id = ns("git_repos_pivot"),
  #     onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
  #     shiny.fluent::PivotItem(id = "git_add_repo_card", itemKey = "git_add_repo_card", headerText = i18n$t("add_git_repo")),
  #     shiny.fluent::PivotItem(id = "git_repos_management_card", itemKey = "git_repos_management_card", headerText = i18n$t("git_repos_management")),
  #     shiny.fluent::PivotItem(id = "git_repo_options_card", itemKey = "git_repo_options_card", headerText = i18n$t("git_repo_options")),
  #     shiny.fluent::PivotItem(id = "git_edit_repo_card", itemKey = "git_edit_repo_card", headerText = i18n$t("edit_git_repo"))
  #   ),
  #   forbidden_cards,
  #   
  #   # --- --- --- --- --
  #   # Add repo card ----
  #   # --- --- --- --- --
  #   
  #   shinyjs::hidden(
  #     div(id = ns("git_add_repo_card"),
  #       make_card(i18n$t("add_git_repo"), div(
  #         shiny.fluent::Pivot(
  #           id = ns("git_add_repo_pivot"),
  #           onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-add_repo_tab', item.props.id)")),
  #           shiny.fluent::PivotItem(id = "git_add_repo_map", itemKey = "git_add_repo_map", headerText = i18n$t("with_map")),
  #           shiny.fluent::PivotItem(id = "git_add_repo_url", itemKey = "git_add_repo_url", headerText = i18n$t("with_url")),
  #         ),
  #         div(
  #           id = ns("git_add_repo_map_div"),
  #           leaflet::leafletOutput(ns("git_repos_map"), height = 500),
  #           shinyjs::hidden(
  #             div(
  #               id = ns("git_add_repo_map_details_div"),
  #               shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), br(),
  #                 div(
  #                   id = ns("api_key_git_repo_with_map_div"),
  #                   shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
  #                     div(strong(i18n$t("api_key")), style = "margin-top:5px;"),
  #                     div(shiny.fluent::TextField.shinyInput(ns("api_key_git_repo_with_map")), style = "width:600px;")
  #                   ),
  #                   style = "margin-top:15px; margin-left:0px;"
  #                 ),
  #                 div(shiny.fluent::PrimaryButton.shinyInput(ns("add_git_repo_with_map"), i18n$t("add")), style = "margin-top:15px;")
  #               ), br(),
  #               div(
  #                 uiOutput(ns("render_git_repo_description_with_map")),
  #                 style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px; padding-top: 10px;"
  #               )
  #             )
  #           )
  #         ),
  #         shinyjs::hidden(
  #           div(
  #             id = ns("git_add_repo_url_div"),
  #             make_textfield(i18n = i18n, ns = ns, label = "name", id = "name", width = "300px"),
  #             make_textfield(i18n = i18n, ns = ns, label = "repo_url_address", id = "repo_url_address", width = "600px"),
  #             make_textfield(i18n = i18n, ns = ns, label = "raw_files_url_address", id = "raw_files_url_address", width = "600px"),
  #             make_textfield(i18n = i18n, ns = ns, label = "api_key", id = "api_key_git_repo_with_url", width = "600px"), br(),
  #             shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
  #               shiny.fluent::PrimaryButton.shinyInput(ns("add_git_repo_with_url"), i18n$t("add")),
  #               shiny.fluent::DefaultButton.shinyInput(ns("show_git_repo_description"), i18n$t("show_description"))
  #             ), br(),
  #             div(
  #               uiOutput(ns("render_git_repo_description_with_url")),
  #               style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px; padding-top: 10px;"
  #             )
  #           )
  #         )
  #       ))
  #     )
  #   ),
  #   
  #   # --- --- --- --- --- --- --
  #   # Repos management card ----
  #   # --- --- --- --- --- --- --
  #   
  #   shinyjs::hidden(
  #     div(id = ns("git_repos_management_card"),
  #       make_card(i18n$t("git_repos_management"),
  #         div(
  #           div(DT::DTOutput(ns("git_repos_datatable")), style = "margin-top:-30px; z-index:2"),
  #           div(
  #             shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
  #               shiny.fluent::PrimaryButton.shinyInput(ns("save_git_repos_management"), i18n$t("save")),
  #               shiny.fluent::DefaultButton.shinyInput(ns("delete_selection"), i18n$t("delete_selection"))
  #             ),
  #             style = "position:relative; z-index:2; margin-top:-30px;"
  #           )
  #         )
  #       ), br()
  #     )
  #   ),
  #   
  #   # --- --- --- --- --- --
  #   # Repo options card ----
  #   # --- --- --- --- --- --
  #   
  #   shinyjs::hidden(
  #     div(id = ns("git_repo_options_card"),
  #       make_card(i18n$t("git_repo_options"),
  #         div(
  #           make_combobox(i18n = i18n, ns = ns, label = "git_repo", id = "options_selected_repo", width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
  #           make_textfield(i18n = i18n, ns = ns, label = "repo_url_address", id = "options_repo_url_address", width = "600px"),
  #           make_textfield(i18n = i18n, ns = ns, label = "raw_files_url_address", id = "options_raw_files_url_address", width = "600px"),
  #           make_textfield(i18n = i18n, ns = ns, label = "api_key", id = "options_api_key", width = "600px"), 
  #           div(
  #             div(class = "input_title", paste0(i18n$t("grant_access_to"), " :")),
  #             shiny.fluent::ChoiceGroup.shinyInput(ns("users_allowed_read_group"), options = list(
  #               list(key = "everybody", text = i18n$t("everybody_who_has_access_to_dataset")),
  #               list(key = "people_picker", text = i18n$t("choose_users"))
  #             ), className = "inline_choicegroup"),
  #             shinyjs::hidden(uiOutput(ns("users_allowed_read_div")))
  #           ), br(),
  #           shiny.fluent::PrimaryButton.shinyInput(ns("save_git_repo_options"), i18n$t("save")),
  #         )
  #       ), br()
  #     )
  #   ),
  #   
  #   # --- --- --- --- ---
  #   # Edit repo card ----
  #   # --- --- --- --- ---
  #   
  #   shinyjs::hidden(
  #     div(id = ns("git_edit_repo_card"),
  #       make_shiny_ace_card(i18n$t("edit_git_repo"),
  #         div(
  #           shiny.fluent::Pivot(
  #             id = ns("git_edit_repo_pivot"),
  #             onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-git_edit_repo_current_tab', item.props.id)")),
  #             shiny.fluent::PivotItem(id = "git_edit_repo_files", itemKey = "git_edit_repo_files", headerText = i18n$t("files")),
  #             shiny.fluent::PivotItem(id = "git_edit_repo_readme", itemKey = "git_edit_repo_readme", headerText = i18n$t("readme"))
  #           ),
  #           shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
  #             make_combobox(i18n = i18n, ns = ns, label = "git_repo", id = "edit_repo_selected_repo", width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
  #             make_textfield(i18n = i18n, ns = ns, label = "api_key", id = "edit_repo_api_key", width = "300px"),
  #             div(shiny.fluent::PrimaryButton.shinyInput(ns("edit_repo_load_repo"), i18n$t("load")), style = "margin-top:39px")
  #           ),
  #           div(
  #             id = ns("git_edit_repo_files_div"), br(),
  #             shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
  #               make_dropdown(i18n = i18n, ns = ns, label = "category", id = "repo_category",
  #                 options = list(
  #                   list(key = "studies", text = i18n$t("studies")),
  #                   list(key = "patient_lvl_plugins", text = i18n$t("patient_lvl_plugins")),
  #                   list(key = "aggregated_plugins", text = i18n$t("aggregated_plugins")),
  #                   list(key = "scripts", text = i18n$t("scripts")),
  #                   list(key = "datasets", text = i18n$t("datasets")),
  #                   list(key = "vocabularies", text = i18n$t("vocabularies"))
  #                 ), value = "studies", width = "300px"),
  #               shinyjs::hidden(
  #                 div(
  #                   id = ns("edit_repo_studies_dataset_div"),
  #                   make_dropdown(i18n = i18n, ns = ns, label = "studies_dataset", id = "edit_repo_studies_dataset", width = "300px")
  #                 )
  #               ),
  #               make_dropdown(i18n = i18n, ns = ns, label = "add_files", id = "edit_repo_add_selected_files", width = "300px", multiSelect = TRUE),
  #               div(shiny.fluent::DefaultButton.shinyInput(ns("edit_repo_add_files"), i18n$t("add")), style = "margin-top:39px")
  #             ), br(),
  #             uiOutput(ns("edit_repo_error_message")),
  #             DT::DTOutput(ns("edit_repo_files")),
  #             shinyjs::hidden(div(
  #               id = ns("edit_repo_repo_files_div"),
  #               div(
  #                 shiny.fluent::DefaultButton.shinyInput(ns("edit_repo_delete_selection"), i18n$t("delete_selection")),
  #                 style = "margin-top:-30px;"
  #               )
  #             ))
  #           ),
  #           shinyjs::hidden(
  #             div(
  #               id = ns("git_edit_repo_readme_div"),
  #               br(),
  #               div(
  #                 div("README", style = "font-weight:bold; margin-top:7px; margin-right:5px;"),
  #                 shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
  #                   div(shiny.fluent::Toggle.shinyInput(ns("hide_readme_editor"), value = FALSE), style = "margin-top:9px;"),
  #                   div(i18n$t("hide_editor"), style = "font-weight:bold; margin-top:9px; margin-right:30px;")
  #                 ),
  #                 shinyAce::aceEditor(ns("git_repo_readme"), "", mode = "markdown", 
  #                   code_hotkeys = list(
  #                     "markdown", 
  #                     list(
  #                       save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
  #                       run_all = list(win = "CTRL-SHIFT-ENTER|CTRL-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER|CTRL-ENTER|CMD-ENTER"),
  #                       comment = list(win = "CTRL-SHIFT-C", mac = "CTRL-SHIFT-C|CMD-SHIFT-C")
  #                     )
  #                   ),
  #                   autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000),
  #                 style = "width: 100%;"),
  #               shinyjs::hidden(div(id = ns("br_div"), br())),
  #               shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
  #                 shiny.fluent::PrimaryButton.shinyInput(ns("git_repo_readme_save"), i18n$t("save")), " ",
  #                 shiny.fluent::DefaultButton.shinyInput(ns("git_repo_readme_preview"), i18n$t("preview")), " ",
  #                 shiny.fluent::DefaultButton.shinyInput(ns("git_repo_readme_generate"), i18n$t("generate_content"))
  #               ), br(),
  #               div(id = ns("description_markdown_output"),
  #                 uiOutput(ns("description_markdown_result")), 
  #                 style = "width: 99%; border-style: dashed; border-width: 1px; padding:0px 8px 0px 8px; margin-right: 5px;")
  #             )
  #           ), br(),
  #           shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
  #             make_textfield(i18n = i18n, ns = ns, label = "commit_message", id = "commit_message", width = "620px"),
  #             div(shiny.fluent::PrimaryButton.shinyInput(ns("commit_and_push"), i18n$t("commit_and_push")), style = "margin-top:39px")
  #           )
  #         )
  #       ), br()
  #     )
  #   ), br()
  # )
  div()
}

#' settings_r_console Server Functions
#'
#' @noRd 

mod_git_repos_server <- function(id, r, d, m, language, i18n, debug){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # # --- --- --- --- --- ---
    # # Show or hide cards ----
    # # --- --- --- --- --- ---
    # 
    # cards <- c("git_add_repo_card", "git_repos_management_card", "git_repo_options_card", "git_edit_repo_card")
    # show_or_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    # if ("git_add_repo_card" %in% r$user_accesses) shinyjs::show("git_add_repo_card")
    # else shinyjs::show("git_add_repo_card_forbidden")
    # 
    # sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    # 
    # # --- --- --- --- --- ---
    # # Help for this page ----
    # # --- --- --- --- --- ---
    # 
    # observeEvent(input$help, if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r$help_settings_git_open_panel <- TRUE)
    # observeEvent(input$hide_panel, r$help_settings_git_open_panel <- FALSE)
    # 
    # r$help_settings_git_open_panel_light_dismiss <- TRUE
    # observeEvent(input$show_modal, r$help_settings_git_open_modal <- TRUE)
    # observeEvent(input$hide_modal, {
    #   r$help_settings_git_open_modal <- FALSE
    #   r$help_settings_git_open_panel_light_dismiss <- TRUE
    # })
    # 
    # sapply(1:10, function(i){
    #   observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_settings_git_page_", i)]] <- now())
    # })
    # 
    # help_settings_git(output = output, r = r, id = id, language = language, i18n = i18n, ns = ns)
    # 
    # # --- --- --- --- --- --
    # # Create a git repo ----
    # # --- --- --- --- --- --
    # 
    # # Show / hide divs
    # observeEvent(input$add_repo_tab, {
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$add_repo_tab"))
    #   
    #   if (input$add_repo_tab == "git_add_repo_map"){
    #     shinyjs::show("git_add_repo_map_div")
    #     shinyjs::hide("git_add_repo_url_div")
    #   }
    #   else {
    #     shinyjs::hide("git_add_repo_map_div")
    #     shinyjs::show("git_add_repo_url_div")
    #   }
    # })
    # 
    # # Add with map
    # 
    # if (r$has_internet){
    #   
    #   ## Download git repos from InterHop git
    #   
    #   git_repos <- tibble::tibble()
    #   
    #   filename <- "https://framagit.org/interhop/linkr/LinkR-content/-/raw/main/git_repos/git_repos.csv"
    #   filename_local <- paste0(r$app_folder, "/temp_files/", r$user_id, "/git_repos/git_repos.csv")
    #   
    #   # Get csv file from remote git
    #   tryCatch(download.file(filename, filename_local, quiet = TRUE),
    #     error = function(e) report_bug(r = r, output = output, error_message = "error_downloading_git_repos_csv",
    #       error_name = "settings_git download git_repos.csv", category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
    #   
    #   tryCatch(git_repos <- vroom::vroom(filename_local, col_types = "cnnccccl", progress = FALSE),
    #     error = function(e) report_bug(r = r, output = output, error_message = "error_loading_git_repos_csv",
    #       error_name = "settings_git load git_repos.csv", category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
    #   
    #   new_cols <- outer("name", r$languages$code, paste, sep = "_") %>% as.vector()
    #   for(col in new_cols) if(!col %in% colnames(git_repos)) git_repos <- git_repos %>% dplyr::mutate(!!col := "")
    #   
    #   git_repos <- git_repos %>% dplyr::mutate(name = ifelse(!is.na(get(paste0("name_", language))), get(paste0("name_", language)), name_en))
    #   
    #   ## Render leaflet map
    #   output$git_repos_map <- leaflet::renderLeaflet({
    #     
    #     if (debug) cat(paste0("\n", now(), " - mod_settings_git - output$git_repos_map"))
    #     
    #     leaflet::leaflet(git_repos) %>%
    #       leaflet::addTiles() %>%
    #       # leaflet::addProviderTiles("Stadia.Outdoors") %>%
    #       leaflet::addProviderTiles("CartoDB.Voyager") %>%
    #       leaflet::addMarkers(
    #         lng = ~lng, lat = ~lat,
    #         clusterOptions = leaflet::markerClusterOptions(),
    #         popup = ~paste("<strong>", name, "</strong>")
    #       )
    #   })
    #   
    #   ## When a repo is selected on the map
    #   observeEvent(input$git_repos_map_marker_click, {
    #     if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$git_repos_map_marker_click"))
    #     
    #     # Show div
    #     shinyjs::show("git_add_repo_map_details_div")
    #     
    #     # Update API key field
    #     shiny.fluent::updateTextField.shinyInput(session = session, "api_key_git_repo_with_map", errorMessage = NULL, value = "")
    #     
    #     git_repo <- git_repos %>% dplyr::filter(
    #       sprintf("%.6f", as.numeric(lat)) == sprintf("%.6f", as.numeric(input$git_repos_map_marker_click$lat)),
    #       sprintf("%.6f", as.numeric(lng)) == sprintf("%.6f", as.numeric(input$git_repos_map_marker_click$lng))
    #     )
    #     
    #     # Disable add button is git repo already in db
    #     if (r$git_repos %>% dplyr::inner_join(git_repo %>% dplyr::select(unique_id), by = "unique_id") %>% nrow() > 0){
    #       shiny.fluent::updateActionButton.shinyInput(session = session, "add_git_repo_with_map", disabled = TRUE)
    #       shinyjs::hide("api_key_git_repo_with_map_div")
    #     }
    #     else {
    #       shiny.fluent::updateActionButton.shinyInput(session = session, "add_git_repo_with_map", disabled = FALSE)
    #       
    #       # If an API key is needed
    #       if (git_repo$api_key_required) shinyjs::show("api_key_git_repo_with_map_div") else shinyjs::hide("api_key_git_repo_with_map_div")
    #     }
    #     
    #     r$show_git_repo_description_trigger <- now()
    #     r$show_git_repo_description_type <- "map"
    #     r$show_git_repo_description_url <- git_repo$raw_files_url_address
    #     
    #     if (substr(r$show_git_repo_description_url, nchar(r$show_git_repo_description_url), 
    #       nchar(r$show_git_repo_description_url)) != "/") r$show_git_repo_description_url <- paste0(r$show_git_repo_description_url, "/")
    #   })
    #   
    #   ## Add with map
    #   observeEvent(input$add_git_repo_with_map, {
    #     if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$add_git_repo_with_map"))
    #     
    #     req(length(input$git_repos_map_marker_click) > 0)
    #     git_repo <- git_repos %>% dplyr::filter(
    #       sprintf("%.6f", as.numeric(lat)) == sprintf("%.6f", as.numeric(input$git_repos_map_marker_click$lat)),
    #       sprintf("%.6f", as.numeric(lng)) == sprintf("%.6f", as.numeric(input$git_repos_map_marker_click$lng))
    #     )
    #     
    #     new_data <- list()
    #     if (paste0("name_", language) %in% colnames(git_repo)) new_data$name <- git_repo[[paste0("name_", language)]]
    #     else new_data$name <- git_repo$name_en
    #     new_data$api_key <- coalesce2(type = "char", x = input$api_key_git_repo_with_map)
    #     new_data$repo_url_address <- git_repo$repo_url_address
    #     new_data$raw_files_url_address <- git_repo$raw_files_url_address
    #     new_data$unique_id <- git_repo$unique_id
    #     
    #     # Check if git repo is not already in database
    #     sql <- glue::glue_sql("SELECT * FROM git_repos WHERE unique_id = {new_data$unique_id} AND deleted IS FALSE", .con = r$db)
    #     check_unique_id <- DBI::dbGetQuery(r$db, sql) %>% nrow() == 0
    #     if (!check_unique_id) show_message_bar(output, "git_repo_already_in_db", "severeWarning", i18n = i18n, ns = ns)
    #     req(check_unique_id)
    #     
    #     # Check if name is not already taken
    #     sql <- glue::glue_sql("SELECT * FROM git_repos WHERE name = {new_data$name} AND deleted IS FALSE", .con = r$db)
    #     check_unique_name <- DBI::dbGetQuery(r$db, sql) %>% nrow() == 0
    #     if (!check_unique_name) show_message_bar(output, "name_already_used", "severeWarning", i18n = i18n, ns = ns)
    #     req(check_unique_name)
    #     
    #     if (git_repo$api_key_required){
    #       if (is.na(input$api_key_git_repo_with_map) | input$api_key_git_repo_with_map == "") shiny.fluent::updateTextField.shinyInput(session, "api_key_git_repo_with_map", errorMessage = i18n$t("provide_valid_api_key"))
    #       else shiny.fluent::updateTextField.shinyInput(session, "api_key_git_repo_with_map", errorMessage = NULL)
    #       req(!is.na(input$api_key_git_repo_with_map) & input$api_key_git_repo_with_map != "")
    #     }
    #     
    #     add_settings_new_data(session = session, output = output, r = r, m = m, i18n = i18n, id = id, data = new_data, table = "git_repos")
    #     
    #     # Update & hide API key field
    #     shiny.fluent::updateTextField.shinyInput(session = session, "api_key_git_repo_with_map", errorMessage = NULL, value = "")
    #     shinyjs::hide("api_key_git_repo_with_map_div")
    #     
    #     # Disable add button
    #     shiny.fluent::updateActionButton.shinyInput(session = session, "add_git_repo_with_map", disabled = TRUE)
    #     
    #     # Reload datatable
    #     r$git_repos_temp <- r$git_repos %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
    #   })
    # }
    # 
    # # Add with URL
    # 
    # ## Show description
    # observeEvent(input$show_git_repo_description, {
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$show_git_repo_description"))
    #   
    #   req(length(input$repo_url_address) > 0)
    #   
    #   r$show_git_repo_description_trigger <- now()
    #   r$show_git_repo_description_type <- "url"
    #   r$show_git_repo_description_url <- input$raw_files_url_address
    #   
    #   if (substr(r$show_git_repo_description_url, nchar(r$show_git_repo_description_url), 
    #     nchar(r$show_git_repo_description_url)) != "/") r$show_git_repo_description_url <- paste0(r$show_git_repo_description_url, "/")
    # })
    # 
    # ## Add git repo
    # observeEvent(input$add_git_repo_with_url, {
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "start")
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$add_git_repo_with_url"))
    #   
    #   new_data <- list()
    #   for (name in c("name", "api_key_git_repo_with_url", "repo_url_address", "raw_files_url_address")) new_data[[name]] <- coalesce2(type = "char", x = input[[name]])
    #   
    #   add_settings_new_data(session = session, output = output, r = r, m = m, i18n = i18n, id = id, 
    #     data = new_data, table = "git_repos", required_textfields = c("name", "repo_url_address", "raw_files_url_address"), req_unique_values = "name")
    #   
    #   # Reload datatable
    #   r$git_repos_temp <- r$git_repos %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
    #   
    #   # Click git repos management card
    #   shinyjs::runjs(glue::glue("$('#settings_git-git_repos_pivot button[name=\"{i18n$t('git_repos_management')}\"]').click();"))
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_git - observer input$add_git_repo_with_url"))
    # })
    # 
    # # Show git repo description (for map & URL)
    # 
    # observeEvent(r$show_git_repo_description_trigger , {
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer r$show_git_repo_description_trigger "))
    #   
    #   readme <- div(shiny.fluent::MessageBar(i18n$t("error_loading_git_readme"), messageBarType = 5), style = "margin-bottom:8px;")
    #   
    #   # Get README on git repo
    #   tryCatch({
    #     dir <- paste0(r$app_folder, "/temp_files/", r$user_id, "/git_repos")
    #     new_file <- paste0(dir, "/", paste0(sample(c(0:9, letters[1:6]), 8, TRUE), collapse = ''), "_README.md")
    #     if (!dir.exists(dir)) dir.create(dir)
    #     
    #     filename_remote <- paste0(r$show_git_repo_description_url, "README.md")
    #     
    #     download.file(filename_remote, new_file, quiet = TRUE)
    #     
    #     con <- textConnection(new_file)
    #     readme <- div(readLines(con, warn = FALSE) %>% includeMarkdown() %>% withMathJax(), style = "margin-top:-15px;")
    #     close(con)
    #   },
    #     error = function(e) add_log_entry(r = r, category = "Error", name = paste0(id, " - load git repo description"), value = toString(e)),
    #     warning = function(w) add_log_entry(r = r, category = "Error", name = paste0(id, " - load git repo description"), value = toString(w)))
    #   
    #   # Render UI
    #   output[[paste0("render_git_repo_description_with_", r$show_git_repo_description_type)]] <- renderUI({
    #     if (debug) cat(paste0("\n", now(), " - mod_settings_git - output$render_git_repo_description_with_map"))
    #     readme
    #   })
    # })
    # 
    # # --- --- --- --- --- --- -
    # # Git repos management ----
    # # --- --- --- --- --- --- -
    # 
    # action_buttons <- c("delete", "options")
    # editable_cols <- c("name", "description")
    # sortable_cols <- c("name", "creator_id", "datetime")
    # column_widths <- c("id" = "80px", "datetime" = "130px", "creator_id" = "200px", "action" = "80px")
    # centered_cols <- c("creator_id", "datetime", "action")
    # searchable_cols <- c("name", "creator_id")
    # factorize_cols <- c("creator_id")
    # hidden_cols <- c("id", "unique_id", "api_key", "repo_url_address", "raw_files_url_address", "deleted", "modified")
    # col_names <- get_col_names("git_repos", i18n)
    # shortened_cols <- c("name" = 50, "creator_id" = 20)
    # 
    # # Prepare data for datatable
    # 
    # observeEvent(r$git_repos, {
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "start")
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer r$git_repos"))
    #   
    #   # Update dropdowns
    #   shiny.fluent::updateComboBox.shinyInput(session, "edit_repo_selected_repo", options = convert_tibble_to_list(r$git_repos, key_col = "id", text_col = "name"))
    #   shiny.fluent::updateComboBox.shinyInput(session, "options_selected_repo", options = convert_tibble_to_list(r$git_repos, key_col = "id", text_col = "name"))
    #   
    #   r$git_repos_temp <- r$git_repos %>% dplyr::mutate(modified = FALSE)
    #   
    #   # Reset fields
    #   
    #   if(nrow(r$git_repos_temp) == 0){
    #     render_datatable(output = output, ns = ns, i18n = i18n,
    #       data = r$git_repos_temp %>% dplyr::mutate(action = character()),
    #       col_names = col_names, output_name = "git_repos_datatable", shortened_cols = shortened_cols,
    #       editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
    #       searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols, selection = "multiple")
    #   }
    #   
    #   req(nrow(r$git_repos_temp) > 0)
    #   
    #   # Render datatable
    #   
    #   r$git_repos_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
    #     table = "scripts", factorize_cols = factorize_cols, action_buttons = action_buttons, data_input = r$git_repos_temp)
    #   
    #   if (length(r$git_repos_datatable_proxy) == 0){
    #     render_datatable(output = output, ns = ns, i18n = i18n, data = r$git_repos_datatable_temp,
    #       output_name = "git_repos_datatable", col_names = col_names, shortened_cols = shortened_cols,
    #       editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
    #       searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols, selection = "multiple")
    #     
    #     # Create a proxy for datatable
    #     
    #     r$git_repos_datatable_proxy <- DT::dataTableProxy("git_repos_datatable", deferUntilFlush = FALSE)
    #   }
    #   
    #   else  DT::replaceData(r$git_repos_datatable_proxy, r$git_repos_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_settings_git - observer r$git_repos"))
    # })
    # 
    # # Updates on datatable data
    # observeEvent(input$git_repos_datatable_cell_edit, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$git_repos_datatable_cell_edit"))
    #   
    #   edit_info <- input$git_repos_datatable_cell_edit
    #   r$git_repos_temp <- DT::editData(r$git_repos_temp, edit_info, rownames = FALSE)
    #   
    #   # Store that this row has been modified
    #   r$git_repos_temp[[edit_info$row, "modified"]] <- TRUE
    # })
    # 
    # # Save updates
    # observeEvent(input$save_git_repos_management, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$save_git_repos_management"))
    #   
    #   req(nrow(r$git_repos) > 0)
    #   
    #   save_settings_datatable_updates(output = output, r = r, ns = ns, table = "git_repos", i18n = i18n, duplicates_allowed = FALSE)
    #   
    #   # Reload datatable
    #   r$git_repos_temp <- r$git_repos_temp %>% dplyr::mutate(modified = FALSE)
    # })
    # 
    # # Delete a row in datatable
    # 
    # git_repos_delete_prefix <- "git_repos"
    # git_repos_dialog_title <- "git_repos_delete"
    # git_repos_dialog_subtext <- "git_repos_delete_subtext"
    # git_repos_react_variable <- "git_repos_delete_confirm"
    # git_repos_table <- "git_repos"
    # git_repos_id_var_sql <- "id"
    # git_repos_id_var_r <- "delete_git_repo"
    # git_repos_delete_message <- "git_repo_deleted"
    # git_repos_reload_variable <- "reload_git_repos"
    # git_repos_information_variable <- "git_repo_deleted"
    # git_repos_delete_variable <- paste0(git_repos_delete_prefix, "_open_dialog")
    # 
    # delete_element(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
    #   delete_prefix = git_repos_delete_prefix, dialog_title = git_repos_dialog_title, dialog_subtext = git_repos_dialog_subtext,
    #   react_variable = git_repos_react_variable, table = git_repos_table, id_var_sql = git_repos_id_var_sql, id_var_r = git_repos_id_var_r,
    #   delete_message = git_repos_delete_message, translation = TRUE, reload_variable = git_repos_reload_variable,
    #   information_variable = git_repos_information_variable)
    # 
    # observeEvent(input$deleted_pressed, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$deleted_pressed"))
    #   
    #   r$delete_git_repo <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
    #   r[[git_repos_delete_variable]] <- TRUE
    #   
    #   # Reload datatable (to unselect rows)
    #   DT::replaceData(r$git_repos_datatable_proxy, r$git_repos_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    # })
    # 
    # observeEvent(input$delete_selection, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$delete_selection"))
    #   
    #   req(length(input$git_repos_datatable_rows_selected) > 0)
    #   
    #   r$delete_git_repo <- r$git_repos[input$git_repos_datatable_rows_selected, ] %>% dplyr::pull(id)
    #   r[[git_repos_delete_variable]] <- TRUE
    # })
    # 
    # # --- --- --- --- --- -
    # # Git repo options ----
    # # --- --- --- --- --- -
    # 
    # # Show / hide people picker input
    # observeEvent(input$users_allowed_read_group, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$users_allowed_read_group"))
    #   
    #   if (input$users_allowed_read_group == "people_picker") shinyjs::show("users_allowed_read_div")
    #   else shinyjs::hide("users_allowed_read_div")
    # })
    # 
    # # When a repo is selected
    # observeEvent(input$options, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$options"))
    #   
    #   # Get link_id variable, to update options div
    #   link_id <- as.integer(substr(input$options, nchar("options_") + 1, nchar(input$options)))
    #   
    #   options <- convert_tibble_to_list(r$git_repos_temp, key_col = "id", text_col = "name")
    #   value <- list(key = link_id, text = r$git_repos_temp %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
    #   
    #   shiny.fluent::updateComboBox.shinyInput(session, "options_selected_repo", options = options, value = value)
    #   
    #   # Reload datatable (to unselect rows)
    #   DT::replaceData(r$git_repos_datatable_proxy, r$git_repos_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    #   
    #   # Set current pivot to edit_plugins_code
    #   shinyjs::runjs(glue::glue("$('#{id}-git_repos_pivot button[name=\"{i18n$t('git_repo_options')}\"]').click();"))
    # })
    # 
    # observeEvent(input$options_selected_repo, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$options_selected_repo"))
    #   
    #   if (length(input$options_selected_repo) > 1) link_id <- input$options_selected_repo$key
    #   else link_id <- input$options_selected_repo
    #   
    #   # Code repo options
    #   
    #   git_repo_options <- r$git_repos_temp %>% dplyr::filter(id == link_id)
    #   
    #   sapply(c("options_repo_url_address", "options_raw_files_url_address", "options_api_key"), function(field) shiny.fluent::updateTextField.shinyInput(session, 
    #     field, value = git_repo_options[[sub("options_", "", field)]]))
    #   
    #   options <- r$options %>% dplyr::filter(category == "git_repo", link_id == !!link_id)
    #   
    #   picker_options <-
    #     r$users %>%
    #     dplyr::left_join(r$users_statuses %>% dplyr::select(user_status_id = id, user_status = name), by = "user_status_id") %>%
    #     dplyr::transmute(
    #       key = id, 
    #       imageInitials = paste0(substr(firstname, 0, 1), substr(lastname, 0, 1)),
    #       text = paste0(firstname, " ", lastname), 
    #       secondaryText = user_status)
    #   
    #   picker_value <-
    #     picker_options %>%
    #     dplyr::mutate(n = 1:dplyr::n()) %>%
    #     dplyr::inner_join(
    #       options %>%
    #         dplyr::filter(name == "user_allowed_read") %>%
    #         dplyr::select(key = value_num),
    #       by = "key"
    #     ) %>%
    #     dplyr::pull(key)
    #   
    #   # Users allowed read group
    #   value_group <- options %>% dplyr::filter(name == "users_allowed_read_group") %>% dplyr::pull(value)
    #   
    #   shiny.fluent::updateChoiceGroup.shinyInput(session, "users_allowed_read_group",
    #     value = options %>% dplyr::filter(name == "users_allowed_read_group") %>% dplyr::pull(value))
    #   output$users_allowed_read_div <- renderUI({
    #     make_people_picker(
    #       i18n = i18n, ns = ns, id = "users_allowed_read", label = "users", options = picker_options, value = picker_value,
    #       width = "100%", style = "padding-bottom:10px;")
    #   })
    # })
    # 
    # # Save updates
    # observeEvent(input$save_git_repo_options, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$save_git_repo_options"))
    #   
    #   if (length(input$options_selected_repo) > 1) link_id <- input$options_selected_repo$key
    #   else link_id <- input$options_selected_repo
    #   
    #   # Check if fields are not empty
    #   empty_field <- list()
    #   fields <- c("repo_url_address", "raw_files_url_address")
    #   for(field in fields){
    #     empty_field[[field]] <- FALSE
    #     if (length(input[[paste0("options_", field)]]) == 0) empty_field[[field]] <- TRUE
    #     else if (is.na(input[[paste0("options_", field)]]) | input[[paste0("options_", field)]] == "") empty_field[[field]] <- TRUE
    #     if (empty_field[[field]]) shiny.fluent::updateTextField.shinyInput(session, paste0("options_", field), errorMessage = i18n$t(paste0("provide_valid_", field)))
    #     else shiny.fluent::updateTextField.shinyInput(session, paste0("options_", field), errorMessage = NULL)
    #   }
    #   
    #   req(!empty_field$repo_url_address, !empty_field$raw_files_url_address)
    #   
    #   data <- list()
    #   data$users_allowed_read <- input$users_allowed_read
    #   data$users_allowed_read_group <- input$users_allowed_read_group
    #   
    #   save_settings_options(output = output, r = r, id = id, category = "git_repo", code_id_input = paste0("options_", link_id),
    #     i18n = i18n, data = data, page_options = "users_allowed_read")
    #   
    #   # Save updates in db
    #   sql <- glue::glue_sql(paste0("UPDATE git_repos SET repo_url_address = {input$options_repo_url_address}, ",
    #     "raw_files_url_address = {input$options_raw_files_url_address}, api_key = {input$options_api_key} WHERE id = {link_id}"), .con = r$db)
    #   query <- DBI::dbSendStatement(r$db, sql)
    #   DBI::dbClearResult(query)
    #   
    #   if (length(input$options_api_key) == 0) new_api_key <- NA_character_ else new_api_key <- input$options_api_key
    #   
    #   r$git_repos <- r$git_repos %>% 
    #     dplyr::mutate(
    #       repo_url_address = dplyr::case_when(id == link_id ~ input$options_repo_url_address, TRUE ~ repo_url_address),
    #       raw_files_url_address = dplyr::case_when(id == link_id ~ input$options_raw_files_url_address, TRUE ~ raw_files_url_address),
    #       api_key = dplyr::case_when(id == link_id ~ new_api_key, TRUE ~ api_key)
    #     )
    # })
    # 
    # # --- --- --- --- --
    # # Edit git repo ----
    # # --- --- --- --- --
    # 
    # # Show / hide divs
    # observeEvent(input$git_edit_repo_current_tab, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$git_edit_repo_current_tab"))
    #   
    #   if (input$git_edit_repo_current_tab == "git_edit_repo_files"){
    #     shinyjs::show("git_edit_repo_files_div")
    #     shinyjs::hide("git_edit_repo_readme_div")
    #   }
    #   else {
    #     shinyjs::hide("git_edit_repo_files_div")
    #     shinyjs::show("git_edit_repo_readme_div")
    #   }
    # })
    # 
    # observeEvent(input$hide_readme_editor, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$hide_readme_editor"))
    #   
    #   if (input$hide_readme_editor){
    #     shinyjs::hide("git_repo_readme")
    #     shinyjs::show("br_div")
    #   } 
    #   else {
    #     shinyjs::show("git_repo_readme")
    #     shinyjs::hide("br_div")
    #   }
    # })
    # 
    # # When a repo is selected
    # observeEvent(input$edit_repo_load_repo, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$edit_repo_load_repo"))
    #   
    #   req(length(input$edit_repo_selected_repo) > 0)
    #   
    #   # Check if username & api_key are not empty
    #   empty_field <- list()
    #   # fields <- c("username", "api_key")
    #   fields <- c("api_key")
    #   for(field in fields){
    #     empty_field[[field]] <- FALSE
    #     if (length(input[[paste0("edit_repo_", field)]]) == 0) empty_field[[field]] <- TRUE
    #     else if (is.na(input[[paste0("edit_repo_", field)]]) | input[[paste0("edit_repo_", field)]] == "") empty_field[[field]] <- TRUE
    #     if (empty_field[[field]]) shiny.fluent::updateTextField.shinyInput(session, paste0("edit_repo_", field), errorMessage = i18n$t(paste0("provide_valid_", field)))
    #     else shiny.fluent::updateTextField.shinyInput(session, paste0("edit_repo_", field), errorMessage = NULL)
    #   }
    #   
    #   # req(!empty_field$username, !empty_field$api_key)
    #   req(!empty_field$api_key)
    #   
    #   # Copy locally git repo
    #   
    #   # Delete old files (> 24 hours)
    #   # git_repos_folder <- paste0(r$app_folder, "/temp_files/", r$user_id, "/git_repos")
    #   # git_repos_dirs <- list.dirs(path = git_repos_folder, full.names = TRUE, recursive = FALSE)
    #   # 
    #   # for (dir in git_repos_dirs) {
    #   #   dir_time <- file.info(dir)$mtime
    #   #   time_diff <- as.numeric(difftime(now(), dir_time, units = "hours"))
    #   #   if (time_diff > 24) unlink(dir, recursive = TRUE, force = TRUE)
    #   # }
    #   
    #   if (length(input$edit_repo_selected_repo) > 1) link_id <- input$edit_repo_selected_repo$key
    #   else link_id <- input$edit_repo_selected_repo
    #   
    #   repo_url <- r$git_repos %>% dplyr::filter(id == link_id) %>% dplyr::pull(repo_url_address)
    #   local_path <- paste0(r$app_folder, "/temp_files/", r$user_id, "/git_repos/", paste0(sample(c(0:9, letters[1:6]), 64, TRUE), collapse = ''))
    #   r$edit_git_local_path <- local_path
    #   
    #   error_loading_git_repo <- TRUE
    #   
    #   tryCatch({
    #     # credentials <- git2r::cred_user_pass(input$edit_repo_username, input$edit_repo_api_key)
    #     credentials <- git2r::cred_user_pass("linkr_user", input$edit_repo_api_key)
    #     r$edit_git_repo_credentials <- credentials
    #     
    #     repo <- git2r::clone(repo_url, local_path, credentials = credentials, progress = FALSE)
    #     r$edit_git_repo_repo <- repo
    #     
    #     # Create dirs & files that don't exist
    #     dirs <- c("datasets", "plugins", "scripts", "studies", "vocabularies")
    #     for (dir in dirs){
    #       dir_path <- paste0(local_path, "/", dir)
    #       file_path <- paste0(local_path, "/", dir, "/.gitkeep")
    #       xml_file_path <- paste0(dir_path, "/", dir, ".xml")
    #       
    #       if (!dir.exists(dir_path)) dir.create(dir_path)
    #       if (!file.exists(file_path)) file.create(file_path)
    #       if (!file.exists(xml_file_path)){
    #         xml <- XML::newXMLDoc()
    #         category_general_node <- XML::newXMLNode(dir, doc = xml)
    #         XML::saveXML(xml, file = xml_file_path) 
    #       }
    #     }
    #     readme_file_path <- paste0(local_path, "/README.md")
    #     if (!file.exists(readme_file_path)) file.create(readme_file_path)
    #     
    #     # Update shinyAce for README
    #     content <- paste(readLines(readme_file_path, warn = FALSE), collapse = "\n")
    #     shinyAce::updateAceEditor(session, "git_repo_readme", value = content)
    #     
    #     # Create .gitignore with .DS_Store in it
    #     writeLines(".DS_Store", paste0(local_path, "/.gitignore"))
    #     
    #     error_loading_git_repo <- FALSE
    #   },
    #     error = function(e) report_bug(r = r, output = output, error_message = "error_downloading_git_repo",
    #       error_name = "settings_git download git repo", category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
    #   
    #   if (error_loading_git_repo){
    #     output$edit_repo_error_message <- renderUI(shiny.fluent::MessageBar(i18n$t("error_connection_remote_git"), messageBarType = 5))
    #     sapply(c("edit_repo_repo_files_div", "edit_repo_files"), shinyjs::hide)
    #   }
    #   else {
    #     output$edit_repo_error_message <- renderUI(tagList())
    #     sapply(c("edit_repo_repo_files_div", "edit_repo_files"), shinyjs::show)
    #     
    #     # Trigger to render datatable
    #     r$edit_repo_datatable_trigger <- now()
    #     
    #     # Trigger to update dropdown of files
    #     r$repo_category_trigger <- now() 
    #   }
    # })
    # 
    # # When a category is selected
    # observeEvent(input$repo_category, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$repo_category"))
    #   r$repo_category_trigger <- now() 
    #   
    #   if (input$repo_category == "studies") shinyjs::show("edit_repo_studies_dataset_div")
    #   else shinyjs::hide("edit_repo_studies_dataset_div")
    # })
    # 
    # observeEvent(r$repo_category_trigger, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer r$repo_category_trigger"))
    #   
    #   req(input$edit_repo_selected_repo)
    #   
    #   # For studies, update studies_dataset dropdown
    #   if (input$repo_category == "studies") shiny.fluent::updateDropdown.shinyInput(session, "edit_repo_studies_dataset", 
    #     options = r$datasets %>% convert_tibble_to_list(key_col = "id", text_col = "name"))
    #   
    #   else {
    #     # Update add_files dropdown
    #     if (input$repo_category == "patient_lvl_plugins") dropdown_options <- r$plugins %>% dplyr::filter(tab_type_id == 1)
    #     else if (input$repo_category == "aggregated_plugins") dropdown_options <- r$plugins %>% dplyr::filter(tab_type_id == 2)
    #     else if (input$repo_category == "vocabularies") dropdown_options <- r$vocabulary
    #     else dropdown_options <- r[[input$repo_category]]
    #     
    #     if (input$repo_category == "vocabularies") dropdown_options <- dropdown_options %>% dplyr::arrange(tolower(vocabulary_name)) %>% convert_tibble_to_list(key_col = "id", text_col = "vocabulary_name")
    #     else dropdown_options <- dropdown_options %>% dplyr::arrange(tolower(name)) %>% convert_tibble_to_list(key_col = "id", text_col = "name")
    #     
    #     shiny.fluent::updateDropdown.shinyInput(session, "edit_repo_add_selected_files", options = dropdown_options)
    #     shiny.fluent::updateDropdown.shinyInput(session, "edit_repo_studies_dataset", options = list(), value = NULL)
    #   }
    #   
    #   # Trigger to render datatable
    #   r$edit_repo_datatable_trigger <- now()
    # })
    # 
    # # When a dataset is selected (for studies)
    # observeEvent(input$edit_repo_studies_dataset, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$edit_repo_studies_dataset"))
    #   
    #   req(input$edit_repo_selected_repo)
    #   
    #   # Update add_files dropdown
    #   sql <- glue::glue_sql("SELECT * FROM studies WHERE dataset_id = {input$edit_repo_studies_dataset} AND deleted IS FALSE", .con = r$db)
    #   r$edit_repo_studies <- DBI::dbGetQuery(r$db, sql)
    #   dropdown_options <- r$edit_repo_studies %>% dplyr::arrange(tolower(name)) %>% convert_tibble_to_list(key_col = "id", text_col = "name")
    #   
    #   shiny.fluent::updateDropdown.shinyInput(session, "edit_repo_add_selected_files", options = dropdown_options)
    #   
    #   # Trigger to render datatable
    #   r$edit_repo_datatable_trigger <- now()
    # })
    # 
    # # Load data for selected category
    # observeEvent(r$edit_repo_datatable_trigger, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer r$edit_repo_datatable_trigger"))
    #   
    #   req(input$edit_repo_selected_repo)
    #   
    #   # Load README.md for this category
    #   
    #   error_loading_repo_category_files <- TRUE
    #   
    #   if (input$repo_category %in% c("patient_lvl_plugins", "aggregated_plugins")) repo_category <- "plugins"
    #   else repo_category <- input$repo_category
    #   
    #   xml_path <- paste0(r$edit_git_local_path, "/", repo_category, "/", repo_category, ".xml")
    #   
    #   if (file.exists(xml_path)){
    #     tryCatch({
    #       
    #       # Read XML file
    #       
    #       repo_category_elements <-
    #         xml2::read_xml(xml_path) %>%
    #         XML::xmlParse() %>%
    #         XML::xmlToDataFrame(nodes = XML::getNodeSet(., paste0("//", get_singular(repo_category))), stringsAsFactors = FALSE) %>%
    #         tibble::as_tibble()
    #       
    #       if (nrow(repo_category_elements) == 0){
    #         r$edit_git_repo_category_elements <- tibble::tibble(row_id = integer(), unique_id = character(), !!rlang::sym(paste0("name_", language)) := character(),
    #           !!rlang::sym(paste0("category_", language)) := character(), version = character(), creation_datetime = character(), update_datetime = character(), action = character())
    #       }
    #       
    #       if (nrow(repo_category_elements) > 0){
    #         prefixes <- c("name", "category")
    #         new_cols <- outer(prefixes, r$languages$code, paste, sep = "_") %>% as.vector()
    #         for(col in new_cols) if(!col %in% colnames(repo_category_elements)) repo_category_elements <- repo_category_elements %>% dplyr::mutate(!!col := "")
    #         
    #         if (input$repo_category %in% c("patient_lvl_plugins", "aggregated_plugins")){
    #           if (input$repo_category == "patient_lvl_plugins") plugin_type <- 1
    #           else if (input$repo_category == "aggregated_plugins") plugin_type <- 2
    #           repo_category_elements <- repo_category_elements %>% dplyr::filter(type == plugin_type)
    #         }
    #         
    #         r$edit_git_repo_category_elements <- repo_category_elements %>%
    #           dplyr::select(unique_id, paste0("name_", language), paste0("category_", language), version, creation_datetime, update_datetime) %>%
    #           dplyr::mutate_at(c("creation_datetime", "update_datetime"), lubridate::ymd_hms) %>%
    #           # dplyr::mutate_at(c("creation_datetime", "update_datetime"), format_datetime, language = language, sec = FALSE) %>%
    #           dplyr::mutate(row_id = dplyr::row_number(), .before = "unique_id") %>%
    #           dplyr::mutate(action = as.character(
    #             shiny::actionButton("delete_%plugin_id%", "", icon = icon("trash-alt"),
    #               onclick = paste0("Shiny.setInputValue('", id, "-edit_git_repo_delete_category_element', this.id, {priority: 'event'})"))
    #           )) %>%
    #           dplyr::mutate(action = stringr::str_replace_all(action, "%plugin_id%", as.character(row_id)))
    #       }
    #       
    #       error_loading_repo_category_files <- FALSE
    #     })
    #   }
    #   
    #   if (error_loading_repo_category_files){
    #     output$edit_repo_error_message <- renderUI(shiny.fluent::MessageBar(i18n$t("error_loading_category_xml_file"), messageBarType = 5))
    #     sapply(c("edit_repo_repo_files_div", "edit_repo_files"), shinyjs::hide)
    #     
    #     r$edit_git_repo_category_elements <- tibble::tibble(row_id = integer(), unique_id = character(), !!rlang::sym(paste0("name_", language)) := character(),
    #       !!rlang::sym(paste0("category_", language)) := character(), version = character(), creation_datetime = character(), update_datetime = character(), action = character())
    #   }
    #   else {
    #     output$edit_repo_error_message <- renderUI(tagList())
    #     sapply(c("edit_repo_repo_files_div", "edit_repo_files"), shinyjs::show)
    #   }
    # })
    # 
    # # Render datatable
    # 
    # edit_git_repo_editable_cols <- ""
    # edit_git_repo_sortable_cols <- c("name", "category", "creation_datetime", "update_datetime")
    # edit_git_repo_column_widths <- c("version" = "100px", "creation_datetime" = "130px", "update_datetime" = "130px", "action" = "100px")
    # edit_git_repo_centered_cols <- c("version", "creation_datetime", "update_datetime", "action")
    # edit_git_repo_searchable_cols <- c("name")
    # edit_git_repo_hidden_cols <- c("row_id", "unique_id")
    # edit_git_repo_col_names <- get_col_names("edit_git_repo_category_elements", i18n)
    # 
    # observeEvent(r$edit_git_repo_category_elements, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer r$edit_git_repo_category_elements"))
    #   
    #   if (length(r$edit_git_repo_category_elements_datatable_proxy) > 0) DT::replaceData(r$edit_git_repo_category_elements_datatable_proxy, 
    #     r$edit_git_repo_category_elements, resetPaging = FALSE, rownames = FALSE)
    #   
    #   # If datatable doesn't exist
    #   if (length(r$edit_git_repo_category_elements_datatable_proxy) == 0){
    #     
    #     render_datatable(output = output, ns = ns, i18n = i18n, data = r$edit_git_repo_category_elements,
    #       output_name = "edit_repo_files", col_names = edit_git_repo_col_names,
    #       editable_cols = edit_git_repo_editable_cols, sortable_cols = edit_git_repo_sortable_cols, 
    #       centered_cols = edit_git_repo_centered_cols, column_widths = edit_git_repo_column_widths,
    #       searchable_cols = edit_git_repo_searchable_cols, filter = TRUE, hidden_cols = edit_git_repo_hidden_cols, selection = "multiple")
    #     
    #     # Create a proxy for datatables
    #     
    #     r$edit_git_repo_category_elements_datatable_proxy <- DT::dataTableProxy("edit_repo_files", deferUntilFlush = FALSE)
    #   }
    # })
    # 
    # # Add a category element
    # 
    # observeEvent(input$edit_repo_add_files, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$edit_repo_add_files"))
    #   
    #   req(input$edit_repo_add_selected_files)
    #   
    #   # Get unique_ids
    #   
    #   req(input$repo_category %in% c("datasets", "patient_lvl_plugins", "aggregated_plugins", "scripts", "vocabularies", "studies"))
    #   
    #   # Category elements
    #   if (input$repo_category == "patient_lvl_plugins") category_elements <- r$plugins %>% dplyr::filter(tab_type_id == 1)
    #   else if (input$repo_category == "aggregated_plugins") category_elements <- r$plugins %>% dplyr::filter(tab_type_id == 2)
    #   else if (input$repo_category == "vocabularies") category_elements <- r$vocabulary
    #   else if (input$repo_category == "studies") category_elements <- r$edit_repo_studies
    #   else category_elements <- r[[input$repo_category]]
    #   category_elements <- category_elements %>% dplyr::filter(id %in% input$edit_repo_add_selected_files)
    #   
    #   # Repo category
    #   if (input$repo_category %in% c("patient_lvl_plugins", "aggregated_plugins")) repo_category <- "plugins"
    #   else repo_category <- input$repo_category
    #   
    #   # Options files
    #   if (repo_category == "plugins") options_files <- c("plugin_ui", "plugin_server", "plugin_translations")
    #   else options_files <- get_singular(repo_category)
    #   
    #   # Repo category path
    #   r$repo_category_path <- paste0(r$edit_git_local_path, "/", repo_category)
    #   r$repo_category <- repo_category
    #   
    #   tryCatch({
    #     for (category_element_id in category_elements %>% dplyr::pull(id)){
    #       
    #       # Get category element row, with associated options and code
    #       category_element <- category_elements %>% dplyr::filter(id == category_element_id)
    #       options <- r$options %>% dplyr::filter(category == get_singular(repo_category), link_id == category_element_id)
    #       code <- r$code %>% dplyr::filter(link_id == category_element_id, category %in% options_files)
    #       
    #       # Create folders if don't exist
    #       # Delete old folder
    #       if (repo_category == "plugins"){
    #         type <- gsub("_plugins", "", input$repo_category)
    #         local_category_element_dir <- paste0(r$app_folder, "/", repo_category, "/", type, "/", options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
    #         git_category_element_dir <- paste0(r$edit_git_local_path, "/", repo_category, "/", type, "/", options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
    #       }
    #       else {
    #         local_category_element_dir <- paste0(r$app_folder, "/", repo_category, "/", options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
    #         git_category_element_dir <- paste0(r$edit_git_local_path, "/", repo_category, "/", options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
    #       }
    #       if (!dir.exists(local_category_element_dir)) dir.create(local_category_element_dir, recursive = TRUE)
    #       if (dir.exists(git_category_element_dir)) unlink(git_category_element_dir, recursive = TRUE, force = TRUE)
    #       if (!dir.exists(git_category_element_dir)) dir.create(git_category_element_dir, recursive = TRUE)
    #       
    #       # Create ui.R, server.R & translations.csv for plugins
    #       if (repo_category == "plugins"){
    #         sapply(c("ui", "server"), function(name) writeLines(code %>% dplyr::filter(category == paste0("plugin_", name)) %>%
    #             dplyr::pull(code) %>% stringr::str_replace_all("''", "'"), paste0(local_category_element_dir, "/", name, ".R")))
    #         writeLines(code %>% dplyr::filter(category == "plugin_translations") %>% dplyr::pull(code) %>% stringr::str_replace_all("''", "'"), paste0(local_category_element_dir, "/translations.csv"))
    #       }
    #       # Create code.R for scripts
    #       if (repo_category %in% c("datasets", "scripts", "vocabularies")) writeLines(code %>% dplyr::filter(category == get_singular(repo_category)) %>% dplyr::pull(code) %>% stringr::str_replace_all("''", "'"), paste0(local_category_element_dir, "/code.R"))
    #       
    #       # Create XML file
    #       xml <- XML::newXMLDoc()
    #       category_general_node <- XML::newXMLNode(repo_category, doc = xml)
    #       category_node <- XML::newXMLNode(get_singular(repo_category), parent = category_general_node)
    #       XML::newXMLNode("app_version", r$app_version, parent = category_node)
    #       
    #       if (repo_category == "plugins"){
    #         if (type == "patient_lvl") type <- 1 else type <- 2
    #         XML::newXMLNode("type", type, parent = category_node)
    #         for(name in c("unique_id", "version", "author", "image", paste0("name_", r$languages$code), paste0("category_", r$languages$code))){
    #           XML::newXMLNode(name, options %>% dplyr::filter(name == !!name) %>% dplyr::pull(value), parent = category_node)
    #         }
    #         for(name in c(paste0("description_", r$languages$code))) XML::newXMLNode(name, options %>% dplyr::filter(name == !!name) %>%
    #             dplyr::pull(value) %>% stringr::str_replace_all("''", "'"), parent = category_node)
    #         for (name in c("creation_datetime", "update_datetime")) XML::newXMLNode(name, category_element %>% dplyr::pull(get(!!name)), parent = category_node)
    #       }
    #       
    #       else if (repo_category %in% c("datasets", "scripts", "vocabularies", "studies")){
    #         for(name in c("unique_id", "version", "author", paste0("name_", r$languages$code), paste0("category_", r$languages$code))) XML::newXMLNode(name, 
    #           options %>% dplyr::filter(name == !!name) %>% dplyr::pull(value), parent = category_node)
    #         for(name in c(paste0("description_", r$languages$code))) XML::newXMLNode(name,
    #           options %>% dplyr::filter(name == !!name) %>% dplyr::pull(value) %>% stringr::str_replace_all("''", "'"), parent = category_node)
    #         for (name in c("creation_datetime", "update_datetime")) XML::newXMLNode(name, category_element %>% dplyr::pull(get(!!name)), parent = category_node)
    #         XML::newXMLNode("code", code %>% dplyr::pull(code) %>% stringr::str_replace_all("''", "'"), parent = category_node)
    #       }
    #       
    #       if (repo_category == "datasets") XML::newXMLNode("omop_version", options %>% dplyr::filter(name == "omop_version") %>% dplyr::pull(value), parent = category_node)
    #       if (repo_category == "vocabularies") XML::newXMLNode("vocabulary_id", category_element %>% dplyr::pull(vocabulary_id), parent = category_node)
    #       
    #       list_of_files <- list.files(local_category_element_dir)
    #       
    #       if (repo_category %in% c("datasets", "plugins", "scripts", "vocabularies", "studies")){
    #         # Add images filenames in the XML
    #         images <- list_of_files[grepl("\\.(png|jpg|jpeg|svg)$", tolower(list_of_files))]
    #         images_node <- XML::newXMLNode("images", paste(images, collapse = ";;;"), parent = category_node)
    #       }
    #       
    #       XML::saveXML(xml, file = paste0(local_category_element_dir, "/", get_singular(repo_category), ".xml"))
    #       
    #       # Update list_of_files with XML file
    #       list_of_files <- list.files(local_category_element_dir)
    #       # Select files to copy (images, R files and XML)
    #       # Allow csv for plugins (translations.csv)
    #       if (repo_category == "plugins") files_to_copy <- list_of_files[grepl("(\\.(png|jpg|jpeg|svg|r|xml)$)|(^translations\\.csv$)", tolower(list_of_files))]
    #       else files_to_copy <- list_of_files[grepl("\\.(png|jpg|jpeg|svg|r|xml)$", tolower(list_of_files))]
    #       
    #       # For studies, add csv files & plugins files
    #       
    #       if (repo_category == "studies"){
    #         
    #         study <- category_element
    #         
    #         corresponding_ids <- list()
    #         for (table in c("plugins", "options", "code")) corresponding_ids[[table]] <- tibble::tibble(old_id = integer(), new_id = integer())
    #         data <- list()
    #         
    #         # Get data
    #         
    #         ### tabs_groups
    #         sql <- glue::glue_sql("SELECT * FROM tabs_groups WHERE id IN ({c(study$patient_lvl_tab_group_id, study$aggregated_tab_group_id)*}) AND deleted IS FALSE", .con = r$db)
    #         data$tabs_groups <- DBI::dbGetQuery(r$db, sql) %>% dplyr::mutate(creator_id = NA_integer_)
    #         corresponding_ids$tabs_groups <- tibble::tibble(old_id = as.integer(data$tabs_groups$id), new_id = c(1L, 2L))
    #         
    #         ### tabs
    #         sql <- glue::glue_sql("SELECT * FROM tabs WHERE tab_group_id IN ({c(study$patient_lvl_tab_group_id, study$aggregated_tab_group_id)*}) AND deleted IS FALSE", .con = r$db)
    #         data$tabs <- DBI::dbGetQuery(r$db, sql) %>% dplyr::mutate(creator_id = NA_integer_)
    #         if (nrow(data$tabs) > 0) corresponding_ids$tabs <- data$tabs %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = 1:dplyr::n())
    #         
    #         ### widgets
    #         sql <- glue::glue_sql("SELECT * FROM widgets WHERE tab_id IN ({data$tabs$id*}) AND deleted IS FALSE", .con = r$db)
    #         data$widgets <- DBI::dbGetQuery(r$db, sql) %>% dplyr::mutate(creator_id = NA_integer_)
    #         if (nrow(data$widgets) > 0) corresponding_ids$widgets <- data$widgets %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = 1:dplyr::n())
    #         
    #         ### widgets_concepts
    #         sql <- glue::glue_sql("SELECT * FROM widgets_concepts WHERE widget_id IN ({data$widgets$id*}) AND deleted IS FALSE", .con = m$db)
    #         data$widgets_concepts <- DBI::dbGetQuery(m$db, sql) %>% dplyr::mutate(creator_id = NA_integer_)
    #         if (nrow(data$widgets_concept) > 0) corresponding_ids$widgets_concepts <- data$widgets_concepts %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = 1:dplyr::n())
    #         
    #         ### widgets_options
    #         sql <- glue::glue_sql("SELECT * FROM widgets_options WHERE widget_id IN ({data$widgets$id*}) AND deleted IS FALSE", .con = m$db)
    #         data$widgets_options <- DBI::dbGetQuery(m$db, sql) %>% dplyr::mutate(creator_id = NA_integer_, person_id = NA_integer_)
    #         if (nrow(data$widgets_options) > 0) corresponding_ids$widgets_options <- data$widgets_options %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = 1:dplyr::n())
    #         
    #         ### plugins
    #         sql <- glue::glue_sql("SELECT * FROM plugins WHERE id IN ({data$widgets %>% dplyr::distinct(plugin_id) %>% dplyr::pull()*}) AND deleted IS FALSE", .con = r$db)
    #         data$plugins <- DBI::dbGetQuery(r$db, sql)
    #         if (nrow(data$plugins) > 0){
    #           if (nrow(corresponding_ids$plugins) > 0) last_row <- max(corresponding_ids$plugins$new_id)
    #           else last_row <- 0L
    #           corresponding_ids$plugins <- corresponding_ids$plugins %>% dplyr::bind_rows(
    #             data$plugins %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = last_row + 1:dplyr::n()))
    #         }
    #         
    #         ### options (for plugins)
    #         sql <- glue::glue_sql("SELECT * FROM options WHERE category = 'plugin' AND link_id IN ({data$plugins %>% dplyr::distinct(id) %>% dplyr::pull()*}) AND deleted IS FALSE", .con = r$db)
    #         data$options <- DBI::dbGetQuery(r$db, sql)
    #         if (nrow(data$options) > 0){
    #           if (nrow(corresponding_ids$options) > 0) last_row <- max(corresponding_ids$options$new_id)
    #           else last_row <- 0L
    #           corresponding_ids$options <- corresponding_ids$options %>% dplyr::bind_rows(
    #             data$options %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = last_row + 1:dplyr::n()))
    #         }
    #         
    #         ### code (for plugins)
    #         sql <- glue::glue_sql("SELECT * FROM code WHERE category IN ('plugin_ui', 'plugin_server', 'plugin_translations') AND link_id IN ({data$plugins %>% dplyr::distinct(id) %>% dplyr::pull()*}) AND deleted IS FALSE", .con = r$db)
    #         data$code <- DBI::dbGetQuery(r$db, sql)
    #         if (nrow(data$code) > 0){
    #           if (nrow(corresponding_ids$code) > 0) last_row <- max(corresponding_ids$code$new_id)
    #           else last_row <- 0L
    #           corresponding_ids$code <- corresponding_ids$code %>% dplyr::bind_rows(
    #             data$code %>% dplyr::select(old_id = id) %>% dplyr::mutate(new_id = last_row + 1:dplyr::n()))
    #         }
    #         
    #         # Change IDs
    #         
    #         ### tabs_groups
    #         data$tabs_groups <- data$tabs_groups %>% dplyr::mutate(id = c(1L, 2L), .before = "category")
    #         
    #         ### tabs
    #         if (nrow(data$tabs) > 0) data$tabs <- data$tabs %>%
    #           dplyr::left_join(corresponding_ids$tabs %>% dplyr::select(id = old_id, new_id), by = "id") %>%
    #           dplyr::left_join(corresponding_ids$tabs %>% dplyr::select(parent_tab_id = old_id, new_parent_tab_id = new_id), by = "parent_tab_id") %>%
    #           dplyr::left_join(corresponding_ids$tabs_groups %>% dplyr::select(tab_group_id = old_id, new_tab_group_id = new_id), by = "tab_group_id") %>%
    #           dplyr::select(-id, -parent_tab_id, -tab_group_id) %>%
    #           dplyr::rename(id = new_id, parent_tab_id = new_parent_tab_id, tab_group_id = new_tab_group_id) %>%
    #           dplyr::relocate(id, .before = "category") %>%
    #           dplyr::relocate(parent_tab_id, .before = "display_order") %>%
    #           dplyr::relocate(tab_group_id, .before = "parent_tab_id")
    #         
    #         ### widgets
    #         if (nrow(data$widgets) > 0) data$widgets <- data$widgets %>%
    #           dplyr::left_join(corresponding_ids$widgets %>% dplyr::select(id = old_id, new_id = new_id), by = "id") %>%
    #           dplyr::left_join(corresponding_ids$tabs %>% dplyr::select(tab_id = old_id, new_tab_id = new_id), by = "tab_id") %>%
    #           dplyr::left_join(corresponding_ids$plugins %>% dplyr::select(plugin_id = old_id, new_plugin_id = new_id), by = "plugin_id") %>%
    #           dplyr::select(-id, -tab_id, -plugin_id) %>%
    #           dplyr::rename(id = new_id, tab_id = new_tab_id, plugin_id = new_plugin_id) %>%
    #           dplyr::relocate(id, .before = "category") %>%
    #           dplyr::relocate(tab_id, .after = "name") %>%
    #           dplyr::relocate(plugin_id, .before = "display_order")
    #         
    #         ### widgets_concepts
    #         if (nrow(data$widgets_concepts) > 0) data$widgets_concepts <- data$widgets_concepts %>%
    #           dplyr::left_join(corresponding_ids$widgets_concepts %>% dplyr::select(id = old_id, new_id = new_id), by = "id") %>%
    #           dplyr::left_join(corresponding_ids$widgets %>% dplyr::select(widget_id = old_id, new_widget_id = new_id), by = "widget_id") %>%
    #           dplyr::select(-id, -widget_id) %>%
    #           dplyr::rename(id = new_id, widget_id = new_widget_id) %>%
    #           dplyr::relocate(id, .before = "concept_id") %>%
    #           dplyr::relocate(widget_id, .after = "id")
    #         
    #         ### widgets_options
    #         if (nrow(data$widgets_options) > 0) data$widgets_options <- data$widgets_options %>%
    #           dplyr::left_join(corresponding_ids$widgets_options %>% dplyr::select(id = old_id, new_id = new_id), by = "id") %>%
    #           dplyr::left_join(corresponding_ids$widgets %>% dplyr::select(widget_id = old_id, new_widget_id = new_id), by = "widget_id") %>%
    #           dplyr::select(-id, -widget_id) %>%
    #           dplyr::rename(id = new_id, widget_id = new_widget_id) %>%
    #           dplyr::relocate(id, .before = "person_id") %>%
    #           dplyr::relocate(widget_id, .after = "id")
    #         
    #         ### plugins
    #         if (nrow(data$plugins) > 0) data$plugins <- data$plugins %>%
    #           dplyr::left_join(corresponding_ids$plugins %>% dplyr::select(id = old_id, new_id), by = "id") %>%
    #           dplyr::select(-id) %>%
    #           dplyr::rename(id = new_id) %>%
    #           dplyr::relocate(id, .before = "name")
    #         
    #         ### options
    #         if (nrow(data$options) > 0) data$options <- data$options %>%
    #           dplyr::left_join(corresponding_ids$options %>% dplyr::select(id = old_id, new_id), by = "id") %>%
    #           dplyr::left_join(corresponding_ids$plugins %>% dplyr::select(link_id = old_id, new_link_id = new_id), by = "link_id") %>%
    #           dplyr::select(-id, -link_id) %>%
    #           dplyr::rename(id = new_id, link_id = new_link_id) %>%
    #           dplyr::relocate(id, .before = "category") %>%
    #           dplyr::relocate(link_id, .before = "name")
    #         
    #         ### code
    #         if (nrow(data$code) > 0) data$code <- data$code %>%
    #           dplyr::left_join(corresponding_ids$code %>% dplyr::select(id = old_id, new_id), by = "id") %>%
    #           dplyr::left_join(corresponding_ids$plugins %>% dplyr::select(link_id = old_id, new_link_id = new_id), by = "link_id") %>%
    #           dplyr::select(-id, -link_id) %>%
    #           dplyr::rename(id = new_id, link_id = new_link_id) %>%
    #           dplyr::relocate(id, .before = "category") %>%
    #           dplyr::relocate(link_id, .before = "code")
    #         
    #         # Create CSV files
    #         dir.create(paste0(git_category_element_dir, "/app_database"))
    #         for (csv_file in c("tabs_groups", "tabs", "widgets", "widgets_concepts", "widgets_options",
    #           "plugins", "options", "code")) readr::write_csv(data[[csv_file]], paste0(git_category_element_dir, "/app_database/", csv_file, ".csv"))
    #         
    #         # Copy plugins folders
    #         dir.create(paste0(git_category_element_dir, "/plugins"))
    #         dir.create(paste0(git_category_element_dir, "/plugins/patient_lvl"))
    #         dir.create(paste0(git_category_element_dir, "/plugins/aggregated"))
    #         
    #         if (nrow(data$plugins) > 0){
    #           for (i in 1:nrow(data$plugins)){
    #             plugin <- data$plugins[i, ]
    #             options <- data$options %>% dplyr::filter(category == "plugin" & link_id == plugin$id)
    #             unique_id <- options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value)
    #             if (plugin$tab_type_id == 1) type <- "patient_lvl" else type <- "aggregated"
    #             
    #             # Create XML file
    #             
    #             xml <- XML::newXMLDoc()
    #             plugins_node <- XML::newXMLNode("plugins", doc = xml)
    #             plugin_node <- XML::newXMLNode("plugin", parent = plugins_node)
    #             XML::newXMLNode("app_version", r$app_version, parent = plugin_node)
    #             XML::newXMLNode("type", plugin$tab_type_id, parent = plugin_node)
    #             for(name in c("unique_id", "version", "author", "image", paste0("name_", r$languages$code), paste0("category_", r$languages$code))){
    #               XML::newXMLNode(name, options %>% dplyr::filter(name == !!name) %>% dplyr::pull(value), parent = plugin_node)
    #             }
    #             for(name in c(paste0("description_", r$languages$code))) XML::newXMLNode(name, options %>% dplyr::filter(name == !!name) %>%
    #                 dplyr::pull(value) %>% stringr::str_replace_all("''", "'"), parent = plugin_node)
    #             for (name in c("creation_datetime", "update_datetime")) XML::newXMLNode(name, plugin %>% dplyr::pull(get(!!name)), parent = plugin_node)
    #             
    #             plugin_dir <- paste0(r$app_folder, "/plugins/", type, "/", unique_id)
    #             if (!dir.exists(plugin_dir)) dir.create(plugin_dir)
    #             
    #             list_of_files <- list.files(plugin_dir)
    #             images <- list_of_files[grepl("\\.(png|jpg|jpeg|svg)$", tolower(list_of_files))]
    #             images_node <- XML::newXMLNode("images", paste(images, collapse = ";;;"), parent = plugin_node)
    #             
    #             XML::saveXML(xml, file = paste0(plugin_dir, "/plugin.xml"))
    #             
    #             # Copy files
    #             
    #             list_of_files <- list.files(plugin_dir)
    #             dir.create(paste0(git_category_element_dir, "/plugins/", type, "/", unique_id))
    #             file.copy(paste0(plugin_dir, "/", list_of_files), paste0(git_category_element_dir, "/plugins/", type, "/", unique_id, "/", list_of_files))
    #           }
    #         }
    #       }
    #       
    #       # Copy all files in temp git folder
    #       file.copy(paste0(local_category_element_dir, "/", files_to_copy),  paste0(git_category_element_dir, "/", files_to_copy), overwrite = TRUE)
    #     }
    #   }, error = function(e) report_bug(r = r, output = output, error_message = "error_creating_xml_files",
    #     error_name = paste0(id, " - create git repo XML files"), category = "Error", error_report = toString(e), i18n = i18n))
    #   
    #   # Clear dropdown
    #   
    #   if (input$repo_category == "patient_lvl_plugins") dropdown_options <- r$plugins %>% dplyr::filter(tab_type_id == 1)
    #   else if (input$repo_category == "aggregated_plugins") dropdown_options <- r$plugins %>% dplyr::filter(tab_type_id == 2)
    #   else if (input$repo_category == "vocabularies") dropdown_options <- r$vocabulary
    #   else if (input$repo_category == "studies") dropdown_options <- r$edit_repo_studies
    #   else dropdown_options <- r[[input$repo_category]]
    #   
    #   if (input$repo_category == "vocabularies") dropdown_options <- dropdown_options %>% dplyr::arrange(tolower(vocabulary_name)) %>% convert_tibble_to_list(key_col = "id", text_col = "vocabulary_name")
    #   else dropdown_options <- dropdown_options %>% dplyr::arrange(tolower(name)) %>% convert_tibble_to_list(key_col = "id", text_col = "name")
    #   
    #   shiny.fluent::updateDropdown.shinyInput(session, "edit_repo_add_selected_files", options = dropdown_options, value = NULL)
    #   
    #   # Create global XML file
    #   r$edit_repo_create_global_xml_trigger <- now()
    # })
    # 
    # # Delete a category element
    # 
    # observeEvent(input$edit_git_repo_delete_category_element, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$edit_git_repo_delete_category_element"))
    #   
    #   r$delete_edit_git_repo_category_elements_open_dialog <- TRUE
    #   r$delete_edit_git_repo_category_elements <- as.integer(substr(input$edit_git_repo_delete_category_element, nchar("delete_") + 1, 100))
    #   
    #   # Reload datatable (to unselect rows)
    #   DT::replaceData(r$edit_git_repo_category_elements_datatable_proxy, r$edit_git_repo_category_elements, resetPaging = FALSE, rownames = FALSE)
    # })
    # 
    # observeEvent(input$edit_repo_delete_selection, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$edit_repo_delete_selection"))
    #   
    #   req(length(input$edit_repo_files_rows_selected) > 0)
    #   
    #   r$delete_edit_git_repo_category_elements <- r$edit_git_repo_category_elements[input$edit_repo_files_rows_selected, ] %>% dplyr::pull(row_id)
    #   r$delete_edit_git_repo_category_elements_open_dialog <- TRUE
    # })
    # 
    # r$delete_edit_git_repo_category_elements_open_dialog <- FALSE
    # output$edit_repo_delete_confirm <- shiny.fluent::renderReact({
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - output$edit_repo_delete_confirm"))
    #   
    #   shiny.fluent::Dialog(
    #     hidden = !r$delete_edit_git_repo_category_elements_open_dialog,
    #     onDismiss = htmlwidgets::JS("function() { Shiny.setInputValue('delete_edit_git_repo_hide_dialog', Math.random()); }"),
    #     dialogContentProps = list(
    #       type = 0,
    #       title = i18n$t("edit_git_repo_delete_file"),
    #       closeButtonAriaLabel = "Close",
    #       subText = tagList(i18n$t("edit_git_repo_delete_file_subtext"), br(), br())
    #     ),
    #     modalProps = list(),
    #     shiny.fluent::DialogFooter(
    #       shiny.fluent::PrimaryButton.shinyInput(ns("delete_edit_git_repo_delete_confirmed"), text = i18n$t("delete")),
    #       shiny.fluent::DefaultButton.shinyInput(ns("delete_edit_git_repo_delete_canceled"), text = i18n$t("dont_delete"))
    #     )
    #   )
    # })
    # 
    # observeEvent(input$delete_edit_git_repo_hide_dialog, {
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$delete_edit_git_repo_hide_dialog"))
    #   r$delete_edit_git_repo_category_elements_open_dialog <- FALSE
    # })
    # observeEvent(input$delete_edit_git_repo_delete_canceled, {
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$delete_edit_git_repo_delete_canceled"))
    #   r$delete_edit_git_repo_category_elements_open_dialog <- FALSE
    # })
    # 
    # # Fields to create XML file
    # prefixes <- c("description", "name", "category")     
    # new_cols <- outer(prefixes, r$languages$code, paste, sep = "_") %>% as.vector()
    # 
    # repo_category_xml_fields <- list()
    # 
    # repo_category_xml_fields$plugins <- tibble::tibble(
    #   app_version = character(), type = character(), unique_id = character(), 
    #   version = character(), author = character(), image = character())
    # for(col in new_cols) if(!col %in% colnames(repo_category_xml_fields$plugins)) repo_category_xml_fields$plugins <- 
    #   repo_category_xml_fields$plugins %>% dplyr::mutate(!!col := "")
    # 
    # repo_category_xml_fields$scripts <- tibble::tibble(
    #   app_version = character(), unique_id = character(),  version = character(), author = character())
    # for(col in new_cols) if(!col %in% colnames(repo_category_xml_fields$scripts)) repo_category_xml_fields$scripts <- 
    #   repo_category_xml_fields$scripts %>% dplyr::mutate(!!col := "")
    # 
    # # Fields are the same for scripts, datasets & vocabularies
    # repo_category_xml_fields$datasets <- repo_category_xml_fields$scripts
    # repo_category_xml_fields$vocabularies <- repo_category_xml_fields$scripts
    # repo_category_xml_fields$studies <- repo_category_xml_fields$scripts
    # 
    # observeEvent(input$delete_edit_git_repo_delete_confirmed, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$delete_edit_git_repo_delete_confirmed"))
    #   
    #   tryCatch({
    #     unique_ids <- 
    #       r$edit_git_repo_category_elements %>%
    #       dplyr::filter(row_id %in% r$delete_edit_git_repo_category_elements) %>%
    #       dplyr::pull(unique_id)
    #     
    #     # Not working for studies, datasets & vocabularies for now
    #     req(input$repo_category %in% c("datasets", "patient_lvl_plugins", "aggregated_plugins", "scripts", "vocabularies", "studies"))
    #     
    #     if (input$repo_category %in% c("patient_lvl_plugins", "aggregated_plugins")){
    #       repo_category <- "plugins" 
    #       type <- gsub("_plugins", "", input$repo_category)
    #       repo_category_path <- paste0(r$edit_git_local_path, "/", repo_category)
    #       repo_category_delete_path <- paste0(r$edit_git_local_path, "/", repo_category, "/", type)
    #     }
    #     else {
    #       repo_category <- input$repo_category
    #       repo_category_path <- paste0(r$edit_git_local_path, "/", repo_category)
    #       repo_category_delete_path <- repo_category_path
    #     }
    #     
    #     sapply(unique_ids, function(unique_id) unlink(paste0(repo_category_delete_path, "/", unique_id), recursive = TRUE, force = TRUE))
    #     
    #     r$repo_category_path <- repo_category_path
    #     r$repo_category <- repo_category
    #     
    #     # Create global XML file
    #     r$edit_repo_create_global_xml_trigger <- now()
    #     
    #     show_message_bar(output, "file_deleted", "warning", i18n = i18n, ns = ns)
    #     
    #   }, error = function(e) report_bug(r = r, output = output, error_message = "error_deleting_file",
    #     error_name = paste0(id, " - delete git repo file"), category = "Error", error_report = toString(e), i18n = i18n))
    #   
    #   r$delete_edit_git_repo_category_elements_open_dialog <- FALSE
    # })
    # 
    # observeEvent(r$edit_repo_create_global_xml_trigger, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer r$edit_repo_create_global_xml_trigger"))
    #   
    #   repo_category_path <- r$repo_category_path
    #   repo_category <- r$repo_category
    #   
    #   # Delete XML file
    #   unlink(paste0(repo_category_path, "/", repo_category, ".xml"), force = TRUE)
    #   
    #   # Create XML file
    #   
    #   ## Convert XML to tibble
    #   
    #   dirs <- character()
    #   if (repo_category == "plugins") for (category in c("patient_lvl", "aggregated")) dirs <- c(dirs, list.dirs(paste0(repo_category_path, "/", category), full.names = TRUE))
    #   else dirs <- list.dirs(repo_category_path, full.names = TRUE)
    #   
    #   category_tibble <- repo_category_xml_fields[[repo_category]]
    #   
    #   for (dir in dirs){
    #     if ((repo_category %not_in% c("plugins", "studies") & dir != repo_category_path) |
    #         (repo_category == "plugins" & dir != paste0(repo_category_path, "/patient_lvl") & dir != paste0(repo_category_path, "/aggregated")) |
    #         (repo_category == "studies" & dir != repo_category_path & !grepl("app_database$", dir) & !grepl("plugins$", dir)
    #           & !grepl("aggregated", dir) & !grepl("patient_lvl", dir))){
    #       category_tibble <-
    #         category_tibble %>%
    #         dplyr::bind_rows(
    #           xml2::read_xml(paste0(dir, "/", get_singular(repo_category), ".xml")) %>%
    #             XML::xmlParse() %>%
    #             XML::xmlToDataFrame(nodes = XML::getNodeSet(., paste0("//", get_singular(repo_category))), stringsAsFactors = FALSE) %>%
    #             tibble::as_tibble()
    #         )
    #     }
    #   }
    #   
    #   ## Convert tibble to XML
    #   
    #   category_xml <- XML::newXMLDoc()
    #   category_general_node <- XML::newXMLNode(repo_category, doc = category_xml)
    #   
    #   category_nodes <- apply(category_tibble, 1, function(x) {
    #     category_node <- XML::newXMLNode(get_singular(repo_category))
    #     XML::addChildren(category_node, lapply(names(x), function(y) XML::newXMLNode(y, x[y])))
    #   })
    #   
    #   XML::xmlParent(category_nodes) <- category_general_node
    #   
    #   XML::saveXML(category_xml, file = paste0(repo_category_path, "/", repo_category, ".xml"))
    #   
    #   # Trigger to render datatable
    #   r$edit_repo_datatable_trigger <- now()
    # })
    # 
    # # Save README
    # 
    # observeEvent(input$git_repo_readme_save, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$git_repo_readme_save"))
    #   
    #   tryCatch({
    #       writeLines(input$git_repo_readme, paste0(r$edit_git_local_path, "/README.md"))
    #       show_message_bar(output,  "modif_saved", "success", i18n, ns = ns)
    #     },
    #     error = function(e) report_bug(r = r, output = output, error_message = "error_update_readme",
    #       error_name = "settings_git update README", category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
    # })
    # 
    # # Preview README
    # 
    # observeEvent(input$git_repo_readme_preview, {
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$git_repo_readme_preview"))
    #   r$git_repo_readme_preview_trigger <- now()
    # })
    # 
    # observeEvent(input$git_repo_readme_run_all, {
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$git_repo_readme_run_all"))
    #   r$git_repo_readme_preview_trigger <- now()
    # })
    # 
    # # Generate README
    # 
    # observeEvent(input$git_repo_readme_generate, {
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$git_repo_readme_generate"))
    #   
    #   git_repo_raw_files_url_address <- r$git_repos %>% dplyr::filter(id == input$edit_repo_selected_repo$key) %>% dplyr::pull(raw_files_url_address)
    #   if (substr(git_repo_raw_files_url_address, nchar(git_repo_raw_files_url_address), nchar(git_repo_raw_files_url_address)) != "/") git_repo_raw_files_url_address <- paste0(git_repo_raw_files_url_address, "/")
    #   
    #   # readme <- paste0("## ", input$edit_repo_selected_repo$text)
    #   readme <- ""
    #   
    #   for (category in c("studies", "plugins_patient_lvl", "plugins_aggregated", "scripts", "datasets", "vocabularies")){
    #     
    #     if (category == "plugins_patient_lvl"){
    #       category <- "plugins"
    #       plugins_type <- 1
    #     }
    #     if (category == "plugins_aggregated"){
    #       category <- "plugins"
    #       plugins_type <- 2
    #     }
    #     
    #     icon <- switch(category, "studies" = "file-alt", "plugins" = "terminal", "scripts" = "code", "datasets" = "database", "vocabularies" = "list")
    #     readme_category <- paste0("\n\n## <i class='fa fa-", icon, "' style='color: steelblue;'></i> ", stringr::str_to_title(category))
    #     if (category == "studies") readme_category <- ""
    #     
    #     if (category == "plugins"){
    #       if (plugins_type == 1) readme_category <- paste0(readme_category, " - Patient-level data")
    #       if (plugins_type == 2) readme_category <- paste0(readme_category, " - Aggregated data")
    #     }
    #     readme_category <- paste0(readme_category, "\n\n")
    #     
    #     tryCatch({
    #       # Load XML
    #       xml_path <- paste0(r$edit_git_local_path, "/", category, "/", category, ".xml") 
    #       
    #       category_elements <-
    #         xml2::read_xml(xml_path) %>%
    #         XML::xmlParse() %>%
    #         XML::xmlToDataFrame(nodes = XML::getNodeSet(., paste0("//", get_singular(category))), stringsAsFactors = FALSE) %>%
    #         tibble::as_tibble()
    #       
    #       if (category == "plugins") category_elements <- category_elements %>% dplyr::filter(type == plugins_type)
    #       
    #       if (nrow(category_elements) > 0){
    #         
    #         category_elements <- category_elements %>% dplyr::arrange(name_en)
    #         
    #         for (i in 1:nrow(category_elements)){
    #           
    #           category_element <- category_elements[i, ]
    #           
    #           category_folder <- paste0(git_repo_raw_files_url_address, category, "/")
    #           if (category == "plugins"){
    #             if (plugins_type == 1) category_folder <- paste0(category_folder, "patient_lvl/")
    #             if (plugins_type == 2) category_folder <- paste0(category_folder, "aggregated/")
    #           }
    #           category_folder <- paste0(category_folder, category_element$unique_id)
    #           
    #           description <- category_element$description_en %>%
    #             stringr::str_replace_all(paste0("%", get_singular(category), "_folder%"), category_folder)
    #           
    #           readme_category <- paste0(
    #             readme_category,
    #             "<details style = 'border:solid 1px; padding:10px; margin:5px 0px 5px 0px;'>\n",
    #             "<summary><span style = 'font-size:15px;'>", category_element$name_en, "</summary>\n\n",
    #             description, "\n",
    #             "</details>\n\n"
    #           )
    #         }
    #         readme_category <- paste0(readme_category, "\n")
    #       }
    #       
    #       if (nrow(category_elements) == 0) readme_category <- paste0(readme_category, "No ", category, " available.")
    #       
    #     }, error = function(e) report_bug(r = r, output = output, error_message = "error_generate_readme",
    #       error_name = "settings_git generate readme", category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
    #     
    #     readme <- paste0(readme, readme_category)
    #   }
    #   
    #   # Keep intro if an intro was written
    #   intro <- paste0(
    #     "## ", input$edit_repo_selected_repo$text ,"\n\n",
    #     "## <i class='fa fa-file-alt' style='color: steelblue;'></i> Studies")
    #   
    #   regex <- "(.*?)(\n+## <i class='fa fa-file-alt' style='color: steelblue;'></i> Studies)"
    #   
    #   if (grepl(regex, input$git_repo_readme)){
    #     end_pos <- stringr::str_locate(input$git_repo_readme, "<i class='fa fa-file-alt' style='color: steelblue;'></i> Studies")[2]
    #     
    #     intro <- substr(input$git_repo_readme, 1, end_pos)
    #   }
    #   
    #   readme <- paste0(intro, readme)
    #   
    #   shinyAce::updateAceEditor(session, "git_repo_readme", value = readme)
    # })
    # 
    # observeEvent(r$git_repo_readme_preview_trigger, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer r$git_repo_readme_preview_trigger"))
    #   
    #   tryCatch({
    #     
    #     # Clear temp dir
    #     
    #     markdown_settings <- paste0("```{r setup, include=FALSE}\nknitr::opts_knit$set(root.dir = '", 
    #       r$app_folder, "/temp_files/", r$user_id, "/markdowns')\n",
    #       "knitr::opts_chunk$set(root.dir = '", r$app_folder, "/temp_files/", r$user_id, "/markdowns', fig.path = '", r$app_folder, "/temp_files/", r$user_id, "/markdowns')\n```\n")
    #     
    #     markdown_file <- paste0(markdown_settings, input$git_repo_readme)
    #     
    #     # Create temp dir
    #     dir <- paste0(r$app_folder, "/temp_files/", r$user_id, "/markdowns")
    #     file <- paste0(dir, "/", paste0(sample(c(0:9, letters[1:6]), 8, TRUE), collapse = ''), "_", now() %>% stringr::str_replace_all(":", "_"), ".Md")
    #     if (!dir.exists(dir)) dir.create(dir)
    #     
    #     # Create the markdown file
    #     knitr::knit(text = markdown_file, output = file, quiet = TRUE)
    #     
    #     output$description_markdown_result <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(file))))
    #   }, error = function(e) "")
    # })
    # 
    # # Commit & push
    # 
    # observeEvent(input$commit_and_push, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_settings_git - observer input$commit_and_push"))
    #   
    #   # Check if commit message is not empty
    #   
    #   empty_commit_message <- FALSE
    #   if (length(input$commit_message) == 0) empty_commit_message <- TRUE
    #   if (length(input$commit_message) > 0) if (is.na(input$commit_message) | input$commit_message == "") empty_commit_message <- TRUE
    #   
    #   if (empty_commit_message) shiny.fluent::updateTextField.shinyInput(session, "commit_message", errorMessage = i18n$t("provide_valid_commit_message"))
    #   else shiny.fluent::updateTextField.shinyInput(session, "commit_message", errorMessage = NULL)
    #   
    #   req(!empty_commit_message)
    #   
    #   tryCatch({
    #     repo <- r$edit_git_repo_repo
    #     credentials <- r$edit_git_repo_credentials
    #     
    #     git2r::add(repo, ".")
    #     
    #     if (length(git2r::status(repo, unstaged = FALSE, untracked = FALSE, ignored = FALSE)$staged) > 0){
    #       
    #       git2r::commit(repo, message = input$commit_message)
    #       
    #       # Create main branch if doesn't exist
    #       git_branches <- names(git2r::branches(repo))
    #       if (!"main" %in% git_branches){
    #         git2r::branch_create(commit = git2r::last_commit(repo), name = "main")
    #         git2r::checkout(repo, "main")
    #       }
    #       
    #       git2r::push(repo, "origin", "refs/heads/main", credentials = credentials)
    #     }
    #     
    #     shiny.fluent::updateTextField.shinyInput(session, "commit_message", value = "")
    #     
    #     show_message_bar(output, "success_update_remote_git_repo", "success", i18n = i18n, ns = ns)
    #   }, error = function(e) report_bug(r = r, output = output, error_message = "error_update_remote_git_repo",
    #     error_name = "settings_git commit & push repo", category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
    # })
  })
}
