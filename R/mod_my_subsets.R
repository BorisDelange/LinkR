#' my_subsets UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_my_subsets_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  
  cards <- c("subsets_datatable_card", "subsets_persons_card", "subsets_edit_code_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  })
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    shiny.fluent::reactOutput(ns("subset_persons_delete_confirm")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "subset_main", text = i18n$t("my_subsets"))
    ), maxDisplayedItems = 3),
    shinyjs::hidden(
      div(id = ns("menu"),
        shiny.fluent::Pivot(
          id = ns("subsets_pivot"),
          onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
          shiny.fluent::PivotItem(id = "subsets_datatable_card", itemKey = "datatable_card", headerText = i18n$t("subsets_management")),
          shiny.fluent::PivotItem(id = "subsets_edit_code_card", itemKey = "edit_code_card", headerText = i18n$t("edit_subset_code")),
          shiny.fluent::PivotItem(id = "subsets_persons_card", itemKey = "subsets_persons_card", headerText = i18n$t("subset_persons"))
        )
      )
    ),
    forbidden_cards,
    div(
      id = ns("choose_a_study_card"),
      make_card("", div(shiny.fluent::MessageBar(i18n$t("choose_study_and_dataset_left_side"), messageBarType = 5), style = "margin-top:10px;"))
    ),
    
    # --- --- --- --- --- -- --- -
    # Subsets management card ----
    # --- --- --- --- --- -- --- -
    
    shinyjs::hidden(
      div(
        id = ns("subsets_datatable_card"),
        make_card(i18n$t("subsets_management"),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              make_textfield(i18n = i18n, ns = ns, label = "name", id = "subset_name", width = "300px"),
              div(shiny.fluent::PrimaryButton.shinyInput(ns("add_subset"), i18n$t("add")), style = "margin-top:38px;"),
              style = "position:relative; z-index:1; width:500px;"
            ),
            div(DT::DTOutput(ns("subsets_datatable")), style = "margin-top:-30px; z-index:2"),
            div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("save_subsets_management"), i18n$t("save")),
                shiny.fluent::DefaultButton.shinyInput(ns("delete_selection"), i18n$t("delete_selection"))
              ),
              style = "position:relative; z-index:2; margin-top:-30px;"
            )
          )
        )
      )
    ),
    
    # --- --- --- --- --- -- -
    # Subset persons card ----
    # --- --- --- --- --- -- -
    
    shinyjs::hidden(
      div(
        id = ns("subsets_persons_card"),
        make_card(i18n$t("subset_persons"),
          div(
            div(
              make_combobox(i18n = i18n, ns = ns, label = "subset", id = "persons_selected_subset", width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
              style = "position:relative; z-index:1; width:500px;"
            ),
            div(DT::DTOutput(ns("subset_persons_datatable")), style = "margin-top:-30px; z-index:2"),
            shinyjs::hidden(
              div(
                id = ns("subset_persons_buttons"),
                shiny.fluent::DefaultButton.shinyInput(ns("delete_selected_persons"), i18n$t("delete_selection")),
                style = "position:relative; z-index:1; margin-top:-30px; width:500px;"
              )
            ), br(),
            conditionalPanel(condition = "input.persons_selected_subset == null | input.persons_selected_subset == ''", ns = ns, br())
          )
        ),
        conditionalPanel(condition = "input.persons_selected_subset != null & input.persons_selected_subset != ''", ns = ns,
          make_card(i18n$t("add_persons_to_subset"),
            div(
              div(DT::DTOutput(ns("subset_add_persons_datatable")), style = "margin-top:-30px; z-index:2"),
              shinyjs::hidden(
                div(
                  id = ns("subset_add_persons_buttons"),
                  shiny.fluent::PrimaryButton.shinyInput(ns("subset_add_persons"), i18n$t("add")),
                  style = "position:relative; z-index:1; margin-top:-30px; width:500px;"
                )
              ), br()
            )  
          )
        )
      )
    ),
    
    # --- --- --- --- --- --- --
    # Subset edit code card ----
    # --- --- --- --- --- --- --
    
    shinyjs::hidden(
      div(
        id = ns("subsets_edit_code_card"),
        make_shiny_ace_card(i18n$t("edit_subset_code"),
          div(
            shiny.fluent::Stack(
              tokens = list(childrenGap = 5),
              make_combobox(i18n = i18n, ns = ns, label = "subset", id = "code_selected_subset",
                width = "300px", allowFreeform = FALSE, multiSelect = FALSE), br(),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                div(shiny.fluent::Toggle.shinyInput(ns("hide_editor"), value = FALSE), style = "margin-top:9px;"),
                div(i18n$t("hide_editor"), style = "font-weight:bold; margin-top:9px; margin-right:30px;")
              )
            ),
            conditionalPanel(condition = "input.hide_editor == false", ns = ns,
              div(shinyAce::aceEditor(
                ns("ace_edit_code"), "", mode = "r", 
                code_hotkeys = list(
                  "r", list(
                    run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
                    run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
                    save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S")
                  )
                ),
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000))
            ),
            shiny.fluent::Stack(
              tokens = list(childrenGap = 5),
              conditionalPanel(condition = "input.hide_editor == true", ns = ns, br()),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("execute_code"), i18n$t("run_code")),
                shiny.fluent::DefaultButton.shinyInput(ns("save_code"), i18n$t("save"))
              ), br(),
              div(textOutput(ns("datetime_code_execution")), style = "color:#878787;"), br(),
              div(verbatimTextOutput(ns("code_result")), 
                style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
            )
          )
        )
      )
    ), br()
  )
}
    
#' my_subsets Server Functions
#'
#' @noRd 
mod_my_subsets_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), 
  i18n = character(), language = "en", perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Close message bar
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("subsets_datatable_card", "subsets_persons_card", "subsets_edit_code_card")
    show_or_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    
    # --- --- --- --- --- -
    # Show message bar ----
    # --- --- --- --- --- -
    
    # This allows to show message in multiple pages at the same time (eg when loading a dataset in Studies page, render message bar in Subsets page)
    
    observeEvent(r$show_message_bar, show_message_bar(output, r$show_message_bar$message, r$show_message_bar$type, i18n = i18n, ns = ns))
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    observeEvent(input$help, if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r$help_my_subsets_open_panel <- TRUE)
    observeEvent(input$hide_panel, r$help_my_subsets_open_panel <- FALSE)
    
    r$help_my_subsets_open_panel_light_dismiss <- TRUE
    observeEvent(input$show_modal, r$help_my_subsets_open_modal <- TRUE)
    observeEvent(input$hide_modal, {
      r$help_my_subsets_open_modal <- FALSE
      r$help_my_subsets_open_panel_light_dismiss <- TRUE
    })
    
    observeEvent(shiny.router::get_page(), {
      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - ", id, " - observer shiny_router::change_page"))
      
      # Close help pages when page changes
      r$help_my_subsets_open_panel <- FALSE
      r$help_my_subsets_open_modal <- FALSE
    })
    
    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_my_subsets_page_", i)]] <- Sys.time())
    })
    
    help_my_subsets(output = output, r = r, id = id, language = language, i18n = i18n, ns = ns)
    
    observeEvent(input$copy_code_1, r$help_my_subsets_copy_code_1 <- Sys.time())
    
    # --- --- --- --- --- --- --- --
    # When a dataset is selected ----
    # --- --- --- --- --- --- --- --
    
    observeEvent(r$selected_dataset, {
      
      if (debug) print(paste0(Sys.time(), " - mod_subsets - observer r$selected_dataset"))
      
      shinyjs::show("choose_a_study_card")
      sapply(c("subsets_datatable_card", "subsets_datatable_card_forbidden", "subsets_persons_card", "subsets_persons_card_forbidden",
        "subsets_edit_code_card", "subsets_edit_code_card_forbidden", "menu"), shinyjs::hide)
      
      # Reset fields
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_subset", options = list(), value = NULL)
      shiny.fluent::updateComboBox.shinyInput(session, "persons_selected_subset", options = list(), value = NULL)
      shinyAce::updateAceEditor(session, "ace_edit_code", value = "")
    })
    
    # --- --- --- --- --- --- ---
    # When a study is selected ----
    # --- --- --- --- --- --- ---
    
    observeEvent(m$selected_study, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer m$selected_study"))
      
      req(!is.na(m$selected_study))
      
      # Show first card & hide "choose a study" card
      shinyjs::hide("choose_a_study_card")
      shinyjs::show("menu")
      if (length(input$current_tab) == 0){
        if ("subsets_datatable_card" %in% r$user_accesses) shinyjs::show("subsets_datatable_card")
        else shinyjs::show("subsets_datatable_card_forbidden")
      }
      else {
        if (input$current_tab %in% r$user_accesses) shinyjs::show(input$current_tab)
        else shinyjs::show(paste0(input$current_tab, "_forbidden"))
      }
      
      # Reset fields
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_subset", options = list(), value = NULL)
      shiny.fluent::updateComboBox.shinyInput(session, "persons_selected_subset", options = list(), value = NULL)
      shinyAce::updateAceEditor(session, "ace_edit_code", value = "")
      
      if (length(r$subset_persons_datatable_proxy) > 0){
        DT::replaceData(r$subset_persons_datatable_proxy,
          tibble::tibble(id = integer(), subset_id = integer(), person_id = integer(),
            creator_id = factor(), datetime = character(), deleted = integer(), modified = logical()), 
          resetPaging = FALSE, rownames = FALSE)
      }
      if (length(r$subset_add_persons_datatable_proxy) > 0) DT::replaceData(r$subset_add_persons_datatable_proxy, tibble::tibble(person_id = integer()), resetPaging = FALSE, rownames = FALSE)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_subsets - observer m$selected_study"))
    })
    
    # --- --- --- --- --- -
    # Update dropdowns ----
    # --- --- --- --- --- -
    
    observeEvent(m$subsets, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer m$subsets"))
      
      options <- convert_tibble_to_list(m$subsets %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_subset", options = options)
      shiny.fluent::updateComboBox.shinyInput(session, "persons_selected_subset", options = options)
    })
    
    # --- --- --- --- -- -
    # Create a subset ----
    # --- --- --- --- -- -
    
    observeEvent(input$add_subset, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$add_subset"))
      
      new_data <- list()
      new_data$name <- coalesce2(type = "char", x = input$subset_name)
      new_data$subset_name <- new_data$name
      new_data$description <- ""
      new_data$study <- m$selected_study
      
      add_settings_new_data(session = session, output = output, r = r, d = d, m = m, i18n = i18n, id = "my_subsets", 
        data = new_data, table = "subsets", required_textfields = "subset_name", req_unique_values = "name")
      
      # Reload datatable
      m$subsets_temp <- m$subsets %>% dplyr::filter(study_id == m$selected_study) %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_subsets - observer input$add_subset"))
    })
    
    # --- --- --- --- --- ---
    # Subsets management ----
    # --- --- --- --- --- ---
    
    # Action buttons for each tab / page
    action_buttons <- c("sub_datatable", "edit_code", "delete")
    
    subsets_management_editable_cols <- c("name")
    subsets_management_sortable_cols <- c("id", "name", "description", "study_id", "creator_id", "datetime")
    subsets_management_column_widths <- c("id" = "80px", "datetime" = "130px", "action" = "100px", "creator_id" = "200px")
    subsets_management_centered_cols <- c("id", "creator", "datetime", "action")
    subsets_management_searchable_cols <- c("name", "description", "creator_id")
    subsets_management_factorize_cols <- c("study_id", "creator_id")
    subsets_management_hidden_cols <- c("id", "description", "study_id","deleted", "modified")
    subsets_management_col_names <- get_col_names("subsets", i18n)
    
    # Load data for datatable
    observeEvent(m$subsets, {
      
      if (debug) print(paste0(Sys.time(), " - mod_subsets - observer m$subsets"))
      
      m$subsets_temp <- m$subsets %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
    })
    
    # Reload datatable
    observeEvent(m$subsets_temp, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer m$subsets_temp"))

      # Reload datatable_temp variable
      if (nrow(m$subsets_temp) == 0) m$subsets_datatable_temp <- tibble::tibble(id = integer(), name = character(), description = character(), study_id = factor(),
        creator_id = factor(), datetime = character(), deleted = integer(), modified = logical(), action = character())

      if (nrow(m$subsets_temp) > 0) m$subsets_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
        table = "subsets", factorize_cols = subsets_management_factorize_cols, action_buttons = action_buttons, data_input = m$subsets_temp)

      # Reload data of datatable
      if (length(m$subsets_datatable_proxy) == 0){
        
        # Render datatable
        
        render_datatable(output = output, ns = ns, i18n = i18n, data = m$subsets_datatable_temp,
          output_name = "subsets_datatable", col_names = get_col_names("subsets", i18n),
          editable_cols = subsets_management_editable_cols, sortable_cols = subsets_management_sortable_cols, centered_cols = subsets_management_centered_cols,
          column_widths = subsets_management_column_widths, searchable_cols = subsets_management_searchable_cols,
          filter = TRUE, factorize_cols = subsets_management_factorize_cols, hidden_cols = subsets_management_hidden_cols,
          selection = "multiple")
        
        # Create a proxy for datatable
        
        m$subsets_datatable_proxy <- DT::dataTableProxy("subsets_datatable", deferUntilFlush = FALSE)
      }
      
      if (length(m$subsets_datatable_proxy) > 0) DT::replaceData(m$subsets_datatable_proxy, m$subsets_datatable_temp, resetPaging = FALSE, rownames = FALSE)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_subsets - observer m$subsets_temp"))
    })
    
    # Updates on datatable data
    observeEvent(input$subsets_datatable_cell_edit, {

      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$subsets_datatable_cell_edit"))

      edit_info <- input$subsets_datatable_cell_edit
      m$subsets_temp <- DT::editData(m$subsets_temp, edit_info, rownames = FALSE)

      # Store that this row has been modified
      m$subsets_temp[[edit_info$row, "modified"]] <- TRUE
    })
    
    # Save updates
    observeEvent(input$save_subsets_management, {

      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$save_subsets_management"))

      req(nrow(m$subsets %>% dplyr::filter(study_id == m$selected_study)) > 0)

      save_settings_datatable_updates(output = output, r = r, m = m, ns = ns,
        table = "subsets", r_table = "subsets", i18n = i18n, duplicates_allowed = FALSE)

      # Update sidenav dropdown with the new study
      r$reload_subsets <- Sys.time()
    })
    
    # Delete a row in datatable
    
    subset_delete_prefix <- "subset"
    subset_dialog_title <- "subsets_delete"
    subset_dialog_subtext <- "subsets_delete_subtext"
    subset_react_variable <- "delete_confirm"
    subset_table <- "subsets"
    subset_id_var_sql <- "id"
    subset_id_var_r <- "delete_subset"
    subset_delete_message <- "subsets_deleted"
    subset_reload_variable <- "reload_subsets"
    subset_information_variable <- "subset_deleted"
    subset_delete_variable <- paste0(subset_delete_prefix, "_open_dialog")

    delete_element(r = r, m = m, input = input, output = output, session = session, ns = ns, i18n = i18n,
      delete_prefix = subset_delete_prefix, dialog_title = subset_dialog_title, dialog_subtext = subset_dialog_subtext,
      react_variable = subset_react_variable, table = subset_table, id_var_sql = subset_id_var_sql, id_var_r = subset_id_var_r,
      delete_message = subset_delete_message, translation = TRUE, reload_variable = subset_reload_variable,
      information_variable = subset_information_variable)

    # Delete one row (with icon on DT)
    
    observeEvent(input$deleted_pressed, {

      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$deleted_pressed"))

      r$delete_subset <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
      r[[subset_delete_variable]] <- TRUE

      # Reload datatable (to unselect rows)
      DT::replaceData(m$subsets_datatable_proxy, m$subsets_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    })
    
    # Delete multiple rows (with "Delete selection" button)
    
    observeEvent(input$delete_selection, {

      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$delete_selection"))

      req(length(input$subsets_datatable_rows_selected) > 0)

      r$delete_subset <- m$subsets_temp[input$subsets_datatable_rows_selected, ] %>% dplyr::pull(id)
      r[[subset_delete_variable]] <- TRUE
    })
    
    observeEvent(r$reload_subsets, {

      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer r$reload_subsets"))

      m$subsets_temp <- m$subsets %>% dplyr::filter(study_id == m$selected_study) %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)

      # Reset selected subset
      m$selected_subset <- NA_integer_
    })
    
    # --- --- --- --- --- -
    # Edit subset code ----
    # --- --- --- --- --- -
    
    # Button "Edit code" is clicked on the datatable
    observeEvent(input$edit_code, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$edit_code"))
      
      # Get link_id variable, to update code editor
      link_id <- as.integer(substr(input$edit_code, nchar("edit_code_") + 1, nchar(input$edit_code)))
      
      options <- convert_tibble_to_list(m$subsets %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = link_id, text = m$subsets %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_subset", options = options, value = value)
      shiny.fluent::updateComboBox.shinyInput(session, "persons_selected_subset", options = options, value = value)
      
      # Reload datatable (to unselect rows)
      DT::replaceData(m$subsets_datatable_proxy, m$subsets_datatable_temp, resetPaging = FALSE, rownames = FALSE)
      
      # Set current pivot to edit_code_card
      shinyjs::runjs(glue::glue("$('#{id}-subsets_pivot button[name=\"{i18n$t('edit_subset_code')}\"]').click();"))
    })
    
    observeEvent(input$code_selected_subset, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_subsets - observer input$code_selected_subset"))
      
      if (length(input$code_selected_subset) > 1) link_id <- input$code_selected_subset$key
      else link_id <- input$code_selected_subset
      
      if (length(input$persons_selected_subset) > 0){
        if (length(input$persons_selected_subset) > 1) subset_persons_link_id <- input$persons_selected_subset$key
        else subset_persons_link_id <- input$persons_selected_subset
      }
      else subset_persons_link_id <- 0L

      if (link_id != subset_persons_link_id){
        options <- convert_tibble_to_list(m$subsets %>% dplyr::arrange(name), key_col = "id", text_col = "name")
        value <- list(key = link_id, text = m$subsets %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
        shiny.fluent::updateComboBox.shinyInput(session, "persons_selected_subset", options = options, value = value)
      }
      
      # Get code from database
      code <- r$code %>% dplyr::filter(category == "subset" & link_id == !!link_id) %>% dplyr::pull(code)
      
      shinyAce::updateAceEditor(session, "ace_edit_code", value = code)
      output$code_result <- renderText("")
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_subsets - observer input$code_selected_subset"))
    })
    
    # When save button is clicked, or CTRL+C or CMD+C is pushed
    
    observeEvent(input$save_code, {
      if (debug) print(paste0(Sys.time(), " - mod_subsets - observer input$save_code"))
      r$subset_code_save <- Sys.time()
    })
    
    observeEvent(input$ace_edit_code_save, {
      if (debug) print(paste0(Sys.time(), " - mod_subsets - observer input$ace_edit_code_save"))
      r$subset_code_save <- Sys.time()
    })
    
    observeEvent(r$subset_code_save, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_subsets - observer r$subset_code_save"))
      
      req(input$code_selected_subset)
      
      if (length(input$code_selected_subset) > 1) link_id <- input$code_selected_subset$key
      else link_id <- input$code_selected_subset
      
      subset_code <- input$ace_edit_code %>% stringr::str_replace_all("'", "''")
      
      subset_code_id <- r$code %>% dplyr::filter(category == "subset" & link_id == !!link_id) %>% dplyr::pull(id)
      
      sql <- glue::glue_sql("UPDATE code SET code = {subset_code} WHERE id = {subset_code_id}", .con = m$db)
      DBI::dbSendStatement(r$db, sql) -> query
      DBI::dbClearResult(query)
      
      r$code <- r$code %>% dplyr::mutate(code = dplyr::case_when(id == subset_code_id ~ subset_code, TRUE ~ code)) 
      
      # Update datetime in subsets table

      new_datetime <- as.character(Sys.time())
      sql <- glue::glue_sql("UPDATE subsets SET datetime = {new_datetime} WHERE id = {link_id}", .con = m$db)
      DBI::dbSendStatement(m$db, sql) -> query
      DBI::dbClearResult(query)
      m$subsets <- m$subsets %>% dplyr::mutate(datetime = dplyr::case_when(id == link_id ~ new_datetime, TRUE ~ datetime))

      # Notify user
      show_message_bar(output, "modif_saved", "success", i18n = i18n, ns = ns)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_subsets - observer r$subset_code_save"))
    })
    
    # When Execute code button is clicked
    
    observeEvent(input$execute_code, {
      if (debug) print(paste0(Sys.time(), " - mod_subsets - observer input$execute_code"))
      r$subset_execute_code <- input$ace_edit_code
      r$subset_execute_code_trigger <- Sys.time()
    })

    observeEvent(input$ace_edit_code_run_selection, {
      if (debug) print(paste0(Sys.time(), " - mod_subsets - observer input$ace_edit_code_run_selection"))
      if(!shinyAce::is.empty(input$ace_edit_code_run_selection$selection)) r$subset_execute_code <- input$ace_edit_code_run_selection$selection
      else r$subset_execute_code <- input$ace_edit_code_run_selection$line
      r$subset_execute_code_trigger <- Sys.time()
    })

    observeEvent(input$ace_edit_code_run_all, {
      if (debug) print(paste0(Sys.time(), " - mod_subsets - observer input$ace_edit_code_run_all"))
      r$subset_execute_code <- input$ace_edit_code
      r$subset_execute_code_trigger <- Sys.time()
    })

    observeEvent(r$subset_execute_code_trigger, {

      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_subsets - observer r$subset_execute_code_trigger"))

      if (length(input$code_selected_subset) > 1) link_id <- input$code_selected_subset$key
      else link_id <- input$code_selected_subset
      
      edited_code <- r$subset_execute_code %>% 
        stringr::str_replace_all("%dataset_id%", as.character(r$selected_dataset)) %>%
        stringr::str_replace_all("%subset_id%", as.character(link_id)) %>%
        stringr::str_replace_all("\r", "\n")

      # Variables to hide
      new_env_vars <- list("r" = NA)
      # Variables to keep
      for (var in c("d", "m", "i18n", "output", "ns")) new_env_vars[[var]] <- eval(parse(text = var))
      new_env <- rlang::new_environment(data = new_env_vars, parent = pryr::where("r"))
      
      options('cli.num_colors' = 1)
      
      # Capture console output of our code
      captured_output <- capture.output(
        tryCatch(eval(parse(text = edited_code), envir = new_env), error = function(e) print(e), warning = function(w) print(w)))
      
      # Restore normal value
      options('cli.num_colors' = NULL)
      
      output$datetime_code_execution <- renderText(format_datetime(Sys.time(), language))
      output$code_result <- renderText(paste(captured_output, collapse = "\n"))

      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_subsets - observer r$subset_execute_code"))
    })
    
    # --- --- --- --- --- --- --
    # Manage subset persons ----
    # --- --- --- --- --- --- --
    
    # Button "sub_datatable" is clicked on the datatable
    observeEvent(input$sub_datatable, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$sub_datatable"))
      
      # Get link_id variable, to update code editor
      link_id <- as.integer(substr(input$sub_datatable, nchar("sub_datatable_") + 1, nchar(input$sub_datatable)))
      
      options <- convert_tibble_to_list(m$subsets %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = link_id, text = m$subsets %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_subset", options = options, value = value)
      shiny.fluent::updateComboBox.shinyInput(session, "persons_selected_subset", options = options, value = value)
      
      # Reload datatable (to unselect rows)
      DT::replaceData(m$subsets_datatable_proxy, m$subsets_datatable_temp, resetPaging = FALSE, rownames = FALSE)
      
      # Set current pivot to subsets_persons_card
      shinyjs::runjs(glue::glue("$('#{id}-subsets_pivot button[name=\"{i18n$t('subset_persons')}\"]').click();"))
    })
    
    observeEvent(input$persons_selected_subset, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_subsets - observer input$persons_selected_subset"))
      
      if (length(input$persons_selected_subset) > 1) link_id <- input$persons_selected_subset$key
      else link_id <- input$persons_selected_subset
      
      if (length(input$code_selected_subset) > 0){
        if (length(input$code_selected_subset) > 1) subset_code_link_id <- input$code_selected_subset$key
        else subset_code_link_id <- input$code_selected_subset
      }
      else subset_code_link_id <- 0L
      
      if (link_id != subset_code_link_id){
        options <- convert_tibble_to_list(m$subsets %>% dplyr::arrange(name), key_col = "id", text_col = "name")
        value <- list(key = link_id, text = m$subsets %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
        shiny.fluent::updateComboBox.shinyInput(session, "code_selected_subset", options = options, value = value)
      }
      
      # Get persons for this subset
      
      sql <- glue::glue_sql("SELECT * FROM subset_persons WHERE deleted IS FALSE AND subset_id = {link_id}", .con = db)
      data <- DBI::dbGetQuery(m$db, sql)
      r$subset_persons <- data
      
      # Load data for datatable
      if (nrow(data) == 0) {
        
        r$subset_persons_temp <- tibble::tibble(id = integer(), subset_id = integer(), person_id = character(),
          creator_id = factor(), datetime = character(), deleted = integer(), modified = logical())
      }
      
      if (nrow(data) > 0){
        
        r$subset_persons_temp <- data %>% 
          dplyr::arrange(person_id) %>%
          dplyr::mutate(modified = FALSE) %>% dplyr::arrange(person_id) %>%
          dplyr::mutate_at("person_id", as.character) %>%
          dplyr::mutate_at("creator_id", as.factor)
      }
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_subsets - observer input$persons_selected_subset"))
    })
    
    subset_persons_sortable_cols <- c("person_id", "creator_id", "datetime")
    subset_persons_column_widths <- c("datetime" = "130px", "creator_id" = "200px")
    subset_persons_centered_cols <- c("creator", "datetime")
    subset_persons_searchable_cols <- c("person_id", "creator_id")
    subset_persons_factorize_cols <- c("creator_id")
    subset_persons_hidden_cols <- c("id", "subset_id", "deleted", "creator_id", "modified")
    subset_persons_col_names <- get_col_names("subset_persons", i18n)
    
    # Prepare data for subset_persons_datatable
    
    observeEvent(r$subset_persons_temp, {
      
      if (debug) print(paste0(Sys.time(), " - mod_subsets - observer r$subset_persons_temp"))
      
      # Subset persons datatable

      if (length(r$subset_persons_datatable_proxy) == 0){

        # Render datatable

        render_datatable(output = output, ns = ns, i18n = i18n, data = r$subset_persons_temp,
          output_name = "subset_persons_datatable", col_names = get_col_names("subset_persons", i18n),
          sortable_cols = subset_persons_sortable_cols, centered_cols = subset_persons_centered_cols,
          column_widths = subset_persons_column_widths, searchable_cols = subset_persons_searchable_cols,
          filter = TRUE, factorize_cols = subset_persons_factorize_cols, hidden_cols = subset_persons_hidden_cols,
          selection = "multiple")

        # Create a proxy for datatable

        r$subset_persons_datatable_proxy <- DT::dataTableProxy("subset_persons_datatable", deferUntilFlush = FALSE)
        
        # Hide no data div
        
        shinyjs::show("subset_persons_buttons")
      }

      if (length(r$subset_persons_datatable_proxy) > 0) DT::replaceData(r$subset_persons_datatable_proxy, r$subset_persons_temp, 
        resetPaging = FALSE, rownames = FALSE)
      
      # Update persons not already added in subset
      
      if (nrow(d$person) > 0) r$subset_add_persons <-
        d$person %>%
        dplyr::select(person_id) %>%
        dplyr::anti_join(r$subset_persons_temp %>% dplyr::select(person_id) %>% dplyr::mutate_at("person_id", as.integer), by = "person_id") %>%
        dplyr::mutate_at("person_id", as.character)
    })
    
    # Prepare data for subset_add_persons_datatable
    
    observeEvent(r$subset_add_persons, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer r$subset_add_persons"))
      
      # Subset persons datatable
      
      if (length(r$subset_add_persons_datatable_proxy) == 0){
        
        # Render datatable
        
        render_datatable(output = output, ns = ns, i18n = i18n, data = r$subset_add_persons,
          output_name = "subset_add_persons_datatable", col_names = get_col_names("subset_add_persons", i18n),
          filter = TRUE, sortable_cols = "person_id", searchable_cols = "person_id", selection = "multiple")
        
        # Create a proxy for datatable
        
        r$subset_add_persons_datatable_proxy <- DT::dataTableProxy("subset_add_persons_datatable", deferUntilFlush = FALSE)
        
        # Hide no data div
        
        shinyjs::show("subset_add_persons_buttons")
      }
      
      if (length(r$subset_add_persons_datatable_proxy) > 0) DT::replaceData(r$subset_add_persons_datatable_proxy, r$subset_add_persons, 
        resetPaging = FALSE, rownames = FALSE)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_my_subsets - observer r$subset_add_persons"))
    })

    # Delete rows in datatable

    r$subset_persons_open_dialog <- FALSE
    
    output$subset_persons_delete_confirm <- shiny.fluent::renderReact({
      
      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - output$subset_persons_delete_confirm"))
      
      shiny.fluent::Dialog(
        hidden = !r$subset_persons_open_dialog,
        onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('subset_persons_hide_dialog', Math.random()); }")),
        dialogContentProps = list(
          type = 0,
          title = i18n$t("subset_persons_delete"),
          closeButtonAriaLabel = "Close",
          subText = tagList(i18n$t("subset_persons_delete_subtext"), br(), br())
        ),
        modalProps = list(),
        shiny.fluent::DialogFooter(
          shiny.fluent::PrimaryButton.shinyInput(ns("subset_persons_delete_confirmed"), text = i18n$t("delete")),
          shiny.fluent::DefaultButton.shinyInput(ns("subset_persons_delete_canceled"), text = i18n$t("dont_delete"))
        )
      )
    })
    
    # Whether to close or not delete dialog box
    observeEvent(input$subset_persons_hide_dialog, {
      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$subset_persons_hide_dialog"))
      r$subset_persons_open_dialog <- FALSE 
    })
    observeEvent(input$subset_persons_delete_canceled, {
      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$subset_persons_delete_canceled"))
      r$subset_persons_open_dialog <- FALSE
    })
    
    # When the deletion is confirmed
    
    observeEvent(input$subset_persons_delete_confirmed, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$subset_persons_delete_confirmed"))
      
      if (length(input$persons_selected_subset) > 1) link_id <- input$persons_selected_subset$key
      else link_id <- input$persons_selected_subset
      
      remove_persons_from_subset(output = output, r = r, m = m, persons = r$delete_subset_persons, subset_id = link_id, i18n = i18n, ns = ns)
      
      r$subset_persons <- r$subset_persons %>% dplyr::anti_join(r$delete_subset_persons %>% dplyr::select(person_id), by = "person_id")
      
      r$subset_persons_open_dialog <- FALSE
      
      r$reload_subset_persons <- Sys.time()
    })

    # Delete multiple rows (with "Delete selection" button)

    observeEvent(input$delete_selected_persons, {

      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$delete_selected_persons"))

      req(length(input$subset_persons_datatable_rows_selected) > 0)

      r$delete_subset_persons <- r$subset_persons_temp[input$subset_persons_datatable_rows_selected, ] %>% 
        dplyr::select(person_id) %>% dplyr::mutate_at("person_id", as.integer)
      r$subset_persons_open_dialog <- TRUE
    })

    observeEvent(r$reload_subset_persons, {

      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer r$reload_subset_persons"))

      r$subset_persons_temp <- r$subset_persons %>% dplyr::mutate(modified = FALSE) %>% dplyr::arrange(person_id)
    })
    
    # Add persons to subset
    
    observeEvent(input$subset_add_persons, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer input$subset_add_persons"))
      
      if (length(input$persons_selected_subset) > 1) link_id <- input$persons_selected_subset$key
      else link_id <- input$persons_selected_subset
      
      # Selected persons
      persons <- r$subset_add_persons[input$subset_add_persons_datatable_rows_selected, ] %>% 
        dplyr::select(person_id) %>% dplyr::mutate_at("person_id", as.integer)
      
      # Add persons to subset
      add_persons_to_subset(output = output, r = r, m = m, persons = persons, subset_id = link_id, i18n = i18n, ns = ns)
      
      # Reload r$subset_persons
      r$reload_subset_add_persons <- Sys.time()
    })
    
    observeEvent(r$reload_subset_add_persons, {
      
      if (debug) print(paste0(Sys.time(), " - mod_my_subsets - observer r$reload_subset_add_persons"))
      
      if (length(input$persons_selected_subset) > 1) link_id <- input$persons_selected_subset$key
      else link_id <- input$persons_selected_subset
      
      sql <- glue::glue_sql("SELECT * FROM subset_persons WHERE deleted IS FALSE AND subset_id = {link_id}", .con = db)
      r$subset_persons <- DBI::dbGetQuery(m$db, sql)
      
      r$subset_persons_temp <- r$subset_persons %>% 
        dplyr::mutate(modified = FALSE) %>%
        dplyr::arrange(person_id) %>%
        dplyr::mutate_at("person_id", as.character) %>%
        dplyr::mutate_at("creator_id", as.factor)
    })
    
  })
}
