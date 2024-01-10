#' messages UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_messages_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  
  cards <- c("study_messages_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  })
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    shiny.fluent::reactOutput(ns("conversation_delete_confirm")),
    shiny.fluent::reactOutput(ns("message_delete_confirm")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "dataset_main", text = i18n$t("messages"))
    ), maxDisplayedItems = 3),
    
    # --- --- -- -- --
    # Pivot items ----
    # --- --- -- -- --
    
    shinyjs::hidden(
      div(id = ns("menu"),
        shiny.fluent::Pivot(
          id = ns("messages_pivot"),
          onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
          shiny.fluent::PivotItem(id = "study_messages_card", itemKey = "study_messages_card", headerText = i18n$t("study_messages"))
        )
      )
    ),
    div(
      id = ns("choose_a_dataset_card"),
      make_card("", div(shiny.fluent::MessageBar(i18n$t("choose_a_damatart_left_side"), messageBarType = 5), style = "margin-top:10px;"))
    ),
    forbidden_cards,
    
    # --- --- --- --- -- -- --
    # Study messages card ----
    # --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("study_messages_card"),
        div(id = ns("study_messages_content"),
          make_shiny_ace_card(i18n$t("messages"),
            div(
              shiny.fluent::Pivot(
                id = ns("study_messages_pivot"),
                onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-messages_current_tab', item.props.id)")),
                shiny.fluent::PivotItem(id = "all_messages", itemKey = "all_messages", headerText = i18n$t("all_messages")),
                shiny.fluent::PivotItem(id = "new_conversation", itemKey = "new_conversation", headerText = i18n$t("new_conversation"))
              ),
              div(
                id = ns("all_messages_div"),
                shiny.fluent::Stack(
                  tokens = list(childrenGap = 5),
                  DT::DTOutput(ns("study_conversations")),
                  uiOutput(ns("conversation_object")), br(),
                  shinyjs::hidden(
                    div(
                      id = ns("conversation_new_message_title_div"),
                      shiny.fluent::DefaultButton.shinyInput(ns("conversation_new_message"), i18n$t("new_message")),
                      shinyjs::hidden(
                        div(id = ns("conversation_hide_new_message_div"),
                          shiny.fluent::DefaultButton.shinyInput(ns("conversation_hide_new_message"), i18n$t("hide_editor")))
                      )
                    )
                  )
                ),
                shinyjs::hidden(
                  div(
                    id = ns("new_message_text_div_1"),
                      shinyAce::aceEditor(
                        ns("new_message_text"), "", mode = "markdown",
                        code_hotkeys = list("markdown", list(run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"))),
                        autoScrollEditorIntoView = TRUE, minLines = 10, maxLines = 1000
                      ), 
                    style = "width: 100%;")
                ),
                shiny.fluent::Stack(
                  tokens = list(childrenGap = 5),
                  shinyjs::hidden(
                    div(
                      id = ns("new_message_text_div_2"),
                      shiny.fluent::Stack(
                        tokens = list(childrenGap = 5),
                        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                          shiny.fluent::PrimaryButton.shinyInput(ns("send_new_message"), i18n$t("send")), " ",
                          shiny.fluent::DefaultButton.shinyInput(ns("preview_new_message"), i18n$t("preview")),
                          shiny.fluent::Toggle.shinyInput(ns("new_message_as_rmarkdown"), value = FALSE, style = "margin-top:6px;"),
                          div(class = "toggle_title", i18n$t("rmarkdown"), style = "padding-top:6px;")
                        ),
                        uiOutput(ns("new_message_preview"))
                      ),
                      style = "float:left; width:100%;"
                    )
                  ), br(), br(),
                  uiOutput(ns("selected_conversation"))
                )
              ),
              shinyjs::hidden(
                div(
                  id = ns("new_conversation_div"),
                  make_textfield(i18n = i18n, ns = ns, label = "object", id = "new_conversation_name"),
                  div(shinyAce::aceEditor(
                    ns("new_conversation_text"), "", mode = "markdown", 
                    code_hotkeys = list("markdown", list(run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"))),
                    autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000
                  ), style = "width: 100%;"),
                  shiny.fluent::Stack(
                    tokens = list(childrenGap = 5),
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                      shiny.fluent::PrimaryButton.shinyInput(ns("send_new_conversation"), i18n$t("send")), " ",
                      shiny.fluent::DefaultButton.shinyInput(ns("preview_new_conversation"), i18n$t("preview")),
                      shiny.fluent::Toggle.shinyInput(ns("new_conversation_as_rmarkdown"), value = FALSE, style = "margin-top:6px;"),
                      div(class = "toggle_title", i18n$t("rmarkdown"), style = "padding-top:6px;")
                    ),
                    uiOutput(ns("new_conversation_preview"))
                  )
                )
              )
            )
          )
        ),
        div(
          id = ns("choose_a_study_card_messages"),
          make_card("", div(shiny.fluent::MessageBar(i18n$t("choose_a_study_left_side"), messageBarType = 5), style = "margin-top:10px;"))
        )
      )
    ), br()
  )
}
    
#' messages Server Functions
#'
#' @noRd 
mod_messages_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), 
  i18n = character(), language = "en", perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - start"))
    
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    messages_timer <- reactiveTimer(10000, session)
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("study_messages_card")
    show_or_hide_cards(r = r, input = input, session = session, id = id, cards = cards)

    # --- --- --- --- --- -
    # Show message bar ----
    # --- --- --- --- --- -
    
    # This allows to show message in multiple pages at the same time (eg when loading a dataset in Studies page, render message bar in Subsets page)
    
    observeEvent(r$show_message_bar, show_message_bar(output, r$show_message_bar$message, r$show_message_bar$type, i18n = i18n, ns = ns))
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    observeEvent(input$help, if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r$help_messages_open_panel <- TRUE)
    observeEvent(input$hide_panel, r$help_messages_open_panel <- FALSE)
    
    r$help_messages_open_panel_light_dismiss <- TRUE
    observeEvent(input$show_modal, r$help_messages_open_modal <- TRUE)
    observeEvent(input$hide_modal, {
      r$help_messages_open_modal <- FALSE
      r$help_messages_open_panel_light_dismiss <- TRUE
    })
    
    # observeEvent(shiny.router::get_page(), {
    #   if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - ", id, " - observer shiny_router::change_page"))
    # 
    #   # Close help pages when page changes
    #   r$help_messages_open_panel <- FALSE
    #   r$help_messages_open_modal <- FALSE
    # })
    
    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_messages_page_", i)]] <- Sys.time())
    })
    
    help_messages(output = output, r = r, id = id, language = language, i18n = i18n, ns = ns)
    
    observeEvent(input$copy_code_1, r$help_messages_copy_code_1 <- Sys.time())
    
    # --- --- --- --- --- --- --- --
    # When a dataset is selected ----
    # --- --- --- --- --- --- --- --
    
    observeEvent(r$selected_dataset, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer r$selected_dataset"))
      
      # Show first card & hide "choose a dataset" card
      shinyjs::hide("choose_a_dataset_card")
      shinyjs::show("menu")
      if (length(input$current_tab) == 0){
        if ("study_messages_card" %in% r$user_accesses) shinyjs::show("study_messages_card")
        else shinyjs::show("study_messages_card_forbidden")
      }
      else {
        if (input$current_tab %in% r$user_accesses) shinyjs::show(input$current_tab)
        else shinyjs::show(paste0(input$current_tab, "_forbidden"))
      }
      
      # Hide messages card & reset fields
      sapply(c("choose_a_study_card_messages", "choose_a_study_card_description"), shinyjs::show)
      sapply(c("study_messages_content", "conversation_new_message_title_div", "new_message_text_div_1", "new_message_text_div_2", "studies_description_content"), shinyjs::hide)
      shiny.fluent::updateTextField.shinyInput(session, "new_conversation_name", value = "")
      shinyAce::updateAceEditor(session, "new_conversation_text", value = "")
      shinyAce::updateAceEditor(session, "new_message_text", value = "")
      output$conversation_object <- renderUI("")
      output$new_conversation_preview <- renderUI("")
      output$selected_conversation <- renderUI("")
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_messages - observer r$selected_dataset"))
    })
    
    # --- --- --- --- --- --- --- --
    # When a study is selected ----
    # --- --- --- --- --- --- --- --
    
    observeEvent(m$selected_study, {
      
      if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer r$selected_study"))
      
      req(!is.na(m$selected_study))
      # Show first card & hide "choose a dataset" card
      sapply(c("choose_a_study_card_messages", "choose_a_study_card_description"), shinyjs::hide)
      sapply(c("study_messages_content", "studies_description_content"), shinyjs::show)
      
      # Reset new conversation fields
      shiny.fluent::updateTextField.shinyInput(session, "new_conversation_name", value = "")
      shinyAce::updateAceEditor(session, "new_conversation_text", value = "")
      shinyAce::updateAceEditor(session, "new_message_text", value = "")
      output$conversation_object <- renderUI("")
      output$new_conversation_preview <- renderUI("")
      output$selected_conversation <- renderUI("")
      shinyjs::hide("conversation_new_message_title_div", "new_message_text_div_1", "new_message_text_div_2")
    })
    
    # --- --- --- -
    # Messages ----
    # --- --- --- -
      
      # --- --- --- --- --
      ## All messages ----
      # --- --- --- --- --

      observeEvent(m$selected_study, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer m$selected_study"))

        req(!is.na(m$selected_study))
        
        # All users of the study have access to the study conversations
        # Remove conversations deleted by the user (still visible for other users)

        sql <- glue::glue_sql(paste0(
          "SELECT m.id, c.id AS conversation_id, c.name AS conversation_name, m.message, m.filepath, m.creator_id, m.datetime, im.read, m.deleted ",
          "FROM messages m ",
          "INNER JOIN conversations c ON m.conversation_id = c.id ",
          "LEFT JOIN inbox_messages im ON m.id = im.message_id AND im.receiver_id = {r$user_id} AND im.deleted IS FALSE ",
          "WHERE category = 'study_message' AND m.study_id = {m$selected_study} ",
          "AND m.conversation_id NOT IN (",
          " SELECT DISTINCT(udc.conversation_id) FROM user_deleted_conversations udc WHERE udc.user_id = {r$user_id}",
          ")"), .con = r$db)

        r$study_messages <- DBI::dbGetQuery(r$db, sql) %>%
          tibble::as_tibble() %>% dplyr::mutate_at("datetime", as.POSIXct) %>% dplyr::arrange(dplyr::desc(datetime))

        if (nrow(r$study_messages) > 0) r$study_conversations <- r$study_messages %>%
          dplyr::group_by(conversation_id) %>%
          dplyr::summarize(conversation_name = max(conversation_name), datetime = max(datetime), read = min(read)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(unread_messages = dplyr::case_when(read == 1 ~ 0, TRUE ~ 1)) %>%
          dplyr::select(-read) %>%
          dplyr::arrange(dplyr::desc(unread_messages), dplyr::desc(datetime)) %>%
          dplyr::relocate(unread_messages, .after = "datetime") %>%
          dplyr::mutate_at("datetime", as.character) %>%
          dplyr::mutate(
            datetime = stringr::str_replace_all(datetime, "T|Z", ""),
            action = as.character(shiny::actionButton(ns("delete_conversation_%conversation_id%"), "", icon = icon("trash-alt"),
              onclick = paste0("Shiny.setInputValue('", id, "-conversation_deletion', this.id, {priority: 'event'})")))
          ) %>%
          dplyr::mutate(action = stringr::str_replace_all(action, "%conversation_id%", as.character(conversation_id)))

        if (nrow(r$study_messages) == 0) r$study_conversations <- tibble::tibble(conversation_id = integer(),
          conversation_name = character(), datetime = character(), unread_messages = integer(), action = character())
        
        r$study_conversations_temp <- r$study_conversations %>% dplyr::mutate(modified = FALSE)
        
        r$study_reload_conversations_datatable_clear_selection <- "all"
        r$study_reload_conversations_datatable <- Sys.time()
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_messages - observer m$selected_study"))
      })
    
      conv_searchable_cols <- c("conversation_name", "unread_messages")
      conv_factorize_cols <- c("unread_messages")
      conv_column_widths <- c("datetime" = "150px", "unread_messages" = "150px", "action" = "80px")
      conv_sortable_cols <- c("id", "conversation_name", "datetime", "unread_messages")
      conv_centered_cols <- c("id", "datetime", "unread_messages", "action")
      conv_col_names <- get_col_names(table_name = "study_conversations", i18n = i18n)
      conv_hidden_cols <- c("conversation_id", "modified", "unread_messages")
    
      # Reload conversations
      
      observeEvent(r$study_reload_conversations_datatable, {
        
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer r$study_reload_conversations_datatable"))
        
        # Render datatable
        if (length(r$study_conversations_datatable_proxy) == 0){
          render_datatable(output = output, ns = ns, i18n = i18n, data = r$study_conversations_temp,
            output_name = "study_conversations", col_names = conv_col_names,
            sortable_cols = conv_sortable_cols, centered_cols = conv_centered_cols, column_widths = conv_column_widths,
            searchable_cols = conv_searchable_cols, filter = TRUE, factorize_cols = conv_factorize_cols, 
            hidden_cols = conv_hidden_cols, bold_rows = c("unread_messages" = 1))

          # Create a proxy for datatatable
          r$study_conversations_datatable_proxy <- DT::dataTableProxy("study_conversations", deferUntilFlush = FALSE)
        }

        if (length(r$study_conversations_datatable_proxy) > 0) DT::replaceData(r$study_conversations_datatable_proxy,
          r$study_conversations_temp, resetPaging = FALSE, rownames = FALSE, clearSelection = r$study_reload_conversations_datatable_clear_selection)
      })
      
      # Timer to update conversations & messages
      
      observe({
        
        # if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer to update messages"))
        
        messages_timer()
        req(!is.na(m$selected_study))
        
        sql <- glue::glue_sql(paste0(
          "SELECT m.id, c.id AS conversation_id, c.name AS conversation_name, m.message, m.filepath, m.creator_id, m.datetime, im.read, m.deleted ",
          "FROM messages m ",
          "INNER JOIN conversations c ON m.conversation_id = c.id ",
          "LEFT JOIN inbox_messages im ON m.id = im.message_id AND im.receiver_id = {r$user_id} AND im.deleted IS FALSE ",
          "WHERE category = 'study_message' AND m.study_id = {m$selected_study} ",
          "AND m.conversation_id NOT IN (",
          " SELECT DISTINCT(udc.conversation_id) FROM user_deleted_conversations udc WHERE udc.user_id = {r$user_id}",
          ")"), .con = r$db)
        
        study_messages <- DBI::dbGetQuery(r$db, sql) %>%
          tibble::as_tibble() %>% dplyr::mutate_at("datetime", as.POSIXct) %>% dplyr::arrange(dplyr::desc(datetime))
        
        if (study_messages %>% dplyr::select(id, deleted) %>% dplyr::anti_join(r$study_messages %>% dplyr::select(id, deleted), by = c("id", "deleted")) %>% nrow() > 0 & nrow(study_messages) > 0){
          
          r$study_messages <- study_messages
          
          r$study_conversations <- r$study_messages %>%
            dplyr::group_by(conversation_id) %>%
            dplyr::summarize(conversation_name = max(conversation_name), datetime = max(datetime), read = min(read)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(unread_messages = dplyr::case_when(read == 1 ~ 0, TRUE ~ 1)) %>%
            dplyr::select(-read) %>%
            dplyr::arrange(dplyr::desc(unread_messages), dplyr::desc(datetime)) %>%
            dplyr::relocate(unread_messages, .after = "datetime") %>%
            dplyr::mutate_at("datetime", as.character) %>%
            dplyr::mutate(datetime = stringr::str_replace_all(datetime, "T|Z", "")) %>%
            dplyr::mutate(
              datetime = stringr::str_replace_all(datetime, "T|Z", ""),
              action = as.character(shiny::actionButton(ns("delete_conversation_%conversation_id%"), "", icon = icon("trash-alt"),
                onclick = paste0("Shiny.setInputValue('", id, "-conversation_deletion', this.id, {priority: 'event'})")))
            ) %>%
            dplyr::mutate(action = stringr::str_replace_all(action, "%conversation_id%", as.character(conversation_id)))
          
          r$study_conversations_temp <- r$study_conversations %>% dplyr::mutate(modified = FALSE)
          
          # Unselect rows
          r$study_reload_conversations_datatable_clear_selection <- "all"
          
          # Reload datatable
          r$study_reload_conversations_datatable <- Sys.time()
          
          r$study_reload_conversation <- Sys.time()
          r$study_reload_conversation_type <- "refresh_conversation"
        }
        
        # if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_messages - observer to update messages"))
      })
      
      # --- --- --- --- --- --- ---
      ## Delete a conversation ----
      # --- --- --- --- --- --- ---
      
      r$study_delete_conversation_open_dialog <- FALSE
      
      output$conversation_delete_confirm <- shiny.fluent::renderReact({
        
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - output$conversation_delete_confirm"))
        
        shiny.fluent::Dialog(
          hidden = !r$study_delete_conversation_open_dialog,
          onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('study_delete_conversation_hide_dialog', Math.random()); }")),
          dialogContentProps = list(
            type = 0,
            title = i18n$t("study_delete_conversation_title"),
            closeButtonAriaLabel = "Close",
            subText = tagList(i18n$t("study_delete_conversation_subtext"), br(), br())
          ),
          modalProps = list(),
          shiny.fluent::DialogFooter(
            shiny.fluent::PrimaryButton.shinyInput(ns("study_delete_conversation_delete_confirmed"), text = i18n$t("delete")),
            shiny.fluent::DefaultButton.shinyInput(ns("study_delete_conversation_delete_canceled"), text = i18n$t("dont_delete"))
          )
        )
      })
      
      # Whether to close or not delete dialog box
      observeEvent(input$study_delete_conversation_hide_dialog, {
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer input$study_delete_conversation_hide_dialog"))
        r$study_delete_conversation_open_dialog <- FALSE 
      })
      observeEvent(input$study_delete_conversation_delete_canceled, {
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer input$study_delete_conversation_delete_canceled"))
        r$study_delete_conversation_open_dialog <- FALSE
      })
      
      # When the deletion is confirmed
      
      observeEvent(input$study_delete_conversation_delete_confirmed, {
        
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer input$study_delete_conversation_delete_confirmed"))
        
        link_id <- substr(input$conversation_deletion, nchar(paste0(id, "-delete_conversation_")) + 1, nchar(input$conversation_deletion)) %>% as.integer()
        
        new_data <- tibble::tibble(
          id = get_last_row(r$db, "user_deleted_conversations") + 1, conversation_id = link_id,
          user_id = r$user_id, datetime = as.character(Sys.time()))
        DBI::dbAppendTable(r$db, "user_deleted_conversations", new_data)
        
        r$study_conversations <- r$study_conversations %>% dplyr::filter(conversation_id != link_id)
        r$study_conversations_temp <- r$study_conversations %>% dplyr::mutate(modified = FALSE)
        
        r$study_delete_conversation_open_dialog <- FALSE
        
        r$study_reload_conversations_datatable <- Sys.time()
        
        output$conversation_object <- renderUI("")
        output$selected_conversation <- renderUI("")
        shinyjs::hide("conversation_new_message_title_div", "new_message_text_div_1", "new_message_text_div_2")
      })
      
      observeEvent(input$conversation_deletion, {
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer input$conversation_deletion"))
        r$study_delete_conversation_open_dialog <- TRUE
      })
      
      # --- --- --- --- --- --
      ## Delete a message ----
      # --- --- --- --- --- --
      
      r$study_delete_message_open_dialog <- FALSE
      
      output$message_delete_confirm <- shiny.fluent::renderReact({
        
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - output$message_delete_confirm"))
        
        shiny.fluent::Dialog(
          hidden = !r$study_delete_message_open_dialog,
          onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('study_delete_message_hide_dialog', Math.random()); }")),
          dialogContentProps = list(
            type = 0,
            title = i18n$t("study_delete_message_title"),
            closeButtonAriaLabel = "Close",
            subText = tagList(i18n$t("study_delete_message_subtext"), br(), br())
          ),
          modalProps = list(),
          shiny.fluent::DialogFooter(
            shiny.fluent::PrimaryButton.shinyInput(ns("study_delete_message_delete_confirmed"), text = i18n$t("delete")),
            shiny.fluent::DefaultButton.shinyInput(ns("study_delete_message_delete_canceled"), text = i18n$t("dont_delete"))
          )
        )
      })
      
      # Whether to close or not delete dialog box
      observeEvent(input$study_delete_message_hide_dialog, {
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer input$study_delete_message_hide_dialog"))
        r$study_delete_message_open_dialog <- FALSE 
      })
      observeEvent(input$study_delete_message_delete_canceled, {
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer input$study_delete_message_delete_canceled"))
        r$study_delete_message_open_dialog <- FALSE
      })
      
      # When the deletion is confirmed
      
      observeEvent(input$study_delete_message_delete_confirmed, {
        
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer input$study_delete_message_delete_confirmed"))
        
        link_id <- substr(input$message_deletion, nchar(paste0(id, "-delete_message_")) + 1, nchar(input$message_deletion)) %>% as.integer()
        print(link_id)
        
        sql <- glue::glue_sql("UPDATE messages SET deleted = TRUE WHERE id = {link_id}", .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql)
        DBI::dbClearResult(query)
        
        r$study_messages <- r$study_messages %>% dplyr::mutate(deleted = ifelse(id == link_id, TRUE, deleted))
        r$study_messages_temp <- r$study_messages %>% dplyr::mutate(modified = FALSE)
        
        r$study_delete_message_open_dialog <- FALSE
        
        # Reload conversation
        r$study_reload_conversation <- Sys.time()
        r$study_reload_conversation_type <- "refresh_conversation"
      })
      
      observeEvent(input$message_deletion, {
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer input$message_deletion"))
        r$study_delete_message_open_dialog <- TRUE
      })
      
      # --- --- --- --- --- --- --- --- --- -
      ## When a conversation is selected ----
      # --- --- --- --- --- --- --- --- --- -
      
      observeEvent(input$study_conversations_rows_selected, {
        
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer input$study_conversations_rows_selected"))

        # Show conversation messages
        
        r$study_reload_conversation <- Sys.time()
        r$study_reload_conversation_type <- "select_conversation"
        r$study_selected_conversation <- r$study_conversations_temp[input$study_conversations_rows_selected, ]
        
        # Mark conversation as read
        r$study_conversations_temp <- r$study_conversations_temp %>%
          dplyr::mutate(unread_messages = dplyr::case_when(
            conversation_id == r$study_conversations_temp[input$study_conversations_rows_selected, "conversation_id"] %>% dplyr::pull() ~ 0,
            TRUE ~ unread_messages
          ))
        
        # Reload datatable
        r$study_reload_conversations_datatable_clear_selection <- "none"
        r$study_reload_conversations_datatable <- Sys.time()
      })

      observeEvent(r$study_reload_conversation, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer r$study_reload_conversation"))

        req(r$study_selected_conversation)
        
        conversation_messages <- r$study_messages %>% dplyr::inner_join(r$study_selected_conversation %>% dplyr::select(conversation_id), by = "conversation_id") %>%
          dplyr::arrange(dplyr::desc(datetime))

        conversation_messages_ui <- tagList()

        colours <- c("#f94144","#f3722c","#f8961e","#f9844a","#f9c74f","#90be6d","#43aa8b","#4d908e","#577590","#277da1")
        conversation_messages_users <- conversation_messages %>% dplyr::group_by(creator_id) %>% dplyr::slice(1) %>% dplyr::ungroup() %>%
          dplyr::select(creator_id) %>%
          dplyr::left_join(r$users %>% dplyr::transmute(creator_id = id, initials = paste0(toupper(substr(firstname, 1, 1)), toupper(substr(lastname, 1, 1)))), by = "creator_id") %>%
          dplyr::mutate(colour = "")
        conversation_messages_users$colour <- sample(colours, nrow(conversation_messages_users), replace = TRUE, prob = rep(0.1, 10))

        for (i in 1:nrow(conversation_messages)){

          study_message <- conversation_messages[i, ]

          if (study_message$deleted) message_div <- div(tags$em(i18n$t("deleted_message")))
          else message_div <- div(HTML(study_message$message))

          if (study_message$filepath != "" & !study_message$deleted){
            
            fail_load_message <- TRUE
            
            tryCatch({
              message_div <- 
                includeMarkdown(study_message$filepath) %>%
                gsub('(<code class="[^"]*)"', '\\1 code_highlight"', .) %>%
                stringr::str_replace_all("<code>", "<code class=\"code_highlight\">") %>%
                HTML()
              
              fail_load_message <- FALSE
            }, error = function(e) report_bug(r = r, output = output, error_message = "fail_load_message",
              error_name = paste0(id, " - load message - message_id = ", study_message$id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns),
              warning = function(w) report_bug(r = r, output = output, error_message = "fail_load_message",
                error_name = paste0(id, " - load message - message_id = ", study_message$id), category = "Error", error_report = w, i18n = i18n, ns = ns))
            
            if (fail_load_message) message_div <- div(
              tags$em(i18n$t("fail_load_message_show_raw_message")), br(), br(), HTML(study_message$message))
          }

          date <- study_message$datetime %>% as.Date()
          if (language == "fr") date <- date %>% format(format = "%d-%m-%Y")
          
          if (!study_message$deleted & study_message$creator_id == r$user_id) deletion_div <- div(
              actionButton(ns(paste0("delete_message_", study_message$id)), "X", style = "padding:0px 5px 0px 5px;",
              onclick = paste0("Shiny.setInputValue('", id, "-message_deletion', this.id, {priority: 'event'})")), 
            style = "float:right; margin-top:-10px; margin-right:-10px;")
          else deletion_div <- ""

          if (study_message$creator_id == r$user_id){
            study_message_ui <- div(
              div(
                deletion_div,
                div(
                  div(paste0(date, ", ", format(study_message$datetime, "%H:%M"))),
                  style = "font-size:12px; margin-bottom:10px; color:#878787"
                ),
                message_div,
                style = "background-color:#E6F1F8; margin-top:10px; padding:15px; border-radius:10px; width:80%; float:right;"
              )
            )
          }
          else {
            if (is.na(study_message$read) | study_message$read) study_message_style <- "background-color:#F5F5F5; margin-top:10px; padding:15px; border-radius:10px;"
            else study_message_style <- "background-color:#ECF8E7; margin-top:10px; padding:15px; border-radius:10px;"

            creator_name <- r$users %>% dplyr::filter(id == study_message$creator_id) %>%
              dplyr::mutate(creator_name = paste0(firstname, " ", lastname)) %>% dplyr::pull(creator_name)

            author_span <- shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 30),
              div(creator_name), div(paste0(date, ", ", format(study_message$datetime, "%H:%M")))
            )
            
            if (!study_message$deleted & study_message$creator_id == r$user_id) deletion_div <- div(
                actionButton(ns(paste0("delete_message_", study_message$id)), "X", style = "padding:0px 5px 0px 5px;",
                onclick = paste0("Shiny.setInputValue('", id, "-message_deletion', this.id, {priority: 'event'})")), 
              style = "float:right; margin-top:-10px; margin-right:-10px;")
            else deletion_div <- ""

            study_message_ui <-
            div(
              deletion_div,
              div(
                div(
                  shiny.fluent::Persona(size = 2,
                    imageInitials = conversation_messages_users %>% dplyr::filter(creator_id == study_message$creator_id) %>% dplyr::pull(initials),
                    initialsColor = conversation_messages_users %>% dplyr::filter(creator_id == study_message$creator_id) %>% dplyr::pull(colour)
                  ),
                  style = "float:left; margin-left:-40px; margin-top:10px;"
                ),
                div(
                  div(
                    author_span,
                    style = "font-size:12px; margin-bottom:10px; color:#878787"
                  ),
                  message_div,
                  style = study_message_style
                )
              ),
              style = "width:80%; float:left; margin-left:30px;"
            )
          }

          conversation_messages_ui <- tagList(conversation_messages_ui, study_message_ui)
        }

        output$selected_conversation <- renderUI(conversation_messages_ui)
        shinyjs::show("conversation_new_message_title_div", "new_message_text_div_1", "new_message_text_div_2")

        if (r$study_reload_conversation_type != "refresh_conversation"){
          output$new_message_preview <- renderUI("")
          output$conversation_object <- renderUI(tagList(i18n$t("object"), " : ", strong(r$study_selected_conversation$conversation_name)))
          shinyAce::updateAceEditor(session, "new_message_text", value = "")
          sapply(c("new_message_text_div_1", "new_message_text_div_2", "conversation_hide_new_message_div"), function(name) shinyjs::hide(name))
          shinyjs::show("conversation_new_message")
        }

        # Update inbox_messages
        # Create a row if user hasn't a row (admin who has access to all studies or an account created after the conversation was sent)
        
        sql <- glue::glue_sql("SELECT * FROM inbox_messages WHERE receiver_id = {r$user_id} AND message_id IN ({conversation_messages$id*})", .con = r$db)
        has_inbox_messages <- DBI::dbGetQuery(r$db, sql)
        if (nrow(has_inbox_messages) == 0){
          new_data <- conversation_messages %>% dplyr::select(message_id = id) %>%
            dplyr::mutate(id = 1:dplyr::n() + get_last_row(r$db, "inbox_messages"), .before = "message_id") %>%
            dplyr::mutate(receiver_id = r$user_id, read = TRUE, datetime = as.character(Sys.time()), deleted = FALSE)
          DBI::dbAppendTable(r$db, "inbox_messages", new_data)
        }
        
        sql <- glue::glue_sql("UPDATE inbox_messages SET read = TRUE WHERE receiver_id = {r$user_id} AND message_id IN ({conversation_messages$id*})", .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql)
        DBI::dbClearResult(query)

        # Update r$study_messages
        r$study_messages <- r$study_messages %>%
          dplyr::left_join(conversation_messages %>% dplyr::transmute(id, new_read = TRUE), by = "id") %>%
          dplyr::mutate(read = dplyr::case_when(
            as.logical(read) ~ TRUE, new_read ~ TRUE, TRUE ~ FALSE
          )) %>%
          dplyr::select(-new_read) %>%
          dplyr::arrange(dplyr::desc(datetime))
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_messages - observer r$study_reload_conversation"))
      })

      # --- --- --- --- -
      ## New message ----
      # --- --- --- --- -
      
      observeEvent(input$messages_current_tab, {
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer input$messages_current_tab"))
        if (input$messages_current_tab == "new_conversation") shinyjs::runjs(sprintf("document.getElementById('%s').focus();", session$ns("new_conversation_name")))
        if (input$messages_current_tab == "all_messages"){
          shinyjs::show("all_messages_div")
          shinyjs::hide("new_conversation_div")
        }
        else {
          shinyjs::hide("all_messages_div")
          shinyjs::show("new_conversation_div")
        }
      })

      observeEvent(input$conversation_new_message, {
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer input$conversation_new_message"))
        sapply(c("new_message_text_div_1", "new_message_text_div_2", "conversation_hide_new_message_div"), function(name) shinyjs::show(name))
        shinyjs::hide("conversation_new_message")
        
        # Place cursor on aceEditor
        shinyjs::runjs(sprintf('var editor = ace.edit("%s"); editor.focus();', session$ns("new_message_text")))
      })

      observeEvent(input$conversation_hide_new_message, {
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer input$conversation_hide_new_message"))
        sapply(c("new_message_text_div_1", "new_message_text_div_2", "conversation_hide_new_message_div"), function(name) shinyjs::hide(name))
        shinyjs::show("conversation_new_message")
      })

      # --- --- --- --- --- --- --- --- --- --- -- -
      ## Preview for new message & conversation ----
      # --- --- --- --- --- --- --- --- --- --- -- -

      observeEvent(input$preview_new_conversation, {
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer input$preview_new_conversation"))
        r$study_preview_trigger <- Sys.time()
        r$study_preview_type <- "conversation"
      })

      observeEvent(input$preview_new_message, {
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer input$preview_new_message"))
        r$study_preview_trigger <- Sys.time()
        r$study_preview_type <- "message"
      })

      observeEvent(r$study_preview_trigger, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer r$study_preview_trigger"))

        req(r$study_preview_type %in% c("message", "conversation"))

        type <- r$study_preview_type
        if (type == "conversation") background_color <- "#E6F1F8"
        if (type == "message") background_color <- "#ECF8E7"

        if (input[[paste0("new_", type, "_text")]] == "") output[[paste0("new_", type, "_preview")]] <- renderUI("")

        req(input[[paste0("new_", type, "_text")]] != "")

        if (input[[paste0("new_", type, "_as_rmarkdown")]]){
 
          tryCatch({

            new_text <- input[[paste0("new_", type, "_text")]] %>% 
              stringr::str_replace_all("\r", "\n")

            # Clear temp dir
            # unlink(paste0(r$app_folder, "/temp_files"), recursive = TRUE, force = TRUE)

            markdown_settings <- paste0("```{r setup, include=FALSE}\n",
              "knitr::opts_knit$set(root.dir = '", r$app_folder, "/temp_files/", r$user_id, "/markdowns/')\n",
              "knitr::opts_chunk$set(root.dir = '", r$app_folder, "/temp_files/", r$user_id, "/markdowns/', fig.path = '", r$app_folder, "/temp_files/", r$user_id, "/markdowns/',",
              "dpi = 600, out.width='600px')\n",
              "```\n")

            markdown_file <- paste0(markdown_settings, new_text)

            # Create temp dir
            dir <- paste0(r$app_folder, "/temp_files/", r$user_id, "/markdowns")
            new_file <- paste0(dir, "/", paste0(sample(c(0:9, letters[1:6]), 8, TRUE), collapse = ''), "_", as.character(Sys.time()) %>% stringr::str_replace_all(":", "_"), ".Md")
            if (!dir.exists(dir)) dir.create(dir)

            # Variables to hide
            new_env_vars <- list("r" = NA)
            # Variables to keep
            for (var in c("d", "m", "i18n")) new_env_vars[[var]] <- eval(parse(text = var))
            new_env <- rlang::new_environment(data = new_env_vars, parent = pryr::where("r"))
            
            # Create the markdown file
            knitr::knit(text = markdown_file, envir = new_env, output = new_file, quiet = TRUE)

            markdown_ui <- includeMarkdown(new_file) %>%
              gsub('(<code class="[^"]*)"', '\\1 code_highlight"', .) %>%
              stringr::str_replace_all("<code>", "<code class=\"code_highlight\">") %>%
              HTML()
            
            output[[paste0("new_", type, "_preview")]] <- renderUI(
              div(
                div(
                  div(paste0(i18n$t("today"), ", ", format(Sys.time(), "%H:%M"))),
                  style = "font-size:12px; margin-bottom:10px; color:#878787"
                ),
                markdown_ui,
                style = paste0("background-color:", background_color, "; word-wrap: break-word; margin-top:10px; padding:15px 15px 0px 15px; border-radius:10px; float:left; width:80%;")
              )
            )
          }, error = function(e) "")
        }

        else {

          new_text <- input[[paste0("new_", type, "_text")]] %>% stringr::str_replace_all("\n", "<br />")

          output[[paste0("new_", type, "_preview")]] <- renderUI(
            div(
              div(
                div(paste0(i18n$t("today"), ", ", format(Sys.time(), "%H:%M"))),
                style = "font-size:12px; margin-bottom:10px; color:#878787"
              ),
              div(HTML(new_text)),
              style = paste0("background-color:", background_color, "; margin-top:10px; padding:15px; border-radius:10px; float:left; width:80%;")
            )
          )
        }
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_messages - observer r$study_preview_trigger"))
      })

      # --- --- --- --- --- --- --- --- --- --
      ## Send new message or conversation ----
      # --- --- --- --- --- --- --- --- --- --

      observeEvent(input$send_new_conversation, {
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer input$send_new_conversation"))
        r$study_save_message_conversation_trigger <- Sys.time()
        r$study_save_message_conversation_type <- "conversation"
      })
      
      observeEvent(input$new_conversation_text_run_all, {
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer input$new_conversation_text_run_all"))
        r$study_save_message_conversation_trigger <- Sys.time()
        r$study_save_message_conversation_type <- "conversation"
      })

      observeEvent(input$send_new_message, {
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer input$send_new_message"))
        r$study_save_message_conversation_trigger <- Sys.time()
        r$study_save_message_conversation_type <- "message"
      })
      
      observeEvent(input$new_message_text_run_all, {
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer input$new_message_text_run_all"))
        r$study_save_message_conversation_trigger <- Sys.time()
        r$study_save_message_conversation_type <- "message"
      })

      observeEvent(r$study_save_message_conversation_trigger, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) cat(paste0("\n", Sys.time(), " - mod_messages - observer r$study_save_message_conversation_trigger"))

        req(r$study_save_message_conversation_type %in% c("message", "conversation"))
        type <- r$study_save_message_conversation_type

        # Is conversation name empty ?
        if (type == "conversation"){
          if (is.na(input$new_conversation_name) | input$new_conversation_name == "")
            shiny.fluent::updateTextField.shinyInput(session, "new_conversation_name", errorMessage = i18n$t("provide_valid_object"))
          else shiny.fluent::updateTextField.shinyInput(session, "new_conversation_name", errorMessage = NULL)

          req(!is.na(input$new_conversation_name) & input$new_conversation_name != "")
        }

        # Is text is empty ?
        req(input[[paste0("new_", type, "_text")]] != "")

        # If Rmarkdown toggle is TRUE, save the file

        if (input[[paste0("new_", type, "_as_rmarkdown")]]){

          tryCatch({

            new_text <- input[[paste0("new_", type, "_text")]] %>% 
              stringr::str_replace_all("\r", "\n")

            unique_id <- paste0(Sys.time() %>% stringr::str_replace_all(":| |-", "") , paste0(sample(c(0:9, letters[1:6]), 24, TRUE), collapse = ''))
            new_dir <- paste0(r$app_folder, "/messages/", unique_id)

            markdown_settings <- paste0("```{r setup, include=FALSE}\n",
              "knitr::opts_knit$set(root.dir = '", new_dir, "/')\n",
              "knitr::opts_chunk$set(root.dir = '", new_dir, "/', fig.path = '", new_dir, "/',",
              "dpi = 600, out.width='600px')\n",
              "```\n")

            markdown_file <- paste0(markdown_settings, new_text)

            # Create new dir
            new_file <- paste0(new_dir, "/", as.character(Sys.time()) %>% stringr::str_replace_all(":| |-", ""), ".Md")
            if (!dir.exists(new_dir)) dir.create(new_dir)
            
            # Variables to hide
            new_env_vars <- list("r" = NA)
            # Variables to keep
            for (var in c("d", "m", "i18n")) new_env_vars[[var]] <- eval(parse(text = var))
            new_env <- rlang::new_environment(data = new_env_vars, parent = pryr::where("r"))

            # Create the markdown file
            knitr::knit(text = markdown_file, envir = new_env, output = new_file, quiet = TRUE)

          }, error = function(e) "")
        }
        else {
          new_text <- input[[paste0("new_", type, "_text")]]
          new_file <- ""
        }

        # Get list of users authorized to see this study

        users_allowed_read_group <- r$options %>% dplyr::filter(category == "study", link_id == m$selected_study, name == "users_allowed_read_group") %>% dplyr::pull(value)
        if (users_allowed_read_group == "everybody") receivers_ids <- r$users %>% dplyr::pull(id)
        if (users_allowed_read_group == "people_picker") receivers_ids <- r$options %>% dplyr::filter(category == "study", link_id == m$selected_study, name == "user_allowed_read") %>% dplyr::pull(value_num)

        # Add data to database

        if (type == "conversation"){
          conversation_id <- get_last_row(r$db, "conversations") + 1
          conversation_name <- input$new_conversation_name
        }
        if (type == "message"){
          conversation_id <- r$study_selected_conversation$conversation_id
          conversation_name <- r$study_selected_conversation$conversation_name
        }

        new_message_id <- get_last_row(r$db, "messages") + 1

        if (type == "conversation"){
          # Conversations table
          new_data <- tibble::tribble(
            ~id, ~name, ~datetime, ~deleted,
            conversation_id, input$new_conversation_name, as.character(Sys.time()), FALSE)
          DBI::dbAppendTable(r$db, "conversations", new_data)
        }

        # Messages table
        new_data <- tibble::tribble(
          ~id, ~conversation_id, ~study_id, ~category, ~message, ~filepath, ~creator_id, ~datetime, ~deleted,
          new_message_id, conversation_id, m$selected_study, "study_message",
          new_text, new_file, r$user_id, as.character(Sys.time()), FALSE)
        DBI::dbAppendTable(r$db, "messages", new_data)

        # Inbox_messages table
        new_data <- tibble::tibble(
          id = seq(get_last_row(r$db, "inbox_messages") + 1, get_last_row(r$db, "inbox_messages") + length(receivers_ids), 1),
          message_id = new_message_id, receiver_id = receivers_ids, read = FALSE, datetime = as.character(Sys.time()), deleted = FALSE) %>%
          dplyr::mutate(read = dplyr::case_when(receiver_id == r$user_id ~ TRUE, TRUE ~ read))
        DBI::dbAppendTable(r$db, "inbox_messages", new_data)

        # Reset fields
        if (type == "conversation") shiny.fluent::updateTextField.shinyInput(session, "new_conversation_name", value = "")
        shinyAce::updateAceEditor(session, paste0("new_", type, "_text"), value = "")
        output[[paste0("new_", type, "_preview")]] <- renderUI("")

        if (type == "conversation"){

          # Reload conversations datatable
          
          r$study_conversations <- r$study_conversations %>%
            dplyr::bind_rows(
              tibble::tibble(
                conversation_id = conversation_id, conversation_name = conversation_name, 
                datetime = as.character(Sys.time()), unread_messages = 0, action = ""
              )
            ) %>%
            dplyr::mutate(
              action = as.character(shiny::actionButton(ns("delete_conversation_%conversation_id%"), "", icon = icon("trash-alt"),
                onclick = paste0("Shiny.setInputValue('", id, "-conversation_deletion', this.id, {priority: 'event'})")))
            ) %>%
            dplyr::mutate(action = stringr::str_replace_all(action, "%conversation_id%", as.character(conversation_id))) %>%
            dplyr::arrange(dplyr::desc(unread_messages), dplyr::desc(datetime))
          
          r$study_conversations_temp <- r$study_conversations %>% dplyr::mutate(modified = FALSE)
          
          r$study_reload_conversations_datatable_clear_selection <- "all"
          r$study_reload_conversations_datatable <- Sys.time()
        }

        # Reload conversation

        r$study_messages <- r$study_messages %>%
          dplyr::bind_rows(
            tibble::tribble(
              ~id, ~conversation_id, ~conversation_name, ~message, ~filepath, ~creator_id, ~datetime, ~read, ~deleted,
              new_message_id, conversation_id, conversation_name, new_text, new_file, r$user_id, Sys.time(), 0, FALSE
            )
          ) %>%
          dplyr::arrange(dplyr::desc(datetime))

        if (type == "message"){
          r$study_reload_conversation <- Sys.time()
          r$study_reload_conversation_type <- "add_message"
        } 

        # Notify user only for conversation
        # Set current pivot to messages
        # Hide messages for selected conversation
        if (type == "conversation"){
          show_message_bar(output, message = "new_conversation_added", type = "success", i18n = i18n, ns = ns)
          shinyjs::runjs(glue::glue("$('#{id}-study_messages_pivot button[name=\"{i18n$t('all_messages')}\"]').click();"))
          output$selected_conversation <- renderUI("")
          shinyjs::hide("conversation_new_message_title_div", "new_message_text_div_1", "new_message_text_div_2")
        }
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_messages - observer r$study_save_message_conversation_trigger"))
      })
  })
}
