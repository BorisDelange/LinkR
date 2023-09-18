#' scripts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_scripts_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  
  cards <- c("all_scripts_card", "dataset_scripts_card", "scripts_datatable_card", 
    "scripts_creation_card", "scripts_edit_code_card", "scripts_options_card", "import_script_card", "export_script_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  })
  
  all_scripts_cards <- list()
  for (name in c("local", "remote_git")){
    all_scripts_cards[[name]] <- tagList(
      conditionalPanel(condition = paste0("input.all_scripts_source == '", name, "'"), ns = ns, 
        DT::DTOutput(ns(paste0(name, "_scripts_datatable"))), br(),
        shinyjs::hidden(
          div(id = ns(paste0(name, "_selected_script_markdown_div")),
            uiOutput(ns(paste0(name, "_selected_script_markdown"))),
            style = "width:99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
        )
      )   
    )
  }
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    shiny.fluent::reactOutput(ns("script_delete_confirm")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = id, text = i18n$t("scripts"))
    ), maxDisplayedItems = 3),
    
    # --- --- -- -- --
    # Pivot items ----
    # --- --- -- -- --
    
    # shinyjs::hidden(
    div(id = ns("menu"),
      shiny.fluent::Pivot(
        id = ns("scripts_pivot"),
        onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
        shiny.fluent::PivotItem(id = "dataset_scripts_card", itemKey = "dataset_scripts_card", headerText = i18n$t("choose_dataset_scripts")),
        shiny.fluent::PivotItem(id = "all_scripts_card", itemKey = "all_scripts_card", headerText = i18n$t("all_scripts_card")),
        shiny.fluent::PivotItem(id = "scripts_datatable_card", itemKey = "scripts_datatable_card", headerText = i18n$t("scripts_management")),
        shiny.fluent::PivotItem(id = "scripts_edit_code_card", itemKey = "scripts_edit_code_card", headerText = i18n$t("edit_script_code")),
        shiny.fluent::PivotItem(id = "scripts_options_card", itemKey = "scripts_options_card", headerText = i18n$t("script_options")),
        shiny.fluent::PivotItem(id = "import_script_card", itemKey = "import_script_card", headerText = i18n$t("import_scripts")),
        shiny.fluent::PivotItem(id = "export_script_card", itemKey = "export_script_card", headerText = i18n$t("export_scripts"))
      )
    ),
    # ),
    
    div(
      id = ns("choose_a_dataset_card"),
      make_card("", div(shiny.fluent::MessageBar(i18n$t("choose_a_damatart_left_side"), messageBarType = 5), style = "margin-top:10px;")),
      br()
    ),
    forbidden_cards,
    
    # --- --- --- --- --- --- --
    # Scripts catalog card ----
    # --- --- --- --- --- --- --
    
    shinyjs::hidden(
      div(
        id = ns("all_scripts_card"),
        make_card(i18n$t("all_scripts_card"),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              div(
                shiny.fluent::ChoiceGroup.shinyInput(ns("all_scripts_source"), value = "local", options = list(
                  list(key = "local", text = i18n$t("local_plural")),
                  list(key = "remote_git", text = i18n$t("git_remote_scripts"))
                ), className = "inline_choicegroup"),
                style = "width:322px;"
              ),
              conditionalPanel(condition = "input.all_scripts_source == 'remote_git'", ns = ns,
                div(
                  shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                    div(strong(i18n$t("remote_git_repo")), style = "margin-top:8px;"),
                    div(shiny.fluent::Dropdown.shinyInput(ns("remote_git_repo")), style = "width:322px;margin-top:3px;")
                  )
                )
              )
            ),
            all_scripts_cards$local,
            all_scripts_cards$remote_git
          )
        ), br()
      )
    ),
    
    # --- --- --- --- --- --- --
    # Dataset scripts card ----
    # --- --- --- --- --- --- --
    
    shinyjs::hidden(
      div(
        id = ns("dataset_scripts_card"),
        div(
          class = glue::glue("card ms-depth-8 ms-sm{12} ms-xl{12}"),
          shiny.fluent::Text(variant = "large", i18n$t("choose_dataset_scripts"), block = TRUE), br(),
          div(shiny.fluent::Dropdown.shinyInput(ns("dataset_scripts_category"), i18n$t("category")), style = "width:300px"),
          div(uiOutput(ns("dataset_scripts_bucket_list"))),
          shiny.fluent::PrimaryButton.shinyInput(ns("save_dataset_scripts"), i18n$t("save"))
        ), 
        make_card(i18n$t("scripts_cache_memory"),
          div(
            br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              make_toggle(i18n = i18n, ns = ns, id = "activate_scripts_cache", label = "activate_scripts_cache", inline = TRUE)),
            conditionalPanel(condition = "input.activate_scripts_cache == true", ns = ns, uiOutput(ns("scripts_cache_infos"))), br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), 
              shiny.fluent::PrimaryButton.shinyInput(ns("save_cache_settings"), i18n$t("save")),
              conditionalPanel(condition = "input.activate_scripts_cache == true", ns = ns, 
                shiny.fluent::DefaultButton.shinyInput(ns("reload_cache"), i18n$t("reload_cache")))
            )
          )
        ), br()
      )
    ),
    
    # --- --- --- --- --- -- -- --
    # Scripts management card ----
    # --- --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("scripts_datatable_card"),
        make_card(i18n$t("scripts_management"),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              make_textfield(i18n = i18n, ns = ns, label = "name", id = "script_name", width = "300px"),
              div(shiny.fluent::PrimaryButton.shinyInput(ns("add_script"), i18n$t("add")), style = "margin-top:38px;"),
              style = "position:relative; z-index:1; width:500px;"
            ),
            div(DT::DTOutput(ns("scripts_datatable")), style = "margin-top:-30px; z-index:2"),
            div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("save_scripts_management"), i18n$t("save")),
                shiny.fluent::DefaultButton.shinyInput(ns("delete_selection"), i18n$t("delete_selection"))
              ),
              style = "position:relative; z-index:2; margin-top:-30px;"
            )
          )
        ), br()
      )
    ),
    
    # --- --- --- --- --- --- --
    # Edit script code card ----
    # --- --- --- --- --- --- --
    
    shinyjs::hidden(
      div(
        id = ns("scripts_edit_code_card"),
        make_shiny_ace_card(i18n$t("edit_script_code"),
          div(
            shiny.fluent::Stack(
              tokens = list(childrenGap = 5),
              make_combobox(i18n = i18n, ns = ns, label = "script", id = "code_selected_script",
                width = "300px", allowFreeform = FALSE, multiSelect = FALSE), br(),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                div(shiny.fluent::Toggle.shinyInput(ns("hide_code_editor"), value = FALSE), style = "margin-top:9px;"),
                div(i18n$t("hide_editor"), style = "font-weight:bold; margin-top:9px; margin-right:30px;")
              ),
              shinyjs::hidden(div(id = ns("div_br"), br()))
            ),
            div(shinyAce::aceEditor(ns("ace_edit_code"), "", mode = "r",
              code_hotkeys = list(
                "r", list(
                  run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
                  run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
                  save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S")
                )
              ),
              autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;"),
            shiny.fluent::Stack(
              tokens = list(childrenGap = 5),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("execute_code"), i18n$t("run_code")),
                shiny.fluent::DefaultButton.shinyInput(ns("save_code"), i18n$t("save"))
              ), br(),
              div(textOutput(ns("datetime_code_execution")), style = "color:#878787;"), br(),
              div(id = ns("console_output"), verbatimTextOutput(ns("console_result")),
                style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
            )
          )
        ), br()
      )
    ),
    
    # --- --- --- --- -- -- --
    # Script options card ----
    # --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("scripts_options_card"),
        make_shiny_ace_card(i18n$t("script_options"),
          div(
            shiny.fluent::Stack(
              tokens = list(childrenGap = 5),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
                make_combobox(i18n = i18n, ns = ns, label = "script", id = "options_selected_script",
                  width = "320px", allowFreeform = FALSE, multiSelect = FALSE),
                make_textfield(i18n = i18n, ns = ns, label = "author", id = "script_author", width = "320px"),
                make_textfield(i18n = i18n, ns = ns, label = "version", id = "script_version", width = "80px")
              ), 
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
                make_textfield(i18n = i18n, ns = ns, label = "name_fr", id = "script_name_fr", width = "320px"),
                make_textfield(i18n = i18n, ns = ns, label = "name_en", id = "script_name_en", width = "320px")
              ),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
                make_textfield(i18n = i18n, ns = ns, label = "category_fr", id = "script_category_fr", width = "320px"),
                make_textfield(i18n = i18n, ns = ns, label = "category_en", id = "script_category_en", width = "320px")
              ), br(),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                div(paste0(i18n$t("description"), " :"), style = "font-weight:bold; margin-top:7px; margin-right:5px;"),
                shiny.fluent::ChoiceGroup.shinyInput(ns("script_description_language"), value = "fr", options = list(
                  list(key = "fr", text = "FR"),
                  list(key = "en", text = "EN")
                ), className = "inline_choicegroup")
              )
            ),
            conditionalPanel(condition = "input.script_description_language == 'fr'", ns = ns,
              div(shinyAce::aceEditor(ns("script_description_fr"), "", mode = "markdown", 
                code_hotkeys = list(
                  "markdown", 
                  list(
                    save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
                    run_all = list(win = "CTRL-SHIFT-ENTER|CTRL-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER|CTRL-ENTER|CMD-ENTER") 
                  )
                ),
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;")),
            conditionalPanel(condition = "input.script_description_language == 'en'", ns = ns,
              div(shinyAce::aceEditor(ns("script_description_en"), "", mode = "markdown", 
                code_hotkeys = list(
                  "markdown", 
                  list(
                    save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
                    run_all = list(win = "CTRL-SHIFT-ENTER|CTRL-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER|CTRL-ENTER|CMD-ENTER") 
                  )
                ),
                autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;")),
            shiny.fluent::Stack(
              tokens = list(childrenGap = 5),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 21),
                shiny.fluent::PrimaryButton.shinyInput(ns("save_options_description"), i18n$t("save")), " ",
                shiny.fluent::DefaultButton.shinyInput(ns("execute_options_description"), i18n$t("run_code"))
              ),
              br(), br(),
              div(id = ns("description_markdown_output"),
                uiOutput(ns("description_markdown_result")), 
                style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px; padding-top: 10px;")
            )
          )
        ), br()
      )
    ),
    
    # --- --- --- --- --- -- -
    # Import scripts card ----
    # --- --- --- --- --- -- -
    
    shinyjs::hidden(
      div(
        id = ns("import_script_card"),
        make_card(i18n$t("import_scripts"),
          div(br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), 
              make_toggle(i18n = i18n, ns = ns, label = "replace_already_existing_scripts", inline = TRUE)), br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::DefaultButton.shinyInput(ns("import_scripts_browse"), i18n$t("choose_zip_file"), style = "width:270px;"),
              uiOutput(ns("import_scripts_status"))), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("import_scripts_button"), i18n$t("import_scripts"), iconProps = list(iconName = "Download"), style = "width:270px;"), br(),
            shinyjs::hidden(
              div(
                id = ns("imported_scripts_div"), br(),
                strong(i18n$t("imported_scripts")),
                div(DT::DTOutput(ns("imported_scripts")))
              )
            ),
            div(style = "display:none;", fileInput(ns("import_scripts_upload"), label = "", multiple = FALSE, accept = ".zip"))
          )
        ), br()
      )
    ),
    
    # --- --- --- --- --- -- -
    # Export scripts card ----
    # --- --- --- --- --- -- -
    
    shinyjs::hidden(
      div(
        id = ns("export_script_card"),
        make_shiny_ace_card(i18n$t("export_scripts"),
          div(
            shiny.fluent::Stack(
              horizontal = TRUE, tokens = list(childrenGap = 10),
              div(
                div(id = ns("scripts_to_export_title"), class = "input_title", i18n$t("scripts_to_export")),
                shiny.fluent::Dropdown.shinyInput(ns("scripts_to_export"), multiSelect = TRUE,
                  onChanged = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-scripts_to_export_trigger', Math.random())"))),
                style = "width:400px;"
              ),
              div(shiny.fluent::PrimaryButton.shinyInput(ns("export_selected_scripts"), 
                i18n$t("export_scripts"), iconProps = list(iconName = "Upload")), style = "margin-top:38px;"),
              style = "position:relative; z-index:1; width:700px;"
            ),
            div(DT::DTOutput(ns("scripts_to_export_datatable")), style = "margin-top:-30px; z-index:2"),
            div(style = "visibility:hidden;", downloadButton(ns("export_scripts_download"), label = ""))
          )
        ), br()
      )
    )
  ) -> result
  
  result
}

#' scripts Server Functions
#'
#' @noRd 
mod_scripts_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(), 
  language = "en", i18n = character(), perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (perf_monitoring) monitor_perf(r = r, action = "start")
    if (debug) print(paste0(Sys.time(), " - mod_scripts - start"))
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("all_scripts_card", "dataset_scripts_card", "scripts_datatable_card",
      "scripts_creation_card", "scripts_edit_code_card", "scripts_options_card", "import_script_card", "export_script_card")
    # show_or_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    
    observeEvent(input$current_tab, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$current_tab"))
      
      sapply(cards %>% setdiff(., input$current_tab), shinyjs::hide)
      sapply(cards %>% setdiff(., input$current_tab), function(card) shinyjs::hide(paste0(card, "_forbidden")))
      
      if (input$current_tab %in% r$user_accesses){
        show_card <- FALSE
        if (input$current_tab != "dataset_scripts_card"){
          show_card <- TRUE
          shinyjs::hide("choose_a_dataset_card")
        }
        
        else if (input$current_tab == "dataset_scripts_card") if (length(r$selected_dataset) > 0) if(!is.na(r$selected_dataset)) show_card <- TRUE
        
        if (!show_card & input$current_tab == "dataset_scripts_card") shinyjs::show("choose_a_dataset_card")
        
        if (show_card) shinyjs::show(input$current_tab)
      }
      else shinyjs::show(paste0(input$current_tab, "_forbidden"))
    })
    
    # Close message bar
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    # --- --- --- --- --- -
    # Show message bar ----
    # --- --- --- --- --- -
    
    # This allows to show message in multiple pages at the same time (eg when loading a dataset in Studies page, render message bar in Subsets page)
    
    observeEvent(r$show_message_bar, show_message_bar(output, r$show_message_bar$message, r$show_message_bar$type, i18n = i18n, ns = ns))
    if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - show_message_bars"))
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    observeEvent(input$help, if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r$help_scripts_open_panel <- TRUE)
    observeEvent(input$hide_panel, r$help_scripts_open_panel <- FALSE)
    
    r$help_scripts_open_panel_light_dismiss <- TRUE
    observeEvent(input$show_modal, r$help_scripts_open_modal <- TRUE)
    observeEvent(input$hide_modal, {
      r$help_scripts_open_modal <- FALSE
      r$help_scripts_open_panel_light_dismiss <- TRUE
    })
    
    observeEvent(shiny.router::get_page(), {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer shiny_router::change_page"))

      # Close help pages when page changes
      # r$help_scripts_open_panel <- FALSE
      # r$help_scripts_open_modal <- FALSE

      # Load Export scripts page, to load DT (doesn't update with other DT if not already loaded once)
      if (shiny.router::get_page() == "scripts" & length(r$scripts_page_loaded) == 0){
        if ("export_script_card" %in% r$user_accesses) shinyjs::show("export_script_card")
        shinyjs::delay(500, shinyjs::hide("export_script_card"))
        r$scripts_page_loaded <- TRUE
      }
    })
    
    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_scripts_page_", i)]] <- Sys.time())
    })
    
    help_scripts(output = output, r = r, id = id, language = language, i18n = i18n, ns = ns)
    
    observeEvent(input$copy_code_1, r$help_scripts_copy_code_1 <- Sys.time())
    # observeEvent(input$copy_code_2, r$help_scripts_copy_code_2 <- Sys.time())
    observeEvent(input$copy_code_3, r$help_scripts_copy_code_3 <- Sys.time())
    
    # --- --- --- --- --- -
    # Update dropdowns ----
    # --- --- --- --- --- -
    
    observeEvent(r$scripts, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$scripts 1"))
      
      options <- convert_tibble_to_list(r$scripts%>% dplyr::arrange(name), key_col = "id", text_col = "name")
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_script", options = options)
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_script", options = options)
      shiny.fluent::updateComboBox.shinyInput(session, "scripts_description_selected_script", options = options)
      
      r$reload_local_scripts_datatable <- Sys.time()
    })
    
    # --- --- --- --- -
    # Reset fields ----
    # --- --- --- --- -
    
    reset_scripts_fields <- function(session){
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - function reset_scripts_fields"))
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_script", value = NULL)
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_script", value = NULL)
      shiny.fluent::updateComboBox.shinyInput(session, "scripts_description_selected_script", value = NULL)
      
      output$scripts_description_markdown_result <- renderUI("")
      shinyAce::updateAceEditor(session, "ace_edit_code", value = "")
      output$console_result <- renderText("")
      output$datetime_code_execution <- renderText("")
      blank_data <- data <- tibble::tribble(~id)
      names(blank_data) <- c("")
      output$table_result <- DT::renderDT(blank_data, options = list(dom = "<'datatable_length'l><'top't><'bottom'p>"), 
        rownames = FALSE, selection = "single", escape = FALSE, server = TRUE)
      shinyAce::updateAceEditor(session, "ace_options_description", value = "")
      output$description_markdown_result <- renderUI("")
      output$scripts_cache_infos <- renderUI("")
    }
    
    # --- --- --- --- --- --- --- --
    # When a dataset is selected ----
    # --- --- --- --- --- --- --- --
    
    observeEvent(r$selected_dataset, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$selected_dataset"))
      
      # Reset fields
      # reset_scripts_fields(session = session)
      
      # activate_scripts_cache option
      value <- r$options %>% 
        dplyr::filter(category == "dataset", name == "activate_scripts_cache", link_id == r$selected_dataset) %>% dplyr::pull(value_num)
      shiny.fluent::updateToggle.shinyInput(session, "activate_scripts_cache", value = as.logical(value))
      
      # Show first card & hide "choose a dataset" card
      shinyjs::hide("choose_a_dataset_card")
      # shinyjs::show("menu")
      if (length(input$current_tab) == 0){
        if ("dataset_scripts_card" %in% r$user_accesses) shinyjs::show("dataset_scripts_card")
        else shinyjs::show("dataset_scripts_card_forbidden")
      }
      if (length(input$current_tab) > 0){
        if ("dataset_scripts_card" %in% r$user_accesses) if (input$current_tab == "dataset_scripts_card") shinyjs::show("dataset_scripts_card")
      }
      
      # Update category dropdown
      
      categories_options <-
        tibble::tibble(key_col = "all_scripts", text_col = i18n$t("all_scripts")) %>% dplyr::bind_rows(
          r$scripts %>%
            dplyr::left_join(r$options %>% dplyr::filter(category == "script", name == paste0("category_", language)) %>%
              dplyr::select(id = link_id, category = value), by = "id") %>%
            dplyr::filter(!is.na(category) & category != "") %>%
            dplyr::select(category) %>%
            dplyr::group_by(category) %>%
            dplyr::slice(1) %>%
            dplyr::ungroup() %>%
            dplyr::arrange(category) %>%
            dplyr::select(key_col = category, text_col = category)) %>%
        convert_tibble_to_list(key_col = "key_col", text_col = "text_col")

      shiny.fluent::updateDropdown.shinyInput(session, "dataset_scripts_category", options = categories_options, value = "all_scripts")
      
      # Update bucket_list
      
      r$scripts_reload_bucket_list_var <- Sys.time()
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer r$selected_dataset"))
    })
    
    # --- --- --- --- --- -
    # Dataset scripts ----
    # --- --- --- --- --- -
    
    # Select scripts category
    
    observeEvent(input$dataset_scripts_category, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$dataset_scripts_category"))
      
      r$scripts_update_bucket_list <- Sys.time()
    })
    
    # Reload bucket_list scripts var
    
    observeEvent(r$scripts_reload_bucket_list_var, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$scripts_reload_bucket_list_var"))
      
      req(r$selected_dataset)
      
      r$bucket_list_all_scripts <- r$local_scripts
      
      r$bucket_list_selected_scripts <- r$local_scripts %>% dplyr::inner_join(
        r$options %>%
          dplyr::filter(category == "dataset_scripts", link_id == r$selected_dataset) %>%
          dplyr::select(id = value_num),
        by = "id"
      )
      
      r$scripts_update_bucket_list <- Sys.time()
    })
    
    # Update bucket_list
    
    observeEvent(r$scripts_update_bucket_list, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$scripts_update_bucket_list"))
            
      req(input$dataset_scripts_category)
      
      all_scripts <- r$bucket_list_all_scripts
      selected_scripts <- r$bucket_list_selected_scripts
      available_scripts <- all_scripts %>% dplyr::anti_join(selected_scripts, by = "id")
      
      if (input$dataset_scripts_category != "all_scripts"){
        selected_scripts <- selected_scripts %>% dplyr::filter(category == input$dataset_scripts_category)
        available_scripts <- available_scripts %>% dplyr::filter(category == input$dataset_scripts_category)
      }
      
      output$dataset_scripts_bucket_list <- renderUI({
        
        sortable::bucket_list(
          header = NULL,
          group_name = ns("all_dataset_scripts"),
          orientation = "horizontal",
          sortable::add_rank_list(text = i18n$t("selected_scripts"), labels = selected_scripts$name, input_id = ns("dataset_selected_scripts")),
          sortable::add_rank_list(text = i18n$t("available_scripts"), labels = available_scripts$name, input_id = ns("dataset_available_scripts"))
        )
      })
    })
    
    # Updates on bucket_list
    
    observeEvent(input$dataset_selected_scripts, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$dataset_selected_scripts"))
      
      if (length(input$dataset_selected_scripts) == 0){
        if (input$dataset_scripts_category == "all_scripts") r$bucket_list_selected_scripts <- r$bucket_list_selected_scripts %>% dplyr::slice(0)
        else r$bucket_list_selected_scripts <- r$bucket_list_selected_scripts %>% dplyr::filter(category != input$dataset_scripts_category)
      }
      if (length(input$dataset_selected_scripts) > 0){
        if (input$dataset_scripts_category == "all_scripts") r$bucket_list_selected_scripts <- r$bucket_list_all_scripts %>% 
          dplyr::filter(name %in% input$dataset_selected_scripts)
          
        else r$bucket_list_selected_scripts <- r$bucket_list_selected_scripts %>%
          dplyr::filter(is.na(category) | category == "" | category != input$dataset_scripts_category) %>%
          dplyr::bind_rows(r$bucket_list_all_scripts %>% dplyr::filter(category == input$dataset_scripts_category & name %in% input$dataset_selected_scripts))
      }
    })
    
    # Save dataset scripts
    
    observeEvent(input$save_dataset_scripts, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$save_dataset_scripts"))
      
      # Delete rows in options table concerning the scripts for this dataset
      
      sql <- glue::glue_sql("DELETE FROM options WHERE category = 'dataset_scripts' AND link_id = {r$selected_dataset}", .con = r$db)
      DBI::dbSendStatement(r$db, sql) -> query
      DBI::dbClearResult(query)
      r$options <- r$options %>% dplyr::filter(category != "dataset_scripts" | (category == "dataset_scripts" & link_id != r$selected_dataset))

      # Add in options table informations concerning the scripts for this dataset
      
      r$bucket_list_selected_scripts

      data_insert <- r$bucket_list_selected_scripts %>%
        dplyr::transmute(category = "dataset_scripts", link_id = r$selected_dataset, name = "", value = "",
          value_num = id, datetime = as.character(Sys.time()), deleted = FALSE)

      data_insert$id <- seq.int(nrow(data_insert)) + get_last_row(r$db, "options")
      data_insert <- data_insert %>% dplyr::relocate(id)

      DBI::dbAppendTable(r$db, "options", data_insert)
      r$options <- r$options %>% dplyr::bind_rows(data_insert)
      
      show_message_bar(output,  "modif_saved", "success", i18n, ns = ns)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer input_save_dataset_scripts"))
    })
    
    # Save cache settings
    
    observeEvent(input$save_cache_settings, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$save_cache_settings"))
      
      sql <- glue::glue_sql(paste0("UPDATE options SET value_num = {as.integer(input$activate_scripts_cache)} ",
        "WHERE category = 'dataset' AND name = 'activate_scripts_cache' AND link_id = {r$selected_dataset} AND deleted IS FALSE"), .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      r$options <- r$options %>% dplyr::mutate(value_num = dplyr::case_when(
        category == "dataset" & name == "activate_scripts_cache" & link_id == r$selected_dataset ~ as.numeric(input$activate_scripts_cache),
        TRUE ~ value_num
      ))
      
      show_message_bar(output,  "modif_saved", "success", i18n, ns = ns)
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer input$save_cache_settings"))
    })
    
    # Update scripts_cache_infos UI
    
    observeEvent(r$update_scripts_cache_card, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$update_scripts_cache_card"))
      
      loaded_scripts_file_path <- paste0(r$app_folder, "/datasets/", r$selected_dataset, "/loaded_scripts.csv")
      if (file.exists(loaded_scripts_file_path)) dataset_loaded_scripts <- readr::read_csv(loaded_scripts_file_path, show_col_types = FALSE)
      if (!file.exists(loaded_scripts_file_path)) dataset_loaded_scripts <- tibble::tibble()
      
      if (nrow(dataset_loaded_scripts) > 0){
        
        datetime <- dataset_loaded_scripts %>% dplyr::slice(1) %>% dplyr::pull(datetime) %>% format_datetime(language, sec = FALSE)
        
        dataset_loaded_scripts <- dataset_loaded_scripts %>%
          dplyr::left_join(r$scripts %>% dplyr::select(id, name), by = "id") %>%
          dplyr::mutate(name = dplyr::case_when(is.na(name) ~ i18n$t("deleted_script"), TRUE ~ name))
        
        loaded_scripts <- list()
        loaded_scripts$success <- dataset_loaded_scripts %>% dplyr::filter(status == "success")
        loaded_scripts$failure <- dataset_loaded_scripts %>% dplyr::filter(status == "failure")
        
        for (status in c("success", "failure")){
          if (nrow(loaded_scripts[[status]]) == 0) loaded_scripts[[status]] <- "/"
          else {
            my_list <- tagList()
            for (i in 1:nrow(loaded_scripts[[status]])){
              row <- loaded_scripts[[status]][i, ]
              my_list <- tagList(my_list, tags$li(row$name))
            }
            loaded_scripts[[status]] <- tagList(tags$ul(my_list))
          }
        }
        
        output$scripts_cache_infos <- renderUI({
          if (debug) print(paste0(Sys.time(), " - mod_scripts - output$scripts_cache_infos"))
          
          tagList(
            br(), div(
              strong(i18n$t("last_cache_load_datetime")), " : ", datetime, br(), br(),
              span(i18n$t("scripts_loaded_successfully"), style = "font-weight:bold; color:#0078D5;"), " : ", loaded_scripts$success,
              span(i18n$t("scripts_with_load_failure"), style = "font-weight:bold; color:#CB181D;"), " : ", loaded_scripts$failure,
              style = "border:solid 1px #DDDCDE; padding:8px; margin:0px 10px 0px 10px;"
            )
          )
        })
      }
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer r$update_scripts_cache_card"))
    })
    
    # Reload script cache
    
    observeEvent(input$reload_cache, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$reload_cache"))
      
      r$load_scripts <- Sys.time()
      r$force_reload_scripts_cache <- TRUE
    })
    
    # --- --- --- --- -- -
    # Scripts catalog ----
    # --- --- --- --- -- -
    
    # Update dropdown of remote git repos
    
    observeEvent(r$git_repos, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$git_repos"))
      
      shiny.fluent::updateDropdown.shinyInput(session, "remote_git_repo", 
        options = convert_tibble_to_list(r$git_repos %>% dplyr::filter(category == "script"), key_col = "id", text_col = "name"))
    })
    
    observeEvent(r$reload_local_scripts_datatable, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$reload_local_scripts_datatable"))
      
      if (nrow(r$scripts) == 0) r$local_scripts <- tibble::tibble(id = integer(), name = character(), unique_id = character(), description = character(),
        category = character(), author = character(), version = character(), creation_datetime = character(), update_datetime = character())
      
      if (nrow(r$scripts) > 0) r$local_scripts <- r$scripts %>% 
        dplyr::left_join(r$options %>% dplyr::filter(category == "script", name == "author") %>% dplyr::select(id = link_id, author = value), by = "id") %>%
        dplyr::left_join(r$options %>% dplyr::filter(category == "script", name == "version") %>% dplyr::select(id = link_id, version = value), by = "id") %>%
        dplyr::left_join(r$options %>% dplyr::filter(category == "script", name == paste0("category_", language)) %>% dplyr::select(id = link_id, category = value), by = "id") %>%
        dplyr::left_join(r$options %>% dplyr::filter(category == "script", name == paste0("description_", language)) %>% dplyr::select(id = link_id, description = value), by = "id") %>%
        dplyr::left_join(r$options %>% dplyr::filter(category == "script", name == "unique_id") %>% dplyr::select(id = link_id, unique_id = value), by = "id") %>%
        dplyr::select(-deleted) %>%
        dplyr::relocate(unique_id, description, category, author, version, unique_id, .after = "name") %>%
        dplyr::arrange(name)
      
      # Create datatable if doesn't exist
      
      if (length(r$local_scripts_datatable_proxy) == 0){
        
        sortable_cols <- c("name", "creation_datetime", "update_datetime", "category")
        column_widths <- c("creation_datetime" = "130px", "update_datetime" = "130px", "author" = "100px", "version" = "80px")
        centered_cols <- c("author", "creation_datetime", "update_datetime", "version", "category")
        searchable_cols <- c("name", "category", "author")
        factorize_cols <- c("category", "author")
        hidden_cols <- c("id", "description", "unique_id")
        col_names <- get_col_names("local_scripts", i18n)
        
        render_datatable(output = output, ns = ns, i18n = i18n, data = r$local_scripts, 
          col_names = col_names, output_name = "local_scripts_datatable", selection = "single",
          sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, factorize_cols = factorize_cols, filter = TRUE, hidden_cols = hidden_cols)
      }
      
      if (length(r$local_scripts_datatable_proxy) > 0){
        r$local_scripts_datatable_proxy <- DT::dataTableProxy("local_scripts_datatable", deferUntilFlush = FALSE)
        DT::replaceData(r$local_scripts_datatable_proxy, r$local_scripts, resetPaging = FALSE, rownames = FALSE)
      }
    })
    
    # When script is selected
    
    observeEvent(input$local_scripts_datatable_rows_selected, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$local_scripts_datatable_rows_selected"))
      r$datatable_script_selected <- Sys.time()
      r$datatable_script_selected_type <- "local"
    })
    
    observeEvent(input$remote_git_scripts_datatable_rows_selected, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$remote_git_scripts_datatable_rows_selected"))
      r$datatable_script_selected <- Sys.time()
      r$datatable_script_selected_type <- "remote_git"
    })
    
    observeEvent(r$datatable_script_selected, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$datatable_script_selected"))
      
      type <- r$datatable_script_selected_type
      
      script_description <- r[[paste0(type, "_scripts")]][input[[paste0(type, "_scripts_datatable_rows_selected")]], ] %>% 
        dplyr::pull(description)
      
      if (type == "local") script_description <- script_description %>% stringr::str_replace_all("''", "'")
      
      tryCatch({

        # Clear temp dir
        unlink(paste0(r$app_folder, "/temp_files"), recursive = TRUE, force = TRUE)

        markdown_settings <- paste0("```{r setup, include=FALSE}\nknitr::opts_knit$set(root.dir = '",
          r$app_folder, "/temp_files')\n",
          "knitr::opts_chunk$set(root.dir = '", r$app_folder, "/temp_files/', fig.path = '", r$app_folder, "/temp_files/')\n```\n")
        
        markdown_file <- paste0(markdown_settings, script_description)

        # Create temp dir
        dir <- paste0(r$app_folder, "/temp_files")
        file <- paste0(dir, "/", as.character(Sys.time()) %>% stringr::str_replace_all(":", "_") %>% stringr::str_replace_all(" ", "_"), ".Md")
        if (!dir.exists(dir)) dir.create(dir)

        shinyjs::show(paste0(type, "_selected_script_markdown_div"))
        
        # Create the markdown file
        knitr::knit(text = markdown_file, output = file, quiet = TRUE)

        output[[paste0(type, "_selected_script_markdown")]] <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(file))))
        
      }, error = function(e) report_bug(r = r, output = output, error_message = "error_loading_script_description",
        error_name = "scripts catalog - render markdown description", category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
    })
    
    # Download scripts from repo git
    
    observeEvent(input$remote_git_repo, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$remote_git_repo"))
      
      # Get URL of remote git repo
      url_address <- r$git_repos %>% dplyr::filter(id == input$remote_git_repo) %>% dplyr::pull(url_address)
      if (substr(url_address, nchar(url_address), nchar(url_address)) != "/") url_address <- paste0(url_address, "/")
      
      error_loading_remote_git <- TRUE
      scripts_file <- paste0(r$app_folder, "/temp_files/scripts.xml")
      
      if (r$has_internet){
        
        tryCatch({
          xml2::download_xml(paste0(url_address, "scripts.xml"), scripts_file)
          error_loading_remote_git <- FALSE
        }, error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_connection_remote_git", 
          error_name = "scripts_catalog load scripts.xml", category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
      }
      
      if (error_loading_remote_git) r$remote_git_scripts <- tibble::tibble(name = character(), description = character(),
        category = character(), author = character(), version = character(), creation_datetime = character(), update_datetime = character(), action = character())
      
      else {
        r$remote_git_scripts_full <-
          xml2::read_xml(scripts_file) %>%
          XML::xmlParse() %>%
          XML::xmlToDataFrame(nodes = XML::getNodeSet(., "//script")) %>%
          tibble::as_tibble()
        
        r$remote_git_scripts <- r$remote_git_scripts_full %>%
          dplyr::select(name = paste0("name_", language), unique_id, description = paste0("description_", language), 
            category = paste0("category_", language), author, version, creation_datetime, update_datetime)
      }
      
      r$update_remote_git_scripts_datatable <- Sys.time()
    })
    
    # Update remote_git_scripts datatable
    
    observeEvent(r$update_remote_git_scripts_datatable, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$update_remote_git_scripts_datatable"))
      
      req(r$remote_git_scripts)
      
      # Merge with local scripts, to know if a plugin is already installed
        
      if (nrow(r$remote_git_scripts) == 0){
        remote_git_scripts <- tibble::tibble(name = character(), unique_id = character(), description = character(),
          category = character(), author = character(), version = character(), creation_datetime = character(), update_datetime = character())
      }
      
      if (nrow(r$remote_git_scripts) > 0){
        r$remote_git_scripts <- r$remote_git_scripts %>%
          dplyr::left_join(
            r$local_scripts %>% dplyr::select(unique_id, local_script_version = version),
            by = "unique_id"
          ) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(compare_versions = compareVersion(local_script_version, version)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(action = dplyr::case_when(
            is.na(local_script_version) ~ as.character(tagList(
              actionButton("add_remote_git_script_%unique_id%", "", icon = icon("plus"),
                onclick = paste0("Shiny.setInputValue('", id, "-add_remote_git_script', this.id, {priority: 'event'})")))),
            !is.na(local_script_version) & compare_versions == -1 ~ as.character(tagList(
              actionButton("add_remote_git_script_%unique_id%", "", icon = icon("refresh"),
                onclick = paste0("Shiny.setInputValue('", id, "-add_remote_git_script', this.id, {priority: 'event'})")))),
            TRUE ~ ""
          )) %>%
          dplyr::mutate(action = stringr::str_replace_all(action, "%unique_id%", unique_id)) %>%
          dplyr::select(-local_script_version, -compare_versions)
        
        remote_git_scripts <- r$remote_git_scripts
      }
      
      # Create datatable if doesn't exist

      if (length(r$remote_scripts_datatable_proxy) == 0){

        sortable_cols <- c("name", "creation_datetime", "update_datetime", "category")
        column_widths <- c("creation_datetime" = "130px", "update_datetime" = "130px", "author" = "100px", "action" = "80px", "version" = "80px")
        centered_cols <- c("author", "creation_datetime", "update_datetime", "version", "category", "action")
        searchable_cols <- c("name", "category", "author")
        factorize_cols <- c("category", "author")
        hidden_cols <- c("unique_id", "description")
        col_names <- get_col_names("remote_git_scripts", i18n)

        render_datatable(output = output, ns = ns, i18n = i18n, data = remote_git_scripts,
          col_names = col_names, output_name = "remote_git_scripts_datatable", selection = "single",
          sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, factorize_cols = factorize_cols, filter = TRUE, hidden_cols = hidden_cols)
      }

      if (length(r$remote_git_scripts_datatable_proxy) > 0){
        r$remote_git_scripts_datatable_proxy <- DT::dataTableProxy("remote_git_scripts_datatable", deferUntilFlush = FALSE)
        DT::replaceData(r$remote_git_scripts_datatable_proxy, r$remote_git_scripts, resetPaging = FALSE, rownames = FALSE)
      }
    })
    
    # Download a script from remote git
    
    observeEvent(input$add_remote_git_script, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$add_remote_git_script"))
      
      unique_id <- substr(input$add_remote_git_script, nchar("add_remote_git_script_") + 1, nchar(input$add_remote_git_script))

      script_updated <- FALSE
      link_id <- integer(0)
      
      # Delete old script if exists
      if (nrow(r$options %>% dplyr::filter(category == "script" & name == "unique_id" & value == unique_id)) > 0){
        
        link_id <- r$options %>% dplyr::filter(category == "script" & name == "unique_id" & value == unique_id) %>% dplyr::pull(link_id)
        
        sql <- glue::glue_sql("DELETE FROM options WHERE category = 'script' & link_id = {link_id}", .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql) -> query
        DBI::dbClearResult(query)
        r$options <- r$options %>% dplyr::filter(category != "script" | (category == "script" & link_id != !!link_id))

        sql <- glue::glue_sql("DELETE FROM code WHERE category = 'script' & link_id = {link_id}", .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql) -> query
        DBI::dbClearResult(query)
        r$code <- r$code %>% dplyr::filter(category != "script" | (category == "script" & link_id != !!link_id))

        sql <- glue::glue_sql("DELETE FROM scripts WHERE id = {link_id}", .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql) -> query
        DBI::dbClearResult(query)
        r$scripts <- r$scripts %>% dplyr::filter(id != link_id)
        
        script_updated <- TRUE
      }
      
      # Add new script
      
      script <- r$remote_git_scripts_full %>% dplyr::filter(unique_id == !!unique_id)
      
      last_row <- list()
      for (name in c("scripts", "options", "code")) last_row[[name]] <- get_last_row(r$db, name)
      
      new_data <- list()
      
      link_id <- last_row$scripts + 1
      
      new_data$scripts <- tibble::tibble(id = last_row$scripts + 1, name = script[[paste0("name_", language)]], 
        creation_datetime = script$creation_datetime, update_datetime = script$update_datetime, deleted = FALSE)
      
      for (name in c("description_fr", "description_en", "code")) script[[name]] <- script[[name]] %>% stringr::str_replace_all("'", "''")
      
      new_data$options <- tibble::tribble(
        ~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
        last_row$options + 1, "script", link_id, "version", script$version, NA_integer_, r$user_id, script$creation_datetime, FALSE,
        last_row$options + 2, "script", link_id, "unique_id", script$unique_id, NA_integer_, r$user_id, script$creation_datetime, FALSE,
        last_row$options + 3, "script", link_id, "author", script$author, NA_integer_, r$user_id, script$creation_datetime, FALSE,
        last_row$options + 4, "script", link_id, "description_fr", script$description_fr, NA_integer_, r$user_id, script$creation_datetime, FALSE,
        last_row$options + 5, "script", link_id, "description_en", script$description_en, NA_integer_, r$user_id, script$creation_datetime, FALSE,
        last_row$options + 6, "script", link_id, "category_fr", script$category_fr, NA_integer_, r$user_id, script$creation_datetime, FALSE,
        last_row$options + 7, "script", link_id, "category_en", script$category_en, NA_integer_, r$user_id, script$creation_datetime, FALSE,
        last_row$options + 8, "script", link_id, "name_fr", script$name_fr, NA_integer_, r$user_id, script$creation_datetime, FALSE,
        last_row$options + 9, "script", link_id, "name_en", script$name_en, NA_integer_, r$user_id, script$creation_datetime, FALSE)
      
      new_data$code <- tibble::tribble(
        ~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
        last_row$code + 1, "script", link_id, script$code, r$user_id, script$creation_datetime, FALSE)
      
      for (name in c("scripts", "options", "code")){
        DBI::dbAppendTable(r$db, name, new_data[[name]])
        r[[name]] <- r[[name]] %>% dplyr::bind_rows(new_data[[name]])
      }
      
      if (script_updated) show_message_bar(output, message = "script_updated", type = "success", i18n = i18n, ns = ns)
      else show_message_bar(output, message = "script_imported", type = "success", i18n = i18n, ns = ns)
      
      r$local_scripts <- r$local_scripts %>%
        dplyr::filter(id != link_id) %>%
        dplyr::bind_rows(tibble::tibble(
          id = link_id, name = script[[paste0("name_", language)]], unique_id = script$unique_id,
          description = script[[paste0("description_", language)]], category = script[[paste0("category_", language)]],
          author = script$author, version = script$version, creation_datetime = script$creation_datetime, update_datetime = script$update_datetime
        ))
      
      r$update_remote_git_scripts_datatable <- Sys.time()
    })
    
    # --- --- --- -- -- --
    # Create a script ----
    # --- --- --- -- -- --
    
    observeEvent(input$add_script, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$add_script"))
      
      new_data <- list()
      new_data$name <- coalesce2(type = "char", x = input$script_name)
      new_data$script_name <- new_data$name
      
      add_settings_new_data(session = session, output = output, r = r, m = m, i18n = i18n, id = "scripts",
        data = new_data, table = "scripts", required_textfields = "script_name", req_unique_values = "name")
      
    })
    
    # --- --- --- --- --- ---
    # Scripts management ----
    # --- --- --- --- --- ---
    
    action_buttons <- c("delete", "edit_code", "options")
    sortable_cols <- c("id", "name", "creation_datetime", "update_datetime")
    column_widths <- c("id" = "80px", "creation_datetime" = "130px", "update_datetime" = "130px", "action" = "100px")
    centered_cols <- c("id", "creator", "creation_datetime", "update_datetime", "action")
    searchable_cols <- c("name")
    hidden_cols <- c("id", "description", "deleted", "modified")
    col_names <- get_col_names("scripts", i18n)
    
    # Prepare data for datatable
    
    observeEvent(r$scripts, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$scripts 2"))
      
      # data_source_id <- r$datasets %>% dplyr::filter(id == r$selected_dataset) %>% dplyr::pull(data_source_id)
      
      if(nrow(r$scripts) == 0){
        
        data_scripts_datatable <- tibble::tibble(id = integer(), name = character(),
          creation_datetime = character(), update_datetime = character(), deleted = integer(), modified = logical(), action = character())
        data_export_scripts_datatable <- data_scripts_datatable
      }
      
      if(nrow(r$scripts) > 0){
        
        r$scripts_temp <- r$scripts %>%
          dplyr::mutate_at(c("creation_datetime", "update_datetime"), format_datetime, language = "en", sec = FALSE) %>%
          dplyr::mutate(modified = FALSE)
        
        # Reset selected scripts for export_scripts and export_scripts_selected
        r$export_scripts_temp  <- r$scripts_temp
        r$export_scripts_selected <- r$export_scripts_temp %>% dplyr::slice(0)
        
        # Prepare data for datatables
        
        r$scripts_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
          table = "scripts", action_buttons = action_buttons, data_input = r$scripts_temp)
        data_scripts_datatable <- r$scripts_datatable_temp
        
        r$export_scripts_datatable_temp <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
          table = "scripts", action_buttons = "add", data_input = r$export_scripts_temp)
        data_export_scripts_datatable <- r$export_scripts_datatable_temp
      }
      
      # Render datatables
      
      if (length(r$scripts_datatable_proxy) == 0){
        
        render_datatable(output = output, ns = ns, i18n = i18n,
          data = data_scripts_datatable, 
          col_names = col_names, output_name = "scripts_datatable", selection = "multiple",
          sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, hidden_cols = hidden_cols)
        
        render_datatable(output = output, ns = ns, i18n = i18n, data = data_export_scripts_datatable,
          output_name = "scripts_to_export_datatable", col_names = col_names,
          sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, hidden_cols = hidden_cols)
        
        # Create a proxy for datatables
        
        r$scripts_datatable_proxy <- DT::dataTableProxy("scripts_datatable", deferUntilFlush = FALSE)
        r$scripts_to_export_datatable_proxy <- DT::dataTableProxy("scripts_to_export_datatable", deferUntilFlush = FALSE)
      }
      
      else {
        DT::replaceData(r$scripts_datatable_proxy, data_scripts_datatable, resetPaging = FALSE, rownames = FALSE)
        DT::replaceData(r$scripts_to_export_datatable_proxy, data_export_scripts_datatable, resetPaging = FALSE, rownames = FALSE)
      }
      
      # Reload remote git scripts datatable
      r$update_remote_git_scripts_datatable <- Sys.time()
      
      # Reload bucket_list scripts var
      r$scripts_reload_bucket_list_var <- Sys.time()
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer r$scripts"))
    })
    
    # Updates on datatable data
    observeEvent(input$scripts_datatable_cell_edit, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$scripts_datatable_cell_edit"))
      
      edit_info <- input$scripts_datatable_cell_edit
      r$scripts_temp <- DT::editData(r$scripts_temp, edit_info, rownames = FALSE)
      
      # Store that this row has been modified
      r$scripts_temp[[edit_info$row, "modified"]] <- TRUE
    })
    
    # Save updates
    
    observeEvent(input$save_scripts_management, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$save_scripts_management"))
      
      req(nrow(r$scripts) > 0)
      
      save_settings_datatable_updates(output = output, r = r, ns = ns, table = "scripts", i18n = i18n, duplicates_allowed = FALSE)
      
      # Update sidenav dropdown with the new study
      r$reload_scripts <- Sys.time()
    })
    
    # Delete a row in datatable
    
    script_delete_prefix <- "script"
    script_dialog_title <- "scripts_delete"
    script_dialog_subtext <- "scripts_delete_subtext"
    script_react_variable <- "script_delete_confirm"
    script_table <- "scripts"
    script_id_var_sql <- "id"
    script_id_var_r <- "delete_scripts"
    script_delete_message <- "script_deleted"
    script_reload_variable <- "reload_scripts"
    script_information_variable <- "script_deleted"
    script_delete_variable <- paste0(script_delete_prefix, "_open_dialog")
    
    delete_element(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
      delete_prefix = script_delete_prefix, dialog_title = script_dialog_title, dialog_subtext = script_dialog_subtext,
      react_variable = script_react_variable, table = script_table, id_var_sql = script_id_var_sql, id_var_r = script_id_var_r,
      delete_message = script_delete_message, translation = TRUE, reload_variable = script_reload_variable,
      information_variable = script_information_variable)
    
    # Delete one row (with icon on DT)
    
    observeEvent(input$deleted_pressed, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$deleted_pressed"))
      
      r$delete_scripts <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
      r[[script_delete_variable]] <- TRUE
      reset_scripts_fields(session = session)
    })
    
    # Delete multiple rows (with "Delete selection" button)
    
    observeEvent(input$delete_selection, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$delete_selection"))
      
      req(length(input$scripts_datatable_rows_selected) > 0)
      
      r$delete_scripts <- r$scripts_temp[input$scripts_datatable_rows_selected, ] %>% dplyr::pull(id)
      r[[script_delete_variable]] <- TRUE
      reset_scripts_fields(session = session)
    })
    
    # observeEvent(input$deleted_pressed, {
    #   
    #   if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$deleted_pressed"))
    # 
    #   r$delete_script <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
    #   r[[script_delete_variable]] <- TRUE
    # 
    #   reset_scripts_fields(session = session)
    #   
    #   # Reload datatable (to unselect rows)
    #   DT::replaceData(r$scripts_datatable_proxy, r$scripts_datatable_temp, resetPaging = FALSE, rownames = FALSE)
    # })
    
    observeEvent(r$reload_scripts, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$reload_scripts"))
      
      # Reload sidenav dropdown with reloading scripts
      # update_r(r = r, table = "scripts")
      
      # Reload datatable
      r$scripts_temp <- r$scripts %>% 
        dplyr::mutate_at(c("creation_datetime", "update_datetime"), format_datetime, language = "en", sec = FALSE) %>%
        dplyr::mutate(modified = FALSE)
    })
    
    observeEvent(input$edit_code, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$edit_code"))
      
      link_id <- as.integer(substr(input$edit_code, nchar("edit_code_") + 1, nchar(input$edit_code)))
      
      options <- convert_tibble_to_list(r$scripts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = link_id, text = r$scripts %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_script", options = options, value = value)
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_script", options = options, value = value)
      
      # Set current pivot to edit_plugins_code
      shinyjs::runjs(glue::glue("$('#{id}-scripts_pivot button[name=\"{i18n$t('edit_script_code')}\"]').click();"))
    })
    
    observeEvent(input$options, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$options"))
      
      # Get link_id variable, to update options div
      link_id <- as.integer(substr(input$options, nchar("options_") + 1, nchar(input$options)))
      
      options <- convert_tibble_to_list(r$scripts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = link_id, text = r$scripts %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_script", options = options, value = value)
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_script", options = options, value = value)
      
      # Set current pivot to edit_plugins_code
      shinyjs::runjs(glue::glue("$('#{id}-scripts_pivot button[name=\"{i18n$t('script_options')}\"]').click();"))
    })
    
    # --- --- --- --- --- -
    # Edit script code ----
    # --- --- --- --- --- -
    
    observeEvent(input$code_selected_script, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$code_selected_script"))
      
      if (length(input$code_selected_script) > 1) link_id <- input$code_selected_script$key
      else link_id <- input$code_selected_script
      if (length(input$options_selected_script) > 0){
        if (length(input$options_selected_script) > 1) options_link_id <- input$options_selected_script$key
        else options_link_id <- input$options_selected_script
      }
      else options_link_id <- 0L
      
      if (link_id != options_link_id){
        options <- convert_tibble_to_list(r$scripts %>% dplyr::arrange(name), key_col = "id", text_col = "name")
        value <- list(key = link_id, text = r$scripts %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
        shiny.fluent::updateComboBox.shinyInput(session, "options_selected_script", options = options, value = value)
      }
      
      # Get code from database
      code <- r$code %>% dplyr::filter(category == "script" & link_id == !!link_id) %>% dplyr::pull(code) %>% stringr::str_replace_all("''", "'")
      
      shinyAce::updateAceEditor(session, "ace_edit_code", value = code)
      
    })
    
    # Save updates
    
    observeEvent(input$ace_edit_code_save, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$ace_edit_code_save"))
      r$script_save_code <- Sys.time()
    })
    observeEvent(input$save_code, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$save_code"))
      r$script_save_code <- Sys.time()
    })
    
    observeEvent(r$script_save_code, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$script_save_code"))
      
      if (length(input$code_selected_script) > 1) link_id <- input$code_selected_script$key
      else link_id <- input$code_selected_script
      
      req(!is.null(link_id))
      
      # Update code
      
      code_id <- r$code %>% dplyr::filter(category == "script" & link_id == !!link_id) %>% dplyr::pull(id)
      
      ace_edit_code <- stringr::str_replace_all(input$ace_edit_code, "'", "''")
      sql <- glue::glue_sql("UPDATE code SET code = {ace_edit_code} WHERE id = {code_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql) -> query
      DBI::dbClearResult(query)
      r$code <- r$code %>% dplyr::mutate(code = dplyr::case_when(id == code_id ~ ace_edit_code, TRUE ~ code))
      
      # Update datetime in plugins table
      
      new_update_datetime <- as.character(Sys.time())
      sql <- glue::glue_sql("UPDATE scripts SET update_datetime = {new_update_datetime} WHERE id = {link_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      r$scripts <- r$scripts %>% dplyr::mutate(update_datetime = dplyr::case_when(id == link_id ~ new_update_datetime, TRUE ~ update_datetime))
      
      # Notify user
      show_message_bar(output,  "modif_saved", "success", i18n, ns = ns)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer r$scripts_save_code"))
    })
    
    # Execute code
    
    observeEvent(input$execute_code, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$execute_code"))
      r$script_code <- input$ace_edit_code
      r$script_code_trigger <- Sys.time()
    })
    
    observeEvent(input$ace_edit_code_run_selection, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$ace_edit_code_run_selection"))
      if(!shinyAce::is.empty(input$ace_edit_code_run_selection$selection)) r$script_code <- input$ace_edit_code_run_selection$selection
      else r$script_code <- input$ace_edit_code_run_selection$line
      r$script_code_trigger <- Sys.time()
    })
    
    observeEvent(input$ace_edit_code_run_all, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$ace_edit_code_run_all"))
      r$script_code <- input$ace_edit_code
      r$script_code_trigger <- Sys.time()
    })
    
    observeEvent(r$script_code_trigger, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$script_code_trigger"))
      
      edited_code <- r$script_code %>% stringr::str_replace_all("\r", "\n")
      
      # Variables to hide
      new_env_vars <- list("r" = NA)
      # Variables to keep
      for (var in c("d", "m", "output", "i18n")) new_env_vars[[var]] <- eval(parse(text = var))
      new_env <- rlang::new_environment(data = new_env_vars, parent = pryr::where("r"))
      
      options('cli.num_colors' = 1)
      
      # Capture console output of our code
      captured_output <- capture.output(
        tryCatch(eval(parse(text = edited_code), envir = new_env), error = function(e) print(e), warning = function(w) print(w)))
      
      # Restore normal value
      options('cli.num_colors' = NULL)
      
      output$datetime_code_execution <- renderText(format_datetime(Sys.time(), language))
      output$console_result <- renderText(paste(captured_output, collapse = "\n"))
    })
    
    # Hide ace editor
    
    observeEvent(input$hide_code_editor, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$hide_code_editor"))
      
      if (input$hide_code_editor){
        shinyjs::hide("ace_edit_code")
        shinyjs::show("div_br") 
      }
      else {
        shinyjs::show("ace_edit_code")
        shinyjs::hide("div_br") 
      }
    })
    
    # --- --- --- --- -- -- --
    # Edit script options ----
    # --- --- --- --- -- -- --
    
    observeEvent(input$options_selected_script, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$options_selected_script"))
      
      if (length(input$options_selected_script) > 1) link_id <- input$options_selected_script$key
      else link_id <- input$options_selected_script
      if (length(input$code_selected_script) > 0){
        if (length(input$code_selected_script) > 1) code_link_id <- input$code_selected_script$key
        else code_link_id <- input$code_selected_script
      }
      else code_link_id <- 0L
      
      if (link_id != code_link_id)
        shiny.fluent::updateComboBox.shinyInput(session, "code_selected_script", 
          options = convert_tibble_to_list(r$scripts %>% dplyr::arrange(name), key_col = "id", text_col = "name"),
          value = list(key = link_id, text = r$scripts %>% dplyr::filter(id == link_id) %>% dplyr::pull(name)))
      
      options <- r$options %>% dplyr::filter(category == "script", link_id == !!link_id)
      
      for (field in c("version", "author", "name_fr", "name_en", "category_fr", "category_en")){
        value <- options %>% dplyr::filter(name == field) %>% dplyr::pull(value) %>% stringr::str_replace_all("''", "'")
        if (is.na(value)) value <- ""
        
        shiny.fluent::updateTextField.shinyInput(session, paste0("script_", field), value = value)
      } 
      
      for (field in c("description_fr", "description_en")) shinyAce::updateAceEditor(session,
        paste0("script_", field), value = options %>% dplyr::filter(name == field) %>% dplyr::pull(value) %>% stringr::str_replace_all("''", "'"))
      
    })
    
    # Save updates
    
    observeEvent(input$script_description_fr_save, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$script_description_fr_save"))
      r$script_save_options <- Sys.time()
    })
    observeEvent(input$script_description_en_save, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$script_description_en_save"))
      r$script_save_options <- Sys.time()
    })
    observeEvent(input$save_options_description, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$save_options_description"))
      r$script_save_options <- Sys.time()
    })
    
    observeEvent(r$script_save_options, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer r$script_save_options"))
      
      req(length(input$options_selected_script) > 0)
      if (length(input$options_selected_script) > 1) link_id <- input$options_selected_script$key
      else link_id <- input$code_selected_script
      
      script_name <- input[[paste0("script_name_", language)]]
      if (is.na(script_name) | script_name == "") shiny.fluent::updateTextField.shinyInput(session, 
        paste0("script_name_", language), errorMessage = i18n$t("provide_valid_name"))
      
      req(!is.na(script_name) & script_name != "")
      
      duplicate_names <- FALSE
      current_names <- r$scripts_temp %>% dplyr::filter(id != link_id) %>% dplyr::pull(name)
      if (script_name %in% current_names){
        duplicate_names <- TRUE
        shiny.fluent::updateTextField.shinyInput(session, paste0("script_name_", language), errorMessage = i18n$t("name_already_used"))
      }
      
      req(!duplicate_names)
      
      if (!is.na(script_name) & script_name != "") shiny.fluent::updateTextField.shinyInput(session, 
        paste0("script_name_", language), errorMessage = NULL)
      
      data <- list()
      for (field in c("script_version", "script_author",
        "script_name_fr", "script_name_en", "script_category_fr", "script_category_en",
        "script_description_fr", "script_description_en")) data[[stringr::str_replace(field, "script_", "")]] <- input[[field]]
      
      save_settings_options(output = output, r = r, id = id, category = "script", code_id_input = paste0("options_", link_id),
        i18n = i18n, data = data, page_options = c("version", "author", "description_fr", "description_en",
          "name_fr", "name_en", "category_fr", "category_en"))
      
      # Change script_name & update_datetime in scripts table
      new_update_datetime <- as.character(Sys.time())
      sql <- glue::glue_sql("UPDATE scripts SET name = {script_name}, update_datetime = {new_update_datetime} WHERE id = {link_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      r$scripts <- r$scripts %>% dplyr::mutate(
        name = dplyr::case_when(id == link_id ~ script_name, TRUE ~ name),
        update_datetime = dplyr::case_when(id == link_id ~ new_update_datetime, TRUE ~ update_datetime))
      r$scripts_temp <- r$scripts %>%
        dplyr::mutate_at(c("creation_datetime", "update_datetime"), format_datetime, language = "en", sec = FALSE) %>%
        dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer input$save_options_description"))
    })
    
    # Render markdown
    
    observeEvent(input$execute_options_description, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$execute_options_description"))
      r$script_options_description_trigger <- Sys.time()
    })
    
    observeEvent(input$script_description_fr_run_all, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$script_description_fr_run_all"))
      r$script_options_description_trigger <- Sys.time()
    })
    
    observeEvent(input$script_description_en_run_all, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$script_description_en_run_all"))
      r$script_options_description_trigger <- Sys.time()
    })
    
    observeEvent(r$script_options_description_trigger, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$execute_options_description"))
      
      options_description <- isolate(input[[paste0("script_description_", input$script_description_language)]] %>% stringr::str_replace_all("\r", "\n"))
      
      tryCatch({
        
        # Clear temp dir
        unlink(paste0(r$app_folder, "/temp_files"), recursive = TRUE, force = TRUE)
        
        markdown_settings <- paste0("```{r setup, include=FALSE}\nknitr::opts_knit$set(root.dir = '", 
          r$app_folder, "/temp_files')\n",
          "knitr::opts_chunk$set(root.dir = '", r$app_folder, "/temp_files', fig.path = '", r$app_folder, "/temp_files')\n```\n")
        
        markdown_file <- paste0(markdown_settings, options_description)
        
        # Create temp dir
        dir <- paste0(r$app_folder, "/temp_files")
        file <- paste0(dir, "/", as.character(Sys.time()) %>% stringr::str_replace_all(":", "_"), ".Md")
        if (!dir.exists(dir)) dir.create(dir)
        
        # Create the markdown file
        knitr::knit(text = markdown_file, output = file, quiet = TRUE)
        
        output$description_markdown_result <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(file))))
      }, error = function(e) "")
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer input$execute_options_description"))
    })
    
    # --- --- --- --- - -
    # Import scripts ----
    # --- --- --- --- - -
    
    observeEvent(input$import_scripts_browse, {
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$import_scripts_browse"))
      shinyjs::click("import_scripts_upload")
    })
    
    output$import_scripts_status <- renderUI({
      if (debug) print(paste0(Sys.time(), " - mod_scripts - output$import_scripts_status"))
      
      tagList(div(
        span(i18n$t("loaded_file"), " : ", style = "padding-top:5px;"),
        span(input$import_scripts_upload$name, style = "font-weight:bold; color:#0078D4;"), style = "padding-top:5px;"))
    })
    
    observeEvent(input$import_scripts_button, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$import_scripts_button"))
      
      req(input$import_scripts_upload)
      
      tryCatch({
        
        # Extract ZIP file
        
        temp_dir <- paste0(r$app_folder, "/temp_files/", Sys.time() %>% stringr::str_replace_all(":| |-", ""), paste0(sample(c(0:9, letters[1:6]), 24, TRUE), collapse = ''))
        zip::unzip(input$import_scripts_upload$datapath, exdir = temp_dir)
        
        # Read XML file
        
        scripts <-
          xml2::read_xml(paste0(temp_dir, "/scripts/scripts.xml")) %>%
          XML::xmlParse() %>%
          XML::xmlToDataFrame(nodes = XML::getNodeSet(., "//script")) %>%
          tibble::as_tibble() %>%
          dplyr::left_join(
            r$scripts %>%
              dplyr::inner_join(
                r$options %>% dplyr::filter(category == "script", name == "unique_id") %>% dplyr::select(id = link_id, unique_id = value),
                by = "id"
              ) %>%
              dplyr::select(id, unique_id),
            by = "unique_id"
          ) %>%
          dplyr::mutate(name = dplyr::case_when(
            language == "fr" ~ name_fr, TRUE ~ name_en
          )) %>%
          dplyr::relocate(id)
        
        if (!input$replace_already_existing_scripts) scripts <- scripts %>% dplyr::filter(is.na(id))
        
        # Loop over each script
        
        if (nrow(scripts) > 0){
          
          for (i in 1:nrow(scripts)){
            
            script <- scripts[i, ]
            
            # Delete old rows
            
            if (!is.na(script$id)){
              
              sql <- glue::glue_sql("DELETE FROM scripts WHERE id = {script$id}", .con = r$db)
              query <- DBI::dbSendStatement(r$db, sql)
              DBI::dbClearResult(query)
              r$scripts <- r$scripts %>% dplyr::filter(id != script$id)
              
              sql <- glue::glue_sql("DELETE FROM options WHERE category = 'script' AND link_id = {script$id}", .con = r$db)
              query <- DBI::dbSendStatement(r$db, sql)
              DBI::dbClearResult(query)
              r$options <- r$options %>% dplyr::filter(link_id != script$id | (link_id == script$id & category != "script"))
              
              sql <- glue::glue_sql("DELETE FROM code WHERE category = 'script' AND link_id = {script$id}", .con = r$db)
              query <- DBI::dbSendStatement(r$db, sql)
              DBI::dbClearResult(query)
              r$code <- r$code %>% dplyr::filter(link_id != script$id | (link_id == script$id & category == "script"))
            }
            
            for (name in c("description_fr", "description_en", "code")) script[[name]] <- script[[name]] %>% stringr::str_replace_all("'", "''")
            
            # Scripts table
            
            new_row <- get_last_row(r$db, "scripts") + 1
            
            new_data <- tibble::tribble(
              ~id, ~name, ~creation_datetime, ~update_datetime, ~deleted,
              new_row, as.character(script[[paste0("name_", language)]]), script$creation_datetime, script$update_datetime, FALSE)
            
            DBI::dbAppendTable(r$db, "scripts", new_data)
            r$scripts <- r$scripts %>% dplyr::bind_rows(new_data)
            add_log_entry(r = r, category = paste0("scripts - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_data))
            
            # Options table
            
            last_row_options <- get_last_row(r$db, "options")
            
            new_options <- tibble::tribble(~id, ~category, ~link_id, ~name, ~value, ~value_num, ~creator_id, ~datetime, ~deleted,
              last_row_options + 1, "script", new_row, "version", script$version, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
              last_row_options + 2, "script", new_row, "unique_id", script$unique_id, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
              last_row_options + 3, "script", new_row, "author", script$author, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
              last_row_options + 4, "script", new_row, "description_fr", script$description_fr, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
              last_row_options + 5, "script", new_row, "description_en", script$description_en, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
              last_row_options + 6, "script", new_row, "category_fr", script$category_fr, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
              last_row_options + 7, "script", new_row, "category_en", script$category_en, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
              last_row_options + 8, "script", new_row, "name_fr", script$name_fr, NA_integer_, r$user_id, as.character(Sys.time()), FALSE,
              last_row_options + 9, "script", new_row, "name_en", script$name_en, NA_integer_, r$user_id, as.character(Sys.time()), FALSE
            )
            
            DBI::dbAppendTable(r$db, "options", new_options)
            r$options <- r$options %>% dplyr::bind_rows(new_options)
            add_log_entry(r = r, category = paste0("code", " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_options))
            
            # Code table
            last_row_code <- get_last_row(r$db, "code")
            
            new_code <- tibble::tribble(
              ~id, ~category, ~link_id, ~code, ~creator_id, ~datetime, ~deleted,
              last_row_code + 1, "script", new_row, script$code, r$user_id, as.character(Sys.time()), FALSE)
            
            DBI::dbAppendTable(r$db, "code", new_code)
            r$code <- r$code %>% dplyr::bind_rows(new_code)
            add_log_entry(r = r, category = paste0("code", " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_code))
            
            # Copy files
            # Create folder if doesn't exist
            script_dir <- paste0(r$app_folder, "/scripts/", script$unique_id)
            if (!dir.exists(script_dir)) dir.create(script_dir, recursive = TRUE)
            
            list_of_files <- list.files(paste0(temp_dir, "/scripts/", script$unique_id))
            
            # Copy files to temp dir
            file.copy(
              paste0(paste0(temp_dir, "/scripts/", script$unique_id), "/", list_of_files),
              paste0(script_dir, "/", list_of_files),
              overwrite = TRUE
            )
            
            r$show_script_details <- Sys.time()
            
            # Reload datatable
            r$scripts_temp <- r$scripts %>%
              dplyr::mutate_at(c("creation_datetime", "update_datetime"), format_datetime, language = "en", sec = FALSE) %>%
              dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
          }
        }
        
        # Show imported scripts
        
        col_names <- c(i18n$t("id"), i18n$t("name"), i18n$t("version"), i18n$t("unique_id"),
          i18n$t("description_fr"), i18n$t("description_en"), i18n$t("app_version"), i18n$t("author"),
          i18n$t("name"), i18n$t("name"), i18n$t("category"), i18n$t("category"), i18n$t("created_on"), i18n$t("updated_on"), i18n$t("code"))
        centered_cols <- c("author", "version", "id", "creation_datetime", "update_datetime")
        column_widths <- c("author" = "100px", "version" = "80px", "id" = "50px", "creation_datetime" = "130px", "update_datetime" = "130px")
        hidden_cols <- c("id", "type", "unique_id", "image", "app_version", "description_fr", "description_en",
          "name_en", "name_fr", "category_en", "category_fr", "code")
        
        shinyjs::show("imported_scripts_div")
        
        print(scripts)
        
        render_datatable(output = output, ns = ns, i18n = i18n,
          data = scripts %>% dplyr::mutate_at(c("creation_datetime", "update_datetime"), format_datetime, language = "en", sec = FALSE),
          output_name = "imported_scripts", col_names = col_names, centered_cols = centered_cols, column_widths = column_widths,
          filter = FALSE, hidden_cols = hidden_cols, datatable_dom = "")
        
        show_message_bar(output,  "success_importing_script", "success", i18n = i18n, time = 15000, ns = ns)
      },
        error = function(e) report_bug(r = r, output = output, error_message = "error_importing_script",
          error_name = paste0(id, " - import scripts"), category = "Error", error_report = toString(e), i18n = i18n, ns = ns),
        warning = function(w) report_bug(r = r, output = output, error_message = "error_importing_script",
          error_name = paste0(id, " - import scripts"), category = "Error", error_report = w, i18n = i18n, ns = ns))
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - observer input$import_scripts_button"))
    })
    
    # --- --- --- --- - -
    # Export scripts ----
    # --- --- --- --- - -
    
    # When add button is clicked
    observeEvent(input$add_item, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$add_item"))
      
      # Get ID of selected script
      link_id <- as.integer(substr(input$add_item, nchar("add_item_") + 1, nchar(input$add_item)))
      
      # If this script is not already selected, add it to the selected items dropdown
      
      value <- integer(1)
      if (nrow(r$export_scripts_selected) > 0) value <- r$export_scripts_selected %>% dplyr::pull(id)
      
      if (link_id %not_in% value){
        
        r$export_scripts_selected <- r$export_scripts_temp %>% dplyr::filter(id == link_id) %>%
          dplyr::bind_rows(r$export_scripts_selected)
        
        # Update dropdown of selected items
        options <- convert_tibble_to_list(r$export_scripts_selected, key_col = "id", text_col = "name", i18n = i18n)
        value <- r$export_scripts_selected %>% dplyr::pull(id)
        shiny.fluent::updateDropdown.shinyInput(session, "scripts_to_export",
          options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
      }
      
    })
    
    # When dropdown is modified
    observeEvent(input$scripts_to_export_trigger, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$scripts_to_export"))
      
      r$export_scripts_selected <- r$export_scripts_selected %>%
        dplyr::filter(id %in% input$scripts_to_export)
      
      options <- convert_tibble_to_list(r$export_scripts_selected, key_col = "id", text_col = "name", i18n = i18n)
      value <- r$export_scripts_selected %>% dplyr::pull(id)
      shiny.fluent::updateDropdown.shinyInput(session, "scripts_to_export",
        options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
    })
    
    # Export scripts
    observeEvent(input$export_selected_scripts, {
      
      if (debug) print(paste0(Sys.time(), " - mod_scripts - observer input$export_scripts"))
      
      req(nrow(r$export_scripts_selected) > 0)
      
      shinyjs::click("export_scripts_download")
    })
    
    output$export_scripts_download <- downloadHandler(
      
      filename = function() paste0("linkr_export_scripts_",
        Sys.time() %>% stringr::str_replace_all(" ", "_") %>% stringr::str_replace_all(":", "_") %>% as.character(), ".zip"),
      
      content = function(file){
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_scripts - output$export_scripts_download"))
        
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        
        temp_dir <- paste0(r$app_folder, "/temp_files/", Sys.time() %>% stringr::str_replace_all(":| |-", ""), paste0(sample(c(0:9, letters[1:6]), 24, TRUE), collapse = ''))
        dir.create(paste0(temp_dir, "/scripts"), recursive = TRUE)
        
        for (script_id in r$export_scripts_selected %>% dplyr::pull(id)){
          
          script <- r$scripts %>% dplyr::filter(id == script_id)
          options <- r$options %>% dplyr::filter(category == "script", link_id == script_id)
          code <- r$code %>% dplyr::filter(link_id == script_id, category == "script") %>% dplyr::pull(code) %>% stringr::str_replace_all("''", "'")
          
          # Create folder if doesn't exist
          script_dir <- paste0(r$app_folder, "/scripts/", options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
          if (!dir.exists(script_dir)) dir.create(script_dir, recursive = TRUE)
          
          # Create ui.R & server.R
          # writeLines(code %>% dplyr::filter(category == "script") %>% dplyr::pull(code), paste0(script_dir, "/", name, ".R"))
          
          # Create XML file
          xml <- XML::newXMLDoc()
          scripts_node <- XML::newXMLNode("scripts", doc = xml)
          script_node <- XML::newXMLNode("script", parent = scripts_node, doc = xml)
          XML::newXMLNode("app_version", r$app_version, parent = script_node)
          for(name in c("unique_id", "version", "author",  "name_fr", "name_en", "category_fr", "category_en")) XML::newXMLNode(name, 
            options %>% dplyr::filter(name == !!name) %>% dplyr::pull(value), parent = script_node)
          for(name in c("description_fr", "description_en")) XML::newXMLNode(name,
            options %>% dplyr::filter(name == !!name) %>% dplyr::pull(value) %>% stringr::str_replace_all("''", "'"), parent = script_node)
          for (name in c("creation_datetime", "update_datetime")) XML::newXMLNode(name, script %>% dplyr::pull(get(!!name)), parent = script_node)
          XML::newXMLNode("code", code, parent = script_node)
          XML::saveXML(xml, file = paste0(script_dir, "/script.xml"))
          
          list_of_files <- list.files(script_dir)
          
          # Copy files to temp dir
          temp_dir_copy <- paste0(temp_dir, "/scripts/", options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
          if (!dir.exists(temp_dir_copy)) dir.create(temp_dir_copy, recursive = TRUE)
          file.copy(
            paste0(script_dir, "/", list_of_files),
            paste0(temp_dir_copy, "/", list_of_files),
            overwrite = TRUE
          )
        }
        
        # Create XML file with all exported scripts
        scripts_dir <- paste0(temp_dir, "/scripts")
        
        scripts_tibble <- tibble::tibble(app_version = character(), unique_id = character(), version = character(), author = character(),
          name_fr = character(), name_en = character(), category_fr = character(), category_en = character(), 
          description_fr = character(), description_en = character(), creation_datetime = character(), update_datetime = character(), code = character())
        
        dirs <- list.dirs(scripts_dir, full.names = TRUE)
        for (dir in dirs){
          if (dir != scripts_dir){
            scripts_tibble <-
              scripts_tibble %>%
              dplyr::bind_rows(
                xml2::read_xml(paste0(dir, "/script.xml")) %>%
                  XML::xmlParse() %>%
                  XML::xmlToDataFrame(nodes = XML::getNodeSet(., "//script")) %>%
                  tibble::as_tibble()
              )
          }
        }
        
        scripts_xml <- XML::newXMLDoc()
        scripts_node <- XML::newXMLNode("scripts", doc = scripts_xml)
        
        scripts_nodes <- apply(scripts_tibble, 1, function(x) {
          script_node <- XML::newXMLNode("script")
          XML::addChildren(script_node, lapply(names(x), function(y) XML::newXMLNode(y, x[y])))
        })
        
        XML::xmlParent(scripts_nodes) <- scripts_node
        
        XML::saveXML(scripts_xml, file = paste0(scripts_dir, "/scripts.xml"))
        
        # Create a ZIP
        
        zip::zipr(file, paste0(temp_dir, "/scripts"))
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_scripts - output$export_scripts_download"))
      }
    )
    
  })
}
