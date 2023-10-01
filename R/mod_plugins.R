#' plugins UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plugins_ui <- function(id = character(), i18n = character(), language = tibble::tibble(), languages = character()){
  ns <- NS(id)
  
  cards <- c("all_plugins_card", "plugins_datatable_card", "plugins_edit_code_card", "plugins_options_card", "import_plugin_card", "export_plugin_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  })
  
  vocabulary_concepts_div <- div(
    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 50),
      make_combobox(i18n = i18n, ns = ns, label = "vocabulary", id = "vocabulary", allowFreeform = FALSE, multiSelect = FALSE, width = "300px"),
      make_dropdown(i18n = i18n, ns = ns, label = "columns_concepts", id = "vocabulary_concepts_table_cols", width = "300px", multiSelect = TRUE,
        options = list(
          list(key = 0, text = i18n$t("concept_id")),
          list(key = 1, text = i18n$t("concept_name")),
          list(key = 2, text = i18n$t("concept_display_name")),
          list(key = 3, text = i18n$t("domain_id")),
          list(key = 4, text = i18n$t("concept_class_id")),
          list(key = 5, text = i18n$t("standard_concept")),
          list(key = 6, text = i18n$t("concept_code")),
          list(key = 7, text = i18n$t("num_patients")),
          list(key = 8, text = i18n$t("num_rows")),
          list(key = 9, text = i18n$t("action"))
        ),
        value = c(0, 1, 2, 7, 8, 9)
      ),
      make_dropdown(i18n = i18n, ns = ns, label = "columns_mapped_concepts", id = "vocabulary_mapped_concepts_table_cols", width = "300px", multiSelect = TRUE,
        options = list(
          list(key = 1, text = i18n$t("concept_id")),
          list(key = 2, text = i18n$t("relationship_id")),
          list(key = 3, text = i18n$t("mapped_concept_id")),
          list(key = 4, text = i18n$t("concept_name_2")),
          list(key = 5, text = i18n$t("concept_display_name_2")),
          list(key = 6, text = i18n$t("domain_id")),
          list(key = 7, text = i18n$t("num_patients")),
          list(key = 8, text = i18n$t("num_rows")),
          list(key = 10, text = i18n$t("action"))
        ),
        value = c(2, 3, 4, 5, 7, 8, 9)
      )
    ),
    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
      div(
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
          div(shiny.fluent::Toggle.shinyInput(ns("show_mapped_concepts"), value = TRUE), style = "margin-top:30px; margin-bottom:5px;"),
          div(i18n$t("show_mapped_concepts"), style = "font-weight:bold; margin-top:30px;; margin-bottom:5px;")
        ),
        style = "width:330px;"
      ),
      conditionalPanel(condition = "input.show_mapped_concepts == true", ns = ns, 
        div(
          shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
            div(shiny.fluent::Toggle.shinyInput(ns("merge_mapped_concepts"), value = TRUE), style = "margin-top:30px;; margin-bottom:5px;"),
            div(i18n$t("merge_mapped_concepts"), style = "font-weight:bold; margin-top:30px;; margin-bottom:5px;")
          ),
          style = "width:330px;"
        )
      ),
      div(
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
          div(shiny.fluent::Toggle.shinyInput(ns("hide_concepts_datatables"), value = FALSE), style = "margin-top:30px;; margin-bottom:5px;"),
          div(i18n$t("hide_concepts_datatables"), style = "font-weight:bold; margin-top:30px;; margin-bottom:5px;")
        )
      )
    ),
    shiny.fluent::Stack(
      horizontal = TRUE, tokens = list(childrenGap = 50),
      div(
        div(id = ns("vocabulary_selected_concepts_title"), class = "input_title", i18n$t("vocabulary_selected_concepts")),
        div(shiny.fluent::Dropdown.shinyInput(ns("vocabulary_selected_concepts"), value = NULL, options = list(), multiSelect = TRUE,
          onChanged = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-vocabulary_selected_concepts_trigger', Math.random())"))), style = "width:650px;")
      ),
      div(shiny.fluent::DefaultButton.shinyInput(ns("reset_vocabulary_concepts"), i18n$t("reset")), style = "margin-top:38px;")
    ),
    div(DT::DTOutput(ns("plugin_vocabulary_concepts")), class = "vocabulary_table"),
    div(DT::DTOutput(ns("plugin_vocabulary_mapped_concepts")), class = "vocabulary_table"),
    div(id = ns("blank_space"), br()),
    style = "position:relative; z-index:1;"
  )
  
  # Plugins options & description divs (with distinct languages fields)
  plugin_options_divs <- tagList()
  plugin_description_divs <- tagList()
  
  for (lang in languages$code){
    
    plugin_options_div <- shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
      div(
        div(class = "input_title", paste0(i18n$t("name"), " (", toupper(lang), ")")),
        div(shiny.fluent::TextField.shinyInput(ns(paste0("plugin_name_", lang))), style = "width:320px;")
      ),
      div(
        div(class = "input_title", paste0(i18n$t("category"), " (", toupper(lang), ")")),
        div(shiny.fluent::TextField.shinyInput(ns(paste0("plugin_category_", lang))), style = "width:320px;")
      )
    )
    
    plugin_description_div <- div(
      div(paste0(i18n$t("description"), " (", toupper(lang), ") :"), style = "font-weight:bold; margin-top:7px; margin-right:5px;"),
      shinyAce::aceEditor(ns(paste0("plugin_description_", lang)), "", mode = "markdown", 
        code_hotkeys = list(
          "markdown", 
          list(
            save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
            run_all = list(win = "CTRL-SHIFT-ENTER|CTRL-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER|CTRL-ENTER|CMD-ENTER") 
          )
        ),
      autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000), style = "width: 100%;")
    
    if (lang == language) condition <- paste0("input.plugin_language == '", lang, "' || input.plugin_language == null")
    else condition <- paste0("input.plugin_language == '", lang, "'")
    
    plugin_options_div <- conditionalPanel(condition = paste0("input.plugin_language == '", lang, "'"), ns = ns, plugin_options_div)
    
    plugin_options_divs <- tagList(plugin_options_divs, conditionalPanel(condition = condition, ns = ns, plugin_options_div))
    plugin_description_divs <- tagList(plugin_description_divs, conditionalPanel(condition = condition, ns = ns, plugin_description_div))
  }
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    shiny.fluent::reactOutput(ns("plugin_delete_confirm")),
    shiny.fluent::reactOutput(ns("plugin_image_delete_confirm")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = id, text = i18n$t(id))
    ), maxDisplayedItems = 3),
    
    # --- --- -- -- --
    # Pivot items ----
    # --- --- -- -- --
    
    shiny.fluent::Pivot(
      id = ns("plugins_pivot"),
      onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
      shiny.fluent::PivotItem(id = "all_plugins_card", itemKey = "all_plugins_card", headerText = i18n$t("all_plugins")),
      shiny.fluent::PivotItem(id = "plugins_datatable_card", itemKey = "plugins_datatable_card", headerText = i18n$t("plugins_management")),
      shiny.fluent::PivotItem(id = "plugins_edit_code_card", itemKey = "plugins_edit_code_card", headerText = i18n$t("edit_plugin_code")),
      shiny.fluent::PivotItem(id = "plugins_options_card", itemKey = "plugins_options_card", headerText = i18n$t("plugin_options")),
      shiny.fluent::PivotItem(id = "import_plugin_card", itemKey = "import_plugin_card", headerText = i18n$t("import_plugins")),
      shiny.fluent::PivotItem(id = "export_plugin_card", itemKey = "export_plugin_card", headerText = i18n$t("export_plugins"))
    ),
    forbidden_cards,
    
    # --- --- --- --- --- --- -
    # Plugins catalog card ----
    # --- --- --- --- --- --- -
    
    shinyjs::hidden(
      div(
        id = ns("all_plugins_card"),
        div(id = ns("all_plugins_document_cards"),
          make_card(i18n$t("all_plugins"),
            div(
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
                div(
                  shiny.fluent::ChoiceGroup.shinyInput(ns("all_plugins_source"), value = "local", options = list(
                    list(key = "local", text = i18n$t("local_plural")),
                    list(key = "remote_git", text = i18n$t("git_remote_plugins"))
                  ), className = "inline_choicegroup"),
                  style = "width:322px;"
                ),
                conditionalPanel(condition = "input.all_plugins_source == 'remote_git'", ns = ns,
                  div(
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                      div(strong(i18n$t("remote_git_repo")), style = "margin-top:8px;"),
                      div(shiny.fluent::Dropdown.shinyInput(ns("remote_git_repo")), style = "width:322px;margin-top:3px;")
                    )
                  )
                )
              ),
              conditionalPanel(condition = "input.all_plugins_source == 'local'", ns = ns,
                make_dropdown(i18n = i18n, ns = ns, label = "category", id = "local_plugins_category", width = "322px"),
                uiOutput(ns("all_plugins_local"))),
              conditionalPanel(condition = "input.all_plugins_source == 'remote_git'", ns = ns,
                make_dropdown(i18n = i18n, ns = ns, label = "category", id = "remote_git_plugins_category", width = "322px"),
                uiOutput(ns("all_plugins_remote_git"))), br(),
              div(shiny.fluent::DefaultButton.shinyInput(ns("reload_plugins_document_cards"), i18n$t("refresh")), style = "margin-top:2px; width:320px;")
            )
          )
        ),
        shinyjs::hidden(
          div(id = ns("all_plugins_plugin_details"),
            make_card(
              uiOutput(ns("all_plugins_plugin_details_title")),
              uiOutput(ns("all_plugins_plugin_details_content"))
            )
          )
        ), br(),
      )
    ),
    
    # --- --- --- --- --- -- -- --
    # Plugins management card ----
    # --- --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("plugins_datatable_card"),
        make_card(i18n$t("plugins_management"),
          div(
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
              make_textfield(i18n = i18n, ns = ns, label = "name", id = "plugin_name", width = "300px"),
              div(shiny.fluent::PrimaryButton.shinyInput(ns("add_plugin"), i18n$t("add")), style = "margin-top:38px;"),
              style = "position:relative; z-index:1; width:500px;"
            ),
            div(DT::DTOutput(ns("plugins_datatable")), style = "margin-top:-30px; z-index:2"),
            div(
              shiny.fluent::DefaultButton.shinyInput(ns("delete_selection"), i18n$t("delete_selection")),
              style = "position:relative; z-index:2; margin-top:-30px;"
            )
          )
        ), br()
      )
    ),
    
    # --- --- --- --- --- --- --
    # Edit plugin code card ----
    # --- --- --- --- --- --- --
    
    shinyjs::hidden(
      div(
        id = ns("plugins_edit_code_card"),
        make_shiny_ace_card(i18n$t("edit_plugin_code"),
          div(
            shiny.fluent::Stack(
              tokens = list(childrenGap = 5),
              make_combobox(i18n = i18n, ns = ns, label = "plugin", id = "code_selected_plugin",
                width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
              vocabulary_concepts_div,
              div(
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                  shiny.fluent::ChoiceGroup.shinyInput(ns("edit_code_ui_server"), value = "ui", options = list(
                    list(key = "ui", text = i18n$t("ui")),
                    list(key = "server", text = i18n$t("server")),
                    list(key = "translations", text = i18n$t("translations"))
                  ), className = "inline_choicegroup"),
                  div(shiny.fluent::Toggle.shinyInput(ns("hide_editor"), value = FALSE), style = "margin-top:9px;"),
                  div(i18n$t("hide_editor"), style = "font-weight:bold; margin-top:9px; margin-right:30px;")
                ),
                style = "z-index:2"
              ),
              conditionalPanel(condition = "input.hide_editor == true", ns = ns, br())
            ),
            conditionalPanel(condition = "input.hide_editor == false", ns = ns,
              conditionalPanel(condition = "input.edit_code_ui_server == 'ui'", ns = ns,
                div(shinyAce::aceEditor(
                  ns("ace_edit_code_ui"), "", mode = "r", 
                  code_hotkeys = list(
                    "r", list(
                      run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
                      run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
                      save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S")
                    )
                  ),
                  autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000
                ), style = "width: 100%;")),
              conditionalPanel(condition = "input.edit_code_ui_server == 'server'", ns = ns,
                div(shinyAce::aceEditor(
                  ns("ace_edit_code_server"), "", mode = "r", 
                  code_hotkeys = list(
                    "r", list(
                      run_selection = list(win = "CTRL-ENTER", mac = "CTRL-ENTER|CMD-ENTER"),
                      run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
                      save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S")
                    )
                  ),
                  autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000
                ), style = "width: 100%;")),
              conditionalPanel(condition = "input.edit_code_ui_server == 'translations'", ns = ns,
                div(shinyAce::aceEditor(
                  ns("ace_edit_code_translations"), "", mode = "text",
                  code_hotkeys = list("r", list(save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"))),
                  autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000
                ), style = "width: 100%;"))
            ),
            shiny.fluent::Stack(
              tokens = list(childrenGap = 5),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("execute_code"), i18n$t("run_code")),
                shiny.fluent::DefaultButton.shinyInput(ns("save_code"), i18n$t("save")),
              ), br(),
              div(textOutput(ns("datetime_code_execution")), style = "color:#878787;"),
              shiny::uiOutput(ns("code_result_ui")), br(),
              div(verbatimTextOutput(ns("code_result_server")), 
                style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
            )
          )
        ), br()
      )
    ),
    
    # --- --- --- --- -- -- --
    # Plugin options card ----
    # --- --- --- --- -- -- --
    
    shinyjs::hidden(
      div(
        id = ns("plugins_options_card"),
        make_shiny_ace_card(i18n$t("plugin_options"),
          div(
            shiny.fluent::Stack(
              tokens = list(childrenGap = 5),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
                make_combobox(i18n = i18n, ns = ns, label = "plugin", id = "options_selected_plugin",
                  width = "320px", allowFreeform = FALSE, multiSelect = FALSE),
                make_dropdown(i18n = i18n, ns = ns, label = "language", id = "plugin_language", 
                  options = convert_tibble_to_list(languages, key_col = "code", text_col = "language"), value = language, width = "320px"),
                make_textfield(i18n = i18n, ns = ns, label = "version", id = "plugin_version", width = "80px")
              ),
              make_textfield(i18n = i18n, ns = ns, label = "author_s", id = "plugin_author", width = "660px"),
              plugin_options_divs,
              br(),
              div(
                div(class = "input_title", paste0(i18n$t("grant_access_to"), " :")),
                shiny.fluent::ChoiceGroup.shinyInput(ns("users_allowed_read_group"), options = list(
                  list(key = "everybody", text = i18n$t("everybody")),
                  list(key = "people_picker", text = i18n$t("choose_users"))
                ), className = "inline_choicegroup"),
                conditionalPanel(condition = "input.users_allowed_read_group == 'people_picker'", ns = ns,
                  uiOutput(ns("users_allowed_read_div"))
                )
              ), br(),
              div(
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
                  make_dropdown(i18n = i18n, ns = ns, label = "image_url", id = "plugin_image", width = "320px"),
                  div(shiny.fluent::DefaultButton.shinyInput(ns("delete_image"), i18n$t("delete_this_image")), style = "margin-top:39px;"),
                  div(shiny.fluent::DefaultButton.shinyInput(ns("import_image"), i18n$t("import_image")), style = "margin-top:39px;"),
                  div(
                    shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                      make_toggle(i18n = i18n, ns = ns, label = "resize_image", inline = TRUE)), 
                  style = "margin-top:44px;"),
                )
              ), 
              br(),
              conditionalPanel(condition = "input.plugin_image != null & input.plugin_image != ''", ns = ns,
                div(imageOutput(ns("render_image")), style = "border:solid #ECEBE9 1px; width:318px; height:200px;"), br()
              ),
            ),
            plugin_description_divs,
            shiny.fluent::Stack(
              tokens = list(childrenGap = 5),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                shiny.fluent::PrimaryButton.shinyInput(ns("save_plugin_options"), i18n$t("save")),
                shiny.fluent::DefaultButton.shinyInput(ns("execute_options_description"), i18n$t("preview"))
              ),
              br(), br(),
              div(id = ns("description_markdown_output"),
                uiOutput(ns("description_markdown_result")), 
                style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px; padding-top: 10px;"),
              div(style = "display:none;", fileInput(ns("import_image_file"), label = "", multiple = FALSE, accept = c(".jpg", ".jpeg", ".png", ".svg")))
            )
          )
        ), br()
      )
    ),
    
    # --- --- --- --- --- -- -
    # Import plugins card ----
    # --- --- --- --- --- -- -
    
    shinyjs::hidden(
      div(
        id = ns("import_plugin_card"),
        make_card(i18n$t("import_plugins"),
          div(br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), 
              make_toggle(i18n = i18n, ns = ns, label = "replace_already_existing_plugins", inline = TRUE)), br(),
            shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
              shiny.fluent::DefaultButton.shinyInput(ns("import_plugins_browse"), i18n$t("choose_zip_file"), style = "width:270px;"),
              uiOutput(ns("import_plugins_status"))), br(),
            shiny.fluent::PrimaryButton.shinyInput(ns("import_plugins_button"), i18n$t("import_plugins"), iconProps = list(iconName = "Download"), style = "width:270px;"), br(),
            shinyjs::hidden(
              div(
                id = ns("imported_plugins_div"), br(),
                strong(i18n$t("imported_plugins")),
                div(DT::DTOutput(ns("imported_plugins")))
              )
            ),
            div(style = "display:none;", fileInput(ns("import_plugins_upload"), label = "", multiple = FALSE, accept = ".zip"))
          )
        ), br()
      )
    ),
    
    # --- --- --- --- --- -- -
    # Export plugins card ----
    # --- --- --- --- --- -- -
    
    shinyjs::hidden(
      div(
        id = ns("export_plugin_card"),
        make_shiny_ace_card(i18n$t("export_plugins"),
          div(
            shiny.fluent::Stack(
              horizontal = TRUE, tokens = list(childrenGap = 10),
              div(
                div(id = ns("plugins_to_export_title"), class = "input_title", i18n$t("plugins_to_export")),
                shiny.fluent::Dropdown.shinyInput(ns("plugins_to_export"), multiSelect = TRUE,
                  onChanged = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-plugins_to_export_trigger', Math.random())"))),
                style = "width:400px;"
              ),
              div(shiny.fluent::PrimaryButton.shinyInput(ns("export_selected_plugins"), 
                i18n$t("export_plugins"), iconProps = list(iconName = "Upload")), style = "margin-top:38px;"),
              style = "position:relative; z-index:1; width:700px;"
            ),
            div(DT::DTOutput(ns("plugins_to_export_datatable")), style = "margin-top:-30px; z-index:2"),
            div(style = "visibility:hidden;", downloadButton(ns("export_plugins_download"), label = ""))
          )
        ), br()
      )
    )
  ) -> result
  
  result
}

#' plugins Server Functions
#'
#' @noRd 
mod_plugins_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), m = shiny::reactiveValues(),
  language = "en", i18n = character(), perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (debug) print(paste0(Sys.time(), " - mod_plugins - start"))
    
    col_types <- tibble::tribble(
      ~table, ~col_types,
      "plugins", "iccicl",
      "code", "icicicl"
    )
    
    # Prefix depending on page id
    if (id == "plugins_patient_lvl"){
      prefix <- "patient_lvl"
      tab_type_id <- 1 
    }
    if (id == "plugins_aggregated"){
      prefix <- "aggregated"
      tab_type_id <- 2
    }
    
    # r[[paste0(prefix, "_plugins_datatable_loaded")]] <- FALSE
    
    # Close message bar
    sapply(1:20, function(i) observeEvent(input[[paste0("close_message_bar_", i)]], shinyjs::hide(paste0("message_bar", i))))
    
    # --- --- --- --- --- ---
    # Show or hide cards ----
    # --- --- --- --- --- ---
    
    cards <- c("all_plugins_card", "plugins_datatable_card", "plugins_edit_code_card",
      "plugins_options_card", "import_plugin_card", "export_plugin_card")
    show_or_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    
    # Show first card
    if ("all_plugins_card" %in% r$user_accesses) shinyjs::show("all_plugins_card")
    else shinyjs::show("all_plugins_card_forbidden")
    
    # --- --- --- --- --- --- --
    # Load page from header ----
    # --- --- --- --- --- --- --
    
    observeEvent(shiny.router::get_page(), {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - ", id, " - observer shiny_router::change_page"))

      if (prefix == "aggregated" & shiny.router::get_page() == "plugins" & r$plugins_page == "plugins_patient_lvl") shiny.router::change_page("plugins_patient_lvl")
      else if (prefix == "patient_lvl" & shiny.router::get_page() == "plugins" & r$plugins_page == "plugins_aggregated") shiny.router::change_page("plugins_aggregated")

      # Close help pages when page changes
      # r[[paste0("help_plugins_", prefix, "_open_panel")]] <- FALSE
      # r[[paste0("help_plugins_", prefix, "_open_modal")]] <- FALSE

      # Load Export plugins page, to load DT (doesn't update with other DT if not already loaded once)
      if (shiny.router::get_page() == paste0("plugins_", prefix) & length(r[[paste0(prefix, "_plugins_page_loaded")]]) == 0){
        sapply(c("plugins_datatable_card", "export_plugin_card"), function(card) if (card %in% r$user_accesses) shinyjs::show(card))
        shinyjs::delay(500, sapply(c("plugins_datatable_card", "export_plugin_card"), shinyjs::hide))
        r[[paste0(prefix, "_plugins_page_loaded")]] <- TRUE
      }
    })
    
    # --- --- --- --- --- -
    # Update dropdowns ----
    # --- --- --- --- --- -
    
    observeEvent(r$plugins, {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$plugins"))
      
      options <- convert_tibble_to_list(r$plugins %>% dplyr::filter(tab_type_id == !!tab_type_id) %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_plugin", options = options)
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_plugin", options = options)
      
      # Reload category dropdowns
      
      categories_options <-
        tibble::tibble(key = "all", text = i18n$t("all_categories")) %>%
        dplyr::bind_rows(
          r$plugins %>%
            dplyr::left_join(
              r$options %>% dplyr::filter(category == "plugin", name == paste0("category_", language)) %>% dplyr::select(id = link_id, category = value),
              by = "id") %>%
            dplyr::filter(tab_type_id == !!tab_type_id & category != "" & !is.na(category)) %>%
            dplyr::group_by(category) %>% dplyr::slice(1) %>% dplyr::ungroup() %>%
            dplyr::select(key = category, text = category) %>%
            dplyr::arrange(key)
        )

      shiny.fluent::updateDropdown.shinyInput(session, "local_plugins_category",
        options = convert_tibble_to_list(categories_options, key_col = "key", text_col = "text"), value = "all")
    })
    
    # --- --- --- --- --- ---
    # Help for this page ----
    # --- --- --- --- --- ---
    
    observeEvent(input$help, if (id == shiny.router::get_page() %>% stringr::str_replace_all("/", "_")) r[[paste0("help_plugins_", prefix, "_open_panel")]] <- TRUE)
    observeEvent(input$hide_panel, r[[paste0("help_plugins_", prefix, "_open_panel")]] <- FALSE)
    
    r[[paste0("help_plugins_", prefix, "_open_panel_light_dismiss")]] <- TRUE
    observeEvent(input$show_modal, r[[paste0("help_plugins_", prefix, "_open_modal")]] <- TRUE)
    observeEvent(input$hide_modal, {
      r[[paste0("help_plugins_", prefix, "_open_modal")]] <- FALSE
      r[[paste0("help_plugins_", prefix, "_open_panel_light_dismiss")]] <- TRUE
    })
    
    sapply(1:10, function(i){
      observeEvent(input[[paste0("help_page_", i)]], r[[paste0("help_plugins_", prefix, "_page_", i)]] <- Sys.time())
    })
    
    help_plugins(output = output, r = r, id = id, prefix = prefix, language = language, i18n = i18n, ns = ns)
    
    observeEvent(input$copy_code_1, r$help_plugins_copy_code_1 <- Sys.time())
    observeEvent(input$copy_code_2, r$help_plugins_copy_code_2 <- Sys.time())
    observeEvent(input$copy_code_3, r$help_plugins_copy_code_3 <- Sys.time())
    observeEvent(input$copy_code_4, r$help_plugins_copy_code_4 <- Sys.time())
    
    # --- --- --- --- -- -
    # Plugins catalog ----
    # --- --- --- --- -- -
    
    # Update dropdown of remote git repos
    
    observeEvent(r$git_repos, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$git_repos"))
      
      shiny.fluent::updateDropdown.shinyInput(session, "remote_git_repo", 
        options = convert_tibble_to_list(r$git_repos, key_col = "id", text_col = "name"))
    })
    
    # Update plugins catalog when a remote git repo is selected
    
    observeEvent(input$remote_git_repo, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$remote_git_repo"))
      
      # Get URL of remote git repo
      url_address <- r$git_repos %>% dplyr::filter(id == input$remote_git_repo) %>% dplyr::pull(url_address)
      if (substr(url_address, nchar(url_address), nchar(url_address)) != "/") url_address <- paste0(url_address, "/")
      url_address <- paste0(url_address, "plugins/")
      r$url_address <- url_address
      
      error_loading_remote_git <- TRUE
      plugins_file <- paste0(r$app_folder, "/temp_files/plugins.xml")
      
      if (r$has_internet){
        
        tryCatch({
          xml2::download_xml(paste0(url_address, "plugins.xml"), plugins_file)
          error_loading_remote_git <- FALSE
        }, error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_connection_remote_git", 
          error_name = "plugins_catalog load plugins.xml", category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
      }
      
      if (error_loading_remote_git) r$remote_git_plugins <- tibble::tibble(type = integer(), unique_id = character())
      
      else r$remote_git_plugins <-
        xml2::read_xml(plugins_file) %>%
        XML::xmlParse() %>%
        XML::xmlToDataFrame(nodes = XML::getNodeSet(., "//plugin")) %>%
        tibble::as_tibble()
      
      r$error_loading_remote_git <- error_loading_remote_git
      
      # Update dropdown of other page (patient_lvl or aggregated)
      if (prefix == "patient_lvl") r$aggregated_remote_git_repo <- input$remote_git_repo
      else r$patient_lvl_remote_git_repo <- input$remote_git_repo
      
      # Update category dropdown
      if (nrow(r$remote_git_plugins) > 0){
        categories_options <- 
          tibble::tibble(key = "all", text = i18n$t("all_categories")) %>%
          dplyr::bind_rows(
            r$remote_git_plugins %>%
              dplyr::rename(category = !!paste0("category_", language)) %>%
              dplyr::filter(type == !!tab_type_id & category != "" & !is.na(category)) %>%
              dplyr::group_by(category) %>% dplyr::slice(1) %>% dplyr::ungroup() %>%
              dplyr::arrange(category) %>%
              dplyr::select(key = category, text = category)
          )
        
        shiny.fluent::updateDropdown.shinyInput(session, "remote_git_plugins_category", 
          options = convert_tibble_to_list(categories_options, key_col = "key", text_col = "text"), value = "all")
      }
      if (nrow(r$remote_git_plugins) == 0) shiny.fluent::updateDropdown.shinyInput(session, "remote_git_plugins_category", options = list(), value = NULL)
      
      r$reload_plugins_document_cards <- Sys.time()
    })
    
    observeEvent(r[[paste0(prefix, "_remote_git_repo")]], {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$.._remote_git_repo"))
      
      shiny.fluent::updateDropdown.shinyInput(session, "remote_git_repo",
        options = convert_tibble_to_list(r$git_repos, key_col = "id", text_col = "name"),
        value = r[[paste0(prefix, "_remote_git_repo")]])
    })
    
    observeEvent(r$plugins, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$plugins"))
      r$reload_plugins_document_cards <- Sys.time()
    }, once = TRUE)
    
    observeEvent(input$reload_plugins_document_cards, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$reload_plugins_document_cards"))
      r$reload_plugins_document_cards <- Sys.time()
    })
    
    observeEvent(input$local_plugins_category, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$local_plugins_category"))
      r$reload_plugins_document_cards <- Sys.time()
    })
    
    observeEvent(r$reload_plugins_document_cards, {

      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$reload_plugins_document_cards"))

      req(input$local_plugins_category)

      if (length(r$error_loading_remote_git) == 0){
        r$remote_git_plugins <- tibble::tibble(type = integer(), unique_id = character())
        error_loading_remote_git <- TRUE
      }
      if (length(r$error_loading_remote_git) > 0) error_loading_remote_git <- r$error_loading_remote_git

      plugins <- list()
      plugins$remote_git <- r$remote_git_plugins %>% dplyr::filter(type == tab_type_id) %>% dplyr::mutate(id = unique_id)
      if (nrow(plugins$remote_git) > 0) plugins$remote_git <- plugins$remote_git %>% dplyr::arrange(get(paste0("name_", language)))
      plugins$local <- r$plugins %>% dplyr::filter(tab_type_id == !!tab_type_id) %>% dplyr::arrange(name)
      r[[paste0(prefix, "_plugins_images")]] <- tibble::tibble(type = character(), id = character(), image_url = character())

      # Filter local plugins on category
      if (input$local_plugins_category != "all") plugins$local <- plugins$local %>%
        dplyr::left_join(
          r$options %>% dplyr::filter(category == "plugin", name == paste0("category_", language)) %>% dplyr::select(id = link_id, category = value),
          by = "id") %>%
        dplyr::filter(category == input$local_plugins_category)

      # Filter remote plugins on category
      if (length(input$remote_git_plugins_category) > 0) if (input$remote_git_plugins_category != "all") plugins$remote_git <- plugins$remote_git %>%
        dplyr::filter(get(paste0("category_", language)) == input$remote_git_plugins_category)

      sapply(c("remote_git", "local"), function(type){

        all_plugins <- tagList()
        all_plugins_document_cards <- tagList()

        if (type == "remote_git" & length(r$error_loading_remote_git) == 0) renderUI(tagList(br(), shiny.fluent::MessageBar(i18n$t("choose_remote_git"), messageBarType = 5)))
        else if (type == "remote_git" & error_loading_remote_git) output$all_plugins_remote_git <- renderUI(tagList(br(), shiny.fluent::MessageBar(i18n$t("error_connection_remote_git"), messageBarType = 3)))
        else if (nrow(plugins[[type]]) == 0) output[[paste0("all_plugins_", type)]] <- renderUI(tagList(br(), shiny.fluent::MessageBar(i18n$t("no_available_plugin"), messageBarType = 5)))

        else if (nrow(plugins[[type]]) > 0){

          i <- 0

          for(plugin_id in plugins[[type]] %>% dplyr::pull(id)){

            if (type == "local"){
              plugin <- list()
              plugin$name <- r$plugins %>% dplyr::filter(id == plugin_id) %>% dplyr::pull(name)
              plugin$options <- r$options %>% dplyr::filter(category == "plugin", link_id == plugin_id)
              plugin$unique_id <- plugin$options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value)
              plugin$author <- plugin$options %>% dplyr::filter(name == "author") %>% dplyr::pull(value)
              plugin$version <- plugin$options %>% dplyr::filter(name == "version") %>% dplyr::pull(value)
              plugin$category <- plugin$options %>% dplyr::filter(name == paste0("category_", language)) %>% dplyr::pull(value)

              if (plugin$options %>% dplyr::filter(name == "image") %>% dplyr::pull(value) != ""){
                plugin$image_url <- paste0(r$app_folder, "/plugins/", prefix, "/",
                  plugin$options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value), "/",
                  plugin$options %>% dplyr::filter(name == "image") %>% dplyr::pull(value))
              }
              else plugin$image_url <- ""
            }

            if (type == "remote_git"){
              plugin <- plugins$remote_git %>% dplyr::filter(id == plugin_id)

              plugin$name <- plugin[[paste0("name_", language)]]

              if (plugin$image != "") plugin$image_url <- paste0(r$url_address, prefix, "/", plugin$unique_id, "/", plugin$image)
              else plugin$image_url <- ""
            }

            r[[paste0(prefix, "_plugins_images")]] <- r[[paste0(prefix, "_plugins_images")]] %>% dplyr::bind_rows(
              tibble::tribble(~type, ~id, ~image_url, type, as.character(plugin_id), plugin$image_url))

            if (type == "remote_git") image_div <- uiOutput(ns(paste0(plugin_id, "_image")))
            if (type == "local") image_div <- imageOutput(ns(paste0(plugin_id, "_image")), width = "320px", height = "200px")

            all_plugins_document_cards <- tagList(all_plugins_document_cards,

              shiny.fluent::Link(
                div(
                  div(image_div, style = "height:200px; width:320px; background-color:#FAF9F8;"),
                  div(style = "height:20px;"),
                  div(shiny.fluent::Text(plugin$name, variant = "large"), style = "margin-left:10px; margin-top:-10px; margin-bottom:5px;"),
                  div(
                    div(shiny.fluent::Persona(imageInitials = gsub("([A-Z])[^A-Z]+", "\\1", plugin$author),
                      size = 2, initialsColor = sample(1:10, 1)), style = "float:left; margin-left:10px; margin-top:10px;"),
                    div(
                      div(shiny.fluent::Text(plugin$author, variant = "medium", style = "font-weight:bold;")),
                      div(shiny.fluent::Text(plugin$version, variant = "small"), style = "margin-top:-4px;"),
                      style = "float:left; margin-top:5px;"
                    )
                  ),
                  style = "height:290px; width:320px; border:solid 1px #ECEBE9; img:hover{border:5px;}"
                ),
                onClick = htmlwidgets::JS(paste0("function() { ",
                  "Shiny.setInputValue('", id, "-show_plugin_details', Math.random());",
                  "Shiny.setInputValue('", id, "-plugin_id', '", plugin_id, "');",
                  "}"))
              )
            )

            i <- i + 1

            if (i %% 3 == 0){
              all_plugins <- tagList(all_plugins, br(),
                div(
                  shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
                    all_plugins_document_cards
                  )
                )
              )

              all_plugins_document_cards <- tagList()
            }
          }

          if (i %% 3 != 0){
            all_plugins <- tagList(all_plugins, br(),
              div(
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
                  all_plugins_document_cards
                )
              )
            )
          }

          output[[paste0("all_plugins_", type)]] <- renderUI(tagList(all_plugins))
        }
      })

      r[[paste0(prefix, "_load_plugins_images")]] <- Sys.time()

      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer r$reload_plugins_document_cards"))
    })
    
    observeEvent(r[[paste0(prefix, "_load_plugins_images")]], {
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$..load_plugins_images"))
      
      req(nrow(r[[paste0(prefix, "_plugins_images")]]) > 0)
      sapply(1:nrow(r[[paste0(prefix, "_plugins_images")]]), function(i){
        
        row <- r[[paste0(prefix, "_plugins_images")]][i, ]
        
        if (row$type == "remote_git"){
          output[[paste0(row$id, "_image")]] <- renderUI({
            req(row$image_url != "")
            tags$img(src = row$image_url, width = 318, height = 200)
          })
        }
        if (row$type == "local"){
          output[[paste0(row$id, "_image")]] <- renderImage({
            req(row$image_url != "")
            list(src = row$image_url, width = 318, height = 200)
          },
            deleteFile = FALSE)
        }
      })
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer r$..load_plugins_images"))
    })
    
    observeEvent(input$show_plugin_details, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$show_plugin_details"))
      r$show_plugin_details <- Sys.time()
    })
    
    observeEvent(r$show_plugin_details, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$show_plugin_details"))
      
      req(input$show_plugin_details)
      
      # Render markdown description
      
      dir <- paste0(r$app_folder, "/temp_files")
      
      markdown_settings <- paste0("```{r setup, include=FALSE}\nknitr::opts_knit$set(root.dir = '", dir, "')\n",
        "knitr::opts_chunk$set(root.dir = '", dir, "', fig.path = '", dir, "')\n```\n")
      
      # For local plugins
      
      if (input$all_plugins_source == "local"){
        plugin <- r$plugins %>% dplyr::filter(id == input$plugin_id)
        plugin_options <- r$options %>% dplyr::filter(category == "plugin", link_id == input$plugin_id)
        
        plugin_description <- paste0(
          "**Auteur** : ", plugin_options %>% dplyr::filter(name == "author") %>% dplyr::pull(value), "<br />",
          "**Version** : ", plugin_options %>% dplyr::filter(name == "version") %>% dplyr::pull(value), "\n\n",
          plugin_options %>% dplyr::filter(name == paste0("description_", tolower(language))) %>% dplyr::pull(value) %>% stringr::str_replace_all("''", "'")
        )
        
        plugin_folder <- paste0(r$app_folder, "/plugins/", prefix, "/", plugin_options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
      }
      
      # For remote_git plugins
      
      if (input$all_plugins_source == "remote_git"){
        plugin <- r$remote_git_plugins %>% dplyr::filter(unique_id == input$plugin_id)
        
        plugin$name <- plugin[[paste0("name_", language)]]
        
        plugin_description <- paste0(
          "**Auteur** : ", plugin$name, "<br />",
          "**Version** : ", plugin$version, "\n\n",
          plugin %>% dplyr::pull(paste0("description_", tolower(language)))
        )
        
        plugin_folder <- paste0(r$url_address, prefix, "/", plugin$unique_id)
      }
      
      # Change %plugin_folder% for images
      plugin_description <- plugin_description %>% stringr::str_replace_all("%plugin_folder%", plugin_folder)
      
      markdown_file <- paste0(markdown_settings, plugin_description)
      
      # Create temp dir
      file <- paste0(dir, "/", as.character(Sys.time()) %>% stringr::str_replace_all(":", "_"), ".Md")
      if (!dir.exists(dir)) dir.create(dir)
      
      # Create the markdown file
      knitr::knit(text = markdown_file, output = file, quiet = TRUE)
      
      update_plugin_div <- ""
      
      if (input$all_plugins_source == "remote_git"){
        
        plugins <- 
          r$plugins %>%
          dplyr::filter(tab_type_id == !!tab_type_id) %>%
          dplyr::left_join(
            r$options %>% dplyr::filter(category == "plugin", name == "unique_id") %>% dplyr::select(id = link_id, unique_id = value),
            by = "id"
          ) %>%
          dplyr::left_join(
            r$options %>% dplyr::filter(category == "plugin", name == "version") %>% dplyr::select(id = link_id, version = value),
            by = "id"
          )
        
        plugins_join <- plugins %>% dplyr::inner_join(plugin %>% dplyr::select(unique_id), by = "unique_id")
        
        # Check if this plugin is already installed
        
        if (nrow(plugins_join) > 0){
          
          # Check if this plugin is up to date
          
          if (compareVersion(plugins_join %>% dplyr::pull(version), plugin$version) == 0) update_plugin_div <- shiny.fluent::DefaultButton.shinyInput(
            ns("update_plugin"), i18n$t("plugin_up_to_date"), disabled = TRUE)
          else if (compareVersion(plugins_join %>% dplyr::pull(version), plugin$version) == 1) update_plugin_div <- shiny.fluent::DefaultButton.shinyInput(
            ns("update_plugin"), i18n$t("local_plugin_newer"), disabled = TRUE)
          else update_plugin_div <- shiny.fluent::PrimaryButton.shinyInput(ns("update_plugin"), i18n$t("update_plugin"))
        }
        
        else update_plugin_div <- shiny.fluent::PrimaryButton.shinyInput(ns("install_plugin"), i18n$t("install_plugin"))
        
        update_plugin_div <- tagList(update_plugin_div, br())
      }
      
      output$all_plugins_plugin_details_title <- renderUI(
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
          div(shiny.fluent::ActionButton.shinyInput(ns("all_plugins_show_document_cards"), "", iconProps = list(iconName = "Back")), style = "position:relative; bottom:6px;"), 
          plugin$name
        )
      )
      
      output$all_plugins_plugin_details_content <- renderUI(
        tagList(
          update_plugin_div,
          div(
            div(class = "markdown", tagList(withMathJax(includeMarkdown(file)))),
            style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;"
          )
        )
      )
      
      shinyjs::hide("all_plugins_document_cards")
      shinyjs::show("all_plugins_plugin_details")
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer r$show_plugin_details"))
    })
    
    observeEvent(input$all_plugins_show_document_cards, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$all_plugins_show_document_cards"))
      shinyjs::show("all_plugins_document_cards")
      shinyjs::hide("all_plugins_plugin_details")
    })
    
    # Install of update a remote_git plugin
    
    observeEvent(input$install_plugin, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$install_plugin"))
      r$install_update_plugin <- "install"
      r$install_update_plugin_trigger <- Sys.time()
    })
    
    observeEvent(input$update_plugin, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$update_plugin"))
      r$install_update_plugin <- "update"
      r$install_update_plugin_trigger <- Sys.time()
    })
    
    observeEvent(r$install_update_plugin_trigger, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$install_update_plugin_trigger"))
      
      req(isTruthy(input$install_plugin) || isTruthy(input$update_plugin))
      
      plugin <- r$remote_git_plugins %>% dplyr::filter(unique_id == input$plugin_id)
      
      # Add columns that don't exist (if a language has been added after the creation of the plugin)
      prefixes <- c("description", "name", "category")
      new_cols <- outer(prefixes, r$languages$code, paste, sep = "_") %>% as.vector()
      for(col in new_cols) if(!col %in% colnames(plugin)) plugin <- plugin %>% dplyr::mutate(!!col := "")
      
      if (plugin[[paste0("name_", language)]] == "") plugin[[paste0("name_", language)]] <- plugin$name_en
      
      tryCatch({
        
        # Delete translations file
        # Create plugin folder in translations folder if doesn't exist
        translations_dir <- paste0(r$app_folder, "/translations/", plugin$unique_id)
        if (dir.exists(translations_dir)) unlink(translations_dir, recursive = TRUE)
        
        # Delete old rows
        
        if (r$install_update_plugin == "install") new_row <- get_last_row(r$db, "plugins") + 1
        
        if (r$install_update_plugin == "update"){
          
          new_row <- r$options %>% dplyr::filter(category == "plugin", name == "unique_id", value == plugin$unique_id) %>% 
            dplyr::select(id = link_id, value) %>%
            dplyr::inner_join(r$plugins, by = "id") %>%
            dplyr::pull(id)
          
          sql <- glue::glue_sql("DELETE FROM plugins WHERE id = {new_row}", .con = r$db)
          query <- DBI::dbSendStatement(r$db, sql)
          DBI::dbClearResult(query)
          r$plugins <- r$plugins %>% dplyr::filter(id != new_row)
          
          sql <- glue::glue_sql("DELETE FROM options WHERE category = 'plugin' AND link_id = {new_row}", .con = r$db)
          query <- DBI::dbSendStatement(r$db, sql)
          DBI::dbClearResult(query)
          r$options <- r$options %>% dplyr::filter(category != "plugin" | (category == "plugin" & link_id != new_row))
          
          sql <- glue::glue_sql("DELETE FROM code WHERE category IN ('plugin_ui', 'plugin_server', 'plugin_translations') AND link_id = {new_row}", .con = r$db)
          query <- DBI::dbSendStatement(r$db, sql)
          DBI::dbClearResult(query)
          r$code <- r$code %>% dplyr::filter(category %not_in% c("plugin_ui", "plugin_server", "plugin_translations") | 
              (category %in% c("plugin_ui", "plugin_server", "plugin_translations") & link_id != new_row))
        }
        
        for (name in c(paste0("description_", r$languages$code))) if (name %in% colnames(plugin)) plugin[[name]] <- plugin[[name]] %>% stringr::str_replace_all("'", "''")
        
        # Plugin table
        
        new_data <- tibble::tribble(
          ~id, ~name, ~tab_type_id, ~creation_datetime, ~update_datetime, ~deleted,
          new_row, as.character(plugin[[paste0("name_", language)]]), as.integer(tab_type_id), plugin$creation_datetime, plugin$update_datetime, FALSE)
        
        DBI::dbAppendTable(r$db, "plugins", new_data)
        add_log_entry(r = r, category = paste0("plugins - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_data))
        r$plugins <- r$plugins %>% dplyr::bind_rows(new_data)
        
        # Options table
        
        last_row_options <- get_last_row(r$db, "options")
        
        new_options <- tibble::tribble(
          ~name, ~value, ~value_num,
          "users_allowed_read_group", "everybody", 1,
          "user_allowed_read", "", r$user_id,
          "version", plugin$version, NA_integer_,
          "unique_id", plugin$unique_id, NA_integer_,
          "author", plugin$author, NA_integer_,
          "image", plugin$image, NA_integer_
        ) %>%
          dplyr::bind_rows(
            r$languages %>%
              tidyr::crossing(col_prefix = c("description", "category", "name")) %>%
              dplyr::rowwise() %>%
              dplyr::mutate(
                name = paste0(col_prefix, "_", code),
                value = plugin[[paste0(col_prefix, "_", code)]],
                value_num = NA_integer_
              ) %>%
              dplyr::select(-code, -language, -col_prefix)
          ) %>%
          dplyr::mutate(id = last_row_options + dplyr::row_number(), category = "plugin", link_id = new_row, .before = "name") %>%
          dplyr::mutate(creator_id = r$user_id, datetime = as.character(Sys.time()), deleted = FALSE)
        
        DBI::dbAppendTable(r$db, "options", new_options)
        add_log_entry(r = r, category = paste0("code", " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_options))
        r$options <- r$options %>% dplyr::bind_rows(new_options)
        
        # Code table
        
        plugin_ui_code <- 
          paste0(r$url_address, prefix, "/", plugin$unique_id, "/ui.R") %>%
          readLines(warn = FALSE) %>% paste(collapse = "\n") %>%
          stringr::str_replace_all("'", "''")
        
        plugin_server_code <-
          paste0(r$url_address, prefix, "/", plugin$unique_id, "/server.R") %>%
          readLines(warn = FALSE) %>% paste(collapse = "\n") %>%
          stringr::str_replace_all("'", "''")
        
        plugin_translations_code <-
          paste0(r$url_address, prefix, "/", plugin$unique_id, "/translations.csv") %>%
          readLines(warn = FALSE) %>% paste(collapse = "\n") %>%
          stringr::str_replace_all("'", "''")
        
        last_row_code <- get_last_row(r$db, "code")
        
        new_code <- tibble::tibble(
          id = last_row_code + 1:3,
          category = c("plugin_ui", "plugin_server", "plugin_translations"),
          link_id = new_row, code = c(plugin_ui_code, plugin_server_code, plugin_translations_code), 
          creator_id = r$user_id, datetime = as.character(Sys.time()), deleted = FALSE
        )
        
        DBI::dbAppendTable(r$db, "code", new_code)
        add_log_entry(r = r, category = paste0("code", " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_code))
        r$code <- r$code %>% dplyr::bind_rows(new_code)
        
        # Copy files
        # Create folder if doesn't exist
        plugin_dir <- paste0(r$app_folder, "/plugins/", prefix, "/", plugin$unique_id)
        if (!dir.exists(plugin_dir)) dir.create(plugin_dir, recursive = TRUE)
        
        # list_of_files <- list.files(paste0(temp_dir, "/plugins/", prefix, "/", plugin$unique_id))
        
        # Copy images
        tryCatch({
          if (nchar(plugin$images) > 0){
            images <- stringr::str_split(plugin$images, ";;;")
            for (image in images){
              url <- paste0(r$url_address, prefix, "/", plugin$unique_id, "/", image)
              url <- gsub(" ", "%20", url)
              destfile <- paste0(plugin_dir, "/", image)
              download.file(url, destfile, quiet = TRUE)
            }
          }
        }, error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_install_remote_git_plugin", 
          error_name = paste0("install_remote_git_plugin - download_images - id = ", plugin$unique_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
        
        r$show_plugin_details <- Sys.time()
        
        # Reload datatable
        r[[paste0(prefix, "_plugins_temp")]] <- r$plugins %>% dplyr::filter(tab_type_id == !!tab_type_id) %>% 
          dplyr::mutate_at(c("creation_datetime", "update_datetime"), format_datetime, language = "en", sec = FALSE) %>%
          dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
        
        show_message_bar(output,  "success_installing_remote_git_plugin", "success", i18n = i18n, ns = ns)
        
      }, error = function(e) if (nchar(e[1]) > 0) report_bug(r = r, output = output, error_message = "error_install_remote_git_plugin", 
        error_name = paste0("install_remote_git_plugin - id = ", plugin$unique_id), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer r$install_update_plugin_trigger"))
    })
    
    # --- --- --- --- -- -
    # Create a plugin ----
    # --- --- --- --- -- -
    
    observeEvent(input$add_plugin, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$add_plugin"))
      
      new_data <- list()
      new_data$name <- coalesce2(type = "char", x = input$plugin_name)
      new_data$plugin_name <- new_data$name
      new_data$tab_type <- tab_type_id
      
      add_settings_new_data(session = session, output = output, r = r, m = m, i18n = i18n, id = id, 
        data = new_data, table = "plugins", required_textfields = "plugin_name", req_unique_values = "name")
      
      # Reload datatable
      r[[paste0(prefix, "_plugins_temp")]] <- r$plugins %>% dplyr::filter(tab_type_id == !!tab_type_id) %>% 
        dplyr::mutate_at(c("creation_datetime", "update_datetime"), format_datetime, language = "en", sec = FALSE) %>%
        dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer input$add_plugin"))
    })
    
    # --- --- --- --- --- ---
    # Plugins management ----
    # --- --- --- --- --- ---
    
    # Action buttons for each tab / page
    action_buttons_plugins_management <- c("delete", "edit_code", "options")
    action_buttons_export_plugins <- "add"
    
    editable_cols <- ""
    sortable_cols <- c("id", "name", "creation_datetime", "update_datetime")
    column_widths <- c("id" = "80px", "creation_datetime" = "130px", "update_datetime" = "130px", "action" = "100px")
    centered_cols <- c("id", "creation_datetime", "update_datetime", "action")
    searchable_cols <- c("name")
    hidden_cols <- c("id", "description", "tab_type_id", "deleted", "modified")
    col_names <- get_col_names("plugins", i18n)
    
    # Prepare data for datatable
    
    observeEvent(r$plugins, {
      
      if (nrow(r$plugins) == 0){
        r[[paste0(prefix, "_plugins_temp")]] <- tibble::tibble(id = integer(), name = character(),
          tab_type_id = integer(), creation_datetime = character(), update_datetime = character(), deleted = integer(), modified = logical())
      }
      else r[[paste0(prefix, "_plugins_temp")]] <- r$plugins %>% dplyr::filter(tab_type_id == !!tab_type_id) %>%
          dplyr::mutate_at(c("creation_datetime", "update_datetime"), format_datetime, language = "en", sec = FALSE) %>%
          dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
    }, once = TRUE)
    
    observeEvent(r[[paste0(prefix, "_plugins_temp")]], {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$..plugins_temp"))
      r[[paste0(prefix, "_plugins_reload_datatables")]] <- Sys.time()
    })
    observeEvent(r[[paste0(prefix, "_export_plugins_temp")]], {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$..export_plugins_temp"))
      r[[paste0(prefix, "_plugins_reload_datatables")]] <- Sys.time()
    })
    
    observeEvent(r[[paste0(prefix, "_plugins_reload_datatables")]], {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$..plugins_reload_datatables"))
      
      # Reset selected plugins for export_plugins & export_plugins_selected
      r[[paste0(prefix, "_export_plugins_temp")]] <- r[[paste0(prefix, "_plugins_temp")]]
      r[[paste0(prefix, "_export_plugins_selected")]] <- r[[paste0(prefix, "_export_plugins_temp")]] %>% dplyr::slice(0)
      
      if (nrow(r[[paste0(prefix, "_plugins_temp")]]) == 0){
        
        # Default data for datatable
        
        data_plugins_datatable <- tibble::tibble(id = integer(), name = character(),
          tab_type_id = integer(), creation_datetime = character(), update_datetime = character(), deleted = integer(), modified = logical(), action = character())
        data_export_plugins_datatable <- data_plugins_datatable
        
      }
      if (nrow(r[[paste0(prefix, "_plugins_temp")]]) > 0){
        
        # Prepare data for datatables
        
        r[[paste0(prefix, "_plugins_datatable_temp")]] <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
          table = "plugins", action_buttons = action_buttons_plugins_management, data_input = r[[paste0(prefix, "_plugins_temp")]])
        data_plugins_datatable <- r[[paste0(prefix, "_plugins_datatable_temp")]]
        
        r[[paste0(prefix, "_export_plugins_datatable_temp")]] <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
          table = "plugins", action_buttons = action_buttons_export_plugins, data_input = r[[paste0(prefix, "_export_plugins_temp")]])
        data_export_plugins_datatable <- r[[paste0(prefix, "_export_plugins_datatable_temp")]]
      }
      
      # Render datatables
      
      # If datatable already exists
      if (length(r[[paste0(prefix, "_plugins_datatable_proxy")]]) > 0){
        DT::replaceData(r[[paste0(prefix, "_plugins_datatable_proxy")]], data_plugins_datatable, resetPaging = FALSE, rownames = FALSE)
        DT::replaceData(r[[paste0(prefix, "_plugins_to_export_datatable_proxy")]], data_export_plugins_datatable, resetPaging = FALSE, rownames = FALSE)
      }
      
      # If datatable doesn't exist
      if (length(r[[paste0(prefix, "_plugins_datatable_proxy")]]) == 0){
        
        render_datatable(output = output, ns = ns, i18n = i18n, data = data_plugins_datatable,
          output_name = "plugins_datatable", col_names = get_col_names(table_name = "plugins", i18n = i18n),
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, hidden_cols = hidden_cols, selection = "multiple")
        
        render_datatable(output = output, ns = ns, i18n = i18n, data = data_export_plugins_datatable,
          output_name = "plugins_to_export_datatable", col_names = get_col_names(table_name = "plugins", i18n = i18n),
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, hidden_cols = hidden_cols)
        
        # Create a proxy for datatables
        
        r[[paste0(prefix, "_plugins_datatable_proxy")]] <- DT::dataTableProxy("plugins_datatable", deferUntilFlush = FALSE)
        r[[paste0(prefix, "_plugins_to_export_datatable_proxy")]] <- DT::dataTableProxy("plugins_to_export_datatable", deferUntilFlush = FALSE)
      }
      
      # Update dropdowns
      options <- convert_tibble_to_list(r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      if (length(input$options_selected_plugin) == 0) link_id <- NULL
      else if (length(input$options_selected_plugin) > 1) link_id <- input$options_selected_plugin$key
      else link_id <- input$options_selected_plugin
      
      if (is.null(link_id)) value <- NULL
      if (!is.null(link_id)) value <- list(key = link_id, text = r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_plugin", options = options, value = value)
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_plugin", options = options, value = value)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer r$..plugins_reload_datatables"))
    })
    
    observeEvent(r[[paste0(prefix, "_export_plugins_temp")]], {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$..export_plugins_temp"))
      
      # Reload datatable_temp variable
      if (nrow(r[[paste0(prefix, "_export_plugins_temp")]]) == 0){
        data <- tibble::tibble(id = integer(), name = character(), tab_type_id = integer(),
          creation_datetime = character(), update_datetime = character(), deleted = integer(), modified = logical(), action = character())
      }
      if (nrow(r[[paste0(prefix, "_export_plugins_temp")]]) > 0){
        r[[paste0(prefix, "_export_plugins_datatable_temp")]] <- prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = id,
          table = "plugins", action_buttons = action_buttons_export_plugins, data_input = r[[paste0(prefix, "_export_plugins_temp")]])
        data <- r[[paste0(prefix, "_export_plugins_datatable_temp")]]
      }
      
      # Reload data of datatable
      if (length(r[[paste0(prefix, "_export_plugins_datatable_proxy")]]) > 0) DT::replaceData(r[[paste0(prefix, "_export_plugins_datatable_proxy")]], data, resetPaging = FALSE, rownames = FALSE)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer r$..export_plugins_temp"))
    })
    
    # Updates on datatable data
    observeEvent(input$plugins_datatable_cell_edit, {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$plugins_datatable_cell_edit"))
      
      edit_info <- input$plugins_datatable_cell_edit
      r[[paste0(prefix, "_plugins_temp")]] <- DT::editData(r[[paste0(prefix, "_plugins_temp")]], edit_info, rownames = FALSE)
      
      # Store that this row has been modified
      r[[paste0(prefix, "_plugins_temp")]][[edit_info$row, "modified"]] <- TRUE
    })
    
    observeEvent(input$plugins_to_export_datatable_cell_edit, {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$plugins_to_export_datatable_cell_edit"))
      
      edit_info <- input$plugins_to_export_datatable_cell_edit
      r[[paste0(prefix, "_export_plugins_temp")]] <- DT::editData(r[[paste0(prefix, "_export_plugins_temp")]], edit_info, rownames = FALSE)
      
      # Store that this row has been modified
      r[[paste0(prefix, "_export_plugins_temp")]][[edit_info$row, "modified"]] <- TRUE
    })
    
    # Save updates
    observeEvent(input$save_plugins_management, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$save_plugins_management"))
      
      if (nrow(r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(modified)) == 0) show_message_bar(output,  "modif_saved", "success", i18n = i18n, ns = ns)
      
      req(nrow(r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(modified)) > 0)
      
      save_settings_datatable_updates(output = output, r = r, ns = ns, 
        table = "plugins", r_table = paste0(prefix, "_plugins"), i18n = i18n, duplicates_allowed = FALSE)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer input$save_plugins_management"))
    })
    
    # Delete a row or multiple rows in datatable
    
    plugin_delete_prefix <- paste0(prefix, "_plugin")
    plugin_dialog_title <- "plugins_delete"
    plugin_dialog_subtext <- "plugins_delete_subtext"
    plugin_react_variable <- "plugin_delete_confirm"
    plugin_table <- "plugins"
    plugin_id_var_sql <- "id"
    plugin_id_var_r <- "delete_plugins"
    plugin_delete_message <- "plugins_deleted"
    plugin_reload_variable <- paste0("reload" , prefix, "_plugins")
    plugin_information_variable <- "plugin_deleted"
    plugin_delete_variable <- paste0(plugin_delete_prefix, "_open_dialog")
    
    delete_element(r = r, input = input, output = output, session = session, ns = ns, i18n = i18n,
      delete_prefix = plugin_delete_prefix, dialog_title = plugin_dialog_title, dialog_subtext = plugin_dialog_subtext,
      react_variable = plugin_react_variable, table = plugin_table, id_var_sql = plugin_id_var_sql, id_var_r = plugin_id_var_r,
      delete_message = plugin_delete_message, translation = TRUE, reload_variable = plugin_reload_variable,
      information_variable = plugin_information_variable, app_folder = r$app_folder, prefix = prefix)
    
    # Delete one row (with icon on DT)
    
    observeEvent(input$deleted_pressed, {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$deleted_pressed"))
      
      r$delete_plugins <- as.integer(substr(input$deleted_pressed, nchar("delete_") + 1, 100))
      r[[plugin_delete_variable]] <- TRUE
    })
    
    # Delete multiple rows (with "Delete selection" button)
    
    observeEvent(input$delete_selection, {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$delete_selection"))
      
      req(length(input$plugins_datatable_rows_selected) > 0)
      
      r$delete_plugins <- r[[paste0(prefix, "_plugins_temp")]][input$plugins_datatable_rows_selected, ] %>% dplyr::pull(id)
      r[[plugin_delete_variable]] <- TRUE
    })
    
    observeEvent(r[[plugin_reload_variable]], {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$reload..plugins"))
      
      # Reload datatable
      r[[paste0(prefix, "_plugins_temp")]] <- r$plugins %>% dplyr::filter(tab_type_id == !!tab_type_id) %>% 
        dplyr::mutate_at(c("creation_datetime", "update_datetime"), format_datetime, language = "en", sec = FALSE) %>%
        dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      # Reload remote_git description if opened
      r$show_plugin_details <- Sys.time()
      
      # Reload export plugin datatable
      
      # Reset "edit plugin code" fields and "plugin options" fields
      
      options <- convert_tibble_to_list(r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(tab_type_id == !!tab_type_id) %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_plugin", options = options, value = NULL)
      shiny.fluent::updateChoiceGroup.shinyInput(session, "edit_code_ui_server", value = "ui")
      shiny.fluent::updateToggle.shinyInput(session, "hide_editor", value = FALSE)
      sapply(c("ace_edit_code_ui", "ace_edit_code_server", "ace_edit_code_translations"),
        function(name) shinyAce::updateAceEditor(session, name, value = ""))
      
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_plugin", options = options, value = NULL)
      sapply(c("plugin_author", "plugin_version", paste0("plugin_name_", r$languages$code), paste0("plugin_category_", r$languages$code)),
        function(name) shiny.fluent::updateTextField.shinyInput(session, name, value = ""))
      shiny.fluent::updateChoiceGroup.shinyInput(session, "users_allowed_read_group", value = "everybody")
      shiny.fluent::updateDropdown.shinyInput(session, "plugin_image", options = list(), value = NULL)
      sapply(paste0("plugin_description_", r$languages$code), function(name) shinyAce::updateAceEditor(session, name, value = ""))
      
      shiny.fluent::updateComboBox.shinyInput(session, "plugins_to_export", value = "")
    })
    
    observeEvent(input$edit_code, {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$edit_code"))
      
      link_id <- as.integer(substr(input$edit_code, nchar("edit_code_") + 1, nchar(input$edit_code)))
      
      options <- convert_tibble_to_list(r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(tab_type_id == !!tab_type_id) %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = link_id, text = r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_plugin", options = options, value = value)
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_plugin", options = options, value = value)
      
      # Set current pivot to edit_plugins_code
      shinyjs::runjs(glue::glue("$('#{id}-plugins_pivot button[name=\"{i18n$t('edit_plugin_code')}\"]').click();"))
    })
    
    observeEvent(input$options, {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$options"))
      
      # Get link_id variable, to update options div
      link_id <- as.integer(substr(input$options, nchar("options_") + 1, nchar(input$options)))
      
      options <- convert_tibble_to_list(r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(tab_type_id == !!tab_type_id) %>% dplyr::arrange(name), key_col = "id", text_col = "name")
      value <- list(key = link_id, text = r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
      
      shiny.fluent::updateComboBox.shinyInput(session, "code_selected_plugin", options = options, value = value)
      shiny.fluent::updateComboBox.shinyInput(session, "options_selected_plugin", options = options, value = value)
      
      # Set current pivot to edit_plugins_code
      shinyjs::runjs(glue::glue("$('#{id}-plugins_pivot button[name=\"{i18n$t('plugin_options')}\"]').click();"))
    })
    
    # --- --- --- -- -- -
    # Plugin options ----
    # --- --- --- -- -- -
    
    observeEvent(input$options_selected_plugin, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$options_selected_plugin"))
      
      if (length(input$options_selected_plugin) > 1) link_id <- input$options_selected_plugin$key
      else link_id <- input$options_selected_plugin
      if (length(input$code_selected_plugin) > 0){
        if (length(input$code_selected_plugin) > 1) code_link_id <- input$code_selected_plugin$key
        else code_link_id <- input$code_selected_plugin
      }
      else code_link_id <- 0L
      
      if (link_id != code_link_id){
        options <- convert_tibble_to_list(r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(tab_type_id == !!tab_type_id) %>% dplyr::arrange(name), key_col = "id", text_col = "name")
        value <- list(key = link_id, text = r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
        shiny.fluent::updateComboBox.shinyInput(session, "code_selected_plugin", options = options, value = value)
      }
      
      # Plugin options
      
      options <- r$options %>% dplyr::filter(category == "plugin", link_id == !!link_id)
      req(nrow(options) > 0)
      
      # All users
      picker_options <-
        r$users %>%
        dplyr::left_join(r$users_statuses %>% dplyr::select(user_status_id = id, user_status = name), by = "user_status_id") %>%
        dplyr::transmute(
          key = id,
          imageInitials = paste0(substr(firstname, 0, 1), substr(lastname, 0, 1)),
          text = paste0(firstname, " ", lastname),
          secondaryText = user_status)
      
      # Users who has access
      value <-
        picker_options %>%
        dplyr::mutate(n = 1:dplyr::n()) %>%
        dplyr::inner_join(
          options %>%
            dplyr::filter(name == "user_allowed_read") %>%
            dplyr::select(key = value_num),
          by = "key"
        ) %>%
        dplyr::pull(key)
      
      selected_items <- picker_options %>% dplyr::filter(key %in% value)
      
      shiny.fluent::updateChoiceGroup.shinyInput(session, "users_allowed_read_group",
        value = options %>% dplyr::filter(name == "users_allowed_read_group") %>% dplyr::pull(value))
      
      output$users_allowed_read_div <- renderUI({
        make_people_picker(
          i18n = i18n, ns = ns, id = "users_allowed_read", label = "users", options = picker_options, value = value,
          width = "100%", style = "padding-bottom:10px;")
      })
      
      # Plugin version, author, image and descriptions
      
      plugin_folder <- paste0(r$app_folder, "/plugins/", prefix, "/", options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
      files_list <- list.files(path = plugin_folder, pattern = "*.\\.(jpeg|jpg|JPG|JPEG|png|PNG|SVG|svg)$")
      shiny.fluent::updateDropdown.shinyInput(session, "plugin_image", 
        options = convert_tibble_to_list(tibble::tibble(text = c("", files_list), key = c("", files_list)), key_col = "key", text_col = "text"),
        value = options %>% dplyr::filter(name == "image") %>% dplyr::pull(value))
      
      for (field in c("version", "author", paste0("name_", r$languages$code), paste0("category_", r$languages$code))){
        value <- options %>% dplyr::filter(name == field) %>% dplyr::pull(value) %>% stringr::str_replace_all("''", "'")
        if (is.na(value)) value <- ""
        
        shiny.fluent::updateTextField.shinyInput(session, paste0("plugin_", field), value = value)
      }
      
      for (field in paste0("description_", r$languages$code)) shinyAce::updateAceEditor(session,
        paste0("plugin_", field), value = options %>% dplyr::filter(name == field) %>% dplyr::pull(value) %>% stringr::str_replace_all("''", "'"))
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer input$options_selected_plugin"))
    })
    
    # Render selected image
    
    output$render_image <- renderImage({
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - output$render_image"))
      
      req(length(input$plugin_image) > 0)
      req(input$plugin_image != "")
      
      link_id <- input$options_selected_plugin$key
      options <- r$options %>% dplyr::filter(category == "plugin", link_id == !!link_id)
      plugin_folder <- paste0(r$app_folder, "/plugins/", prefix, "/", options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
      
      list(src = paste0(plugin_folder, "/", input$plugin_image), width = 318, height = 200)
    }, deleteFile = FALSE)
    
    # Save updates
    
    sapply(r$languages$code, function(lang){
      observeEvent(input[[paste0("plugin_description_", lang, "_save")]], {
        if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$plugin_description_", lang, "_save"))
        r[[paste0(id, "_save_options")]] <- Sys.time()
      })
    })
    observeEvent(input$save_plugin_options, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$save_plugin_options"))
      r[[paste0(id, "_save_options")]] <- Sys.time()
    })
    
    observeEvent(r[[paste0(id, "_save_options")]], {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$..save_options"))
      
      req(length(input$options_selected_plugin) > 0)
      if (length(input$options_selected_plugin) > 1) link_id <- input$options_selected_plugin$key
      else link_id <- input$code_selected_plugin
      
      plugin_name <- input[[paste0("plugin_name_", language)]]
      if (is.na(plugin_name) | plugin_name == "") shiny.fluent::updateTextField.shinyInput(session, 
        paste0("plugin_name_", language), errorMessage = i18n$t("provide_valid_name"))
      
      req(!is.na(plugin_name) & plugin_name != "")
      
      duplicate_names <- FALSE
      current_names <- r$plugins %>% dplyr::filter(tab_type_id == !!tab_type_id, id != link_id) %>% dplyr::pull(name)
      if (plugin_name %in% current_names){
        duplicate_names <- TRUE
        shiny.fluent::updateTextField.shinyInput(session, paste0("plugin_name_", language), errorMessage = i18n$t("name_already_used"))
      }
      
      req(!duplicate_names)
      
      if (!is.na(plugin_name) & plugin_name != "") shiny.fluent::updateTextField.shinyInput(session, 
        paste0("plugin_name_", language), errorMessage = NULL)
      
      data <- list()
      data$users_allowed_read <- unique(input$users_allowed_read)
      for (field in c("users_allowed_read_group", "plugin_version", "plugin_author", "plugin_image",
        paste0("plugin_name_", r$languages$code), paste0("plugin_category_", r$languages$code),
        paste0("plugin_description_", r$languages$code))) data[[stringr::str_replace(field, "plugin_", "")]] <- input[[field]]
      
      save_settings_options(output = output, r = r, id = id, category = "plugin", code_id_input = paste0("options_", link_id),
        i18n = i18n, data = data, page_options = c("users_allowed_read", "version", "author", "image",
          paste0("name_", r$languages$code), paste0("category_", r$languages$code), paste0("description_", r$languages$code)))
      
      # Change plugin_name & update_datetime in plugins table
      new_update_datetime <- as.character(Sys.time())
      sql <- glue::glue_sql("UPDATE plugins SET name = {plugin_name}, update_datetime = {new_update_datetime} WHERE id = {link_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      
      r$plugins <- r$plugins %>% dplyr::mutate(
        name = dplyr::case_when(id == link_id ~ plugin_name, TRUE ~ name),
        update_datetime = dplyr::case_when(id == link_id ~ new_update_datetime, TRUE ~ update_datetime))
      r[[paste0(prefix, "_plugins_temp")]] <- r$plugins %>% dplyr::filter(tab_type_id == !!tab_type_id) %>%
        dplyr::mutate_at(c("creation_datetime", "update_datetime"), format_datetime, language = "en", sec = FALSE) %>%
        dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      r$show_plugin_details <- Sys.time()
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer r$..save_options"))
    })
    
    # Delete an image
    
    observeEvent(input$delete_image, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$delete_image"))
      req(length(input$plugin_image) > 0 & input$plugin_image != "")
      r[[paste0(prefix, "_plugins_delete_image")]] <- TRUE
    })
    
    r[[paste0(prefix, "_plugins_delete_image")]] <- FALSE
    output$plugin_image_delete_confirm <- shiny.fluent::renderReact({
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - output$plugin_image_delete_confirm"))
      
      shiny.fluent::Dialog(
        hidden = !r[[paste0(prefix, "_plugins_delete_image")]],
        onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", prefix, "_plugin_delete_image_hide_dialog', Math.random()); }")),
        dialogContentProps = list(
          type = 0,
          title = i18n$t("plugin_image_delete"),
          closeButtonAriaLabel = "Close",
          subText = tagList(i18n$t("plugin_image_delete_subtext"), br(), br())
        ),
        modalProps = list(),
        shiny.fluent::DialogFooter(
          shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "_plugin_delete_image_delete_confirmed")), text = i18n$t("delete")),
          shiny.fluent::DefaultButton.shinyInput(ns(paste0(prefix, "_plugin_delete_image_delete_canceled")), text = i18n$t("dont_delete"))
        )
      )
    })
    
    observeEvent(input[[paste0(prefix, "_plugin_delete_image_hide_dialog")]], {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$..plugin_delete_image_hide_dialog"))
      r[[paste0(prefix, "_plugins_delete_image")]] <- FALSE
    })
    observeEvent(input[[paste0(prefix, "_plugin_delete_image_delete_canceled")]], {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$..plugin_delete_image_delete_canceled"))
      r[[paste0(prefix, "_plugins_delete_image")]] <- FALSE
    })
    
    observeEvent(input[[paste0(prefix, "_plugin_delete_image_delete_confirmed")]], {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$..plugin_delete_image_delete_confirmed"))
      
      req(input$plugin_image != "")
      tryCatch({
        link_id <- input$options_selected_plugin$key
        
        plugin <- r$plugins %>% dplyr::filter(id == link_id) %>%
          dplyr::left_join(
            r$options %>% dplyr::filter(category == "plugin", name == "unique_id") %>% dplyr::select(id = link_id, unique_id = value),
            by = "id"
          )
        
        plugin_folder <- paste0(r$app_folder, "/plugins/", prefix, "/", plugin$unique_id)
        unlink(paste0(plugin_folder, "/", input$plugin_image))
        
        files_list <- list.files(path = plugin_folder, pattern = "*.\\.(jpeg|jpg|JPG|JPEG|png|PNG|SVG|svg)$")
        shiny.fluent::updateDropdown.shinyInput(session, "plugin_image", 
          options = convert_tibble_to_list(tibble::tibble(text = c("", files_list), key = c("", files_list)), key_col = "key", text_col = "text"),
          value = "")
        
        show_message_bar(output,  "image_deleted", "success", i18n = i18n, ns = ns)
        
      }, error = function(e) report_bug(r = r, output = output, error_message = "error_deleting_image",
        error_name = paste0(id, " - delete plugin image"), category = "Error", error_report = toString(e), i18n = i18n))
      
      r[[paste0(prefix, "_plugins_delete_image")]] <- FALSE
    })
    
    # Import an image
    
    observeEvent(input$import_image, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$import_image"))
      shinyjs::click("import_image_file")
    })
    
    observeEvent(input$import_image_file, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$import_image_file"))
      
      tryCatch({
        
        # if (length(input$options_selected_plugin) > 1) link_id <- input$options_selected_plugin$key
        # else link_id <- input$code_selected_plugin
        link_id <- input$options_selected_plugin$key
        
        plugin <- r$plugins %>%
          dplyr::filter(id == link_id) %>%
          dplyr::left_join(
            r$options %>% dplyr::filter(category == "plugin", name == "unique_id") %>% dplyr::select(id = link_id, unique_id = value),
            by = "id"
          )
        
        plugin_folder <- paste0(r$app_folder, "/plugins/", prefix, "/", plugin$unique_id)
        
        if (!dir.exists(plugin_folder)) dir.create(plugin_folder, recursive = TRUE)
        
        # If option resize is activated, resize and save the image
        if (input$resize_image) resize_and_pad_image(input_path = input$import_image_file$datapath, output_path = paste0(plugin_folder, "/", input$import_image_file$name), 
          target_width = 2544, target_height = 1600, i18n = i18n)
        
        # Else, just save the image
        else file.copy(input$import_image_file$datapath, paste0(plugin_folder, "/", input$import_image_file$name), overwrite = TRUE)
        
        # Update dropdown
        
        options <- r$options %>% dplyr::filter(category == "plugin", link_id == !!link_id)
        
        files_list <- list.files(path = plugin_folder, pattern = "*.\\.(jpeg|jpg|JPG|JPEG|png|PNG|SVG|svg)$")
        shiny.fluent::updateDropdown.shinyInput(session, "plugin_image", 
          options = convert_tibble_to_list(tibble::tibble(text = c("", files_list), key = c("", files_list)), key_col = "key", text_col = "text"),
          value = options %>% dplyr::filter(name == "image") %>% dplyr::pull(value))
        
        show_message_bar(output,  "image_imported", "success", i18n = i18n, ns = ns)
        
      }, error = function(e) report_bug(r = r, output = output, error_message = "error_importing_image",
        error_name = paste0(id, " - import plugin image"), category = "Error", error_report = toString(e), i18n = i18n))
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer input$import_image_file"))
    })
    
    # Render markdown
    
    observeEvent(input$execute_options_description, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$execute_options_description"))
      r[[paste0(prefix, "_plugin_options_description_trigger")]] <- Sys.time()
    })
    
    observeEvent(input$plugin_description_fr_run_all, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$plugin_description_fr_run_all"))
      r[[paste0(prefix, "_plugin_options_description_trigger")]] <- Sys.time()
    })
    
    observeEvent(input$plugin_description_en_run_all, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$plugin_description_en_run_all"))
      r[[paste0(prefix, "_plugin_options_description_trigger")]] <- Sys.time()
    })
    
    observeEvent(r[[paste0(prefix, "_plugin_options_description_trigger")]], {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$.._plugin_options_description_trigger"))
      
      options <- r$options %>% dplyr::filter(category == "plugin", link_id == input$options_selected_plugin$key)
      plugin_folder <- paste0(r$app_folder, "/plugins/", prefix, "/", options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
      
      options_description <- isolate(input[[paste0("plugin_description_", input$plugin_language)]] %>% 
          stringr::str_replace_all("\r", "\n")) %>%
          stringr::str_replace_all("%plugin_folder%", plugin_folder)
      
      tryCatch({
        
        # Clear temp dir
        # unlink(paste0(r$app_folder, "/temp_files"), recursive = TRUE, force = TRUE)
        
        markdown_settings <- paste0("```{r setup, include=FALSE}\nknitr::opts_knit$set(root.dir = '", 
          r$app_folder, "/temp_files')\n",
          "knitr::opts_chunk$set(root.dir = '", r$app_folder, "/temp_files', fig.path = '", r$app_folder, "/temp_files')\n```\n")
        
        markdown_file <- paste0(markdown_settings, options_description)
        
        # Create temp dir
        dir <- paste0(r$app_folder, "/temp_files")
        file <- paste0(dir, "/", paste0(sample(c(0:9, letters[1:6]), 8, TRUE), collapse = ''), "_", as.character(Sys.time()) %>% stringr::str_replace_all(":", "_"), ".Md")
        if (!dir.exists(dir)) dir.create(dir)
        
        # Create the markdown file
        knitr::knit(text = markdown_file, output = file, quiet = TRUE)
        
        output$description_markdown_result <- renderUI(div(class = "markdown", withMathJax(includeMarkdown(file))))
      })
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer r$.._plugin_options_description_trigger"))
    })
    
    # --- --- --- --- --- -
    # Edit plugin code ----
    # --- --- --- --- --- -
    
    observeEvent(input$code_selected_plugin, {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$code_selected_plugin"))
      
      if (length(input$code_selected_plugin) > 1) link_id <- input$code_selected_plugin$key
      else link_id <- input$code_selected_plugin
      if (length(input$options_selected_plugin) > 0){
        if (length(input$options_selected_plugin) > 1) options_link_id <- input$options_selected_plugin$key
        else options_link_id <- input$options_selected_plugin
      }
      else options_link_id <- 0L
      
      if (link_id != options_link_id){
        options <- convert_tibble_to_list(r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(tab_type_id == !!tab_type_id) %>% dplyr::arrange(name), key_col = "id", text_col = "name")
        value <- list(key = link_id, text = r[[paste0(prefix, "_plugins_temp")]] %>% dplyr::filter(id == link_id) %>% dplyr::pull(name))
        shiny.fluent::updateComboBox.shinyInput(session, "options_selected_plugin", options = options, value = value)
      }
      
      # Get code from database
      
      req(r$code %>% dplyr::filter(category == "plugin_ui" & link_id == !!link_id) %>% nrow() > 0)
      
      code <- list()
      code$ui <- r$code %>% dplyr::filter(category == "plugin_ui" & link_id == !!link_id) %>% dplyr::pull(code)
      code$server <- r$code %>% dplyr::filter(category == "plugin_server" & link_id == !!link_id) %>% dplyr::pull(code) 
      code$translations <- r$code %>% dplyr::filter(category == "plugin_translations" & link_id == !!link_id) %>% dplyr::pull(code)
      
      for(name in c("ui", "server", "translations")) code[[name]] <- code[[name]] %>% stringr::str_replace_all("''", "'")
      
      shinyAce::updateAceEditor(session, "ace_edit_code_ui", value = code$ui)
      shinyAce::updateAceEditor(session, "ace_edit_code_server", value = code$server)
      shinyAce::updateAceEditor(session, "ace_edit_code_translations", value = code$translations)
      
      # Reset code_result textOutput
      output$datetime_code_execution <- renderText("")
      output$code_result_ui <- renderUI("")
      output$code_result_server <- renderText("")
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer input$code_selected_plugin"))
    })
    
    # Load vocabulary concepts
    
    # --- --- --- --- --- --- --
    ## Vocabulary datatable ----
    # --- --- --- --- --- --- --
    
    # Load vocabularies attached to this dataset
    observeEvent(r$dataset_vocabularies, {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$dataset_vocabularies"))
      
      if (nrow(r$dataset_vocabularies) == 0) shiny.fluent::updateComboBox.shinyInput(session, "vocabulary", options = list(), value = NULL)
      if (nrow(r$dataset_vocabularies) > 0){
        vocabulary_options <- convert_tibble_to_list(data = r$dataset_vocabularies, key_col = "vocabulary_id", text_col = "vocabulary_name", i18n = i18n)
        shiny.fluent::updateComboBox.shinyInput(session, "vocabulary", options = vocabulary_options, value = NULL)
      }
    })
    
    # Reload vocabulary concepts
    
    observeEvent(input$vocabulary, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$vocabulary"))
      r[[paste0(prefix, "_reload_plugin_vocabulary_concepts")]] <- Sys.time()
    })
    
    observeEvent(input$show_mapped_concepts, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$show_mapped_concepts"))
      r[[paste0(prefix, "_reload_plugin_vocabulary_concepts")]] <- Sys.time()
      if (input$show_mapped_concepts & !input$hide_concepts_datatables) shinyjs::show("plugin_vocabulary_mapped_concepts")
      else shinyjs::hide("plugin_vocabulary_mapped_concepts")
    })
    
    observeEvent(r[[paste0(prefix, "_reload_plugin_vocabulary_concepts")]], {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$..reload_plugin_vocabulary_concepts"))
      
      req(length(d$dataset_all_concepts) > 0, nrow(d$dataset_all_concepts) > 0)
      
      plugin_vocabulary_concepts <- d$dataset_all_concepts %>% 
        dplyr::filter(vocabulary_id_1 == input$vocabulary) %>%
        dplyr::select(concept_id = concept_id_1, concept_name = concept_name_1, concept_display_name = concept_display_name_1,
          relationship_id, domain_id, concept_class_id, standard_concept, concept_code,
          count_persons_rows, count_concepts_rows, add_concept_input)
      
      if (input$show_mapped_concepts) plugin_vocabulary_concepts <- plugin_vocabulary_concepts %>%
        dplyr::group_by(concept_id) %>%
        dplyr::summarize(count_persons_rows = max(count_persons_rows), count_concepts_rows = sum(count_concepts_rows)) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(
          plugin_vocabulary_concepts %>% dplyr::group_by(concept_id) %>% dplyr::slice(1) %>% dplyr::ungroup() %>% dplyr::select(-count_persons_rows, -count_concepts_rows),
          by = "concept_id"
        ) %>%
        dplyr::relocate(count_persons_rows, count_concepts_rows, .after = "concept_code") %>%
        dplyr::left_join(
          plugin_vocabulary_concepts %>% dplyr::filter(is.na(relationship_id)) %>% dplyr::transmute(concept_id, no_mapping = TRUE),
          by = "concept_id"
        ) %>%
        dplyr::mutate(add_concept_input = ifelse(is.na(no_mapping), "", add_concept_input)) %>%
        dplyr::select(-no_mapping)
      
      if (!input$show_mapped_concepts) plugin_vocabulary_concepts <- plugin_vocabulary_concepts %>%
        dplyr::filter(is.na(relationship_id))
      
      plugin_vocabulary_concepts <- plugin_vocabulary_concepts %>%
        dplyr::select(-relationship_id) %>%
        dplyr::mutate_at(c("add_concept_input"), stringr::str_replace_all, "%ns%", id) %>%
        dplyr::mutate_at("add_concept_input", stringr::str_replace_all, "%input_prefix%", "add_concept") %>%
        dplyr::mutate_at("add_concept_input", stringr::str_replace_all, "%input_prefix_2%", "") %>%
        dplyr::mutate_at("concept_id", as.character) %>%
        dplyr::mutate(add_concept_input = stringr::str_replace_all(add_concept_input, "%concept_id_1%", concept_id))
      
      r[[paste0(prefix, "_plugin_vocabulary_concepts")]] <- plugin_vocabulary_concepts
      
      if (length(r[[paste0(prefix, "_plugin_vocabulary_concepts_proxy")]]) == 0){
        editable_cols <- c("concept_display_name")
        searchable_cols <- c("concept_id", "concept_name", "concept_display_name")
        column_widths <- c("concept_id" = "80px", "action" = "80px")
        sortable_cols <- c("concept_id", "concept_name", "concept_display_name", "count_persons_rows", "count_concepts_rows")
        centered_cols <- c("concept_id", "count_persons_rows", "count_concepts_rows", "add_concept_input")
        col_names <- get_col_names(table_name = "plugins_vocabulary_concepts_with_counts", i18n = i18n)
        hidden_cols <- c("domain_id", "concept_class_id", "standard_concept", "concept_code")
        column_widths <- c("concept_id" = "120px", "count_persons_rows" = "80px", "count_concepts_rows" = "80px", 
          "add_concept_input" = "80px")
        
        # Render datatable
        render_datatable(output = output, ns = ns, i18n = i18n, data = plugin_vocabulary_concepts,
          output_name = "plugin_vocabulary_concepts", col_names =  col_names,
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, hidden_col = hidden_cols)
        
        # Create a proxy for datatatable
        r[[paste0(prefix, "_plugin_vocabulary_concepts_proxy")]] <- DT::dataTableProxy("plugin_vocabulary_concepts", deferUntilFlush = FALSE)
        
        if (input$hide_concepts_datatables) shinyjs::show("blank_space") else shinyjs::hide("blank_space")
      }
      else DT::replaceData(r[[paste0(prefix, "_plugin_vocabulary_concepts_proxy")]], r[[paste0(prefix, "_plugin_vocabulary_concepts")]], resetPaging = FALSE, rownames = FALSE)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer r$..reload_plugin_vocabulary_concepts"))
    })
    
    # Update which cols are hidden
    
    observeEvent(input$vocabulary_concepts_table_cols, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$vocabulary_concepts_table_cols"))
      
      req(length(r[[paste0(prefix, "_plugin_vocabulary_concepts_proxy")]]) > 0)
      
      r[[paste0(prefix, "_plugin_vocabulary_concepts_proxy")]] %>%
        DT::showCols(0:9) %>%
        DT::hideCols(setdiff(0:9, input$vocabulary_concepts_table_cols))
    })
    
    observeEvent(input$vocabulary_mapped_concepts_table_cols, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$vocabulary_mapped_concepts_table_cols"))
      
      req(length(r[[paste0(prefix, "_plugin_vocabulary_mapped_concepts_proxy")]]) > 0)
      
      r[[paste0(prefix, "_plugin_vocabulary_mapped_concepts_proxy")]] %>%
        DT::showCols(1:9) %>%
        DT::hideCols(setdiff(1:9, input$vocabulary_mapped_concepts_table_cols))
    })
    
    # Hide datatables
    
    observeEvent(input$hide_concepts_datatables, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$hide_concepts_datatables"))
      
      req(input$vocabulary)
      
      sapply(c("plugin_vocabulary_concepts", "plugin_vocabulary_mapped_concepts"), function(datatable) if (input$hide_concepts_datatables) 
        shinyjs::hide(datatable) else shinyjs::show(datatable))
      if (input$hide_concepts_datatables) shinyjs::show("blank_space") else shinyjs::hide("blank_space")
    })
    
    # Updates in datatable
    
    observeEvent(input$plugin_vocabulary_concepts_cell_edit, {

      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$plugin_vocabulary_concepts_cell_edit"))

      edit_info <- input$plugin_vocabulary_concepts_cell_edit
      r[[paste0(prefix, "_plugin_vocabulary_concepts")]] <- DT::editData(r[[paste0(prefix, "_plugin_vocabulary_concepts")]], edit_info, rownames = FALSE)
    })
    
    observeEvent(input$plugin_vocabulary_mapped_concepts_cell_edit, {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$plugin_vocabulary_mapped_concepts_cell_edit"))
      
      edit_info <- input$plugin_vocabulary_mapped_concepts_cell_edit
      r[[paste0(prefix, "_plugin_vocabulary_mapped_concepts")]] <- DT::editData(r[[paste0(prefix, "_plugin_vocabulary_mapped_concepts")]], edit_info, rownames = FALSE)
    })
    
    # Show mapped concepts
    
    observeEvent(input$plugin_vocabulary_concepts_rows_selected, {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$plugin_vocabulary_concepts_rows_selected"))
      
      req(input$show_mapped_concepts)
      
      selected_concept <- r[[paste0(prefix, "_plugin_vocabulary_concepts")]][input$plugin_vocabulary_concepts_rows_selected, ]
      
      r[[paste0(prefix, "_plugin_vocabulary_mapped_concepts")]] <- d$dataset_all_concepts %>%
        dplyr::filter(concept_id_1 == selected_concept$concept_id, !is.na(relationship_id)) %>%
        dplyr::transmute(concept_id_1, relationship_id, concept_id_2, concept_name_2,
          count_persons_rows, count_concepts_rows) %>%
        dplyr::left_join(
          d$dataset_all_concepts %>%
            dplyr::select(concept_id_2 = concept_id_1, concept_display_name_2 = concept_display_name_1, domain_id, add_concept_input),
          by = "concept_id_2"
        ) %>% 
        dplyr::relocate(concept_display_name_2, .after = "concept_name_2") %>%
        dplyr::relocate(domain_id, .after = "concept_display_name_2") %>%
        dplyr::mutate(id = NA_integer_, .before = "concept_id_1")
      
      if (nrow(r[[paste0(prefix, "_plugin_vocabulary_mapped_concepts")]]) > 0) r[[paste0(prefix, "_plugin_vocabulary_mapped_concepts")]] <-
        r[[paste0(prefix, "_plugin_vocabulary_mapped_concepts")]] %>%
        dplyr::group_by_all() %>% dplyr::slice(1) %>% dplyr::ungroup() %>%
        dplyr::mutate_at(c("add_concept_input"), stringr::str_replace_all, "%ns%", id) %>%
        dplyr::mutate_at("add_concept_input", stringr::str_replace_all, "%input_prefix%", "add_mapped_concept") %>%
        dplyr::mutate_at("add_concept_input", stringr::str_replace_all, "%input_prefix_2%", "") %>%
        dplyr::mutate_at(c("concept_id_1", "concept_id_2"), as.character) %>%
        # Add a unique id (rows are not unique with only concept_id_2, cause there can be multiple concept_relationship)
        dplyr::mutate(id = 1:dplyr::n()) %>%
        dplyr::mutate(add_concept_input = stringr::str_replace_all(add_concept_input, "%concept_id_1%", as.character(id)))
      
      if (length(r[[paste0(prefix, "_plugin_vocabulary_mapped_concepts_proxy")]]) == 0){
        editable_cols <- c("concept_display_name_2")
        searchable_cols <- c("relationship_id", "concept_id_2", "concept_name_2", "concept_display_name_2")
        column_widths <- c("concept_id_1" = "120px", "concept_id_2" = "120px", "count_persons_rows" = "80px", "count_concepts_rows" = "80px", 
          "add_concept_input" = "80px")
        sortable_cols <- c("relationship_id", "concept_id_2", "concept_name_2", "concept_display_name_2", "count_persons_rows", "count_concepts_rows")
        centered_cols <- c("concept_id_1", "concept_id_2", "count_persons_rows", "count_concepts_rows", "add_concept_input")
        col_names <- get_col_names(table_name = "plugins_vocabulary_mapped_concepts_with_counts", i18n = i18n)
        hidden_cols <- c("id", "concept_id_1", "domain_id")
        column_widths <- c("concept_id" = "100px", "count_persons_rows" = "80px", "count_concepts_rows" = "80px", 
          "add_concept_input" = "80px")
        
        # Render datatable
        render_datatable(output = output, ns = ns, i18n = i18n, data = r[[paste0(prefix, "_plugin_vocabulary_mapped_concepts")]],
          output_name = "plugin_vocabulary_mapped_concepts", col_names =  col_names,
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, hidden_col = hidden_cols)
        
        # Create a proxy for datatatable
        r[[paste0(prefix, "_plugin_vocabulary_mapped_concepts_proxy")]] <- DT::dataTableProxy("plugin_vocabulary_mapped_concepts", deferUntilFlush = FALSE)
      }
      else DT::replaceData(r[[paste0(prefix, "_plugin_vocabulary_mapped_concepts_proxy")]], r[[paste0(prefix, "_plugin_vocabulary_mapped_concepts")]], resetPaging = FALSE, rownames = FALSE)
    })
    
    # --- --- --- --- --- -- -
    # Vocabulary concepts ----
    # --- --- --- --- --- -- -
    
    # When add button is clicked
    observeEvent(input$concept_selected, {

      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$concept_selected"))

      # Initiate r variable if doesn't exist
      if (length(r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]]) == 0) r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]] <- tibble::tibble( 
          concept_id = integer(), concept_name = character(), concept_display_name = character(), domain_id = character(),
          mapped_to_concept_id = integer(), merge_mapped_concepts = logical())

      if (grepl("mapped", input$concept_selected)) type <- "mapped_concept"
      else type <- "concept"
      
      # Get ID of selected concept
      link_id <- as.integer(substr(input$concept_selected, nchar(paste0(id, "-add_", type, "_")) + 1, nchar(input$concept_selected)))
      if (type == "concept") concept_id <- link_id
      if (type == "mapped_concept") concept_id <- r[[paste0(prefix, "_plugin_vocabulary_mapped_concepts")]] %>% dplyr::filter(id == link_id) %>% dplyr::pull(concept_id_2)
      
      # If this concept is not already selected, add it to the vocabulary_selected_concepts dropdown

      if (link_id %not_in% r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]]$concept_id){
        
        if (type == "concept") new_data <- r[[paste0(prefix, "_plugin_vocabulary_concepts")]] %>%
          dplyr::mutate_at("concept_id", as.integer) %>%
          dplyr::filter(concept_id == link_id) %>%
          dplyr::transmute(concept_id, concept_name, concept_display_name, domain_id, mapped_to_concept_id = NA_integer_, merge_mapped_concepts = FALSE)
        
        if (type == "mapped_concept"){
          
          selected_concept <- r[[paste0(prefix, "_plugin_vocabulary_mapped_concepts")]] %>%
            dplyr::mutate_at(c("concept_id_1", "concept_id_2"), as.integer) %>%
            dplyr::filter(id == link_id)
          
          new_data <- r[[paste0(prefix, "_plugin_vocabulary_concepts")]] %>%
            dplyr::mutate_at("concept_id", as.integer) %>%
            dplyr::filter(concept_id == selected_concept$concept_id_1) %>%
            dplyr::transmute(concept_id, concept_name, concept_display_name, domain_id,
              mapped_to_concept_id = NA_integer_, merge_mapped_concepts = input$merge_mapped_concepts) %>%
            dplyr::bind_rows(
              selected_concept %>%
              dplyr::transmute(concept_id = concept_id_2, concept_name = concept_name_2, concept_display_name = concept_display_name_2, domain_id,
                mapped_to_concept_id = concept_id_1, merge_mapped_concepts = input$merge_mapped_concepts)
            ) %>%
            dplyr::bind_rows(
              r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]] %>% dplyr::filter(mapped_to_concept_id == selected_concept$concept_id_1) %>%
                dplyr::mutate(merge_mapped_concepts = input$merge_mapped_concepts)
            )
          
          # Add also original concept, which concepts are mapped from
          r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]] <- r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]] %>% 
            dplyr::filter(concept_id != selected_concept$concept_id_1, (is.na(mapped_to_concept_id) | mapped_to_concept_id != selected_concept$concept_id_1))
        }
        
        r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]] <- new_data %>% dplyr::bind_rows(r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]])
      }
      
      # Update dropdown of selected concepts
      
      r[[paste0(prefix, "_plugin_vocabulary_update_selected_concepts_dropdown")]] <- Sys.time()

      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer input$concept_selected"))

    })
    
    # When reset button is clicked
    observeEvent(input$reset_vocabulary_concepts, {

      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$reset_vocabulary_concepts"))

      # Reset r$..plugin_vocabulary_selected_concepts
      r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]] <- r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]] %>% dplyr::slice(0)

      r[[paste0(prefix, "_plugin_vocabulary_update_selected_concepts_dropdown")]] <- Sys.time()
    })
    
    # When dropdown is modified
    observeEvent(input$vocabulary_selected_concepts_trigger, {

      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$vocabulary_selected_concepts_trigger"))

      if (length(input$vocabulary_selected_concepts) == 0) r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]] <- r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]] %>% dplyr::slice(0)
      if (length(input$vocabulary_selected_concepts) > 0) {
        r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]] <- r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]] %>%
          dplyr::filter(concept_id %in% input$vocabulary_selected_concepts)
        
        # Delete also mapped items
        r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]] <- r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]] %>%
          dplyr::filter(is.na(mapped_to_concept_id) | mapped_to_concept_id %in% r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]]$concept_id)
      }
      
      r[[paste0(prefix, "_plugin_vocabulary_update_selected_concepts_dropdown")]] <- Sys.time()
    })
    
    # Update dropdown of selected concepts
    
    observeEvent(r[[paste0(prefix, "_plugin_vocabulary_update_selected_concepts_dropdown")]], {
      
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$..plugin_vocabulary_update_selected_concepts_dropdown"))
      
      options <- convert_tibble_to_list(
        r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]] %>%
          dplyr::mutate(concept_name = ifelse(!is.na(mapped_to_concept_id), paste0("--- ", concept_name), concept_name)), 
        key_col = "concept_id", text_col = "concept_name", i18n = i18n)
      value <- r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]] %>% dplyr::pull(concept_id)
      shiny.fluent::updateDropdown.shinyInput(session, "vocabulary_selected_concepts",
        options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
    })
    
    # --- --- --- -- --
    ## Execute code ----
    # --- --- --- -- --
    
    observeEvent(input$execute_code, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$execute_code"))
      r[[paste0(id, "_ui_code")]] <- input$ace_edit_code_ui
      r[[paste0(id, "_server_code")]] <- input$ace_edit_code_server
      r[[paste0(id, "_trigger_code")]] <- Sys.time()
    })
    
    observeEvent(input$ace_edit_code_ui_run_selection, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$ace_edit_code_ui_run_selection"))
      if(!shinyAce::is.empty(input$ace_edit_code_ui_run_selection$selection)) r[[paste0(id, "_ui_code")]] <- input$ace_edit_code_ui_run_selection$selection
      else r[[paste0(id, "_ui_code")]] <- input$ace_edit_code_ui_run_selection$line
      r[[paste0(id, "_server_code")]] <- ""
      r[[paste0(id, "_trigger_code")]] <- Sys.time()
    })
    
    observeEvent(input$ace_edit_code_server_run_selection, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$ace_edit_code_server_run_selection"))
      if(!shinyAce::is.empty(input$ace_edit_code_server_run_selection$selection)) r[[paste0(id, "_server_code")]] <- input$ace_edit_code_server_run_selection$selection
      else r[[paste0(id, "_server_code")]] <- input$ace_edit_code_server_run_selection$line
      r[[paste0(id, "_ui_code")]] <- ""
      r[[paste0(id, "_trigger_code")]] <- Sys.time()
    })
    
    observeEvent(input$ace_edit_code_ui_run_all, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$ace_edit_code_ui_run_all"))
      r[[paste0(id, "_ui_code")]] <- input$ace_edit_code_ui
      r[[paste0(id, "_server_code")]] <- input$ace_edit_code_server
      r[[paste0(id, "_trigger_code")]] <- Sys.time()
    })
    
    observeEvent(input$ace_edit_code_server_run_all, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$ace_edit_code_server_run_all"))
      r[[paste0(id, "_server_code")]] <- input$ace_edit_code_server
      r[[paste0(id, "_ui_code")]] <- input$ace_edit_code_ui
      r[[paste0(id, "_trigger_code")]] <- Sys.time()
    })
    
    observeEvent(r[[paste0(id, "_trigger_code")]], {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$..trigger_code"))
      
      var_check <- TRUE
      if (length(r$selected_dataset) == 0) var_check <- FALSE
      if (length(r$selected_dataset) > 0){
        if (is.na(r$selected_dataset) | is.na(m$selected_study)) var_check <- FALSE
        if (prefix == "patient_lvl" & is.na(m$selected_person)) var_check <- FALSE
      }
      
      if (!var_check) show_message_bar(output, message = "load_some_patient_data_plugin", i18n = i18n, ns = ns)
      
      req(var_check)
      
      # Get vocabulary concepts
      
      if (length(r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]]) > 0) selected_concepts <- r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]]
      if (length(r[[paste0(prefix, "_plugin_vocabulary_selected_concepts")]]) == 0) selected_concepts <- tibble::tibble(
        concept_id = integer(), concept_name = character(), concept_display_name = character(), domain_id = character(),
        mapped_to_concept_id = integer(), merge_mapped_concepts = logical())
      
      # concept_id concept_name   concept_display_name domain_id   mapped_to_concept_id merge_mapped_concepts
      
      if (length(input$code_selected_plugin) > 1) link_id <- input$code_selected_plugin$key
      else link_id <- input$code_selected_plugin
      
      ui_code <- r[[paste0(id, "_ui_code")]]
      server_code <- r[[paste0(id, "_server_code")]]
      
      widget_id <- get_last_row(r$db, paste0(prefix, "_widgets")) + 10^6 %>% as.integer()
      
      # Create a session number, to inactivate older observers
      # Reset all older observers
      
      session_code <- "plugin_test"
      if (length(m[[session_code]]) == 0) session_num <- 1L
      if (length(m[[session_code]]) > 0) session_num <- m[[session_code]] + 1
      m[[session_code]] <- session_num
      
      # NB : req(m[[session_code]] == session_num) & req(m$selected_study == %study_id%) must be put at the beginning of each observeEvent in plugins code
      
      ui_code <- ui_code %>% 
        stringr::str_replace_all("%tab_id%", "1") %>%
        stringr::str_replace_all("%widget_id%", as.character(widget_id)) %>%
        stringr::str_replace_all("%study_id%", as.character(m$selected_study)) %>%
        stringr::str_replace_all("\r", "\n")
      
      if (prefix == "patient_lvl") ui_code <- ui_code %>% stringr::str_replace_all("%patient_id%", as.character(m$selected_person))
      
      server_code <- server_code %>% 
        stringr::str_replace_all("%tab_id%", "1") %>%
        stringr::str_replace_all("%widget_id%", as.character(widget_id)) %>%
        stringr::str_replace_all("%req%", "req(m[[session_code]] == session_num)\nreq(m$selected_study == %study_id%)") %>%
        stringr::str_replace_all("%study_id%", as.character(m$selected_study)) %>%
        stringr::str_replace_all("\r", "\n")
      
      if (prefix == "patient_lvl") server_code <- server_code %>% stringr::str_replace_all("%patient_id%", as.character(m$selected_person))
      
      output$code_result_ui <- renderUI(make_shiny_ace_card("", tryCatch(eval(parse(text = ui_code)), error = function(e) p(toString(e)), warning = function(w) p(toString(w)))))
      
      # Create translations file
      
      if (input$ace_edit_code_translations != ""){
        
        tryCatch({
          # Get plugin unique_id
          plugin_unique_id <- r$options %>% dplyr::filter(category == "plugin", name == "unique_id", link_id == !!link_id) %>% dplyr::pull(value)
          
          # Create plugin folder in translations folder if doesn't exist
          new_dir <- paste0(r$app_folder, "/translations/", plugin_unique_id)
          if (!dir.exists(new_dir)) dir.create(new_dir)
          
          # Create a csv with all languages
          data <- read.csv(text = input$ace_edit_code_translations, header = TRUE, stringsAsFactors = FALSE)
          
          # Create one csv by language
          for(lang in names(data)[-1]){
            # Create a new dataframe with base & current language cols
            data_lang <- data[, c("base", lang)]
            
            file_name <- paste0(new_dir, "/translation_", lang, ".csv")
            
            # Create csv
            write.csv(data_lang, file_name, row.names = FALSE)
          }
        },
        error = function(e) report_bug(r = r, output = output, error_message = "error_creating_translations_file",
          error_name = paste0(id, " - create translations files"), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
        
        tryCatch({
          i18np <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = new_dir))
          i18np$set_translation_language(language)},
          error = function(e) report_bug(r = r, output = output, error_message = "error_creating_new_translator",
            error_name = paste0(id, " - create i18np translator"), category = "Error", error_report = toString(e), i18n = i18n, ns = ns))
      }
      
      # New environment, to authorize access to selected variables from shinyAce editor
      # We choose which vars to keep access to
      
      # Variables to hide
      new_env_vars <- list("r" = NA)
      
      # Variables to keep
      variables_to_keep <- c("d", "m", "session_code", "session_num", "i18n", "selected_concepts", "debug")
      if (exists("i18np")) variables_to_keep <- c(variables_to_keep, "i18np")
      
      for (var in variables_to_keep) new_env_vars[[var]] <- eval(parse(text = var))
      new_env <- rlang::new_environment(data = new_env_vars, parent = pryr::where("r"))
      
      options('cli.num_colors' = 1)
      
      # Capture console output of our code
      captured_output <- capture.output(
        tryCatch(eval(parse(text = server_code), envir = new_env), error = function(e) print(e), warning = function(w) print(w)))
      
      # Restore normal value
      options('cli.num_colors' = NULL)
      
      output$code_result_server <- renderText(paste(captured_output, collapse = "\n"))
      
      output$datetime_code_execution <- renderText(format_datetime(Sys.time(), language))
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer r$..trigger_code"))
    })
    
    # --- --- --- -- ---
    ## Save updates ----
    # --- --- --- -- ---
    
    observeEvent(input$ace_edit_code_ui_save, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$ace_edit_code_ui_save"))
      r[[paste0(id, "_save_code")]] <- Sys.time()
    })
    observeEvent(input$ace_edit_code_server_save, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$ace_edit_code_server_save"))
      r[[paste0(id, "_save_code")]] <- Sys.time()
    })
    observeEvent(input$ace_edit_code_translations_save, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$ace_edit_code_translations_save"))
      r[[paste0(id, "_save_code")]] <- Sys.time()
    })
    observeEvent(input$save_code, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$save_code"))
      r[[paste0(id, "_save_code")]] <- Sys.time()
    })
    
    observeEvent(r[[paste0(id, "_save_code")]], {
      
      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer r$..save_code"))
      
      if (length(input$code_selected_plugin) > 1) link_id <- input$code_selected_plugin$key
      else link_id <- input$code_selected_plugin
      
      req(!is.null(link_id))
      
      # Update code
      
      for (name in c("ui", "server", "translations")){
        
        code_id <- r$code %>% dplyr::filter(category == paste0("plugin_", name) & link_id == !!link_id) %>% dplyr::pull(id)
        code <- stringr::str_replace_all(input[[paste0("ace_edit_code_", name)]], "'", "''")
        
        sql <- glue::glue_sql("UPDATE code SET code = {code} WHERE id = {code_id}", .con = r$db)
        query <- DBI::dbSendStatement(r$db, sql)
        DBI::dbClearResult(query)
        r$code <- r$code %>% dplyr::mutate(code = dplyr::case_when(id == code_id ~ !!code, TRUE ~ code))
      }
      
      # Update datetime in plugins table
      
      new_update_datetime <- as.character(Sys.time())
      sql <- glue::glue_sql("UPDATE plugins SET update_datetime = {new_update_datetime} WHERE id = {link_id}", .con = r$db)
      query <- DBI::dbSendStatement(r$db, sql)
      DBI::dbClearResult(query)
      r$plugins <- r$plugins %>% dplyr::mutate(update_datetime = dplyr::case_when(id == link_id ~ new_update_datetime, TRUE ~ update_datetime))
      
      # Update datatable
      r[[paste0(prefix, "_plugins_temp")]] <- r$plugins %>% dplyr::filter(tab_type_id == !!tab_type_id) %>%
        dplyr::mutate_at(c("creation_datetime", "update_datetime"), format_datetime, language = "en", sec = FALSE) %>%
        dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
      
      # Notify user
      show_message_bar(output,  "modif_saved", "success", i18n = i18n, ns = ns)
      
      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer r$..save_code"))
    })
    
    # --- --- --- --- - -
    # Import plugins ----
    # --- --- --- --- - -

    observeEvent(input$import_plugins_browse, {
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$import_plugins_browse"))
      shinyjs::click("import_plugins_upload")
    })

    output$import_plugins_status <- renderUI({
      if (debug) print(paste0(Sys.time(), " - mod_plugins - output$import_plugins_status"))

      tagList(div(
        span(i18n$t("loaded_file"), " : ", style = "padding-top:5px;"),
        span(input$import_plugins_upload$name, style = "font-weight:bold; color:#0078D4;"), style = "padding-top:5px;"))
    })

    observeEvent(input$import_plugins_button, {

      if (perf_monitoring) monitor_perf(r = r, action = "start")
      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$import_plugins_button"))

      req(input$import_plugins_upload)

      tryCatch({

        # Extract ZIP file

        temp_dir <- paste0(r$app_folder, "/temp_files/", Sys.time() %>% stringr::str_replace_all(":| |-", ""), paste0(sample(c(0:9, letters[1:6]), 24, TRUE), collapse = ''))
        zip::unzip(input$import_plugins_upload$datapath, exdir = temp_dir)

        # Read XML file

        plugins <-
          xml2::read_xml(paste0(temp_dir, "/plugins/plugins.xml")) %>%
          XML::xmlParse() %>%
          XML::xmlToDataFrame(nodes = XML::getNodeSet(., "//plugin")) %>%
          tibble::as_tibble() %>%
          dplyr::left_join(
            r$plugins %>%
              dplyr::filter(tab_type_id == !!tab_type_id) %>%
              dplyr::inner_join(
                r$options %>% dplyr::filter(category == "plugin", name == "unique_id") %>% dplyr::select(id = link_id, unique_id = value),
                by = "id"
              ) %>%
              dplyr::select(id, unique_id),
            by = "unique_id"
          ) 
        
        prefixes <- c("description", "name", "category")
        new_cols <- outer(prefixes, r$languages$code, paste, sep = "_") %>% as.vector()
        for(col in new_cols) if(!col %in% colnames(plugins)) plugins <- plugins %>% dplyr::mutate(!!col := character())
        
        plugins <- plugins %>% dplyr::mutate(name = get(paste0("name_", language))) %>% dplyr::relocate(id)

        if (!input$replace_already_existing_plugins) plugins <- plugins %>% dplyr::filter(is.na(id))

        # Select plugin type
        plugins <- plugins %>% dplyr::filter(type == tab_type_id)

        # Loop over each plugin

        if (nrow(plugins) > 0){

          for (i in 1:nrow(plugins)){

            plugin <- plugins[i, ]

            # Delete old rows

            if (!is.na(plugin$id)){

              sql <- glue::glue_sql("DELETE FROM plugins WHERE id = {plugin$id}", .con = r$db)
              query <- DBI::dbSendStatement(r$db, sql)
              DBI::dbClearResult(query)
              r$plugins <- r$plugins %>% dplyr::filter(id != plugin$id)

              sql <- glue::glue_sql("DELETE FROM options WHERE category = 'plugin' AND link_id = {plugin$id}", .con = r$db)
              query <- DBI::dbSendStatement(r$db, sql)
              DBI::dbClearResult(query)
              r$options <- r$options %>% dplyr::filter(link_id != plugin$id | (link_id == plugin$id & category != "plugin"))

              sql <- glue::glue_sql("DELETE FROM code WHERE category IN ('plugin_ui', 'plugin_server', 'plugin_translations') AND link_id = {plugin$id}", .con = r$db)
              query <- DBI::dbSendStatement(r$db, sql)
              DBI::dbClearResult(query)
              r$code <- r$code %>% dplyr::filter(link_id != plugin$id | (link_id == plugin$id & category %not_in% c("plugin_ui", "plugin_server", "plugin_translations")))
            }

            for (name in paste0("description_", r$languages$code)) plugin[[name]] <- plugin[[name]] %>% stringr::str_replace_all("'", "''")

            # Plugins table

            new_row <- get_last_row(r$db, "plugins") + 1

            new_data <- tibble::tribble(
              ~id, ~name, ~tab_type_id, ~creation_datetime, ~update_datetime, ~deleted,
              new_row, as.character(plugin[[paste0("name_", language)]]), as.integer(tab_type_id), plugin$creation_datetime, plugin$update_datetime, FALSE)

            DBI::dbAppendTable(r$db, "plugins", new_data)
            r$plugins <- r$plugins %>% dplyr::bind_rows(new_data)
            add_log_entry(r = r, category = paste0("plugins - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_data))

            # Options table

            last_row_options <- get_last_row(r$db, "options")
            
            new_options <- tibble::tribble(
              ~name, ~value, ~value_num,
              "users_allowed_read_group", "everybody", 1,
              "user_allowed_read", "", r$user_id,
              "version", plugin$version, NA_integer_,
              "unique_id", plugin$unique_id, NA_integer_,
              "author", plugin$author, NA_integer_,
              "image", plugin$image, NA_integer_
            ) %>%
              dplyr::bind_rows(
                r$languages %>%
                  tidyr::crossing(col_prefix = c("description", "category", "name")) %>%
                  dplyr::rowwise() %>%
                  dplyr::mutate(
                    name = paste0(col_prefix, "_", code),
                    value = plugin[[paste0(col_prefix, "_", code)]],
                    value_num = NA_integer_
                  ) %>%
                  dplyr::select(-code, -language, -col_prefix)
              ) %>%
              dplyr::mutate(id = last_row_options + dplyr::row_number(), category = "plugin", link_id = new_row, .before = "name") %>%
              dplyr::mutate(creator_id = r$user_id, datetime = as.character(Sys.time()), deleted = FALSE)

            DBI::dbAppendTable(r$db, "options", new_options)
            r$options <- r$options %>% dplyr::bind_rows(new_options)
            add_log_entry(r = r, category = paste0("code", " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_options))

            # Code table

            plugin_ui_code <- paste0(temp_dir, "/plugins/", prefix, "/", plugin$unique_id, "/ui.R") %>% readLines(warn = FALSE) %>% paste(collapse = "\n") %>% stringr::str_replace_all("'", "''")
            plugin_server_code <- paste0(temp_dir, "/plugins/", prefix, "/", plugin$unique_id, "/server.R") %>% readLines(warn = FALSE) %>% paste(collapse = "\n") %>% stringr::str_replace_all("'", "''")
            plugin_translations_code <- paste0(temp_dir, "/plugins/", prefix, "/", plugin$unique_id, "/translations.csv") %>% readLines(warn = FALSE) %>% paste(collapse = "\n") %>% stringr::str_replace_all("'", "''")

            last_row_code <- get_last_row(r$db, "code")
            
            new_code <- tibble::tibble(
              id = last_row_code + 1:3,
              category = c("plugin_ui", "plugin_server", "plugin_translations"),
              link_id = new_row, code = c(plugin_ui_code, plugin_server_code, plugin_translations_code), 
              creator_id = r$user_id, datetime = as.character(Sys.time()), deleted = FALSE
            )

            DBI::dbAppendTable(r$db, "code", new_code)
            r$code <- r$code %>% dplyr::bind_rows(new_code)
            add_log_entry(r = r, category = paste0("code", " - ", i18n$t("insert_new_data")), name = i18n$t("sql_query"), value = toString(new_code))

            # Copy files
            # Create folder if doesn't exist
            plugin_dir <- paste0(r$app_folder, "/plugins/", prefix, "/", plugin$unique_id)
            if (!dir.exists(plugin_dir)) dir.create(plugin_dir, recursive = TRUE)

            list_of_files <- list.files(paste0(temp_dir, "/plugins/", prefix, "/", plugin$unique_id))

            # Copy files to temp dir
            file.copy(
              paste0(paste0(temp_dir, "/plugins/", prefix, "/", plugin$unique_id), "/", list_of_files),
              paste0(plugin_dir, "/", list_of_files),
              overwrite = TRUE
            )

            r$show_plugin_details <- Sys.time()

            # Reload datatable
            r[[paste0(prefix, "_plugins_temp")]] <- r$plugins %>% dplyr::filter(tab_type_id == !!tab_type_id) %>%
              dplyr::mutate_at(c("creation_datetime", "update_datetime"), format_datetime, language = "en", sec = FALSE) %>%
              dplyr::mutate(modified = FALSE) %>% dplyr::arrange(name)
          }
        }

        # Show imported plugins

        col_names <- c(i18n$t("name"), i18n$t("version"), i18n$t("author_s"), i18n$t("created_on"), i18n$t("updated_on"))
        centered_cols <- c("author", "version", "creation_datetime", "update_datetime")
        column_widths <- c("author" = "100px", "version" = "80px", "creation_datetime" = "130px", "update_datetime" = "130px")

        shinyjs::show("imported_plugins_div")

        data <- plugins %>% dplyr::select(name, version, author, creation_datetime, update_datetime) %>%
          dplyr::mutate_at(c("creation_datetime", "update_datetime"), format_datetime, language = language, sec = FALSE)
        
        render_datatable(output = output, ns = ns, i18n = i18n, data = data,
          output_name = "imported_plugins", col_names = col_names, centered_cols = centered_cols, column_widths = column_widths,
          filter = FALSE, datatable_dom = "")

        show_message_bar(output,  "success_importing_plugin", "success", i18n = i18n, time = 15000, ns = ns)
      },
        error = function(e) report_bug(r = r, output = output, error_message = "error_importing_plugin",
          error_name = paste0(id, " - import plugins"), category = "Error", error_report = toString(e), i18n = i18n, ns = ns),
        warning = function(w) report_bug(r = r, output = output, error_message = "error_importing_plugin",
          error_name = paste0(id, " - import plugins"), category = "Error", error_report = w, i18n = i18n, ns = ns))

      if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - observer input$import_plugins_button"))
    })
    
    # --- --- --- --- - -
    # Export plugins ----
    # --- --- --- --- - -
    
    # When add button is clicked
    observeEvent(input$add_item, {

      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$add_item"))

      # Get ID of selected plugin
      link_id <- as.integer(substr(input$add_item, nchar("add_item_") + 1, nchar(input$add_item)))

      # If this plugin is not already selected, add it to the selected items dropdown

      value <- integer(1)
      if (nrow(r[[paste0(prefix, "_export_plugins_selected")]]) > 0) value <- r[[paste0(prefix, "_export_plugins_selected")]] %>% dplyr::pull(id)

      if (link_id %not_in% value){

        r[[paste0(prefix, "_export_plugins_selected")]] <- r[[paste0(prefix, "_export_plugins_temp")]] %>% dplyr::filter(id == link_id) %>%
          dplyr::bind_rows(r[[paste0(prefix, "_export_plugins_selected")]])

        # Update dropdown of selected items
        options <- convert_tibble_to_list(r[[paste0(prefix, "_export_plugins_selected")]], key_col = "id", text_col = "name", i18n = i18n)
        value <- r[[paste0(prefix, "_export_plugins_selected")]] %>% dplyr::pull(id)
        shiny.fluent::updateDropdown.shinyInput(session, "plugins_to_export",
          options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
      }

    })

    # When dropdown is updated
    observeEvent(input$plugins_to_export_trigger, {

      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$plugins_to_export"))

      r[[paste0(prefix, "_export_plugins_selected")]] <- r[[paste0(prefix, "_export_plugins_selected")]] %>%
        dplyr::filter(id %in% input$plugins_to_export)

      options <- convert_tibble_to_list(r[[paste0(prefix, "_export_plugins_selected")]], key_col = "id", text_col = "name", i18n = i18n)
      value <- r[[paste0(prefix, "_export_plugins_selected")]] %>% dplyr::pull(id)
      shiny.fluent::updateDropdown.shinyInput(session, "plugins_to_export",
        options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || ")
    })

    # Export plugins
    observeEvent(input$export_selected_plugins, {

      if (debug) print(paste0(Sys.time(), " - mod_plugins - observer input$export_plugins"))

      req(nrow(r[[paste0(prefix, "_export_plugins_selected")]]) > 0)

      shinyjs::click("export_plugins_download")
    })

    output$export_plugins_download <- downloadHandler(

      filename = function() paste0("linkr_export_plugins_",
        Sys.time() %>% stringr::str_replace_all(" ", "_") %>% stringr::str_replace_all(":", "_") %>% as.character(), ".zip"),

      content = function(file){

        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_plugins - output$export_plugins_download"))

        owd <- setwd(tempdir())
        on.exit(setwd(owd))

        temp_dir <- paste0(r$app_folder, "/temp_files/", Sys.time() %>% stringr::str_replace_all(":| |-", ""), paste0(sample(c(0:9, letters[1:6]), 24, TRUE), collapse = ''))
        dir.create(paste0(temp_dir, "/plugins/", prefix), recursive = TRUE)

        for (plugin_id in r[[paste0(prefix, "_export_plugins_selected")]] %>% dplyr::pull(id)){

          plugin <- r$plugins %>% dplyr::filter(id == plugin_id)
          options <- r$options %>% dplyr::filter(category == "plugin", link_id == plugin_id)
          code <- r$code %>% dplyr::filter(link_id == plugin_id, category %in% c("plugin_ui", "plugin_server", "plugin_translations"))

          # Create folder if doesn't exist
          plugin_dir <- paste0(r$app_folder, "/plugins/", prefix, "/", options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
          if (!dir.exists(plugin_dir)) dir.create(plugin_dir, recursive = TRUE)

          # Create ui.R & server.R
          sapply(c("ui", "server"), function(name) writeLines(code %>% dplyr::filter(category == paste0("plugin_", name)) %>%
              dplyr::pull(code) %>% stringr::str_replace_all("''", "'"), paste0(plugin_dir, "/", name, ".R")))
          writeLines(code %>% dplyr::filter(category == "plugin_translations") %>% dplyr::pull(code) %>% stringr::str_replace_all("''", "'"), paste0(plugin_dir, "/translations.csv"))

          # Create XML file
          xml <- XML::newXMLDoc()
          plugins_node <- XML::newXMLNode("plugins", doc = xml)
          plugin_node <- XML::newXMLNode("plugin", parent = plugins_node)
          XML::newXMLNode("app_version", r$app_version, parent = plugin_node)
          XML::newXMLNode("type", tab_type_id, parent = plugin_node)
          for(name in c("unique_id", "version", "author", "image", paste0("name_", r$languages$code), paste0("category_", r$languages$code))){
            XML::newXMLNode(name, options %>% dplyr::filter(name == !!name) %>% dplyr::pull(value), parent = plugin_node)
          }
          for(name in c(paste0("description_", r$languages$code))) XML::newXMLNode(name, options %>% dplyr::filter(name == !!name) %>%
              dplyr::pull(value) %>% stringr::str_replace_all("''", "'"), parent = plugin_node)
          for (name in c("creation_datetime", "update_datetime")) XML::newXMLNode(name, plugin %>% dplyr::pull(get(!!name)), parent = plugin_node)

          list_of_files <- list.files(plugin_dir)

          # Add images filenames in the XML
          images <- list_of_files[grepl("\\.png$|\\.jpg|\\.jpeg|\\.svg", tolower(list_of_files))]
          images_node <- XML::newXMLNode("images", paste(images, collapse = ";;;"), parent = plugin_node)

          # Create XML file
          XML::saveXML(xml, file = paste0(plugin_dir, "/plugin.xml"))

          # Copy files to temp dir
          temp_dir_copy <- paste0(temp_dir, "/plugins/", prefix, "/", options %>% dplyr::filter(name == "unique_id") %>% dplyr::pull(value))
          if (!dir.exists(temp_dir_copy)) dir.create(temp_dir_copy, recursive = TRUE)
          file.copy(
            paste0(plugin_dir, "/", list_of_files),
            paste0(temp_dir_copy, "/", list_of_files),
            overwrite = TRUE
          )
        }

        # Create XML file with all exported plugins

        plugins_dir <- paste0(temp_dir, "/plugins")
        
        plugins_tibble <- tibble::tibble(app_version = character(), type = character(), unique_id = character(), version = character(), author = character(), image = character())
        prefixes <- c("description", "name", "category")
        new_cols <- outer(prefixes, r$languages$code, paste, sep = "_") %>% as.vector()
        for(col in new_cols) if(!col %in% colnames(plugins_tibble)) plugins_tibble <- plugins_tibble %>% dplyr::mutate(!!col := character())
        plugins_tibble <- plugins_tibble %>% dplyr::mutate(creation_datetime = character(), update_datetime = character())
        
        for (category in c("patient_lvl", "aggregated")){

          dirs <- list.dirs(paste0(plugins_dir, "/", category), full.names = TRUE)

          for (dir in dirs){
            if (dir != paste0(plugins_dir, "/", category)){

              xml_file <- xml2::read_xml(paste0(dir, "/plugin.xml"))
              image_nodes <- xml2::xml_find_all(xml_file, "//images/image")
              images <- character(0)
              if (length(image_nodes) > 0) images <- xml2::xml_text(image_nodes)

              df <- xml_file %>%
                XML::xmlParse() %>%
                XML::xmlToDataFrame(nodes = XML::getNodeSet(., "//plugin")) %>%
                tibble::as_tibble()
              df$images <- paste(images, collapse = ";;;")

              plugins_tibble <- plugins_tibble %>% dplyr::bind_rows(df)
            }
          }
        }

        # r$plugins_tibble <- plugins_tibble

        plugins_xml <- XML::newXMLDoc()
        plugins_node <- XML::newXMLNode("plugins", doc = plugins_xml)

        plugins_nodes <- apply(plugins_tibble, 1, function(x) {
          plugin_node <- XML::newXMLNode("plugin")
          XML::addChildren(plugin_node, lapply(names(x), function(y) XML::newXMLNode(y, x[y])))
        })

        XML::xmlParent(plugins_nodes) <- plugins_node

        XML::saveXML(plugins_xml, file = paste0(plugins_dir, "/plugins.xml"))

        # Create a ZIP

        zip::zipr(file, paste0(temp_dir, "/plugins"))

        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_plugins - output$export_plugins_download"))
      }
    )
    
  })
}
