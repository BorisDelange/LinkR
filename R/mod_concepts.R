#' @noRd 
mod_concepts_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  
  cards <- c("vocabularies_concepts_card", "vocabularies_mapping_card")
  
  forbidden_cards <- tagList()
  sapply(cards, function(card){
    forbidden_cards <<- tagList(forbidden_cards, forbidden_card(ns = ns, name = card, i18n = i18n))
  })
  
  div(
    class = "main",
    render_settings_default_elements(ns = ns),
    shiny.fluent::reactOutput(ns("help_panel")),
    shiny.fluent::reactOutput(ns("help_modal")),
    shiny.fluent::reactOutput(ns("dataset_all_concepts_reload_cache")),
    shiny.fluent::Breadcrumb(items = list(
      list(key = "vocabularies_main", text = i18n$t("vocabularies"))
    ), maxDisplayedItems = 3),
    
    # --- --- -- -- --
    # Pivot items ----
    # --- --- -- -- --
    
    # shinyjs::hidden(
    div(id = ns("menu"),
      shiny.fluent::Pivot(
        onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-current_tab', item.props.id)")),
        shiny.fluent::PivotItem(id = "vocabularies_concepts_card", itemKey = "vocabularies_concepts_card", headerText = i18n$t("concepts")),
        shiny.fluent::PivotItem(id = "vocabularies_mapping_card", itemKey = "vocabularies_mapping_card", headerText = i18n$t("concepts_mapping"))
      )
    ),
    # ),
    
    # div(
    #   id = ns("choose_a_dataset_card"),
    #   make_card("", div(shiny.fluent::MessageBar(i18n$t("choose_a_damatart_left_side"), messageBarType = 5), style = "margin-top:10px;"))
    # ),
    forbidden_cards,
    
    # --- --- --- --- --
    # Concepts card ----
    # --- --- --- --- --
    
    shinyjs::hidden(
      div(
        id = ns("vocabularies_concepts_card"),
        make_card(i18n$t("concepts"),
          div(
            shinyjs::hidden(
              div(
                id = ns("vocabularies_concepts_div"),
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                  make_combobox(i18n = i18n, ns = ns, label = "vocabulary", id = "vocabulary", width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
                  div(style = "width:20px;"),
                  make_dropdown(i18n = i18n, ns = ns, label = "columns", id = "vocabulary_table_cols", width = "300px", multiSelect = TRUE,
                    options = list(
                      list(key = 1, text = i18n$t("vocabulary_id_1")),
                      list(key = 2, text = i18n$t("concept_id_1")),
                      list(key = 3, text = i18n$t("concept_name_1")),
                      list(key = 4, text = i18n$t("concept_display_name_1")),
                      list(key = 5, text = i18n$t("relationship_id")),
                      list(key = 6, text = i18n$t("vocabulary_id_2")),
                      list(key = 7, text = i18n$t("concept_id_2")),
                      list(key = 8, text = i18n$t("concept_name_2")),
                      list(key = 9, text = i18n$t("domain_id")),
                      list(key = 10, text = i18n$t("concept_class_id")),
                      list(key = 11, text = i18n$t("standard_concept")),
                      list(key = 12, text = i18n$t("concept_code")),
                      list(key = 13, text = i18n$t("valid_start_date")),
                      list(key = 14, text = i18n$t("valid_end_date")),
                      list(key = 15, text = i18n$t("invalid_reason")),
                      list(key = 16, text = i18n$t("num_patients")),
                      list(key = 17, text = i18n$t("num_rows"))
                    ),
                    value = c(2, 3, 4, 9, 16, 17)
                  ),
                  div(style = "width:10px;"),
                  div(shiny.fluent::Toggle.shinyInput(ns("vocabulary_show_mapped_concepts"), value = FALSE), style = "margin-top:45px;"),
                  div(i18n$t("show_mapped_concepts"), style = "font-weight:bold; margin-top:45px; margin-right:30px;")
                ),
                div(
                  id = ns("vocabulary_concepts_table_view_div"),
                  DT::DTOutput(ns("vocabulary_concepts")),
                  div(id = ns("br_div"), br()),
                  shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                    shiny.fluent::DefaultButton.shinyInput(ns("reload_vocabulary_concepts_cache"), i18n$t("reload_cache")),
                    shiny.fluent::PrimaryButton.shinyInput(ns("save_vocabulary_concepts"), i18n$t("save"))
                  ), br(),
                  div(
                    id = ns("vocabulary_datatable_selected_item_div"),
                    div(uiOutput(ns("vocabulary_datatable_selected_item")), style = "display:relative; float:left; width:50%;"),
                    div(
                      shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                        div(shiny.fluent::Dropdown.shinyInput(ns("vocabulary_datatable_selected_item_plot_variable")), style = "width:50%; margin-left:42px;")
                      ),
                      uiOutput(ns("vocabulary_datatable_selected_item_error_message")),
                      plotly::plotlyOutput(ns("vocabulary_datatable_selected_item_plot"), height = "280px"), 
                      style = "display:relative; float:right; width:50%;"
                    )
                  )
                )
              )
            ),
            div(
              id = ns("choose_a_dataset_card"),
              div(shiny.fluent::MessageBar(i18n$t("choose_a_damatart_left_side"), messageBarType = 5), style = "margin-top:10px;")
            )
          )
        )
      )
    ),
    
    # --- --- --- -- --
    # Mapping card ----
    # --- --- --- -- --
    
    shinyjs::hidden(
      div(
        id = ns("vocabularies_mapping_card"),
        make_card(i18n$t("concepts_mapping"),
          div(
            shiny.fluent::reactOutput(ns("mappings_delete_confirm")),
            div(
              shiny.fluent::Pivot(
                id = ns("vocabularies_mapping_pivot"),
                onLinkClick = htmlwidgets::JS(paste0("item => Shiny.setInputValue('", id, "-mapping_current_tab', item.props.id)")),
                shiny.fluent::PivotItem(id = "vocabularies_mapping_add", itemKey = "vocabularies_mapping_add", headerText = i18n$t("add")),
                shiny.fluent::PivotItem(id = "vocabularies_mapping_management", itemKey = "vocabularies_mapping_management", headerText = i18n$t("evaluate_and_edit"))
              )
            ),
            div(
              id = ns("vocabularies_mapping_add_div"),
              div(
                div(
                  shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                    make_combobox(i18n = i18n, ns = ns, label = "vocabulary_1", id = "vocabulary_mapping_1", width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
                    div(style = "width:10px;"),
                    div(shiny.fluent::Toggle.shinyInput(ns("vocabulary_show_only_not_mapped_concepts"), value = FALSE), style = "margin-top:45px;"),
                    div(i18n$t("show_only_not_mapped_concepts"), style = "font-weight:bold; margin-top:45px; margin-right:30px;")
                  ),
                  DT::DTOutput(ns("vocabulary_mapping_1_dt"))
                ),
                div(
                  shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                    make_combobox(i18n = i18n, ns = ns, label = "vocabulary_2", id = "vocabulary_mapping_2", width = "300px", allowFreeform = FALSE, multiSelect = FALSE),
                    div(style = "width:10px;"),
                    div(shiny.fluent::Toggle.shinyInput(ns("vocabulary_show_only_used_concepts"), value = TRUE), style = "margin-top:45px;"),
                    div(i18n$t("show_only_used_concepts"), style = "font-weight:bold; margin-top:45px; margin-right:30px;")
                  ),
                  DT::DTOutput(ns("vocabulary_mapping_2_dt"))
                ),
                style = "width:100%; display:grid; grid-template-columns:1fr 1fr; grid-gap:20px;"
              ), br(),
              shinyjs::hidden(
                div(
                  id = ns("vocabulary_mapping_selected_concepts_div"),
                  br(),
                  div(
                    div(uiOutput(ns("vocabulary_mapping_selected_concept_1")), style = "border:dashed 1px; padding:10px;"),
                    div(
                      shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                        div(shiny.fluent::Toggle.shinyInput(ns("show_all_relationships"), value = FALSE), style = "margin-top:30px; margin-bottom:5px;"),
                        div(i18n$t("show_all_relationships"), style = "font-weight:bold; margin-top:30px; margin-bottom:5px;")
                      ),
                      make_combobox(i18n = i18n, ns = ns, label = "concept_1_is_to_concept_2", id = "relationship_id", width = "300px", multiSelect = FALSE, allowFreeform = FALSE,
                        options = list(
                          list(key = "Maps to", text = i18n$t("maps_to")),
                          list(key = "Mapped from", text = i18n$t("mapped_from")),
                          list(key = "Is a", text = i18n$t("is_a")),
                          list(key = "Subsumes", text = i18n$t("subsumes"))
                        ),
                        value = "Maps to")
                    ),
                    div(uiOutput(ns("vocabulary_mapping_selected_concept_2")), style = "border:dashed 1px; padding:10px;"),
                    style = "width:100%; display:grid; grid-template-columns:2fr 1fr 2fr; grid-gap:20px;"
                  ),
                  make_textfield(label = "comment", id = "mapping_comment", i18n = i18n, ns = ns), br(),
                  shiny.fluent::PrimaryButton.shinyInput(ns("add_mapping"), i18n$t("add")),
                  br(),
                  DT::DTOutput(ns("vocabulary_added_mappings"))
                )
              )
            ),
            div(
              id = ns("vocabularies_mapping_management_div"),
              shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                make_dropdown(i18n = i18n, ns = ns, label = "columns", id = "vocabulary_mapping_eval_cols", width = "300px", multiSelect = TRUE,
                  options = list(
                    list(key = 1, text = i18n$t("vocabulary_id_1")),
                    list(key = 2, text = i18n$t("concept_id_1")),
                    list(key = 3, text = i18n$t("concept_name_1")),
                    list(key = 4, text = i18n$t("relationship_id")),
                    list(key = 5, text = i18n$t("vocabulary_id_2")),
                    list(key = 6, text = i18n$t("concept_id_2")),
                    list(key = 7, text = i18n$t("concept_name_2")),
                    list(key = 8, text = i18n$t("comment")),
                    list(key = 9, text = i18n$t("creator")),
                    list(key = 10, text = i18n$t("datetime")),
                    list(key = 11, text = i18n$t("positive_evals")),
                    list(key = 12, text = i18n$t("negative_evals")),
                    list(key = 13, text = i18n$t("action"))
                  ),
                  value = c(2, 4, 6, 10, 11, 12, 13)
                ), div(style = "width:10px;"),
                div(shiny.fluent::Toggle.shinyInput(ns("vocabulary_show_only_not_evaluated_concepts"), value = FALSE), style = "margin-top:45px;"),
                div(i18n$t("vocabulary_show_only_not_evaluated_concepts"), style = "font-weight:bold; margin-top:45px; margin-right:30px;"),
                div(shiny.fluent::Toggle.shinyInput(ns("vocabulary_show_mapping_details"), value = TRUE), style = "margin-top:45px;"),
                div(i18n$t("vocabulary_show_mapping_details"), style = "font-weight:bold; margin-top:45px; margin-right:30px;")
              ),
              div(DT::DTOutput(ns("vocabulary_evaluate_mappings")), style = "z-index:2"),
              div(
                shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
                  shiny.fluent::PrimaryButton.shinyInput(ns("save_mappings_evaluation"), i18n$t("save")),
                  shiny.fluent::DefaultButton.shinyInput(ns("mapping_delete_selection"), i18n$t("delete_selection"))
                ),
                style = "position:relative; z-index:1; margin-top:-30px; width:500px;"), br(),
                shinyjs::hidden(
                  div(
                    id = ns("vocabulary_mapping_details_div"),
                    div(uiOutput(ns("vocabulary_mapping_details_left")), style = "border:dashed 1px; padding:10px; margin:10px; flex:1;"),
                    div(uiOutput(ns("vocabulary_mapping_details_center")), style = "border:dashed 1px; padding:10px; margin:10px; flex:1;"),
                    div(uiOutput(ns("vocabulary_mapping_details_right")), style = "border:dashed 1px; padding:10px; margin:10px; flex:1;"),
                    style = "display:flex;"
                  )
                )
              # )
            )
            # )
          )
        )
      )
    ), br()
  )
}

#' @noRd 
mod_concepts_server <- function(id, r, d, m, language, i18n, debug){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (debug) cat(paste0("\n", now(), " - mod_vocabularies - start"))
    
    # # --- --- --- --- --- ---
    # # Show or hide cards ----
    # # --- --- --- --- --- ---
    # 
    # cards <- c("vocabularies_concepts_card", "vocabularies_mapping_card")
    # show_or_hide_cards(r = r, input = input, session = session, id = id, cards = cards)
    # 
    # # Show first card
    # if ("vocabularies_concepts_card" %in% r$user_accesses) shinyjs::show("vocabularies_concepts_card")
    # else shinyjs::show("vocabularies_concepts_card_forbidden")
    # 
    # observeEvent(input$current_tab, {
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$current_tab"))
    #   
    #   # Load evaluate and edit page, to load DT (doesn't update with other DT if not already loaded once)
    #   
    #   if (input$current_tab == "vocabularies_mapping_card" & length(r$vocabularies_page_loaded) == 0){
    #     shinyjs::show("vocabularies_mapping_management_div")
    #     shinyjs::delay(500, shinyjs::hide("vocabularies_mapping_management_div"))
    #     r$vocabularies_page_loaded <- TRUE
    #   }
    # })
    # 
    # observeEvent(r$user_accesses, {
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer r$user_accesses"))
    #   
    #   if ("vocabularies_evaluate_mapping" %not_in% r$user_accesses) shinyjs::hide("save_mappings_evaluation")
    #   if ("vocabularies_delete_mapping" %not_in% r$user_accesses) shinyjs::hide("mapping_delete_selection")
    # })
    # 
    # observeEvent(input$mapping_current_tab, {
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$mapping_current_tab"))
    #   
    #   if (input$mapping_current_tab == "vocabularies_mapping_add"){
    #     shinyjs::show("vocabularies_mapping_add_div")
    #     shinyjs::hide("vocabularies_mapping_management_div")
    #   }
    #   else if (input$mapping_current_tab == "vocabularies_mapping_management"){
    #     shinyjs::hide("vocabularies_mapping_add_div")
    #     shinyjs::show("vocabularies_mapping_management_div")
    #   }
    # })
    
    # --- --- --- --- --- -- -
    # Vocabulary concepts ----
    # --- --- --- --- --- -- -
    
    # # Show / hide vocabulary_concepts_table_view_div
    # observeEvent(input$vocabulary_concepts_pivot, {
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer r$vocabulary_concepts_pivot"))
    #   if (input$vocabulary_concepts_pivot == "vocabulary_concepts_table_view") shinyjs::show("vocabulary_concepts_table_view_div")
    #   else shinyjs::hide("vocabulary_concepts_table_view_div")
    # })
    # 
    # observeEvent(r$vocabulary, {
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer r$vocabulary 1"))
    #   # r$reload_vocabulary_dropdown <- now()
    #   
    #   vocabulary_options <- r$vocabulary %>% dplyr::arrange(vocabulary_id) %>% convert_tibble_to_list(key_col = "vocabulary_id", text_col = "vocabulary_id")
    #   for (var in c("vocabulary_mapping_1", "vocabulary_mapping_2")) shiny.fluent::updateComboBox.shinyInput(session, var, options = vocabulary_options, value = NULL)
    # })
    # 
    # observeEvent(r$selected_dataset, {
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "start")
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer r$selected_dataset 1"))
    #   
    #   # Show first card & hide "choose a dataset" card
    #   shinyjs::hide("choose_a_dataset_card")
    #   if ("vocabularies_concepts_card" %in% r$user_accesses) shinyjs::show("vocabularies_concepts_div")
    #   
    #   r$load_dataset_all_concepts <- now()
    #   
    #   # Reset UI of selected item
    #   output$vocabulary_datatable_selected_item <- renderUI("")
    #   output$vocabulary_datatable_selected_item_plot <- plotly::renderPlotly(plotly::plotly_empty(data = tibble::tibble(x = 1), type = "scatter", mode = "lines") %>% plotly::config(displayModeBar = FALSE))
    #   shinyjs::hide("vocabulary_datatable_selected_item_div")
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$selected_dataset 2"))
    # })
    # 
    # Load all concepts

    observeEvent(r$load_dataset_all_concepts, {
      if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer r$load_dataset_all_concepts"))

      # Create dataset folder if doesn't exist
      dataset_folder <- paste0(r$app_folder, "/datasets_files/", r$selected_dataset)
      if (!dir.exists(dataset_folder)) dir.create(dataset_folder)

      # Load csv file if it exists

      dataset_all_concepts_filename <- paste0(dataset_folder, "/dataset_all_concepts.csv")

      if (file.exists(dataset_all_concepts_filename)) d$dataset_all_concepts <- vroom::vroom(dataset_all_concepts_filename, col_types = "icicccciccccccccii", progress = FALSE)

      if (!file.exists(dataset_all_concepts_filename)){

        # Load all concepts for this dataset, with rows count

        omop_version <- r$options %>% dplyr::filter(category == "dataset" & link_id == r$selected_dataset & name == "omop_version") %>% dplyr::pull(value)

        tables <- c("person", "visit_occurrence", "visit_detail", "condition_occurrence", "drug_exposure", "procedure_occurrence", "device_exposure",
          "measurement", "observation", "specimen", "drug_era", "dose_era", "condition_era", "note", "specimen")
        if (omop_version %in% c("5.3", "5.0")) tables <- c(tables, "death")

        main_cols <- c(
          "condition_occurrence" = "condition",
          "drug_exposure" = "drug",
          "procedure_occurrence" = "procedure",
          "device_exposure" = "device",
          "measurement" = "measurement",
          "observation" = "observation",
          "specimen" = "specimen",
          "drug_era" = "drug",
          "dose_era" = "drug",
          "condition_era" = "condition"
        )

        secondary_cols <- list(
          "person" = c("gender", "race", "ethnicity"),
          "condition_occurrence" = c("condition_type", "condition_status"),
          "drug_exposure" = c("drug_type", "route"),
          "procedure_occurrence" = "procedure_type",
          "device_exposure" = "device_type",
          "measurement" = c("measurement_type", "value_as", "unit"),
          "observation" = c("observation_type", "qualifier", "value_as", "unit"),
          "note" = c("note_type", "note_class", "encoding", "language"),
          "specimen" = c("specimen_type", "unit", "anatomic_site", "disease_status"),
          "dose_era" = "unit"
        )

        # Get all concept_ids for this dataset
        dataset_concept_ids <- NA_integer_
        for (table in tables){
          # concept_ids <- d[[table]] %>% dplyr::select(dplyr::contains("concept_id")) %>% unlist(use.names = FALSE) %>% as.integer() %>% unique()

          concept_ids <- d[[table]] %>%
            dplyr::select(dplyr::contains("concept_id")) %>%
            dplyr::distinct() %>%
            dplyr::collect() %>%
            unlist(use.names = FALSE) %>%
            as.character() %>%
            purrr::discard(~is.na(.) | !grepl("^[0-9]+$", .)) %>%
            as.integer() %>%
            unique()

          dataset_concept_ids <- unique(c(dataset_concept_ids, concept_ids))
        }

        sql <- glue::glue_sql(paste0(
          "SELECT * ",
          "FROM concept ",
          "WHERE concept_id IN ({dataset_concept_ids*}) ",
          # "WHERE vocabulary_id IN ({r$dataset_vocabularies %>% dplyr::pull(vocabulary_id)*}) ",
          "ORDER BY concept_id"), .con = m$db)
        dataset_all_concepts <- DBI::dbGetQuery(m$db, sql) %>% tibble::as_tibble() %>% dplyr::mutate(concept_display_name = NA_character_, .after = "concept_name")

        # Count rows

        # count_rows <- tibble::tibble(concept_id = integer(), count_persons_rows = integer(), count_concepts_rows = integer(), count_secondary_concepts_rows = integer())
        count_rows <- tibble::tibble(concept_id = integer(), count_persons_rows = numeric(), count_concepts_rows = numeric())

        # EDIT : we do not use anymore the count_secondary_concepts_rows col
        # count_concepts_rows & count_secondary_concepts_rows are now merged

        if (omop_version == "5.3"){
          secondary_cols <- rlist::list.append(secondary_cols, "visit_occurrence" = c("visit", "visit_type", "visit_source", "admitting_source", "discharge_to"))
          secondary_cols <- rlist::list.append(secondary_cols, "visit_detail" = c("visit_detail", "visit_detail_type", "visit_detail_source", "admitting_source", "discharge_to"))
        }
        else if (omop_version %in% c("5.4", "6.0")){
          secondary_cols <- rlist::list.append(secondary_cols, "visit_occurrence" = c("visit", "visit_type", "visit_source", "admitted_from", "discharge_to"))
          secondary_cols <- rlist::list.append(secondary_cols, "visit_detail" = c("visit_detail", "visit_detail_type", "visit_detail_source", "admitted_from", "discharge_to"))
        }

        if (omop_version %in% c("5.3", "5.0")) secondary_cols <- rlist::list.append(secondary_cols, "death" = c("death_type", "cause"))

        for(table in tables){
          if (d[[table]] %>% dplyr::count() %>% dplyr::pull() > 0){
            if (table %in% names(main_cols)){
              # Group by concept_id cols & source_concept_id cols (except for specimen & era tables)
              if (paste0(main_cols[[table]], "_concept_id") %in% colnames(d[[table]])){
                count_rows <-
                  count_rows %>%
                  dplyr::bind_rows(
                    d[[table]] %>%
                      dplyr::group_by_at(paste0(main_cols[[table]], "_concept_id")) %>%
                      # dplyr::summarize(count_persons_rows = dplyr::n_distinct(person_id), count_concepts_rows = dplyr::n(), count_secondary_concepts_rows = 0L) %>%
                      dplyr::summarize(count_persons_rows = as.numeric(dplyr::n_distinct(person_id)), count_concepts_rows = as.numeric(dplyr::n())) %>%
                      dplyr::ungroup() %>%
                      dplyr::rename(concept_id = paste0(main_cols[[table]], "_concept_id")) %>%
                      dplyr::collect()
                  )
                if (table %not_in% c("specimen", "drug_era", "dose_era", "condition_era")) count_rows <-
                  count_rows %>%
                  dplyr::bind_rows(
                    d[[table]] %>%
                      dplyr::group_by_at(paste0(main_cols[[table]], "_source_concept_id")) %>%
                      # dplyr::summarize(count_persons_rows = dplyr::n_distinct(person_id), count_concepts_rows = dplyr::n(), count_secondary_concepts_rows = 0L) %>%
                      dplyr::summarize(count_persons_rows = as.numeric(dplyr::n_distinct(person_id)), count_concepts_rows = as.numeric(dplyr::n())) %>%
                      dplyr::ungroup() %>%
                      dplyr::rename(concept_id = paste0(main_cols[[table]], "_source_concept_id")) %>%
                      dplyr::collect()
                  )
              }
              else report_bug(r = r, output = output, error_message = "error_calculating_num_rows_concepts_dataset",
                error_name = paste0("mod_vocabularies - observer r$merge_concepts_and_d_vars - dataset_id = ", r$selected_dataset),
                category = "Error", error_report = paste0("table = ", table, " / col = ", main_cols[[table]], "_concept_id"), i18n = i18n, ns = ns)
            }
            if (table %in% names(secondary_cols)){
              for (col in secondary_cols[[table]]){
                if (paste0(col, "_concept_id") %in% colnames(d[[table]])){
                  count_rows <-
                    count_rows %>%
                    dplyr::bind_rows(
                      d[[table]] %>%
                        dplyr::group_by_at(paste0(col, "_concept_id")) %>%
                        # dplyr::summarize(count_persons_rows = dplyr::n_distinct(person_id), count_concepts_rows = 0L, count_secondary_concepts_rows = dplyr::n()) %>%
                        dplyr::summarize(count_persons_rows = as.numeric(dplyr::n_distinct(person_id)), count_concepts_rows = as.numeric(dplyr::n())) %>%
                        dplyr::ungroup() %>%
                        dplyr::rename(concept_id = paste0(col, "_concept_id")) %>%
                        dplyr::collect()
                    )
                }
                else report_bug(r = r, output = output, error_message = "error_calculating_num_rows_concepts_dataset",
                  error_name = paste0("mod_vocabularies - observer r$merge_concepts_and_d_vars - dataset_id = ", r$selected_dataset),
                  category = "Error", error_report = paste0("table = ", table, " / col = ", col, "_concept_id"), i18n = i18n, ns = ns)
              }
            }
          }
        }

        if (nrow(count_rows) > 0){

          # Group count_rows : if a concept_id is in distinct tables, it will produce multiple rows by concept_id
          count_rows <- count_rows %>%
            dplyr::filter(!is.na(concept_id)) %>%
            dplyr::group_by(concept_id) %>%
            dplyr::summarize(count_persons_rows = as.numeric(max(count_persons_rows)), count_concepts_rows = as.numeric(sum(count_concepts_rows))) %>%
            # dplyr::summarize(count_persons_rows = max(count_persons_rows), count_concepts_rows = sum(count_concepts_rows),
              # count_secondary_concepts_rows = sum(count_secondary_concepts_rows)) %>%
            dplyr::ungroup()

          # Merge count_rows, transform count_rows cols to integer, to be sortable
          dataset_all_concepts <- dataset_all_concepts %>%
            dplyr::left_join(count_rows, by = "concept_id") %>%
            # dplyr::mutate_at(c("count_persons_rows", "count_concepts_rows", "count_secondary_concepts_rows"), as.integer) %>%
            dplyr::mutate_at(c("count_persons_rows", "count_concepts_rows"), as.numeric) %>%
            # dplyr::filter(count_concepts_rows > 0 | count_secondary_concepts_rows > 0)
            dplyr::filter(count_concepts_rows > 0)
        }

        if (nrow(count_rows) == 0) dataset_all_concepts <- dataset_all_concepts %>% dplyr::slice(0)

        dataset_all_concepts <- dataset_all_concepts %>%
          dplyr::rename(concept_id_1 = concept_id, concept_name_1 = concept_name, concept_display_name_1 = concept_display_name, vocabulary_id_1 = vocabulary_id) %>%
          dplyr::mutate(relationship_id = NA_character_, vocabulary_id_2 = NA_character_, concept_id_2 = NA_integer_, concept_name_2 = NA_character_, .after = "concept_display_name_1") %>%
          dplyr::relocate(vocabulary_id_1, .before = "concept_id_1")

        # Load d$concept & d$concept_relationship if not already loaded from mod_settings_data_management.R
        # Convert cols to char and arrange cols as done in mod_settings_data_management.R

        tables <- c("concept", "concept_relationship")

        cols_to_char <- list()
        cols_to_char$concept = "concept_id"
        cols_to_char$relationship = "relationship_concept_id"

        cols_order <- list()
        cols_order$concept <- "concept_id"
        cols_order$concept_relationship <- "concept_id_1"

        for (table in tables){
          if (length(d[[table]]) == 0){
            if (table == "concept") sql <- glue::glue_sql("SELECT * FROM concept", .con = m$db)
            else if (table == "concept_relationship") sql <- glue::glue_sql(paste0(
              "SELECT cr.* FROM concept_relationship cr WHERE cr.id NOT IN ( ",
                "WITH cr2 AS (",
                  "SELECT cru.concept_relationship_id, ",
                  "SUM(CASE WHEN cre.evaluation_id = 1 THEN 1 ELSE 0 END) AS positive_evals, ",
                  "SUM(CASE WHEN cre.evaluation_id = 2 THEN 1 ELSE 0 END) AS negative_evals ",
                  "FROM concept_relationship_user cru ",
                  "LEFT JOIN concept_relationship_evals cre ON cru.concept_relationship_id = cre.concept_relationship_id ",
                  "GROUP BY cru.concept_relationship_id ",
                  "HAVING positive_evals = 0 OR (positive_evals > 0 AND positive_evals <= negative_evals) ",
                ") ",
              "SELECT concept_relationship_id FROM cr2 ",
              ")"), .con = m$db)

            d[[table]] <- DBI::dbGetQuery(m$db, sql) %>%
              dplyr::arrange(cols_order[[table]]) %>%
              dplyr::mutate_at(cols_to_char[[table]], as.character) %>%
              dplyr::mutate(modified = FALSE)
          }
        }

        # Merge mapped concepts

        if (nrow(d$concept) > 0 & nrow(dataset_all_concepts) > 0){
          dataset_all_concepts <- dataset_all_concepts %>%
            dplyr::bind_rows(
              dataset_all_concepts %>%
                dplyr::select(vocabulary_id_2 = vocabulary_id_1, concept_id_2 = concept_id_1, concept_name_2 = concept_name_1,
                  count_persons_rows, count_concepts_rows) %>%
                dplyr::left_join(
                  d$concept_relationship %>%
                    dplyr::mutate_at(c("concept_id_1", "concept_id_2"), as.integer) %>%
                    dplyr::select(concept_id_1, concept_id_2, relationship_id),
                  by = "concept_id_2"
                ) %>%
                dplyr::filter(concept_id_1 != concept_id_2) %>%
                dplyr::left_join(
                  d$concept %>%
                    dplyr::mutate_at("concept_id", as.integer) %>%
                    dplyr::select(vocabulary_id_1 = vocabulary_id, concept_id_1 = concept_id, concept_name_1 = concept_name,
                      domain_id, concept_class_id, standard_concept, concept_code, valid_start_date, valid_end_date, invalid_reason),
                  by = "concept_id_1"
                ) %>%
                dplyr::arrange(dplyr::desc(count_concepts_rows)) %>%
                dplyr::mutate(concept_display_name_1 = NA_character_, .after = "concept_name_1")
            )
        }

        if (nrow(dataset_all_concepts) == 0){
          dataset_all_concepts <- dataset_all_concepts %>% dplyr::mutate(
            # count_persons_rows = integer(), count_concepts_rows = integer(), count_secondary_concepts_rows = integer(), add_concept_input = character())
            count_persons_rows = numeric(), count_concepts_rows = numeric(), add_concept_input = character())
        }

        else {
          # Add plus col
          dataset_all_concepts <- dataset_all_concepts %>%
            dplyr::mutate(
              add_concept_input = as.character(shiny::actionButton(NS("%ns%")("%input_prefix%_%concept_id_1%"), "", icon = icon("plus"),
                onclick = paste0("Shiny.setInputValue('%ns%-%input_prefix_2%concept_selected', this.id, {priority: 'event'})")))
            )
        }

        # Delete old rows
        sql <- glue::glue_sql("DELETE FROM concept_dataset WHERE dataset_id = {r$selected_dataset}", .con = m$db)
        query <- DBI::dbSendStatement(m$db, sql)
        DBI::dbClearResult(query)

        # Add new rows to database
        if (nrow(dataset_all_concepts) > 0) DBI::dbAppendTable(m$db, "concept_dataset", dataset_all_concepts %>%
          dplyr::transmute(id = get_last_row(m$db, "concept_dataset") + 1:dplyr::n(), concept_id = concept_id_1, dataset_id = r$selected_dataset, vocabulary_id = vocabulary_id_1,
            # count_persons_rows, count_concepts_rows, count_secondary_concepts_rows))
            count_persons_rows, count_concepts_rows, count_secondary_concepts_rows = as.numeric(0)))

        # Save data as csv
        readr::write_csv(dataset_all_concepts, dataset_all_concepts_filename, progress = FALSE)

        # Update d var
        d$dataset_all_concepts <- dataset_all_concepts
      }

      # Get user's modifications on items names & concept_display_names

      sql <- glue::glue_sql(paste0(
        "SELECT id, concept_id, concept_name, concept_display_name ",
        "FROM concept_user ",
        # "WHERE user_id = {r$user_id} AND vocabulary_id IN ({r$dataset_vocabularies %>% dplyr::pull(vocabulary_id)*})"
        "WHERE user_id = {r$user_id}"), .con = m$db)
      dataset_user_concepts <- DBI::dbGetQuery(m$db, sql) %>% tibble::as_tibble()

      # Merge tibbles
      if (nrow(dataset_user_concepts) > 0) d$dataset_all_concepts <-
        d$dataset_all_concepts %>%
        dplyr::left_join(
          dataset_user_concepts %>% dplyr::select(concept_id_1 = concept_id, new_concept_name_1 = concept_name, new_concept_display_name_1 = concept_display_name),
          by = "concept_id_1"
        ) %>%
        dplyr::mutate(
          concept_name_1 = dplyr::case_when(!is.na(new_concept_name_1) ~ new_concept_name_1, TRUE ~ concept_name_1),
          concept_display_name_1 = dplyr::case_when(!is.na(new_concept_display_name_1) ~ new_concept_display_name_1, TRUE ~ concept_display_name_1)
        ) %>%
        dplyr::left_join(
          dataset_user_concepts %>% dplyr::select(concept_id_2 = concept_id, new_concept_name_2 = concept_name),
          by = "concept_id_2"
        ) %>%
        dplyr::mutate(
          concept_name_2 = dplyr::case_when(!is.na(new_concept_name_2) ~ new_concept_name_2, TRUE ~ concept_name_2)
        ) %>%
        dplyr::select(-new_concept_name_1, -new_concept_display_name_1, -new_concept_name_2)

      if (length(r$dataset_vocabulary_concepts_datatable_proxy) > 0) DT::replaceData(r$dataset_vocabulary_concepts_datatable_proxy,
        r$dataset_vocabulary_concepts %>% dplyr::slice(0), resetPaging = FALSE, rownames = FALSE)

      r$dataset_vocabularies <-
        r$vocabulary %>%
          dplyr::filter(vocabulary_id %in% unique(c(unique(d$dataset_all_concepts$vocabulary_id_1), unique(d$dataset_all_concepts$vocabulary_id_2)))) %>%
          dplyr::arrange(vocabulary_id)

      # Join d$person, d$visit_occurrence & d$visit_detail with d$dataset_all_concepts
      r$merge_concepts_and_d_vars <- now()

      # Then load d$dataset_drug_strength
      r$load_dataset_drug_strength <- now()

      # Update vocabulary combobox
      # vocabulary_options <- r$vocabulary %>% dplyr::filter(vocabulary_id %in% unique(c(unique(d$dataset_all_concepts$vocabulary_id_1), unique(d$dataset_all_concepts$vocabulary_id_2))))
      if (nrow(r$dataset_vocabularies) > 0) shiny.fluent::updateComboBox.shinyInput(session, "vocabulary",
        options = convert_tibble_to_list(tibble::tibble(id = 0L, vocabulary_id = i18n$t("all_vocabularies")) %>% dplyr::bind_rows(r$dataset_vocabularies %>% dplyr::select(id, vocabulary_id)),
          key_col = "id", text_col = "vocabulary_id"), value = NULL)
    })
    # 
    # observeEvent(r$load_dataset_drug_strength, {
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "start")
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer r$load_dataset_drug_strength"))
    #   
    #   dataset_drug_strength_filename <- paste0(r$app_folder, "/datasets_files/", r$selected_dataset, "/dataset_drug_strength.csv")
    #   
    #   if (file.exists(dataset_drug_strength_filename)) d$dataset_drug_strength <- vroom::vroom(dataset_drug_strength_filename, col_types = "iicicncncnciccc", progress = FALSE)
    #   
    #   if (!file.exists(dataset_drug_strength_filename)){
    #     
    #     failed_load_data <- TRUE
    #     
    #     if (nrow(d$dataset_all_concepts) > 0){
    #     
    #       # Get all distincts drug_concept_id, with mappings
    #       drug_concept_ids <- 
    #         d$dataset_all_concepts %>% dplyr::filter(domain_id == "Drug") %>% dplyr::distinct(concept_id_1) %>% dplyr::select(drug_concept_id = concept_id_1) %>%
    #         dplyr::bind_rows(
    #           d$dataset_all_concepts %>% dplyr::filter(domain_id == "Drug") %>% dplyr::distinct(concept_id_2) %>% dplyr::select(drug_concept_id = concept_id_2)
    #         ) %>%
    #         dplyr::distinct(drug_concept_id)
    #       
    #       # Load d$drug_strength if not loaded yet
    #       if (length(d$drug_strength) == 0){
    #         sql <- glue::glue_sql("SELECT * FROM drug_strength", .con = m$db)
    #         d$drug_strength <- DBI::dbGetQuery(m$db, sql) %>% 
    #           tibble::as_tibble() %>%
    #           dplyr::arrange(drug_concept_id) %>%
    #           dplyr::mutate_at(c("drug_concept_id", "ingredient_concept_id", "amount_unit_concept_id", "numerator_unit_concept_id", "denominator_unit_concept_id"), as.character) %>%
    #           dplyr::mutate(modified = FALSE)
    #       }
    #       
    #       if (nrow(d$drug_strength) > 0){
    #         
    #         drug_strength <-
    #           d$drug_strength %>%
    #           dplyr::select(-modified) %>%
    #           dplyr::mutate_at(c("drug_concept_id", "ingredient_concept_id", "amount_unit_concept_id", 
    #             "numerator_unit_concept_id", "denominator_unit_concept_id"), as.integer) %>%
    #           dplyr::inner_join(
    #             drug_concept_ids, by = "drug_concept_id"
    #           )
    #         
    #         concepts_ids <- list()
    #         concepts <- list()
    #         
    #         for (col in c("drug", "ingredient", "amount_unit", "numerator_unit", "denominator_unit")){
    #           
    #           concepts_ids[[col]] <- drug_strength %>% 
    #             dplyr::distinct(!!rlang::sym(paste0(col, "_concept_id"))) %>% 
    #             dplyr::mutate_at(paste0(col, "_concept_id"), as.integer) %>% dplyr::pull()
    #           
    #           sql <- glue::glue_sql(paste0("SELECT concept_id AS {paste0(col, '_concept_id')}, ",
    #           "concept_name AS {paste0(col, '_concept_name')} FROM concept WHERE concept_id IN ({concepts_ids[[col]]*})"), .con = m$db)
    #           
    #           concepts[[col]] <- DBI::dbGetQuery(m$db, sql) %>% tibble::as_tibble()
    #           
    #           drug_strength <- drug_strength %>% 
    #             dplyr::left_join(concepts[[col]], by = paste0(col, "_concept_id")) %>%
    #             dplyr::relocate(!!paste0(col, "_concept_name"), .after = paste0(col, "_concept_id"))
    #         }
    #         
    #         failed_load_data <- FALSE
    #       }
    #     }
    #     
    #     if (failed_load_data) drug_strength <- tibble::tibble(id = integer(), drug_concept_id = integer(), drug_concept_name = character(),
    #       ingredient_concept_id = integer(), ingredient_concept_name = character(), 
    #       amount_value = numeric(), amount_unit_concept_id = integer(), amount_unit_concept_name = integer(),
    #       numerator_value = numeric(), numerator_unit_concept_id = integer(), numerator_unit_concept_name = integer(),
    #       denominator_value = numeric(), denominator_unit_concept_id = integer(), denominator_unit_concept_name = integer(),
    #       box_size = integer(), valid_start_date = character(), valid_end_date = character(), invalid_reason = character())
    #     
    #     # Save as csv
    #     readr::write_csv(drug_strength, dataset_drug_strength_filename, progress = FALSE)
    #     
    #     d$dataset_drug_strength <- drug_strength
    #   }
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$load_dataset_drug_strength"))
    # })
    # 
    # # When a vocabulary is selected, filter on it
    # 
    # observeEvent(input$vocabulary, {
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$vocabulary"))
    #   
    #   r$reload_vocabulary_datatable <- now()
    #   
    #   shinyjs::hide("br_div")
    #   sapply(c("vocabulary_datatable_selected_item_div", "save_vocabulary_concepts"), shinyjs::show)
    # })
    # 
    # # Update which cols are hidden
    # observeEvent(input$vocabulary_table_cols, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$vocabulary_table_cols"))
    #   
    #   req(length(r$dataset_vocabulary_concepts_datatable_proxy) > 0)
    #   
    #   r$dataset_vocabulary_concepts_datatable_proxy %>%
    #     DT::showCols(1:17) %>%
    #     DT::hideCols(setdiff(1:17, input$vocabulary_table_cols))
    # })
    # 
    # # Show mapped concepts in the datatable
    # 
    # observeEvent(input$vocabulary_show_mapped_concepts, {
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$vocabulary_show_mapped_concepts"))
    #   
    #   req(length(input$vocabulary) > 0)
    #   
    #   r$reload_vocabulary_datatable <- now()
    # })
    # 
    # # Reload datatable
    # 
    # observeEvent(r$reload_vocabulary_datatable, {
    # 
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer r$reload_vocabulary_datatable"))
    # 
    #   # Reset row details
    #   output$vocabulary_datatable_selected_item <- renderUI("")
    #   output$vocabulary_datatable_selected_item_plot <- plotly::renderPlotly(plotly::plotly_empty(data = tibble::tibble(x = 1), type = "scatter", mode = "lines") %>% plotly::config(displayModeBar = FALSE))
    #   shinyjs::hide("vocabulary_datatable_selected_item_div")
    # 
    #   vocabulary_id <- "all_vocabularies"
    #   if (input$vocabulary$key != 0) vocabulary_id <- r$vocabulary %>% dplyr::filter(id == input$vocabulary$key) %>% dplyr::pull(vocabulary_id)
    # 
    #   # req(nrow(d$dataset_all_concepts) > 0)
    # 
    #   # Filter only used concepts in d vars
    #   r$dataset_vocabulary_concepts <- d$dataset_all_concepts %>%
    #     # dplyr::filter(count_concepts_rows > 0 | count_secondary_concepts_rows > 0) %>%
    #     dplyr::filter(count_concepts_rows > 0) %>%
    #     # dplyr::select(-count_secondary_concepts_rows, -add_concept_input) %>%
    #     dplyr::select(-add_concept_input) %>%
    #     dplyr::arrange(dplyr::desc(count_concepts_rows)) 
    #   
    #   if (vocabulary_id != "all_vocabularies") r$dataset_vocabulary_concepts <- r$dataset_vocabulary_concepts %>% dplyr::filter(vocabulary_id_1 == !!vocabulary_id)
    # 
    #   if (!input$vocabulary_show_mapped_concepts) r$dataset_vocabulary_concepts <- r$dataset_vocabulary_concepts %>% dplyr::filter(is.na(relationship_id))
    # 
    #   # Merge count_concepts_rows & count_secondary_concepts_rows cols
    #   # r$dataset_vocabulary_concepts <- r$dataset_vocabulary_concepts %>% dplyr::mutate(
    #   #   count_concepts_rows = ifelse(count_secondary_concepts_rows > 0, count_secondary_concepts_rows, count_concepts_rows)) %>%
    #   #   dplyr::select(-count_secondary_concepts_rows) %>%
    #   #   dplyr::arrange(dplyr::desc(count_concepts_rows))
    # 
    #   r$dataset_vocabulary_concepts <- r$dataset_vocabulary_concepts %>%
    #     dplyr::mutate(modified = FALSE) %>%
    #     dplyr::mutate_at("concept_id_1", as.character)
    # 
    #   editable_cols <- c("concept_name_1", "concept_display_name_1")
    #   searchable_cols <- c("vocabulary_id_1", "concept_id_1", "concept_name_1", "relationship_id", "vocabulary_id_2", "concept_id_2", "concept_name_2", "concept_display_name_1", "domain_id")
    #   column_widths <- c("count_persons_rows" = "80px", "count_concepts_rows" = "80px")
    #   sortable_cols <- c("concept_id_1", "concept_name_1", "relationship_id", "concept_id_2", "concept_name_2", "concept_display_name_1", "domain_id", "count_persons_rows", "count_concepts_rows")
    #   factorize_cols <- c("vocabulary_id_1", "vocabulary_id_2", "relationship_id", "domain_id", "concept_class_id", "standard_concept", "invalid_reason")
    #   centered_cols <- c("concept_id_1", "relationship_id", "concept_id_2", "domain_id", "count_persons_rows", "count_concepts_rows")
    #   col_names <- get_col_names(table_name = "dataset_vocabulary_concepts_with_counts", i18n = i18n)
    #   hidden_cols <- c("id", "vocabulary_id_1", "relationship_id", "vocabulary_id_2", "concept_id_2", "concept_name_2", "concept_class_id", 
    #     "standard_concept", "concept_code", "valid_start_date", "valid_end_date", "invalid_reason", "modified")
    #   value_show_cols <- c(2, 3, 4, 9, 16, 17)
    # 
    #   if (input$vocabulary_show_mapped_concepts){
    #     hidden_cols <- c("id", "vocabulary_id_1", "concept_display_name_1", "vocabulary_id_2", "concept_id_2", "concept_class_id", 
    #       "standard_concept", "concept_code", "valid_start_date", "valid_end_date", "invalid_reason", "modified", "relationship_id", "concept_name_2")
    #     value_show_cols <- c(2, 3, 5, 7, 8, 16, 17)
    #   }
    # 
    #   shiny.fluent::updateDropdown.shinyInput(session, "vocabulary_table_cols", value = value_show_cols)
    # 
    #   # Render datatable
    #   render_datatable(output = output, ns = ns, i18n = i18n, data = r$dataset_vocabulary_concepts,
    #     output_name = "vocabulary_concepts", col_names = col_names,
    #     editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
    #     searchable_cols = searchable_cols, filter = TRUE, hidden_cols = hidden_cols, factorize_cols = factorize_cols)
    # 
    #   # Create a proxy for datatatable
    #   r$dataset_vocabulary_concepts_datatable_proxy <- DT::dataTableProxy("vocabulary_concepts", deferUntilFlush = FALSE)
    # })
    # 
    # # Reload cache
    # 
    # observeEvent(input$reload_vocabulary_concepts_cache, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$reload_vocabulary_concepts_cache"))
    #   
    #   r$dataset_all_concepts_reload_cache_open_dialog <- TRUE
    # })
    # 
    # r$dataset_all_concepts_reload_cache_open_dialog <- FALSE
    # 
    # output$dataset_all_concepts_reload_cache <- shiny.fluent::renderReact({
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - output$dataset_all_concepts_reload_cache"))
    #   
    #   shiny.fluent::Dialog(
    #     hidden = !r$dataset_all_concepts_reload_cache_open_dialog,
    #     onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('vdataset_all_concepts_reload_cache_hide_dialog', Math.random()); }")),
    #     dialogContentProps = list(
    #       type = 0,
    #       title = i18n$t("dataset_all_concepts_reload_cache"),
    #       closeButtonAriaLabel = "Close",
    #       subText = tagList( i18n$t("dataset_all_concepts_reload_cache_subtext"), br(), br())
    #     ),
    #     shiny.fluent::DialogFooter(
    #       shiny.fluent::PrimaryButton.shinyInput(ns("dataset_all_concepts_reload_cache_confirmed"), text = i18n$t("yes")),
    #       shiny.fluent::DefaultButton.shinyInput(ns("dataset_all_concepts_reload_cache_canceled"), text = i18n$t("no"))
    #     )
    #   )
    # })
    # 
    # observeEvent(input$dataset_all_concepts_reload_cache_canceled, r$dataset_all_concepts_reload_cache_open_dialog <- FALSE)
    # 
    # observeEvent(input$dataset_all_concepts_reload_cache_confirmed, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$dataset_all_concepts_reload_cache_confirmed"))
    #   
    #   # Close dialog box
    #   r$dataset_all_concepts_reload_cache_open_dialog <- FALSE
    #   
    #   file.remove(paste0(r$app_folder, "/datasets_files/", r$selected_dataset, "/dataset_all_concepts.csv"))
    #   
    #   shinyjs::delay(100, {
    #     r$load_dataset_all_concepts <- now()
    #     show_message_bar(output, "cache_reloaded", type = "success", i18n = i18n, ns = ns)
    #   })
    # })
    # 
    # # Updates on datatable data
    # observeEvent(input$vocabulary_concepts_cell_edit, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$vocabulary_concepts_cell_edit"))
    #   
    #   edit_info <- input$vocabulary_concepts_cell_edit
    #   r$dataset_vocabulary_concepts <- DT::editData(r$dataset_vocabulary_concepts, edit_info, rownames = FALSE)
    #   
    #   # Store that this row has been modified
    #   r$dataset_vocabulary_concepts[[edit_info$row, "modified"]] <- TRUE
    #   concept_id <- r$dataset_vocabulary_concepts[[edit_info$row, "concept_id_1"]]
    #   
    #   # Save this update in d$dataset_all_concepts also
    #   d$dataset_all_concepts <- d$dataset_all_concepts %>% dplyr::mutate(
    #     concept_name_1 = dplyr::case_when(
    #       concept_id_1 == concept_id ~ r$dataset_vocabulary_concepts[[edit_info$row, "concept_name_1"]],
    #       TRUE ~ concept_name_1),
    #     concept_display_name_1 = dplyr::case_when(
    #       concept_id_1 == concept_id ~ r$dataset_vocabulary_concepts[[edit_info$row, "concept_display_name_1"]],
    #       TRUE ~ concept_display_name_1),
    #     concept_name_2 = dplyr::case_when(
    #       concept_id_2 == concept_id ~ r$dataset_vocabulary_concepts[[edit_info$row, "concept_name_1"]],
    #       TRUE ~ concept_name_2)
    #   )
    # })
    # 
    # # Save updates
    # observeEvent(input$save_vocabulary_concepts, {
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "start")
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$save_vocabulary_concepts"))
    #   
    #   # vocabulary_id <- r$vocabulary %>% dplyr::filter(id == input$vocabulary$key) %>% dplyr::pull(vocabulary_id)
    #   
    #   r$dataset_vocabulary_concept_user <- 
    #     r$dataset_vocabulary_concepts %>%
    #     dplyr::filter(modified) %>%
    #     dplyr::transmute(user_id = r$user_id, concept_id = concept_id_1, concept_name = concept_name_1, concept_display_name = concept_display_name_1, vocabulary_id = vocabulary_id_1)
    #   r$dataset_vocabulary_concept_user_temp <- r$dataset_vocabulary_concept_user %>% dplyr::mutate(modified = TRUE)
    #   
    #   save_settings_datatable_updates(output = output, r = r, m = m, ns = ns, table = "concept_user", r_table = "dataset_vocabulary_concept_user", duplicates_allowed = TRUE, i18n = i18n)
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer input$save_vocabulary_concepts"))
    # })
    # 
    # vocabulary_concepts_row_details <- tibble::tribble(
    #   ~domain_id, ~table, ~concept_id, ~cols,
    #   "Measurement", "measurement", "measurement_concept_id", list("value_as_number" = "values_as_number", "value_as_concept_id" = "values_as_concept_id"),
    #   "Observation", "observation", "observation_concept_id", list("value_as_number" = "values_as_number", "value_as_string" = "values_as_string", "value_as_concept_id" = "values_as_concept_id"),
    #   "Drug", "drug_exposure", "drug_concept_id", list("quantity" = "quantities"),
    #   "Procedure", "procedure_occurrence", "procedure_concept_id", list("quantity" = "quantities"),
    #   "Specimen", "specimen", "specimen_concept_id", list("quantity" = "quantities")
    # )
    # 
    # # When a row is selected
    # observeEvent(input$vocabulary_concepts_rows_selected, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$vocabulary_concepts_rows_selected"))
    #   
    #   shinyjs::show("vocabulary_datatable_selected_item_div")
    #   
    #   r$vocabulary_concepts_rows_selected_trigger <- now()
    #   r$vocabulary_concepts_rows_selected_type <- "main_vocab"
    # })
    # 
    # observeEvent(r$vocabulary_concepts_rows_selected_trigger, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer r$vocabulary_concepts_rows_trigger"))
    #   
    #   if (r$vocabulary_concepts_rows_selected_type == "main_vocab") selected_concept <- r$dataset_vocabulary_concepts[input$vocabulary_concepts_rows_selected, ]
    #   else if (r$vocabulary_concepts_rows_selected_type == "mapping_vocab_1") selected_concept <- r$dataset_vocabulary_concepts_mapping_1[input$vocabulary_mapping_1_dt_rows_selected, ]
    #   else if (r$vocabulary_concepts_rows_selected_type == "mapping_vocab_2") selected_concept <- r$dataset_vocabulary_concepts_mapping_2[input$vocabulary_mapping_2_dt_rows_selected, ]
    #   
    #   r$vocabulary_selected_concept <- selected_concept
    #   
    #   concept_values <- tagList()
    #   
    #   row <- vocabulary_concepts_row_details %>% dplyr::filter(domain_id == selected_concept$domain_id)
    #   
    #   hide_dropdown <- FALSE
    #   
    #   if (r$vocabulary_concepts_rows_selected_type == "main_vocab"){
    #     if (nrow(row) == 0) hide_dropdown <- TRUE
    #     if (nrow(row) > 0) if (d[[row$table]] %>% dplyr::count() %>% dplyr::pull() == 0) hide_dropdown <- TRUE
    #   }
    # 
    #   if (hide_dropdown){
    #     shinyjs::hide("vocabulary_datatable_selected_item_plot")
    #     output$vocabulary_datatable_selected_item_error_message <- renderUI(tagList(br(),
    #       div(shiny.fluent::MessageBar(i18n$t("no_data_to_show"), messageBarType = 5), style = "width:300px; margin-left:42px;")))
    #     shiny.fluent::updateDropdown.shinyInput(session, "vocabulary_datatable_selected_item_plot_variable", options = list(), value = NULL)
    #   }
    #   
    #   if (nrow(row) > 0 & r$vocabulary_concepts_rows_selected_type %in% c("main_vocab", "mapping_vocab_1")){
    #     if (d[[row$table]] %>% dplyr::count() %>% dplyr::pull() > 0){
    #       
    #       if (r$vocabulary_concepts_rows_selected_type == "main_vocab") shinyjs::show("vocabulary_datatable_selected_item_plot")
    #       
    #       if (r$vocabulary_concepts_rows_selected_type != "main_vocab") concept_id <- selected_concept$concept_id
    #       else if (!is.na(selected_concept$concept_id_2)) concept_id <- selected_concept$concept_id_2
    #       else concept_id <- selected_concept$concept_id_1
    #       
    #       if ("tbl_lazy" %in% class(d[[row$table]])) values <- d[[row$table]] %>% dplyr::filter(rlang::sym(row$concept_id) == concept_id) %>% dplyr::collect()
    #       else values <- d[[row$table]] %>% dplyr::filter(get(row$concept_id) == concept_id)
    #       
    #       # Add values for source_concept_id cols (except for Specimen domain)
    #       if (row$domain_id != "Specimen") values <- values %>% dplyr::bind_rows(
    #         if ("tbl_lazy" %in% class(d[[row$table]])) d[[row$table]] %>% dplyr::filter(rlang::sym(row$concept_id %>% stringr::str_replace("concept_id", "source_concept_id")) == concept_id) %>% dplyr::collect()
    #         else d[[row$table]] %>% dplyr::filter(get(row$concept_id %>% stringr::str_replace("concept_id", "source_concept_id")) == concept_id)
    #       )
    #       
    #       r$vocabulary_selected_concept_values <- values
    #       
    #       if (nrow(values) > 0){
    #         
    #         unit <- character()
    #         if (selected_concept$domain_id %in% c("Measurement", "Observation")){
    #           values <- values %>% 
    #             dplyr::left_join(d$dataset_all_concepts %>% dplyr::select(unit_concept_id = concept_id_1, unit_concept_code = concept_name_1), by = "unit_concept_id")
    #           unit <- values %>% dplyr::filter(!is.na(unit_concept_code)) %>% dplyr::distinct(unit_concept_code) %>% dplyr::pull()
    #         } 
    #         r$vocabulary_selected_concept_unit <- unit
    #         
    #         values <- values %>% dplyr::slice_sample(n = 5, replace = TRUE)
    #         
    #         cols <- row$cols %>% unlist()
    #         
    #         selected_item_plot_variable_options <- list()
    #         
    #         i <- FALSE
    #         
    #         for (name in names(cols)){
    #           col <- cols[[name]]
    #           
    #           selected_item_plot_variable_options <- rlist::list.append(selected_item_plot_variable_options, list(key = name, text = i18n$t(col)))
    #           if (!i) selected_item_plot_variable_value <- name
    #           
    #           if (nrow(values %>% dplyr::filter(!is.na(get(name)))) == 0) concept_values <- tagList(concept_values, br(), strong(i18n$t(col)), " : /")
    #           else {
    #             if (name == "value_as_number") concept_values <- tagList(concept_values, br(),
    #               strong(i18n$t("values_as_number")), " : ", values %>% 
    #                 dplyr::mutate(result = ifelse(!is.na(unit_concept_code), paste0(value_as_number, " ", unit_concept_code), value_as_number)) %>% dplyr::pull(result) %>% toString())
    #             
    #             else if (name == "value_as_concept_id"){
    #               values <- values %>% dplyr::left_join(
    #                 d$dataset_all_concepts %>% dplyr::select(value_as_concept_id = concept_id_1, value_as_concept_name = concept_name_1),
    #                 by = "value_as_concept_id"
    #               )
    #               concept_values <- tagList(concept_values, br(),
    #                 strong(i18n$t("values_as_concept_id")), " : ", values %>% dplyr::pull(value_as_concept_name) %>% toString())
    #             }
    #             
    #             else concept_values <- tagList(concept_values, br(),
    #               strong(i18n$t(col)), " : ", values %>% dplyr::slice_sample(n = 5, replace = TRUE) %>% 
    #                 dplyr::pull(name) %>% toString())
    #           }
    #           
    #           i <- TRUE
    #         }
    #         
    #         # Update dropdown for plotly var
    #         if (r$vocabulary_concepts_rows_selected_type == "main_vocab") shiny.fluent::updateDropdown.shinyInput(session, 
    #           "vocabulary_datatable_selected_item_plot_variable", options = selected_item_plot_variable_options, value = selected_item_plot_variable_value)
    #       }
    #     }
    #   }
    #   
    #   concept_info <- tagList()
    #   
    #   if (r$vocabulary_concepts_rows_selected_type != "main_vocab") for (name in c("concept_id", "concept_name")) concept_info <- 
    #     tagList(concept_info, strong(i18n$t(name)), " : ", selected_concept[[name]], br())
    #   
    #   else if ((r$vocabulary_concepts_rows_selected_type == "main_vocab" & is.na(selected_concept$relationship_id))) for (name in 
    #     c("concept_id_1", "concept_name_1", "concept_display_name_1")){
    #     value <- selected_concept[[name]]
    #     if (is.na(value)) value <- ""
    #     concept_info <- tagList(concept_info, strong(i18n$t(stringr::str_replace(name, "_1", ""))), " : ", value, br())
    #   }
    #   
    #   else for (name in c("concept_id_1", "concept_name_1", "concept_display_name_1", "relationship_id",
    #     "concept_id_2", "concept_name_2")){
    #     value <- selected_concept[[name]]
    #     if (is.na(value)) value <- ""
    #     concept_info <- tagList(concept_info, strong(i18n$t(name)), " : ", value, br())
    #   }
    #   
    #   concept_info <- tagList(concept_info, br())
    #   
    #   for (name in c("domain_id", "standard_concept", "concept_code")) concept_info <- 
    #     tagList(concept_info, strong(i18n$t(name)), " : ", selected_concept[[name]], br())
    #   
    #   concept_info <- tagList(concept_info, concept_values, br(), br())
    #   
    #   if (r$vocabulary_concepts_rows_selected_type == "main_vocab"){
    #     output$vocabulary_datatable_selected_item <- renderUI(concept_info)
    #     
    #     # Update plotly
    #     r$vocabulary_datatable_selected_item_reload_plot <- now()
    #   }
    #   else if (r$vocabulary_concepts_rows_selected_type == "mapping_vocab_1") output$vocabulary_mapping_selected_concept_1 <- renderUI(concept_info)
    #   else if (r$vocabulary_concepts_rows_selected_type == "mapping_vocab_2") output$vocabulary_mapping_selected_concept_2 <- renderUI(concept_info)
    # })
    # 
    # observeEvent(input$vocabulary_datatable_selected_item_plot_variable, {
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$vocabulary_datatable_selected_item_plot_variable"))
    #   r$vocabulary_datatable_selected_item_reload_plot <- now()
    # })
    # 
    # # Update plotly
    # 
    # observeEvent(r$vocabulary_datatable_selected_item_reload_plot, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer r$vocabulary_datatable_selected_item_reload_plot"))
    #   
    #   if(length(input$vocabulary_datatable_selected_item_plot_variable) == 0){
    #     output$vocabulary_datatable_selected_item_error_message <- renderUI(tagList(br(),
    #       div(shiny.fluent::MessageBar(i18n$t("no_data_to_show"), messageBarType = 5), style = "width:300px; margin-left:42px;")))
    #     shinyjs::hide("vocabulary_datatable_selected_item_plot")
    #   }
    #   
    #   req(length(input$vocabulary_datatable_selected_item_plot_variable) > 0)
    #   
    #   selected_concept <- r$vocabulary_selected_concept
    #   row <- vocabulary_concepts_row_details %>% dplyr::filter(domain_id == selected_concept$domain_id)
    #   values <- r$vocabulary_selected_concept_values
    #   plot_variable <- input$vocabulary_datatable_selected_item_plot_variable
    #   unit <- r$vocabulary_selected_concept_unit
    #   
    #   req(plot_variable %in% names(row$cols %>% unlist()))
    #   
    #   error_message <- NA_character_
    #   
    #   # Render empty plotly if there's only distinct value of the concept
    #   # Or if there's too much distinct categorical values
    #   
    #   n_distinct_values <- values %>% dplyr::distinct(get(plot_variable)) %>% dplyr::rename("col" := "get(plot_variable)") %>% dplyr::filter(!is.na(col)) %>% nrow()
    #   
    #   if (n_distinct_values <= 1){
    #     concept_plot <- plotly::plotly_empty(data = tibble::tibble(x = 1), type = "scatter", mode = "lines") %>% plotly::config(displayModeBar = FALSE)
    #     error_message <- "no_data_to_show"
    #   }
    #   
    #   if (n_distinct_values > 1){
    #     
    #     # Keep unit if it is unique
    #     x_axis_name <- selected_concept$concept_name_1
    #     text_part_2 <- ""
    #     
    #     if (length(unit) == 1){
    #       x_axis_name <- paste0(selected_concept$concept_name_1, " (", unit, ")")
    #       text_part_2 <- paste0("<br />", i18n$t("unit"), " : ", unit)
    #     }
    #     
    #     values <- values %>% dplyr::mutate(row_id = 1:dplyr::n())
    #     values <- values %>% dplyr::left_join(
    #       values %>% dplyr::select(row_id, !!plot_variable) %>% dplyr::group_by_at(plot_variable) %>% 
    #         dplyr::summarize(row_id, count = dplyr::n()) %>% dplyr::ungroup() %>% dplyr::select(-!!plot_variable),
    #       by = "row_id"
    #     ) %>% dplyr::select(-row_id)
    #     
    #     if (plot_variable %in% c("value_as_concept_id", "value_as_string")){
    #       
    #       if (plot_variable == "value_as_concept_id") plot_variable <- "value_as_concept_name"
    #       
    #       if (n_distinct_values > 5){
    #         suppressWarnings(concept_plot <- plotly::plotly_empty(data = tibble::tibble(x = 1), type = "scatter", mode = "lines") %>% plotly::config(displayModeBar = FALSE))
    #         error_message <- "too_many_x_axis_values"
    #       }
    #       
    #       if (n_distinct_values <= 5) concept_plot <- values %>%
    #           ggplot2::ggplot(ggplot2::aes(x = get(plot_variable),
    #             text = paste0(i18n$t("value"), " : ", get(plot_variable), text_part_2, "<br />", i18n$t("row_number"), " : ", count))) +
    #           # ggplot2::geom_histogram(fill = "#4F86C6", stat = "count", bins = input$vocabulary_datatable_selected_item_plot_bins) +
    #           ggplot2::geom_histogram(fill = "#4F86C6", stat = "count") +
    #           ggplot2::labs(
    #             x = selected_concept$concept_name_1, 
    #             y = paste0(i18n$t("proportion"), " (%)")) +
    #           ggplot2::theme(axis.title = ggplot2::element_text(size = 10), axis.text = ggplot2::element_text(size = 10))
    #     }
    #     
    #     else {
    #       concept_plot <- values %>%
    #         ggplot2::ggplot(ggplot2::aes(x = get(plot_variable), y = 100 * ..count.. / sum(..count..),
    #           text = paste0(i18n$t("value"), " : ", get(plot_variable), text_part_2, "<br />", i18n$t("row_number"), " : ", count))) +
    #         # ggplot2::geom_histogram(fill = "#4F86C6", bins = input$vocabulary_datatable_selected_item_plot_bins) +
    #         ggplot2::geom_histogram(fill = "#4F86C6") +
    #         ggplot2::labs(
    #           x = x_axis_name, 
    #           y = paste0(i18n$t("proportion"), " (%)")) +
    #         ggplot2::theme(axis.title = ggplot2::element_text(size = 10), axis.text = ggplot2::element_text(size = 10))
    #     }
    #     
    #     concept_plot <- concept_plot %>% 
    #       plotly::ggplotly(tooltip = "text", source = "vocabulary_concept_plot") %>%
    #       plotly::config(displayModeBar = FALSE) %>%
    #       plotly::style(hoverlabel = list(bgcolor = "white", font = list(size = 12))) %>%
    #       plotly::layout(xaxis = list(tickfont = list(size = 12)), yaxis = list(tickfont = list(size = 12))) %>%
    #       suppressWarnings() %>% suppressMessages()
    #   }
    #   
    #   output$vocabulary_datatable_selected_item_plot <- plotly::renderPlotly(concept_plot)
    #   
    #   if (!is.na(error_message)){
    #     output$vocabulary_datatable_selected_item_error_message <- renderUI(tagList(br(),
    #       div(shiny.fluent::MessageBar(i18n$t(error_message), messageBarType = 5), style = "width:300px; margin-left:42px;")))
    #     shinyjs::hide("vocabulary_datatable_selected_item_plot")
    #   } 
    #   else {
    #     output$vocabulary_datatable_selected_item_error_message <- renderUI("")
    #     shinyjs::show("vocabulary_datatable_selected_item_plot")
    #   }
    #   
    # })
    # 
    # # Update bins of plotly histogram
    # 
    # # observeEvent(plotly::event_data(event = "plotly_relayout", source = "vocabulary_concept_plot"), {
    # # 
    # #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$vocabulary_datatable_selected_item_plot_bins"))
    # # 
    # #   relayout_data <- plotly::event_data(event = "plotly_relayout", source = "vocabulary_concept_plot")
    # #   
    # #   if (!is.null(relayout_data$`xaxis.range[0]`)) {
    # #     x_min <- relayout_data$`xaxis.range[0]`
    # #     x_max <- relayout_data$`xaxis.range[1]`
    # #     bin_width <- (x_max - x_min) / 20
    # #     plotly::plotlyProxy("vocabulary_datatable_selected_item_plot", session) 
    # #     
    # #     plotly::plotlyProxyInvoke("relayout", list(xaxis = list(range = list(x_min, x_max), 
    # #       xbins = list(size = bin_width))))
    # #   }
    # # })
    # 
    # # --- --- --- --- --
    # # Items mapping ----
    # # --- --- --- --- --
    # 
    # # --- --- --- --- --- --
    # ## Create a mapping ----
    # # --- --- --- --- --- --
    # 
    # observeEvent(input$show_all_relationships, {
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$show_all_relationships"))
    #   if (input$show_all_relationships){
    #     # Get relationships from database
    #     sql <- glue::glue_sql("SELECT * FROM relationship", .con = m$db)
    #     d$relationship <- DBI::dbGetQuery(m$db, sql)
    #     relationship <- d$relationship %>%
    #       dplyr::mutate(relationship_short_name = dplyr::case_when(
    #         relationship_id == "Maps to" ~ i18n$t("maps_to"),
    #         relationship_id == "Mapped from" ~ i18n$t("mapped_from"),
    #         relationship_id == "Is a" ~ i18n$t("is_a"),
    #         relationship_id == "Subsumes" ~ i18n$t("subsumes"),
    #         TRUE ~ relationship_id
    #       )) %>%
    #       dplyr::arrange(relationship_id)
    #     
    #     relationship_options <- convert_tibble_to_list(relationship, key_col = "relationship_id", text_col = "relationship_short_name")
    #   }
    #   else relationship_options <- list(
    #     list(key = "Maps to", text = i18n$t("maps_to")),
    #     list(key = "Mapped from", text = i18n$t("mapped_from")),
    #     list(key = "Is a", text = i18n$t("is_a")),
    #     list(key = "Subsumes", text = i18n$t("subsumes"))
    #   )
    #   
    #   shiny.fluent::updateComboBox.shinyInput(session, "relationship_id", options = relationship_options, 
    #     value = list(key = "Maps to", text = i18n$t("maps_to")))
    # })
    # 
    # observeEvent(input$vocabulary_mapping_1, {
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$vocabulary_mapping_1"))
    #   r$vocabulary_mapping_reload <- paste0(now(), "_mapping_1")
    #   shinyjs::show("vocabulary_mapping_selected_concepts_div")
    # })
    # observeEvent(input$vocabulary_mapping_2, {
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$vocabulary_mapping_2"))
    #   r$vocabulary_mapping_reload <- paste0(now(), "_mapping_2")
    #   shinyjs::show("vocabulary_mapping_selected_concepts_div")
    # })
    # observeEvent(input$vocabulary_show_only_not_mapped_concepts, {
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$show_only_not_mapped_concepts"))
    #   req(length(r$vocabulary_mapping_reload) > 0)
    #   r$vocabulary_mapping_reload <- paste0(now(), "_mapping_1")
    # })
    # observeEvent(input$vocabulary_show_only_used_concepts, {
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$vocabulary_show_only_used_concepts"))
    #   req(length(r$vocabulary_mapping_reload) > 0)
    #   r$vocabulary_mapping_reload <- paste0(now(), "_mapping_2")
    # })
    # 
    # observeEvent(r$vocabulary_mapping_reload, {
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "start")
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer r$vocabulary_mapping_reload"))
    #   
    #   if (grepl("mapping_1", r$vocabulary_mapping_reload)) mapping <- "mapping_1"
    #   else if (grepl("mapping_2", r$vocabulary_mapping_reload)) mapping <- "mapping_2"
    #   
    #   req(length(input[[paste0("vocabulary_", mapping)]]$key) > 0)
    #   
    #   # Show only not mapped concepts ?
    #   
    #   show_only_not_mapped_concepts <- FALSE
    #   if (length(input$vocabulary_show_only_not_mapped_concepts) > 0) if (input$vocabulary_show_only_not_mapped_concepts) show_only_not_mapped_concepts <- TRUE
    #   
    #   if (show_only_not_mapped_concepts) sql <- glue::glue_sql(
    #     paste0("SELECT c.* FROM concept c ",
    #       "WHERE c.vocabulary_id = {input[[paste0('vocabulary_', mapping)]]$key} ",
    #       "AND c.concept_id NOT IN (SELECT cr.concept_id_1 FROM concept_relationship cr) ",
    #       "AND c.concept_id NOT IN (SELECT cr.concept_id_2 FROM concept_relationship cr) ",
    #       "ORDER BY concept_id"), .con = m$db)
    #   
    #   else sql <- glue::glue_sql("SELECT * FROM concept WHERE vocabulary_id = {input[[paste0('vocabulary_', mapping)]]$key} ORDER BY concept_id", .con = m$db)
    #  
    #   r[[paste0("dataset_vocabulary_concepts_", mapping)]] <- DBI::dbGetQuery(m$db, sql) %>% tibble::as_tibble()
    #   
    #   # Merge count_rows from d$dataset_all_concepts (for mapping_1) or from database (for mapping_2)
    #   # For mapping_1, keep only concepts used in current dataset tables
    #   
    #   if (mapping == "mapping_1"){
    #     load_dataset_concepts <- FALSE
    #     if (length(r$selected_dataset) > 0) if (!is.na(r$selected_dataset)) load_dataset_concepts <- TRUE
    #       
    #     if (load_dataset_concepts) r$dataset_vocabulary_concepts_mapping_1 <- 
    #       r$dataset_vocabulary_concepts_mapping_1 %>%
    #       dplyr::left_join(
    #         d$dataset_all_concepts %>% 
    #           dplyr::filter(is.na(relationship_id)) %>%
    #           dplyr::select(concept_id = concept_id_1, count_concepts_rows),
    #         by = "concept_id"
    #       )
    #   }
    #   
    #   if (mapping == "mapping_2"){
    #     # In that case, we sum all count_concepts_rows from various datasets
    #     
    #     sql <- glue::glue_sql(paste0("SELECT concept_id, COUNT(DISTINCT(dataset_id)) AS count_datasets, ",
    #     "SUM(count_concepts_rows) AS count_concepts_rows ",
    #     "FROM concept_dataset ",
    #     "WHERE vocabulary_id = {input[[paste0('vocabulary_', mapping)]]$key}",
    #     "GROUP BY concept_id "), .con = m$db)
    #     concept_dataset <- DBI::dbGetQuery(m$db, sql)
    #     
    #     r$dataset_vocabulary_concepts_mapping_2 <- 
    #       r$dataset_vocabulary_concepts_mapping_2 %>%
    #       dplyr::left_join(
    #         concept_dataset %>% dplyr::select(concept_id, count_datasets, count_concepts_rows),
    #         by = "concept_id"
    #       ) %>%
    #       dplyr::mutate(count_datasets = ifelse(is.na(count_datasets), 0L, count_datasets))
    #     
    #     # Show only used concepts ?
    #     
    #     if (input$vocabulary_show_only_used_concepts) r$dataset_vocabulary_concepts_mapping_2 <- r$dataset_vocabulary_concepts_mapping_2 %>% dplyr::filter(count_concepts_rows > 0)
    #   }
    #   
    #   # Convert concept_id to character to be sortable
    #   r[[paste0("dataset_vocabulary_concepts_", mapping)]] <- r[[paste0("dataset_vocabulary_concepts_", mapping)]] %>%
    #     dplyr::mutate_at("concept_id", as.character)
    #   
    #   if ("count_concepts_rows" %in% colnames(r[[paste0("dataset_vocabulary_concepts_", mapping)]])) r[[paste0("dataset_vocabulary_concepts_", mapping)]] <- r[[paste0("dataset_vocabulary_concepts_", mapping)]] %>% 
    #     dplyr::mutate(count_concepts_rows = ifelse(is.na(count_concepts_rows), 0L, count_concepts_rows)) %>%
    #     dplyr::arrange(dplyr::desc(count_concepts_rows))
    #   
    #   searchable_cols <- c("concept_id", "concept_name", "count_datasets", "count_concepts_rows")
    #   column_widths <- c("concept_id" = "100px", "count_datasets" = "80px", "count_concepts_rows" = "80px")
    #   sortable_cols <- c("concept_id", "concept_name", "count_datasets", "count_concepts_rows")
    #   centered_cols <- c("concept_id", "count_datasets", "count_concepts_rows")
    #   hidden_cols <- c("id", "domain_id", "vocabulary_id", "concept_class_id", "standard_concept", "concept_code",
    #     "valid_start_date", "valid_end_date", "invalid_reason")
    #   shortened_cols <- c("concept_name" = 40)
    #   
    #   if (mapping == "mapping_1"){
    #     if (load_dataset_concepts) col_names <- get_col_names(table_name = "mapping_vocabulary_concepts_with_counts", i18n = i18n)
    #     else col_names <- get_col_names(table_name = "mapping_vocabulary_concepts", i18n = i18n)
    #   }
    #   if (mapping == "mapping_2") col_names <- get_col_names(table_name = "mapping_vocabulary_concepts_with_counts_and_datasets", i18n = i18n)
    #   
    #   # Render datatable
    #   render_datatable(output = output, ns = ns, i18n = i18n, data = r[[paste0("dataset_vocabulary_concepts_", mapping)]],
    #     output_name = paste0("vocabulary_", mapping, "_dt"), col_names = col_names, datatable_dom = "<'top't><'bottom'p>",
    #     sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
    #     searchable_cols = searchable_cols, filter = TRUE, hidden_cols = hidden_cols, shortened_cols = shortened_cols)
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$vocabulary_mapping_reload"))
    # })
    # 
    # # When a row is selected
    # observeEvent(input$vocabulary_mapping_1_dt_rows_selected, {
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$vocabulary_mapping_1_dt_rows_selected"))
    #   
    #   r$vocabulary_concepts_rows_selected_trigger <- now()
    #   r$vocabulary_concepts_rows_selected_type <- "mapping_vocab_1"
    # })
    # observeEvent(input$vocabulary_mapping_2_dt_rows_selected, {
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$vocabulary_mapping_2_dt_rows_selected"))
    #   
    #   r$vocabulary_concepts_rows_selected_trigger <- now()
    #   r$vocabulary_concepts_rows_selected_type <- "mapping_vocab_2"
    # })
    # 
    # # When a mapping id added
    # 
    # observeEvent(input$add_mapping, {
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "start")
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$add_mapping"))
    #   
    #   req(length(input$vocabulary_mapping_1_dt_rows_selected) > 0)
    #   req(length(input$vocabulary_mapping_2_dt_rows_selected) > 0)
    #   req(nrow(r$dataset_vocabulary_concepts_mapping_1[input$vocabulary_mapping_1_dt_rows_selected, ]) > 0)
    #   req(nrow(r$dataset_vocabulary_concepts_mapping_2[input$vocabulary_mapping_2_dt_rows_selected, ]) > 0)
    #   
    #   concept_1 <- r$dataset_vocabulary_concepts_mapping_1[input$vocabulary_mapping_1_dt_rows_selected, ] %>% dplyr::mutate_at("concept_id", as.integer)
    #   concept_2 <- r$dataset_vocabulary_concepts_mapping_2[input$vocabulary_mapping_2_dt_rows_selected, ] %>% dplyr::mutate_at("concept_id", as.integer)
    #   
    #   if (length(input$relationship_id) > 1) relationship_id <- input$relationship_id$key
    #   else relationship_id <- input$relationship_id
    #   
    #   # Check if mapping already added in database
    #   
    #   check_duplicates <- FALSE
    #   
    #   sql <- glue::glue_sql(paste0("SELECT * FROM concept_relationship WHERE ",
    #     "concept_id_1 = {concept_1$concept_id} AND concept_id_2 = {concept_2$concept_id} AND relationship_id = {relationship_id}"), .con = m$db)
    #   existing_mapping <- DBI::dbGetQuery(m$db, sql)
    #   
    #   if (nrow(existing_mapping) > 0) show_message_bar(output, "vocabulary_mapping_already_exists", "severeWarning", i18n, ns = ns)
    #   req(nrow(existing_mapping) == 0)
    #   
    #   vocabulary_mapping_same_concepts <- FALSE
    #   if (concept_1$concept_id == concept_2$concept_id & relationship_id %not_in% c("Maps to", "Mapped from")){
    #     show_message_bar(output,  "vocabulary_mapping_same_concepts", "severeWarning", i18n, ns = ns)
    #     vocabulary_mapping_same_concepts <- TRUE
    #   } 
    #   req(!vocabulary_mapping_same_concepts)
    #   
    #   # Add new mapping to r$vocabulary_added_mappings
    #   # Add also the reverse of this mapping
    #   
    #   last_row_concept_relationship <- get_last_row(m$db, "concept_relationship")
    #   if (relationship_id %in% c("Maps to", "Mapped from", "Is a", "Subsumes")) reverse_relationship_id <- switch(relationship_id, 
    #     "Maps to" = "Mapped from", "Mapped from" = "Maps to", "Is a" = "Subsumes", "Subsumes" = "Is a")
    #   else reverse_relationship_id <- d$relationship %>% dplyr::filter(relationship_id == !!relationship_id) %>% dplyr::pull(reverse_relationship_id)
    #   
    #   new_row_r_var <- tibble::tribble(
    #     ~id, ~concept_id_1, ~concept_name_1, ~concept_id_2, ~concept_name_2, ~relationship_id, ~valid_start_date, ~valid_end_date, ~invalid_reason,
    #     last_row_concept_relationship + 2, concept_1$concept_id, concept_1$concept_name, concept_2$concept_id, concept_2$concept_name, relationship_id, as.character(Sys.Date()), "2099-12-31", NA_character_,
    #     last_row_concept_relationship + 1, concept_2$concept_id, concept_2$concept_name, concept_1$concept_id, concept_1$concept_name, reverse_relationship_id, as.character(Sys.Date()), "2099-12-31", NA_character_)
    #   
    #   new_row_db <- new_row_r_var %>% dplyr::select(-concept_name_1, -concept_name_2)
    #   
    #   new_row_datatable <- new_row_db %>%
    #     dplyr::mutate(relationship_id = dplyr::case_when(
    #       relationship_id == "Maps to" ~ i18n$t("maps_to"),
    #       relationship_id == "Mapped from" ~ i18n$t("mapped_from"),
    #       relationship_id == "Is a" ~ i18n$t("is_a"),
    #       relationship_id == "Subsumes" ~ i18n$t("subsumes"),
    #       TRUE ~ relationship_id)) %>%
    #     dplyr::transmute(id, vocabulary_id_1 = c(input$vocabulary_mapping_1$key,input$vocabulary_mapping_2$key), concept_id_1, relationship_id, 
    #       vocabulary_id_2 = c(input$vocabulary_mapping_2$key, input$vocabulary_mapping_1$key), concept_id_2)
    #   
    #   r$vocabulary_added_mappings <- r$vocabulary_added_mappings %>% dplyr::bind_rows(new_row_datatable) %>% dplyr::arrange(dplyr::desc(id))
    #   
    #   last_row_concept_relationship_user <- get_last_row(m$db, "concept_relationship_user")
    #   
    #   mapping_comment <- ""
    #   if (length(input$mapping_comment) > 0) mapping_comment <- input$mapping_comment
    #   
    #   new_row_db_user <- tibble::tribble(~id, ~concept_relationship_id, ~comment, ~creator_id, ~datetime,
    #     last_row_concept_relationship_user + 1, last_row_concept_relationship + 1, mapping_comment, r$user_id, now(),
    #     last_row_concept_relationship_user + 2, last_row_concept_relationship + 2, mapping_comment, r$user_id, now())
    #   
    #   # Add new mapping to database
    # 
    #   DBI::dbAppendTable(m$db, "concept_relationship", new_row_db)
    #   DBI::dbAppendTable(m$db, "concept_relationship_user", new_row_db_user)
    # 
    #   # Notify user
    #   show_message_bar(output, "vocabulary_mapping_added", "success", i18n, ns = ns)
    # 
    #   # Update datatables
    #   
    #   action_button_visibility <- list()
    #   for (action in c("evaluate", "delete")){
    #     if (paste0("vocabularies_", action, "_mapping") %in% r$user_accesses) action_button_visibility[[action]] <- "visible"
    #     else action_button_visibility[[action]] <- "hidden"
    #   }
    #   
    #   r$reload_vocabulary_added_mappings_datatable <- now()
    # 
    #   r$dataset_vocabulary_concepts_evaluate_mappings <- r$dataset_vocabulary_concepts_evaluate_mappings %>%
    #     dplyr::bind_rows(
    #       new_row_r_var %>%
    #         dplyr::transmute(
    #           concept_relationship_id = id,
    #           vocabulary_id_1 = c(input$vocabulary_mapping_1$key, input$vocabulary_mapping_2$key), concept_id_1, concept_name_1, relationship_id,
    #           vocabulary_id_2 = c(input$vocabulary_mapping_2$key, input$vocabulary_mapping_1$key), concept_id_2, concept_name_2,
    #           creator_name = r$users %>% dplyr::filter(id == r$user_id) %>% dplyr::mutate(creator_name = paste0(firstname, " ", lastname)) %>% dplyr::pull(creator_name),
    #           datetime = now(), positive_evals = 0L, negative_evals = 0L) %>%
    #         dplyr::mutate(
    #           action = as.character(tagList(
    #             shiny::actionButton("positive_eval_%concept_relationship_id%", "", icon = icon("thumbs-up"),
    #               onclick = paste0("Shiny.setInputValue('", !!id, "-concept_mapping_evaluated_positive', this.id, {priority: 'event'})"),
    #               style = paste0("background-color:#E8E9EC; color:black; border-color:#8E8F9D; border-radius:3px; border-width:1px; visibility:", action_button_visibility$evaluate, ";")),
    #             shiny::actionButton("negative_eval_%concept_relationship_id%", "", icon = icon("thumbs-down"),
    #               onclick = paste0("Shiny.setInputValue('", !!id, "-concept_mapping_evaluated_negative', this.id, {priority: 'event'})"),
    #               style = paste0("background-color:#E8E9EC; color:black; border-color:#8E8F9D; border-radius:3px; border-width:1px; visibility:", action_button_visibility$evaluate, ";")),
    #             shiny::actionButton("remove_%concept_relationship_id%", "", icon = icon("trash-alt"),
    #               onclick = paste0("Shiny.setInputValue('", !!id, "-concept_mapping_deleted_pressed', this.id, {priority: 'event'})"),
    #               style = paste0("background-color:#E8E9EC; color:black; border-color:#8E8F9D; border-radius:3px; border-width:1px; visibility:", action_button_visibility$delete, ";"))
    #           )),
    #           user_evaluation_id = NA_integer_,
    #           modified = FALSE
    #         ) %>%
    #         dplyr::mutate(relationship_id = dplyr::case_when(
    #           relationship_id == "Maps to" ~ i18n$t("maps_to"),
    #           relationship_id == "Mapped from" ~ i18n$t("mapped_from"),
    #           relationship_id == "Is a" ~ i18n$t("is_a"),
    #           relationship_id == "Subsumes" ~ i18n$t("subsumes"),
    #           TRUE ~ relationship_id)) %>%
    #         dplyr::mutate(action = stringr::str_replace_all(action, "%concept_relationship_id%", as.character(concept_relationship_id))) %>%
    #         dplyr::mutate_at(c("concept_id_1", "concept_id_2"), as.character)
    #     ) %>%
    #     dplyr::arrange(dplyr::desc(concept_relationship_id))
    # 
    #   DT::replaceData(r$dataset_vocabulary_concepts_evaluate_mappings_datatable_proxy, r$dataset_vocabulary_concepts_evaluate_mappings, resetPaging = FALSE, rownames = FALSE)
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer input$add_mapping"))
    # })
    # 
    # # Table to summarize added mappings
    # 
    # observeEvent(r$vocabulary, {
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer r$vocabulary 2"))
    #   
    #   r$reload_vocabulary_evaluate_mappings_datatable <- now()
    #   r$reload_vocabulary_evaluate_mappings_datatable_type <- "reload_data"
    # })
    # 
    # observeEvent(r$selected_dataset, {
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "start")
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer r$selected_dataset 2"))
    #   
    #   r$vocabulary_added_mappings <- tibble::tibble(id = integer(), vocabulary_id_1 = character(), concept_id_1 = integer(),
    #     relationship_id = character(), vocabulary_id_2 = character(), concept_id_2 = integer())
    # 
    #   output$vocabulary_selected_concept_mapping_1 <- renderText("")
    #   output$vocabulary_selected_concept_mapping_2 <- renderText("")
    # 
    #   render_datatable(output = output, ns = ns, i18n = i18n, data = tibble::tibble(), output_name = "vocabulary_mapping_1_dt", datatable_dom = "")
    #   render_datatable(output = output, ns = ns, i18n = i18n, data = tibble::tibble(), output_name = "vocabulary_mapping_2_dt", datatable_dom = "")
    # 
    #   r$reload_vocabulary_added_mappings_datatable <- now()
    #   r$reload_vocabulary_evaluate_mappings_datatable <- now()
    #   r$reload_vocabulary_evaluate_mappings_datatable_type <- "reload_data"
    #   
    #   sapply(c("vocabulary_mapping_1", "vocabulary_mapping_2"), function(input_id) shiny.fluent::updateComboBox.shinyInput(
    #     session, input_id, options = r$vocabulary %>% convert_tibble_to_list(key_col = "vocabulary_id", text_col = "vocabulary_id"), value = NULL))
    #   
    #   sapply(c(1, 2), function(num) output[[paste0("vocabulary_mapping_selected_concept_", num)]] <- renderUI(""))
    #   
    #   shinyjs::show("br_div")
    #   sapply(c("vocabulary_datatable_selected_item_div", "save_vocabulary_concepts", "vocabulary_mapping_selected_concepts_div"), shinyjs::hide)
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$selected_dataset 1"))
    # })
    # 
    # observeEvent(r$reload_vocabulary_added_mappings_datatable, {
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "start")
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer r$reload_vocabulary_added_mappings_datatable"))
    # 
    #   centered_cols <- c("vocabulary_id_1", "concept_id_1", "relationship_id", "vocabulary_id_2", "concept_id_2")
    #   col_names <- get_col_names(table_name = "dataset_vocabulary_concepts_mapping", i18n = i18n)
    #   hidden_cols <- c("id")
    # 
    #   # Render datatable
    #   render_datatable(output = output, ns = ns, i18n = i18n, data = r$vocabulary_added_mappings, datatable_dom = "<'top't><'bottom'p>",
    #     output_name = "vocabulary_added_mappings", col_names = col_names, centered_cols = centered_cols, hidden_cols = hidden_cols)
    # 
    #   # Create a proxy for datatatable
    #   r$vocabulary_added_mappings_datatable_proxy <- DT::dataTableProxy("vocabulary_added_mappings", deferUntilFlush = FALSE)
    #   
    #   if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$reload_vocabulary_added_mappings_datatable"))
    # })
    # 
    # # --- --- --- --- - --
    # ## Manage mapping ----
    # # --- --- --- --- - --
    # 
    # # Reload datatable  
    # 
    # observeEvent(r$reload_vocabulary_evaluate_mappings_datatable, {
    # 
    #   if (perf_monitoring) monitor_perf(r = r, action = "start")
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer r$reload_vocabulary_evaluate_mappings_datatable"))
    # 
    #   action_button_visibility <- list()
    #   for (action in c("evaluate", "delete")){
    #     if (paste0("vocabularies_", action, "_mapping") %in% r$user_accesses) action_button_visibility[[action]] <- "visible"
    #     else action_button_visibility[[action]] <- "hidden"
    #   }
    #   
    #   # Get all concepts mappings
    #   
    #   if (r$reload_vocabulary_evaluate_mappings_datatable_type %in% c("reload_data", "filter_data")){
    # 
    #     # data_source <- r$datasets %>% dplyr::filter(id == r$selected_dataset) %>% dplyr::pull(data_source_id)
    # 
    #     # vocabulary_ids <- r$vocabulary %>%
    #     #   dplyr::filter(
    #     #     grepl(paste0("^", data_source, "$"), data_source_id) |
    #     #       grepl(paste0(", ", data_source, "$"), data_source_id) |
    #     #       grepl(paste0("^", data_source, ","), data_source_id) |
    #     #       grepl(paste0(", ", data_source, ","), data_source_id)
    #     #   ) %>% dplyr::pull(vocabulary_id)
    # 
    #     sql <- glue::glue_sql(paste0("SELECT cr.id AS concept_relationship_id, ",
    #       "c1.vocabulary_id AS vocabulary_id_1, cr.concept_id_1, c1.concept_name AS concept_name_1, cr.relationship_id, ",
    #       "c2.vocabulary_id AS vocabulary_id_2, cr.concept_id_2, c2.concept_name AS concept_name_2, cru.comment, cru.creator_id, cru.datetime ",
    #       "FROM concept_relationship_user cru ",
    #       "INNER JOIN concept_relationship cr ON cru.concept_relationship_id = cr.id ",
    #       "INNER JOIN concept c1 ON cr.concept_id_1 = c1.concept_id ",
    #       "INNER JOIN concept c2 ON cr.concept_id_2 = c2.concept_id"), .con = m$db)
    #       # "INNER JOIN concept c1 ON cr.concept_id_1 = c1.concept_id AND c1.vocabulary_id IN ({vocabulary_ids*}) ",
    #       # "INNER JOIN concept c2 ON cr.concept_id_2 = c2.concept_id AND c2.vocabulary_id IN ({vocabulary_ids*})"), .con = m$db)
    #     r$dataset_vocabulary_concepts_evaluate_mappings <- DBI::dbGetQuery(m$db, sql)
    # 
    #     # Join with evaluations
    # 
    #     sql <- glue::glue_sql(paste0("SELECT * FROM concept_relationship_evals ",
    #     " WHERE concept_relationship_id IN ({r$dataset_vocabulary_concepts_evaluate_mappings$concept_relationship_id*})"), .con = m$db)
    #     vocabulary_mapping_evals <- DBI::dbGetQuery(m$db, sql) %>% tibble::as_tibble() %>% dplyr::mutate_at("evaluation_id", as.integer)
    # 
    #     r$dataset_vocabulary_concepts_evaluate_mappings <- r$dataset_vocabulary_concepts_evaluate_mappings %>%
    #       dplyr::mutate(relationship_id = dplyr::case_when(
    #         relationship_id == "Maps to" ~ i18n$t("maps_to"),
    #         relationship_id == "Mapped from" ~ i18n$t("mapped_from"),
    #         relationship_id == "Is a" ~ i18n$t("is_a"),
    #         relationship_id == "Subsumes" ~ i18n$t("subsumes"),
    #         TRUE ~ relationship_id)) %>%
    #       dplyr::left_join(vocabulary_mapping_evals %>% dplyr::select(eval_id = id, concept_relationship_id, evaluation_id), by = "concept_relationship_id") %>%
    #       dplyr::group_by(concept_relationship_id, vocabulary_id_1, concept_id_1, concept_name_1, relationship_id, vocabulary_id_2, concept_id_2, concept_name_2, comment, creator_id, datetime) %>%
    #       dplyr::summarize(
    #         positive_evals = sum(evaluation_id == 1, na.rm = TRUE),
    #         negative_evals = sum(evaluation_id == 2, na.rm = TRUE)
    #       ) %>%
    #       dplyr::ungroup() %>%
    #       dplyr::mutate(
    #         positive_evals = ifelse(positive_evals > 0, positive_evals, 0L),
    #         negative_evals = ifelse(negative_evals > 0, negative_evals, 0L)
    #       ) %>%
    #       dplyr::mutate_at(c("concept_id_1", "concept_id_2"), as.character) %>%
    #       dplyr::left_join(r$users %>% dplyr::transmute(creator_id = id, creator_name = paste0(firstname, " ", lastname)), by = "creator_id") %>%
    #       dplyr::relocate(creator_name, .after = "creator_id") %>%
    #       dplyr::select(-creator_id) %>%
    #       dplyr::left_join(vocabulary_mapping_evals %>%
    #           dplyr::filter(creator_id == r$user_id) %>%
    #           dplyr::select(concept_relationship_id, user_evaluation_id = evaluation_id), by = "concept_relationship_id")
    # 
    #     # Create action column
    #    
    #     r$dataset_vocabulary_concepts_evaluate_mappings <- r$dataset_vocabulary_concepts_evaluate_mappings %>%
    #       dplyr::mutate(action = as.character(
    #         tagList(
    #           shiny::actionButton("positive_eval_%concept_relationship_id%", "", icon = icon("thumbs-up"),
    #             onclick = paste0("Shiny.setInputValue('", id, "-concept_mapping_evaluated_positive', this.id, {priority: 'event'})"),
    #             style = paste0("background-color:#E8E9EC; color:black; border-color:#8E8F9D; border-radius:3px; border-width:1px; visibility:", action_button_visibility$evaluate, ";")),
    #           shiny::actionButton("negative_eval_%concept_relationship_id%", "", icon = icon("thumbs-down"),
    #             onclick = paste0("Shiny.setInputValue('", id, "-concept_mapping_evaluated_negative', this.id, {priority: 'event'})"),
    #             style = paste0("background-color:#E8E9EC; color:black; border-color:#8E8F9D; border-radius:3px; border-width:1px; visibility:", action_button_visibility$evaluate, ";")),
    #           shiny::actionButton("remove_%concept_relationship_id%", "", icon = icon("trash-alt"),
    #             onclick = paste0("Shiny.setInputValue('", id, "-concept_mapping_deleted_pressed', this.id, {priority: 'event'})"),
    #             style = paste0("background-color:#E8E9EC; color:black; border-color:#8E8F9D; border-radius:3px; border-width:1px; visibility:", action_button_visibility$delete, ";"))
    #         )), .after = "negative_evals")
    #     
    #     if (nrow(r$dataset_vocabulary_concepts_evaluate_mappings) > 0) r$dataset_vocabulary_concepts_evaluate_mappings <-
    #       r$dataset_vocabulary_concepts_evaluate_mappings %>%
    #         dplyr::mutate(action = stringr::str_replace_all(action, "%concept_relationship_id%", as.character(concept_relationship_id)))
    # 
    #     # Update action buttons with user evaluations
    #     r$dataset_vocabulary_concepts_evaluate_mappings <- r$dataset_vocabulary_concepts_evaluate_mappings %>%
    #       dplyr::mutate(
    #         positive_eval_button_background_color = dplyr::case_when(
    #           user_evaluation_id == 1 ~ "#5FBAFF",
    #           user_evaluation_id == 2 ~ "#E8E9EC"
    #         ),
    #         positive_eval_button_color = dplyr::case_when(
    #           user_evaluation_id == 1 ~ "white",
    #           user_evaluation_id == 2 ~ "black"
    #         ),
    #         negative_eval_button_background_color = dplyr::case_when(
    #           user_evaluation_id == 1 ~ "#E8E9EC",
    #           user_evaluation_id == 2 ~ "#FF434C"
    #         ),
    #         negative_eval_button_color = dplyr::case_when(
    #           user_evaluation_id == 1 ~ "black",
    #           user_evaluation_id == 2 ~ "white"
    #         )
    #       ) %>%
    #       dplyr::mutate(action = dplyr::case_when(
    #         !is.na(user_evaluation_id) ~ as.character(tagList(
    #           shiny::actionButton("positive_eval_%concept_relationship_id%", "", icon = icon("thumbs-up"),
    #             onclick = paste0("Shiny.setInputValue('", id, "-concept_mapping_evaluated_positive', this.id, {priority: 'event'})"),
    #             style = paste0("background-color:", positive_eval_button_background_color, "; color:", positive_eval_button_color, "; border-color:#8E8F9D; border-radius:3px; border-width:1px; visibility:", action_button_visibility$evaluate, ";")),
    #           shiny::actionButton("negative_eval_%concept_relationship_id%", "", icon = icon("thumbs-down"),
    #             onclick = paste0("Shiny.setInputValue('", id, "-concept_mapping_evaluated_negative', this.id, {priority: 'event'})"),
    #             style = paste0("background-color:", negative_eval_button_background_color, "; color:", negative_eval_button_color, "; border-color:#8E8F9D; border-radius:3px; border-width:1px; visibility:", action_button_visibility$evaluate, ";")),
    #           shiny::actionButton("remove_%concept_relationship_id%", "", icon = icon("trash-alt"),
    #             onclick = paste0("Shiny.setInputValue('", id, "-concept_mapping_deleted_pressed', this.id, {priority: 'event'})"),
    #             style = paste0("background-color:#E8E9EC; color:black; border-color:#8E8F9D; border-radius:3px; border-width:1px; visibility:", action_button_visibility$delete, ";"))
    #         )),
    #         TRUE ~ action
    #       )) %>%
    #       dplyr::select(-positive_eval_button_background_color, -positive_eval_button_color, -negative_eval_button_background_color, -negative_eval_button_color)
    #     
    #     if (nrow(r$dataset_vocabulary_concepts_evaluate_mappings) > 0) r$dataset_vocabulary_concepts_evaluate_mappings <-
    #       r$dataset_vocabulary_concepts_evaluate_mappings %>%
    #       dplyr::mutate(action = stringr::str_replace_all(action, "%concept_relationship_id%", as.character(concept_relationship_id)))
    #   }
    #     
    #   # Select only mappings without evaluation by current user
    # 
    #   if(length(input$vocabulary_show_only_not_evaluated_concepts) > 0) if(input$vocabulary_show_only_not_evaluated_concepts) r$dataset_vocabulary_concepts_evaluate_mappings <- 
    #     r$dataset_vocabulary_concepts_evaluate_mappings %>% dplyr::filter(is.na(user_evaluation_id))
    #   
    #   # Render datatable
    # 
    #   r$dataset_vocabulary_concepts_evaluate_mappings <- r$dataset_vocabulary_concepts_evaluate_mappings %>%
    #     dplyr::arrange(dplyr::desc(concept_relationship_id)) %>% dplyr::mutate(modified = FALSE)
    # 
    #   searchable_cols <- c("concept_id_1", "concept_name_1", "concept_id_2", "concept_name_2", "relationship_id", "creator_name", "positive_evals", "negative_evals", "vocabulary_id_1", "vocabulary_id_2")
    #   # Remove factorize_cols : bug with evaluation if a filter is made
    #   # factorize_cols <- c("relationship_id", "creator_name", "vocabulary_id_1", "vocabulary_id_2")
    #   sortable_cols <- c("concept_id_1", "concept_id_2", "relationship_id", "creator_name", "datetime", "positive_evals", "negative_evals", "vocabulary_id_1", "vocabulary_id_2")
    #   centered_cols <- c("datetime", "action", "vocabulary_id_1", "concept_id_1", "vocabulary_id_2", "concept_id_2", "relationship_id", "creator_name", "positive_evals", "negative_evals")
    #   col_names <- get_col_names(table_name = "dataset_vocabulary_concepts_mapping_evals", i18n = i18n)
    #   hidden_cols <- c("concept_relationship_id", "modified", "user_evaluation_id", "comment", "creator_name", "vocabulary_id_1", "vocabulary_id_2", "concept_name_1", "concept_name_2")
    #   column_widths <- c("action" = "100px", "datetime" = "130px", "positive_evals" = "80px", "negative_evals" = "80px")
    # 
    #   selection <- "multiple"
    #   vocabulary_show_mapping_details <- FALSE
    #   if (length(input$vocabulary_show_mapping_details) > 0) if (input$vocabulary_show_mapping_details) selection <- "single"
    #   
    #   # Render datatable
    #   render_datatable(output = output, ns = ns, i18n = i18n, data = r$dataset_vocabulary_concepts_evaluate_mappings,
    #     output_name = "vocabulary_evaluate_mappings", hidden_cols = hidden_cols, centered_cols = centered_cols, searchable_cols = searchable_cols,
    #     col_names = col_names, filter = TRUE, sortable_cols = sortable_cols, column_widths = column_widths, #factorize_cols = factorize_cols,
    #     selection = selection
    #   )
    # 
    #   # Create a proxy for datatatable
    #   r$dataset_vocabulary_concepts_evaluate_mappings_datatable_proxy <- DT::dataTableProxy("vocabulary_evaluate_mappings", deferUntilFlush = FALSE)
    # 
    #   if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$reload_vocabulary_evaluate_mappings_datatable"))
    # })
    # 
    # # Reload when toggle input$vocabulary_show_only_not_evaluated_concepts is activated
    # 
    # observeEvent(input$vocabulary_show_only_not_evaluated_concepts, {
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$vocabulary_show_only_not_evaluated_concepts"))
    #   # req(length(r$selected_dataset) > 0, !is.na(r$selected_dataset))
    #   r$reload_vocabulary_evaluate_mappings_datatable <- now()
    #   r$reload_vocabulary_evaluate_mappings_datatable_type <- "filter_data"
    # })
    # 
    # # Reload when toggle input$vocabulary_show_mapping_details is activated
    # 
    # observeEvent(input$vocabulary_show_mapping_details, {
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$vocabulary_show_mapping_details"))
    #   # req(length(r$selected_dataset) > 0, !is.na(r$selected_dataset))
    #   
    #   # Reload UI outputs
    #   for(position in c("left", "center", "right")) output[[paste0("vocabulary_mapping_details_", position)]] <- renderUI("")
    #   
    #   shinyjs::show("vocabulary_mapping_details_div")
    #   
    #   r$reload_vocabulary_evaluate_mappings_datatable <- now()
    #   r$reload_vocabulary_evaluate_mappings_datatable_type <- "reload_datatable"
    # })
    # 
    # # Update which cols are hidden
    # 
    # observeEvent(input$vocabulary_mapping_eval_cols, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$vocabulary_mapping_eval_cols"))
    #   
    #   req(length(r$dataset_vocabulary_concepts_evaluate_mappings_datatable_proxy) > 0)
    #   
    #   r$dataset_vocabulary_concepts_evaluate_mappings_datatable_proxy %>%
    #     DT::showCols(1:13) %>%
    #     DT::hideCols(setdiff(1:13, input$vocabulary_mapping_eval_cols))
    # })
    # 
    # # When an evaluation button is clicked
    # 
    # observeEvent(input$concept_mapping_evaluated_positive, {
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$concept_mapping_evaluated_positive"))
    #   r$concept_mapping_evaluation_type <- "positive"
    #   r$concept_mapping_evaluation_update <- now()
    # })
    # 
    # observeEvent(input$concept_mapping_evaluated_negative, {
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$concept_mapping_evaluated_positive"))
    #   r$concept_mapping_evaluation_type <- "negative"
    #   r$concept_mapping_evaluation_update <- now()
    # })
    # 
    # observeEvent(r$concept_mapping_evaluation_update, {
    # 
    #   if (perf_monitoring) monitor_perf(r = r, action = "start")
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer r$concept_mapping_evaluation_update"))
    # 
    #   prefix <- r$concept_mapping_evaluation_type
    #   new_evaluation_id <- switch(r$concept_mapping_evaluation_type, "positive" = 1L, "negative" = 2L)
    # 
    #   link_id <- as.integer(substr(input[[paste0("concept_mapping_evaluated_", prefix)]], nchar(paste0(prefix, "_eval_")) + 1, nchar(input[[paste0("concept_mapping_evaluated_", prefix)]])))
    # 
    #   # If we cancel current evaluation
    #   current_evaluation_id <- r$dataset_vocabulary_concepts_evaluate_mappings %>% dplyr::filter(concept_relationship_id == link_id) %>% dplyr::pull(user_evaluation_id)
    # 
    #   if (!is.na(current_evaluation_id)) if ((current_evaluation_id == 1 & new_evaluation_id == 1) | (current_evaluation_id == 2 & new_evaluation_id == 2)) new_evaluation_id <- NA_integer_
    # 
    #   # Change actionButtons style
    #   if (is.na(new_evaluation_id)){
    #     positive_eval_button_style <- list(background_color = "#E8E9EC", color = "black")
    #     negative_eval_button_style <- list(background_color = "#E8E9EC", color = "black")
    #   }
    #   else if (prefix == "positive"){
    #     positive_eval_button_style <- list(background_color = "#5FBAFF", color = "white")
    #     negative_eval_button_style <- list(background_color = "#E8E9EC", color = "black")
    #   }
    #   else if (prefix == "negative"){
    #     positive_eval_button_style <- list(background_color = "#E8E9EC", color = "black")
    #     negative_eval_button_style <- list(background_color = "#FF434C", color = "white")
    #   }
    # 
    #   # Update temp variable
    #   
    #   action_button_visibility <- list()
    #   for (action in c("evaluate", "delete")){
    #     if (paste0("vocabularies_", action, "_mapping") %in% r$user_accesses) action_button_visibility[[action]] <- "visible"
    #     else action_button_visibility[[action]] <- "hidden"
    #   }
    # 
    #   r$dataset_vocabulary_concepts_evaluate_mappings <- r$dataset_vocabulary_concepts_evaluate_mappings %>%
    #     dplyr::mutate(user_evaluation_id = ifelse(
    #       concept_relationship_id == link_id, new_evaluation_id, user_evaluation_id
    #     )) %>%
    #     dplyr::mutate(
    #       positive_evals = dplyr::case_when(
    #         concept_relationship_id == link_id & is.na(current_evaluation_id) & new_evaluation_id == 1 ~ positive_evals + 1L,
    #         concept_relationship_id == link_id & current_evaluation_id == 1 & is.na(new_evaluation_id) ~ positive_evals - 1L,
    #         concept_relationship_id == link_id & current_evaluation_id == 2 & new_evaluation_id == 1 ~ positive_evals + 1L,
    #         concept_relationship_id == link_id & current_evaluation_id == 1 & new_evaluation_id == 2 ~ positive_evals - 1L,
    #         TRUE ~ positive_evals
    #       ),
    #       negative_evals = dplyr::case_when(
    #         concept_relationship_id == link_id & is.na(current_evaluation_id) & new_evaluation_id == 2 ~ negative_evals + 1L,
    #         concept_relationship_id == link_id & current_evaluation_id == 2 & is.na(new_evaluation_id) ~ negative_evals - 1L,
    #         concept_relationship_id == link_id & current_evaluation_id == 1 & new_evaluation_id == 2 ~ negative_evals + 1L,
    #         concept_relationship_id == link_id & current_evaluation_id == 2 & new_evaluation_id == 1 ~ negative_evals - 1L,
    #         TRUE ~ negative_evals
    #       )
    #     ) %>%
    #     dplyr::mutate(action = dplyr::case_when(
    #       concept_relationship_id == link_id ~ as.character(tagList(
    #         shiny::actionButton(paste0("positive_eval_", link_id), "", icon = icon("thumbs-up"),
    #           onclick = paste0("Shiny.setInputValue('", id, "-concept_mapping_evaluated_positive', this.id, {priority: 'event'})"),
    #           style = paste0("background-color:", positive_eval_button_style$background_color, "; color:", positive_eval_button_style$color, "; border-color:#8E8F9D; border-radius:3px; border-width:1px; visibility:", action_button_visibility$evaluate, ";")),
    #         shiny::actionButton(paste0("negative_eval_", link_id), "", icon = icon("thumbs-down"),
    #           onclick = paste0("Shiny.setInputValue('", id, "-concept_mapping_evaluated_negative', this.id, {priority: 'event'})"),
    #           style = paste0("background-color:", negative_eval_button_style$background_color, "; color:", negative_eval_button_style$color, "; border-color:#8E8F9D; border-radius:3px; border-width:1px; visibility: ", action_button_visibility$evaluate, ";")),
    #         shiny::actionButton(paste0("remove_", link_id), "", icon = icon("trash-alt"),
    #           onclick = paste0("Shiny.setInputValue('", id, "-concept_mapping_deleted_pressed', this.id, {priority: 'event'})"),
    #           style = paste0("background-color:#E8E9EC; color:black; border-color:#8E8F9D; border-radius:3px; border-width:1px; visibility: ", action_button_visibility$delete, ";"))
    #       )),
    #       TRUE ~ action
    #     )) %>%
    #     dplyr::mutate(modified = dplyr::case_when(
    #       concept_relationship_id == link_id ~ TRUE,
    #       TRUE ~ modified
    #     ))
    # 
    #   # Reload datatable
    #   DT::replaceData(r$dataset_vocabulary_concepts_evaluate_mappings_datatable_proxy, r$dataset_vocabulary_concepts_evaluate_mappings, resetPaging = FALSE, rownames = FALSE)
    # 
    #   if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer r$concept_mapping_evaluation_update"))
    # })
    # 
    # # Delete a row or multiple rows in datatable
    # 
    # r$mappings_open_dialog <- FALSE
    # 
    # output$mappings_delete_confirm <- shiny.fluent::renderReact({
    #   shiny.fluent::Dialog(
    #     hidden = !r$mappings_open_dialog,
    #     onDismiss = htmlwidgets::JS("function() { Shiny.setInputValue('mappings_hide_dialog', Math.random()); }"),
    #     dialogContentProps = list(
    #       type = 0,
    #       title = i18n$t("mapping_delete"),
    #       closeButtonAriaLabel = "Close",
    #       subText = tagList(i18n$t("mapping_delete_subtext"), br(), br())
    #     ),
    #     modalProps = list(),
    #     shiny.fluent::DialogFooter(
    #       shiny.fluent::PrimaryButton.shinyInput(ns("mappings_delete_confirmed"), text = i18n$t("delete")),
    #       shiny.fluent::DefaultButton.shinyInput(ns("mappings_delete_canceled"), text = i18n$t("dont_delete"))
    #     )
    #   )
    # })
    # 
    # # Whether to close or not delete dialog box
    # observeEvent(input$mappings_hide_dialog, r$mappings_open_dialog <- FALSE)
    # observeEvent(input$mappings_delete_canceled, r$mappings_open_dialog <- FALSE)
    # 
    # # When the deletion is confirmed
    # observeEvent(input$mappings_delete_confirmed, {
    #   
    #   r$mappings_open_dialog <- FALSE
    #   
    #   # Delete mappings from concept_relationship_user AND concept_relationship
    #   sql <- glue::glue_sql("DELETE FROM concept_relationship_user WHERE concept_relationship_id IN ({r$delete_mappings*})", .con = m$db)
    #   DBI::dbSendStatement(m$db, sql) -> query
    #   DBI::dbClearResult(query)
    #   
    #   sql <- glue::glue_sql("DELETE FROM concept_relationship WHERE id IN ({r$delete_mappings*})", .con = m$db)
    #   DBI::dbSendStatement(m$db, sql) -> query
    #   DBI::dbClearResult(query)
    #   
    #   # Delete also evaluations
    #   sql <- glue::glue_sql("DELETE FROM concept_relationship_evals WHERE concept_relationship_id IN ({r$delete_mappings*})", .con = m$db)
    #   DBI::dbSendStatement(m$db, sql) -> query
    #   DBI::dbClearResult(query)
    #   r$dataset_vocabulary_concepts_evaluate_mappings <- r$dataset_vocabulary_concepts_evaluate_mappings %>%
    #     dplyr::filter(concept_relationship_id %not_in% r$delete_mappings)
    #   
    #   r$reload_mappings_evals <- now()
    #   
    #   show_message_bar(output, "mapping_deleted", type = "warning", i18n = i18n, ns = ns)
    # })
    # 
    # # Delete one row (with icon on DT)
    # 
    # observeEvent(input$concept_mapping_deleted_pressed, {
    # 
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$concept_mapping_deleted_pressed"))
    # 
    #   r$delete_mappings <- as.integer(substr(input$concept_mapping_deleted_pressed, nchar("delete_") + 1, 100))
    #   r$mappings_open_dialog <- TRUE
    #   
    #   # Reload datatable (to unselect rows)
    #   DT::replaceData(r$dataset_vocabulary_concepts_evaluate_mappings_datatable_proxy, r$dataset_vocabulary_concepts_evaluate_mappings, resetPaging = FALSE, rownames = FALSE)
    # })
    # 
    # # Delete multiple rows (with "Delete selection" button)
    # 
    # observeEvent(input$mapping_delete_selection, {
    # 
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$mapping_delete_selection"))
    # 
    #   req(length(input$vocabulary_evaluate_mappings_rows_selected) > 0)
    # 
    #   r$delete_mappings <- r$dataset_vocabulary_concepts_evaluate_mappings[input$vocabulary_evaluate_mappings_rows_selected, ] %>% dplyr::pull(concept_relationship_id)
    #   r$mappings_open_dialog <- TRUE
    # })
    # 
    # # Reload data
    # 
    # observeEvent(r$reload_mappings_evals, {
    # 
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer r$reload_mappings_evals"))
    # 
    #   # Reload datatable
    #   DT::replaceData(r$dataset_vocabulary_concepts_evaluate_mappings_datatable_proxy, r$dataset_vocabulary_concepts_evaluate_mappings, resetPaging = FALSE, rownames = FALSE)
    # })
    # 
    # # Save updates
    # 
    # observeEvent(input$save_mappings_evaluation, {
    # 
    #   if (perf_monitoring) monitor_perf(r = r, action = "start")
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$save_mappings_evaluation"))
    # 
    #   # Update database
    # 
    #   if (nrow(r$dataset_vocabulary_concepts_evaluate_mappings %>% dplyr::filter(modified)) == 0) show_message_bar(output,  "modif_saved", "success", i18n = i18n, ns = ns)
    # 
    #   req(nrow(r$dataset_vocabulary_concepts_evaluate_mappings %>% dplyr::filter(modified)) > 0)
    # 
    #   sql <- glue::glue_sql(paste0("DELETE FROM concept_relationship_evals WHERE creator_id = {r$user_id} ",
    #     "AND concept_relationship_id IN ({r$dataset_vocabulary_concepts_evaluate_mappings %>% dplyr::filter(modified) %>% dplyr::pull(concept_relationship_id)*})"), .con = m$db)
    #   query <- DBI::dbSendStatement(m$db, sql)
    #   DBI::dbClearResult(query)
    # 
    #   if (nrow(r$dataset_vocabulary_concepts_evaluate_mappings %>% dplyr::filter(modified, !is.na(user_evaluation_id))) > 0){
    #     new_data <- r$dataset_vocabulary_concepts_evaluate_mappings %>%
    #       dplyr::filter(modified, !is.na(user_evaluation_id)) %>%
    #       dplyr::transmute(concept_relationship_id, creator_id = r$user_id, evaluation_id = user_evaluation_id, datetime = now()) %>%
    #       dplyr::mutate(id = get_last_row(m$db, "concept_relationship_evals") + 1:dplyr::n(), .before = "concept_relationship_id")
    # 
    #     DBI::dbAppendTable(m$db, "concept_relationship_evals", new_data)
    #   }
    # 
    #   show_message_bar(output, "modif_saved", "success", i18n = i18n, ns = ns)
    # 
    #   if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_vocabularies - observer input$save_mappings_evaluation"))
    # })
    # 
    # # When a row is selected
    # observeEvent(input$vocabulary_evaluate_mappings_rows_selected, {
    #   if (debug) cat(paste0("\n", now(), " - mod_vocabularies - observer input$vocabulary_evaluate_mappings_rows_selected"))
    #   
    #   req(input$vocabulary_show_mapping_details)
    #   
    #   last_row_selected <- input$vocabulary_evaluate_mappings_rows_selected[length(input$vocabulary_evaluate_mappings_rows_selected)]
    #   selected_row <- r$dataset_vocabulary_concepts_evaluate_mappings[last_row_selected, ]
    #   
    #   sql <- glue::glue_sql(paste0("SELECT concept_id, COUNT(DISTINCT(dataset_id)) AS count_datasets, ",
    #     # "SUM(count_concepts_rows) + SUM(count_secondary_concepts_rows) AS count_concepts_rows ",
    #     "SUM(count_concepts_rows) AS count_concepts_rows ",
    #     "FROM concept_dataset ",
    #     "WHERE concept_id IN ({c(selected_row$concept_id_1, selected_row$concept_id_2)*})",
    #     "GROUP BY concept_id "), .con = m$db)
    #   concept_dataset <- DBI::dbGetQuery(m$db, sql)
    #   
    #   sql <- glue::glue_sql("SELECT * FROM concept WHERE concept_id = {selected_row$concept_id_1}", .con = m$db)
    #   concept_1 <- DBI::dbGetQuery(m$db, sql) %>%
    #     dplyr::left_join(concept_dataset %>% dplyr::select(concept_id, count_datasets, count_concepts_rows), by = "concept_id")
    #   
    #   if (length(d$dataset_all_concepts) > 0) if (nrow(d$dataset_all_concepts) > 0) concept_1 <- concept_1 %>%
    #     # Sum counts if this concepts has been mapped to multiple concepts
    #     dplyr::left_join(d$dataset_all_concepts %>% 
    #         dplyr::transmute(concept_id = concept_id_1, count_concepts_rows_current_dataset = count_concepts_rows) %>%
    #         # dplyr::transmute(concept_id = concept_id_1, count_concepts_rows_current_dataset = count_concepts_rows + count_secondary_concepts_rows) %>%
    #         dplyr::group_by(concept_id) %>%
    #         dplyr::summarize(count_concepts_rows_current_dataset = sum(count_concepts_rows_current_dataset)) %>%
    #         dplyr::ungroup(), by = "concept_id")
    #   if ("count_concepts_rows_current_dataset" %not_in% names(concept_1)) concept_1 <- concept_1 %>%
    #     dplyr::mutate(count_concepts_rows_current_dataset = 0L)
    #     
    #   concept_1 <- 
    #     concept_1 %>%
    #     dplyr::mutate(
    #       count_datasets = ifelse(is.na(count_datasets), 0L, count_datasets),
    #       count_concepts_rows = ifelse(is.na(count_concepts_rows), 0L, count_concepts_rows),
    #       count_concepts_rows_current_dataset = ifelse(is.na(count_concepts_rows_current_dataset), 0L, count_concepts_rows_current_dataset)
    #     )
    #   
    #   # Left panel
    #   output$vocabulary_mapping_details_left <- renderUI(tagList(
    #     strong(i18n$t("vocabulary_id_1")), " : ", selected_row$vocabulary_id_1, br(),
    #     strong(i18n$t("concept_id_1")), " : ", selected_row$concept_id_1, br(),
    #     strong(i18n$t("concept_name_1")), " : ", concept_1$concept_name, br(),
    #     strong(i18n$t("domain_id")), " : ", concept_1$domain_id, br(),
    #     strong(i18n$t("standard_concept")), " : ", concept_1$standard_concept, br(),
    #     strong(i18n$t("concept_code")), " : ", concept_1$concept_code, br(), br(),
    #     strong(i18n$t("num_datasets")), " : ", concept_1$count_datasets, br(),
    #     strong(i18n$t("num_rows_with_mapping")), " : ", concept_1$count_concepts_rows, br(),
    #     strong(i18n$t("num_rows_current_dataset")), " : ", concept_1$count_concepts_rows_current_dataset, br(),
    #   ))
    #   
    #   # Middle panel
    #   output$vocabulary_mapping_details_center <- renderUI(tagList(
    #     strong(i18n$t("relationship_id")), " : ", selected_row$relationship_id, br(), br(),
    #     strong(i18n$t("creator")), " : ", selected_row$creator_name, br(),
    #     strong(i18n$t("datetime")), " : ", selected_row$datetime, br(), br(),
    #     strong(i18n$t("comment")), " : ", selected_row$comment, br(), br(),
    #     strong(i18n$t("positive_evals")), " : ", selected_row$positive_evals, br(),
    #     strong(i18n$t("negative_evals")), " : ", selected_row$negative_evals
    #   ))
    #   
    #   sql <- glue::glue_sql("SELECT * FROM concept WHERE concept_id = {selected_row$concept_id_2}", .con = m$db)
    #   concept_2 <- DBI::dbGetQuery(m$db, sql) %>%
    #     dplyr::left_join(concept_dataset %>% dplyr::select(concept_id, count_datasets, count_concepts_rows), by = "concept_id")
    #     
    #   if (length(d$dataset_all_concepts) > 0) if (nrow(d$dataset_all_concepts) > 0) concept_2 <- concept_2 %>%
    #     dplyr::left_join(d$dataset_all_concepts %>% 
    #       dplyr::transmute(concept_id = concept_id_1, count_concepts_rows_current_dataset = count_concepts_rows) %>%
    #       # dplyr::transmute(concept_id = concept_id_1, count_concepts_rows_current_dataset = count_concepts_rows + count_secondary_concepts_rows) %>%
    #       dplyr::group_by(concept_id) %>%
    #       dplyr::summarize(count_concepts_rows_current_dataset = sum(count_concepts_rows_current_dataset)) %>%
    #       dplyr::ungroup(), by = "concept_id")
    #   if ("count_concepts_rows_current_dataset" %not_in% names(concept_2)) concept_2 <- concept_2 %>%
    #     dplyr::mutate(count_concepts_rows_current_dataset = 0L)
    #   
    #   concept_2 <-
    #     concept_2 %>%
    #     dplyr::mutate(
    #       count_datasets = ifelse(is.na(count_datasets), 0L, count_datasets),
    #       count_concepts_rows = ifelse(is.na(count_concepts_rows), 0L, count_concepts_rows),
    #       count_concepts_rows_current_dataset = ifelse(is.na(count_concepts_rows_current_dataset), 0L, count_concepts_rows_current_dataset)
    #     )
    #   
    #   # Right panel
    #   output$vocabulary_mapping_details_right <- renderUI(tagList(
    #     strong(i18n$t("vocabulary_id_2")), " : ", selected_row$vocabulary_id_2, br(),
    #     strong(i18n$t("concept_id_2")), " : ", selected_row$concept_id_2, br(),
    #     strong(i18n$t("concept_name_2")), " : ", concept_2$concept_name, br(),
    #     strong(i18n$t("domain_id")), " : ", concept_2$domain_id, br(),
    #     strong(i18n$t("standard_concept")), " : ", concept_2$standard_concept, br(),
    #     strong(i18n$t("concept_code")), " : ", concept_2$concept_code, br(), br(),
    #     strong(i18n$t("num_datasets")), " : ", concept_2$count_datasets, br(),
    #     strong(i18n$t("num_rows_with_mapping")), " : ", concept_2$count_concepts_rows, br(),
    #     strong(i18n$t("num_rows_current_dataset")), " : ", concept_2$count_concepts_rows_current_dataset, br(),
    #   ))
    # })
  })
}
