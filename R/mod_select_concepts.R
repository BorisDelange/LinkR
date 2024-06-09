#' @noRd 
mod_select_concepts_server <- function(id, r, d, m, language, i18n, debug){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # |-------------------------------- -----
    
    # Initiate vars ----
    
    # ...
    
    # # Load vocabularies attached to this dataset
    # observeEvent(r$dataset_vocabularies, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer r$dataset_vocabularies"))
    #   
    #   if (nrow(r$dataset_vocabularies) == 0) vocabulary_options = list()
    #   if (nrow(r$dataset_vocabularies) > 0) vocabulary_options <- convert_tibble_to_list(data = r$dataset_vocabularies, key_col = "vocabulary_id", text_col = "vocabulary_id", i18n = i18n)
    #   
    #   for(name in c("widget_creation_vocabulary", "widget_settings_vocabulary")) shiny.fluent::updateComboBox.shinyInput(
    #     session, name, options = vocabulary_options, value = NULL)
    # })
    # 
    # # Reload vocabulary concepts
    # 
    # observeEvent(input$widget_creation_vocabulary, {
    #   if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$widget_creation_vocabulary"))
    #   r$data_reload_widget_vocabulary_concepts <- now()
    #   r$data_reload_widget_vocabulary_concepts_type <- "widget_creation"
    # })
    # 
    # # observeEvent(input$widget_settings_vocabulary, {
    # #   if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$widget_settings_vocabulary"))
    # #   r$data_reload_widget_vocabulary_concepts <- now()
    # #   r$data_reload_widget_vocabulary_concepts_type <- "widget_settings"
    # # })
    # 
    # observeEvent(r$data_reload_widget_vocabulary_concepts, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer r$..reload_widget_vocabulary_concepts"))
    #   
    #   req(length(d$dataset_all_concepts) > 0, nrow(d$dataset_all_concepts) > 0)
    #   
    #   type <- r$data_reload_widget_vocabulary_concepts_type
    #   
    #   vocabulary_id <- input[[paste0(type, "_vocabulary")]]$key
    #   req(length(vocabulary_id) > 0)
    #   
    #   widget_vocabulary_concepts <- d$dataset_all_concepts %>%
    #     dplyr::filter(vocabulary_id_1 == vocabulary_id) %>%
    #     dplyr::select(concept_id = concept_id_1, concept_name = concept_name_1, concept_display_name = concept_display_name_1,
    #                   relationship_id, domain_id, concept_class_id, standard_concept, concept_code,
    #                   count_persons_rows, count_concepts_rows, add_concept_input)
    #   
    #   widget_vocabulary_concepts <- widget_vocabulary_concepts %>% dplyr::filter(is.na(relationship_id))
    #   
    #   widget_vocabulary_concepts <-
    #     widget_vocabulary_concepts %>%
    #     dplyr::select(-relationship_id) %>%
    #     dplyr::mutate_at(c("add_concept_input"), stringr::str_replace_all, "%ns%", id) %>%
    #     dplyr::mutate_at("add_concept_input", stringr::str_replace_all, "%input_prefix%", paste0(type, "_add_concept")) %>%
    #     dplyr::mutate_at("add_concept_input", stringr::str_replace_all, "%input_prefix_2%", paste0(type, "_")) %>%
    #     dplyr::mutate_at("concept_id", as.character) %>%
    #     dplyr::mutate(add_concept_input = stringr::str_replace_all(add_concept_input, "%concept_id_1%", concept_id))
    #   
    #   r[[paste0("data_", type, "_vocabulary_concepts")]] <- widget_vocabulary_concepts
    #   
    #   if (length(r[[paste0("widget_", type, "_vocabulary_concepts_proxy")]]) == 0){
    #     editable_cols <- c("concept_display_name")
    #     searchable_cols <- c("concept_id", "concept_name", "concept_display_name")
    #     column_widths <- c("concept_id" = "80px", "action" = "80px")
    #     sortable_cols <- c("concept_id", "concept_name", "concept_display_name", "count_persons_rows", "count_concepts_rows")
    #     centered_cols <- c("concept_id", "count_persons_rows", "count_concepts_rows", "add_concept_input")
    #     col_names <- get_col_names("plugins_vocabulary_concepts_with_counts", i18n)
    #     hidden_cols <- c("domain_id", "concept_class_id", "standard_concept", "concept_code")
    #     column_widths <- c("concept_id" = "120px", "count_persons_rows" = "80px", "count_concepts_rows" = "80px", "add_concept_input" = "80px")
    #     
    #     # Render datatable
    #     render_datatable(output = output, ns = ns, i18n = i18n, data = widget_vocabulary_concepts,
    #                      output_name = paste0(type, "_vocabulary_concepts"), col_names =  col_names,
    #                      editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
    #                      searchable_cols = searchable_cols, filter = TRUE, hidden_col = hidden_cols)
    #     
    #     # Create a proxy for datatatable
    #     r[[paste0("data_", type, "_vocabulary_concepts_proxy")]] <- DT::dataTableProxy(paste0(type, "_vocabulary_concepts"), deferUntilFlush = FALSE)
    #   }
    #   else DT::replaceData(r[[paste0("data_", type, "_vocabulary_concepts_proxy")]], r[[paste0("data_", type, "_vocabulary_concepts")]], resetPaging = FALSE, rownames = FALSE)
    # })
    # 
    # # Update which cols are hidden
    # 
    # observeEvent(input$widget_creation_vocabulary_concepts_table_cols, {
    #   if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$widget_creation_vocabulary_concepts_table_cols"))
    #   
    #   req(length(r$data_widget_creation_vocabulary_concepts_proxy) > 0)
    #   
    #   r$data_widget_creation_vocabulary_concepts_proxy %>%
    #     DT::showCols(0:9) %>%
    #     DT::hideCols(setdiff(0:9, input$widget_creation_vocabulary_concepts_table_cols))
    # })
    # 
    # # observeEvent(input$widget_settings_vocabulary_concepts_table_cols, {
    # #   if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$widget_settings_vocabulary_concepts_table_cols"))
    # #   
    # #   req(length(r[[paste0(category, "_widget_settings_vocabulary_concepts_proxy")]]) > 0)
    # #   
    # #   r[[paste0(category, "_widget_settings_vocabulary_concepts_proxy")]] %>%
    # #     DT::showCols(0:9) %>%
    # #     DT::hideCols(setdiff(0:9, input$widget_settings_vocabulary_concepts_table_cols))
    # # })
    # 
    # observeEvent(input$widget_creation_vocabulary_mapped_concepts_table_cols, {
    #   if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$widget_creation_vocabulary_mapped_concepts_table_cols"))
    #   
    #   req(length(r$data_widget_creation_vocabulary_mapped_concepts_proxy) > 0)
    #   
    #   r$data_widget_creation_vocabulary_mapped_concepts_proxy %>%
    #     DT::showCols(0:8) %>%
    #     DT::hideCols(setdiff(0:8, input$widget_creation_vocabulary_mapped_concepts_table_cols))
    # })
    # 
    # # observeEvent(input$widget_settings_vocabulary_mapped_concepts_table_cols, {
    # #   if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$widget_settings_vocabulary_mapped_concepts_table_cols"))
    # #   
    # #   req(length(r[[paste0(category, "_widget_settings_vocabulary_mapped_concepts_proxy")]]) > 0)
    # #   
    # #   r[[paste0(category, "_widget_settings_vocabulary_mapped_concepts_proxy")]] %>%
    # #     DT::showCols(0:8) %>%
    # #     DT::hideCols(setdiff(0:8, input$widget_settings_vocabulary_mapped_concepts_table_cols))
    # # })
    # 
    # # Updates in datatable
    # 
    # observeEvent(input$widget_creation_vocabulary_concepts_cell_edit, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$widget_creation_vocabulary_concepts_cell_edit"))
    #   
    #   edit_info <- input$widget_creation_vocabulary_concepts_cell_edit
    #   r$data_widget_creation_vocabulary_concepts <- DT::editData(r$data_widget_creation_vocabulary_concepts, edit_info, rownames = FALSE)
    # })
    # 
    # # observeEvent(input$widget_creation_vocabulary_mapped_concepts_cell_edit, {
    # #   
    # #   if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$widget_creation_vocabulary_mapped_concepts_cell_edit"))
    # #   
    # #   edit_info <- input$widget_creation_vocabulary_mapped_concepts_cell_edit
    # #   r[[paste0(category, "_widget_creation_vocabulary_mapped_concepts")]] <- DT::editData(r[[paste0(category, "_widget_creation_vocabulary_mapped_concepts")]], edit_info, rownames = FALSE)
    # # })
    # 
    # observeEvent(input$widget_settings_vocabulary_concepts_cell_edit, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$widget_settings_vocabulary_concepts_cell_edit"))
    #   
    #   edit_info <- input$widget_settings_vocabulary_concepts_cell_edit
    #   r$data_widget_settings_vocabulary_concepts <- DT::editData(r$data_widget_settings_vocabulary_concepts, edit_info, rownames = FALSE)
    # })
    # 
    # # observeEvent(input$widget_settings_vocabulary_mapped_concepts_cell_edit, {
    # #   
    # #   if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$widget_settings_vocabulary_mapped_concepts_cell_edit"))
    # #   
    # #   edit_info <- input$widget_settings_vocabulary_mapped_concepts_cell_edit
    # #   r[[paste0(category, "_widget_settings_vocabulary_mapped_concepts")]] <- DT::editData(r[[paste0(category, "_widget_settings_vocabulary_mapped_concepts")]], edit_info, rownames = FALSE)
    # # })
    # 
    # # Add a concept to selected concepts
    # 
    # observeEvent(input$widget_creation_concept_selected, {
    #   if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$widget_creation_concept_selected"))
    #   r$data_widget_concept_selected <- now()
    #   r$data_widget_concept_selected_type <- "widget_creation"
    # })
    # 
    # # observeEvent(input$widget_settings_concept_selected, {
    # #   if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$widget_settings_concept_selected"))
    # #   r$data_widget_concept_selected <- now()
    # #   r$data_widget_concept_selected_type <- "widget_settings"
    # # })
    # 
    # observeEvent(r$data_widget_concept_selected, {
    #   if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer r$..widget_concept_selected"))
    #   
    #   type <- r$data_widget_concept_selected_type
    #   
    #   # Initiate r variable if doesn't exist
    #   if (length(r[[paste0("data_", type, "_vocabulary_selected_concepts")]]) == 0) r[[paste0("data_", type, "_vocabulary_selected_concepts")]] <- tibble::tibble(
    #     concept_id = integer(), concept_name = character(), concept_display_name = character(), domain_id = character(),
    #     mapped_to_concept_id = integer(), merge_mapped_concepts = logical())
    #   
    #   # if (grepl("mapped", input[[paste0(type, "_concept_selected")]])) sub_type <- "mapped_concept"
    #   # else sub_type <- "concept"
    #   
    #   # Get ID of selected concept
    #   # link_id <- as.integer(substr(input[[paste0(type, "_concept_selected")]], nchar(paste0(id, "-", type, "_add_", sub_type, "_")) + 1, nchar(input[[paste0(type, "_concept_selected")]])))
    #   # concept_id <- as.integer(substr(input[[paste0(type, "_concept_selected")]], nchar(paste0(id, "-", type, "_add_", sub_type, "_")) + 1, nchar(input[[paste0(type, "_concept_selected")]])))
    #   concept_id <- as.integer(substr(input[[paste0(type, "_concept_selected")]], nchar(paste0(id, "-", type, "_add_concept_")) + 1, nchar(input[[paste0(type, "_concept_selected")]])))
    #   # concept_id <- link_id
    #   # if (sub_type == "concept") concept_id <- link_id
    #   # if (sub_type == "mapped_concept") concept_id <- r[[paste0(category, "_", type, "_vocabulary_mapped_concepts")]] %>% 
    #   #   dplyr::filter(id == link_id) %>% dplyr::pull(concept_id_2)
    #   
    #   # If this concept is not already selected, add it to the vocabulary_selected_concepts dropdown
    #   
    #   if (concept_id %not_in% r[[paste0("data_", type, "_vocabulary_selected_concepts")]]$concept_id){
    #     
    #     # if (sub_type == "concept") 
    #     new_data <- r[[paste0("data_", type, "_vocabulary_concepts")]] %>%
    #       dplyr::mutate_at("concept_id", as.integer) %>%
    #       dplyr::filter(concept_id == !!concept_id) %>%
    #       dplyr::transmute(concept_id, concept_name, concept_display_name, domain_id, mapped_to_concept_id = NA_integer_, merge_mapped_concepts = FALSE)
    #     
    #     # if (sub_type == "mapped_concept"){
    #     #   
    #     #   selected_concept <- r[[paste0(category, "_", type, "_vocabulary_mapped_concepts")]] %>%
    #     #     dplyr::mutate_at(c("concept_id_1", "concept_id_2"), as.integer) %>%
    #     #     dplyr::filter(id == link_id)
    #     #   
    #     #   new_data <- r[[paste0("data_", type, "_vocabulary_concepts")]] %>%
    #     #     dplyr::mutate_at("concept_id", as.integer) %>%
    #     #     dplyr::filter(concept_id == selected_concept$concept_id_1) %>%
    #     #     dplyr::transmute(concept_id, concept_name, concept_display_name, domain_id, 
    #     #                      mapped_to_concept_id = NA_integer_, merge_mapped_concepts = input[[paste0(type, "_merge_mapped_concepts")]]) %>%
    #     #     dplyr::bind_rows(
    #     #       selected_concept %>%
    #     #         dplyr::transmute(concept_id = concept_id_2, concept_name = concept_name_2, concept_display_name = concept_display_name_2, domain_id,
    #     #                          mapped_to_concept_id = concept_id_1, merge_mapped_concepts = input[[paste0(type, "_merge_mapped_concepts")]])
    #     #     ) %>%
    #     #     dplyr::bind_rows(
    #     #       r[[paste0("data_", type, "_vocabulary_selected_concepts")]] %>% 
    #     #         dplyr::filter(mapped_to_concept_id == selected_concept$concept_id_1) %>%
    #     #         dplyr::mutate(merge_mapped_concepts = input[[paste0(type, "_merge_mapped_concepts")]])
    #     #     )
    #     #   
    #     #   # Add also original concept, which concepts are mapped from
    #     #   r[[paste0("data_", type, "_vocabulary_selected_concepts")]] <- r[[paste0("data_", type, "_vocabulary_selected_concepts")]] %>% 
    #     #     dplyr::filter(concept_id != selected_concept$concept_id_1, (is.na(mapped_to_concept_id) | mapped_to_concept_id != selected_concept$concept_id_1))
    #     # }
    #     
    #     r[[paste0("data_", type, "_vocabulary_selected_concepts")]] <- new_data %>% dplyr::bind_rows(r[[paste0("data_", type, "_vocabulary_selected_concepts")]])
    #   }
    #   
    #   # Update dropdown of selected concepts
    #   
    #   r$data_widget_vocabulary_update_selected_concepts_dropdown <- now()
    #   r$data_widget_vocabulary_update_selected_concepts_dropdown_type <- type
    #   
    # })
    # 
    # # When reset button is clicked
    # observeEvent(input$widget_creation_reset_vocabulary_concepts, {
    #   if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$reset_vocabulary_concepts"))
    #   
    #   r$data_widget_creation_vocabulary_selected_concepts <- r$data_widget_creation_vocabulary_selected_concepts %>% dplyr::slice(0)
    #   r$data_widget_vocabulary_selected_concepts_trigger <- now()
    #   r$data_widget_vocabulary_selected_concepts_trigger_type <- "widget_creation"
    # })
    # 
    # # observeEvent(input$widget_settings_reset_vocabulary_concepts, {
    # #   if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$reset_vocabulary_concepts"))
    # #   
    # #   r[[paste0(category, "_widget_settings_vocabulary_selected_concepts")]] <- r[[paste0(category, "_widget_settings_vocabulary_selected_concepts")]] %>% dplyr::slice(0)
    # #   r$data_widget_vocabulary_selected_concepts_trigger <- now()
    # #   r$data_widget_vocabulary_selected_concepts_trigger_type <- "widget_settings"
    # # })
    # 
    # # When dropdown is modified
    # observeEvent(input$widget_creation_vocabulary_selected_concepts_trigger, {
    #   if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$widget_creation_vocabulary_selected_concepts_trigger"))
    #   r$data_widget_vocabulary_selected_concepts_trigger <- now()
    #   r$data_widget_vocabulary_selected_concepts_trigger_type <- "widget_creation"
    # })
    # # observeEvent(input$widget_settings_vocabulary_selected_concepts_trigger, {
    # #   if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$widget_settings_vocabulary_selected_concepts_trigger"))
    # #   r$data_widget_vocabulary_selected_concepts_trigger <- now()
    # #   r$data_widget_vocabulary_selected_concepts_trigger_type <- "widget_settings"
    # # })
    # 
    # observeEvent(r$data_widget_vocabulary_selected_concepts_trigger, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer r$..widget_vocabulary_selected_concepts_trigger"))
    #   
    #   type <- r$data_widget_vocabulary_selected_concepts_trigger_type
    #   
    #   if (length(input[[paste0(type, "_vocabulary_selected_concepts")]]) == 0) r[[paste0("data_", type, "_vocabulary_selected_concepts")]] <- r[[paste0("data_", type, "_vocabulary_selected_concepts")]] %>% dplyr::slice(0)
    #   if (length(input[[paste0(type, "_vocabulary_selected_concepts")]]) > 0) {
    #     r[[paste0("data_", type, "_vocabulary_selected_concepts")]] <- r[[paste0("data_", type, "_vocabulary_selected_concepts")]] %>%
    #       dplyr::filter(concept_id %in% input[[paste0(type, "_vocabulary_selected_concepts")]])
    #     
    #     # Delete also mapped concepts
    #     r[[paste0("data_", type, "_vocabulary_selected_concepts")]] <- r[[paste0("data_", type, "_vocabulary_selected_concepts")]] %>%
    #       dplyr::filter(is.na(mapped_to_concept_id) | mapped_to_concept_id %in% r[[paste0("data_", type, "_vocabulary_selected_concepts")]]$concept_id)
    #   }
    #   
    #   r$data_widget_vocabulary_update_selected_concepts_dropdown <- now()
    #   r$data_widget_vocabulary_update_selected_concepts_dropdown_type <- type
    # })
    # 
    # # Update dropdown of selected concepts
    # observeEvent(r$data_widget_vocabulary_update_selected_concepts_dropdown, {
    #   
    #   if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer r$..widget_vocabulary_update_selected_concepts_dropdown"))
    #   
    #   type <- r$data_widget_vocabulary_update_selected_concepts_dropdown_type
    #   
    #   options <- convert_tibble_to_list(
    #     r[[paste0("data_", type, "_vocabulary_selected_concepts")]] %>%
    #       dplyr::mutate(concept_name = ifelse(!is.na(mapped_to_concept_id), paste0("--- ", concept_name), concept_name)), 
    #     key_col = "concept_id", text_col = "concept_name", i18n = i18n)
    #   value <- r[[paste0("data_", type, "_vocabulary_selected_concepts")]] %>% dplyr::pull(concept_id)
    #   shiny.fluent::updateDropdown.shinyInput(
    #     session, paste0(type, "_vocabulary_selected_concepts"),
    #     options = options, value = value, multiSelect = TRUE, multiSelectDelimiter = " || "
    #   )
    # })
  })
}