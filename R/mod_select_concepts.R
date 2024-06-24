#' @noRd 
mod_select_concepts_ui <- function(id, language, languages, i18n){
  ns <- NS(id)
  
  tagList(
    
    # Select concepts modal ----
    shinyjs::hidden(
      div(
        id = ns("select_concepts_modal"),
        div(
          div(
            tags$h1(i18n$t("select_concepts")),
            shiny.fluent::IconButton.shinyInput(ns("close_select_concepts_modal_1"), iconProps = list(iconName = "ChromeClose")),
            class = "select_concepts_modal_head small_close_button"
          ),
          div(
            div(
              div(
                div(
                  make_combobox(i18n, ns, id = "vocabulary", label = "vocabulary", allowFreeform = FALSE, multiSelect = FALSE, width = "200px"),
                  make_dropdown(i18n, ns, id = "vocabulary_dt_cols", label = "columns", multiSelect = TRUE,
                    options = list(
                      list(key = 0, text = i18n$t("concept_id")),
                      list(key = 1, text = i18n$t("concept_name")),
                      list(key = 2, text = i18n$t("domain_id")),
                      list(key = 3, text = i18n$t("vocabulary_id")),
                      list(key = 4, text = i18n$t("concept_class_id")),
                      list(key = 5, text = i18n$t("standard_concept")),
                      list(key = 6, text = i18n$t("concept_code")),
                      list(key = 7, text = i18n$t("num_patients")),
                      list(key = 8, text = i18n$t("num_rows")),
                      list(key = 9, text = i18n$t("action"))
                    ),
                    value = c(0, 1, 2, 7, 8, 9),
                    width = "200px"
                  ),
                  div(
                    create_hover_card(
                      ui = shiny.fluent::IconButton.shinyInput(ns("reload_dataset_concepts"), iconProps = list(iconName ="SyncOccurence")), 
                      text = i18n$t("reload_dataset_concepts")
                    ),
                    style = "margin-top: 17px;",
                    class = "small_icon_button"
                  ),
                  style = "display: flex; gap: 10px;"
                ),
                DT::DTOutput(ns("vocabulary_concepts")),
                class = "widget",
                style = "height: auto; padding: 10px"
              ),
              div(
                div(
                  class = "widget",
                  style = "width: 50%;"
                ),
                div(
                  class = "widget",
                  style = "width: 50%;"
                ),
                style = "display: flex; height: 100%;"
              ),
              style = "width: 100%; display:flex; flex-direction: column;"
            ),
            div(
              uiOutput(ns("selected_concepts_list")),
              style = "width: 500px; overflow-y: auto; padding-top: 10px;",
              class = "widget"
            ),
            style = "display: flex; height: calc(100% - 30px);"
          ),
          div(
            shiny.fluent::PrimaryButton.shinyInput(ns("close_select_concepts_modal_2"), i18n$t("confirm")),
            style = "display: flex; justify-content: flex-end; margin-right: 10px;"
          ),
          class = "select_concepts_modal_content"
        ),
        class = "select_concepts_modal"
      )
    ),
    
    # Reload dataset concepts modal ----
    
    shinyjs::hidden(
      div(
        id = ns("reload_dataset_concepts_modal"),
        div(
          tags$h1(i18n$t("reload_dataset_concepts_title")), tags$p(i18n$t("reload_dataset_concepts_text")),
          div(
            shiny.fluent::DefaultButton.shinyInput(ns("close_reload_dataset_concepts_modal"), i18n$t("dont_reload")),
            div(shiny.fluent::PrimaryButton.shinyInput(ns("confirm_reload_dataset_concepts"), i18n$t("reload"))),
            class = "reload_dataset_concepts_modal_buttons"
          ),
          class = "reload_dataset_concepts_modal_content"
        ),
        class = "reload_dataset_concepts_modal"
      )
    )
  )
}

#' @noRd 
mod_select_concepts_server <- function(id, r, d, m, language, i18n, debug){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Open / close modal ----
    
    observeEvent(input$open_select_concepts_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$open_select_concepts_modal"))
      shinyjs::show("select_concepts_modal")
    })
    
    observeEvent(input$close_select_concepts_modal_1, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$close_select_concepts_modal_1"))
      shinyjs::hide("select_concepts_modal")
    })
    
    observeEvent(input$close_select_concepts_modal_2, {
      if (debug) cat(paste0("\n", now(), " - mod_data - observer input$close_select_concepts_modal_2"))
      shinyjs::hide("select_concepts_modal")
    })
    
    # Reload vocabularies ----
    observeEvent(r$dataset_vocabularies, {
      if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer r$dataset_vocabularies"))
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_vocabularies', Math.random())"))
    })
    
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_vocabularies', Math.random())"))

    observeEvent(input$reload_vocabularies, {
      if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer r$reload_vocabularies"))
      
      req(r$dataset_vocabularies)
      if (nrow(r$dataset_vocabularies) == 0) vocabulary_options = list()
      if (nrow(r$dataset_vocabularies) > 0) vocabulary_options <- convert_tibble_to_list(data = r$dataset_vocabularies, key_col = "vocabulary_id", text_col = "vocabulary_id", i18n = i18n)

      shiny.fluent::updateComboBox.shinyInput(session, "vocabulary", options = vocabulary_options, value = NULL)
      
      # Initiate selected_concepts var
      r[[paste0(id, "_selected_concepts")]] <- tibble::tibble(
        concept_id = integer(), concept_name = character(), domain_id = character(), vocabulary_id = character(),
        mapped_to_concept_id = integer(), merge_mapped_concepts = logical()
      )
    })

    # Reload vocabulary concepts ----
    observeEvent(input$vocabulary, {

      if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$vocabulary"))
      
      req(length(d$dataset_all_concepts) > 0, nrow(d$dataset_all_concepts) > 0)
      
      shinyjs::show("vocabulary_concepts")

      vocabulary_id <- input$vocabulary$key
      req(length(vocabulary_id) > 0)

      widget_vocabulary_concepts <- 
        d$dataset_all_concepts %>%
        dplyr::filter(vocabulary_id_1 == vocabulary_id) %>%
        dplyr::select(
          concept_id = concept_id_1, concept_name = concept_name_1,
          relationship_id, domain_id, vocabulary_id = vocabulary_id_1, concept_class_id, standard_concept, concept_code,
          count_persons_rows, count_concepts_rows, add_concept_input
        ) %>%
        dplyr::filter(is.na(relationship_id)) %>%
        dplyr::select(-relationship_id) %>%
        dplyr::mutate_at(c("add_concept_input"), stringr::str_replace_all, "%ns%", id) %>%
        dplyr::mutate_at("add_concept_input", stringr::str_replace_all, "%input_prefix%", "add_concept") %>%
        dplyr::mutate_at("add_concept_input", stringr::str_replace_all, "%input_prefix_2%", "") %>%
        dplyr::mutate_at("concept_id", as.character) %>%
        dplyr::mutate(add_concept_input = stringr::str_replace_all(add_concept_input, "%concept_id_1%", concept_id)) %>%
        dplyr::arrange(dplyr::desc(count_concepts_rows))

      r[[paste0(id, "_vocabulary_concepts")]] <- widget_vocabulary_concepts

      if (length(r[[paste0(id, "_vocabulary_concepts_proxy")]]) == 0){
        
        editable_cols <- c("concept_name")
        searchable_cols <- c("concept_id", "concept_name", "domain_id", "concept_class_id", "standard_concept", "concept_code", "concept_display_name")
        factorize_cols <- c("domain_id", "concept_class_id", "standard_concept")
        column_widths <- c(
          "concept_name" = "300px", "domain_id" = "100px", "concept_class_id" = "100px", "concept_id" = "80px", "action" = "80px",
          "concept_id" = "100px", "count_persons_rows" = "40px", "count_concepts_rows" = "40px"
        )
        sortable_cols <- c("concept_id", "concept_name", "domain_id", "vocabulary_id", "count_persons_rows", "count_concepts_rows")
        centered_cols <- c("concept_id", "count_persons_rows", "count_concepts_rows", "add_concept_input")
        col_names <- unname(sapply(colnames(widget_vocabulary_concepts), i18n$t))
        hidden_cols <- ""

        # Render datatable
        render_datatable(
          output = output, ns = ns, i18n = i18n, data = widget_vocabulary_concepts,
          output_name = "vocabulary_concepts", col_names =  col_names, datatable_dom = "<'top't><'bottom'p>",
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, hidden_col = hidden_cols, factorize_cols = factorize_cols, selection = "none"
        )

        # Create a proxy for datatatable
        r[[paste0(id, "_vocabulary_concepts_proxy")]] <- DT::dataTableProxy("vocabulary_concepts", deferUntilFlush = FALSE)
        
        shinyjs::delay(50, r[[paste0(id, "_vocabulary_concepts_proxy")]] %>% DT::hideCols(c(3, 4, 5)))
      }
      else DT::replaceData(r[[paste0(id, "_vocabulary_concepts_proxy")]], r[[paste0(id, "_vocabulary_concepts")]], resetPaging = FALSE, rownames = FALSE)
    })

    # Show / hide DT cols ----

    observeEvent(input$vocabulary_dt_cols, {
      if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$vocabulary_dt_cols"))

      req(length(r[[paste0(id, "_vocabulary_concepts_proxy")]]) > 0)

      r[[paste0(id, "_vocabulary_concepts_proxy")]] %>%
        DT::showCols(0:9) %>%
        DT::hideCols(setdiff(0:9, input$vocabulary_dt_cols))
    })

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
    
    # Select / unselect a concept ----
    observeEvent(input$concept_selected, {
      if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$concept_selected"))
      
      concept_id <- as.numeric(substr(input$concept_selected, nchar(paste0(id , "-add_concept_")) + 1, nchar(input$concept_selected)))
      
      # Add concept
      
      if (concept_id %not_in% r[[paste0(id, "_selected_concepts")]]$concept_id){
        r[[paste0(id, "_selected_concepts")]] <-
          r[[paste0(id, "_selected_concepts")]] %>%
          dplyr::bind_rows(
            r[[paste0(id, "_vocabulary_concepts")]] %>%
              dplyr::mutate_at("concept_id", as.numeric) %>%
              dplyr::filter(concept_id == !!concept_id) %>%
              dplyr::transmute(concept_id, concept_name, domain_id, vocabulary_id, mapped_to_concept_id = NA_integer_, merge_mapped_concepts = FALSE)
          )     
        
        shinyjs::runjs(paste0("$('#", input$concept_selected, " i').removeClass('fa-plus').addClass('fa-minus');"))
      }
      
      # Remove concept
      else {
        r[[paste0(id, "_selected_concepts")]] <- r[[paste0(id, "_selected_concepts")]] %>% dplyr::filter(concept_id != !!concept_id)
        
        shinyjs::runjs(paste0("$('#", input$concept_selected, " i').removeClass('fa-minus').addClass('fa-plus');"))
      }
      
      # Update selected concepts list
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_selected_concepts_list', Math.random())"))
    })
    
    # Update selected concepts list ----
    
    observeEvent(input$update_selected_concepts_list, {
      if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$update_selected_concepts_list"))
      
      selected_concepts_ui <- tagList()
      selected_concepts_list_ui <- tagList()
      
      if (nrow(r[[paste0(id, "_selected_concepts")]]) > 0){
        for (i in 1:nrow(r[[paste0(id, "_selected_concepts")]])){
          row <- r[[paste0(id, "_selected_concepts")]][i, ]
          
          if (row$domain_id %in% c("Drug", "Condition", "Procedure", "Measurement", "Observation")) concept_style <- paste0(tolower(row$domain_id), "_domain_id")
          else concept_style <- "default_domain_id"
          
          concept_style <- paste0("selected_concept ", concept_style)
          concept_name <- paste0(row$vocabulary_id, " - ", row$concept_name)
          
          selected_concepts_list_ui <- tagList(
            selected_concepts_list_ui,
            div(
              div(
                shiny.fluent::IconButton.shinyInput(ns(paste0("remove_concept_", row$concept_id)), iconProps = list(iconName = "Cancel"), style = "height: 20px; margin: 0; font-size: 10px;"),
                onclick = paste0(
                  "Shiny.setInputValue('", id, "-remove_concept_trigger', Math.random());",
                  "Shiny.setInputValue('", id, "-remove_concept', ", row$concept_id, ");"
                ),
                class = "small_icon_button"
              ),
              create_hover_card(ui = div(concept_name, class = concept_style), text = concept_name),
              style = "display: flex; margin: 2px 0;"
            )
          )
          
          selected_concepts_ui <- tagList(
            selected_concepts_ui, 
            create_hover_card(ui = div(concept_name, class = concept_style, style = "display: inline-block;"), text = concept_name)
          )
        }
      }
      
      shinyjs::runjs(paste0(
        "$('#", id, "-selected_concepts').css('height', 'auto');",
        "$('#", id, "-selected_concepts').css('justify-content', 'left');",
        "$('#", id, "-selected_concepts').css('align-items', 'flex-wrap;');"
      ))
      output$selected_concepts <- renderUI(selected_concepts_ui)
      output$selected_concepts_list <- renderUI(selected_concepts_list_ui)
    })
    
    # Remove a concept ----
    observeEvent(input$remove_concept_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$remove_concept_trigger"))
      
      shinyjs::runjs(paste0("$('#", id, "-add_concept_", input$remove_concept, " i').removeClass('fa-minus').addClass('fa-plus');"))
      
      r[[paste0(id, "_selected_concepts")]] <- r[[paste0(id, "_selected_concepts")]] %>% dplyr::filter(concept_id != input$remove_concept)
      
      # Update selected concepts list
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_selected_concepts_list', Math.random())"))
    })
    
    # Reload dataset concepts ----
    observeEvent(input$reload_dataset_concepts, {
      if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$reload_dataset_concepts"))
      shinyjs::show("reload_dataset_concepts_modal")
    })
    
    observeEvent(input$close_reload_dataset_concepts_modal, {
      if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$close_reload_dataset_concepts_modal"))
      shinyjs::hide("reload_dataset_concepts_modal")
    })
    
    observeEvent(input$confirm_reload_dataset_concepts, {
      if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$confirm_reload_dataset_concepts"))
      
      # Remove dataset_all_concepts file
      dataset_folder <- paste0(r$app_folder, "/datasets_files/", r$selected_dataset)
      dataset_all_concepts_filename <- paste0(dataset_folder, "/dataset_all_concepts.csv")
      if (file.exists(dataset_all_concepts_filename)) unlink(dataset_all_concepts_filename)
      
      # Reset fields
      shinyjs::hide("vocabulary_concepts")
      r[[paste0(id, "_selected_concepts")]] <- tibble::tibble(
        concept_id = integer(), concept_name = character(), domain_id = character(), vocabulary_id = character(),
        mapped_to_concept_id = integer(), merge_mapped_concepts = logical()
      )
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_selected_concepts_list', Math.random())"))
      
      # Reload concepts count
      load_dataset_concepts(r, d, m)
      
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-reload_vocabularies', Math.random())"))
      
      shinyjs::hide("reload_dataset_concepts_modal")
    })
  })
}