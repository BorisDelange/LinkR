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
                  div(shiny.fluent::Dropdown.shinyInput(ns("vocabulary"), label = i18n$t("vocabulary"), allowFreeform = FALSE, multiSelect = FALSE), style = "width: 200px;"),
                  div(
                    shiny.fluent::Dropdown.shinyInput(
                      ns("vocabulary_dt_cols"), label = i18n$t("columns"), multiSelect = TRUE,
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
                      value = c(0, 1, 2, 7, 8, 9)
                    ), 
                    style = "width: 200px;"
                  ),
                  style = "display: flex; gap: 10px;"
                ),
                br(),
                DT::DTOutput(ns("vocabulary_concepts")),
                class = "widget",
                style = "padding: 10px; height: calc(100% - 50px);"
              ),
              style = "width: 100%;"
            ),
            div(
              uiOutput(ns("selected_concepts_list")),
              style = "width: 500px; overflow-y: auto; padding-top: 10px; height: calc(100% - 45px);",
              class = "widget"
            ),
            style = "display: flex; height: calc(100% - 65px);"
          ),
          div(
            shiny.fluent::PrimaryButton.shinyInput(ns("close_select_concepts_modal_2"), i18n$t("confirm")),
            style = "display: flex; justify-content: flex-end; margin-right: 10px;"
          ),
          class = "select_concepts_modal_content"
        ),
        class = "select_concepts_modal"
      )
    )
  )
}

#' @noRd 
mod_select_concepts_server <- function(id, r, d, m, language, i18n, debug, user_accesses){
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
      
      req(length(r$dataset_vocabularies) > 0)
      if (nrow(r$dataset_vocabularies) == 0) vocabulary_options = list()
      if (nrow(r$dataset_vocabularies) > 0) vocabulary_options <-
        r$dataset_vocabularies %>% 
        dplyr::select(vocabulary_id) %>%
        dplyr::mutate(text = vocabulary_id) %>%
        dplyr::bind_rows(tibble::tibble(vocabulary_id = "all_vocabularies", text = i18n$t("all_vocabularies"))) %>%
        dplyr::arrange(vocabulary_id != "all_vocabularies", vocabulary_id) %>%
        convert_tibble_to_list(key_col = "vocabulary_id", text_col = "text")
      
      shiny.fluent::updateDropdown.shinyInput(session, "vocabulary", options = vocabulary_options, value = NULL)
      
      # Initiate selected_concepts var
      r[[paste0(id, "_selected_concepts")]] <- tibble::tibble(
        concept_id = integer(), concept_name = character(), domain_id = character(), vocabulary_id = character(),
        mapped_to_concept_id = integer(), merge_mapped_concepts = logical()
      )
    })

    # Reload vocabulary concepts ----
    observeEvent(input$vocabulary, {

      if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$vocabulary"))
      
      req(length(d$dataset_concept) > 0, nrow(d$dataset_concept) > 0)
      
      shinyjs::show("vocabulary_concepts")
      req(length(input$vocabulary) > 0)
      
      if (input$vocabulary == "all_vocabularies") data <- d$dataset_concept
      else data <- d$dataset_concept %>% dplyr::filter(vocabulary_id == input$vocabulary)

      widget_vocabulary_concepts <- 
        data %>%
        dplyr::select(
          concept_id, concept_name, domain_id, vocabulary_id, concept_class_id, standard_concept, concept_code,
          count_persons_rows, count_concepts_rows, add_concept_input
        ) %>%
        dplyr::mutate_at(c("add_concept_input"), stringr::str_replace_all, "%ns%", id) %>%
        dplyr::mutate_at("concept_id", as.character) %>%
        dplyr::mutate(add_concept_input = stringr::str_replace_all(add_concept_input, "%concept_id%", concept_id)) %>%
        dplyr::arrange(dplyr::desc(count_concepts_rows))

      r[[paste0(id, "_vocabulary_concepts")]] <- widget_vocabulary_concepts

      if (length(r[[paste0(id, "_vocabulary_concepts_proxy")]]) == 0){
        
        editable_cols <- c("concept_name")
        searchable_cols <- c("concept_id", "concept_name", "domain_id", "vocabulary_id", "concept_class_id", "standard_concept", "concept_code", "concept_display_name")
        factorize_cols <- c("domain_id", "concept_class_id", "standard_concept", "vocabulary_id")
        column_widths <- c(
          "domain_id" = "100px", "concept_class_id" = "100px", "concept_id" = "80px", "add_concept_input" = "80px",
          "concept_id" = "100px", "count_persons_rows" = "40px", "count_concepts_rows" = "40px"
        )
        sortable_cols <- c("concept_id", "concept_name", "domain_id", "vocabulary_id", "count_persons_rows", "count_concepts_rows")
        centered_cols <- c("concept_id", "count_persons_rows", "count_concepts_rows", "add_concept_input")
        col_names <- unname(sapply(colnames(widget_vocabulary_concepts), i18n$t))
        hidden_cols <- ""

        # Render datatable
        render_datatable(
          output = output, ns = ns, i18n = i18n, data = widget_vocabulary_concepts,
          output_name = "vocabulary_concepts", col_names =  col_names, page_length = 18, datatable_dom = "<'top't><'bottom'p>",
          editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
          searchable_cols = searchable_cols, filter = TRUE, hidden_col = hidden_cols, factorize_cols = factorize_cols, selection = "none", enable_keyboard_navigation = FALSE
        )

        # Create a proxy for datatatable
        r[[paste0(id, "_vocabulary_concepts_proxy")]] <- DT::dataTableProxy("vocabulary_concepts", deferUntilFlush = FALSE)
        
        shinyjs::delay(50, r[[paste0(id, "_vocabulary_concepts_proxy")]] %>% DT::hideCols(c(3, 4, 5, 6)))
      }
      # replaceData is safe here since selection = 'none' disables the Select extension, which otherwise conflicts with server-side processing.
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
    observeEvent(input$add_concept_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$add_concept_trigger"))
      
      concept_id <- input$concept_selected
      
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
        
        shinyjs::runjs(paste0("$('#", id, "-add_concept_", input$concept_selected, " i').removeClass('fa-plus').addClass('fa-minus');"))
      }
      
      # Remove concept
      else {
        r[[paste0(id, "_selected_concepts")]] <- r[[paste0(id, "_selected_concepts")]] %>% dplyr::filter(concept_id != !!concept_id)
        
        shinyjs::runjs(paste0("$('#", id, "-add_concept_", input$concept_selected, " i').removeClass('fa-minus').addClass('fa-plus');"))
      }
      
      # Update selected concepts list
      shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-update_selected_concepts_list', Math.random())"))
    })
    
    # Remove a concept ----
    observeEvent(input$remove_concept_trigger, {
      if (debug) cat(paste0("\n", now(), " - mod_select_concepts - (", id, ") - observer input$remove_concept_trigger"))
      
      shinyjs::runjs(paste0("$('#", id, "-add_concept_", input$remove_concept, " i').removeClass('fa-minus').addClass('fa-plus');"))
      
      r[[paste0(id, "_selected_concepts")]] <- r[[paste0(id, "_selected_concepts")]] %>% dplyr::filter(concept_id != input$remove_concept)
      
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
      
      if (id == "data"){
        
        action <- input$opened_widget_modal
        
        shinyjs::runjs(paste0(
          "$('#", id, "-", action, "_widget_selected_concepts').css('height', 'auto');",
          "$('#", id, "-", action, "_widget_selected_concepts').css('justify-content', 'left');",
          "$('#", id, "-", action, "_widget_selected_concepts').css('align-items', 'flex-wrap;');"
        ))
        output[[paste0(action, "_widget_selected_concepts")]] <- renderUI(selected_concepts_ui)
      }
      else if (id == "plugins"){
        shinyjs::runjs(paste0(
          "$('#", id, "-selected_concepts').css('height', 'auto');",
          "$('#", id, "-selected_concepts').css('justify-content', 'left');",
          "$('#", id, "-selected_concepts').css('align-items', 'flex-wrap;');"
        ))
        output$selected_concepts <- renderUI(selected_concepts_ui) 
      }
      output$selected_concepts_list <- renderUI(selected_concepts_list_ui)
    })
  })
}
