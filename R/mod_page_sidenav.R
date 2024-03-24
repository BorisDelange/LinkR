#' page_sidenav UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_page_sidenav_ui <- function(id = character(), i18n = character()){
  ns <- NS(id)
  result <- ""
  
  # --- --- -
  # Home ----
  # --- --- -
  
  if (id == "home"){
    div(
      class = "sidenav",
      div(class = "reduced_sidenav"),
      div(class = "extended_sidenav")
    ) -> result
  }

  
  # --- --- --- --
  # Dropdowns ----
  # --- --- --- --
  
  if (id %in% c("my_studies", "my_subsets", "messages", "vocabularies", "scripts", "patient_level_data", "aggregated_data")){
    
    dropdowns <- function(names){
      
      result <- tagList()
      
      sapply(names, function(name){
        
        if (name == "person"){
          allowFreeform <- TRUE
          autoComplete <- "off"
        } else {
          allowFreeform <- FALSE
          autoComplete <- "on"
        }
        
        width <- "250px"
        
        result <<- tagList(result,
          div(id = ns(paste0(name, "_title")), class = "input_title", i18n$t(name)),
          shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 5),
            div(shiny.fluent::ComboBox.shinyInput(ns(name), allowFreeform = allowFreeform, autoComplete = autoComplete), style = paste0("min-width:", width, "; max-width:", width, ";"))
          )
        )
      })
      
      result
    }
  }
  
  # --- --- --- ---
  # My studies ----
  # --- --- --- ---
  
  if (id == "my_studies") div(class = "sidenav", div(class = "reduced_sidenav"), div(class = "extended_sidenav", dropdowns(c("dataset")))) -> result
  
  # --- --- --- ---
  # My subsets ----
  # --- --- --- ---
  
  if (id == "my_subsets") div(class = "sidenav", div(class = "reduced_sidenav"), div(class = "extended_sidenav", dropdowns(c("dataset", "study")))) -> result
  
  # --- --- --- -
  # Messages ----
  # --- --- --- -
  
  if (id == "messages") div(class = "sidenav", div(class = "reduced_sidenav"), div(class = "extended_sidenav", dropdowns(c("dataset", "study")))) -> result
  
  # --- --- --- --- -
  # Vocabularies ----
  # --- --- --- --- -
  
  if (id == "vocabularies") div(class = "sidenav", div(class = "reduced_sidenav"), div(class = "extended_sidenav", dropdowns(c("dataset")))) -> result
  
  # --- --- -- -
  # Scripts ----
  # --- --- -- -
  
  if (id == "scripts") div(class = "sidenav", div(class = "reduced_sidenav"), div(class = "extended_sidenav", dropdowns(c("dataset")))) -> result
  
  # --- --- --- --- --- ---
  # Patient-level data ----
  # --- --- --- --- --- ---
  
  if (id == "patient_level_data"){
    div(class = "sidenav",
      div(class = "reduced_sidenav",
        div(
          onclick = paste0("Shiny.setInputValue('", id, "-show_hide_sidenav', Math.random());"),
          class = "button_hide_sidenav"
        )
      ),
      div(class = "extended_sidenav",
        dropdowns(c("person", "visit_detail")),
        br(), div(id = ns("hr2"), hr()),
        uiOutput(ns("person_info")),
        div(
          onclick = paste0("Shiny.setInputValue('", id, "-show_hide_sidenav', Math.random());"),
          class = "button_show_sidenav"
        )
      )
    ) -> result
  }
  
  # --- --- --- --- -- -
  # Aggregated data ----
  # --- --- --- --- -- -
  
  if (id == "aggregated_data") div(
    class = "sidenav",
    div(class = "reduced_sidenav"),
    div(class = "extended_sidenav",
      div(i18n$t("data"), class = "input_title", style = "font-size:14.5px;"),
      div(
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 0),
          shiny.fluent::DefaultButton.shinyInput(ns("data_page_ind"), i18n$t("individual"), style = "width:125px;"), 
          shiny.fluent::PrimaryButton.shinyInput(ns("data_page_agg"), i18n$t("aggregated"), style = "width:125px;")
        ), style = "width:250px;"
      ),                               
      dropdowns(c("dataset", "study", "subset"))
    )
  ) -> result
  
  # --- --- -- -
  # Plugins ----
  # --- --- -- -
  
  if (id %in% c("plugins_patient_lvl", "plugins_aggregated")){
    
    div(
      class = "sidenav",
      div(class = "reduced_sidenav"),
      div(class = "extended_sidenav", style = "width:20px;")
    ) -> result
  }
  
  # --- --- --- -
  # Settings ----
  # --- --- --- -
  
  if (grepl("^settings", id)){
    
    links_data_management <- list()
    
    lapply(c("datasets", "vocabularies"), function(page){
      links_data_management <<- rlist::list.append(links_data_management, list(name = i18n$t(page),
        id = ns(page), key = page, url = shiny.router::route_link(paste0("settings/", page))))
    })
    
    links <- list()
    for(page in c("general_settings", "app_db", "git", "users", "dev", "data_management", "log")){
      # Sub links for data management
      if (page == "data_management") links <- rlist::list.append(links, list(name = i18n$t(page),
        id = ns(page), key = page, links = links_data_management, selectedKey = substr(id, nchar("settings") + 2, 100), isExpanded = TRUE))

      # No sub links
      else if (page == "git") links <- rlist::list.append(links, list(name = i18n$t("remote_git_repos"), id = ns(page), key = page, url = shiny.router::route_link(paste0("settings/", page))))
      else links <- rlist::list.append(links, list(name = i18n$t(page), id = ns(page), key = page, url = shiny.router::route_link(paste0("settings/", page))))
    }
    
    div(class = "sidenav",
      div(class = "reduced_sidenav"),
      div(class = "extended_sidenav",
        shiny.fluent::Nav(
          groups = list(
            list(links = links)
          ),
          selectedKey = substr(id, nchar("settings") + 2, 100),
          styles = list(
            root = list(
              height = "100%",
              boxSizing = "border-box",
              overflowY = "auto"
            )
          )
        )
      )
    ) -> result
  }
  
  result
}
    
#' page_sidenav Server Functions
#'
#' @noRd 
mod_page_sidenav_server <- function(id = character(), r = shiny::reactiveValues(), d = shiny::reactiveValues(), 
  m = shiny::reactiveValues(), i18n = character(), language = "en", perf_monitoring = FALSE, debug = FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    if (debug) cat(paste0("\n", now(), " - mod_page_sidenav (", id, ") - start"))
    
    # Show / hide sidenav
    
    r$show_hide_sidenav <- "hide"
    
    observeEvent(input$show_hide_sidenav, {
      if (debug) cat(paste0("\n", now(), " - mod_page_sidenav - observer input$show_hide_sidenav"))

      if (r$show_hide_sidenav == "hide"){
        r$show_hide_sidenav <- "show"
        shinyjs::runjs(paste0("$('.extended_sidenav').css('display', 'none');"))
        shinyjs::runjs(paste0("$('.reduced_sidenav').css('display', 'block');"))
        shinyjs::runjs(paste0("$('.grid-container').css('grid-template-areas', '\"header header header\" \"sidenav main main\" \"footer footer footer\"');"))
        shinyjs::runjs(paste0("$('.main').css('left', '20px');"))
      }
      else {
        r$show_hide_sidenav <- "hide"
        shinyjs::runjs(paste0("$('.extended_sidenav').css('display', 'block');"))
        shinyjs::runjs(paste0("$('.reduced_sidenav').css('display', 'none');"))
        shinyjs::runjs(paste0("$('.grid-container').css('grid-template-areas', '\"header header header\" \"sidenav sidenav main\" \"footer footer footer\"');"))
        shinyjs::runjs(paste0("$('.main').css('left', '0px');"))
      }
    })
    
    # --- --- --- -
    # Projects ----
    # --- --- --- -
    
    # if (grepl("^home", id)) shinyjs::runjs(paste0("$('.grid-container').css('grid-template-areas', '\"header header header\" \"sidenav main main\" \"footer footer footer\"');"))
    
    # --- --- -- -
    # Plugins ----
    # --- --- -- -
    
    # if (id %in% c("plugins_patient_lvl", "plugins_aggregated")){
    #   
    #   # Current tab
    #   observeEvent(r[[paste0(id, "_current_tab")]], {
    #     if (debug) cat(paste0("\n", now(), " - mod_page_sidenav - observer r$..current_tab"))
    #     
    #     current_tab <- r[[paste0(id, "_current_tab")]]
    #     
    #     sapply(c(paste0(id, "_edit_code_div")), shinyjs::hide)
    #     
    #     if (current_tab == "plugins_edit_code_card") shinyjs::show(paste0(id, "_edit_code_div"))
    #   })
    #   
    #   # Display
    #   sapply(c("plugin", "script", "concepts", "editor", "code_result"), function(name) observeEvent(input[[paste0(id, "_edit_code_", name, "_div")]], {
    #     r[[paste0(id, "_edit_code_", name, "_div")]] <- input[[paste0(id, "_edit_code_", name, "_div")]]
    #   }))
    #   
    #   observeEvent(input[[paste0(id, "_edit_code_side_by_side_divs")]], {
    #     r[[paste0(id, "_edit_code_side_by_side_divs")]] <- input[[paste0(id, "_edit_code_side_by_side_divs")]]
    #   })
    #   
    #   # Edit code commands
    #   observeEvent(input[[paste0(id, "_edit_code_ui_server")]], {
    #     if (debug) cat(paste0("\n", now(), " - mod_page_sidenav - observer input$edit_code_ui_server"))
    #     r[[paste0(id, "_edit_code_ui_server")]] <- input[[paste0(id, "_edit_code_ui_server")]]
    #   })
    #   
    #   # Edit code actions
    #   
    #   sapply(c("save", "execute"), function(name) observeEvent(input[[paste0(id, "_edit_code_", name, "_code")]], {
    #     if (debug) cat(paste0("\n", now(), " - mod_page_sidenav - observer input$.._edit_code_", name, "_code"))
    #     r[[paste0(id, "_edit_code_", name, "_code")]] <- input[[paste0(id, "_edit_code_", name, "_code")]]
    #   }))
    # }
    
    if (id %in% c("my_studies", "my_subsets", "messages", "vocabularies", "scripts", "patient_level_data", "aggregated_data")){
      
      # --- --- --- --- --- --- --- --- -
      # Patient-lvl & agregated data ----
      # --- --- --- --- --- --- --- --- -
      
      # Changing page between patient-lvl & aggregated data
      
      r$data_page <- "patient_level_data"
      
      if (id == "patient_level_data") observeEvent(input$data_page_agg, {
        shiny.router::change_page("aggregated_data")
        r$data_page <- "aggregated_data"
      })
      if (id == "aggregated_data") observeEvent(input$data_page_ind, {
        shiny.router::change_page("patient_level_data")
        r$data_page <- "patient_level_data"
      })
      
      # --- --- --- --- -- -
      # Selected dataset ----
      # --- --- --- --- -- -
      
      # observeEvent(r$datasets, {
      #   
      #   if (debug) cat(paste0("\n", now(), " - mod_page_sidenav - observer r$datasets"))
      #   
      #   # Update dropdown
      #   shinyjs::delay(100, {
      #     
      #     value <- NULL
      #     if (length(r$selected_dataset) > 0) if (nrow(r$datasets %>% dplyr::filter(id == r$selected_dataset)) > 0) value <- list(
      #       key = r$selected_dataset, text = r$datasets %>% dplyr::filter(id == r$selected_dataset) %>% dplyr::pull(name))
      #     
      #     shiny.fluent::updateComboBox.shinyInput(session, "dataset", 
      #       options = convert_tibble_to_list(r$datasets %>% dplyr::arrange(name), key_col = "id", text_col = "name"), value = value)
      #     
      #     sapply(c("study", "subset", "person", "visit_detail", "person_status", "hr1", "hr2", "exclusion_reason_div"), function(element){
      #       sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::hide)
      #     })
      #     shinyjs::hide("exclusion_reason_div")
      #   })
      # })
      
      # observeEvent(input$dataset, {
      #   
      #   if (debug) cat(paste0("\n", now(), " - mod_page_sidenav - observer input$dataset"))
      #   
      #   # Save value in r$selected_dropdown, to update patient-level data dropdowns AND aggregated data dropdowns
      #   r$selected_dataset <- input$dataset$key
      #   
      #   sapply(c("subset", "person", "visit_detail", "person_status", "hr1", "hr2", "exclusion_reason_div"), function(element){
      #     sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::hide)
      #   })
      #   sapply(c("study"), function(element) sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::show))
      #   
      #   # Reset dropdowns & uiOutput
      #   # Hide exclusion_reason dropdown
      #   
      #   shiny.fluent::updateComboBox.shinyInput(session, "subset", options = list(), value = NULL)
      #   shiny.fluent::updateComboBox.shinyInput(session, "person", options = list(), value = NULL)
      #   shiny.fluent::updateComboBox.shinyInput(session, "visit_detail", options = list(), value = NULL)
      #   shiny.fluent::updateComboBox.shinyInput(session, "person_status", options = list(), value = NULL)
      #   shiny.fluent::updateComboBox.shinyInput(session, "exclusion_reason", options = list(), value = NULL)
      #   shinyjs::hide("exclusion_reason_div")
      #   output$person_info <- renderUI("")
      # })
      
      # Update the two pages dropdowns (patient-level data page & aggregated data page)
      # observeEvent(r$selected_dataset, {
      #   
      #   if (debug) cat(paste0("\n", now(), " - mod_page_sidenav - observer r$selected_dataset"))
      #   
      #   shiny.fluent::updateComboBox.shinyInput(session, "dataset", options = 
      #     convert_tibble_to_list(r$datasets %>% dplyr::arrange(name), key_col = "id", text_col = "name"), 
      #       value = list(key = r$selected_dataset))
      #   
      #   # Reset m$selected_study (to reset main display)
      #   if (length(m$selected_study) == 0) m$selected_study <- NA_integer_
      #   if (!is.na(m$selected_study)) m$selected_study <- NA_integer_
      #   
      #   # Reset of data variables, load of vocabulary code happens in mod_my_studies.R
      #   # With this solution, code is run only one time
      #   # Here, code is run for each page
      # })
      
      # --- --- --- --- -
      # Selected study --
      # --- --- --- --- -
      
      if (id %in% c("my_studies", "my_subsets", "patient_level_data", "aggregated_data", "messages")){
      
        # observeEvent(r$studies, {
        #   
        #   if (debug) cat(paste0("\n", now(), " - mod_page_sidenav - observer r$studies"))
        #   
        #   if (nrow(r$studies) == 0) shiny.fluent::updateComboBox.shinyInput(session, "study", options = list(), value = NULL, 
        #     errorMessage = i18n$t("no_study_available"))
        #   
        #   if (nrow(r$studies) > 0) shiny.fluent::updateComboBox.shinyInput(session, "study",
        #     options = convert_tibble_to_list(r$studies %>% dplyr::arrange(name), key_col = "id", text_col = "name"), value = NULL)
        # })
        # 
        # observeEvent(input$study, {
        #   
        #   if (perf_monitoring) monitor_perf(r = r, action = "start")
        #   if (debug) cat(paste0("\n", now(), " - mod_page_sidenav - observer input$study"))
        # 
        #   req(input$study$key)
        #   
        #   # Prevent multiple changes of m$selected_study
        #   # We have to keep multiple observers, cause we use input variable
        #   if (is.na(m$selected_study)) m$selected_study <- input$study$key
        #   if (!is.na(m$selected_study) & m$selected_study != input$study$key) m$selected_study <- input$study$key
        # 
        #   # Reset dropdowns & uiOutput
        #   shiny.fluent::updateComboBox.shinyInput(session, "person", options = list(), value = NULL)
        #   shiny.fluent::updateComboBox.shinyInput(session, "visit_detail", options = list(), value = NULL)
        #   shiny.fluent::updateComboBox.shinyInput(session, "person_status", options = list(), value = NULL)
        #   shiny.fluent::updateComboBox.shinyInput(session, "exclusion_reason", options = list(), value = NULL)
        #   
        #   sapply(c("person", "visit_detail", "person_status", "hr1", "hr2", "exclusion_reason_div"), function(element){
        #     sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::hide)
        #   })
        #   sapply(c("subset"), function(element) sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::show))
        #   shinyjs::hide("exclusion_reason_div")
        #   output$person_info <- renderUI("")
        #   
        #   if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_page_sidenav - observer input$study"))
        # })
        # 
        # observeEvent(m$selected_study, {
        #   
        #   if (debug) cat(paste0("\n", now(), " - mod_page_sidenav - observer m$selected_study"))
        #   
        #   req(input$dataset$key & !is.na(m$selected_study))
        #   studies <- r$studies %>% dplyr::filter(dataset_id == input$dataset$key)
        #   
        #   shiny.fluent::updateComboBox.shinyInput(session, "study", options =
        #       convert_tibble_to_list(studies %>% dplyr::arrange(name), key_col = "id", text_col = "name"),
        #     value = list(key = m$selected_study))
        #   
        #   # Load of subsets is done in mod_my_studies.R
        #   # With this solution, code is run only one time
        # })
        
        observeEvent(m$subsets, {
          req(!is.na(m$selected_study))
          
          # Update subset dropdown
          if (nrow(m$subsets) == 0) shiny.fluent::updateComboBox.shinyInput(session, "subset", options = list(), value = NULL, errorMessage = i18n$t("no_subset_available"))
          if (nrow(m$subsets) > 0) shiny.fluent::updateComboBox.shinyInput(session, "subset", options = convert_tibble_to_list(m$subsets, key_col = "id", text_col = "name"), value = NULL)
          
          # Reset other dropdowns & uiOutput
          shiny.fluent::updateComboBox.shinyInput(session, "person", options = list(), value = NULL)
          shiny.fluent::updateComboBox.shinyInput(session, "visit_detail", options = list(), value = NULL)
          shiny.fluent::updateComboBox.shinyInput(session, "person_status", options = list(), value = NULL)
          shiny.fluent::updateComboBox.shinyInput(session, "exclusion_reason", options = list(), value = NULL)
          
          sapply(c("person", "visit_detail", "person_status", "hr1", "hr2", "exclusion_reason_div"), function(element){
            sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::hide)
          })
          sapply(c("subset"), function(element) sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::show))
          shinyjs::hide("exclusion_reason_div")
          output$person_info <- renderUI("")
        })
      }
      
      # --- --- --- --- --
      # Selected subset --
      # --- --- --- --- --
      
      if (id %in% c("my_studies", "my_subsets", "patient_level_data", "aggregated_data")){
        
        observeEvent(m$subsets, {
          
          if (debug) cat(paste0("\n", now(), " - mod_page_sidenav - observer m$subsets"))
          
          if (nrow(m$subsets) == 0) shiny.fluent::updateComboBox.shinyInput(session, "subset", options = list(), value = NULL, 
            errorMessage = i18n$t("no_subset_available"))
          
          if (nrow(m$subsets) > 0) shiny.fluent::updateComboBox.shinyInput(session, "subset",
            options = convert_tibble_to_list(m$subsets %>% dplyr::arrange(name), key_col = "id", text_col = "name"), value = NULL)
        })
        
        observeEvent(input$subset, {
          
          if (perf_monitoring) monitor_perf(r = r, action = "start")
          if (debug) cat(paste0("\n", now(), " - mod_page_sidenav - observer input$subset"))
          
          req(input$subset$key)
          
          # Prevent multiple changes of m$selected_study
          # We have to keep multiple observers, cause we use input variable
          if (is.na(m$selected_subset)) m$selected_subset <- input$subset$key
          if (!is.na(m$selected_subset) & m$selected_subset != input$subset$key) m$selected_subset <- input$subset$key
  
          # Reset dropdowns & uiOutput
          shiny.fluent::updateComboBox.shinyInput(session, "visit_detail", options = list(), value = NULL)
          shiny.fluent::updateComboBox.shinyInput(session, "person_status", options = list(), value = NULL)
          shiny.fluent::updateComboBox.shinyInput(session, "exclusion_reason", options = list(), value = NULL)
          sapply(c("visit_detail", "person_status", "hr2", "exclusion_reason_div"), function(element){
            sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::hide)
          })
          sapply(c("person", "hr1"), function(element) sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::show))
          shinyjs::hide("exclusion_reason_div")
          output$person_info <- renderUI("")
          
          if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_page_sidenav - observer input$subset"))
        })
        
        observeEvent(m$selected_subset, {
          
          if (debug) cat(paste0("\n", now(), " - mod_page_sidenav - observer m$selected_dataset"))
          
          req(input$study$key)
          shiny.fluent::updateComboBox.shinyInput(session, "subset", options = convert_tibble_to_list(m$subsets, key_col = "id", text_col = "name"),
            value = list(key = m$selected_subset))
        })
      }
      
      if (id == "patient_level_data"){
        observeEvent(m$subset_persons, {
          
          if (debug) cat(paste0("\n", now(), " - mod_page_sidenav - observer m$subset_persons"))
          
          persons <- tibble::tibble()
          
          person_ids <- m$subset_persons$person_id
          
          if (nrow(m$subset_persons) > 0 & d$person %>% dplyr::count() %>% dplyr::pull() > 0){
            persons <- d$person %>% dplyr::filter(person_id %in% person_ids) %>% dplyr::collect() %>%
              dplyr::left_join(d$dataset_all_concepts %>% dplyr::filter(is.na(relationship_id)) %>% dplyr::select(gender_concept_id = concept_id_1, gender_concept_name = concept_name_1), by = "gender_concept_id")
          }
          
          if (nrow(persons) == 0){
            # Set selected_person to NA, not to display a chart when no person is selected
            m$selected_person <- NA_integer_
            shiny.fluent::updateComboBox.shinyInput(session, "person", options = list(), value = NULL, errorMessage = i18n$t("no_person_in_subset")) 
          }
          
          if (nrow(persons) > 0){
            # Order persons by person_id
            persons <- persons %>% dplyr::arrange(person_id)
            
            # Update persons dropdown
            shiny.fluent::updateComboBox.shinyInput(session, "person", 
              options = convert_tibble_to_list(data = persons %>% dplyr::slice_head(n = 100) %>% dplyr::mutate(name_display = paste0(person_id, " - ", gender_concept_name)),
                key_col = "person_id", text_col = "name_display"), value = NULL)
          }
          
          # Reset other dropdowns & uiOutput
          shiny.fluent::updateComboBox.shinyInput(session, "visit_detail", options = list(), value = NULL)
          shiny.fluent::updateComboBox.shinyInput(session, "person_status", options = list(), value = NULL)
          shiny.fluent::updateComboBox.shinyInput(session, "exclusion_reason", options = list(), value = NULL)
          sapply(c("visit_detail", "person_status", "hr2", "exclusion_reason_div"), function(element){
            sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::hide)
          })
          sapply(c("person", "hr1"), function(element) sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::show))
          shinyjs::hide("exclusion_reason_div")
          output$person_info <- renderUI("")
          
        })
      }
      
      # --- --- --- --- -- -
      # Selected person ----
      # --- --- --- --- -- -
      
      if (id == "patient_level_data"){
      
        # When a text is searched in the ComboBox (with javascript script in tags$script)
        observeEvent(input$person_trigger, {
          
          if (debug) cat(paste0("\n", now(), " - mod_page_sidenav - observer input$person_trigger"))
          
          input_value <- input$person_trigger
          if (nchar(input_value) >= 2) {
            filtered_person <- d$person
            if ("tbl_lazy" %in% class(d$person)) filtered_person <- filtered_person %>%
              dplyr::filter(dplyr::sql(paste0("CAST(person_id AS TEXT) LIKE '%", input_value, "%'")))
            else filtered_person <- filtered_person %>%
                dplyr::filter(stringr::str_detect(person_id, stringr::regex(input_value, ignore_case = TRUE)))
            filtered_person <- filtered_person %>%
              dplyr::collect() %>%
              dplyr::slice_head(n = 100) %>%
              dplyr::left_join(d$dataset_all_concepts %>% dplyr::filter(is.na(relationship_id)) %>% dplyr::select(gender_concept_id = concept_id_1, gender_concept_name = concept_name_1), by = "gender_concept_id") %>%
              dplyr::mutate(name_display = paste0(person_id, " - ", gender_concept_name))
            
            shiny.fluent::updateComboBox.shinyInput(session, "person", options = convert_tibble_to_list(filtered_person, key_col = "person_id", text_col = "name_display"))
          } else {
            shiny.fluent::updateComboBox.shinyInput(session, "person", 
              options = convert_tibble_to_list(data = d$person %>% 
                  dplyr::arrange(person_id) %>%
                  dplyr::collect() %>%
                  dplyr::slice_head(n = 100) %>% 
                  dplyr::left_join(d$dataset_all_concepts %>% dplyr::filter(is.na(relationship_id)) %>% dplyr::select(gender_concept_id = concept_id_1, gender_concept_name = concept_name_1), by = "gender_concept_id") %>%
                  dplyr::mutate(name_display = paste0(person_id, " - ", gender_concept_name)),
              key_col = "person_id", text_col = "name_display"), value = NULL)
          }
        })
        
        observeEvent(input$person, {
          
          if (debug) cat(paste0("\n", now(), " - mod_page_sidenav - observer input$person"))
          if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_page_sidenav - observer input$person"))
          
          # Check if the entry exists
          
          req(input$person$text)
          
          if (length(input$person$key) == 0){
            person_text <- input$person$text
            person <- d$person %>%
              dplyr::left_join(d$dataset_all_concepts %>% dplyr::filter(is.na(relationship_id)) %>% dplyr::select(gender_concept_id = concept_id_1, gender_concept_name = concept_name_1), by = "gender_concept_id") %>%
              dplyr::mutate(name_display = paste0(person_id, " - ", gender_concept_name)) %>%
              dplyr::filter(name_display == person_text) %>% dplyr::collect()
          }
          if (length(input$person$key) > 0){
            person_key <- input$person$key
            person <- d$person %>% dplyr::filter(person_id == person_key) %>% dplyr::collect() %>%
              dplyr::left_join(d$dataset_all_concepts %>% dplyr::filter(is.na(relationship_id)) %>% dplyr::select(gender_concept_id = concept_id_1, gender_concept_name = concept_name_1), by = "gender_concept_id") %>%
              dplyr::mutate(name_display = paste0(person_id, " - ", gender_concept_name))
          }
          req(nrow(person) == 1)
          
          person_id <- person$person_id
          m$selected_person <- person_id
          
          no_stay_available <- FALSE
          if (d$visit_detail %>% dplyr::count() %>% dplyr::pull() == 0) no_stay_available <- TRUE
          if (d$visit_detail %>% dplyr::count() %>% dplyr::pull() > 0) if (d$visit_detail %>% dplyr::filter(person_id == !!person_id) %>% dplyr::count() %>% dplyr::pull() == 0) no_stay_available <- TRUE
          
          if (no_stay_available) shiny.fluent::updateComboBox.shinyInput(session, "visit_detail", options = list(), value = NULL, errorMessage = i18n$t("no_stay_available"))
  
          if (!no_stay_available){
            
            visit_detail <- d$visit_detail %>% dplyr::filter(person_id == !!person_id, is.na(visit_detail_parent_id)) %>% dplyr::collect() %>% 
              dplyr::left_join(d$dataset_all_concepts %>% dplyr::filter(is.na(relationship_id)) %>% dplyr::select(visit_detail_concept_id = concept_id_1, visit_detail_concept_name = concept_name_1), by = "visit_detail_concept_id") %>%
              dplyr::arrange(visit_detail_start_datetime)
            
            if ("visit_detail_concept_name" %in% colnames(visit_detail)){
              if (tolower(language) == "fr") visit_details <- convert_tibble_to_list(data = visit_detail %>% dplyr::mutate(name_display = paste0(visit_detail_concept_name, " - ",
                format(as.POSIXct(visit_detail_start_datetime), format = "%d-%m-%Y"), " ", tolower(i18n$t("to")), " ",  format(as.POSIXct(visit_detail_end_datetime), format = "%d-%m-%Y"))),
                key_col = "visit_detail_id", text_col = "name_display")
              else visit_details <- convert_tibble_to_list(data = visit_detail %>% dplyr::mutate(name_display = paste0(visit_detail_concept_name, " - ",
                format(as.POSIXct(visit_detail_start_datetime), format = "%Y-%m-%d"), " ", tolower(i18n$t("to")), " ",  format(as.POSIXct(visit_detail_end_datetime), format = "%Y-%m-%d"))),
                key_col = "visit_detail_id", text_col = "name_display")
            }
            else {
              if (tolower(language) == "fr") visit_details <- convert_tibble_to_list(data = visit_detail %>% dplyr::mutate(name_display = paste0(visit_detail_concept_id, " - ",
                format(as.POSIXct(visit_detail_start_datetime), format = "%d-%m-%Y"), " ", tolower(i18n$t("to")), " ",  format(as.POSIXct(visit_detail_end_datetime), format = "%d-%m-%Y"))),
                key_col = "visit_detail_id", text_col = "name_display")
              else visit_details <- convert_tibble_to_list(data = visit_detail %>% dplyr::mutate(name_display = paste0(visit_detail_concept_id, " - ",
                format(as.POSIXct(visit_detail_start_datetime), format = "%Y-%m-%d"), " ", tolower(i18n$t("to")), " ",  format(as.POSIXct(visit_detail_end_datetime), format = "%Y-%m-%d"))),
                key_col = "visit_detail_id", text_col = "name_display")
            }
            
            # Load visit_details of the person & update dropdown
            shiny.fluent::updateComboBox.shinyInput(session, "visit_detail", options = visit_details, value = NULL)
          }
          
          # Update person informations on sidenav
          style <- "display:inline-block; width:100px; font-weight:bold;"
          output$person_info <- renderUI({
            selected_person <- m$selected_person
            tagList(
              span(i18n$t("person_id"), style = style), person$person_id, br(),
              span(i18n$t("gender"), style = style), person$gender_concept_name
            )
          })
          
          sapply(c("visit_detail", "hr2"), function(element) sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::show))
          
          if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_page_sidenav - observer input$person"))
        })
      }
      
      # --- --- --- --- --- --- --
      # Selected visit_detail ----
      # --- --- --- --- --- --- --
      
      if (id == "patient_level_data"){
        observeEvent(input$visit_detail, {
          
          if (perf_monitoring) monitor_perf(r = r, action = "start")
          if (debug) cat(paste0("\n", now(), " - mod_page_sidenav - observer input$visit_detail"))
          
          m$selected_visit_detail <- input$visit_detail$key
  
          # Update person informations on sidenav
  
          style <- "display:inline-block; width:100px; font-weight:bold;"
          
          person_id <- m$selected_person
          person <- d$person %>% dplyr::filter(person_id == !!person_id) %>% dplyr::collect() %>%
            dplyr::left_join(d$dataset_all_concepts %>% dplyr::filter(is.na(relationship_id)) %>% dplyr::select(gender_concept_id = concept_id_1, gender_concept_name = concept_name_1), by = "gender_concept_id")
          
          visit_detail_id <- input$visit_detail$key
          visit_detail <- d$visit_detail %>% dplyr::filter(visit_detail_id == !!visit_detail_id) %>% dplyr::collect() %>%
            dplyr::left_join(d$dataset_all_concepts %>% dplyr::filter(is.na(relationship_id)) %>% dplyr::select(visit_detail_concept_id = concept_id_1, visit_detail_concept_name = concept_name_1), by = "visit_detail_concept_id")
          
          if (!is.na(person$birth_datetime)) age <- lubridate::interval(person$birth_datetime, visit_detail$visit_detail_start_datetime) / lubridate::years(1)
          else if (is.na(person$birth_datetime) & !is.na(person$year_of_birth)) age <- as.numeric(format(visit_detail$visit_detail_start_datetime, "%Y")) - person$year_of_birth
          else age <- NA_integer_
          
          age_div <- tagList(round(age, 0), " ", i18n$t("years"))
          if (!is.na(age) & age <= 2) age_div <- tagList(round(age * 12, 0), " ", i18n$t("months"))
  
          if ("visit_detail_concept_name" %in% names(visit_detail)) visit_detail_concept_name <- visit_detail %>% dplyr::pull(visit_detail_concept_name)
          else visit_detail_concept_name <- visit_detail %>% dplyr::pull(visit_detail_concept_id)
          
          output$person_info <- renderUI({
            tagList(
              span(i18n$t("person_id"), style = style), m$selected_person, br(),
              span(i18n$t("gender"), style = style), person$gender_concept_name, br(), br(),
              span(i18n$t("visit_detail_id"), style = style), m$selected_visit_detail, br(),
              span(i18n$t("age"), style = style), age_div, br(),
              span(i18n$t("hosp_unit"), style = style), visit_detail_concept_name, br(),
              span(i18n$t("from"), style = style), visit_detail$visit_detail_start_datetime %>% format_datetime(language), br(),
              span(i18n$t("to"), style = style), visit_detail$visit_detail_end_datetime %>% format_datetime(language))
          })
          
          if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_page_sidenav - observer input$visit_detail"))
        })
      }
    }
  })
}
