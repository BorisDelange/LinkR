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
  
  if (grepl("^home", id)){
    div(class = "sidenav",
      shiny.fluent::Nav(
        groups = list(
          list(links = list(
            list(name = i18n$t("home"), key = "home", url = shiny.router::route_link("home")),
            list(name = i18n$t("get_started"), key = "home_get_started", url = shiny.router::route_link("home/get_started")),
            list(name = i18n$t("tutorials"), key = "home_tutorials", url = shiny.router::route_link("home/tutorials")),
            list(name = i18n$t("resources"), key = "home_resources", url = shiny.router::route_link("home/resources"))
            )
          )
        ),
        initialSelectedKey = id,
        selectedKey = id,
        styles = list(
          root = list(
            height = "100%",
            boxSizing = "border-box",
            overflowY = "auto"
          )
        )
      )
    ) -> result
  }

  
  # --- --- --- --
  # Dropdowns ----
  # --- --- --- --
  
  if (id %in% c("my_studies", "my_subsets", "messages", "vocabularies", "scripts", "patient_level_data", "aggregated_data")){
    
    dropdowns <- function(names){
      
      result <- tagList()
      
      sapply(names, function(name){
        
        width <- "250px"
        
        result <<- tagList(result,
          div(id = ns(paste0(name, "_title")), class = "input_title", i18n$t(name)),
          shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 5),
            div(shiny.fluent::ComboBox.shinyInput(ns(name), allowFreeForm = FALSE, autoComplete = "on"), style = paste0("min-width:", width, "; max-width:", width, ";"))
          )
        )
      })
      
      result
    }
  }
  
  # --- --- --- ---
  # My studies ----
  # --- --- --- ---
  
  if (id == "my_studies") div(class = "sidenav", dropdowns(c("dataset"))) -> result
  
  # --- --- --- ---
  # My subsets ----
  # --- --- --- ---
  
  if (id == "my_subsets") div(class = "sidenav", dropdowns(c("dataset", "study"))) -> result
  
  # --- --- --- -
  # Messages ----
  # --- --- --- -
  
  if (id == "messages") div(class = "sidenav", dropdowns(c("dataset", "study"))) -> result
  
  # --- --- --- --- -
  # Vocabularies ----
  # --- --- --- --- -
  
  if (id == "vocabularies") div(class = "sidenav", dropdowns(c("dataset"))) -> result
  
  # --- --- -- -
  # Scripts ----
  # --- --- -- -
  
  if (id == "scripts") div(class = "sidenav", dropdowns(c("dataset"))) -> result
  
  # --- --- --- --- --- ---
  # Patient-level data ----
  # --- --- --- --- --- ---
  
  if (id == "patient_level_data"){
    div(class = "sidenav",
      div(i18n$t("data"), class = "input_title", style = "font-size:14.5px;"),
      div(
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 0),
          shiny.fluent::PrimaryButton.shinyInput(ns("data_page_ind"), i18n$t("individual"), style = "width:125px;"), 
          shiny.fluent::DefaultButton.shinyInput(ns("data_page_agg"), i18n$t("aggregated"), style = "width:125px;")
        ), style = "width:250px;"
      ),
      dropdowns(c("dataset", "study", "subset")),
      br(), div(id = ns("hr1"), hr()),
      dropdowns(c("person", "visit_detail")),
      br(), div(id = ns("hr2"), hr()),
      uiOutput(ns("person_info"))
    ) -> result
  }
  
  # --- --- --- --- -- -
  # Aggregated data ----
  # --- --- --- --- -- -
  
  if (id == "aggregated_data") div(
    class = "sidenav", 
    div(i18n$t("data"), class = "input_title", style = "font-size:14.5px;"),
    div(
      shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 0),
        shiny.fluent::DefaultButton.shinyInput(ns("data_page_ind"), i18n$t("individual"), style = "width:125px;"), 
        shiny.fluent::PrimaryButton.shinyInput(ns("data_page_agg"), i18n$t("aggregated"), style = "width:125px;")
      ), style = "width:250px;"
    ),                               
    dropdowns(c("dataset", "study", "subset"))
  ) -> result
  
  # --- --- --- --- --- --- --
  # Plugins patient-level ----
  # --- --- --- --- --- --- --
  
  if (id == "plugins_patient_lvl"){
    
    div(
      class = "sidenav", 
      div(i18n$t("data"), class = "input_title", style = "font-size:14.5px;"),
      div(
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 0),
          shiny.fluent::PrimaryButton.shinyInput(ns("plugins_page_ind"), i18n$t("individual"), style = "width:125px;"), 
          shiny.fluent::DefaultButton.shinyInput(ns("plugins_page_agg"), i18n$t("aggregated"), style = "width:125px;")
        ), style = "width:250px;"
      )
    ) -> result
  }
  
  # --- --- --- --- --- ---
  # Plugins aggregated ----
  # --- --- --- --- --- ---
  
  if (id == "plugins_aggregated"){
    
    div(
      class = "sidenav", 
      div(i18n$t("data"), class = "input_title", style = "font-size:14.5px;"),
      div(
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 0),
          shiny.fluent::DefaultButton.shinyInput(ns("plugins_page_ind"), i18n$t("individual"), style = "width:125px;"), 
          shiny.fluent::PrimaryButton.shinyInput(ns("plugins_page_agg"), i18n$t("aggregated"), style = "width:125px;")
        ), style = "width:250px;"
      )
    ) -> result
  }
  
  # --- --- --- -
  # Settings ----
  # --- --- --- -
  
  if (grepl("^settings", id)){
    
    links_data_management <- list()
    lapply(c("data_sources", "datasets", "vocabularies"), function(page){
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
    
    if (debug) print(paste0(Sys.time(), " - mod_plugins - start"))
    
    # --- --- -- -
    # Plugins ----
    # --- --- -- -
    
    if (id %in% c("plugins_patient_lvl", "plugins_aggregated")){
      
      # Changing page between patient-lvl & aggregated plugins
      
      r$plugins_page <- "plugins_patient_lvl"
      
      if (id == "plugins_patient_lvl") observeEvent(input$plugins_page_agg, {
        shiny.router::change_page("plugins_aggregated")
        r$plugins_page <- "plugins_aggregated"
      })
      if (id == "plugins_aggregated") observeEvent(input$plugins_page_ind, {
        shiny.router::change_page("plugins_patient_lvl")
        r$plugins_page <- "plugins_patient_lvl"
      })
    }
    
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
      
      observeEvent(r$datasets, {
        
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer r$datasets"))
        
        # Update dropdown
        shiny.fluent::updateComboBox.shinyInput(session, "dataset", 
          options = convert_tibble_to_list(r$datasets %>% dplyr::arrange(name), key_col = "id", text_col = "name"), value = NULL)
        
        sapply(c("study", "subset", "person", "visit_detail", "person_status", "hr1", "hr2", "exclusion_reason_div"), function(element){
          sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::hide)
        })
        shinyjs::hide("exclusion_reason_div")
      })
      
      observeEvent(input$dataset, {
        
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer input$dataset"))
        
        # Save value in r$selected_dropdown, to update patient-level data dropdowns AND aggregated data dropdowns
        r$selected_dataset <- input$dataset$key
        
        sapply(c("subset", "person", "visit_detail", "person_status", "hr1", "hr2", "exclusion_reason_div"), function(element){
          sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::hide)
        })
        sapply(c("study"), function(element) sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::show))
        
        # Reset dropdowns & uiOutput
        # Hide exclusion_reason dropdown
        
        shiny.fluent::updateComboBox.shinyInput(session, "subset", options = list(), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "person", options = list(), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "visit_detail", options = list(), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "person_status", options = list(), value = NULL)
        shiny.fluent::updateComboBox.shinyInput(session, "exclusion_reason", options = list(), value = NULL)
        shinyjs::hide("exclusion_reason_div")
        output$person_info <- renderUI("")
      })
      
      # Update the two pages dropdowns (patient-level data page & aggregated data page)
      observeEvent(r$selected_dataset, {
        
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer r$selected_dataset"))
        
        shiny.fluent::updateComboBox.shinyInput(session, "dataset", options = 
          convert_tibble_to_list(r$datasets %>% dplyr::arrange(name), key_col = "id", text_col = "name"), 
            value = list(key = r$selected_dataset))
        
        # Reset m$selected_study (to reset main display)
        if (length(m$selected_study) == 0) m$selected_study <- NA_integer_
        if (!is.na(m$selected_study)) m$selected_study <- NA_integer_
        
        # Reset of data variables, load of vocabulary code happens in mod_my_studies.R
        # With this solution, code is run only one time
        # Here, code is run for each page
      })
      
      # --- --- --- --- -
      # Selected study ----
      # --- --- --- --- -
      
      observeEvent(r$studies, {
        
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer r$studies"))
        
        if (nrow(r$studies) == 0) shiny.fluent::updateComboBox.shinyInput(session, "study", options = list(), value = NULL, 
          errorMessage = i18n$t("no_study_available"))
        
        if (nrow(r$studies) > 0) shiny.fluent::updateComboBox.shinyInput(session, "study",
          options = convert_tibble_to_list(r$studies %>% dplyr::arrange(name), key_col = "id", text_col = "name"), value = NULL)
      })
      
      observeEvent(input$study, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer input$study"))

        req(input$study$key)
        
        # Prevent multiple changes of m$selected_study
        # We have to keep multiple observers, cause we use input variable
        if (is.na(m$selected_study)) m$selected_study <- input$study$key
        if (!is.na(m$selected_study) & m$selected_study != input$study$key) m$selected_study <- input$study$key

        # Reset dropdowns & uiOutput
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
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_page_sidenav - observer input$study"))
      })
      
      observeEvent(m$selected_study, {
        
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer m$selected_study"))
        
        req(input$dataset$key & !is.na(m$selected_study))
        studies <- r$studies %>% dplyr::filter(dataset_id == input$dataset$key)
        
        shiny.fluent::updateComboBox.shinyInput(session, "study", options =
            convert_tibble_to_list(studies %>% dplyr::arrange(name), key_col = "id", text_col = "name"),
          value = list(key = m$selected_study))
        
        # Load of subsets is done in mod_my_studies.R
        # With this solution, code is run only one time
      })
      
      observeEvent(m$subsets, {
        req(!is.na(m$selected_study))
        
        # Update subset dropdown
        if (nrow(m$subsets) == 0) shiny.fluent::updateComboBox.shinyInput(session, "subset", options = list(), value = NULL, errorMessage = i18n$t("no_subset_available"))
        if (nrow(m$subsets) > 0) shiny.fluent::updateComboBox.shinyInput(session, "subset", options = convert_tibble_to_list(m$subsets, key_col = "id", text_col = "name"), value = NULL)
      })
      
      # --- --- --- --- --
      # Selected subset ----
      # --- --- --- --- --
      
      observeEvent(m$subsets, {
        
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer m$subsets"))
        
        if (nrow(m$subsets) == 0) shiny.fluent::updateComboBox.shinyInput(session, "subset", options = list(), value = NULL, 
          errorMessage = i18n$t("no_study_available"))
        
        if (nrow(m$subsets) > 0) shiny.fluent::updateComboBox.shinyInput(session, "subset",
          options = convert_tibble_to_list(m$subsets %>% dplyr::arrange(name), key_col = "id", text_col = "name"), value = NULL)
      })
      
      observeEvent(input$subset, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer input$subset"))
        
        req(input$subset$key)
        
        # Prevent multiple changes of m$selected_study
        # We have to keep multiple observers, cause we use input variable
        if (is.na(m$selected_subset)) m$selected_subset <- input$subset$key
        if (!is.na(m$selected_subset) & m$selected_subset != input$subset$key) m$selected_subset <- input$subset$key

        # Reset dropdown & uiOutput
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
        
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer m$selected_dataset"))
        
        req(input$study$key)
        shiny.fluent::updateComboBox.shinyInput(session, "subset", options = convert_tibble_to_list(m$subsets, key_col = "id", text_col = "name"),
          value = list(key = m$selected_subset))
      })
      
      observeEvent(m$subset_persons, {
        
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer m$subset_persons"))
        
        persons <- tibble::tibble()
        
        if (nrow(m$subset_persons) > 0 & nrow(d$person) > 0){
          persons <- d$person %>% dplyr::inner_join(m$subset_persons %>% dplyr::select(person_id), by = "person_id")
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
            options = convert_tibble_to_list(data = persons %>% dplyr::mutate(name_display = paste0(person_id, " - ", gender_concept_name)),
              key_col = "person_id", text_col = "name_display"), value = NULL)
        }
        
      })
      
      # --- --- --- --- -- -
      # Selected person ----
      # --- --- --- --- -- -
      
      observeEvent(input$person, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_page_sidenav - observer input$person"))
        
        m$selected_person <- input$person$key
        
        if (nrow(d$visit_detail %>% dplyr::filter(person_id == input$person$key)) == 0) shiny.fluent::updateComboBox.shinyInput(session, "person", options = list(), value = NULL, errorMessage = i18n$t("no_person_in_subset"))
        if (nrow(d$visit_detail %>% dplyr::filter(person_id == input$person$key)) > 0){
          
          if ("visit_detail_concept_name" %in% colnames(d$visit_detail)){
            if (tolower(language) == "fr") visit_details <- convert_tibble_to_list(data = d$visit_detail %>% dplyr::filter(person_id == input$person$key) %>% dplyr::mutate(name_display = paste0(visit_detail_concept_name, " - ",
              format(as.POSIXct(visit_detail_start_datetime), format = "%d-%m-%Y"), " ", tolower(i18n$t("to")), " ",  format(as.POSIXct(visit_detail_end_datetime), format = "%d-%m-%Y"))),
              key_col = "visit_detail_id", text_col = "name_display")
            else visit_details <- convert_tibble_to_list(data = d$visit_detail %>% dplyr::filter(person_id == input$person$key) %>% dplyr::mutate(name_display = paste0(visit_detail_concept_name, " - ",
              format(as.POSIXct(visit_detail_start_datetime), format = "%Y-%m-%d"), " ", tolower(i18n$t("to")), " ",  format(as.POSIXct(visit_detail_end_datetime), format = "%Y-%m-%d"))),
              key_col = "visit_detail_id", text_col = "name_display")
          }
          else {
            if (tolower(language) == "fr") visit_details <- convert_tibble_to_list(data = d$visit_detail %>% dplyr::filter(person_id == input$person$key) %>% dplyr::mutate(name_display = paste0(visit_detail_concept_id, " - ",
              format(as.POSIXct(visit_detail_start_datetime), format = "%d-%m-%Y"), " ", tolower(i18n$t("to")), " ",  format(as.POSIXct(visit_detail_end_datetime), format = "%d-%m-%Y"))),
              key_col = "visit_detail_id", text_col = "name_display")
            else visit_details <- convert_tibble_to_list(data = d$visit_detail %>% dplyr::filter(person_id == input$person$key) %>% dplyr::mutate(name_display = paste0(visit_detail_concept_id, " - ",
              format(as.POSIXct(visit_detail_start_datetime), format = "%Y-%m-%d"), " ", tolower(i18n$t("to")), " ",  format(as.POSIXct(visit_detail_end_datetime), format = "%Y-%m-%d"))),
              key_col = "visit_detail_id", text_col = "name_display")
          }
          
          # Load visit_details of the person & update dropdown
          shiny.fluent::updateComboBox.shinyInput(session, "visit_detail", options = visit_details, value = NULL)
        }
        
        # Update person informations on sidenav
        
        style <- "display:inline-block; width:100px; font-weight:bold;"
        output$person_info <- renderUI({
          tagList(
            span(i18n$t("person_id"), style = style), m$selected_person, br(),
            span(i18n$t("gender"), style = style), d$person %>% dplyr::filter(person_id == m$selected_person) %>% dplyr::pull(gender_concept_name)
          )
        })
        
        sapply(c("visit_detail", "hr2"), function(element) sapply(c(element, paste0(element, "_title"), paste0(element, "_page")), shinyjs::show))
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_page_sidenav - observer input$person"))
      })
      
      # --- --- --- --- --- --- --
      # Selected visit_detail ----
      # --- --- --- --- --- --- --
      
      observeEvent(input$visit_detail, {
        
        if (perf_monitoring) monitor_perf(r = r, action = "start")
        if (debug) print(paste0(Sys.time(), " - mod_page_sidenav - observer input$visit_detail"))
        
        m$selected_visit_detail <- input$visit_detail$key

        # Update person informations on sidenav

        style <- "display:inline-block; width:100px; font-weight:bold;"
        
        gender <- d$person %>% dplyr::filter(person_id == m$selected_person) %>% dplyr::pull(gender_concept_name)
        
        birth_datetime <- d$person %>% dplyr::filter(person_id == m$selected_person) %>% dplyr::pull(birth_datetime)
        year_of_birth <- d$person %>% dplyr::filter(person_id == m$selected_person) %>% dplyr::pull(year_of_birth)
        visit_detail_start_datetime <- d$visit_detail %>% dplyr::filter(visit_detail_id == input$visit_detail$key) %>% dplyr::pull(visit_detail_start_datetime)
        
        if (!is.na(birth_datetime)) age <- lubridate::interval(birth_datetime, visit_detail_start_datetime) / lubridate::years(1)
        if (is.na(birth_datetime)) age <- as.numeric(format(visit_detail_start_datetime, "%Y")) - year_of_birth

        age_div <- tagList(age, " ", i18n$t("years"))
        if (!is.na(age) & age <= 2) age_div <- tagList(round(age * 12, 0), " ", i18n$t("months"))

        visit_detail <- d$visit_detail %>% dplyr::filter(visit_detail_id == m$selected_visit_detail)
        visit_detail_start_datetime <- visit_detail %>% dplyr::pull(visit_detail_start_datetime) %>% format_datetime(language)
        visit_detail_end_datetime <- visit_detail %>% dplyr::pull(visit_detail_end_datetime) %>% format_datetime(language)
        if ("visit_detail_concept_name" %in% names(visit_detail)) visit_detail_concept_name <- visit_detail %>% dplyr::pull(visit_detail_concept_name)
        else visit_detail_concept_name <- visit_detail %>% dplyr::pull(visit_detail_concept_id)
        
        output$person_info <- renderUI({
          tagList(
            span(i18n$t("person_id"), style = style), m$selected_person, br(),
            span(i18n$t("gender"), style = style), gender, br(), br(),
            span(i18n$t("visit_detail_id"), style = style), m$selected_visit_detail, br(),
            span(i18n$t("age"), style = style), age_div, br(),
            span(i18n$t("hosp_unit"), style = style), visit_detail_concept_name, br(),
            span(i18n$t("from"), style = style), visit_detail_start_datetime, br(),
            span(i18n$t("to"), style = style), visit_detail_end_datetime)
        })
        
        if (perf_monitoring) monitor_perf(r = r, action = "stop", task = paste0("mod_page_sidenav - observer input$visit_detail"))
      })
    }
  })
}
