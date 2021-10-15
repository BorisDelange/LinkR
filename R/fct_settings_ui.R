##########################################
# Default elements                       #
##########################################

#' Render UI of settings default elements
#' 
#' @description Set default UI elements on top of the page : message_bar outputs, react output to confirm delete of a table element
#' 
#' @param ns Shiny namespace
#' @return Shiny UI elements / HTML code
#' @examples
#' settings_default_elements(ns = NS("settings_datamart"))

render_settings_default_elements <- function(ns = shiny::NS()){
  tagList(shiny::uiOutput(ns("message_bar1")), shiny::uiOutput(ns("message_bar2")), shiny::uiOutput(ns("message_bar3")), 
    shiny::uiOutput(ns("message_bar4")), shiny::uiOutput(ns("message_bar5")), shiny.fluent::reactOutput(ns("delete_confirm")))
}

# Delete asap
settings_default_elements <- function(ns = shiny::NS(), prefix = ""){
  tagList(shiny::uiOutput(ns("message_bar1")), shiny::uiOutput(ns("message_bar2")), shiny::uiOutput(ns("message_bar3")), 
    shiny::uiOutput(ns("message_bar4")), shiny::uiOutput(ns("message_bar5")), shiny.fluent::reactOutput(ns("delete_confirm")))
}

##########################################
# Toggle card                            #
##########################################

#' Render UI of settings toggle card
#' 
#' @description At the top of main page, there's a card with toggle buttons, it allows to show or hide distinct cards of the page.
#' @param ns Shiny namespace
#' @param cards A list containing distinct cards (list)
#' @param activated Which toggles are set to ON (character)
#' @param language Language used (character)
#' @return Shiny UI elements / HTML code
#' @examples
#' cards <- list(
#'   list(key = "creation_card", label = "create_datamart"),
#'   list(key = "datatable_card", label = "datamarts_management"),
#'   list(key = "edit_code_card", label = "edit_datamart_code"))
#' settings_toggle_card(ns = NS("settings_datamart"), cards = cards, activated = c("creation_card", "datatable_card"), language = "EN")

render_settings_toggle_card <- function(language = "EN", ns = shiny::NS(), cards = list(), activated = ""){
  
  toggles <- tagList()
  # For each card, create a toggle
  sapply(cards, function(card){
    if (card$label != "") toggles <<- tagList(toggles, 
      make_toggle(language, ns, label = card$label, 
        id = paste0(card$key, "_toggle"), value = ifelse(card$key %in% activated, TRUE, FALSE), inline = TRUE))
  })
  # Render card with distinct togglesmo
  make_card("",
    shiny.fluent::Stack(
      horizontal = TRUE, tokens = list(childrenGap = 10), toggles
    )
  )
}

# Delete asap
settings_toggle_card <- function(language = "EN", ns = shiny::NS(), cards = list(), activated = ""){
  
  toggles <- tagList()
  # For each card, create a toggle
  sapply(cards, function(card){
    if (card$label != "") toggles <<- tagList(toggles, 
      make_toggle(language, ns, label = card$label, 
        id = paste0(card$key, "_toggle"), value = ifelse(card$key %in% activated, TRUE, FALSE), inline = TRUE))
  })
  # Render card with distinct toggles
  make_card("",
    shiny.fluent::Stack(
      horizontal = TRUE, tokens = list(childrenGap = 10), toggles
    )
  )
}

##########################################
# Creation card                          #
##########################################

#' Render UI of settings creation card
#' 
#' @param language Language used (character)
#' @param ns Shiny namespace
#' @param title Title used to create the card, it will be translated with translate function (character)
#' @param textfields A character vector containing distinct textfields to render in the card (character)
#' @param textfields_width Width of the textfields, CSS code, so it could be "100%", "200px" etc (character)
#' @param dropdowns A character vector containing distinct dropdowns to render in the card (character)
#' @param dropdowns_width Width of the dropdowns, CSS code, so it could be "100%", "200px" etc (character)
#' @return Shiny UI elements / HTML code
#' @examples 
#' render_settings_creation_card(language = "EN", ns = NS("settings_datamart"), title = "create_datamart",
#' textfields = c("name", "description"), dropdowns = "data_source")

render_settings_creation_card <- function(language = "EN", ns = shiny::NS(), title = character(), prefix = "", 
  textfields = character(), textfields_width = "200px", dropdowns = character(), dropdowns_width = "200px"){
  
  div(id = ns("creation_card"),
    make_card(
      title = translate(language, title),
      content = div(
        shiny.fluent::Stack(
          # Horizontal alignment, with gap of 50 px between elements
          horizontal = TRUE, tokens = list(childrenGap = 50),
          # For each textfield, use make_textfield function
          lapply(textfields, function(label){
            make_textfield(language = language, ns = ns, label = label, id = label, width = textfields_width)
          })
        ),
        shiny.fluent::Stack(
          horizontal = TRUE, tokens = list(childrenGap = 50),
          lapply(dropdowns, function(label){
            # Allow multiSelect for thesaurus, column data source
            multiSelect <- FALSE
            if (prefix == "thesaurus") multiSelect <- TRUE
            make_dropdown(language = language, ns = ns, label = label, id = label, multiSelect = multiSelect, width = dropdowns_width)
          })
        ), br(),
        shiny.fluent::PrimaryButton.shinyInput(ns("add"), translate(language, "add"))
      )
    )
  )
}

##########################################
# Datatable card                         #
##########################################

settings_datatable_card <- function(language, ns, title, prefix){
  div(id = ns("datatable_card"),
      make_card(translate(language, title),
        div(
          DT::DTOutput(ns(paste0(prefix, "_management_datatable"))),
          shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "_management_save")), translate(language, "save"), style = "top:-20px;")
        )
      )
  ) -> result
  result
}


##########################################
# Options card                           #
##########################################

settings_options_card <- function(language, ns, id, r, category_filter, link_id_filter, title, prefix){
  options <- r$options %>% dplyr::filter(category == category_filter, link_id == link_id_filter)
  
  people_picker <- ""
  toggles <- ""
  dropdowns <- ""
  options_by_cat <- id_get_other_name(id, "options_by_cat")
  
  ##########################################
  # Users allowed to read option           #
  ##########################################
  
  if("user_allowed_read" %in% options_by_cat){
    # List of users in the database
    form_options <-
      r$users %>%
      dplyr::filter(!deleted) %>%
      dplyr::left_join(r$users_accesses_statuses %>% dplyr::select(user_status_id = id, user_status = name), by = "user_status_id") %>%
      dplyr::transmute(key = id, imageInitials = paste0(substr(firstname, 0, 1), substr(lastname, 0, 1)),
        text = paste0(firstname, " ", lastname), secondaryText = user_status)
    
    # If this is study options, we have to show only users who have access to the parent datamart
    if(category_filter == "study"){
      datamart_id <- r$studies %>% dplyr::filter(id == link_id_filter) %>% dplyr::pull(datamart_id)
      users_allowed_datamart <- 
        r$options %>% 
        dplyr::filter(category == "datamart", link_id == datamart_id, name == "user_allowed_read") %>%
        dplyr::pull(value_num)
      form_options <- form_options %>% dplyr::filter(key %in% users_allowed_datamart)
    }
    
    # Users already allowed
    value <-
      form_options %>%
      dplyr::mutate(n = 1:dplyr::n()) %>%
      dplyr::inner_join(
        options %>%
          dplyr::filter(!deleted, name == "user_allowed_read") %>%
          dplyr::select(key = value_num),
        by = "key"
      ) %>%
      dplyr::pull(key)
    people_picker <- make_people_picker(language, ns, paste0(prefix, "_", id_get_other_name(id, "singular_form"), "_users_allowed_read"),
      options = form_options, value = value, width = "100%")
  }
  
  ##########################################
  # Show only aggregated data option       #
  ##########################################
  
  if ("show_only_aggregated_data" %in% options_by_cat){
    value_show_only_aggregated_data <- options %>% dplyr::filter(name == "show_only_aggregated_data") %>% dplyr::pull(value_num)
    toggles <- tagList(
      htmltools::br(), 
      shiny.fluent::Stack(
        horizontal = TRUE, tokens = list(childrenGap = 10),
        make_toggle(language, ns,
          label = "show_only_aggregated_data",
          id = paste0(prefix, "_", id_get_other_name(id, "singular_form"), "_show_only_aggregated_data"), value = value_show_only_aggregated_data, inline = TRUE)
      )
    )
  }
  
  ##########################################
  # Result                                 #
  ##########################################
  
  div(id = ns("options_card"),
    make_card(tagList(translate(language, title), span(paste0(" (ID = ", link_id_filter, ")"), style = "font-size: 15px;")),
      div(
        toggles, people_picker, htmltools::br(),
        shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10), dropdowns),
        shiny.fluent::PrimaryButton.shinyInput(ns(paste0(prefix, "_options_save")), translate(language, "save"))
      )
    )
  )
}

##########################################
# Edit code card                         #
##########################################

#' Render UI of edit_code card
#' 
#' @param ns Shiny namespace
#' @param id ID of the current page, format = "settings_[PAGE]" (character)
#' @param title Title of the card (character)
#' @param code Code to show in ShinyAce editor (character)
#' @param link_id ID allows to link with code table (integer)
#' @param language Language used (character)

render_settings_code_card <- function(ns = shiny::NS(), id = character(), title = character(), code = character(), link_id = integer(), language = "EN"){
  # For plugin page, choose between UI code or Server code
  choice_ui_server <- tagList()
  
  if (id == "settings_plugins"){
    shiny.fluent::ChoiceGroup.shinyInput(ns("edit_code_choice_ui_server"), value = "ui", options = list(
      list(key = "ui", text = translate(language, "ui")),
      list(key = "server", text = translate(language, "server"))
    ), className = "inline_choicegroup") -> choice_ui_server
  }
  
  div(id = ns("edit_code_card"),
    # Show current ID in the title
    make_card(tagList(translate(language, title), span(paste0(" (ID = ", link_id, ")"), style = "font-size: 15px;")),
      div(
        choice_ui_server,
        div(shinyAce::aceEditor(ns("ace_edit_code"), code, mode = "r", 
          autoScrollEditorIntoView = TRUE, minLines = 30, maxLines = 1000#, 
          # hotkeys = list(run_key = list(win = "Ctrl-Enter|Ctrl-Shift-Enter", mac = "CMD-ENTER|CMD-SHIFT-ENTER"))
        ), style = "width: 100%;"),
        shiny.fluent::PrimaryButton.shinyInput(ns("edit_code_save"), translate(language, "save")), " ",
        shiny.fluent::PrimaryButton.shinyInput(ns("execute_code"), translate(language, "execute_code")), 
        htmltools::br(), htmltools::br(),
        div(shiny::verbatimTextOutput(ns("code_result")), 
          style = "width: 99%; border-style: dashed; border-width: 1px; padding: 0px 8px 0px 8px; margin-right: 5px;")
      )
    )
  )
}