#' Render cards where user has no access
#' 
#' @param ns Shiny namespace
#' @param name Name of the card
#' @param i18n Translator object from shiny.i18n library
#' @examples 
#' \dontrun{
#' render_settings_code_card(ns = NS("settings_datasets"), name = "edit_dataset_code_card", i18n = i18n)
#' }
forbidden_card <- function(ns = character(), name = character(), i18n = character()){
  shinyjs::hidden(
    div(
      id = ns(paste0(name, "_forbidden")),
      make_card("",
        div(shiny.fluent::MessageBar(i18n$t("unauthorized_access_page"), messageBarType = 5), style = "margin-top:10px;")
      )
    )
  )
}

#' Render UI of settings creation card
#' 
#' @param i18n Translator object from shiny.i18n library
#' @param ns Shiny namespace
#' @param id ID of current tab / page (character)
#' @param title Title used to create the card (character)
#' @param textfields A character vector containing distinct textfields to render in the card (character)
#' @param textfields_width Width of the textfields, CSS code, so it could be "100\%", "200px" etc (character)
#' @param dropdowns A character vector containing distinct dropdowns to render in the card (character)
#' @param dropdowns_width Width of the dropdowns, CSS code, so it could be "100\%", "200px" etc (character)
#' @return Shiny UI elements / HTML code
#' @examples 
#' \dontrun{
#' render_settings_creation_card(i18n = i18n, ns = NS("settings_dataset"), title = "create_dataset",
#' textfields = c("name", "description"), dropdowns = "data_source")
#' }
render_settings_creation_card <- function(i18n = character(), ns = character(), id = character(), title = character(), 
  textfields = character(), textfields_width = "200px", dropdowns = character(), dropdowns_width = "200px"){
  
  div(id = ns("creation_card"),
    make_card(
      title = i18n$t(title),
      content = div(
        shiny.fluent::Stack(
          # Horizontal alignment, with gap of 50 px between elements
          horizontal = TRUE, tokens = list(childrenGap = 50),
          # For each textfield, use make_textfield function
          lapply(textfields, function(label){
            if (label == "password") make_textfield(i18n = i18n, ns = ns, label = label, id = label, width = textfields_width, type = "password", canRevealPassword = TRUE)
            else make_textfield(i18n = i18n, ns = ns, label = label, id = label, width = textfields_width)
          })
        ),
        shiny.fluent::Stack(
          horizontal = TRUE, tokens = list(childrenGap = 50),
          lapply(dropdowns, function(label){
            make_dropdown(i18n = i18n, ns = ns, label = label, id = label, width = dropdowns_width)
          })
        ), br(),
        shiny.fluent::PrimaryButton.shinyInput(ns("add"), i18n$t("add"))
      )
    )
  )
}

#' Render UI of settings datatable card
#' 
#' @param i18n Translator object from shiny.i18n library
#' @param ns Shiny namespace
#' @param div_id ID of the div, to show or hide with toggles, default = "datatable_card" (character)
#' @param output_id ID of div & DTOutput, allows to have multiple management_datatable in one tab, default = "management_datatable" (character)
#' @param title Title used to create the card, it will be translated with translate function (character)
#' @examples 
#' \dontrun{
#' render_settings_datatable_card(i18n = i18n, ns = ns, output_id = "management_datatable", title = "datasets_management")
#' }
render_settings_datatable_card <- function(i18n = character(), ns = character(), div_id = "datatable_card",
  output_id = "management_datatable", title = character(), inputs = character(), dropdown_multiselect = FALSE){
  
  inputs_div <- tagList()
  
  if (length(inputs) > 0){
    
    for (name in names(inputs)){
      
      input_type <- inputs[[name]]
      if (input_type == "textfield") inputs_div <- tagList(inputs_div, make_textfield(i18n = i18n, ns = ns, label = name, id = name, width = "300px"))
      if (input_type == "dropdown") inputs_div <- tagList(inputs_div, make_dropdown(i18n = i18n, ns = ns, label = name, id = name, width = "300px", multiSelect = dropdown_multiselect))
    }
    
    inputs_div <- shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),
      inputs_div,
      div(shiny.fluent::PrimaryButton.shinyInput(ns("add"), i18n$t("add")), style = "margin-top:39px;")
    )
  }
  
  div(id = ns(div_id),
    make_card(i18n$t(title),
      div(
        inputs_div,
        div(DT::DTOutput(ns(output_id)), style = "z-index:2"),
        div(
          shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 10),
            shiny.fluent::PrimaryButton.shinyInput(ns("management_save"), i18n$t("save")),
            shiny.fluent::DefaultButton.shinyInput(ns("delete_selection"), i18n$t("delete_selection"))
          ),
          style = "position:relative; z-index:1; margin-top:-30px; width:500px;")
      )
    ), br()
  )
}

#' Render UI of settings default elements
#' 
#' @description Set default UI elements on top of the page : message_bar outputs, react output to confirm delete of a table element
#' 
#' @param ns Shiny namespace
#' @return Shiny UI elements / HTML code
#' @examples
#' \dontrun{
#' render_settings_default_elements(ns = NS("settings_dataset"))
#' }
render_settings_default_elements <- function(ns = character()){
  
  message_bars <- tagList()
  for (i in 1:20) message_bars <- tagList(message_bars, shiny::uiOutput(ns(paste0("message_bar", i))))
  
  div(
    div(class = "message_bars", message_bars), 
    shiny.fluent::reactOutput(ns("delete_confirm"))
  )
}
