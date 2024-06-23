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