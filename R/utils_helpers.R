#' @noRd
convert_tibble_to_list <- function(data = tibble::tibble(), key_col = character(), text_col = character(), null_value = FALSE, i18n = character()){
  
  # Create a null / an empty value (used in dropdowns)
  if (null_value) my_list <- list(list(key = "", text = i18n$t("none")))
  if (!null_value) my_list <- list()
  
  # If our data is not empty, for each row append the list
  if (nrow(data) != 0){
    for (i in 1:nrow(data)){
      my_list <- rlist::list.append(my_list, list(key = data[[i, key_col]], text = data[[i, text_col]]))
    }
  }
  my_list
}

#' @noRd
now <- function(format = "%Y-%m-%d %H:%M:%S"){
  format(Sys.time(), format)
}

#' @noRd
remove_special_chars <- function(text){
  text %>%
    tolower() %>%
    gsub(" ", "_", .) %>%
    gsub("[àáâãäå]", "a", .) %>%
    gsub("[èéêë]", "e", .) %>%
    gsub("[ìíîï]", "i", .) %>%
    gsub("[òóôõö]", "o", .) %>%
    gsub("[ùúûü]", "u", .) %>%
    gsub("ç", "c", .) %>%
    gsub("ñ", "n", .) %>%
    gsub("ÿ", "y", .) %>%
    gsub("ß", "ss", .) %>%
    gsub("[^a-z0-9_]", "", .)
}

#' @noRd
`%not_in%` <- Negate(`%in%`)

#' @noRd
try_catch <- function(trigger = character(), code, widget_id = NA_integer_){

  for (obj_name in c("id", "log_level", "output", "i18n", "ns")) assign(obj_name, get(obj_name, envir = parent.frame()))

  if (!is.na(widget_id)){
    event_message <- paste0("\n[", now(), "] [EVENT] [page_id = ", id, "] [widget_id = ", widget_id, "] event triggered by ", trigger)
    error_message <- paste0("\n[", now(), "] [ERROR] [page_id = ", id, "] [widget_id = ", widget_id, "] error with trigger ", trigger, " - error = ")
  }
  else {
    event_message <- paste0("\n[", now(), "] [EVENT] [page_id = ", id, "] event triggered by ", trigger)
    error_message <- paste0("\n[", now(), "] [ERROR] [page_id = ", id, "] error with trigger ", trigger, " - error = ")
  }

  tryCatch({
    if ("event" %in% log_level) cat(event_message)
    code
  }, error = function(e){
    if ("error" %in% log_level){
      cat(paste0(error_message, toString(e)))
      show_message_bar("an_error_occurred_see_log_for_details", "severeWarning")
    }
    else show_message_bar("an_error_occurred", "severeWarning")
  })
}