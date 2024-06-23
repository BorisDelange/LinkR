#' Convert a tibble to list
#' 
#' @description Converts a tibble to a list for input options in ComboBox & Dropdowns of shiny.fluent library
#' @param data A tibble or a dataframe containing data
#' @param key_col Name of the column containing the key (character)
#' @param text_col Name of the column containing the text (character)
#' @param null_value Add a null value at the beginning (logical)
#' @param i18n Translator object from shiny.i18n library
#' @return A list with this structure : list(list(key = "my_key1", text = "my_text1"), list(key = "my_key2", text = "my_text2"))
#' @examples
#' data <- tibble::tribble(~key, ~text, "my_key1", "my_text1", "my_key2", "my_text2")
#' my_list <- convert_tibble_to_list(data = data, key_col = "key", text_col = "text", null_value = TRUE, i18n = i18n)
#' print(my_list)
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

#' Is integer
#' 
#' @description Test if a number is an integer or an integer64
#' @param x Number to test (int / int64 / numeric)
#' @examples is_integer_or_integer64(634L)
is_integer_or_integer64 <- function(x) {
  is.integer(x) || bit64::is.integer64(x)
}

#' Get system time
#' 
#' @description Get system time with Sys.time() function
#' @param format Format to apply to Sys.time() result
#' @examples now()
now <- function(format = "%Y-%m-%d %H:%M:%S"){
  format(Sys.time(), format)
}

`%not_in%` <- Negate(`%in%`)