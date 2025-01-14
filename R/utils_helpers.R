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
is_integer_or_integer64 <- function(x) {
  is.integer(x) || bit64::is.integer64(x)
}

#' @noRd
now <- function(format = "%Y-%m-%d %H:%M:%OS3"){
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