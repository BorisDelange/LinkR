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

#' Coalesce2
#' 
#' @description Returns a NA value
#' @param type Type of the variable c("char", "int") (character)
#' @param x Variable
#' @return Returns NA if variable is null or empty, returns the variable if not null & not empty
#' @examples 
#' coalesce2("char", "my_char")
coalesce2 <- function(type, x){
  if (length(x) >= 2) return(x)
  if (type == "int"){
    if (is.null(x)) return(NA_integer_)
    if (length(x) == 0) return(NA_integer_)
    if (x == "") return(NA_integer_)
    return(tryCatch(as.integer(x)))
  }
  if (type == "char"){
    if (is.null(x)) return(NA_character_)
    if(length(x) == 0) return(NA_character_)
    if (x == "") return(NA_character_)
    return(tryCatch(as.character(x)))
  }
}

#' Resize and pad image
#' 
#' @description Resize and pad a PNG image
#' @param input_path Path of the input image
#' @param output_path Path of the output image
#' @param target_width Width of the output image
#' @param target_height Height of the output image
#' @return Returns a PNG image with new dimensions
#' @examples
#' resize_and_pad_image("my_image.png", "my_image_resized.png", target_width = 318, target_height = 200)
resize_and_pad_image <- function(input_path, output_path, target_width = 318, target_height = 200, i18n = character()){
  # Check if file exists
  if (!file.exists(input_path)) stop(i18n$t("input_image_doesnt_exist"))
  
  # Read image
  img <- magick::image_read(input_path)
  
  # Input image & output path must be PNG files
  if (tolower(magick::image_info(img)$format) != "png") stop(i18n$t("imported_image_must_be_png"))
  if (!grepl("\\.png$", output_path, ignore.case = TRUE)) stop(i18n$t("exported_image_must_be_png"))
  
  # Calculate original image ratio
  original_aspect_ratio <- magick::image_info(img)$height / magick::image_info(img)$width
  
  # Calculate target ratio
  target_aspect_ratio <- target_height / target_width
  
  # New dimensions
  if (original_aspect_ratio > target_aspect_ratio) {
    new_height <- target_height
    new_width <- as.integer(target_height / original_aspect_ratio)
  } else {
    new_width <- target_width
    new_height <- as.integer(target_width * original_aspect_ratio)
  }
  
  # Resize image
  img_resized <- magick::image_resize(img, paste0(new_width, "x", new_height))
  
  # Center image
  x_off <- floor((target_width - new_width) / 2)
  y_off <- floor((target_height - new_height) / 2)
  
  # New transparent image
  img_transparent <- magick::image_blank(target_width, target_height, "transparent")
  
  # Overlay transparent image & resized image
  img_final <- magick::image_composite(img_transparent, img_resized, operator = "src-over", offset = paste0("+", x_off, "+", y_off))
  
  # Write image
  magick::image_write(img_final, output_path)
}

`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)