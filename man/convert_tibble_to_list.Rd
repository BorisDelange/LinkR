% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_helpers.R
\name{convert_tibble_to_list}
\alias{convert_tibble_to_list}
\title{Convert a tibble to list}
\usage{
convert_tibble_to_list(
  data = tibble::tibble(),
  key_col = character(),
  text_col = character(),
  null_value = FALSE,
  i18n = character()
)
}
\arguments{
\item{data}{A tibble or a dataframe containing data}

\item{key_col}{Name of the column containing the key (character)}

\item{text_col}{Name of the column containing the text (character)}

\item{null_value}{Add a null value at the beginning (logical)}

\item{i18n}{Translator object from shiny.i18n library}
}
\value{
A list with this structure : list(list(key = "my_key1", text = "my_text1"), list(key = "my_key2", text = "my_text2"))
}
\description{
Converts a tibble to a list for input options in ComboBox & Dropdowns of shiny.fluent library
}
\examples{
data <- tibble::tribble(~key, ~text, "my_key1", "my_text1", "my_key2", "my_text2")
my_list <- convert_tibble_to_list(data = data, key_col = "key", text_col = "text", null_value = TRUE, i18n = i18n)
print(my_list)
}
