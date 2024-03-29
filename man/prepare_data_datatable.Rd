% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_settings_server.R
\name{prepare_data_datatable}
\alias{prepare_data_datatable}
\title{Prepare data for datatable}
\usage{
prepare_data_datatable(
  output,
  r = shiny::reactiveValues(),
  ns = character(),
  i18n = character(),
  id = character(),
  table = character(),
  dropdowns = character(),
  dropdowns_multiselect = character(),
  dropdowns_null_value = character(),
  factorize_cols = character(),
  action_buttons = character(),
  data_input = tibble::tibble()
)
}
\arguments{
\item{output}{Shiny output value, to show message bars}

\item{r}{Shiny reactive value, used to communicate between modules (reactiveValue)}

\item{ns}{Shiny namespace}

\item{i18n}{i18n Translator object from shiny.i18n library}

\item{id}{ID of the current page / module}

\item{table}{Table of the database containing the data (character)}

\item{dropdowns}{Names of the dropdowns displayed in the datatable (character)}

\item{dropdowns_multiselect}{Names of the dropdowns which have the multiSelect argument set to TRUE (character)}

\item{dropdowns_null_value}{Names of the dropdowns where a null value is added at the beginning (character)}

\item{factorize_cols}{Names of the columns which will be factorized, to be sortable (character)}

\item{action_buttons}{Names of the action buttons needed (character)}

\item{data_input}{The data which will be transformed (tibble)}
}
\description{
Prepare data to be used in a DT datable
}
\examples{
\dontrun{
 prepare_data_datatable(output = output, r = r, ns = ns, i18n = i18n, id = "settings_users",
table = "users", dropdowns = "user_status", action_buttons = "delete", data_input = r$users_temp)
}
}
