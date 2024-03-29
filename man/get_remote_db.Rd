% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_settings_app_database.R
\name{get_remote_db}
\alias{get_remote_db}
\title{Connection to remote database}
\usage{
get_remote_db(
  r = shiny::reactiveValues(),
  m = shiny::reactiveValues(),
  output,
  i18n = character(),
  ns = character()
)
}
\arguments{
\item{r}{Shiny reactive value, used to communicate between modules}

\item{m}{Shiny reactive value, used to communicate between modules}

\item{output}{Shiny output variable}

\item{i18n}{Translator object from shiny.i18n library}

\item{ns}{Shiny namespace}
}
\description{
Get a connection to a remote database. If the remote connection fails, returns local DBI connection object.
}
\examples{
\dontrun{
get_remote_db(r = r, m = m, output = output, i18n = i18n, ns = ns)
}
}
