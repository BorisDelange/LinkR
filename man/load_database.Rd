% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_settings_app_database.R
\name{load_database}
\alias{load_database}
\title{Load database}
\usage{
load_database(
  r = shiny::reactiveValues(),
  m = shiny::reactiveValues(),
  i18n = character()
)
}
\arguments{
\item{r}{Shiny reactive value, used to communicate between modules}

\item{m}{Shiny reactive value, used to communicate between modules}

\item{i18n}{Translator object from shiny.i18n library}
}
\description{
Load application database with only used tables
}
\examples{
\dontrun{
load_database(r = r, m = m, i18n = i18n)
}
}
