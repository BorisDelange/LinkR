% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_settings_ui.R
\name{render_settings_creation_card}
\alias{render_settings_creation_card}
\title{Render UI of settings creation card}
\usage{
render_settings_creation_card(
  i18n = character(),
  ns = character(),
  id = character(),
  title = character(),
  textfields = character(),
  textfields_width = "200px",
  dropdowns = character(),
  dropdowns_width = "200px"
)
}
\arguments{
\item{i18n}{Translator object from shiny.i18n library}

\item{ns}{Shiny namespace}

\item{id}{ID of current tab / page (character)}

\item{title}{Title used to create the card (character)}

\item{textfields}{A character vector containing distinct textfields to render in the card (character)}

\item{textfields_width}{Width of the textfields, CSS code, so it could be "100\%", "200px" etc (character)}

\item{dropdowns}{A character vector containing distinct dropdowns to render in the card (character)}

\item{dropdowns_width}{Width of the dropdowns, CSS code, so it could be "100\%", "200px" etc (character)}
}
\value{
Shiny UI elements / HTML code
}
\description{
Render UI of settings creation card
}
\examples{
\dontrun{
render_settings_creation_card(i18n = i18n, ns = NS("settings_dataset"), title = "create_dataset",
textfields = c("name", "description"), dropdowns = "data_source")
}
}
