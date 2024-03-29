% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_layout_functions.R
\name{make_shiny_ace_card}
\alias{make_shiny_ace_card}
\title{Make a shiny.fluent card}
\usage{
make_shiny_ace_card(
  title = character(),
  content = character(),
  size = 12,
  style = ""
)
}
\arguments{
\item{title}{Title of the card (character)}

\item{content}{Content of the card (character)}

\item{size}{Size of a card (integer)}

\item{style}{CSS code to custom the card (character)}
}
\value{
Shiny UI elements / HTML code
}
\description{
Creates a shiny.fluent card. Adapted from make_card to render correctly shinyAce div.
}
\examples{
\dontrun{
make_shiny_ace_card(title = "Introduction", content = "This is the text of my introduction card", size = 12)
}
}
