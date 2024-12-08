% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_import_data.R
\name{import_dataset}
\alias{import_dataset}
\title{Import a dataset}
\usage{
import_dataset(
  r,
  d,
  dataset_id = integer(),
  omop_version = "5.4",
  data_source = "disk",
  data_folder = character(),
  con,
  load_tables = character(),
  ...
)
}
\arguments{
\item{r}{A `shiny::reactiveValues` object used for communication between modules.}

\item{d}{A `shiny::reactiveValues` object used for communication between modules.}

\item{dataset_id}{The ID of the dataset, used to create a specific dataset folder in the application directories. Must be an integer.}

\item{omop_version}{The OMOP version of the imported data. Accepted values are `"5.3"`, `"5.4"`, and `"6.0"`. Defaults to `"5.4"`.}

\item{data_source}{The source of the data. Accepted values are `"db"` for a database connection or `"disk"` for disk storage. Defaults to `"disk"`.}

\item{data_folder}{The folder containing the data. Must be a character string.}

\item{con}{A `DBI::dbConnect` object representing the database connection, required if `data_source` is `"db"`.}

\item{load_tables}{A character vector specifying which OMOP tables to load.}

\item{...}{Additional arguments to be passed to the `vroom` function (e.g., `delim`).}
}
\description{
Imports a dataset into the application using the OMOP Common Data Model.
}
\details{
...
}
