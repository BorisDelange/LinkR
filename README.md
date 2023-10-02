
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LinkR <a href="https://framagit.org/interhop/linkr/linkr"><img src="man/figures/hex.png" align="right" width = "123" height="140" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)
![version](https://img.shields.io/badge/version-0.2.0.9033-blue)
<!-- badges: end -->

## Introduction

**LinkR** is a web application that allows for visualization and
analysis of healthcare data.

The application is coded in R using the Shiny library. It uses the
common data model
<a href = "https://ohdsi.github.io/CommonDataModel/" target = "_blank">OMOP</a>.

## Who is the application for?

The application is for:

- **clinicians**, an intuitive interface allows clinicians to analyze
  data and conduct studies without advanced programming knowledge
- **data scientists and statisticians**, LinkR provides access to a full
  R environment, allowing data scientists and statisticians to exploit
  all the features of advanced data analysis
- **healthcare students**, integrated tutorials in the application
  provide healthcare students with an opportunity to learn and practice
  data analysis in the healthcare field

Thus, LinkR facilitates **collaborative work**.

## Sharing and Open Science

LinkR is an **open source** application.

It contributes to **open science** by allowing the sharing of:

- *studies*: import and export your studies in one click, reproduce
  studies with your own data
- *scripts*: share interoperable scripts, thanks to the use of the
  common OMOP data model
- *plugins*: help improve the application by creating plugins and
  sharing them

## Installation

The `remotes` library must be installed, you can install it with:

``` r
install.packages("remotes")
```

Please note that your version of shiny must be **earlier than version
1.7.5**, which causes significant application slowdowns.

You can install version 1.7.4.1 with the following code.

``` r
remotes::install_github("rstudio/shiny@f5b3954")
```

You can install the development version from Framagit, with:

``` r
remotes::install_gitlab("interhop/linkr/linkr", host = "framagit.org")
```

## Launch the app

To launch the Shiny app, run:

``` r
linkr::linkr(language = "en")
```

You can use the following arguments in the linkr function :

- *language*: choose in which the application will be launched ; “en”
  and “fr” are available
- *app_folder*: by default, application files are saved in the home
  folder (`path.expand("~")` to know which folder it is). You can change
  this folder by specifying the target folder in this argument.
- *local*: TRUE or FALSE to allow the app to access the internet
- *show_home_page*: TRUE or FALSE to show home pages (Overview, News,
  Tutorials… pages)

See `?linkr::linkr` for more informations.

The first load may take a few minutes to create the application database
and download the default data.

Use “admin” as **ID & password** for your first connection.

## Contributions

LinkR **needs help** to evolve! You can contribute by creating plugins,
or by helping to improve the application’s code.

## Report a bug

<a href = "https://framagit.org/interhop/linkr/linkr/-/issues" target = "_blank">Go
here</a> to report a bug.

## Support us

LinkR is supported by the **Interhop** association, promoting
open-source and interoperability in healthcare.

You can
**<a href = "https://interhop.org/en/dons/" target = "_blank">make a
donation here</a>**.

## Contact

Email: <linkr-app@pm.me>.
