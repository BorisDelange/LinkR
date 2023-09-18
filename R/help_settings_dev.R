help_settings_dev <- function(output, r = shiny::reactiveValues(), id = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r$help_settings_dev_open_panel,
      br(),
      shiny.fluent::Link(i18n$t("r_console"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("perf_monitoring"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      isLightDismiss = r$help_settings_dev_open_panel_light_dismiss,
      isBlocking = r$help_settings_dev_open_panel_light_dismiss,
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({
    
    shiny.fluent::Modal(
      isOpen = r$help_settings_dev_open_modal, dragOptions = TRUE, isModeless = TRUE, topOffsetFixed = TRUE,
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
      div(
        style = "width: 1000px; padding: 15px 10px 0px 15px;",
        shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
          div(style = list(display = "flex"),
            shiny.fluent::Text(r$help_settings_dev_modal_title, variant = "large"),
            div(style = list(flexGrow = 1)),
            shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
          ),
          r$help_settings_dev_modal_text
        )
      )
    )
  })
  
  load_help_page <- function(r){
    r$help_settings_dev_open_modal <- TRUE
    r$help_settings_dev_open_panel_light_dismiss <- FALSE
  }
  
  # Code divs
  
  code_1 <- "r$datasets %>% dplyr::filter(id == 1)"
  div_code_1 <- div(
    code_1,
    shiny.fluent::IconButton.shinyInput(ns("copy_code_1"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  
  code_2 <- "names(r)"
  div_code_2 <- div(
    code_2,
    shiny.fluent::IconButton.shinyInput(ns("copy_code_2"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  
  # R console
  
  observeEvent(r$help_settings_dev_page_1, {
    
    load_help_page(r)
    
    r$help_settings_dev_modal_title <- i18n$t("r_console")
    
    if (language == "fr"){
      r$help_settings_dev_modal_text <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Vous pouvez utiliser cet éditeur ", strong("comme la console R"), "."),
        p("Ecrivez du code, cliquez sur ", tags$em("Exécuter"), ", le code sera ", strong("interprété avec la fonction ", tags$em("eval")), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Vous avez ici accès à l'ensemble des variables de l'application (", tags$em("r, m, d"), "), sans restrictions."),
        p("Il faut donc faire attention à donner un ", strong("accès restreint"), " à cette console."),
        p("Exemple de code :"),
        div_code_1,
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Les différentes ", tags$em("reactiveValues"), " (variables accessibles depuis les différents modules de l'application Shiny) ",
          "accessibles depuis cette console sont :"),
        tags$ul(
          tags$li(tags$em("r"), " : comporte l'ensemble des variables faisant fonctionner l'application, dont les tables de la base de données principale"),
          tags$li(tags$em("m"), " : comporte les tables de la base de données publique"),
          tags$li(tags$em("d"), " : variable des données et des terminologies, qui sera créée lors du chargement d'un set de données")
        ),
        p("Pour connaître les différents éléments de chacune de ces variables, vous pouvez utiliser la fonction ", tags$em("names"), " :"),
        div_code_2,
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Utilisez les raccourcis :",
          tags$ul(
            tags$li("CMD/CTRL + SHIFT + ENTER : exécute l'ensemble du code"),
            tags$li("CMD/CTRL + ENTER : exécute le code sélectionné")
          )  
        ),
        br()
      )
    }
    
    if (language == "en"){
      r$help_settings_dev_modal_text <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "You can use this editor ", strong("like the R console"), "."),
        p("Write code, click on ", tags$em("Run"), ", the code will be ", strong("interpreted with the ", tags$em("eval"), " function"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "You have access to all the application's variables (", tags$em("r, m, d"), "), without restrictions."),
        p("Therefore, it is important to provide ", strong("restricted access"), " to this console."),
        p("Example of code:"),
        div_code_1,
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "The different ", tags$em("reactiveValues"), " (variables accessible from the different modules of the Shiny application) ",
          "that can be accessed from this console are:"),
        tags$ul(
          tags$li(tags$em("r"), " : contains all the variables that make the application work, including tables from the main database"),
          tags$li(tags$em("m"), " : contains tables from the public database"),
          tags$li(tags$em("d"), " : data & vocabularies variable, which will be created when loading a dataset")
        ),
        p("To see the different elements of each of these variables, you can use the function ", tags$em("names"), " :"),
        div_code_2,
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Use the shortcuts:",
          tags$ul(
            tags$li("CMD/CTRL + SHIFT + ENTER : execute the entire code"),
            tags$li("CMD/CTRL + ENTER : execute the selected code")
          )  
        ),
        br()
      )
    }
  })
  
  # Performances monitoring
  
  observeEvent(r$help_settings_dev_page_2, {
    
    load_help_page(r)
    
    r$help_settings_dev_modal_title <- i18n$t("perf_monitoring")
    
    if (language == "fr"){
      r$help_settings_dev_modal_text <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Si l'argument ", tags$em("perf_monitoring"), " a été réglé sur ", tags$em("TRUE"), " lors du chargement de l'application,",
          " vous verrez ici un tableau renseignant le ", strong("temps pris par les différentes étapes"), " lors du chargement de l'application."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Vous pouvez remettre à zéro le tableau en cliquant sur ", tags$em("Remettre à zéro"), "."),
        p("Ainsi, vous pouvez lancer une nouvelle opération (par exemple charger un set de données) et évaluer ses performances."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_settings_dev_modal_text <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "If the argument ", tags$em("perf_monitoring"), " was set to ", tags$em("TRUE"), " when loading the application,",
          " you will see a table here providing information about the ", strong("time taken by the different stages"), " when loading the application."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "You can reset the table by clicking on ", tags$em("Reset"), "."),
        p("This way, you can launch a new operation (such as loading a dataset) and evaluate its performance."),
        br()
      )
    }
  })
  
  # Copy code divs
  
  observeEvent(r$help_settings_dev_copy_code_1, clipr::write_clip(code_1))
  observeEvent(r$help_settings_dev_copy_code_2, clipr::write_clip(code_2))
}
