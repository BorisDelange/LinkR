help_messages <- function(output, r = shiny::reactiveValues(), id = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r$help_messages_open_panel,
      br(),
      shiny.fluent::Link(i18n$t("study_messages"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      isLightDismiss = r$help_messages_open_panel_light_dismiss,
      isBlocking = r$help_messages_open_panel_light_dismiss,
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({
    
    shiny.fluent::Modal(
      isOpen = r$help_messages_open_modal, dragOptions = TRUE, isModeless = TRUE, topOffsetFixed = TRUE,
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
      div(
        style = "width: 1000px; padding: 15px 10px 0px 15px;",
        shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
          div(style = list(display = "flex"),
            shiny.fluent::Text(r$help_messages_modal_title, variant = "large"),
            div(style = list(flexGrow = 1)),
            shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
          ),
          r$help_messages_modal_text
        )
      )
    )
  })
  
  load_help_page <- function(r){
    r$help_messages_open_modal <- TRUE
    r$help_messages_open_panel_light_dismiss <- FALSE
  }
  
  # Code divs
  
  code_1 <- list()
  div_code_1 <- list()
  code_1$fr <- paste0("Bonjour à tous.\n\n",
    "Voici les données que j'obtiens lorsque je charge la variable *d$person*.\n\n",
    "```{r}\n",
    "d$person\n",
    "```")
  div_code_1$fr <- div(
    span("Bonjour à tous."), br(), br(),
    span("Voici les données que j'obtiens lorsque je charge la variable *d$person*."), br(), br(),
    span("```{r}"), br(),
    span("d$person"), br(),
    span("```"), br(),
    shiny.fluent::IconButton.shinyInput(ns("copy_code_1"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  
  code_1$en <- paste0("Hello everyonde.\n\n",
    "Here are the data I get when I load the *d$person* variable *d$person*.\n\n",
    "```{r}\n",
    "d$person\n",
    "```")
  div_code_1$en <- div(
    span("Hello everyone."), br(), br(),
    span("Here are the data I get when I load the *d$person* variable."), br(), br(),
    span("```{r}"), br(),
    span("d$person"), br(),
    span("```"), br(),
    shiny.fluent::IconButton.shinyInput(ns("copy_code_1"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  
  # Study messages
  
  observeEvent(r$help_messages_page_1, {
    
    load_help_page(r)
    
    r$help_messages_modal_title <- i18n$t("study_messages")
    
    if (language == "fr"){
      r$help_messages_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ", strong("Nouvelle conversation")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Créez une nouvelle conversation en renseignant ", strong("l'objet"), " et le ", strong("premier message"), " de la conversation."),
        p("Tous les utilisateurs ayant accès à l'étude verront cette conversation."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Vous pouvez écrire en RMarkdown en cochant la case ", tags$em("RMarkdown"), "."),
        p("Le ", strong("RMarkdown"), " est du Markdown où l'on peut intégrer du code R."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Vous pouvez ", strong("utiliser les données de l'étude actuellement chargée"), "."),
        p("Voici un exemple de code en RMarkdown."),
        div_code_1$fr,
        p("Cliquez sur ", tags$em("Aperçu"), " pour afficher un aperçu du message, avec le code exécuté."),
        tags$h3(tags$i(class = "fa fa-message", style = "color: steelblue;"), " ", strong("Tous les messages")),
        p("Toutes les conversations s'affichent ici."),
        p("Une conversation non lue s'affichera en gras."),
        p("Répondez à une conversation en cliquant sur ", tags$em("Nouveau message"), "."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_messages_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ", strong("New conversation")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Create a new conversation by filling in ", strong("the subject"), " and the ", strong("first message"), " of the conversation."),
        p("All users with access to the study will see this conversation."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "You can write in RMarkdown by checking the ", tags$em("RMarkdown"), " box."),
        p("RMarkdown is Markdown where you can embed R code."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "You can ", strong("use the data from the currently loaded study"), "."),
        p("Here is an example of code in RMarkdown."),
        div_code_1$en,
        p("Click on ", tags$em("Preview"), " to display a preview of the message, with the code executed."),
        tags$h3(tags$i(class = "fa fa-message", style = "color: steelblue;"), " ", strong("All messages")),
        p("All conversations are displayed here."),
        p("An unread conversation will be displayed in bold."),
        p("Reply to a conversation by clicking on ", tags$em("New message"), "."),
        br()
      )
    }
  })
  
  # Copy code divs
  
  observeEvent(r$help_messages_copy_code_1, clipr::write_clip(code_1[[language]]))
}
