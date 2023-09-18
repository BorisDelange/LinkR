help_plugins <- function(output, r = shiny::reactiveValues(), id = character(), prefix = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r[[paste0("help_plugins_", prefix, "_open_panel")]],
      br(),
      strong(i18n$t("plugins")), br(), br(),
      shiny.fluent::Link(i18n$t("whats_a_plugin"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("local_and_remote_plugins"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      strong(i18n$t("create_and_test_a_plugin")), br(), br(),
      shiny.fluent::Link(i18n$t("plugins_management"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_3', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("edit_plugin_code"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_4', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("plugin_options"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_5', Math.random()); }"))), br(), br(),
      strong(i18n$t("import_and_export_plugins")), br(), br(),
      shiny.fluent::Link(i18n$t("import_and_export_plugins"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_6', Math.random()); }"))), br(), br(),
      isLightDismiss = r[[paste0("help_plugins_", prefix, "_open_panel_light_dismiss")]],
      isBlocking = r[[paste0("help_plugins_", prefix, "_open_panel_light_dismiss")]],
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({
    
    shiny.fluent::Modal(
      isOpen = r[[paste0("help_plugins_", prefix, "_open_modal")]], dragOptions = TRUE, isModeless = FALSE, topOffsetFixed = TRUE,
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
      div(
        style = "width: 1000px; padding: 15px 10px 0px 15px;",
        shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
          div(style = list(display = "flex"),
            shiny.fluent::Text(r[[paste0("help_plugins_", prefix, "_modal_title")]], variant = "large"),
            div(style = list(flexGrow = 1)),
            shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
          ),
          r[[paste0("help_plugins_", prefix, "_modal_text")]]
        )
      )
    )
  })
  
  load_help_page <- function(r){
    r[[paste0("help_plugins_", prefix, "_open_modal")]] <- TRUE
    r[[paste0("help_plugins_", prefix, "_open_panel_light_dismiss")]] <- FALSE
  }
  
  # Code divs
  
  code_1 <- paste0(
    "tagList(\n",
    "  shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),\n",
    "    shiny.fluent::TextField.shinyInput(ns(\"text_input_%widget_id%\")),\n",
    "    shiny.fluent::PrimaryButton.shinyInput(ns(\"submit_%widget_id%\"), i18np$t(\"show\"))\n",
    "  ),\n",
    "  div(verbatimTextOutput(ns(\"text_output_%widget_id%\")), style = \"border:dashed 1px; margin-top:10px;\")\n",
    ")"
  )
  div_code_1 <- div(
    span("tagList("), br(),
    span("shiny.fluent::Stack(horizontal = TRUE, tokens = list(childrenGap = 20),", style = "padding-left:20px;"), br(),
    span("shiny.fluent::TextField.shinyInput(ns(\"text_input_%widget_id%\")),", style = "padding-left:40px;"), br(),
    span("shiny.fluent::PrimaryButton.shinyInput(ns(\"submit_%widget_id%\"), i18np$t(\"show\"))", style = "padding-left:40px;"), br(),
    span("),", style = "padding-left:20px;"), br(),
    span("div(verbatimTextOutput(ns(\"text_output_%widget_id%\")), style = \"border:dashed 1px; margin-top:10px;\")", style = "padding-left:20px;"), br(),
    span(")"),
    shiny.fluent::IconButton.shinyInput(ns("copy_code_1"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  
  code_2 <- paste0(
    "observeEvent(input$submit_%widget_id%, {\n",
    "  %req%\n",
    "  output$text_output_%widget_id% <- renderText(paste0(i18np$t(\"input_text_is\"), \" : \", isolate(input$text_input_%widget_id%)))\n",
    "})"
  )
  div_code_2 <- div(
    span("observeEvent(input$submit_%widget_id%, {"), br(),
    span("%req%", style = "padding-left:20px;"), br(),
    span("output$text_output_%widget_id% <- renderText(paste0(i18np$t(\"input_text_is\"), \" : \", isolate(input$text_input_%widget_id%)))", style = "padding-left:20px;"), br(),
    span("})"),
    shiny.fluent::IconButton.shinyInput(ns("copy_code_2"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  
  code_3 <- paste0(
    "base,en,fr\n",
    "my_word,English traduction of my word,Traduction française de mon mot\n",
    "show,Show,Afficher\n",
    "input_text_is,Input text is,Le texte entré est\n"
  )
  div_code_3 <- div(
    span("base,en,fr"), br(),
    span("my_word,English traduction of my word,Traduction française de mon mot"), br(),
    span("show,Show,Afficher"), br(),
    span("input_text_is,Input text is,Le texte entré est"), br(),
    shiny.fluent::IconButton.shinyInput(ns("copy_code_3"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  
  code_4 <- list()
  div_code_4 <- list()
  code_4$fr <- paste0(
    "## Description\n\n",
    "Ce plugin permet de créer un Flowchart à partir des données d'une étude.\n\n",
    "## Utilisation\n\n",
    "Voici un exemple d'utilisation de ce plugin.\n\n",
    "![Texte si l'image ne s'affiche pas](%plugin_folder%/my_image.jpg)"
  )
  div_code_4$fr <- div(
    span("## Description"), br(), br(),
    span("Ce plugin permet de créer un Flowchart à partir des données d'une étude."), br(), br(),
    span("## Utilisation"), br(), br(),
    span("Voici un exemple d'utilisation de ce plugin."), br(), br(),
    span("![Texte si l'image ne s'affiche pas](%plugin_folder%/my_image.jpg)"),
    shiny.fluent::IconButton.shinyInput(ns("copy_code_4"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  code_4$en <- paste0(
    "## Description\n\n",
    "This plugin allows you to create a Flowchart from study data.\n\n",
    "## Usage\n\n",
    "Here is an example of using this plugin.\n\n",
    "Text if the image does not display"
  )
  div_code_4$en <- div(
    span("## Description"), br(), br(),
    span("This plugin allows you to create a Flowchart from study data."), br(), br(),
    span("## Usage"), br(), br(),
    span("Here is an example of using this plugin."), br(), br(),
    span("Text if the image does not display"),
    shiny.fluent::IconButton.shinyInput(ns("copy_code_4"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  
  # What's a plugin ?
  
  observeEvent(r[[paste0("help_plugins_", prefix, "_page_1")]], {
    
    load_help_page(r)
    
    r[[paste0("help_plugins_", prefix, "_modal_title")]] <- i18n$t("whats_a_plugin")
    
    if (language == "fr"){
      r[[paste0("help_plugins_", prefix, "_modal_text")]] <- div(
        p("Les plugins sont des scripts écrits en R - Shiny permettant ", strong("d'ajouter des fonctionnalités à l'application"), "."),
        tags$h3(tags$i(class = "fa fa-user", style = "color: steelblue;"), " ",
          strong("Plugins de données individuelles")),
        p("Les plugins de données individuelles permettent d'afficher les données, patient par patient, sous un format particulier."),
        p("Quelques exemples :"),
        tags$ul(
          tags$li(strong("Plugin Timeline"), " : permet d'afficher les données sous forme de timeline, par exemple pour représenter les données de prescription."),
          tags$li(strong("Plugin Datatable"), " : permet d'afficher les données sous forme de tableau."),
          tags$li(strong("Plugin Texte"), " : permet d'afficher les données textuelles, de filter le texte et de faire des recherches avec des mots clés.")
        ),
        tags$h3(tags$i(class = "fa fa-users", style = "color: steelblue;"), " ",
          strong("Plugins de données agrégées")),
        p("Les plugins de données agrégées permettent de visualiser et d'analyser des données sur un groupe de patient."),
        p("Quelques exemples :"),
        tags$ul(
          tags$li(strong("Plugin Inclusion / exclusion"), " : permet de créer des critères d'inclusion et d'exclusion et de les appliquer aux patients de mon étude."),
          tags$li(strong("Plugin Flowchart"), " : permet de créer un Flowchart à partir de critères d'inclusion et d'exclusion."),
          tags$li(strong("Plugin Données aberrantes"), " : permet d'analyser les données item par item, de déterminer quelles sont les données aberrantes et les exclure de l'étude."),
          tags$li(strong("Plugin Rapport"), " : permet d'écrire un rapport en intégrant les figures de l'étude, qui se mettront à jour dynamiquement (ex : figure du plugin Flowchart).")
        ), 
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_plugins_", prefix, "_modal_text")]] <- div(
        p("Plugins are R-Shiny scripts that ", strong("add functionality to the application"), "."),
        tags$h3(tags$i(class = "fa fa-user", style = "color: steelblue;"), " ",
          strong("Individual data plugins")),
        p("Individual data plugins allow displaying patient data in a particular format."),
        p("Some examples:"),
        tags$ul(
          tags$li(strong("Timeline Plugin"), " : displays data as a timeline, for example, to represent prescription data."),
          tags$li(strong("Datatable Plugin"), " : displays data as a table."),
          tags$li(strong("Text Plugin"), " : displays text data, filters the text, and searches for keywords.")
        ),
        tags$h3(tags$i(class = "fa fa-users", style = "color: steelblue;"), " ",
          strong("Aggregated data plugins")),
        p("Aggregated data plugins allow visualizing and analyzing data on a group of patients."),
        p("Some examples:"),
        tags$ul(
          tags$li(strong("Inclusion / Exclusion Plugin"), " : allows creating inclusion and exclusion criteria and applying them to patients in my study."),
          tags$li(strong("Flowchart Plugin"), " : creates a flowchart from inclusion and exclusion criteria."),
          tags$li(strong("Outlier Plugin"), " : analyzes data item by item, determines which data points are outliers, and exclude them from the study."),
          tags$li(strong("Report Plugin"), " : writes a report integrating study figures, which will dynamically update (e.g., figure from the Flowchart plugin).")
        ), 
        br()
      )
    }
  })
  
  # Local & remote plugins
  
  observeEvent(r[[paste0("help_plugins_", prefix, "_page_2")]], {
    
    load_help_page(r)
    
    r[[paste0("help_plugins_", prefix, "_modal_title")]] <- i18n$t("local_and_remote_plugins")
    
    if (language == "fr"){
      r[[paste0("help_plugins_", prefix, "_modal_text")]] <- div(
        p("Dans l'onglet ", tags$em("Tous les plugins"), ", vous avez accès aux plugins locaux et à des plugins sur git distant."),
        tags$h3(tags$i(class = "fa fa-house", style = "color: steelblue;"), " ", strong("Plugins locaux")),
        p("Ici sont répertoriés l'ensemble des plugins ", strong("disponibles sur votre instance"), " de l'application."),
        p("Ces plugins sont soit des plugins que vous avez ", strong("créés localement"), ", soit des plugins ", strong("téléchargés"), " depuis un git distant."),
        p("Pour obtenir une ", strong("description du plugin"), ", cliquez sur le nom ou l'image du plugin."),
        p("Cliquez sur ", tags$em("Rafraîchir"), " pour mettre à jour la liste, si des plugins ont été ajoutés ou modifiés entre-temps."),
        tags$h3(tags$i(class = "fa fa-cloud", style = "color: steelblue;"), " ", strong("Plugins sur git distant")),
        p("Vous pouvez accéder à une liste de plugins disponibles sur un git distant (fichiers stockés sur un site type github.com, framagit.org...)."),
        p("Pour obtenir une ", strong("description du plugin"), ", cliquez sur le nom ou l'image du plugin."),
        p("Une fois la description d'un plugin ouvert, vous pouvez le ", strong("télécharger"), " ou le ", strong("mettre à jour"), "."),
        p("Ce plugin sera alors disponible dans la rubrique ", tags$em("Plugins locaux"), " et vous pourrez l'utiliser dans vos études."), 
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_plugins_", prefix, "_modal_text")]] <- div(
        p("In the ", tags$em("All plugins"), " tab, you have access to local plugins and plugins on a remote git."),
        tags$h3(tags$i(class = "fa fa-house", style = "color: steelblue;"), " ", strong("Local plugins")),
        p("Here you will find all plugins ", strong("available on your instance"), " of the application."),
        p("These plugins are either plugins that you have ", strong("created locally"), " or plugins that you have ", strong("downloaded"), " from a remote git."),
        p("To get a ", strong("description of a plugin"), ", click on its name or image."),
        p("Click on ", tags$em("Refresh"), " to update the list, if any plugins have been added or modified in the meantime."),
        tags$h3(tags$i(class = "fa fa-cloud", style = "color: steelblue;"), " ", strong("Plugins on a remote git")),
        p("You can access a list of plugins available on a remote git (files stored on a site like github.com, framagit.org, etc.)."),
        p("To get a ", strong("description of a plugin"), ", click on its name or image."),
        p("Once you have opened the description of a plugin, you can ", strong("download"), " it or ", strong("update"), " it."),
        p("This plugin will then be available in the ", tags$em("Local plugins"), " section and you can use it in your studies."), 
        br()
      )
    }
  })
  
  # Create a plugin
  
  observeEvent(r[[paste0("help_plugins_", prefix, "_page_3")]], {
    
    load_help_page(r)
    
    r[[paste0("help_plugins_", prefix, "_modal_title")]] <- i18n$t("plugins_management")
    
    if (language == "fr"){
      r[[paste0("help_plugins_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ", strong("Créer un plugin")),
        p("Pour créer un plugin, allez dans l'onglet ", tags$em("Gestion des plugins"), "."), 
        p("Choisissez un nom, faites-en sorte qu'il ne soit pas déjà utilisé, puis cliquez sur ", tags$em("Ajouter"), "."),
        tags$h3(tags$i(class = "fa fa-trash", style = "color: steelblue;"), " ", strong("Supprimer un ou des plugins")),
        p("Pour supprimer un ou plusieurs plugins, sélectionnez-les en cliquant dessus dans le tableau puis cliquez sur ", tags$em("Supprimer la sélection"), "."),
        p("Vous pouvez également supprimer un plugin en cliquant sur l'icône  ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), " ."),
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ", strong("Editer le code ou les options d'un plugin")),
        p("Cliquez sur"),
        p(shiny::actionButton("edit_plugin_code_button_help", "", icon = icon("file-code")), " pour ", strong("éditer le code"), " du plugin,"),
        p(shiny::actionButton("edit_plugin_options_button_help", "", icon = icon("cog")), " pour ", strong("éditer les options"), " du plugin."),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_plugins_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-plus", style = "color: steelblue;"), " ", strong("Create a plugin")),
        p("To create a plugin, go to the ", tags$em("Plugin Management"), " tab."),
        p("Choose a name that is not already in use, then click ", tags$em("Add"), "."),
        tags$h3(tags$i(class = "fa fa-trash", style = "color: steelblue;"), " ", strong("Delete one or more plugins")),
        p("To delete one or more plugins, select them by clicking on them in the table, then click ", tags$em("Delete selection"), "."),
        p("You can also delete a plugin by clicking on the  ", shiny::actionButton("delete_button_help", "", icon = icon("trash-alt")), "  icon."),
        tags$h3(tags$i(class = "fa fa-gear", style = "color: steelblue;"), " ", strong("Edit the code or options of a plugin")),
        p("Click"),
        p(shiny::actionButton("edit_plugin_code_button_help", "", icon = icon("file-code")), " to ", strong("edit the code"), " of the plugin,"),
        p(shiny::actionButton("edit_plugin_options_button_help", "", icon = icon("cog")), " to ", strong("edit the options"), " of the plugin."),
        br()
      )
    }
  })
  
  # Edit plugin code
  
  observeEvent(r[[paste0("help_plugins_", prefix, "_page_4")]], {
    
    load_help_page(r)
    
    r[[paste0("help_plugins_", prefix, "_modal_title")]] <- i18n$t("edit_plugin_code")
    
    if (language == "fr"){
      r[[paste0("help_plugins_", prefix, "_modal_text")]] <- div(
        p("Dans cette rubrique, vous pouvez ", strong("écrire le code"), " d'un plugin et le ", strong("tester"), " en chargeant au préalable des données."),
        p("Pour les plugins de données individuelles, il faut ", strong("charger les données"), " d'un patient. Pour les plugins de données agrégées, charger les données d'une étude suffit."),
        tags$h3(tags$i(class = "fa fa-code", style = "color: steelblue;"), " ", strong("UI (user interface) code")),
        p("Vous codez ici ", strong("l'interface utilisateur"), " du plugin."),
        p("Voici un exemple de code :"),
        div_code_1,
        p("Le code doit être intégré au sein d'une fonction ", strong("tagList"), " comme c'est le cas ici."),
        p("N'oubliez pas de déclarer les ID des éléments avec le ", strong("namespace"), ", via la fonction ", strong("ns()"), "."),
        p("Utilisez la balise ", strong("%widget_id%"), ", qui sera remplacée par l'ID du widget une fois le plugin lancé."),
        p("Voici les autres balises que vous pouvez utiliser :"),
        tags$ul(
          tags$li(strong("%tab_id%"), " : sera remplacé par l'ID du tab dans lequel sont contenus le widget et le plugin."),
          tags$li(strong("%study_id%"), " : sera remplacé par l'ID de l'étude en cours.")
        ),
        tags$h3(tags$i(class = "fa fa-code", style = "color: steelblue;"), " ", strong("Server code")),
        p("Vous codez ici le partie ", strong("serveur"), " de votre plugin, avec les ", strong("observers"), "."),
        p("Cela fonctionne exactement comme une application Shiny."),
        p("Voici un exemple de code :"),
        div_code_2,
        p("A chaque début d'observer, vous devez ", strong("insérer cette balise"), " : ", tags$em("%req%"), "."),
        p("Cela permet d'éviter que le code se lance plusieurs fois pour un même observer en cas de modification d'un widget."),
        p("Vous pouvez accéder aux ", strong("concepts sélectionnés"), " par les utilisateurs via la variable suivante : ", tags$em("selected_concepts"), "."),
        p("Elle comprend les colonnes suivantes :"),
        tags$ul(
          tags$li(tags$em("concept_id"), " : ID du concept dans la table CONCEPT"),
          tags$li(tags$em("concept_name"), " : nom du concept"),
          tags$li(tags$em("concept_display_name"), " : nom d'affichage du concept, souvent une abréviation"),
          tags$li(tags$em("domain_id"), " : ID du domaine du concept"),
          tags$li(tags$em("mapped_to_concept_id"), " : ID du concept avec le concept actuel est aligné"),
          tags$li(tags$em("merge_mapped_concepts"), " : si TRUE, il ne faudra afficher que le nom du concept avec lequel le concept actuel est aligné")
        ),
        p("Utilisez les ", strong("raccourcis"), " :"),
        tags$ul(
          tags$li("CMD/CTRL + SHIFT + ENTER : exécute l'ensemble du code"),
          tags$li("CMD/CTRL + ENTER : exécute le code sélectionné"),
          tags$li("CMD/CTRL + S : sauvegarde le code")
        ),
        tags$h3(tags$i(class = "fa fa-language", style = "color: steelblue;"), " ", strong("Traductions")),
        p("Vous pouvez ici créer un fichier CSV permettant d'utiliser des traductions au sein du plugin."),
        p("Le fichier doit être sous la forme suivante."),
        div_code_3,
        p("Vous pouvez ensuite utiliser la fonction ", strong("i18np$t"), " pour traduire des mots dans le plugin."),
        tags$h3(tags$i(class = "fa fa-exclamation-triangle", style = "color: steelblue;"), " ", strong("Bugs possibles")),
        p("Pour éviter les bugs, les codes UI & server du plugin sont exécutés au sein d'une fonction ", strong("tryCatch"), "."),
        p("En cas d'erreur à l'éxécution du code, un message d'erreur s'affichera et le log des bugs sera stocké, disponible depuis la ", strong("page Log"), "."),
        p("Cependant, certains bugs ne peuvent pas être évités."),
        p("Les choses suivantes feront crasher l'application :"),
        tags$ul(
          tags$li("Appeler la fonction i18np$t avec une traduction qui n'existe pas dans le fichier de traduction"),
          tags$li("Avoir des duplicats dans le fichier de traduction."),
          tags$li("Créer un observer contenant un bug.")
        ),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_plugins_", prefix, "_modal_text")]] <- div(
        p("In this section, you can ", strong("write code"), " for a plugin and ", strong("test"), " it by loading data beforehand."),
        p("For individual data plugins, you must ", strong("load patient data"), ". For aggregated data plugins, loading study data is enough."),
        tags$h3(tags$i(class = "fa fa-code", style = "color: steelblue;"), " ", strong("UI (user interface) code")),
        p("Here you code the ", strong("user interface"), " of the plugin."),
        p("Here's an example of code:"),
        div_code_1,
        p("The code must be integrated into a function called ", strong("tagList"), ", as is the case here."),
        p("Don't forget to declare the element IDs with the ", strong("namespace"), " using the function ", strong("ns()"), "."),
        p("Use the ", strong("%widget_id%"), " tag, which will be replaced by the widget's ID once the plugin is launched."),
        p("Here are the other tags you can use:"),
        tags$ul(
          tags$li(strong("%tab_id%"), " : will be replaced by the ID of the tab in which the widget and the plugin are located."),
          tags$li(strong("%study_id%"), " : will be replaced by the ID of the current study.")
        ),
        tags$h3(tags$i(class = "fa fa-code", style = "color: steelblue;"), " ",strong("Server code")),
        p("Here you code the ", strong("server"), " part of your plugin, with the ", strong("observers"), "."),
        p("This works exactly like a Shiny application."),
        p("Here is an example of code:"),
        div_code_2,
        p("At the beginning of each observer, you must ", strong("insert this tag"), ":", tags$em("%req%"), "."), 
        p("It prevents the code from being executed multiple times for the same observer if a widget is modified."),
        p("You can access the ", strong("selected concepts"), " by users via the following variable: ", tags$em("selected_concepts"), "."),
        p("It includes the following columns:"),
        tags$ul(
          tags$li(tags$em("concept_id"), " : ID of the concept in the CONCEPT table"),
          tags$li(tags$em("concept_name"), " : name of the concept"),
          tags$li(tags$em("concept_display_name"), " : display name of the concept, often an abbreviation"),
          tags$li(tags$em("domain_id"), " : domain ID of the concept"),
          tags$li(tags$em("mapped_to_concept_id"), " : ID of the concept with which the current concept is aligned"),
          tags$li(tags$em("merge_mapped_concepts"), " : if TRUE, only the name of the concept to which the current concept is aligned should be displayed")
        ),
        p("Use the ", strong("shortcuts"), ":"),
        tags$ul(
          tags$li("CMD/CTRL + SHIFT + ENTER: executes the entire code"),
          tags$li("CMD/CTRL + ENTER: executes the selected code"),
          tags$li("CMD/CTRL + S: saves the code")
        ),
        tags$h3(tags$i(class = "fa fa-language", style = "color: steelblue;"), " ", strong("Translations")),
        p("Here you can create a CSV file to use translations within the plugin."),
        p("The file must be in the following format:"),
        div_code_3,
        p("You can then use the function ", strong("i18np$t"), " to translate words within the plugin."),
        tags$h3(tags$i(class = "fa fa-exclamation-triangle", style = "color: steelblue;"), " ", strong("Possible bugs")),
        p("To avoid bugs, the plugin's UI & server codes are called within a ", strong("tryCatch"), " function."),
        p("In case of an error during code execution, an error message will be displayed and the bug log will be stored, available from the ", strong("Log page"), "."),
        p("However, some bugs cannot be avoided."),
        p("The following things will crash the application:"),
        tags$ul(
          tags$li("Calling the i18np$t function with a translation that does not exist in the translation file"),
          tags$li("Having duplicates in the translation file."),
          tags$li("Creating an observer containing a bug.")
        ),
        br()
      )
    }
  })
  
  # Plugin options
  
  observeEvent(r[[paste0("help_plugins_", prefix, "_page_5")]], {
    
    load_help_page(r)
    
    r[[paste0("help_plugins_", prefix, "_modal_title")]] <- i18n$t("plugin_options")
    
    if (language == "fr"){
      r[[paste0("help_plugins_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-user", style = "color: steelblue;"), " ", strong("Auteur & version")),
        p("Le nom de l'auteur et la version du plugin seront visibles depuis l'onglet ", tags$em("Tous les plugins"), "."),
        p("Pensez à ", strong("modifier la version du plugin"), " lorsque des modifications sont réalisées dans le code."),
        tags$h3(tags$i(class = "fa fa-grip-lines", style = "color: steelblue;"), " ", strong("Nom et catégorie")),
        p("Nom et catégorie s'affichant dans l'onglet ", tags$em("Tous les plugins"), ", selon la langue choisie au démarrage de l'application."),
        tags$h3(tags$i(class = "fa fa-lock", style = "color: steelblue;"), " ", strong("Accès")),
        p("Choisissez ici qui peut avoir accès à ce plugin."),
        p("Il peut être utile de restreindre l'accès à un plugin en cours de création, en ajoutant uniquement les personnes travaillant sur ce plugin."),
        tags$h3(tags$i(class = "fa fa-image", style = "color: steelblue;"), " ", strong("Images")),
        p("Vous pouvez importer des images en cliquant sur ", tags$em("Importer une image"), "."),
        p("Cette image peut alors être choisie dans le menu déroulant pour être ", strong("l'image présentant le plugin"), " dans l'onglet ", tags$em("Tous les plugins"), "."),
        p("Vous pouvez également utiliser les images importées dans la description du plugin."),
        tags$h3(tags$i(class = "fa fa-file-lines", style = "color: steelblue;"), " ", strong("Description")),
        p("La description s'affiche dans l'onglet ", tags$em("Tous les plugins"), ", lorsque l'on clique sur un plugin."),
        p("Le code ici est du ", strong("Markdown"), ", dont voici un exemple."),
        div_code_4$fr,
        p("Pour ajouter une image, utilisez la balise ", strong("%plugin_folder%"), " qui sera remplacée par le dossier du plugin."),
        p("Cliquez sur ", tags$em("Exécuter"), " pour visualiser le rendu de la description."),
        p("Utilisez les ", strong("raccourcis"), " :"),
        tags$ul(
          tags$li("CMD/CTRL + SHIFT + ENTER : exécute l'ensemble du code"),
          tags$li("CMD/CTRL + ENTER : exécute le code sélectionné"),
          tags$li("CMD/CTRL + S : sauvegarde le code")
        ),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_plugins_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-user", style = "color: steelblue;"), " ", strong("Author & version")),
        p("The author's name and the plugin's version will be visible from the ", tags$em("All plugins"), " tab."),
        p("Remember to ", strong("update the plugin version"), " when changes are made in the code."),
        tags$h3(tags$i(class = "fa fa-grip-lines", style = "color: steelblue;"), " ", strong("Name and category")),
        p("Name and category displayed in the ", tags$em("All plugins"), " tab, depending on the language selected at application startup."),
        tags$h3(tags$i(class = "fa fa-lock", style = "color: steelblue;"), " ", strong("Access")),
        p("Choose here who can access this plugin."),
        p("It may be useful to restrict access to a plugin being created, by only adding the people working on that plugin."),
        tags$h3(tags$i(class = "fa fa-image", style = "color: steelblue;"), " ", strong("Images")),
        p("You can import images by clicking on ", tags$em("Import an image"), "."),
        p("This image can then be selected in the dropdown menu to be ", strong("the image presenting the plugin"), " in the ", tags$em("All plugins"), " tab."),
        p("You can also use imported images in the plugin description."),
        tags$h3(tags$i(class = "fa fa-file-lines", style = "color: steelblue;"), " ", strong("Description")),
        p("The description is displayed in the ", tags$em("All plugins"), " tab, when a plugin is clicked."),
        p("The code here is in ", strong("Markdown"), ", here is an example."),
        div_code_4$en,
        p("To add an image, use the tag ", strong("%plugin_folder%"), " which will be replaced by the plugin folder."),
        p("Click on ", tags$em("Run code"), " to view the render of the description."),
        p("Use the ", strong("shortcuts"), ":"),
        tags$ul(
          tags$li("CMD/CTRL + SHIFT + ENTER: executes the entire code"),
          tags$li("CMD/CTRL + ENTER: executes the selected code"),
          tags$li("CMD/CTRL + S: saves the code")
        ),
        br()
      )
    }
  })
  
  # Import / export a plugin
  
  observeEvent(r[[paste0("help_plugins_", prefix, "_page_6")]], {
    
    load_help_page(r)
    
    r[[paste0("help_plugins_", prefix, "_modal_title")]] <- i18n$t("import_and_export_plugins")
    
    if (language == "fr"){
      r[[paste0("help_plugins_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-download", style = "color: steelblue;"), " ",  strong("Importer un plugin")),
        p("Vous pouvez importer un ou plusieurs plugins ", strong("à partir d'un fichier ZIP"), ", crée depuis l'onglet ", tags$em("Exporter un plugin"), "."),
        p("Si le plugin existe déjà, il ne sera remplacé que si l'option ", tags$em("Remplacer les plugins déjà existants"), " est cochée."),
        tags$h3(tags$i(class = "fa fa-upload", style = "color: steelblue;"), " ", strong("Exporter un plugin")),
        p("Vous pouvez exporter un ou plusieurs plugins, ils seront téléchargés dans un fichier ZIP, que vous pouvez ", strong("partager"), " avec d'autres utilisateurs utilisant LinkR."),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_plugins_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-download", style = "color: steelblue;"), " ", strong("Import a plugin")),
        p("You can import one or several plugins ", strong("from a ZIP file"), ", created from the ", tags$em("Export a plugin"), " tab."),
        p("If the plugin already exists, it will only be replaced if the option ", tags$em("Replace existing plugins"), " is checked."),
        tags$h3(tags$i(class = "fa fa-upload", style = "color: steelblue;"), " ", strong("Export a plugin")),
        p("You can export one or several plugins, they will be downloaded in a ZIP file, which you can ", strong("share"), " with other LinkR users."),
        br()
      )
    }
  })
  
  # Copy code divs
  
  observeEvent(r$help_plugins_copy_code_1, clipr::write_clip(code_1))
  observeEvent(r$help_plugins_copy_code_2, clipr::write_clip(code_2))
  observeEvent(r$help_plugins_copy_code_3, clipr::write_clip(code_3))
  observeEvent(r$help_plugins_copy_code_4, clipr::write_clip(code_4[[language]]))
}
