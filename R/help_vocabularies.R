help_vocabularies <- function(output, r = shiny::reactiveValues(), id = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r$help_vocabularies_open_panel,
      br(),
      strong(i18n$t("vocabularies_and_concepts")), br(), br(),
      shiny.fluent::Link(i18n$t("whats_a_vocabulary"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("vocabulary_concepts"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      strong(i18n$t("vocabularies_concepts_mapping")), br(), br(),
      shiny.fluent::Link(i18n$t("whats_a_concept_mapping"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_3', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("add_concept_mapping"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_4', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("evaluate_and_delete_concepts_mappings"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_5', Math.random()); }"))), br(), br(),
      isLightDismiss = r$help_vocabularies_open_panel_light_dismiss,
      isBlocking = r$help_vocabularies_open_panel_light_dismiss,
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({
    
    shiny.fluent::Modal(
      isOpen = r$help_vocabularies_open_modal, dragOptions = TRUE, isModeless = TRUE, topOffsetFixed = TRUE,
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
      div(
        style = "width: 1000px; padding: 15px 10px 0px 15px;",
        shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
          div(style = list(display = "flex"),
            shiny.fluent::Text(r$help_vocabularies_modal_title, variant = "large"),
            div(style = list(flexGrow = 1)),
            shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
          ),
          r$help_vocabularies_modal_text
        )
      )
    )
  })
  
  load_help_page <- function(r){
    r$help_vocabularies_open_modal <- TRUE
    r$help_vocabularies_open_panel_light_dismiss <- FALSE
  }
  
  # What's a vocabulary ?
  
  observeEvent(r$help_vocabularies_page_1, {
    
    load_help_page(r)
    
    r$help_vocabularies_modal_title <- i18n$t("whats_a_vocabulary")
    
    if (language == "fr"){
      r$help_vocabularies_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-book", style = "color: steelblue;"), " ", strong("Terminologies")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Une ", strong("terminologie"), " est un ", strong("dictionnaire de concepts"), ", associant des ", strong("codes"), " à des ", strong("noms"), "."),
        p("Quelques exemples de concepts issus d'une terminologie :"),
        tags$ul(
          tags$li("42503 - Fréquence cardiaque : ici le concept ayant pour code 42503 est associé au nom ", tags$em("Fréquence cardiaque")),
          tags$li("800902 - Noradrénaline : ici le concept ayant pour code 800902 est associé au nom ", tags$em("Noradrénaline"))
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Un ", strong("entrepôt de données de santé"), " stocke des données en associant des ", strong("valeurs"), " à des ", strong("concepts"),"."),
        p("Quelques exemples :"),
        tags$ul(
          tags$li("patient 409 - datetime 13-01-2022 14:44:32 - concept 42503 - valeur 54"),
          tags$li("ici le concept ayant pour code 42503 (Fréquence cardiaque) est associé à la valeur 54 pour le patient 409 à la date et l'heure indiquées.", style = "margin-left:20px;"),
          tags$li("patient 311 - datetime 12-02-2022 09:41:33 - concept 800902 - valeur 1.25"),
          tags$li("ici le concept ayant pour code 800902 (Noradrénaline) est associé à la valeur 1.25 pour le patient 311 à la date et l'heure indiquées.", style = "margin-left:20px;")
        ),
        tags$h3(tags$i(class = "fa fa-book-atlas", style = "color: steelblue;"), " ", strong("Terminologies standards")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Les logiciels médicaux utilisent classiquement chacun une terminologie qui leur est propre."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Des ", strong("terminologies standards"), " ont été créées pour permettre ", strong("d'homogénéiser"), " les entrepôts de données",
          " et permettre leur ", strong("interopérabilité"), "."),
        p("Quelques exemples de terminologies standardisées :"),
        tags$ul(
          tags$li("LOINC"),
          tags$li("SNOMED"),
          tags$li("RxNorm")
        ),
        p(strong("Athena"), " est un moteur de recherche de concepts, rassemblant plusieurs terminologies. Vous pouvez y accéder ",
          shiny.fluent::Link(href = "https://athena.ohdsi.org/", "via ce lien", target = "_blank"), "."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_vocabularies_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-book", style = "color: steelblue;"), " ", strong("Vocabularies")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "A ", strong("vocabulary"), " is a ", strong("dictionary of concepts"), ", associating ", strong("codes"), " with ", strong("names"), "."),
        p("Some examples of concepts from a vocabulary:"),
        tags$ul(
          tags$li("42503 - Heart rate: here the concept with the code 42503 is associated with the name ", tags$em("Heart rate")),
          tags$li("800902 - Norepinephrine: here the concept with the code 800902 is associated with the name ", tags$em("Norepinephrine"))
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "A ", strong("clinical data warehouse"), " stores data by associating ", strong("values"), " with ", strong("concepts"),"."),
        p("Some examples:"),
        tags$ul(
          tags$li("patient 409 - datetime 13-01-2022 14:44:32 - concept 42503 - value 54"),
          tags$li("here the concept with code 42503 (Heart rate) is associated with the value 54 for patient 409 at the indicated date and time.", style = "margin-left:20px;"),
          tags$li("patient 311 - datetime 12-02-2022 09:41:33 - concept 800902 - value 1.25"),
          tags$li("here the concept with code 800902 (Norepinephrine) is associated with the value 1.25 for patient 311 at the indicated date and time.", style = "margin-left:20px;")
        ),
        tags$h3(tags$i(class = "fa fa-book-atlas", style = "color: steelblue;"), " ", strong("Standard terminologies")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Medical software typically each use a vocabulary of their own."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Some ", strong("standard terminologies"), " have been created to allow ", strong("standardization"), " of data warehouses",
          " and facilitate their ", strong("interoperability"), "."),
        p("Some examples of standardized vocabularies:"),
        tags$ul(
          tags$li("LOINC"),
          tags$li("SNOMED"),
          tags$li("RxNorm")
        ),
        p(strong("Athena"), " is a concept search engine, gathering several terminologies. You can access it ",
          shiny.fluent::Link(href = "https://athena.ohdsi.org/", "via this link", target = "_blank"), "."),
        br()
      )
    }
  })
  
  # Vocabulary concepts
  
  observeEvent(r$help_vocabularies_page_2, {
    
    load_help_page(r)
    
    r$help_vocabularies_modal_title <- i18n$t("vocabulary_concepts")
    
    if (language == "fr"){
      r$help_vocabularies_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-book", style = "color: steelblue;"), " ", strong("Choisir une terminologie")),
        p("Choisissez un ", strong("set de données"), " : l'ensemble des terminologies présentes dans ce set sera disponible."),
        p("Depuis l'onglet ", tags$em("Concepts"), ", choisissez une ", strong("terminologie"), " dans le menu déroulant."),
        p("L'", strong("ensemble des concepts"), " de cette terminologie s'affiche dans le tableau."),
        p("En cliquant sur un concept, vous pouvez ", strong("afficher ses détails"), ", avec :"),
        tags$ul(
          tags$li(strong("ID du concept"), " : identifiant unique du concept"),
          tags$li(strong("Nom du concept"), " : nom du concept que vous pouvez modifier ; cette modification ne s'appliquera que sur votre compte"),
          tags$li(strong("Nom d'affichage du concept"), " : il s'agit du nom qui sera utilisé par les plugins pour afficher le concept (par exemple, vous pouvez indiquer FC pour fréquence cardiaque)"),
          tags$li(strong("ID du domaine"), " : il s'agit du domaine auquel appartient le concept"),
          tags$li(strong("Concept standard"), " : indique si le concept est standard ou non"),
          tags$li(strong("Code du concept"), " : le code du concept dans sa terminologie d'origine"),
          tags$li(strong("Valeurs"), " : donne des exemples de valeurs associées à ce concept (numériques, textuelles ou en tant que concept)"),
        ),
        tags$h3(tags$i(class = "fa fa-filter", style = "color: steelblue;"), " ", strong("Filtrer les concepts")),
        p("Vous pouvez choisir quelles ", strong("colonnes"), " du tableau afficher en cochant les colonnes dans le menu déroulant ", tags$em("Colonnes"), "."),
        p("Vous pouvez ", strong("filtrer"), " les concepts via la barre de recherche en-dessous des noms de colonnes."),
        p("En cochant ", tags$em("Afficher les concepts alignés"), ", vous affichez :"),
        tags$ul(
          tags$li("Les concepts utilisés dans le set de données sélectionné"),
          tags$li("Mais également les concepts non utilisés dans le set de données et alignés sur des concepts eux-mêmes utilisés dans le set")
        ),
        p("La colonne ", tags$em("Patients"), " renseigne le ", strong("nombre de patients"), " ayant au moins une fois ce concept."),
        p("La colonne ", tags$em("Lignes"), " renseigne le ", strong("nombre de lignes"), " dans le set de données concernant ce concept."),
        tags$h3(tags$i(class = "fa fa-magnifying-glass", style = "color: steelblue;"), " ", strong("Chercher un concept")),
        p("Vous pouvez ", strong("rechercher"), " un concept via la barre de recherche, en haut de la colonne ", tags$em("Nom du concept 1"), "."),
        tags$h3(tags$i(class = "fa fa-pen-to-square", style = "color: steelblue;"), " ", strong("Modiifer un concept")),
        p("Vous pouvez ", strong("modifier"), " le nom d'un concept en double cliquant sur le nom, en le modifiant puis en cliquant sur ", tags$em("Sauvegarder"), "."),
        tags$h3(tags$i(class = "fa fa-circle-info", style = "color: steelblue;"), " ", strong("Plus d'informations")),
        p("Pour plus d'informations, rendez-vous sur ", shiny.fluent::Link(href = "https://ohdsi.github.io/CommonDataModel/cdm60.html#CONCEPT", 
          "ce lien (documentation OMOP)", target = "_blank"), "."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_vocabularies_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-book", style = "color: steelblue;"), " ", strong("Choose a vocabulary")),
        p("Choose a ", strong("dataset"), ": all the vocabularies present in this dataset will be available."),
        p("From the ", tags$em("Concepts"), " tab, choose a ", strong("vocabulary"), " from the dropdown menu."),
        p(strong("All the concepts"), " of this vocabulary are displayed in the table."),
        p("By clicking on a concept, you can ", strong("display its details"), ", including: "),
        tags$ul(
          tags$li(strong("Concept ID"), ": unique identifier of the concept"),
          tags$li(strong("Concept name"), ": name of the concept that you can modify; this modification will only apply to your account"),
          tags$li(strong("Concept display name"), ": this is the name that will be used by the plugins to display the concept (for example, you can indicate HR for heart rate)"),
          tags$li(strong("Domain ID"), ": this is the domain to which the concept belongs"),
          tags$li(strong("Standard concept"), ": indicates whether the concept is standard or not"),
          tags$li(strong("Concept code"), ": the code of the concept in its original terminology"),
          tags$li(strong("Values"), ": gives examples of values associated with this concept (numeric, textual or as a concept)"),
        ),
        tags$h3(tags$i(class = "fa fa-filter", style = "color: steelblue;"), " ", strong("Filter the concepts")),
        p("You can choose which ", strong("columns"), " of the table to display by checking the columns in the ", tags$em("Columns"), " dropdown menu."),
        p("You can ", strong("filter"), " the concepts via the search bar below the column names."),
        p("By checking ", tags$em("Show mapped concepts"), ", you display:"),
        tags$ul(
          tags$li("The concepts used in the selected data set"),
          tags$li("But also the concepts not used in the data set and aligned with concepts that are themselves used in the set")
        ),
        p("The ", tags$em("Patients"), " column indicates the ", strong("number of patients"), " who have used this concept at least once."),
        p("The ", tags$em("Rows"), " column indicates the ", strong("number of rows"), " in the dataset concerning this concept."),
        tags$h3(tags$i(class = "fa fa-magnifying-glass", style = "color: steelblue;"), " ", strong("Search for a concept")),
        p("You can ", strong("search"), " for a concept via the search bar at the top of the ", tags$em("Concept name 1"), " column."),
        tags$h3(tags$i(class = "fa fa-pen-to-square", style = "color: steelblue;"), " ", strong("Modifying a concept")),
        p("You can ", strong("modify"), " the name of a concept by double-clicking on the name, modifying it, and then clicking on ", tags$em("Save"), "."),
        tags$h3(tags$i(class = "fa fa-circle-info", style = "color: steelblue;"), " ", strong("More information")),
        p("For more information, visit ", shiny.fluent::Link(href = "https://ohdsi.github.io/CommonDataModel/cdm60.html#CONCEPT", 
          "this link (OMOP documentation)", target = "_blank"), "."),
        br()
      )
      
    }
  })
  
  # What's a concept mapping ?
  
  observeEvent(r$help_vocabularies_page_3, {
    
    load_help_page(r)
    
    r$help_vocabularies_modal_title <- i18n$t("whats_a_concept_mapping")
    
    if (language == "fr"){
      r$help_vocabularies_modal_text <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Il est fréquent que les différentes terminologies comportent des ", strong("concepts similaires"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Pour faire en sorte de ", strong("faciliter les requêtes"), " sur les bases de données, nous procédons à un ", strong("alignement de concepts"), "."),
        p("Il s'agit d'indiquer que tel concept de telle terminologie est lié à tel autre concept de telle autre terminologie."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Nous pouvons ", strong("lier"), " ces deux concepts par un code indiquant qu'ils sont ", strong("similaires"), "."),
        p("Par exemple :"),
        tags$ul(
          tags$li("Terminologie MIMIC-IV, Concept 220045 - Fréquence cardiaque"),
          tags$li("Terminologie LOINC, Concept 8867-4 - Fréquence cardiaque")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Il est également possible d'indiquer des ", strong("relations de type hiérarchique"), "."),
        p("Par exemple :"),
        tags$ul(
          tags$li("Terminologie SNOMED, Concept 102594003 - Anomalie à l'ECG"),
          tags$li("Terminologie SNOMED, Concept 164893009 - Arythmie ventriculaire")
        ),
        p("Nous pouvons ", strong("lier"), " ces deux concepts par un code indiquant que le deuxième concept est ", strong("inclus"), " dans le premier."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_vocabularies_modal_text <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "It is common that different vocabularies contain ", strong("similar concepts"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "To ", strong("facilitate queries"), " on the databases, we proceed to a ", strong("concept mapping"), "."),
        p("This involves indicating that a certain concept from a certain vocabulary is related to another concept from another vocabulary."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "We can ", strong("link"), " these two concepts with a code indicating that they are ", strong("similar"), "."),
        p("For example:"),
        tags$ul(
          tags$li("MIMIC-IV vocabulary, Concept 220045 - Heart rate"),
          tags$li("LOINC vocabulary, Concept 8867-4 - Heart rate")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "It is also possible to indicate ", strong("hierarchical type relationships"), "."),
        p("For example:"),
        tags$ul(
          tags$li("SNOMED vocabulary, Concept 102594003 - ECG anomaly"),
          tags$li("SNOMED vocabulary, Concept 164893009 - Ventricular arrhythmia")
        ),
        p("We can ", strong("link"), " these two concepts with a code indicating that the second concept is ", strong("included"), " in the first."),
        br()
      )
    }
  })
  
  # Add a vocabulary concept mapping
  
  observeEvent(r$help_vocabularies_page_4, {
    
    load_help_page(r)
    
    r$help_vocabularies_modal_title <- i18n$t("add_concept_mapping")
    
    if (language == "fr"){
      r$help_vocabularies_modal_text <- div(
        r$help_vocabularies_modal_text <- div(
          p("Pour ajouter un alignement, allez dans l'onglet ", tags$em("Ajouter"), " puis sélectionnez deux terminologies (elles peuvent être distinctes ou non)."),
          p("Habituellement, une terminologie ", strong("non standard"), " est alignée sur une terminologie ", strong("standard"), ". Dans ce cas, choisissez la terminologie non standard à gauche."),
          tags$p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Terminologie de gauche")),
          p("Vous pouvez filtrer la terminologie de gauche en cochant ", tags$em("Concepts non alignés uniquement"), ", afin de retirer les concepts déjà alignés. Cela concerne ", tags$em("tous les alignements de concepts"), 
            ", qu'ils aient été importés ou qu'ils aient été ajoutés par les utilisateurs de LinkR."),
          p("Le nombre de lignes pour la terminologie de gauche concerne le ", strong("set de données sélectionné"), "."),
          tags$p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Terminologie de droite")),
          p("Vous pouvez filtrer la terminologie de droite en cochant ", tags$em("N'afficher que les concepts utilisés"), ", afin de retirer les concepts qui n'ont jamais été utilisés dans ", strong("aucun set de données"), " chargé sur l'application."),
          p("Le nombre de lignes pour la terminologie de droite concerne ", strong("tous les sets de données"), " chargés sur l'application."),
          p("Est également renseigné le ", strong("nombre de sets de données"), " contenant chaque concept."),
          p("L'intérêt est de pouvoir identifier rapidement les ", strong("concepts fréquemment utilisés"), "."),
          p("En effet, il est parfois difficile d'identifier le ", strong("concept standard"), " correspondant au concept que l'on veut aligner."),
          p("Par exemple, la requête ", tags$a(href = "https://athena.ohdsi.org/search-terms/terms?vocabulary=LOINC&page=1&pageSize=15&query=heart+rate", "heart rate dans Athena", target = "_blank"), 
            ", en filtrant sur la terminologie LOINC, affiche ", strong("plusieurs milliers"), " de résultats. Il est difficile de savoir lequel choisir."),
          p("Dans LinkR, en sélectionnant la terminologie LOINC, en cherchant 'heart rate' et en triant par le nombre de lignes ou de jeux de données, nous obtenons le ", strong("concept adéquat"), " en première position."),
          tags$p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Relation entre les concepts")),
          p("Choisissez ensuite la relation entre le concept de gauche (concept 1) et le concept de droite (concept 2), via le menu déroulant ", tags$em("Le concept 1 par rapport au concept 2"), "."),
          p("Les relations suivantes sont disponibles :"),
          tags$ul(
            tags$li(strong("Correspond à (non-std vers std)"), " : le concept 1 est équivalent au concept 2, le concept 1 n'est pas standard, le concept 2 l'est"),
            tags$li(strong("Correspond à (std vers non-std)"), " : c'est l'inverse, le concept 1 est standard, le concept 2 ne l'est pas"),
            tags$li(strong("Est inclus dans"), " : le concept 1 est inclus dans le concept 2"),
            tags$li(strong("Inclut"), " : le concept 1 inclut le concept 2")
          ),
          br()
        )
      )
    }
    
    if (language == "en"){
      r$help_vocabularies_modal_text <- div(
        p("To add an alignment, go to the ", tags$em("Add"), " tab then select two terminologies (they can be distinct or not)."),
        p("Usually, a ", strong("non-standard"), " terminology is aligned to a ", strong("standard"), " terminology. In this case, choose the non-standard terminology on the left."),
        tags$p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Left Terminology")),
        p("You can filter the left terminology by ticking ", tags$em("Unaligned Concepts Only"), ", in order to remove the concepts that are already aligned. This pertains to ", tags$em("all concept alignments"), 
          ", whether they were imported or added by users of LinkR."),
        p("The number of rows for the left terminology relates to the ", strong("selected data set"), "."),
        tags$p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Right Terminology")),
        p("You can filter the right terminology by ticking ", tags$em("Display Only Used Concepts"), ", in order to remove the concepts that have never been used in ", strong("all data sets"), " loaded on the application."),
        p("The number of rows for the right terminology pertains to ", strong("all data sets"), " loaded on the application."),
        p("Also mentioned is the ", strong("number of data sets"), " containing each concept."),
        p("The goal is to quickly identify ", strong("frequently used concepts"), "."),
        p("Indeed, it is sometimes difficult to identify the ", strong("standard concept"), " corresponding to the concept that you want to align."),
        p("For example, the query ", tags$a(href = "https://athena.ohdsi.org/search-terms/terms?vocabulary=LOINC&page=1&pageSize=15&query=heart+rate", "heart rate in Athena", target = "_blank"), ", filtering on LOINC terminology, displays ",
          strong("several thousand"), " results. It is difficult to know which one to choose."),
        p("In LinkR, by selecting the LOINC terminology, searching for 'heart rate' and sorting by the number of rows or data sets, we obtain the ", strong("suitable concept"), " in the first position."),
        tags$p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Relation between concepts")),
        p("Then choose the relationship between the left concept (concept 1) and the right concept (concept 2), via the drop-down menu ", tags$em("Concept 1 in relation to Concept 2"), "."),
        p("The following relationships are available:"),
        tags$ul(
          tags$li(strong("Corresponds to (non-std to std)"), ": concept 1 is equivalent to concept 2, concept 1 is not standard, concept 2 is"),
          tags$li(strong("Corresponds to (std to non-std)"), ": it's the opposite, concept 1 is standard, concept 2 is not"),
          tags$li(strong("Is included in"), ": concept 1 is included in concept 2"),
          tags$li(strong("Includes"), ": concept 1 includes concept 2")
        ),
        br()
      )
    }
  })
  
  # Evaluate and delete concepts mappings
  
  observeEvent(r$help_vocabularies_page_5, {
    
    load_help_page(r)
    
    r$help_vocabularies_modal_title <- i18n$t("evaluate_and_delete_concepts_mappings")
    
    if (language == "fr"){
      r$help_vocabularies_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-thumbs-up", style = "color: steelblue;"), " ", strong("Évaluer des alignements")),
        p("Pour évaluer des alignements, allez dans l'onglet ", tags$em("Évaluer & éditer"), "."),
        p("Vous pouvez :"),
        tags$ul(
          tags$li("filtrer les alignements que vous n'avez pas encore évalués"),
          tags$li("afficher les détails de l'alignement de concept sélectionné")
        ),
        p("Cliquez sur :"),
        tags$ul(
          tags$li(shiny::actionButton("thumbs_up_button_help", "", icon = icon("thumbs-up")), "  pour valider un alignement conforme"),
          tags$li(shiny::actionButton("thumbs_up_button_help", "", icon = icon("thumbs-down")), "  pour invalider un alignement")
        ),
        p("Les alignements qui comportent plus d'évaluations négatives que positives ou aucune évaluation positive ne seront pas utilisés dans l'application."),
        p("Les ", strong("alignements validés"), " (avec au moins une évaluation positive et plus d'évaluations positives que négatives) seront automatiquement utilisés par l'application, lors de la création de widgets et de plugins."),
        p("N'oubliez pas de ", strong("sauvegarder"), " vos évaluations."),
        tags$h3(tags$i(class = "fa fa-trash", style = "color: steelblue;"), " ", strong("Supprimer des alignements")),
        p("Pour supprimer des alignements, rendez-vous dans l'onglet ", tags$em("Évaluer & éditer"), "."),
        p("Si vous n'avez pas coché ", tags$em("Afficher les détails de l'alignement sélectionné"), ", vous pouvez sélectionner plusieurs alignements à supprimer en cliquant sur ", tags$em("Supprimer la sélection"), "."),
        p("Vous pouvez également supprimer les alignements via l'icône  ", shiny::actionButton("delete_help", "", icon = icon("trash-alt")), "  dans le tableau."),
        br()
        
      )
    }
    
    if (language == "en"){
      r$help_vocabularies_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-thumbs-up", style = "color: steelblue;"), " ", strong("Evaluate mappings")),
        p("To evaluate concepts mappings, go to the ", tags$em("Evaluate & edit"), " tab."),
        p("You can:"),
        tags$ul(
          tags$li("filter mappings that you have not yet evaluated"),
          tags$li("display the details of the selected concept mapping")
        ),
        p("Click on :"),
        tags$ul(
          tags$li(shiny::actionButton("thumbs_up_button_help", "", icon = icon("thumbs-up")), "  to validate a conform mapping"),
          tags$li(shiny::actionButton("thumbs_up_button_help", "", icon = icon("thumbs-down")), "  to invalidate a mpping")
        ),
        p("Concepts mappings that have more negative evaluations than positive or no positive evaluation will not be used in the application."),
        p(strong("Validated mappings"), " (with at least one positive evaluation and more positive evaluations than negative) will be automatically used by the application, in the creation of widgets and plugins."),
        p("Do not forget to ", strong("save"), " your evaluations."),
        tags$h3(tags$i(class = "fa fa-trash", style = "color: steelblue;"), " ", strong("Delete mappings")),
        p("To delete concepts mappings, go to the ", tags$em("Evaluate & edit"), " tab."),
        p("If you did not check ", tags$em("Display details of the selected mapping"), ", you can select several mappings to delete them, by clicking on ", tags$em("Delete selection"), "."),
        p("You can also delete mappings via the  ", shiny::actionButton("delete_help", "", icon = icon("trash-alt")), "  icon in the table."),
        br()
      )
    }
  })
  
}
