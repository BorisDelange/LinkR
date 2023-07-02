help_data <- function(output, r = shiny::reactiveValues(), id = character(), prefix = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
  
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r[[paste0("help_data_", prefix, "_open_panel")]],
      br(),
      strong(i18n$t("data")), br(), br(),
      shiny.fluent::Link(i18n$t("load_data"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("data_model"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("patient_lvl_or_aggregated_data"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_3', Math.random()); }"))), br(), br(),
      strong(i18n$t("tabs")), br(), br(),
      shiny.fluent::Link(i18n$t("whats_a_tab"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_4', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("add_a_tab"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_5', Math.random()); }"))), br(), br(),
      strong(i18n$t("widgets")), br(), br(),
      shiny.fluent::Link(i18n$t("whats_a_widget"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_6', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("add_a_widget"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_7', Math.random()); }"))), br(), br(),
      isLightDismiss = r[[paste0("help_data_", prefix, "_open_panel_light_dismiss")]],
      isBlocking = r[[paste0("help_data_", prefix, "_open_panel_light_dismiss")]],
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({

    shiny.fluent::Modal(
      isOpen = r[[paste0("help_data_", prefix, "_open_modal")]], dragOptions = TRUE, isModeless = TRUE, topOffsetFixed = TRUE,
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
      div(
        style = "width: 1000px; padding: 15px 10px 0px 15px;",
        shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
          div(style = list(display = "flex"),
            shiny.fluent::Text(r[[paste0("help_data_", prefix, "_modal_title")]], variant = "large"),
            div(style = list(flexGrow = 1)),
            shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
          ),
          r[[paste0("help_data_", prefix, "_modal_text")]]
        )
      )
    )
  })
  
  load_help_page <- function(r){
    r[[paste0("help_data_", prefix, "_open_modal")]] <- TRUE
    r[[paste0("help_data_", prefix, "_open_panel_light_dismiss")]] <- FALSE
  }
  
  # Load data
  
  observeEvent(r[[paste0("help_data_", prefix, "_page_1")]], {

    load_help_page(r)
    
    r[[paste0("help_data_", prefix, "_modal_title")]] <- i18n$t("load_data")
    
    if (language == "fr"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-database", style = "color: steelblue;"), " ", strong("Choisir un set de données")),
        p("Un set de données contient les données d'un ", strong("groupe de patients"), "."),
        p("Un même set de données peut contenir ", strong("plusieurs études"), "."),
        p("Choisissez le set de données dans le menu déroulant sur la gauche de l'écran."),
        tags$h3(tags$i(class = "fa fa-rectangle-list", style = "color: steelblue;"), " ", strong("Choisir une étude")),
        p("Choisissez ensuite une étude, dans le menu déroulant."),
        p("Vous pouvez créer des études depuis l'onglet ", tags$em("Mes études"), " en haut de l'écran."),
        p("Une même étude peut contenir ", strong("plusieurs subsets"), "."),
        tags$h3(tags$i(class = "fa fa-users", style = "color: steelblue;"), " ", strong("Choisir un subset")),
        p("Un subset est un sous-ensemble du set de données, sur des patients sélectionnés."),
        p("Il est possible de ", strong("créer d'autres subsets"), " depuis l'onglet ", tags$em("Mes subsets"), "."),
        tags$h3(tags$i(class = "fa fa-user", style = "color: steelblue;"), " ", strong("Choisir un patient & un séjour")),
        p("En chargeant un subset, la liste des patients appartenant à ce subset est chargée dans le menu déroulant ", tags$em("Patient"),
          ", seulement si l'on se trouve dans les ", tags$em("Données individuelles"), "."),
        p("Les ", strong("widgets se mettent à jour"), " à chaque changement de patient & de séjour."), 
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        tags$h3(tags$i(class = "fa fa-database", style = "color: steelblue;"), " ", strong("Choose a dataset")),
        p("A dataset contains the data of a ", strong("group of patients"), "."),
        p("The same dataset can contain ", strong("multiple studies"), "."),
        p("Choose the dataset from the dropdown menu on the left of the screen."),
        tags$h3(tags$i(class = "fa fa-rectangle-list", style = "color: steelblue;"), " ", strong("Choose a study")),
        p("Next, choose a study from the dropdown menu."),
        p("You can create studies from the ", tags$em("My studies"), " tab at the top of the screen."),
        p("The same study can contain ", strong("multiple subsets"), "."),
        tags$h3(tags$i(class = "fa fa-users", style = "color: steelblue;"), " ", strong("Choose a subset")),
        p("A subset is a subset of the dataset, containing selected patients."),
        p("It is possible to ", strong("create other subsets"), " from the ", tags$em("My subsets"), " tab."),
        tags$h3(tags$i(class = "fa fa-user", style = "color: steelblue;"), " ", strong("Choose a patient & a stay")),
        p("When loading a subset, the list of patients belonging to that subset is loaded into the ", tags$em("Patient"),
          " dropdown menu, only if you are in the ", tags$em("Individual data"), " section."),
        p("The ", strong("widgets update"), " every time the patient & stay are changed."), 
        br()
      )
    }
  })
  
  # Data model
  
  observeEvent(r[[paste0("help_data_", prefix, "_page_2")]], {
    
    load_help_page(r)
    
    r[[paste0("help_data_", prefix, "_modal_title")]] <- i18n$t("data_model")
    
    if (language == "fr"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Le modèle de données utilisé par l'application est le ", strong("modèle standard OMOP"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Vous pouvez accéder aux ",
          tags$a(href = "https://ohdsi.github.io/CommonDataModel/cdm60.html", "détails du modèle de données ici", target = "_blank"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Au chargement d'un set de données, les variables suivantes se chargent."),
        strong("Variables des établissements et des praticiens"), " :",
        tags$ul(
          tags$li("d$location"),
          tags$li("d$care_site"),
          tags$li("d$provider")
        ),
        strong("Variables des patients et séjours"), " :",
        tags$ul(
          tags$li("d$person"),
          tags$li("d$death"),
          tags$li("d$observation_period"),
          tags$li("d$visit_occurrence"),
          tags$li("d$visit_detail"),
          tags$li("d$payer_plan_period"),
          tags$li("d$cost")
        ),
        strong("Variables de données cliniques"), " :",
        tags$ul(
          tags$li("d$condition_occurrence"),
          tags$li("d$drug_exposure"),
          tags$li("d$procedure_occurrence"),
          tags$li("d$device_exposure"),
          tags$li("d$measurement"),
          tags$li("d$observation"),
          tags$li("d$note"),
          tags$li("d$note_nlp"),
          tags$li("d$specimen"),
          tags$li("d$fact_relationship"),
          tags$li("d$drug_era"),
          tags$li("d$dose_era"),
          tags$li("d$condition_era")
        ),
        strong("Variables contenant les concepts"), " :",
        tags$ul(
          tags$li("d$dataset_all_concepts", " : rassemble tous les concepts utilisés par le set de données chargé"),
          tags$li("d$dataset_drug_strength", " : contient les informations sur les médicaments, issues de la table OMOP drug_strength")
        )
      )
    }
    
    if (language == "en"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "The data model used by the application is the ", strong("OMOP standard model"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "You can access the ",
          tags$a(href = "https://ohdsi.github.io/CommonDataModel/cdm60.html", "details of the data model here", target = "_blank"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Upon loading a data set, the following variables are loaded."),
        strong("Institution and practitioner variables"), " :",
        tags$ul(
          tags$li("d$location"),
          tags$li("d$care_site"),
          tags$li("d$provider")
        ),
        strong("Patient and stay variables"), " :",
        tags$ul(
          tags$li("d$person"),
          tags$li("d$death"),
          tags$li("d$observation_period"),
          tags$li("d$visit_occurrence"),
          tags$li("d$visit_detail"),
          tags$li("d$payer_plan_period"),
          tags$li("d$cost")
        ),
        strong("Clinical data variables"), " :",
        tags$ul(
          tags$li("d$condition_occurrence"),
          tags$li("d$drug_exposure"),
          tags$li("d$procedure_occurrence"),
          tags$li("d$device_exposure"),
          tags$li("d$measurement"),
          tags$li("d$observation"),
          tags$li("d$note"),
          tags$li("d$note_nlp"),
          tags$li("d$specimen"),
          tags$li("d$fact_relationship"),
          tags$li("d$drug_era"),
          tags$li("d$dose_era"),
          tags$li("d$condition_era")
        ),
        strong("Concepts variables"), " :",
        tags$ul(
          tags$li("d$dataset_all_concepts", " : gathers all the concepts used by the loaded dataset"),
          tags$li("d$dataset_drug_strength", " : contains information about drugs, from the OMOP drug_strength table")
        )
      )
    }
  })
  
  # Patient-level or aggregated data
  
  observeEvent(r[[paste0("help_data_", prefix, "_page_3")]], {
    
    load_help_page(r)
    
    r[[paste0("help_data_", prefix, "_modal_title")]] <- i18n$t("patient_lvl_or_aggregated_data")
    
    if (language == "fr"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p("Vous pouvez choisir dans le menu à gauche de charger les données individuelles ou agrégées."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Modules & plugins différents")),
        p("Selon que vous choisissez les données individuelles ou agrégées, les tabs & widgets chargés diffèrent."),
        p("Lorsque vous chargez une étude, vous chargez :"),
        tags$ul(
          tags$li("D'un côté les onglets & widgets de données individuelles, permettant de ", strong("visualiser les données patient par patient"), "."),
          tags$li("De l'autre côté les onglets & widgets de données agrégées, permettant de ", strong("visualiser les données sur l'ensemble des patients ou sur un subset sélectionné."))
        ),
        p("En pratique, cela crée de ", strong("nouvelles variables"), " filtrant les variables générales sur ",
          strong("le subset, le patient ou sur le séjour sélectionné"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Données agrégées - Variables du subset sélectionné")),
        p("Lorsque vous sélectionnez un subset, les variables suivantes sont créées, avec la même structure que détaillée dans ", tags$em("Modèle de données"), " :"),
        tags$ul(
          tags$li("d$data_subset$persons"),
          tags$li("d$data_subset$condition_occurrence"),
          tags$li("d$data_subset$drug_exposure"),
          tags$li("d$data_subset$procedure_occurrence"),
          tags$li("d$data_subset$device_exposure"),
          tags$li("d$data_subset$measurement"),
          tags$li("d$data_subset$observation"),
          tags$li("d$data_subset$death"),
          tags$li("d$data_subset$note"),
          tags$li("d$data_subset$note_nlp"),
          tags$li("d$data_subset$specimen"),
          tags$li("d$data_subset$fact_relationship"),
          tags$li("d$data_subset$payer_plan_period"),
          tags$li("d$data_subset$cost"),
          tags$li("d$data_subset$drug_era"),
          tags$li("d$data_subset$dose_era"),
          tags$li("d$data_subset$condition_era"),
          tags$li("d$data_subset$person"),
          tags$li("d$data_subset$observation_period"),
          tags$li("d$data_subset$visit_occurrence"),
          tags$li("d$data_subset$visit_detail")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Données individuelles - Variables du patient sélectionné")),
        p("Lorsque vous sélectionnez un patient, les variables suivantes sont créées :"),
        tags$ul(
          tags$li("d$data_person$condition_occurrence"),
          tags$li("d$data_person$drug_exposure"),
          tags$li("d$data_person$procedure_occurrence"),
          tags$li("d$data_person$device_exposure"),
          tags$li("d$data_person$measurement"),
          tags$li("d$data_person$observation"),
          tags$li("d$data_person$note"),
          tags$li("d$data_person$note_nlp"),
          tags$li("d$data_person$fact_relationship"),
          tags$li("d$data_person$payer_plan_period"),
          tags$li("d$data_person$cost"),
          tags$li("d$data_person$specimen"),
          tags$li("d$data_person$death"),
          tags$li("d$data_person$drug_era"),
          tags$li("d$data_person$dose_era"),
          tags$li("d$data_person$condition_era")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Données individuelles - Variables du séjour sélectionné")),
        p("De la même façon, lorsque vous sélectionnez un séjour, les variables suivantes sont créées :"),
        tags$ul(
          tags$li("d$data_visit_detail$condition_occurrence"),
          tags$li("d$data_visit_detail$drug_exposure"),
          tags$li("d$data_visit_detail$procedure_occurrence"),
          tags$li("d$data_visit_detail$device_exposure"),
          tags$li("d$data_visit_detail$measurement"),
          tags$li("d$data_visit_detail$observation"),
          tags$li("d$data_visit_detail$note"),
          tags$li("d$data_visit_detail$note_nlp"),
          tags$li("d$data_visit_detail$fact_relationship"),
          tags$li("d$data_visit_detail$payer_plan_period"),
          tags$li("d$data_visit_detail$cost")
        ), br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p("You can choose in the left menu to load patient-level or aggregated data."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Different modules & plugins")),
        p("Depending on whether you choose patient-level or aggregated data, the tabs & widgets loaded differ."),
        p("When you load a study, you load:"),
        tags$ul(
          tags$li("On one hand, the patient-level data tabs & widgets, allowing to ", strong("visualize data patient by patient"), "."),
          tags$li("On the other hand, the aggregated data tabs & widgets, allowing to ", strong("visualize data on all patients or on a selected subset."))
        ),
        p("In practice, this creates ", strong("new variables"), " filtering the general variables on ",
          strong("the selected subset, patient or stay"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Aggregated Data - selected subset variables")),
        p("When you select a subset, the following variables are created, with the same structure as detailed in ", tags$em("Data model"), " :"),
        tags$ul(
          tags$li("d$data_subset$persons"),
          tags$li("d$data_subset$condition_occurrence"),
          tags$li("d$data_subset$drug_exposure"),
          tags$li("d$data_subset$procedure_occurrence"),
          tags$li("d$data_subset$device_exposure"),
          tags$li("d$data_subset$measurement"),
          tags$li("d$data_subset$observation"),
          tags$li("d$data_subset$death"),
          tags$li("d$data_subset$note"),
          tags$li("d$data_subset$note_nlp"),
          tags$li("d$data_subset$specimen"),
          tags$li("d$data_subset$fact_relationship"),
          tags$li("d$data_subset$payer_plan_period"),
          tags$li("d$data_subset$cost"),
          tags$li("d$data_subset$drug_era"),
          tags$li("d$data_subset$dose_era"),
          tags$li("d$data_subset$condition_era"),
          tags$li("d$data_subset$person"),
          tags$li("d$data_subset$observation_period"),
          tags$li("d$data_subset$visit_occurrence"),
          tags$li("d$data_subset$visit_detail")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Patient-level data - selected patient variables")),
        p("When you select a patient, the following variables are created :"),
        tags$ul(
          tags$li("d$data_person$condition_occurrence"),
          tags$li("d$data_person$drug_exposure"),
          tags$li("d$data_person$procedure_occurrence"),
          tags$li("d$data_person$device_exposure"),
          tags$li("d$data_person$measurement"),
          tags$li("d$data_person$observation"),
          tags$li("d$data_person$note"),
          tags$li("d$data_person$note_nlp"),
          tags$li("d$data_person$fact_relationship"),
          tags$li("d$data_person$payer_plan_period"),
          tags$li("d$data_person$cost"),
          tags$li("d$data_person$specimen"),
          tags$li("d$data_person$death"),
          tags$li("d$data_person$drug_era"),
          tags$li("d$data_person$dose_era"),
          tags$li("d$data_person$condition_era")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Patient-level data - selected stay variables")),
        p("In the same way, when you select a stay, the following variables are created :"),
        tags$ul(
          tags$li("d$data_visit_detail$condition_occurrence"),
          tags$li("d$data_visit_detail$drug_exposure"),
          tags$li("d$data_visit_detail$procedure_occurrence"),
          tags$li("d$data_visit_detail$device_exposure"),
          tags$li("d$data_visit_detail$measurement"),
          tags$li("d$data_visit_detail$observation"),
          tags$li("d$data_visit_detail$note"),
          tags$li("d$data_visit_detail$note_nlp"),
          tags$li("d$data_visit_detail$fact_relationship"),
          tags$li("d$data_visit_detail$payer_plan_period"),
          tags$li("d$data_visit_detail$cost")
        ), br()
      )
    }
  })
  
  # What is a tab
  
  observeEvent(r[[paste0("help_data_", prefix, "_page_4")]], {
    
    load_help_page(r)
    
    r[[paste0("help_data_", prefix, "_modal_title")]] <- i18n$t("whats_a_tab")
    
    if (language == "fr"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p("Une étude est ", strong("structurée autour d'onglets"), ", qui sont des ", strong("pages personnalisées"),
          " sur lesquelles je choisis ", strong("quelles données afficher et sous quelle forme"), "."),
        tags$h3(tags$i(class = "fa fa-user", style = "color: steelblue;"), " ", strong("Onglets de données individuelles")),
        p("Les onglets de données individuelles ", strong("reproduisent un dossier clinique"), "."),
        p("Par exemple, si je fais une étude sur le choc septique, je crée un onglet ", tags$em("Hémodynamique"),
            " où j'affiche la FC, la PAs, la PAd, la PAm & les doses reçues de Noradrénaline."),
        tags$h3(tags$i(class = "fa fa-users", style = "color: steelblue;"), " ", strong("Onglets de données agrégées")),
        p("Les onglets de données agrégées ", strong("permettent de réaliser une étude"), " sur mes données."),
        p("Par exemple, je peux créer un onglet ", tags$em("Critères d'exclusion"), " où je vais créer mes critères ",
          "d'exclusion et les appliquer à mes patients."),
        p("Je peux également créer un onglet ", tags$em("Flowchart"), " pour afficher le flowchart de mon étude."), br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p("An study is ", strong("structured around tabs"), ", which are ", strong("custom pages"),
          " where I choose ", strong("which data to display and in what form"), "."),
        tags$h3(tags$i(class = "fa fa-user", style = "color: steelblue;"), " ", strong("Individual data tabs")),
        p("Individual data tabs ", strong("reproduce a clinical folder"), "."),
        p("For example, if I am conducting a study on septic shock, I can create a ", tags$em("Hemodynamics"),
          " tab where I display HR, SBP, DBP, MAP & the received doses of Norepinephrine."),
        tags$h3(tags$i(class = "fa fa-users", style = "color: steelblue;"), " ", strong("Aggregate data tabs")),
        p("Aggregate data tabs ", strong("allow to conduct a study"), " on my data."),
        p("For example, I can create an ", tags$em("Exclusion criteria"), " tab where I will create my exclusion criteria ",
          "and apply them to my patients."),
        p("I can also create a ", tags$em("Flowchart"), " tab to display the flowchart of my study."), br()
      )
    }
  })
  
  # Add a tab
  
  observeEvent(r[[paste0("help_data_", prefix, "_page_5")]], {
    
    load_help_page(r)
    
    r[[paste0("help_data_", prefix, "_modal_title")]] <- i18n$t("add_a_tab")
    
    if (language == "fr"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p("Pour ajouter un onglet, il faut ", strong("avoir chargé une étude"), " dans le menu déroulant à gauche de l'écran."),
        p("Il faut ensuite cliquer sur l'icône :"),
        div(shiny.fluent::Icon(iconName = "Add"), span(i18n$t("add_a_tab"), style = "padding:0px 0px 10px 10px;")),
        p("Elle se trouve sous le titre (", tags$em("Données individuelles"), " ou ", tags$em("Données agrégées"), ")."),
        p("Ensuite, :"),
        tags$ul(
          tags$li(strong("Choisissez un nom"), " pour cet onglet."),
          tags$li(strong("Choisissez le niveau "), "de l'onglet. Faut-il qu'il soit au même niveau que l'onglet actuel,",
            " ou est-ce un sous-onglet de l'onglet actuellement sélectionné ?")
        ),
        p("Lorsque le menu ", tags$em("Ajouter un onglet"), " est ouvert, cliquez sur la croix à droite du menu pour retourner aux widgets."), br()
      ) 
    }
    
    if (language == "en"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p("To add a tab, you must ", strong("have loaded a study"), " in the dropdown menu on the left side of the screen."),
        p("Then click on the icon:"),
        div(shiny.fluent::Icon(iconName = "Add"), span(i18n$t("add_a_tab"), style = "padding:0px 0px 10px 10px;")),
        p("It is located under the title (", tags$em("Individual Data"), " or ", tags$em("Aggregate Data"), ")."),
        p("Then:"),
        tags$ul(
          tags$li(strong("Choose a name"), " for this tab."),
          tags$li(strong("Choose the level"), " of the tab. Should it be at the same level as the currently selected tab,",
            " or is it a sub-tab of the currently selected tab ?")
        ),
        p("When the ", tags$em("Add a Tab"), " menu is open, click on the cross on the right side of the menu to return to the widgets."), br()
      )
    }
  })
  
  # What is a widget
  
  observeEvent(r[[paste0("help_data_", prefix, "_page_6")]], {
    
    load_help_page(r)
    
    r[[paste0("help_data_", prefix, "_modal_title")]] <- i18n$t("whats_a_widget")
    
    if (language == "fr"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p("Un onglet est ", strong("composé de widgets"), ", qui sont des plugins appliqués à des données."),
        tags$h3(tags$i(class = "fa fa-code", style = "color: steelblue;"), " ", strong("Plugins")),
        p("Les plugins sont des scripts écrits en R - Shiny permettant ", strong("d'ajouter des fonctionnalités à l'application"), "."),
        p("Quelques exemples :"),
        tags$ul(
          tags$li(strong("Plugin Datatable"), " : permet d'afficher des données sous forme de tableau."),
          tags$li(strong("Plugin Timeline"), " : permet d'afficher les données sous forme de timeline, utile pour les prescriptions par exemple."),
          tags$li(strong("Plugin Flowchart"), " : permet de créer un Flowchart à partir des données d'une étude.")
        ),
        p("L'application a vocation à s'enrichir au fur et à mesure par la ", strong("création de nouveaux plugins"), "."),
        p("Les plugins des données individuelles ou agrégées ne sont pas les mêmes."),
        tags$h3(tags$i(class = "fa fa-table", style = "color: steelblue;"), " ", strong("Widgets")),
        p("Un widget est donc un plugin appliqué à des données."),
        p("Je choisis un plugin, quelles données vont être utilisées par ce plugin, puis le ",
        strong("plugin affiche ces données sous la forme désirée"), " (timeline pour le plugin timeline etc)."), br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p("A tab is ", strong("composed of widgets"), ", which are plugins applied to data."),
        tags$h3(tags$i(class = "fa fa-code", style = "color: steelblue;"), " ", strong("Plugins")),
        p("Plugins are R scripts - Shiny allowing ", strong("to add features to the application"), "."),
        p("Some examples:"),
        tags$ul(
          tags$li(strong("Datatable Plugin"), " : allows displaying data in the form of a table."),
          tags$li(strong("Timeline Plugin"), " : allows displaying data in the form of a timeline, useful for prescriptions for example."),
          tags$li(strong("Flowchart Plugin"), " : allows creating a Flowchart from study data.")
        ),
        p("The application is intended to become richer over time by ", strong("creating new plugins"), "."),
        p("The plugins for individual or aggregated data are not the same."),
        tags$h3(tags$i(class = "fa fa-table", style = "color: steelblue;"), " ", strong("Widgets")),
        p("A widget is therefore a plugin applied to data."),
        p("I choose a plugin, which data will be used by this plugin, and then the ",
          strong("plugin displays this data in the desired form"), " (timeline for the timeline plugin, etc.)."), br()
      )
    }
  })
  
  # Add a widget
  
  observeEvent(r[[paste0("help_data_", prefix, "_page_7")]], {
    
    load_help_page(r)
    
    r[[paste0("help_data_", prefix, "_modal_title")]] <- i18n$t("add_a_widget")
    
    if (language == "fr"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", "Pour ajouter un widget, il faut ", strong("avoir chargé une étude "), " dans le menu déroulant à gauche de l'écran puis ",
        strong("avoir sélectionné un onglet"), "."),
        p("Il faut ensuite cliquer sur :"),
          div(shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_widget_help")),
            i18n$t("add_a_widget"), iconProps = list(iconName = "Add"))),
        p("Ensuite, :"),
        tags$ul(
          tags$li(strong("Choisissez un nom"), " pour ce widget."),
          tags$li(strong("Choisissez le plugin "), " que vous souhaitez utiliser pour ce widget")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", "Ajoutez ensuite des concepts."),
        tags$ul(
          tags$li(strong("Sélectionner une terminologie")),
          tags$li(strong("Sélectionner les concepts "), " que vous souhaitez utiliser pour ce widget, avec le plugin sélectionné.")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", "Lorsque le tableau des ", strong("concepts"), " est chargé, vous pouvez filtrer les données pour trouver les concepts qui vous intéresent :"),
        tags$ul(
          tags$li(strong("Nom / nom d'affichage"), " : cherchez dans la barre de texte les items. En double-cliquant sur un nom, vous pouvez le changer : il sera affiché avec ce nouveau nom."),
          tags$li(strong("Domaine"), " : filtrez les concepts selon leur domaine."),
          tags$li(strong("Patients"), " : affiche le nombre total de patients ayant au moins une fois le concept"),
          tags$li(strong("Lignes"), " : nombre d'occurences du concept dans le set de données, tous patients confondus.")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", "Vous pouvez afficher les ", strong("concepts alignés"), " en cochant ", tags$em("Afficher les concepts alignés"), "."),
        p("Un deuxième tableau s'affiche, ", strong("listant les concepts alignés"), " avec le concept sélectionné dans le tableau du haut."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Ajoutez ensuite les concepts en cliquant sur l'icône ", actionButton(ns(paste0(prefix, "_add_concept_help")), "", icon = icon("plus")),
        " dans la dernière colonne du tableau."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Si le bouton ", tags$em("Fusionner les concepts alignés"), " est activé, les différents concepts liés seront fusionnés dans le widget."),
        p("Par exemple, si j'ajoute le concept ", tags$em("Fréquence cardiaque"), " et que les concepts ", tags$em("FC"), " et ", tags$em("Fréq card"), " sont également ajoutés, ",
        "si je sélectionne la fusion, les différents concepts apparaîtront sous le nom ", tags$em("Fréquence cardiaque"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Lorsque le menu ", tags$em("Nouveau widget"), " est ouvert, cliquez sur la croix à droite du menu pour retourner à l'onglet actuel."), 
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", "Lorsque tout cela est fait, cliquez sur ", tags$em("Ajouter"), " pour ajouter le widget paramétré."),
        p("Vous pouvez ajouter plusieurs widgets par onglet."),
        br()
      )
    }
    
    if (language == "en"){
      r[[paste0("help_data_", prefix, "_modal_text")]] <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", "To add a widget, you must ", strong("have loaded a study "), " in the dropdown menu on the left of the screen then ",
          strong("have selected a tab"), "."),
        p("Then you need to click on:"),
        div(shiny.fluent::ActionButton.shinyInput(ns(paste0(prefix, "_add_widget_help")),
          i18n$t("add_a_widget"), iconProps = list(iconName = "Add"))),
        p("Next, :"),
        tags$ul(
          tags$li(strong("Choose a name"), " for this widget."),
          tags$li(strong("Choose the plugin "), " that you wish to use for this widget")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", "Then add concepts."),
        tags$ul(
          tags$li(strong("Select a vocabulary")),
          tags$li(strong("Select the concepts "), " that you wish to use for this widget, with the selected plugin.")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", "When the ", strong("concepts"), " table is loaded, you can filter the data to find the concepts that interest you:"),
        tags$ul(
          tags$li(strong("Name / display name"), " : search in the text bar for items. By double-clicking on a name, you can change it: it will be displayed with this new name."),
          tags$li(strong("Domain"), " : filter the concepts according to their domain."),
          tags$li(strong("Patients"), " : displays the total number of patients having at least once the concept"),
          tags$li(strong("Lines"), " : number of occurrences of the concept in the data set, all patients combined.")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", "You can display the ", strong("aligned concepts"), " by checking ", tags$em("Show aligned concepts"), "."),
        p("A second table is displayed, ", strong("listing the mapped concepts"), " with the concept selected in the top table."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Then add the concepts by clicking on the icon ", actionButton(ns(paste0(prefix, "_add_concept_help")), "", icon = icon("plus")),
          " in the last column of the table."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "If the ", tags$em("Merge mapped concepts"), " button is activated, the different linked concepts will be merged in the widget."),
        p("For example, if I add the concept ", tags$em("Heart Rate"), " and the concepts ", tags$em("HR"), " and ", tags$em("Heart Freq"), " are also added, ",
          "if I select the merge, the different concepts will appear under the name ", tags$em("Heart Rate"), "."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "When the ", tags$em("New widget"), " menu is open, click on the cross on the right of the menu to return to the current tab."), 
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", "When all of this is done, click on ", tags$em("Add"), " to add the configured widget."),
        p("You can add multiple widgets per tab."),
        br()
      )
    }
  })
}
