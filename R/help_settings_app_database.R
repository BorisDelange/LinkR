help_settings_app_database <- function(input, output, r = shiny::reactiveValues(), id = character(), language = "en", i18n = character(), ns = character()){
  
  output$help_panel <- shiny.fluent::renderReact({
    
    shiny.fluent::Panel(
      headerText = i18n$t("help"),
      isOpen = r$help_settings_app_database_open_panel,
      br(),
      shiny.fluent::Link(i18n$t("database_structure"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_1', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("db_connection_infos_card"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_2', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("db_datatable_card"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_3', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("db_request_card"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_4', Math.random()); }"))), br(), br(),
      shiny.fluent::Link(i18n$t("db_export_and_restore"), onClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-help_page_5', Math.random()); }"))), br(), br(),
      isLightDismiss = r$help_settings_app_database_open_panel_light_dismiss,
      isBlocking = r$help_settings_app_database_open_panel_light_dismiss,
      onDismiss = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }")),
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_panel', Math.random()); }"))
    )
  })
  
  output$help_modal <- shiny.fluent::renderReact({
    
    shiny.fluent::Modal(
      isOpen = r$help_settings_app_database_open_modal, dragOptions = TRUE, isModeless = FALSE, topOffsetFixed = TRUE,
      onLightDismissClick = htmlwidgets::JS(paste0("function() { Shiny.setInputValue('", id, "-hide_modal_2', Math.random()); }")),
      div(
        style = "width: 1000px; padding: 15px 10px 0px 15px;",
        shiny.fluent::Stack(tokens = list(childrenGap = "10px"),
          div(style = list(display = "flex"),
            shiny.fluent::Text(r$help_settings_app_database_modal_title, variant = "large"),
            div(style = list(flexGrow = 1)),
            shiny.fluent::IconButton.shinyInput(ns("hide_modal"), iconProps = list(iconName = "Cancel")),
          ),
          r$help_settings_app_database_modal_text
        )
      )
    )
  })
  
  load_help_page <- function(r){
    r$help_settings_app_database_open_modal <- TRUE
    r$help_settings_app_database_open_panel_light_dismiss <- FALSE
  }
  
  # Code divs
  
  code_1 <- paste0("r$datasets\n\n",
    "DBI::dbGetQuery(r$db, \"SELECT * FROM datasets\")")
  div_code_1 <- div(
    "r$datasets", br(), br(),
    "DBI::dbGetQuery(r$db, \"SELECT * FROM datasets\")",
    shiny.fluent::IconButton.shinyInput(ns("copy_code_1"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  
  code_2 <- paste0("m$subsets\n\n",
    "DBI::dbGetQuery(m$db, \"SELECT * FROM patient_lvl_widgets_options\")")
  div_code_2 <- div(
    div("m$subsets", br(), br(),
      "DBI::dbGetQuery(m$db, \"SELECT * FROM patient_lvl_widgets_options\")"),
    shiny.fluent::IconButton.shinyInput(ns("copy_code_2"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  
  code_3 <- "linkr(language = \"fr\", app_folder = \"/Users/username\")"
  div_code_3 <- div(
    code_3,
    shiny.fluent::IconButton.shinyInput(ns("copy_code_3"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  
  code_4 <- paste0(
    "SELECT u.username, u.firstname, u.lastname, ua.name AS user_access_name\n",
    "FROM users u\n",
    "LEFT JOIN users_accesses ua ON u.user_access_id = ua.id"
  )
  div_code_4 <- div(
    "SELECT u.username, u.firstname, u.lastname, ua.name AS user_access_name", br(),
    "FROM users u", br(),
    "LEFT JOIN users_accesses ua ON u.user_access_id = ua.id",
    shiny.fluent::IconButton.shinyInput(ns("copy_code_4"), iconProps = list(iconName = "Copy"), style = "position:absolute; top:5px; right:5px;"),
    style = r$code_style
  )
  
  # Database structure
  
  observeEvent(r$help_settings_app_database_page_1, {
    
    load_help_page(r)
    
    r$help_settings_app_database_modal_title <- i18n$t("database_structure")
    
    if (language == "fr"){
      r$help_settings_app_database_modal_text <- div(
        p("La base de données, qu'elle soit locale ou distante, est composée de ", strong("deux bases de données"), 
          " : la base de données principale et la base de données publique."),
        tags$h3(tags$i(class = "fa fa-database", style = "color: steelblue;"), " ", strong("Base de données principale")),
        p("La base de données principale comprend la ", strong("majorité des tables servant au fonctionnement de l'application"), 
          ", y compris les tables concernant les utilisateurs, les mots de passe cryptés etc."),
        p("Cette BDD a été séparée afin d'y ", strong("restreindre l'accès"), "."),
        p("On accède à ces tables via la variable ", tags$em("r"), ", ou par requête avec la librairie ", tags$em("DBI"), ". Par exemple :"),
        div_code_1,
        tags$h3(tags$i(class = "fa fa-database", style = "color: steelblue;"), " ", strong("Base de données publique")),
        p("La base de données publique comprend les tables qui concernent les ", strong("études et les données"),
          ". Ces données seront accessibles par l'ensemble des utilisateurs."),
        p("On accède à ces tables via la variable ", tags$em("m"), ", ou par requête avec la librairie ", tags$em("DBI"), ". Par exemple :"),
        div_code_2,
        p("Les tables sont détaillées dans la rubrique ", tags$em("Tables de la BDD"), "."), 
        br()
      )
    }
    
    if (language == "en"){
      r$help_settings_app_database_modal_text <- div(
        p("The database, whether local or remote, is composed of ", strong("two databases"),
          " : the main database and the public database."),
        tags$h3(tags$i(class = "fa fa-database", style = "color: steelblue;"), " ", strong("Main database")),
        p("The main database includes the ", strong("majority of tables used to make the application work"),
          ", including tables related to users, encrypted passwords, etc."),
        p("This database has been separated in order to ", strong("restrict access"), "."),
        p("These tables can be accessed via the ", tags$em("r"), " variable, or by query using the ", tags$em("DBI"), " library. For example:"),
        div_code_1,
        tags$h3(tags$i(class = "fa fa-database", style = "color: steelblue;"), " ", strong("Public database")),
        p("The public database includes tables related to ", strong("studies and data"),
          ". This data will be accessible to all users."),
        p("These tables can be accessed via the ", tags$em("m"), " variable, or by query using the ", tags$em("DBI"), " library. For example:"),
        div_code_2,
        p("The tables are detailed in the section ", tags$em("Database Tables"), "."),
        br()
      )
    }
  })
  
  # Database connection informations
  
  observeEvent(r$help_settings_app_database_page_2, {
    
    load_help_page(r)
    
    r$help_settings_app_database_modal_title <- i18n$t("db_connection_infos_card")
    
    if (language == "fr"){
      r$help_settings_app_database_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-database", style = "color: steelblue;"), " ", strong("Type de connexion")),
        p("Au lancement de l'application, une ", strong("connexion"), " est établie vers la ", strong("base de données de l'application"), "."),
        p("Par défaut, la connexion est établie avec une ", strong("base de données locale"), "."),
        p("Il est également possible de se connecter à une ", strong("base de données distante"), "."),
        tags$h3(tags$i(class = "fa fa-house", style = "color: steelblue;"), " ", strong("Connexion locale")),
        p("Lorsque vous choisissez la connexion locale, des ", strong("fichiers SQLite"), " sont créés dans le ", strong("dossier de l'application"),
          ", qui est ", tags$em("app_folder/app_database"), ", où app_folder est le dossier renseigné par l'argument du même nom",
          " lors du lancement de l'application."),
        p("Par exemple, si vous lancez l'application avec :"),
        div_code_3,
        p("Les fichiers de la base de données seront enregistrés dans ", tags$em("/Users/username/linkr/app_database"), "."),
        tags$h3(tags$i(class = "fa fa-cloud", style = "color: steelblue;"), " ", strong("Connexion distante")),
        p("Pour ", strong("configurer une connexion distante"), ", vous devez remplir les champs suivants :"),
        tags$ul(
          tags$li(strong("Librairie SQL"), " : la librairie SQL utilisée par la BDD distante. ",
          " Pour l'instant, uniquement ", tags$em("PostgreSQL"), " et ", tags$em("SQLite"), " sont disponibles."),
          tags$li(strong("Hôte"), " : l'adresse de la base de données distante, par exemple ", tags$em("localhost"), ", ou une adresse URL"),
          tags$li(strong("Nom de la BDD principale"), " : le nom de la BDD principale, qui ", strong("doit être créée au prélable")),
          tags$li(strong("Nom de la BDD publique"), " : le nom de la BDD publique, qui ", strong("doit être créée au prélable")),
          tags$li(strong("Port"), " : le numéro du port, par exemple 5432"),
          tags$li(strong("Utilisateur"), " : le login pour se connecter à la BDD distante"),
          tags$li(strong("Mot de passe"), " : le mot de passe qui est associé au compte")
        ),
        p("Penser à sauvegarder ces paramètres en cliquant sur ", tags$em("Sauvegader"), "."),
        p("Vous pouvez tester la connexion avec les paramètres rentrés en cliquant sur ", tags$em("Tester la connexion"), "."),
        p("Deux cas ce figure :"),
        tags$ul(
          tags$li("Soit la connexion fonctionne, vous avez sauvegardé ces paramètres, et alors au prochain chargement de l'application,",
          " la ", strong("connexion distante sera utilisée par défaut"), "."),
          tags$li("Soit la connexion ne fonctionne pas (ou ne fonctionne plus à un moment donné), et alors on retourne sur la",
            " connexion locale.")
        ), 
        br()
      )
    }
    
    if (language == "en"){
      r$help_settings_app_database_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-database", style = "color: steelblue;"), " ", strong("Connection type")),
        p("At the launch of the application, a ", strong("connection"), " is established to the ", strong("application database"), "."),
        p("By default, the connection is established with a ", strong("local database"), "."),
        p("It is also possible to connect to a ", strong("remote database"), "."),
        tags$h3(tags$i(class = "fa fa-database", style = "color: steelblue;"), " ", strong("Local connection")),
        p("When you choose the local connection, ", strong("SQLite files"), " are created in the ", strong("application folder"),
          ", which is ", tags$em("app_folder/app_database"), ", where app_folder is the folder specified by the argument of the same name",
          " at the launch of the application."),
        p("For example, if you launch the application with:"),
        div_code_3,
        p("The database files will be saved in ", tags$em("/Users/username/linkr/app_database"), "."),
        tags$h3(tags$i(class = "fa fa-database", style = "color: steelblue;"), " ", strong("Remote connection")),
        p("To ", strong("configure a remote connection"), ", you have to fill in the following fields:"),
        tags$ul(
          tags$li(strong("SQL library"), " : the SQL library used by the remote database. ",
            " For now, only ", tags$em("PostgreSQL"), " and ", tags$em("SQLite"), " are available."),
          tags$li(strong("Host"), " : the address of the remote database, for example ", tags$em("localhost"), ", or a URL"),
          tags$li(strong("Name of the main database"), " : the name of the main database, which ", strong("must be created beforehand")),
          tags$li(strong("Name of the public database"), " : the name of the public database, which ", strong("must be created beforehand")),
          tags$li(strong("Port"), " : the port number, for example 5432"),
          tags$li(strong("User"), " : the login to connect to the remote database"),
          tags$li(strong("Password"), " : the password associated with the account")
        ),
        p("Remember to save these settings by clicking on ", tags$em("Save"), "."),
        p("You can test the connection with the entered parameters by clicking on ", tags$em("Test connection"), "."),
        p("Two cases are possible:"),
        tags$ul(
          tags$li("Either the connection works, you have saved these settings, and then the next time you load the application,",
            " the ", strong("remote connection will be used by default"), "."),
          tags$li("Or the connection does not work (or stops working at some point), and then we fall back on the",
            " local connection.")
        ),
        br()
      )
    }
  })
  
  # Database tables
  
  observeEvent(r$help_settings_app_database_page_3, {
    
    load_help_page(r)
    
    r$help_settings_app_database_modal_title <- i18n$t("db_datatable_card")
    
    if (language == "fr"){
      r$help_settings_app_database_modal_text <- div(
        p("Voici les différentes tables des deux bases de données de l'application."),
        p("Choisissez le ", tags$em("Type de connexion"), " (locale ou distante), et la ", tags$em("Base de données"), " (principale ou publique)."),
        tags$h3(tags$i(class = "fa fa-database", style = "color: steelblue;"), " ", strong("Base de données principale")),
        p("Voici les tables composant la BDD principale, classées par groupes."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Utilisateurs et droits d'accès :")),
        tags$ul(
          tags$li(tags$em("users"), " : liste les utilisateurs"),
          tags$li(tags$em("users_accesses"), " : liste les différents accès utilisateurs"),
          tags$li(tags$em("users_statuses"), " : liste les différents statuts des utilisateurs (data scientist, clinicien etc)")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Gestion des données")),
        tags$ul(
          tags$li(tags$em("data_sources"), " : comprend les différentes sources de données"),
          tags$li(tags$em("datasets"), " : les set de données associés aux sources de données"),
          tags$li(tags$em("studies"), " : les différentes études associées aux sets de données")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Onglets, plugins et scripts")),
        tags$ul(
          tags$li(tags$em("patient_lvl_tabs_groups"), " : les onglets sont regroupés au sein de groupes d'onglets"),
          tags$li(tags$em("patient_lvl_tabs"), " : comprend les onglets de type ", tags$em("Données individuelles")),
          tags$li(tags$em("patient_lvl_widgets"), " : comprend les widgets"),
          tags$li(tags$em("aggregated_tabs_groups"), " : ces tables sont l'équivalent de celles de ", tags$em("patient_lvl"), ", pour les données agrégées"),
          tags$li(tags$em("aggregated_tabs")),
          tags$li(tags$em("aggregated_widgets")),
          tags$li(tags$em("plugins"), " : liste les différents plugins"),
          tags$li(tags$em("scripts"), " : comprend les différents scripts"),
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Autres")),
        tags$ul(
          tags$li(tags$em("code"), " : les codes de différentes tables (datasets, thésaurus etc) sont stockés ici"),
          tags$li(tags$em("options"), " : les options de différentes tables (data_sources, datasets etc) sont stockées ici"),
          tags$li(tags$em("conversations"), " : les conversations que l'on créé au sein d'études sont stockées ici"),
          tags$li(tags$em("user_deleted_conversations"), " : liste les conversations supprimées pour chaque utilisateur"),
          tags$li(tags$em("messages"), " : les messages associés aux conversations"),
          tags$li(tags$em("inbox_messages"), " : les messages non lus sont renseignés ici"),
          tags$li(tags$em("log"), " : le log des utilisateurs est stocké ici"),
          tags$li(tags$em("git_repos"), " : cette table permet de stocker les liens vers les git distants")
        ),
        tags$h3(tags$i(class = "fa fa-database", style = "color: steelblue;"), " ", strong("Base de données publique")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Subsets")),
        tags$ul(
          tags$li(tags$em("subsets"), " : les différents subsets associés aux études"),
          tags$li(tags$em("subset_persons"), " : les patients de tel ou tel subset"),
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Données d'études")),
        tags$ul(
          tags$li(tags$em("patient_lvl_widgets_options"), " : cette table comporte les options associés aux widgets, ce qui permet",
            " aux plugins utilisés au sein de widgets d'être configurés par les utilisateurs"),
          tags$li(tags$em("aggregated_widgets_options"), " : l'équivalent de la table précédente pour les données agrégées"),
          tags$li(tags$em("patient_lvl_widgets_concepts"), " : "),
          tags$li(tags$em("aggregated_widgets_concepts"), " : "),
          tags$li(tags$em("persons_options"), " : cette table peut être utilisée pour stocker des informations sur les patients, dans une étude")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Terminologies")),
        tags$ul(
          tags$li(tags$em("concept"), " : table rassemblant les concepts des différentes terminologies"),
          tags$li(tags$em("concept_dataset"), " : cette table stocke les calculs de lignes concernées par concept et par dataset"),
          tags$li(tags$em("concept_user"), " : les modifications apportées sur les concepts par les utilisateurs sont stockées ici"),
          tags$li(tags$em("vocabulary"), " : rassemble les différentes terminologies"),
          tags$li(tags$em("domain"), " : contient les différents domaines OMOP"),
          tags$li(tags$em("concept_class"), " : contient les différentes classes de concept"),
          tags$li(tags$em("concept_relationship"), " : table rassemblant les alignements de concepts"),
          tags$li(tags$em("concept_relationship_user"), " : comprend les alignements de concepts ajoutés par les utilisateurs"),
          tags$li(tags$em("concept_relationship_evals"), " : contient les évaluations des alignements de concepts ajoutés par les utilisateurs"),
          tags$li(tags$em("relationship"), " : rassemble les différentes relations existant pour l'alignement de concepts"),
          tags$li(tags$em("concept_synonym"), " : les synonymes de concepts y sont stockés"),
          tags$li(tags$em("concept_ancestor"), " : contient les relations hiérarchiques entre les concepts"),
          tags$li(tags$em("drug_strength"), " : table rassemblant les informations sur les médicaments")
        ),
        p("Pour plus d'informations sur les tables des terminologies, rendez-vous sur la ", 
          tags$a(href = "https://ohdsi.github.io/CommonDataModel/cdm60.html#CONCEPT", "documentation OMOP", target = "_blank"), "."),
        br()
      )
    }
    
    if (language == "en"){
      r$help_settings_app_database_modal_text <- div(
        p("Here are the different tables of the two databases of the application."),
        p("Choose the ", tags$em("Connection type"), " (local or remote), and the ", tags$em("Database"), " (main or public)."),
        tags$h3(tags$i(class = "fa fa-database", style = "color: steelblue;"), " ", strong("Main Database")),
        p("Here are the tables that make up the main database, classified by groups."),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Users and access rights :")),
        tags$ul(
          tags$li(tags$em("users"), " : list of users"),
          tags$li(tags$em("users_accesses"), " : list of different user accesses"),
          tags$li(tags$em("users_statuses"), " : list of different user statuses (data scientist, clinician etc)")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Data management")),
        tags$ul(
          tags$li(tags$em("data_sources"), " : includes different data sources"),
          tags$li(tags$em("datasets"), " : the datasets associated with data sources"),
          tags$li(tags$em("studies"), " : different studies associated with datasets")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Tabs, plugins and scripts")),
        tags$ul(
          tags$li(tags$em("patient_lvl_tabs_groups"), " : tabs are grouped within tab groups"),
          tags$li(tags$em("patient_lvl_tabs"), " : includes tabs of type ", tags$em("Individual Data")),
          tags$li(tags$em("patient_lvl_widgets"), " : includes widgets"),
          tags$li(tags$em("aggregated_tabs_groups"), " : these tables are equivalent to those of ", tags$em("patient_lvl"), ", for aggregated data"),
          tags$li(tags$em("aggregated_tabs")),
          tags$li(tags$em("aggregated_widgets")),
          tags$li(tags$em("plugins"), " : includes different plugins"),
          tags$li(tags$em("scripts"), " : includes different scripts"),
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Others")),
        tags$ul(
          tags$li(tags$em("code"), " : codes of different tables (datasets, scripts etc) are stored here"),
          tags$li(tags$em("options"), " : options of different tables (data_sources, datasets etc) are stored here"),
          tags$li(tags$em("conversations"), " : conversations created within studies are stored here"),
          tags$li(tags$em("user_deleted_conversations"), " : list of deleted conversations for each user"),
          tags$li(tags$em("messages"), " : messages associated with conversations"),
          tags$li(tags$em("inbox_messages"), " : unread messages are indicated here"),
          tags$li(tags$em("log"), " : user logs are stored here"),
          tags$li(tags$em("git_repos"), " : this table is used to store links to remote git repositories")
        ),
        tags$h3(tags$i(class = "fa fa-database", style = "color: steelblue;"), " ", strong("Public Database")),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Subsets")),
        tags$ul(
          tags$li(tags$em("subsets"), " : different subsets associated with studies"),
          tags$li(tags$em("subset_persons"), " : patients of this or that subset"),
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Study data")),
        tags$ul(
          tags$li(tags$em("patient_lvl_widgets_options"), " : this table has options associated with widgets, which allows",
            " plugins used within widgets to be configured by users"),
          tags$li(tags$em("aggregated_widgets_options"), " : the equivalent of the previous table for aggregated data"),
          tags$li(tags$em("patient_lvl_widgets_concepts"), " : "),
          tags$li(tags$em("aggregated_widgets_concepts"), " : "),
          tags$li(tags$em("persons_options"), " : this table can be used to store information on patients, in a study")
        ),
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Terminologies")),
        tags$ul(
          tags$li(tags$em("concept"), " : table gathering the concepts of the different vocabularies"),
          tags$li(tags$em("concept_dataset"), " : this table stores calculations of lines concerned by concept and by dataset"),
          tags$li(tags$em("concept_user"), " : modifications made on concepts by users are stored here"),
          tags$li(tags$em("vocabulary"), " : gathers the different vocabularies"),
          tags$li(tags$em("domain"), " : contains the different OMOP domains"),
          tags$li(tags$em("concept_class"), " : contains the different concept classes"),
          tags$li(tags$em("concept_relationship"), " : table gathering the mappings of concepts"),
          tags$li(tags$em("concept_relationship_user"), " : includes concept mappings added by users"),
          tags$li(tags$em("concept_relationship_evals"), " : contains evaluations of concept mappings added by users"),
          tags$li(tags$em("relationship"), " : gathers the different relationships existing for concept mapping"),
          tags$li(tags$em("concept_synonym"), " : concept synonyms are stored here"),
          tags$li(tags$em("concept_ancestor"), " : contains the hierarchical relations between the concepts"),
          tags$li(tags$em("drug_strength"), " : table gathering information about drugs")
        ),
        p("For more information on vocabulary tables, visit the ", 
          tags$a(href = "https://ohdsi.github.io/CommonDataModel/cdm60.html#CONCEPT", "OMOP documentation", target = "_blank"), "."),
        br()
      )
    }
  })
  
  # Request database
  
  observeEvent(r$help_settings_app_database_page_4, {
    
    load_help_page(r)
    
    r$help_settings_app_database_modal_title <- i18n$t("db_request_card")
    
    if (language == "fr"){
      r$help_settings_app_database_modal_text <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", strong("Requêtez ici la base de données"), 
          ", en choisissant le ", tags$em("Type de connexion"), " (locale ou distante), et la ", 
          tags$em("Base de données"), " (principale ou publique)."),
        p("Exemple de requête :"),
        div_code_4,
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ",
          "Référez-vous au ", strong("nom des tables"), " dans la catégorie ", tags$em("Tables de la BDD"), "."),
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
      r$help_settings_app_database_modal_text <- div(
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          strong("Query the database here"), ", by choosing the ", tags$em("Type of connection"), " (local or remote), and the ",
          tags$em("Database"), " (main or public)."),
        p("Example of a query:"),
        div_code_4,
        p(tags$i(class = "fa fa-check", style = "color: steelblue;"), " ", 
          "Refer to the ", strong("table names"), " in the category ", tags$em("Database Tables"), "."),
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
  
  # Export and restore database
  
  observeEvent(r$help_settings_app_database_page_5, {
    
    load_help_page(r)
    
    r$help_settings_app_database_modal_title <- i18n$t("db_export_and_restore")
    
    if (language == "fr"){
      r$help_settings_app_database_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-upload", style = "color: steelblue;"), " ", strong("Exporter la base de données")),
        p("Vous pouvez faire des sauvegardes de la base de données depuis l'onglet ", tags$em("Exporter la BDD")),
        p("Vous pouvez choisir quelles tables exporter via les menus déroulants."),
        p("Cliquez ensuite sur ", tags$em("Exporter la base de données"), "."),
        p("La base de données sera alors ", strong("téléchargée au format ZIP"), "."),
        p("Le fichier comprend ", strong("toutes les tables au format CSV"), ", ainsi qu'un fichier ", tags$em("db_info.xml"), 
          " qui renseigne notamment la version de l'application."),
        tags$h3(tags$i(class = "fa fa-download", style = "color: steelblue;"), " ", strong("Restaurer la base de données")),
        p("Pour restaurer la base de données, il vous suffit se sélectionner un fichier de sauvegarde en cliquant sur ", tags$em("Choisir un fichier ZIP"), "."),
        p("De la même façon que pour l'export, choisissez quelles tables importer via les menus déroulants."),
        p("Cliquez ensuite sur ", tags$em("Restaurer la base de données"), "."),
        p("Il vous faudra ", strong("redémarrer l'application"), " pour prendre en compte la restauration.")
      )
    }
    
    if (language == "en"){
      r$help_settings_app_database_modal_text <- div(
        tags$h3(tags$i(class = "fa fa-upload", style = "color: steelblue;"), " ", strong("Export the database")),
        p("You can back up the database from the ", tags$em("Export the DB"), " tab."),
        p("You can choose which tables to export via the dropdown menus."),
        p("Then click on ", tags$em("Export the database"), "."),
        p("The database will then be ", strong("downloaded as a ZIP file"), "."),
        p("The file includes ", strong("all tables in CSV format"), ", as well as an ", tags$em("db_info.xml"), 
          " file which provides information such as the version of the application."),
        tags$h3(tags$i(class = "fa fa-download", style = "color: steelblue;"), " ", strong("Restore the database")),
        p("To restore the database, simply select a backup file by clicking on ", tags$em("Choose a ZIP file"), "."),
        p("In the same way as for the export, choose which tables to import via the dropdown menus."),
        p("Then click on ", tags$em("Restore the database"), "."),
        p("You will need to ", strong("restart the application"), " to take the restoration into account.")
      )
      
    }
  })
  
  # Copy code divs
  
  observeEvent(r$help_settings_app_database_copy_code_1, clipr::write_clip(code_1))
  observeEvent(r$help_settings_app_database_copy_code_2, clipr::write_clip(code_2))
  observeEvent(r$help_settings_app_database_copy_code_3, clipr::write_clip(code_3))
  observeEvent(r$help_settings_app_database_copy_code_4, clipr::write_clip(code_4))
}
