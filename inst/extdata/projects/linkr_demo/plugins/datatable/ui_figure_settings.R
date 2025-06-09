# UI - Figure settings
#
# Insert the UI components for configuring the figure settings in this section.

div(
    div(
        shiny.fluent::Dropdown.shinyInput(
            ns("data_source_%widget_id%"), options = list(
                list(key = "person", text = i18np$t("patient_data")),
                list(key = "visit_detail", text = i18np$t("stay_data"))
            ), value = "person", label = i18np$t("data_to_display")
        ),
        style = "width: 200px;"
    ),
    div(
        div(
            shiny.fluent::Dropdown.shinyInput(
                ns("concepts_choice_%widget_id%"), options = list(
                    list(key = "all_concepts", text = i18np$t("all_concepts")),
                    list(key = "selected_concept_classes", text = i18np$t("selected_concept_classes")),
                    list(key = "selected_concepts", text = i18np$t("selected_concepts"))
                ), value = "all_concepts", label = i18np$t("concepts_to_display")
            ),
            style = "width: 200px;"
        ),
        shinyjs::hidden(
            div(
                id = ns("concept_classes_div_%widget_id%"),
                div(
                    shiny.fluent::Dropdown.shinyInput(
                        ns("concept_classes_%widget_id%"), label = i18np$t("concept_classes"),
                        options = convert_tibble_to_list(concept_class_ids, key_col = "concept_class_id", text_col = "concept_class_id"),
                        multiSelect = TRUE
                    ),
                    style = "width: 200px;"
                )
            )
        ),
        shinyjs::hidden(
            div(
                id = ns("concepts_div_%widget_id%"),
                div(
                    shiny.fluent::Dropdown.shinyInput(
                        ns("concepts_%widget_id%"), label = i18np$t("concepts"),
                        options = convert_tibble_to_list(
                            selected_concepts %>% dplyr::filter(domain_id == "Measurement"),
                            key_col = "concept_id", text_col = "concept_name"
                        ),
                        multiSelect = TRUE
                    ),
                    style = "width: 200px;"
                )
            )
        ),
        style = "display: flex; gap: 10px; padding-bottom: 15px; border-bottom: solid 1px #808080;"
    ),
    div(
        div(i18np$t("num_cols"), style = "font-weight: 600; margin-bottom: 5px;"),
        div(shiny.fluent::SpinButton.shinyInput(ns("num_cols_%widget_id%"), min = 1, max = 20, by = 1, value = 8), style = "width: 200px;"),
        style = "margin-top: 10px;"
    ),
    div(
        shiny.fluent::Dropdown.shinyInput(ns("aggregate_fct_%widget_id%"), label = "Aggregated function", value = "mean", options = list(
            list(key = "min", text = i18np$t("min")),
            list(key = "mean", text = i18np$t("mean")),
            list(key = "median", text = i18np$t("median")),
            list(key = "max", text = i18np$t("max"))
        )),
        style = "width: 200px; padding-bottom: 15px; border-bottom: solid 1px #808080;"
    ),
    div(
        shiny.fluent::Toggle.shinyInput(ns("synchronize_timelines_%widget_id%"), label = i18np$t("synchronize_timelines")),
        style = "margin-top: 15px;"
    )
)
