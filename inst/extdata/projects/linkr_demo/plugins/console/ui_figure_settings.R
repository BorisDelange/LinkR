div(
    div(
        shiny.fluent::Dropdown.shinyInput(
            ns("prog_language_%widget_id%"), label = i18np$t("language"),
            options = list(
                list(key = "r", text = i18np$t("r")),
                list(key = "python", text = i18np$t("python"))
            ),
            value = "r"
        ), 
        style = "width: 150px;"
    ),
    div(
        shiny.fluent::Dropdown.shinyInput(
            ns("output_%widget_id%"), label = i18np$t("output"),
            options = list(
                list(key = "console", text = i18np$t("console")),
                list(key = "ui", text = i18np$t("ui_html")),
                list(key = "figure", text = i18np$t("figure")),
                list(key = "table", text = i18np$t("table")),
                list(key = "datatable", text = i18np$t("datatable")),
                list(key = "dygraphs", text = i18np$t("dygraphs")),
                list(key = "plotly", text = i18np$t("plotly")),
                list(key = "rmarkdown", text = i18np$t("rmarkdown"))
            ),
            value = "figure"
        ), 
        style = "width: 150px;"
    ),
    style = "display: flex; gap: 10px;"
)
