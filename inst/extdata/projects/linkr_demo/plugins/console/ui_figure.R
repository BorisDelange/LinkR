div(
    shinyjs::hidden(verbatimTextOutput(ns("console_output_%widget_id%"))),
    shinyjs::hidden(uiOutput(ns("ui_output_%widget_id%"), style = "height: 100%;")),
    shinyjs::hidden(plotOutput(ns("figure_output_%widget_id%"), height = "calc(100% - 5px)")),
    shinyjs::hidden(tableOutput(ns("table_output_%widget_id%"))),
    shinyjs::hidden(DT::DTOutput(ns("datatable_output_%widget_id%"))),
    shinyjs::hidden(dygraphs::dygraphOutput(ns("dygraphs_output_%widget_id%"))),
    shinyjs::hidden(plotly::plotlyOutput(ns("plotly_output_%widget_id%"), width = "100%", height = "100%")),
    shinyjs::hidden(uiOutput(ns("rmarkdown_output_%widget_id%"))),
    style = "width: 100%; padding: 5px; box-sizing: border-box; height: 100%; overflow-y: auto;"
)
