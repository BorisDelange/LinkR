# UI - Figure page
#
# Insert the UI components for the figure in this section.

div(
    shinyjs::hidden(
        div(
            id = ns("error_message_div_%widget_id%"),
            uiOutput(ns("error_message_%widget_id%"))
        )
    ),
    # Div containing the figure output
    plotly::plotlyOutput(ns("stays_plot_%widget_id%"), width = "100%", height = "100%"),
    style = "width: 100%; padding: 5px; box-sizing: border-box; height: 100%; overflow-y: auto;"
)
