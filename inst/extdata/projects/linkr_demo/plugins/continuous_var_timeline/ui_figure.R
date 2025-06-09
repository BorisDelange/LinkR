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
    div(
        id = ns("dygraph_div_%widget_id%"),
        dygraphs::dygraphOutput(ns("dygraph_%widget_id%"), height = "100%", width = "100%"),
        style = "width: 100%; height: calc(100% - 10px); padding-top: 10px; box-sizing: border-box;"
    ),
    style = "width: 100%; padding: 5px; box-sizing: border-box; height: 100%; overflow-y: auto;"
)
