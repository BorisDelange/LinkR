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
        DT::DTOutput(ns("datatable_%widget_id%")),
        style = "padding: 5px 5px 0 5px; position: relative; z-index: 100;"
    ),
    shinyjs::hidden(
        div(
            id = ns("datetime_slider_div_%widget_id%"),
            sliderInput(
                ns("datetime_slider_%widget_id%"),
                label = NULL,
                ticks = FALSE,
                min = as.Date("1970-01-01"),
                max = Sys.Date(),
                value = c(as.Date("1970-01-01"), Sys.Date()),
                timezone = "+0000",
                width = "100%"
            ),
            style = "width: calc(100% - 130px); padding-left: 130px; overflow: hidden; position: relative; top: -10px;"
        )
    ),
    style = "width: 100%; padding: 5px; box-sizing: border-box; height: 100%; overflow-y: auto;"
)
