# UI - Code page

shinyAce::aceEditor(
    ns("code_%widget_id%"), value = "", mode = "r",
    hotkeys = list(
        save = list(win = "CTRL-S", mac = "CTRL-S|CMD-S"),
        run_all = list(win = "CTRL-SHIFT-ENTER", mac = "CTRL-SHIFT-ENTER|CMD-SHIFT-ENTER"),
        comment = list(win = "CTRL-SHIFT-C", mac = "CTRL-SHIFT-C|CMD-SHIFT-C")
    ),
    autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, fontSize = 11, showPrintMargin = FALSE
)
