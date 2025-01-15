load_files_browser_ui <- function(id, files_list, prefix){
  
  ns <- NS(id)
  
  files_ui <- tagList()
  
  if (nrow(files_list) > 0){
    for (i in 1:nrow(files_list)){
      file <- files_list[i, ]
      
      if (id == "plugins" & file$filename %in% c("ui.R", "server.R", "translations.csv")) icons_div <- tagList()
      else icons_div <- div(
        class = "file-item-icons",
        shinyjs::hidden(
          div(
            id = ns(paste0(prefix, "save_filename_button_div_", file$id)),
            shiny.fluent::IconButton.shinyInput(paste0(prefix, "save_filename_button_", file$id), iconProps = list(iconName = "CheckMark")), 
            class = "small_icon_button",
            onclick = paste0(
              "Shiny.setInputValue('", id, "-", prefix, "save_filename', ", file$id, ", {priority: 'event'});",
              "Shiny.setInputValue('", id, "-", prefix, "save_filename_trigger', Math.random(), {priority: 'event'});"
            )
          )
        ),
        shinyjs::hidden(
          div(
            id = ns(paste0(prefix, "cancel_rename_button_div_", file$id)),
            shiny.fluent::IconButton.shinyInput(paste0(prefix, "cancel_rename_button_", file$id), iconProps = list(iconName = "Clear")), 
            class = "small_icon_button",
            onclick = paste0(
              "Shiny.setInputValue('", id, "-", prefix, "cancel_rename', ", file$id, ", {priority: 'event'});",
              "Shiny.setInputValue('", id, "-", prefix, "cancel_rename_trigger', Math.random(), {priority: 'event'});"
            )
          )
        ),
        div(
          id = ns(paste0(prefix, "edit_filename_button_div_", file$id)),
          shiny.fluent::IconButton.shinyInput(paste0(prefix, "edit_filename_button_", file$id), iconProps = list(iconName = "Edit")), 
          class = "small_icon_button",
          onclick = paste0(
            "Shiny.setInputValue('", id, "-", prefix, "edit_filename', ", file$id, ", {priority: 'event'});",
            "Shiny.setInputValue('", id, "-", prefix, "edit_filename_trigger', Math.random(), {priority: 'event'});"
          )
        ),
        div(
          id = ns(paste0(prefix, "delete_file_button_div_", file$id)),
          shiny.fluent::IconButton.shinyInput(paste0(prefix, "delete_file_button_", file$id), iconProps = list(iconName = "Delete")), 
          class = "small_icon_button",
          onclick = paste0(
            "Shiny.setInputValue('", id, "-", prefix, "delete_file', ", file$id, ", {priority: 'event'});",
            "Shiny.setInputValue('", id, "-", prefix, "delete_file_trigger', Math.random(), {priority: 'event'});"
          )
        ),
        onclick = "event.stopPropagation();"
      )
      
      files_ui <- tagList(
        files_ui,
        tags$li(
          id = ns(paste0(prefix, "file_div_", file$id)),
          class = "file-item",
          div(
            class = "file-item-title",
            tags$i(class = "fa fa-file"),
            shinyjs::hidden(
              div(
                id = ns(paste0(prefix, "edit_filename_textfield_div_", file$id)), 
                shiny.fluent::TextField.shinyInput(
                  ns(paste0(prefix, "edit_filename_textfield_", file$id)),
                  value = file$filename,
                  onKeyPress = htmlwidgets::JS(sprintf(
                    "function(event) {
                      if (event.key === 'Enter') {
                        event.preventDefault();
                        Shiny.setInputValue('%s-%ssave_filename', %s, {priority: 'event'});
                        Shiny.setInputValue('%s-%ssave_filename_trigger', Math.random(), {priority: 'event'});
                      }
                    }", 
                    id, prefix, file$id, id, prefix
                  ))
                ), 
                class = "small_textfield",
                onclick = "event.stopPropagation();"
              )
            ),
            create_hover_card(
              ui = div(
                style = "padding-left: 5px;",
                id = ns(paste0(prefix, "filename_div_", file$id)),
                file$short_filename
              ),
              text = file$filename
            )
          ),
          icons_div,
          onClick = htmlwidgets::JS(paste0("Shiny.setInputValue('", id, "-", prefix, "selected_file', ", file$id, ", {priority: 'event'});"))
        )
      )
    }
  }
  
  div(
    class = "files-browser",
    files_ui
  )
}