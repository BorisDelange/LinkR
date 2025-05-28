load_files_browser_ui <- function(input_prefix, files_list){
  
  # Get variables from other environments
  id <- get("id", envir = parent.frame())
  ns <- NS(id)
  
  files_ui <- tagList()
  
  if (nrow(files_list) > 0){
    for (i in 1:nrow(files_list)){
      file <- files_list[i, ]
      
      if (tolower(file$filename) %not_in% c("readme.md", "plugin.xml")){
      
        file <- file %>% dplyr::mutate(short_filename = ifelse(nchar(filename) >= 23, paste0(substr(filename, 1, 20), "..."), filename))
        
        if (id == "plugins" & file$filename %in% c("ui.R", "server.R", "translations.csv")) icons_div <- tagList()
        else icons_div <- div(
          class = "file-item-icons",
          shinyjs::hidden(
            div(
              id = ns(paste0(input_prefix, "save_filename_button_div_", file$id)),
              shiny.fluent::IconButton.shinyInput(paste0(input_prefix, "save_filename_button_", file$id), iconProps = list(iconName = "CheckMark")), 
              class = "small_icon_button",
              onclick = paste0(
                "Shiny.setInputValue('", id, "-", input_prefix, "save_filename', ", file$id, ", {priority: 'event'});",
                "Shiny.setInputValue('", id, "-", input_prefix, "save_filename_trigger', Math.random(), {priority: 'event'});"
              )
            )
          ),
          shinyjs::hidden(
            div(
              id = ns(paste0(input_prefix, "cancel_rename_button_div_", file$id)),
              shiny.fluent::IconButton.shinyInput(paste0(input_prefix, "cancel_rename_button_", file$id), iconProps = list(iconName = "Clear")), 
              class = "small_icon_button",
              onclick = paste0(
                "Shiny.setInputValue('", id, "-", input_prefix, "cancel_rename', ", file$id, ", {priority: 'event'});",
                "Shiny.setInputValue('", id, "-", input_prefix, "cancel_rename_trigger', Math.random(), {priority: 'event'});"
              )
            )
          ),
          div(
            id = ns(paste0(input_prefix, "edit_filename_button_div_", file$id)),
            shiny.fluent::IconButton.shinyInput(paste0(input_prefix, "edit_filename_button_", file$id), iconProps = list(iconName = "Edit")), 
            class = "small_icon_button",
            onclick = paste0(
              "Shiny.setInputValue('", id, "-", input_prefix, "edit_filename', ", file$id, ", {priority: 'event'});",
              "Shiny.setInputValue('", id, "-", input_prefix, "edit_filename_trigger', Math.random(), {priority: 'event'});"
            )
          ),
          div(
            id = ns(paste0(input_prefix, "delete_file_button_div_", file$id)),
            shiny.fluent::IconButton.shinyInput(paste0(input_prefix, "delete_file_button_", file$id), iconProps = list(iconName = "Delete")), 
            class = "small_icon_button",
            onclick = paste0(
              "Shiny.setInputValue('", id, "-", input_prefix, "delete_file', ", file$id, ", {priority: 'event'});",
              "Shiny.setInputValue('", id, "-", input_prefix, "delete_file_trigger', Math.random(), {priority: 'event'});"
            )
          ),
          onclick = "event.stopPropagation();"
        )
        
        files_ui <- tagList(
          files_ui,
          tags$li(
            id = ns(paste0(input_prefix, "file_div_", file$id)),
            class = "file-item",
            div(
              class = "file-item-title",
              tags$i(class = "fa fa-file"),
              shinyjs::hidden(
                div(
                  id = ns(paste0(input_prefix, "edit_filename_textfield_div_", file$id)), 
                  shiny.fluent::TextField.shinyInput(
                    ns(paste0(input_prefix, "edit_filename_textfield_", file$id)),
                    value = file$filename,
                    onKeyPress = htmlwidgets::JS(sprintf(
                      "function(event) {
                        if (event.key === 'Enter') {
                          event.preventDefault();
                          Shiny.setInputValue('%s-%ssave_filename', %s, {priority: 'event'});
                          Shiny.setInputValue('%s-%ssave_filename_trigger', Math.random(), {priority: 'event'});
                        }
                      }", 
                      id, input_prefix, file$id, id, input_prefix
                    ))
                  ), 
                  class = "small_textfield",
                  onclick = "event.stopPropagation();"
                )
              ),
              create_hover_card(
                ui = div(
                  style = "padding-left: 5px;",
                  id = ns(paste0(input_prefix, "filename_div_", file$id)),
                  file$short_filename
                ),
                text = file$filename
              )
            ),
            icons_div,
            onClick = htmlwidgets::JS(paste0("Shiny.setInputValue('", id, "-", input_prefix, "selected_file', ", file$id, ", {priority: 'event'});"))
          )
        )
      }
    }
  }
  
  div(
    class = "files-browser",
    files_ui
  )
}

files_browser_create_file <- function(input_prefix, r_prefix, folder, element_id, code_hotkeys, user_settings){
  
  # Get variables from other environments
  for (obj_name in c("id", "r")) assign(obj_name, get(obj_name, envir = parent.frame()))
  i18n <- r$i18n
  ns <- NS(id)
  
  # List files to create new filename
  files_list <- list.files(path = folder, pattern = "(?i)*.\\.(r|py|csv|css|js)$")
  base_name <- "untitled"
  extension <- ".R"
  i <- 1
  new_filename <- paste0(base_name, extension)
  while(new_filename %in% files_list) {
    i <- i + 1
    new_filename <- paste0(base_name, i, extension)
  }
  
  # Create file
  writeLines("", paste0(folder, "/", new_filename))
  
  # Update files browser
  if (nrow(r[[paste0(r_prefix, "_files_list")]]) > 0) max_id <- max(r[[paste0(r_prefix, "_files_list")]]$id)
  else max_id <- 0
  
  new_row_id <- max_id + 1
  
  # Make sure that it is not the ID of a deleted file
  while (new_row_id %in% r[[paste0(r_prefix, "_deleted_files_ids")]]) new_row_id <- new_row_id + 1
  
  if (id == "plugins") new_data <- tibble::tibble(id = new_row_id, plugin_id = element_id, filename = new_filename)
  else if (id == "project_files") new_data <- tibble::tibble(id = new_row_id, project_id = element_id, filename = new_filename)
    
  r[[paste0(r_prefix, "_files_list")]] <- r[[paste0(r_prefix, "_files_list")]] %>% dplyr::bind_rows(new_data)
  
  shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-", input_prefix, "reload_files_browser', '", now(format = "%Y-%m-%d %H:%M:%OS3"), "');"))

  # Hide all editors
  sapply(paste0(input_prefix, "editor_div_", r[[paste0(r_prefix, "_editors")]]$id), shinyjs::hide)

  # Create editor if doesn't exist (when we delete and recreate a plugin, some editors could persist...)
  if (new_row_id %not_in% r[[paste0(r_prefix, "_editors")]]$id){
    insertUI(selector = paste0("#", ns(paste0(input_prefix, "editors_div"))), where = "beforeEnd", ui = div(
      id = ns(paste0(input_prefix, "editor_div_", new_row_id)),
      shinyAce::aceEditor(
        ns(paste0(input_prefix, "editor_", new_row_id)), value = "", mode = "r",
        hotkeys = code_hotkeys,
        theme = user_settings$ace_theme, fontSize = user_settings$ace_font_size,
        autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, showPrintMargin = FALSE
      ),
      style = "width: 100%; height: 100%; display: flex; flex-direction: column;"
    ))
  }

  # Add observers for editor hotkeys
  shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-add_code_editor_hotkeys', ", new_row_id, ");"))

  r[[paste0(r_prefix, "_editors")]] <- r[[paste0(r_prefix, "_editors")]] %>% dplyr::bind_rows(r[[paste0(r_prefix, "_files_list")]] %>% dplyr::filter(id == new_row_id))

  shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-", input_prefix, "selected_file', ", new_row_id, ");"))
}

files_browser_delete_file <- function(input_prefix, r_prefix, folder, element_id, file_id){
  
  # Get variables from other environments
  for (obj_name in c("id", "r", "output")) assign(obj_name, get(obj_name, envir = parent.frame()))
  i18n <- r$i18n
  ns <- NS(id)
  
  filename <- r[[paste0(r_prefix, "_files_list")]] %>% dplyr::filter(id == file_id) %>% dplyr::pull(filename)
  
  # Delete file
  file.remove(paste0(folder, "/", filename))
  
  # Reload files browser
  r[[paste0(r_prefix, "_files_list")]] <- r[[paste0(r_prefix, "_files_list")]] %>% dplyr::filter(id != file_id)
  shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-", input_prefix, "reload_files_browser', '", now(format = "%Y-%m-%d %H:%M:%OS3"), "');"))
  
  # Reload tabs if file is opened
  reload_tabs <- FALSE
  if (file_id %in% r[[paste0(r_prefix, "_tabs")]]$id) reload_tabs <- TRUE
  r[[paste0(r_prefix, "_tabs")]] <- r[[paste0(r_prefix, "_tabs")]] %>% dplyr::filter(id != file_id)
  if (reload_tabs) shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-", input_prefix, "reload_files_tab', '", now(format = "%Y-%m-%d %H:%M:%OS3"), "');"))
  
  # Hide all editors
  sapply(paste0(input_prefix, "editor_div_", r[[paste0(r_prefix, "_editors")]]$id), shinyjs::hide)
  
  # Show editor of last file
  if (id == "plugins") first_file_id <- r[[paste0(r_prefix, "_tabs")]] %>% dplyr::filter(plugin_id == element_id)
  else if (id == "project_files") first_file_id <- r[[paste0(r_prefix, "_tabs")]] %>% dplyr::filter(project_id == element_id)
  
  first_file_id <- first_file_id %>% dplyr::slice(1) %>% dplyr::pull(id)
  
  shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-", input_prefix, "selected_file', ", first_file_id, ");"))
  if (length(first_file_id) > 0) shinyjs::delay(50, shinyjs::show(paste0(input_prefix, "editor_div_", first_file_id)))
  
  # Keep track of deleted IDs to avoid loading a deleted Ace Editor after creating a new file
  r[[paste0(r_prefix, "_deleted_files_ids")]] <- c(r[[paste0(r_prefix, "_deleted_files_ids")]], file_id)
  
  shinyjs::hide("delete_file_modal")
  show_message_bar("file_deleted", "warning")
}

reload_files_browser_tabs <- function(input_prefix, r_prefix, element_id, file_id){
  
  # Get variables from other environments
  for (obj_name in c("id", "r")) assign(obj_name, get(obj_name, envir = parent.frame()))
  ns <- NS(id)
  
  tabs_ui <- tagList()
  
  if (id == "plugins") code_tabs <- r[[paste0(r_prefix, "_tabs")]] %>% dplyr::filter(plugin_id == element_id) %>% dplyr::arrange(position)
  else if (id == "project_files") code_tabs <- r[[paste0(r_prefix, "_tabs")]] %>% dplyr::filter(project_id == element_id) %>% dplyr::arrange(position)
  
  tabs_container <- div(
    id = ns(paste0(input_prefix, "tabs")),
    class = "tabs",
    `data-id-prefix` = paste0(ns(paste0(input_prefix, "tab_"))),
    `data-input-id` = ns(paste0(input_prefix, "tab_positions")),
    `data-draggable-class` = "tab"
  )
  
  if (nrow(code_tabs) > 0) {
    
    first_file_id <- code_tabs %>% dplyr::slice(1) %>% dplyr::pull(id)
    selected_tab_id <- first_file_id
    if (length(file_id) > 0) if (file_id %in% code_tabs$id) selected_tab_id <- file_id
    
    tabs_list <- lapply(1:nrow(code_tabs), function(i) {
      file <- code_tabs[i, ]
      
      tab_class <- "tab"
      if (file$id == selected_tab_id) tab_class <- "tab active"
      
      div(
        id = ns(paste0(input_prefix, "tab_", file$id)),
        class = tab_class,
        onclick = paste0("Shiny.setInputValue('", id, "-", input_prefix, "selected_tab', ", file$id, ", {priority: 'event'})"),
        div(
          file$filename, 
          style = "overflow: hidden; white-space: nowrap; text-overflow: ellipsis;"
        ),
        div(
          id = ns(paste0(input_prefix, "close_tab_", file$id)),
          class = "close-tab",
          onclick = paste0(
            "Shiny.setInputValue('", id, "-", input_prefix, "close_selected_tab', ", file$id, ", {priority: 'event'});",
            "Shiny.setInputValue('", id, "-", input_prefix, "close_selected_tab_trigger', Math.random(), {priority: 'event'});",
            "event.stopPropagation();"
          ),
          tags$i(class = "fa fa-times")
        ),
        title = file$filename
      )
    })
    
    tabs_container$children <- tabs_list
  }
  
  tabs_container
}

files_browser_edit_tab_positions <- function(positions, r_prefix, element_id){
  
  # Get variables from other environments
  for (obj_name in c("id", "r")) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  ids <- positions[seq(1, length(positions), 2)]
  pos <- positions[seq(2, length(positions), 2)]
  
  pos_df <- data.frame(
    id = ids,
    position = pos
  )
  
  if (id == "plugins") id_col <- "plugin_id"
  else if (id == "project_files") id_col <- "project_id"
  
  for (i in 1:nrow(pos_df)) {
    r[[paste0(r_prefix, "_tabs")]] <-
      r[[paste0(r_prefix, "_tabs")]] %>%
      dplyr::mutate(
        position = ifelse(
          id == pos_df$id[i] & !!rlang::sym(id_col) == element_id,
          pos_df$position[i],
          position
        )
      )
  }
}

files_browser_open_file <- function(input_prefix, r_prefix, folder, element_id, file_id, code_hotkeys, user_settings){
  
  # Get variables from other environments
  for (obj_name in c("id", "r")) assign(obj_name, get(obj_name, envir = parent.frame()))
  ns <- NS(id)
  
  if (id == "plugins") id_col <- "plugin_id"
  else if (id == "project_files") id_col <- "project_id"
  
  if (nrow(r[[paste0(r_prefix, "_tabs")]]) > 0) new_position <- max(r[[paste0(r_prefix, "_tabs")]] %>% dplyr::filter(!!rlang::sym(id_col) == element_id) %>% dplyr::pull(position)) + 1
  else new_position <- 1
  
  file_row <- 
    r[[paste0(r_prefix, "_files_list")]] %>% 
    dplyr::filter(id == file_id) %>%
    dplyr::mutate(position = new_position)
  file_ext <- sub(".*\\.", "", tolower(file_row$filename))
  ace_mode <- switch(file_ext, "r" = "r", "py" = "python", "")
  
  # Open a new tab ?
  if (file_id %not_in% r[[paste0(r_prefix, "_tabs")]]$id) r[[paste0(r_prefix, "_tabs")]] <- r[[paste0(r_prefix, "_tabs")]] %>% dplyr::bind_rows(file_row)
  
  shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-", input_prefix, "reload_files_tab', '", now(format = "%Y-%m-%d %H:%M:%OS3"), "');"))
  
  # Hide all editors
  sapply(paste0(input_prefix, "editor_div_", r[[paste0(r_prefix, "_editors")]]$id), shinyjs::hide)
  
  # Create an ace editor ?
  if (file_id %not_in%r[[paste0(r_prefix, "_editors")]]$id){
    r[[paste0(r_prefix, "_editors")]] <- r[[paste0(r_prefix, "_editors")]] %>% dplyr::bind_rows(r[[paste0(r_prefix, "_files_list")]] %>% dplyr::filter(id == file_id))
    open_new_editor <- TRUE
  }
  else open_new_editor <- FALSE
  
  # Add editor UI or show it if already exists
  if (open_new_editor){
    file_code <- readLines(paste0(folder, "/", file_row$filename), warn = FALSE) %>% paste(collapse = "\n")
    
    # Create editor
    insertUI(
      selector = paste0("#", ns(paste0(input_prefix, "editors_div"))), where = "beforeEnd", ui = div(
      id = ns(paste0(input_prefix, "editor_div_", file_id)),
      shinyAce::aceEditor(
        ns(paste0(input_prefix, "editor_", file_id)), value = file_code, mode = ace_mode,
        hotkeys = code_hotkeys,
        theme = user_settings$ace_theme, fontSize = user_settings$ace_font_size,
        autoScrollEditorIntoView = TRUE, height = "100%", debounce = 100, showPrintMargin = FALSE
      ),
      style = "width: 100%; height: 100%; display: flex; flex-direction: column;"
    ))
    
    # Add observers for editor hotkeys
    shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-add_code_editor_hotkeys', ", file_id, ");"))
  }
  else shinyjs::delay(50, shinyjs::show(paste0(input_prefix, "editor_div_", file_id)))
  
  # Update selected_file
  shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-", input_prefix, "selected_file', ", file_id, ");"))
}

files_browser_change_tab <- function(input_prefix, r_prefix, file_id){
  
  # Get variables from other environments
  for (obj_name in c("id", "r")) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-", input_prefix, "selected_file', ", file_id, ");"))
  
  # Set current file tab active
  shinyjs::removeClass(selector = paste0("#", id, "-", input_prefix, "tabs .tab"), class = "active")
  shinyjs::addClass(selector = paste0("#", id, "-", input_prefix, "tab_", file_id), class = "active")
  
  # Show current file editor
  shinyjs::delay(50, shinyjs::show(paste0(input_prefix, "editor_div_", file_id)))
  
  # Hide other editors
  hide_ids <- setdiff(r[[paste0(r_prefix, "_tabs")]]$id, file_id)
  sapply(hide_ids, function(file_id) shinyjs::hide(paste0(input_prefix, "editor_div_", file_id)))
}

files_browser_close_file <- function(input_prefix, r_prefix, element_id, file_id){
  
  # Get variables from other environments
  for (obj_name in c("id", "r")) assign(obj_name, get(obj_name, envir = parent.frame()))
  
  if (id == "plugins") id_col <- "plugin_id"
  else if (id == "project_files") id_col <- "project_id"
  
  # Hide all editors
  sapply(paste0(input_prefix, "editor_div_", r[[paste0(r_prefix, "_editors")]]$id), shinyjs::hide)
  
  r[[paste0(r_prefix, "_tabs")]] <- r[[paste0(r_prefix, "_tabs")]] %>% dplyr::filter(id %not_in% file_id)
  
  # Show editor of first file
  first_file_id <-
    r[[paste0(r_prefix, "_tabs")]] %>%
    dplyr::filter(!rlang::sym(id_col) == element_id) %>%
    dplyr::slice(1) %>%
    dplyr::pull(id)
  
  shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-", input_prefix, "selected_file', ", first_file_id, ");"))
  if (length(first_file_id) > 0) shinyjs::delay(50, shinyjs::show(paste0(input_prefix, "editor_div_", first_file_id)))
  
  shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-", input_prefix, "reload_files_tab', '", now(format = "%Y-%m-%d %H:%M:%OS3"), "');"))
}

files_browser_save_file <- function(input_prefix, r_prefix, folder, element_id, file_id, new_code){
  
  # Get variables from other environments
  for (obj_name in c("id", "r", "output")) assign(obj_name, get(obj_name, envir = parent.frame()))
  i18n <- r$i18n
  ns <- NS(id)
  
  if(length(file_id) > 0){
  
    file <- r[[paste0(r_prefix, "_files_list")]] %>% dplyr::filter(id == file_id)
    
    # Update file
    writeLines(new_code, paste0(folder, "/", file$filename))
    
    # Update update_datetime
    if (id == "plugins") sql_table <- "plugins"
    else if (id == "project_files") sql_table <- "studies"
    sql_send_statement(r$db, glue::glue_sql("UPDATE {`sql_table`} SET update_datetime = {now()} WHERE id = {element_id}", .con = r$db))
    
    show_message_bar("modif_saved", "success")
  }
}

files_browser_edit_filename <- function(input_prefix, r_prefix, folder, element_id, file_id, new_name){
  
  # Get variables from other environments
  for (obj_name in c("id", "r", "output")) assign(obj_name, get(obj_name, envir = parent.frame()))
  i18n <- r$i18n
  ns <- NS(id)
  
  old_name <- r[[paste0(r_prefix, "_files_list")]] %>% dplyr::filter(id == file_id) %>% dplyr::pull(filename)
  
  # Check extension
  check_extension <- grepl("\\.(r|py|csv|css|js)$", new_name, ignore.case = TRUE)
  if (!check_extension){
    show_message_bar("invalid_file_extension", "severeWarning")
    return()
  }
  
  # Check name (no space, no special character and not empty)
  if (new_name == "") check_special_char <- FALSE
  else check_special_char <- grepl("^[\\w\\.]+\\.(r|py|csv|css|js)$", new_name, ignore.case = TRUE, perl = TRUE)
  if (!check_special_char){
    show_message_bar("invalid_filename", "severeWarning")
    return()
  }
  
  # Check if name is already used
  if (tolower(new_name) == tolower(old_name)) check_used_name <- TRUE
  else check_used_name <- !file.exists(paste0(folder, "/", new_name))
  if (!check_used_name){
    show_message_bar("name_already_used", "severeWarning")
    return()
  }
  
  # Update file
  file.rename(paste0(folder, "/", old_name), paste0(folder, "/", new_name))
  
  # Update tabs
  r[[paste0(r_prefix, "_tabs")]] <- r[[paste0(r_prefix, "_tabs")]] %>% dplyr::mutate(filename = dplyr::case_when(id == file_id ~ new_name, TRUE ~ filename))
  shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-", input_prefix, "reload_files_tab', '", now(format = "%Y-%m-%d %H:%M:%OS3"), "');"))
  
  # Update files browser
  r[[paste0(r_prefix, "_files_list")]] <- r[[paste0(r_prefix, "_files_list")]] %>% dplyr::mutate(filename = dplyr::case_when(id == file_id ~ new_name, TRUE ~ filename))
  shinyjs::runjs(paste0("Shiny.setInputValue('", id, "-", input_prefix, "reload_files_browser', '", now(format = "%Y-%m-%d %H:%M:%OS3"), "');"))
  
  show_message_bar("modif_saved", "success")
  
  shinyjs::delay(50, sapply(c(paste0(input_prefix, "filename_div_", file_id), paste0(input_prefix, "edit_filename_button_div_", file_id)), shinyjs::show))
  sapply(c(paste0(input_prefix, "edit_filename_textfield_div_", file_id), paste0(input_prefix, "save_filename_button_div_", file_id)), shinyjs::hide)
}