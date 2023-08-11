#' Format datetime
#' 
#' @description Format datetime depending on the selected language
#' @details English format of datetime is like "2023-05-20 16:56:22"\cr
#' French format is like "20-05-2023 16:56:22".\cr
#' Datetime has to be given in the default format ("\%Y-\%m-\%d \%H:\%M:\%S", like "2023-05-20 16:56:22").
#' @param datetime Datetime converted in character vector (character)
#' @param language Selected language, "en" or "fr" (character)
#' @param sec Should we display seconds or not (logical)
#' @examples 
#' \dontrun{
#'   format_datetime(datetime = "2023-05-20 16:56:22", language = "fr", sec = FALSE)
#' }
format_datetime <- function(datetime = character(), language = "en", sec = TRUE){
  tryCatch({
    
    datetime <- as.character(datetime)
    
    if (tolower(language) == "fr"){
      if (sec) datetime <- format(as.POSIXct(datetime), format = "%d-%m-%Y %H:%M:%S")
      else datetime <- format(as.POSIXct(datetime), format = "%d-%m-%Y %H:%M")
    }
    if (tolower(language) == "en"){
      if (sec) datetime <- format(as.POSIXct(datetime), format = "%Y-%m-%d %H:%M:%S")
      else datetime <- format(as.POSIXct(datetime), format = "%Y-%m-%d %H:%M")
    }
  }, error = function(e) "")
  
  datetime
}

#' Make a shiny.fluent card
#' 
#' @description Creates a shiny.fluent card. Code available in shiny.fluent github pages (Articles).
#' @param title Title of the card (character)
#' @param content Content of the card (character)
#' @param size Size of a card (integer)
#' @param style CSS code to custom the card (character)
#' @return Shiny UI elements / HTML code
#' @examples 
#' \dontrun{
#' make_card(title = "Introduction", content = "This is the text of my introduction card", size = 12)
#' }
make_card <- function(title = character(), content = character(), size = 12, style = "") {
  div(
    class = glue::glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    shiny.fluent::Stack(
      tokens = list(childrenGap = 5),
      shiny.fluent::Text(variant = "large", title, block = TRUE),
      content
    )
  )
}

#' Make combobox
#' 
#' @description Creates a shiny.fluent combo box
#' @param i18n Translator object from shiny.i18n library
#' @param ns Shiny namespace
#' @param id ID used for the input (character)
#' @param label Label used for the input (character)
#' @param options Options available for the combobox (list)
#' @param value Value of the combobox (character)
#' @param multiSelect Is multiselection of options is possible ? (logical)
#' @param allowFreeForm Allows user to enter free text, not provided in the options / choices (logical)
#' @param autoComplete Auto completion of the text entered in the combobox, to match with available options / choices ("on" of "off") (character)
#' @param width Width of the dropdown, CSS code so "300px" or "100\%" are accepted
#' @examples 
#' \dontrun{
#' options <- list(
#'   list(key = "my_key1", text = "my_text1"),
#'   list(key = "my_key2", text = "my_text2")
#' )
#' make_combobox(i18n = i18n, ns = NS("settings_datasets"), id = "my_dropdown", label = "my_dropdown",
#'   options = options, value = "my_key1", multiSelect = FALSE, allowFreeform = FALSE, autoComplete = "on",
#'   width = "100%", disabled = FALSE)
#' }
make_combobox <- function(i18n = character(), ns = character(), id = NA_character_, label = character(), 
  options = list(), value = NULL, multiSelect = FALSE, allowFreeform = FALSE, autoComplete = "on", width = NULL){
  
  style <- ""
  if (!is.null(width)) style <- paste0(style, "width: ", width, ";")
  div(
    div(class = "input_title", i18n$t(label)),
    div(shiny.fluent::ComboBox.shinyInput(ns(id), value = value, options = options, multiSelect = multiSelect, 
      allowFreeform = allowFreeform, autoComplete = autoComplete,
      multiSelectDelimiter = ","), style = style)
  )
}

#' Make dropdown
#' 
#' @description Creates a shiny.fluent dropdown
#' @param i18n Translator object from shiny.i18n library
#' @param ns Shiny namespace
#' @param id ID used for the input (character)
#' @param label Label used for the input (character)
#' @param options Options available for the dropdown (list)
#' @param value Value of the dropdown (character)
#' @param multiSelect Is multiselection of options is possible ? (logical)
#' @param width Width of the dropdown, CSS code so "300px" or "100\%" are accepted (character)
#' @param disabled Is the textfield is disabled (character)
#' @examples 
#' \dontrun{
#' options <- list(
#'   list(key = "my_key1", text = "my_text1"),
#'   list(key = "my_key2", text = "my_text2")
#' )
#' make_dropdown(i18n = i18n, ns = NS("settings_datasets"), id = "my_dropdown", label = "my_dropdown",
#'   options = options, value = "my_key1", multiSelect = FALSE, width = "100%", disabled = FALSE)
#' }
make_dropdown <- function(i18n = character(), ns = character(), id = NA_character_, label = character(), 
  options = list(), value = NULL, multiSelect = FALSE, width = NULL, disabled = FALSE){
  
  if (is.na(id)) id <- label
  style <- ""
  if (!is.null(width)) style <- paste0(style, "width: ", width, ";")
  div(
    div(id = ns(paste0(id, "_title")), class = "input_title", i18n$t(label)),
    div(shiny.fluent::Dropdown.shinyInput(ns(id), value = value, options = options, multiSelect = multiSelect, disabled = disabled), style = style)
  )
}

#' Make layout
#' 
#' @description Creates a complete layout with header, sidenav, main & footer
#' @param language Language used ("en" or "fr") (character)
#' @param page Name of the page (character)
#' @param i18n Translator object from shiny.i18n library
#' @param users_accesses_toggles_options A tibble containing users accesses, to add in database if no internet access (tibble)
#' @examples
#' \dontrun{
#' make_layout(language = "fr", page = "my_subsets", i18n = i18n, users_accesses_toggles_options = users_accesses_toggles_options)
#' }
make_layout <- function(language = "en", page = character(), i18n = character(), users_accesses_toggles_options = tibble::tibble()){
  div(class = "grid-container",
    mod_page_header_ui(id = stringr::str_replace(page, "/", "_"), i18n = i18n),
    mod_page_sidenav_ui(id = stringr::str_replace(page, "/", "_"), i18n = i18n),
    mod_page_main_ui(id = stringr::str_replace(page, "/", "_"), language = language, i18n = i18n, users_accesses_toggles_options = users_accesses_toggles_options),
    mod_page_footer_ui(i18n = i18n)
  )
}

#' Make a page
#' 
#' @description Creates a shiny.fluent page. Code available in shiny.fluent github pages (Articles). 
#' @param title Title of the page (character)
#' @param subtitle Subtitle of the page (character)
#' @param contents Contents of the page (character)
#' @return Shiny UI elements / HTML code
#' @examples
#' \dontrun{
#' make_page(title = "My page title", subtitle = "My page subtitle", contents = "shiny::div('My page content')")
#' }

make_page <- function (title = character(), subtitle = character(), contents = character()) {
  tagList(div(
    class = "page-title",
    htmltools::span(title, class = "ms-fontSize-32 ms-fontWeight-semibold", style = "color: #323130"),
    htmltools::span(subtitle, class = "ms-fontSize-14 ms-fontWeight-regular", style = "color: #605E5C; margin: 14px;")
  ),
  contents)
}

#' Make people picker
#' 
#' @description Creates a shiny.fluent people picker
#' @details 
#' Options argument has to be a data.frame or a tibble, with columns = c("key", "imageInitials", "text", "secondaryText").\cr
#' Key is the ID of the choice, text is the text of the choice, imageInitials are the letters put on a circle,
#' SecondaryText is the text under the principal text.
#' @param i18n Translator object from shiny.i18n library
#' @param ns Shiny namespace
#' @param id ID used for the input (character)
#' @param label Label used for the input (character)
#' @param options Options available for the input (data.frame or tibble)
#' @param value Options already selected (character)
#' @param width CSS code for width, could be all CSS forms, as "200px" or "100\%" etc (character)
#' @param style CSS style added to the div containing the people picker (character)
#' @return HTML / Shiny UI code
#' @examples
#' \dontrun{
#' options <- tibble::tribble(~key,  ~imageInitials, ~text, ~secondaryText,
#'   1, "JD", "John Doe", "Clinician",
#'   2, "DA", "Doug Altman", "Statistician")
#' make_people_picker(i18n = i18n, ns = ns, id = "my_people_picker", "My people picker", 
#'   options = options, value = 2, width = "200px")
#' }
make_people_picker <- function(i18n = character(), ns = character(), id = NA_character_, label = character(), 
  options = tibble::tibble(), value = NULL, width = NULL, style = character()){
  
  if (!is.null(value)) default_selected_items <- options %>% dplyr::filter(key %in% value)
  else default_selected_items <- NULL
  
  style <- ""
  if (!is.null(width)) style <- paste0(style, "width: ", width)
  if (is.na(id)) id <- label
  div(
    div(class = "input_title", i18n$t(label)),
    div(shiny.fluent::NormalPeoplePicker.shinyInput(
      ns(id),
      options = options,
      pickerSuggestionsProps = list(
        suggestionsHeaderText = i18n$t("users"),
        noResultsFoundText = i18n$t("no_results_found"),
        showRemoveButtons = TRUE
      ),
      defaultSelectedItems = default_selected_items,
      value = value),
      style = style
    )
  )
}

#' Make textfield
#' 
#' @description Creates a shiny.fluent textfield
#' @param i18n Translator object from shiny.i18n library
#' @param ns Shiny namespace
#' @param label Displayed name of the textfield (character)
#' @param id Id of the textfield (character)
#' @param value Value of the textfield (character) 
#' @param type Type of the input ("password" or NULL) (character)
#' @param canRevealPassword If the type is password, show a button to reveal the password (logical)
#' @param width Width of the dropdown, CSS code so "300px" or "100\%" are accepted (character)
#' @param min_width Min width of the textfield, same format as width (character)
#' @param max_width Max width of the textfield, same format as width (character)
#' @param margin_right Margin at the right side of the textfield, like "30px" (character)
#' @param disabled Is the textfield is disabled (character)
#' @return Shiny UI elements / HTML code
#' @examples
#' \dontrun{
#' make_textfield(i18n = i18n, ns = ns, label = "password", id = "password", width = "300px", type = "password", canRevealPassword = TRUE)
#' }
make_textfield <- function(i18n = character(), ns = character(), label = character(), id = NA_character_, 
  value = NULL, type = NULL, canRevealPassword = NULL, width = NULL, min_width = NULL, max_width = NULL, 
  margin_right = NULL, disabled = FALSE){
  if (is.na(id)) id <- label
  style <- ""
  if (!is.null(width)) style <- paste0(style, "width: ", width, ";")
  if (is.null(width) & !is.null(min_width) & !is.null(max_width)) style <- paste0(style, "min-width: ", min_width, "; max-width: ", max_width, ";")
  if (!is.null(margin_right)) style <- paste0(style, "margin-right:", margin_right, ";")
  div(
    div(class = "input_title", i18n$t(label)),
    div(shiny.fluent::TextField.shinyInput(ns(id), value = value, type = type, canRevealPassword = canRevealPassword, disabled = disabled), style = style)
  )
}

#' Make toggle
#' 
#' @description Creates a shiny.fluent toggle
#' @param i18n Translator object from shiny.i18n library
#' @param ns Shiny namespace
#' @param id ID used for the input (character)
#' @param label Label used for the input (character)
#' @param value Value of the toggle : TRUE or FALSE (logical)
#' @param inline Should the toggle displayed inline (logical)
#' @param bold Should the text of the title be bold (logical)
#' @return HTML / Shiny UI code
#' @examples
#' \dontrun{
#' make_toggle(i18n = i18n, ns = ns, label = "My toggle", id = "my_toggle", value = TRUE, inline = FALSE, bold = TRUE)
#' }
make_toggle <- function(i18n = character(), ns = character(), id = NULL, label = character(), value = FALSE, 
  inline = FALSE, bold = TRUE){
  if (is.null(id)) id <- label
  label <- i18n$t(label)
  if (bold) style <- "" else style <- "font-weight:normal;"
  if (inline){
    tagList(
      shiny.fluent::Toggle.shinyInput(ns(id), value = value),
      div(class = "toggle_title", label, style = style)
    ) -> result
  }
  if (!inline){
    tagList(
      div(class = "input_title", label, style = style),
      shiny.fluent::Toggle.shinyInput(ns(id), value = value)
    )  -> result
  }
  result
}

#' Make a shiny.fluent card
#' 
#' @description Creates a shiny.fluent card. Adapted from make_card to render correctly shinyAce div.
#' @param title Title of the card (character)
#' @param content Content of the card (character)
#' @param size Size of a card (integer)
#' @param style CSS code to custom the card (character)
#' @return Shiny UI elements / HTML code
#' @examples 
#' \dontrun{
#' make_shiny_ace_card(title = "Introduction", content = "This is the text of my introduction card", size = 12)
#' }
make_shiny_ace_card <- function(title = character(), content = character(), size = 12, style = "") {
  div(
    class = glue::glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    shiny.fluent::Text(variant = "large", title, block = TRUE),
    content
  )
}

#' Render a DT datatable
#' 
#' @description Renders a datatable (from library DT)
#' 
#' @param output variable from Shiny, used to render messages on the message bar
#' @param ns Shiny namespace
#' @param i18n Translator object from shiny.i18n library
#' @param data Data used in the datatable (tibble or dataframe)
#' @param output_name Name of the datatable output
#' @param col_names A character vector containing colnames, already translated (character)
#' @param datatable_dom Character containing DOM code for the datatable (character)
#' @param page_length Page length of the datatable, default to 10 rows (integer)
#' @param editable_cols Which cols are editable (character vector)
#' @param sortable_cols Which cols are sortable (character vector)
#' @param centered_cols Which cols are centered (character vector)
#' @param searchable_cols If filter is TRUE, choose which columns are searchable (character)
#' @param filter If TRUE, we can filter with a textfield each column (logical)
#' @param factorize_cols Which columns are factorized (to be filtered with a dropdown) (character)
#' @param column_widths Columns widths (named character vector)
#' @param hidden_cols Which cols are hidden (character vector)
#' @param selection Can we select one or multiple rows ? ("single" or "multiple") (character)
#' @param bold_rows Which cols are displayed with bold text (character vector)
#' @param shortened_cols Which cols are shortened, and with how many characters (named vector)
#' @examples
#' \dontrun{
#' editable_cols <- c("name", "description", "url_address")
#' sortable_cols <- c("name", "creator_id", "datetime")
#' column_widths <- c("id" = "80px", "datetime" = "130px", "creator_id" = "200px", "action" = "80px", "category" = "130px")
#' centered_cols <- c("creator_id", "datetime", "action", "category")
#' searchable_cols <- c("name", "creator_id", "category")
#' factorize_cols <- c("creator_id", "category")
#' hidden_cols <- c("id", "deleted", "modified", "description")
#' col_names <- get_col_names("git_repos", i18n)
#' shortened_cols <- c("name" = 30, "url_address" = 30, "creator_id" = 20)
#' my_data <- tibble::tibble(col_1 = c("value_1", "value_2"), col_2 = c("value_3", "value_4"))
#' 
#' render_datatable(output = output, ns = ns, i18n = i18n, data = my_data,
#'   output_name = "git_repos_datatable", col_names = col_names, shortened_cols = shortened_cols,
#'   editable_cols = editable_cols, sortable_cols = sortable_cols, centered_cols = centered_cols, column_widths = column_widths,
#'   searchable_cols = searchable_cols, filter = TRUE, factorize_cols = factorize_cols, hidden_cols = hidden_cols)
#' }
render_datatable <- function(output, ns = character(), i18n = character(), data = tibble::tibble(),
  output_name = character(), col_names = character(), datatable_dom = "<'datatable_length'l><'top't><'bottom'p>", page_length = 10,
  editable_cols = character(), sortable_cols = character(), centered_cols = character(), searchable_cols = character(), filter = FALSE, 
  factorize_cols = character(), column_widths = character(), hidden_cols = character(), selection = "single",
  bold_rows = character(), shortened_cols = character()
){
  
  # Translation for datatable
  dt_translation <- list(
    paginate = list(previous = i18n$t("dt_previous"), `next` = i18n$t("dt_next")),
    search = i18n$t("dt_search"),
    lengthMenu = i18n$t("dt_entries"),
    emptyTable = i18n$t("dt_empty"))
  
  # Which columns are non editable
  cols <- c(1:length(names(data))) - 1
  editable_cols_vec <- integer()
  sapply(editable_cols, function(col){
    if (col != "") editable_cols_vec <<- c(editable_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  non_editable_cols_vec <- cols[!cols %in% editable_cols_vec]
  
  # Which columns are non sortable
  sortable_cols_vec <- integer()
  sapply(sortable_cols, function(col){
    if (col != "") sortable_cols_vec <<- c(sortable_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  non_sortable_cols_vec <- cols[!cols %in% sortable_cols_vec]
  
  # Which cols are centered
  centered_cols_vec <- integer()
  sapply(centered_cols, function(col){
    if (col != "") centered_cols_vec <<- c(centered_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  
  # Which cols are hidden
  hidden_cols_vec <- integer()
  sapply(hidden_cols, function(col){
    if (col != "") hidden_cols_vec <<- c(hidden_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  
  # Which cols are searchable
  searchable_cols_vec <- integer()
  sapply(searchable_cols, function(col){
    if (col != "") searchable_cols_vec <<- c(searchable_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  })
  non_searchable_cols_vec <- cols[!cols %in% searchable_cols_vec]
  
  # Whici cols are shortened
  shortened_cols_vec <- integer()
  for (col in shortened_cols) if (col != "") shortened_cols_vec <- 
    c(shortened_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  
  # If filter is TRUE
  if (filter) filter_list <- list(position = "top")
  if (!filter) filter_list <- list()
  
  column_defs <- list()
  # Add columns_widths to column_defs
  sapply(names(column_widths), function(name){
    column_defs <<- rlist::list.append(column_defs, list(width = column_widths[[name]], targets = which(grepl(paste0("^", name, "$"), names(data))) - 1))})
  
  # Add centered_cols to column_defs
  column_defs <- rlist::list.append(column_defs, list(className = "dt-body-center", targets = centered_cols_vec))
  
  # Add hidden_cols to column_defs
  column_defs <- rlist::list.append(column_defs, list(visible = FALSE, targets = hidden_cols_vec))
  
  # Add sortables cols to column_defs
  column_defs <- rlist::list.append(column_defs, list(sortable = FALSE, targets = non_sortable_cols_vec))
  
  # Add searchable cols to column_defs
  column_defs <- rlist::list.append(column_defs, list(searchable = FALSE, targets = non_searchable_cols_vec))
  
  # Add shortened cols to column_defs
  sapply(names(shortened_cols), function(name){
    column_defs <<- rlist::list.append(column_defs, list(
      render = htmlwidgets::JS(paste0(
        "function(data, type, full, meta) {",
        "  if (type === 'display' && data.length > ", shortened_cols[[name]], ") {",
        "    return data.substr(0, ", shortened_cols[[name]], ") + '...';",
        "  } else {",
        "    return data;",
        "  }",
        "}"
      )),
      targets = which(grepl(paste0("^", name, "$"), names(data))) - 1))
    })
  
  
  if (length(shortened_cols) > 0){
    shortened_cols_vec <- integer()
    for (col in names(shortened_cols)) if (col != "") shortened_cols_vec <- 
      c(shortened_cols_vec, c(which(grepl(paste0("^", col, "$"), names(data))) - 1))
  }
  
  # Transform searchable cols to factor
  sapply(factorize_cols, function(col) if (col != "") data <<- data %>% dplyr::mutate_at(col, as.factor))
  
  # Rename cols if lengths correspond
  if (length(col_names) == length(names(data))) names(data) <- col_names
  
  # So data is ready to be rendered in the datatable
  
  data <- DT::datatable(
    data,
    options = list(
      dom = datatable_dom,
      pageLength = page_length, displayStart = 0,
      columnDefs = column_defs,
      language = dt_translation,
      compact = TRUE, hover = TRUE
    ),
    editable = list(target = "cell", disable = list(columns = non_editable_cols_vec)),
    filter = filter_list,
    selection = selection,
    
    # Default options
    rownames = FALSE, escape = FALSE,
    
    # Javascript code allowing to have dropdowns & actionButtons on the DataTable
    callback = htmlwidgets::JS("table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-container');
        });
        Shiny.unbindAll(table.table().node());
        Shiny.bindAll(table.table().node());")
  )
  
  # Bold rows with condition
  
  if (length(bold_rows) > 0){
    for (col_name in names(bold_rows)){
      data <- data %>% DT::formatStyle(i18n$t(col_name), target = "row", fontWeight = DT::styleEqual(bold_rows[col_name], "bold"))
    }
  }
  
  output[[output_name]] <- DT::renderDT(data, server = TRUE)
}

#' Display a message bar
#' 
#' @description Displays a shiny.fluent message bar on the top of the page
#' @details The different possible types are : c("info", "error", "blocked", "severeWarning", "success", "warning")
#' @param output Shiny output variable
#' @param message Message that will be displayed, after translation (character)
#' @param type Type of message bar displayed (reference to Microsoft MessageBarType num) (character)
#' @param i18n Translator object from shiny.i18n library
#' @param time Time the message bar will by displayed, in ms (integer)
#' @param ns Shiny namespace
#' @examples 
#' \dontrun{
#' message_bar(output = output, message = "name_already_used", type = "severeWarning", i18n = i18n, time = 5000, ns = ns)
#' }
show_message_bar <- function(output, message = character(), type = "severeWarning", i18n = character(), time = 7000, ns = character()){
  type <- switch(type, "info" = 0, "error" = 1, "blocked" = 2, "severeWarning" = 3, "success" = 4, "warning" = 5)
  
  id <- sample(1:20, 1)
  
  shinyjs::show(paste0("message_bar", id))
  shinyjs::delay(time, shinyjs::hide(paste0("message_bar", id)))
  
  output_message <- i18n$t(message)
  
  if (length(ns) > 0){
    output[[paste0("message_bar", id)]] <- renderUI(div(
      div(shiny.fluent::MessageBar(output_message, messageBarType = type), style = "z-index:1; margin-top:10px; height:30px; margin-bottom:-30px; max-width:calc(100% - 330px);"),
      div(
        shiny.fluent::IconButton.shinyInput(ns(paste0("close_message_bar_", id)), "", iconProps = list(iconName = "Cancel")), 
        style = "position:sticky; z-index:2; margin-left:calc(100% - 362px);"
      ))
    )
  }
  else {
    output[[paste0("message_bar", id)]] <- renderUI(div(
      shiny.fluent::MessageBar(output_message, messageBarType = type),
      style = "margin-top:10px; max-width:calc(100% - 330px);")
    )
  }
}