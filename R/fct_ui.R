#' Format datetime
#' 
#' @description Format datetime depending on the selected language
#' @details English format of datetime is like "2023-05-20 16:56:22"\cr
#' French format is like "20-05-2023 16:56:22".\cr
#' Datetime has to be given in the default format ("\%Y-\%m-\%d \%H:\%M:\%S", like "2023-05-20 16:56:22").
#' @param datetime Datetime converted in character vector (character)
#' @param language Selected language, "en" or "fr" (character)
#' @param sec Should we display seconds or not (logical)
#' @param type Is the object a date or a datetime ? c("datetime", "date") (character)
#' @examples 
#' \dontrun{
#'   format_datetime(datetime = "2023-05-20 16:56:22", language = "fr", sec = FALSE)
#' }
format_datetime <- function(datetime = character(), language = "en", sec = TRUE, type = "datetime"){
  tryCatch({
    
    datetime <- as.character(datetime)
    
    if (tolower(language) == "fr") date_format <- "%d-%m-%Y"
    else date_format <- "%Y-%m-%d"
    
    if (type == "datetime"){
      if (sec) datetime <- format(as.POSIXct(datetime), format = paste0(date_format, " %H:%M:%S"))
      else datetime <- format(as.POSIXct(datetime), format = paste0(date_format, " %H:%M"))
    }
    else if (type == "date") datetime <- format(as.Date(datetime), format = date_format)
  }, error = function(e) "")
  
  datetime
}

#' @noRd
create_hover_card <- function(ui = character(), text = character()){
  escaped_text <- gsub("'", "\\\\'", text)
  escaped_text <- gsub('"', '\\\\"', escaped_text)
  
  shiny.fluent::HoverCard(
  type = "PlainCard",
  plainCardProps = htmlwidgets::JS(paste0("{
    onRenderPlainCard: (a, b, c) => '", escaped_text, "',
    style: { padding: '5px', fontSize: '12px'}
    }")
  ),
  ui
  )
}

#' @noRd
make_card <- function(title = character(), content = character(), size = 12, style = "") {
  additional_style <- "padding:10px 20px 20px 20px;"
  if(length(title) > 0){
    if (length(title) == 1){
      if (!is.na(title) & title != "") additional_style <- ""
    }
    else additional_style <- ""
  }
  
  div(
    class = glue::glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = paste0(additional_style, style),
    shiny.fluent::Stack(
      tokens = list(childrenGap = 5),
      shiny.fluent::Text(variant = "large", title, block = TRUE),
      content
    )
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
#' @noRd
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
  # if (length(col_names) == length(names(data))) names(data) <- col_names
  if (length(col_names) != length(names(data))) col_names <- names(data)
  
  # So data is ready to be rendered in the datatable
  
  data <- DT::datatable(
    data,
    colnames = col_names,
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
    callback = htmlwidgets::JS(
      "table.rows().every(function(i, tab, row) {",
      "  var $this = $(this.node());",
      "  $this.attr('id', this.data()[0]);",
      "  $this.addClass('shiny-input-container');",
      "});",
      "Shiny.unbindAll(table.table().node());",
      "Shiny.bindAll(table.table().node());",
      
      "table.on('draw.dt', function() {",
      "  $('.dataTable tbody tr td').css({",
      "    'height': '12px',",
      "    'padding': '2px 5px'",
      "  });",
      "  $('.dataTable thead tr td div .form-control').css('font-size', '12px');",
      "  $('.dataTable thead tr td').css('padding', '5px');",
      "});"
    )
  )
  
  # Bold rows with condition
  
  if (length(bold_rows) > 0){
    for (col_name in names(bold_rows)){
      data <- data %>% DT::formatStyle(col_name, target = "row", fontWeight = DT::styleEqual(bold_rows[col_name], "bold"))
    }
  }
  
  output[[output_name]] <- DT::renderDT(data, server = TRUE)
}

#' @noRd
show_message_bar <- function(output, message = character(), type = "severeWarning", i18n = character(), time = 7000, ns = character()){
  type <- switch(type, "info" = 0, "error" = 1, "blocked" = 2, "severeWarning" = 3, "success" = 4, "warning" = 5)
  
  output_message <- i18n$t(message)
  
  shinyjs::hide("message_bar")
  
  output$message_bar <- renderUI(div(
    class = "message_bar_container",
    div(shiny.fluent::MessageBar(output_message, messageBarType = type), class = "message_bar"),
    div(
      shiny.fluent::IconButton.shinyInput(ns("close_message_bar"), "", iconProps = list(iconName = "Cancel")), 
      class = "message_bar_close_button"
    ))
  )
  
  shinyjs::delay(50, shinyjs::show("message_bar"))
  shinyjs::delay(time, shinyjs::hide("message_bar"))
}