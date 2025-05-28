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
  editable_cols = character(), sortable_cols = character(), centered_cols = character(),
  searchable_cols = character(), search_filters = NULL, filter = FALSE, 
  factorize_cols = character(), column_widths = character(), hidden_cols = character(), selection = "single",
  bold_rows = character(), shortened_cols = character(), enable_keyboard_navigation = TRUE
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
  
  # Determine if we use the Select extension
  use_select_extension <- enable_keyboard_navigation
  
  # Base options for DataTable
  dt_options <- list(
    dom = datatable_dom,
    pageLength = page_length, 
    displayStart = 0,
    columnDefs = column_defs,
    language = dt_translation,
    compact = TRUE, 
    hover = TRUE,
    rowId = 0,
    # Base style for cells
    drawCallback = htmlwidgets::JS("
      function() {
        $('.dataTable tbody tr td').css({
          'height': '12px',
          'padding': '2px 5px'
        });
        $('.dataTable thead tr td div .form-control').css('font-size', '12px');
        $('.dataTable thead tr td').css('padding', '5px');
      }
    ")
  )
  
  extensions <- character(0)
  if (use_select_extension) {
    extensions <- c(extensions, "Select")
    
    # Configure the Select extension based on the selection parameter
    dt_options$select <- list(
      style = selection,  # Use the selection parameter directly (single, multi, none, etc.)
      info = FALSE,
      items = 'row'
    )
    
    # If selection is "none", we still need to allow keyboard navigation
    # but we'll disable user selection via mouse clicks through CSS
    if (selection == "none") {
      # More thorough approach to prevent selection
      dt_options$drawCallback <- htmlwidgets::JS("
        function() {
          $('.dataTable tbody tr td').css({
            'height': '12px',
            'padding': '2px 5px'
          });
          $('.dataTable thead tr td div .form-control').css('font-size', '12px');
          $('.dataTable thead tr td').css('padding', '5px');
          
          // Disable text selection on the table
          $('.dataTable').css('user-select', 'none');
          
          // Complete override of the Select extension's behavior
          // Unbind all select events first
          $('.dataTable tbody tr').off('.dt');
          
          // Prevent row selection via mouse for everything except buttons
          $('.dataTable tbody').off('click.dt').on('click.dt', function(e) {
            // Allow events to propagate only for interactive elements
            var $target = $(e.target);
            if (!$target.is('button, .btn, a, input, select') && 
                $target.closest('button, .btn, a, input, select').length === 0) {
              e.preventDefault();
              e.stopPropagation();
              
              // Explicitly clear any selections that might have happened
              setTimeout(function() {
                var table = $('.dataTable').DataTable();
                table.rows({selected: true}).deselect();
              }, 0);
              
              return false;
            }
          });
          
          // Additional insurance: clear any selection after any click
          $('.dataTable').on('click', '**', function(e) {
            setTimeout(function() {
              var table = $('.dataTable').DataTable();
              if (table.rows({selected: true}).indexes().length > 0) {
                table.rows({selected: true}).deselect();
              }
            }, 10);
          });
        }
      ");
      
      # Override the datatable select option to api-only 
      dt_options$select = list(
        style = 'api', # Only programmatic selection 
        info = FALSE,
        items = 'row'
      );
      
      # Add custom CSS to prevent selection highlight
      dt_options$initComplete <- paste0(dt_options$initComplete, "; $('<style>.dataTable tr.selected, .dataTable tr.selected td { background-color: transparent !important; }</style>').appendTo('head');");
      } else {
        # Normal style for cells without disabling selection
        dt_options$drawCallback <- htmlwidgets::JS("
          function() {
            $('.dataTable tbody tr td').css({
              'height': '12px',
              'padding': '2px 5px'
            });
            $('.dataTable thead tr td div .form-control').css('font-size', '12px');
            $('.dataTable thead tr td').css('padding', '5px');
          }
        ")
      }
  }
  
  # Define callback for Click + Select
  click_callback <- "
    table.on('select', function(e, dt, type, indexes) {
      if (type === 'row') {
        var rowData = table.rows(indexes).data().toArray();
        var DT_id = table.table().container().parentNode.id;
        
        // Ajustement pour l'indexation R (ajouter 1)
        var r_indexed_indexes = [];
        for (var i = 0; i < indexes.length; i++) {
          r_indexed_indexes.push(indexes[i] + 1);
        }
        
        Shiny.setInputValue(DT_id + '_rows_selected', r_indexed_indexes, {priority: 'event'});
      }
    });
    
    // Add handler for deselect
    table.on('deselect', function(e, dt, type, indexes) {
      try {
        var DT_id = table.table().container().parentNode.id;
        // Send empty array to indicate that no row is selected
        Shiny.setInputValue(DT_id + '_rows_selected', [], {priority: 'event'});
      } catch(err) {
        console.error('Error in deselect event:', err);
      }
    });
  "
  
  # Add code for keyboard navigation if enabled
  if (enable_keyboard_navigation) {
    
    # Create a variable to indicate if selection is disabled
    selection_disabled <- selection == "none"
    
    keyboard_js <- paste0(
      "function(settings, json) {
        // Wait for the table to be fully loaded
        setTimeout(function() {
          // Find the table
          var tableEl = settings.nTable;
          if (!tableEl) return;
          
          // Get DataTable instance
          var table = $(tableEl).DataTable();
          
          // Add tabindex for keyboard navigation
          $(tableEl).attr('tabindex', '0');
          
          // Remove focus outline
          $('<style>#' + tableEl.id + ':focus{outline:0!important}</style>').appendTo('head');
          
          // If selection is disabled, add style to hide selection
          ", 
          if(selection_disabled) {
            "$('<style>.dataTable tr.selected, .dataTable tr.selected td { background-color: transparent !important; }</style>').appendTo('head');"
          } else { 
            "" 
          },
          "
          
          // Configure callback for Select
          ", click_callback, "
          
          // Add keyboard navigation
          $(tableEl).off('keydown').on('keydown', function(e) {
            try {
              // Get only visible (filtered) rows indexes on the current page
              var pageInfo = table.page.info();
              var visibleIndexes = table.rows({search: 'applied', page: 'current'}).indexes();
              var totalVisibleRows = visibleIndexes.length;
              
              if (totalVisibleRows === 0) return; // No visible rows, don't proceed
              
              // Get the index of the selected row, if any
              var selectedIndexes = table.rows({selected: true}).indexes();
              var currentIndex = selectedIndexes.length > 0 ? selectedIndexes[0] : -1;
              
              // Find the position of the currentIndex in the visibleIndexes array
              var currentPosition = -1;
              for (var i = 0; i < visibleIndexes.length; i++) {
                if (visibleIndexes[i] === currentIndex) {
                  currentPosition = i;
                  break;
                }
              }
              
              switch(e.keyCode) {
                // Down arrow
                case 40:
                  if (currentPosition < totalVisibleRows - 1 && currentPosition !== -1) {
                    try {
                      // Deselect all rows first
                      table.rows().deselect();
                      
                      // Get the next visible row index
                      var nextVisibleIndex = visibleIndexes[currentPosition + 1];
                      
                      ", 
                      if(selection_disabled) {
                        paste0("
                        // With selection disabled, just track the current row without visual selection
                        var DT_id = table.table().container().parentNode.id;
                        Shiny.setInputValue(DT_id + '_rows_selected', [nextVisibleIndex + 1], {priority: 'event'});
                        
                        // Scroll if necessary without selecting
                        var nextRow = table.row(nextVisibleIndex).node();
                        if (nextRow && nextRow.scrollIntoView) {
                          nextRow.scrollIntoView({block: 'nearest'});
                        }
                        ")
                      } else {
                        paste0("
                        // Normal selection
                        table.row(nextVisibleIndex).select();
                        
                        // Scroll if necessary
                        var nextRow = table.row(nextVisibleIndex).node();
                        if (nextRow && nextRow.scrollIntoView) {
                          nextRow.scrollIntoView({block: 'nearest'});
                        }
                        ")
                      },
                      "
                    } catch(err) {
                      console.error('Error navigating down:', err);
                    }
                  } else if (currentPosition === -1 && totalVisibleRows > 0) {
                    // If no row is selected, select the first visible one
                    try {
                      var firstVisibleIndex = visibleIndexes[0];
                      ", 
                      if(selection_disabled) {
                        paste0("
                        // With selection disabled, just track the first visible row without visual selection
                        var DT_id = table.table().container().parentNode.id;
                        Shiny.setInputValue(DT_id + '_rows_selected', [firstVisibleIndex + 1], {priority: 'event'});
                        ")
                      } else {
                        "table.row(firstVisibleIndex).select();"
                      },
                      "
                    } catch(err) {
                      console.error('Error selecting first row:', err);
                    }
                  } else if (pageInfo.page < pageInfo.pages - 1) {
                    // If we're at the last row of the current page but not the last page, go to next page
                    table.page('next').draw('page');
                    
                    // Select the first row on the new page
                    setTimeout(function() {
                      var newIndexes = table.rows({search: 'applied', page: 'current'}).indexes();
                      if (newIndexes.length > 0) {
                        var firstRowIndex = newIndexes[0];
                        
                        if (selection_disabled) {
                          var DT_id = table.table().container().parentNode.id;
                          Shiny.setInputValue(DT_id + '_rows_selected', [firstRowIndex + 1], {priority: 'event'});
                        } else {
                          table.rows().deselect();
                          table.row(firstRowIndex).select();
                        }
                      }
                    }, 100);
                  }
                  e.preventDefault();
                  break;
                
                // Up arrow
                case 38:
                  if (currentPosition > 0) {
                    try {
                      // Deselect all rows first
                      table.rows().deselect();
                      
                      // Get the previous visible row index
                      var prevVisibleIndex = visibleIndexes[currentPosition - 1];
                      
                      ", 
                      if(selection_disabled) {
                        paste0("
                        // With selection disabled, just track the current row without visual selection
                        var DT_id = table.table().container().parentNode.id;
                        Shiny.setInputValue(DT_id + '_rows_selected', [prevVisibleIndex + 1], {priority: 'event'});
                        
                        // Scroll if necessary without selecting
                        var prevRow = table.row(prevVisibleIndex).node();
                        if (prevRow && prevRow.scrollIntoView) {
                          prevRow.scrollIntoView({block: 'nearest'});
                        }
                        ")
                      } else {
                        paste0("
                        // Normal selection
                        table.row(prevVisibleIndex).select();
                        
                        // Scroll if necessary
                        var prevRow = table.row(prevVisibleIndex).node();
                        if (prevRow && prevRow.scrollIntoView) {
                          prevRow.scrollIntoView({block: 'nearest'});
                        }
                        ")
                      },
                      "
                    } catch(err) {
                      console.error('Error navigating up:', err);
                    }
                  } else if (currentPosition === -1 && totalVisibleRows > 0) {
                    // If no row is selected, select the first visible one
                    try {
                      var firstVisibleIndex = visibleIndexes[0];
                      ", 
                      if(selection_disabled) {
                        paste0("
                        // With selection disabled, just track the first visible row without visual selection
                        var DT_id = table.table().container().parentNode.id;
                        Shiny.setInputValue(DT_id + '_rows_selected', [firstVisibleIndex + 1], {priority: 'event'});
                        ")
                      } else {
                        "table.row(firstVisibleIndex).select();"
                      },
                      "
                    } catch(err) {
                      console.error('Error selecting first row:', err);
                    }
                  } else if (pageInfo.page > 0) {
                    // If we're at the first row of the current page but not the first page, go to previous page
                    table.page('previous').draw('page');
                    
                    // Select the last row on the new page
                    setTimeout(function() {
                      var newIndexes = table.rows({search: 'applied', page: 'current'}).indexes();
                      if (newIndexes.length > 0) {
                        var lastRowIndex = newIndexes[newIndexes.length - 1];
                        
                        if (selection_disabled) {
                          var DT_id = table.table().container().parentNode.id;
                          Shiny.setInputValue(DT_id + '_rows_selected', [lastRowIndex + 1], {priority: 'event'});
                        } else {
                          table.rows().deselect();
                          table.row(lastRowIndex).select();
                        }
                      }
                    }, 100);
                  }
                  e.preventDefault();
                  break;
                  
                // Left arrow - go to previous page
                case 37:
                  try {
                    if (pageInfo.page > 0) {
                      // Move to previous page
                      table.page('previous').draw('page');
                      
                      // Select the first row on the new page
                      setTimeout(function() {
                        var newIndexes = table.rows({search: 'applied', page: 'current'}).indexes();
                        if (newIndexes.length > 0) {
                          var firstRowIndex = newIndexes[0];
                          
                          if (selection_disabled) {
                            var DT_id = table.table().container().parentNode.id;
                            Shiny.setInputValue(DT_id + '_rows_selected', [firstRowIndex + 1], {priority: 'event'});
                          } else {
                            table.rows().deselect();
                            table.row(firstRowIndex).select();
                          }
                        }
                      }, 100);
                    }
                  } catch(err) {
                    console.error('Error navigating to previous page:', err);
                  }
                  e.preventDefault();
                  break;
                
                // Right arrow - go to next page
                case 39:
                  try {
                    if (pageInfo.page < pageInfo.pages - 1) {
                      // Move to next page
                      table.page('next').draw('page');
                      
                      // Select the first row on the new page
                      setTimeout(function() {
                        var newIndexes = table.rows({search: 'applied', page: 'current'}).indexes();
                        if (newIndexes.length > 0) {
                          var firstRowIndex = newIndexes[0];
                          
                          if (selection_disabled) {
                            var DT_id = table.table().container().parentNode.id;
                            Shiny.setInputValue(DT_id + '_rows_selected', [firstRowIndex + 1], {priority: 'event'});
                          } else {
                            table.rows().deselect();
                            table.row(firstRowIndex).select();
                          }
                        }
                      }, 100);
                    }
                  } catch(err) {
                    console.error('Error navigating to next page:', err);
                  }
                  e.preventDefault();
                  break;
              }
            } catch(err) {
              console.error('Error in key handler:', err);
            }
          });
          
          // Focus on the table
          $(tableEl).focus();
        }, 500);
      }"
    )
  
    dt_options$initComplete <- htmlwidgets::JS(keyboard_js)
  }
  
  if (!is.null(search_filters)) {
    # Initialize the list for column search filters
    column_searches <- list()
    
    # Loop through each filter in the search_filters vector
    for (i in seq_along(search_filters)) {
      if (search_filters[i] != "") {
        # Add the column index (0-based for JavaScript) and the filter value
        # to our column_searches list
        column_searches[[length(column_searches) + 1]] <- list(
          column = i - 1,  # Use 0-based index for JavaScript
          search = search_filters[i]
        )
      }
    }
    
    # Add column filters to the options if any exist
    if (length(column_searches) > 0) {
      # Create a searchCols array with entries for each column
      dt_options$searchCols <- lapply(1:length(names(data)), function(i) {
        col_idx <- i - 1  # Convert to 0-based index for JavaScript
        
        # Find if this column has a filter specification
        filter_idx <- which(sapply(column_searches, function(x) x$column == col_idx))
        
        if (length(filter_idx) > 0) {
          # If a filter exists for this column, apply it
          list(search = column_searches[[filter_idx]]$search)
        } else {
          # Otherwise use an empty search
          list(search = "")
        }
      })
    }
  }
  
  dt <- DT::datatable(
    data,
    colnames = col_names,
    options = dt_options,
    editable = list(target = "cell", disable = list(columns = non_editable_cols_vec)),
    filter = filter_list,
    selection = if(use_select_extension) "none" else selection, # Disable native DT selection if using Select
    rownames = FALSE,
    escape = FALSE,
    extensions = extensions,
    callback = htmlwidgets::JS(
      "table.rows().every(function(i, tab, row) {",
      "  var $this = $(this.node());",
      "  $this.attr('id', this.data()[0]);",
      "  $this.addClass('shiny-input-container');",
      "});",
      "Shiny.unbindAll(table.table().node());",
      "Shiny.bindAll(table.table().node());"
    )
  )
  
  # Bold rows with condition
  
  if (length(bold_rows) > 0){
    for (col_name in names(bold_rows)){
      dt <- dt %>% DT::formatStyle(col_name, target = "row", fontWeight = DT::styleEqual(bold_rows[col_name], "bold"))
    }
  }
  
  output[[output_name]] <- DT::renderDT(dt, server = !use_select_extension)
}

#' @noRd
show_message_bar <- function(id, output, message = character(), type = "severeWarning", i18n = character(), time = 7000, ns = character()){
  # Convert message type to numeric value for Fluent UI
  type <- switch(type, "info" = 0, "error" = 1, "blocked" = 2, "severeWarning" = 3, "success" = 4, "warning" = 5)
  
  # Translate message if i18n is provided
  output_message <- i18n$t(message)
  
  # Clear all previous timeouts
  shinyjs::runjs("
    if (window.messageBarTimeouts) {
      window.messageBarTimeouts.forEach(timeout => clearTimeout(timeout));
    }
    window.messageBarTimeouts = [];
  ")
  
  # Hide any existing message bar
  shinyjs::hide("message_bar")
  
  # Render the new message bar UI
  output$message_bar <- renderUI(div(
    class = "message_bar_container",
    div(shiny.fluent::MessageBar(output_message, messageBarType = type), class = "message_bar"),
    div(
      shiny.fluent::IconButton.shinyInput(ns("close_message_bar"), "", iconProps = list(iconName = "Cancel")), 
      class = "message_bar_close_button"
    ))
  )
  
  # Show the message bar
  shinyjs::show("message_bar")
  
  # Set timeout to hide the message bar
  shinyjs::runjs(paste0("
    window.messageBarTimeouts.push(setTimeout(function() {
      $('#", id, "-message_bar').hide();
    }, ", time, "));
  "))
}

#' @noRd
toggle_comments <- function(input_id, code, selection, session) {
  
  # Get variables from other environments
  id <- get("id", envir = parent.frame())
  
  lines <- strsplit(code, "\n")[[1]]
  if (length(lines) == 0) return()
  start_row <- selection$start$row + 1
  end_row <- selection$end$row + 1
  
  selected_lines <- lines[start_row:end_row]
  
  # Filter out empty lines for analysis
  non_empty_lines <- selected_lines[nchar(trimws(selected_lines)) > 0]
  if (length(non_empty_lines) == 0) return()
  
  # Check if a line is commented
  is_commented <- function(line) {
    trimmed_line <- trimws(line, "left")
    return(startsWith(trimmed_line, "#"))
  }
  
  # If all non-empty lines are commented, uncomment everything
  all_commented <- all(sapply(non_empty_lines, is_commented))
  if (is.na(all_commented)) all_commented <- FALSE
  
  # Process each line
  for (i in start_row:end_row) {
    line <- lines[i]
    
    if (is.na(line) || nchar(trimws(line)) == 0) next
    if (all_commented) lines[i] <- sub("^(\\s*)#\\s*", "\\1", line)
    else lines[i] <- sub("^(\\s*)", "\\1# ", line)
  }
  
  modified_code <- paste(lines, collapse = "\n")
  if (substr(code, nchar(code), nchar(code)) == "\n") {
    modified_code <- paste0(modified_code, "\n")
  }
  
  # Update editor
  shinyAce::updateAceEditor(session, input_id, value = modified_code)
  
  # Restore cursor position
  shinyjs::runjs(sprintf(
    "var editor = ace.edit('%s-%s');
    editor.selection.setRange({
      start: {row: %d, column: %d},
      end: {row: %d, column: %d}
    });
    editor.focus();",
    id, input_id, 
    selection$start$row, selection$start$column,
    selection$end$row, selection$end$column
  ))
}