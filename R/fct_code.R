#' @noRd
capture_python_output <- function(code){
  
  # Clear the global environment in Python
  reticulate::py_run_string("globals().clear()")
  
  # Parameters to get console output
  reticulate::py_run_string(paste0(
    "import sys\n",
    "from io import StringIO\n",
    "original_stdout = sys.stdout\n",
    "original_stderr = sys.stderr\n",
    "sys.stdout = StringIO()\n",
    "sys.stderr = sys.stdout\n"
  ))
  
  # Run python code
  reticulate::py_run_string(code)
  
  # Get console output
  reticulate::py_run_string(paste0(
    "sys.stdout.seek(0)\n",
    "output = sys.stdout.getvalue()\n",
    "sys.stdout = original_stdout\n",
    "sys.stderr = original_stderr\n"
  ))
  
  reticulate::py$output
}

#' Execute selected code or code block from current cursor position
#' 
#' This function executes either the selection or the code block at the current 
#' cursor position in a shinyAce editor and then moves the cursor to the next 
#' executable line.
#' 
#' @param r The reactive to store the code to execute
#' @param id The page or module ID
#' @param editor_id The shinyAce editor ID
#' @param full_code The complete code in the editor
#' @param editor_input The editor input with the selection
#' @param code_store_var The variable name in r to store the code
#' 
#' @return NULL (used for its side effects)
#' @noRd
execute_ace_code <- function(r, id, editor_id, full_code, editor_input, code_store_var) {
  # Function to find the next executable line (non-comment and non-empty)
  next_executable_line <- function(code_lines, current_line_idx) {
    if (current_line_idx >= length(code_lines)) return(NULL)
    
    for (i in (current_line_idx + 1):length(code_lines)) {
      line <- trimws(code_lines[[i]])
      if (nzchar(line) && !startsWith(line, "#")) return(i)
    }
    return(NULL)
  }
  
  # Function to get the next valid code block (multi-line block starting at current line)
  get_next_code_block <- function(code_lines, current_line_idx) {
    # If we're at the end of the file, return null
    if (current_line_idx > length(code_lines) || current_line_idx < 1) return(NULL)
    
    # Get the current line
    current_line <- code_lines[current_line_idx]
    if (trimws(current_line) == "" || startsWith(trimws(current_line), "#")) return(NULL)
    
    # Special handling for function calls with parentheses
    # Check if the current line ends with an open parenthesis or has unbalanced parentheses
    current_line_trimmed <- trimws(current_line)
    if (endsWith(current_line_trimmed, "(") || 
        (sum(gregexpr("\\(", current_line_trimmed)[[1]] != -1) > 
         sum(gregexpr("\\)", current_line_trimmed)[[1]] != -1))) {
      
      # This is likely the start of a multi-line function call
      # Count opening and closing parentheses to find the matching end
      open_count <- sum(gregexpr("\\(", current_line_trimmed)[[1]] != -1)
      close_count <- sum(gregexpr("\\)", current_line_trimmed)[[1]] != -1)
      
      # Start from the current line and continue until we have balanced parentheses
      end_idx <- current_line_idx
      while (open_count > close_count && end_idx < length(code_lines)) {
        end_idx <- end_idx + 1
        next_line <- code_lines[end_idx]
        open_count <- open_count + sum(gregexpr("\\(", next_line)[[1]] != -1)
        close_count <- close_count + sum(gregexpr("\\)", next_line)[[1]] != -1)
      }
      
      if (open_count == close_count) {
        # We found a balanced block
        block_code <- paste(code_lines[current_line_idx:end_idx], collapse = "\n")
        try_parse <- try(parse(text = block_code), silent = TRUE)
        
        if (!inherits(try_parse, "try-error")) {
          return(block_code)
        }
      }
    }
    
    # Try to find a multi-line block starting with the current line
    remaining_lines <- length(code_lines) - current_line_idx + 1
    if (remaining_lines <= 1) return(current_line)  # Only one line left
    
    for (n in 2:remaining_lines) {  # Start with 2 lines (current + next)
      # Create a potential block from current line and n-1 following lines
      line_indices <- current_line_idx:(current_line_idx + n - 1)
      if (any(line_indices > length(code_lines))) break
      
      code_try <- paste(code_lines[line_indices], collapse = "\n")
      try_parse <- try(parse(text = code_try), silent = TRUE)
      
      if (!inherits(try_parse, "try-error")) {
        return(code_try)
      }
    }
    
    # If no multi-line block is found
    return(NULL)
  }
  
  # Function to get a code block when cursor is on a closing brace
  get_block_containing_end <- function(code_lines, end_line_idx) {
    # Start looking from the current line and go backwards
    if (end_line_idx < 1 || end_line_idx > length(code_lines)) return(NULL)
    
    # Try to find the opening of the block
    for (start_idx in seq(end_line_idx - 1, 1, -1)) {
      # Try with progressively earlier starting points
      code_try <- paste(code_lines[start_idx:end_line_idx], collapse = "\n")
      try_parse <- try(parse(text = code_try), silent = TRUE)
      
      if (!inherits(try_parse, "try-error")) {
        return(code_try)
      }
    }
    
    # No valid block found
    return(NULL)
  }
  
  # Main execution logic
  # Check for empty or null inputs
  if (is.null(editor_input) || is.null(full_code) || !nzchar(full_code)) {
    # Handle empty editor case
    return(NULL)
  }
  
  if (!shinyAce::is.empty(editor_input$selection)) {
    # If a selection exists, execute the selection
    r[[code_store_var]] <- editor_input$selection
    
    # Determine the last line of the selection
    last_selection_line <- editor_input$range$end$row + 1
    
    # Find the next executable line after the selection
    lines <- strsplit(full_code, "\n")[[1]]
    next_line <- next_executable_line(lines, last_selection_line)
    if (!is.null(next_line)) {
      shinyjs::runjs(paste0("ace.edit('", id, "-", editor_id, "').gotoLine(", next_line, ", 0, true);"))
    } else {
      # If no executable line is found, go to the next line
      next_line <- last_selection_line + 1
      if (next_line <= length(lines)) {
        shinyjs::runjs(paste0("ace.edit('", id, "-", editor_id, "').gotoLine(", next_line, ", 0, true);"))
      }
    }
    
  } else if (!is.null(full_code) && nzchar(full_code)) {
    lines <- strsplit(full_code, "\n")[[1]]
    # Fix the indexing by adding 1 to convert from 0-based to 1-based
    current_line_idx <- editor_input$range$start$row + 1
    
    # Check if current_line_idx is valid
    if (current_line_idx < 1 || current_line_idx > length(lines)) {
      return(NULL)  # Exit the function if line index is invalid
    }
    
    # Get the current line
    current_line <- lines[[current_line_idx]]
    
    # Check if current line is a closing brace or similar end-of-block marker
    if (trimws(current_line) %in% c("}", ")", "]", "end", "fi", "esac", "done")) {
      # This might be the end of a block - try to find the block start
      block_code <- get_block_containing_end(lines, current_line_idx)
      if (!is.null(block_code)) {
        r[[code_store_var]] <- block_code
        
        # Move to the next executable line after current line
        next_line <- next_executable_line(lines, current_line_idx)
        if (!is.null(next_line)) {
          shinyjs::runjs(paste0("ace.edit('", id, "-", editor_id, "').gotoLine(", next_line, ", 0, true);"))
        } else {
          # If no executable line is found, go to the next line
          next_line <- current_line_idx + 1
          if (next_line <= length(lines)) {
            shinyjs::runjs(paste0("ace.edit('", id, "-", editor_id, "').gotoLine(", next_line, ", 0, true);"))
          }
        }
      } else {
        # No valid block found, treat as regular line
        r[[code_store_var]] <- current_line
        next_line <- next_executable_line(lines, current_line_idx)
        if (!is.null(next_line)) {
          shinyjs::runjs(paste0("ace.edit('", id, "-", editor_id, "').gotoLine(", next_line, ", 0, true);"))
        } else {
          # If no executable line is found, go to the next line
          next_line <- current_line_idx + 1
          if (next_line <= length(lines)) {
            shinyjs::runjs(paste0("ace.edit('", id, "-", editor_id, "').gotoLine(", next_line, ", 0, true);"))
          }
        }
      }
    } else {
      # First check if the current line is a valid block by itself
      try_parse_current <- try(parse(text = current_line), silent = TRUE)
      
      if (!inherits(try_parse_current, "try-error")) {
        # Current line is a valid block by itself
        r[[code_store_var]] <- current_line
        
        # Move to the next executable line
        next_line <- next_executable_line(lines, current_line_idx)
        if (!is.null(next_line)) {
          shinyjs::runjs(paste0("ace.edit('", id, "-", editor_id, "').gotoLine(", next_line, ", 0, true);"))
        } else {
          # If no executable line is found, go to the next line
          next_line <- current_line_idx + 1
          if (next_line <= length(lines)) {
            shinyjs::runjs(paste0("ace.edit('", id, "-", editor_id, "').gotoLine(", next_line, ", 0, true);"))
          }
        }
      } else {
        # Try to get a multi-line code block starting at current line
        block_code <- get_next_code_block(lines, current_line_idx)
        
        if (!is.null(block_code) && nzchar(block_code)) {
          r[[code_store_var]] <- block_code
          
          # Calculate how many lines the block contains
          n_lines <- length(strsplit(block_code, "\n")[[1]])
          last_block_line <- current_line_idx + n_lines - 1
          
          # Find the next executable line after the block
          next_line <- next_executable_line(lines, last_block_line)
          if (!is.null(next_line)) {
            shinyjs::runjs(paste0("ace.edit('", id, "-", editor_id, "').gotoLine(", next_line, ", 0, true);"))
          } else {
            # If no executable line is found, go to the next line
            next_line <- last_block_line + 1
            if (next_line <= length(lines)) {
              shinyjs::runjs(paste0("ace.edit('", id, "-", editor_id, "').gotoLine(", next_line, ", 0, true);"))
            }
          }
        } else {
          # If it's just a single line (not a block) or no valid block found
          r[[code_store_var]] <- current_line
          
          next_line <- next_executable_line(lines, current_line_idx)
          if (!is.null(next_line)) {
            shinyjs::runjs(paste0("ace.edit('", id, "-", editor_id, "').gotoLine(", next_line, ", 0, true);"))
          } else {
            # If no executable line is found, go to the next line
            next_line <- current_line_idx + 1
            if (next_line <= length(lines)) {
              shinyjs::runjs(paste0("ace.edit('", id, "-", editor_id, "').gotoLine(", next_line, ", 0, true);"))
            }
          }
        }
      }
    }
  }
}