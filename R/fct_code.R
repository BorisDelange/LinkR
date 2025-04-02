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
  
  # Function to check if a line contains a pipe operator
  contains_pipe <- function(line) {
    trimmed <- trimws(line)
    return(grepl("%>%", trimmed) || grepl("\\|>", trimmed))
  }
  
  # Function to check if a line is likely a continuation of a pipe chain
  is_pipe_continuation <- function(line) {
    trimmed <- trimws(line)
    # Check if the line starts with a function call (no assignment or control structures)
    # This heuristic attempts to identify lines that are likely pipe continuations
    return(nzchar(trimmed) && 
             !grepl("^\\s*#", trimmed) && # Not a comment
             !grepl("^\\s*[a-zA-Z0-9_.]+\\s*(<-|=)", trimmed) && # Not an assignment
             !grepl("^\\s*(if|for|while|function|repeat)\\s*\\(", trimmed) && # Not a control structure
             grepl("^\\s*[a-zA-Z0-9_.:]+\\s*\\(", trimmed)) # Looks like a function call
  }
  
  # Function to get the next valid code block (multi-line block starting at current line)
  # Now with pipe awareness
  get_next_code_block <- function(code_lines, current_line_idx) {
    # If we're at the end of the file, return null
    if (current_line_idx > length(code_lines) || current_line_idx < 1) return(NULL)
    
    # Get the current line
    current_line <- code_lines[current_line_idx]
    if (trimws(current_line) == "" || startsWith(trimws(current_line), "#")) return(NULL)
    
    # New pipe handling - check if the current line has a pipe or if we need to look backward
    pipe_extension <- FALSE
    start_idx <- current_line_idx
    
    # Check if the current line might be a continuation of a pipe chain
    needs_backward_check <- (!contains_pipe(current_line) && current_line_idx > 1) && 
      (is_pipe_continuation(current_line) || trimws(current_line) == "")
    
    # Special case: if the current line looks like a function call but doesn't have a pipe,
    # it might be the end of a pipe chain
    if (needs_backward_check || is_pipe_continuation(current_line)) {
      # Look backward for pipes
      looking_for_start <- TRUE
      for (i in seq(current_line_idx - 1, 1, -1)) {
        current_line_has_pipe <- contains_pipe(code_lines[i])
        if (current_line_has_pipe) {
          start_idx <- i
          pipe_extension <- TRUE
          looking_for_start <- TRUE
        } else if ((pipe_extension || looking_for_start) && trimws(code_lines[i]) != "") {
          # If we've already found a pipe and this line is not empty, include it in the block
          # Or if we're still looking for the start and this line is not empty
          if (i < start_idx || start_idx == current_line_idx) {
            start_idx <- i
          }
          
          # If this line doesn't look like it would be part of the chain, we might have gone too far back
          if (!grepl("\\($", trimws(code_lines[i])) && 
              !grepl("\\)$", trimws(code_lines[i])) && 
              !is_pipe_continuation(code_lines[i]) &&
              !looking_for_start) {
            break
          }
          
          looking_for_start = FALSE
        } else if (!pipe_extension && !looking_for_start) {
          # If we haven't found a pipe yet and this line is not part of what we're looking for, stop
          break
        }
      }
    }
    
    # Look forward for continuation of pipe chain
    end_idx <- current_line_idx
    looking_for_pipe_continuation <- contains_pipe(current_line) || pipe_extension
    
    if (looking_for_pipe_continuation) {
      for (i in (current_line_idx + 1):length(code_lines)) {
        next_line <- trimws(code_lines[i])
        if (next_line == "" || startsWith(next_line, "#")) {
          # Skip empty lines and comments
          next
        } else if (contains_pipe(code_lines[i])) {
          # This line contains a pipe, so include it and keep looking
          end_idx <- i
          looking_for_pipe_continuation <- TRUE
        } else {
          # This line doesn't have a pipe, but include it as part of the chain
          end_idx <- i
          looking_for_pipe_continuation <- FALSE
          break
        }
      }
    }
    
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
      paren_end_idx <- current_line_idx
      while (open_count > close_count && paren_end_idx < length(code_lines)) {
        paren_end_idx <- paren_end_idx + 1
        next_line <- code_lines[paren_end_idx]
        open_count <- open_count + sum(gregexpr("\\(", next_line)[[1]] != -1)
        close_count <- close_count + sum(gregexpr("\\)", next_line)[[1]] != -1)
      }
      
      if (open_count == close_count) {
        # Update end_idx if the parentheses block is longer
        end_idx <- max(end_idx, paren_end_idx)
      }
    }
    
    # If we found a multi-line block, check if it's valid R code
    if (start_idx != current_line_idx || end_idx != current_line_idx) {
      block_code <- paste(code_lines[start_idx:end_idx], collapse = "\n")
      try_parse <- try(parse(text = block_code), silent = TRUE)
      
      if (!inherits(try_parse, "try-error")) {
        return(list(code = block_code, end_line = end_idx))
      }
    }
    
    # If no multi-line pipe block is found, try the original approach
    # Try to find a multi-line block starting with the current line
    remaining_lines <- length(code_lines) - current_line_idx + 1
    if (remaining_lines <= 1) {
      # Try to parse the single line
      try_parse <- try(parse(text = current_line), silent = TRUE)
      if (!inherits(try_parse, "try-error")) {
        return(list(code = current_line, end_line = current_line_idx))
      }
      return(NULL)
    }
    
    for (n in 2:remaining_lines) {  # Start with 2 lines (current + next)
      # Create a potential block from current line and n-1 following lines
      line_indices <- current_line_idx:(current_line_idx + n - 1)
      if (any(line_indices > length(code_lines))) break
      
      code_try <- paste(code_lines[line_indices], collapse = "\n")
      try_parse <- try(parse(text = code_try), silent = TRUE)
      
      if (!inherits(try_parse, "try-error")) {
        return(list(code = code_try, end_line = current_line_idx + n - 1))
      }
    }
    
    # If no multi-line block is found, try just the current line
    try_parse <- try(parse(text = current_line), silent = TRUE)
    if (!inherits(try_parse, "try-error")) {
      return(list(code = current_line, end_line = current_line_idx))
    }
    
    # If we still can't parse it, return NULL
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
        return(list(code = code_try, start_line = start_idx))
      }
    }
    
    # No valid block found
    return(NULL)
  }
  
  # Add a more aggressive mode for backward search
  aggressive_pipe_backward_search <- function(code_lines, current_line_idx, max_lines_back = 20) {
    if (current_line_idx <= 1) return(NULL)
    
    current_line <- code_lines[current_line_idx]
    
    # If current line contains a pipe, we don't need aggressive search
    if (contains_pipe(current_line)) return(NULL)
    
    # If current line doesn't look like a function call continuation, exit early
    if (!is_pipe_continuation(current_line) && !trimws(current_line) %in% c(")")) {
      return(NULL)
    }
    
    # Start with a minimal context and gradually expand
    min_start <- max(1, current_line_idx - max_lines_back)
    
    # Try different starting points, from closest to furthest
    for (start_idx in seq(current_line_idx - 1, min_start, -1)) {
      # For each start point, try different blocks
      for (end_idx in seq(current_line_idx, current_line_idx + 3)) {
        if (end_idx > length(code_lines)) break
        
        block_code <- paste(code_lines[start_idx:end_idx], collapse = "\n")
        
        # Look for pipes in this block
        has_pipe <- any(sapply(code_lines[start_idx:end_idx], contains_pipe))
        if (!has_pipe) {
          next
        }
        
        try_parse <- try(parse(text = block_code), silent = TRUE)
        
        if (!inherits(try_parse, "try-error")) {
          return(list(code = block_code, start_line = start_idx, end_line = end_idx))
        }
      }
    }
    
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
      block_result <- get_block_containing_end(lines, current_line_idx)
      if (!is.null(block_result)) {
        r[[code_store_var]] <- block_result$code
        
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
      
      if (!inherits(try_parse_current, "try-error") && !contains_pipe(current_line) && !is_pipe_continuation(current_line)) {
        # Current line is a valid block by itself, has no pipe, and is NOT a pipe continuation
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
        # Check if current line might be the end of a pipe chain (like tibble::as_tibble())
        if (is_pipe_continuation(current_line) && current_line_idx > 1) {
          # Look for a potential pipe chain this might be a part of
          for (start_idx in seq(current_line_idx - 1, max(1, current_line_idx - 20), -1)) {
            potential_block <- paste(lines[start_idx:current_line_idx], collapse = "\n")
            try_parse <- try(parse(text = potential_block), silent = TRUE)
            
            if (!inherits(try_parse, "try-error")) {
              # Found a valid block
              r[[code_store_var]] <- potential_block
              
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
              return(NULL)  # Exit the function early
            }
          }
          
          # Try aggressive search as a last resort
          aggressive_result <- aggressive_pipe_backward_search(lines, current_line_idx)
          if (!is.null(aggressive_result)) {
            r[[code_store_var]] <- aggressive_result$code
            
            # Move to the next executable line
            next_line <- next_executable_line(lines, aggressive_result$end_line)
            if (!is.null(next_line)) {
              shinyjs::runjs(paste0("ace.edit('", id, "-", editor_id, "').gotoLine(", next_line, ", 0, true);"))
            } else {
              # If no executable line is found, go to the next line
              next_line <- aggressive_result$end_line + 1
              if (next_line <= length(lines)) {
                shinyjs::runjs(paste0("ace.edit('", id, "-", editor_id, "').gotoLine(", next_line, ", 0, true);"))
              }
            }
            return(NULL)  # Exit the function early
          }
        }
        
        # Try to get a multi-line code block starting at current line, with pipe awareness
        block_result <- get_next_code_block(lines, current_line_idx)
        
        if (!is.null(block_result) && nzchar(block_result$code)) {
          r[[code_store_var]] <- block_result$code
          
          # Find the next executable line after the block
          next_line <- next_executable_line(lines, block_result$end_line)
          if (!is.null(next_line)) {
            shinyjs::runjs(paste0("ace.edit('", id, "-", editor_id, "').gotoLine(", next_line, ", 0, true);"))
          } else {
            # If no executable line is found, go to the next line
            next_line <- block_result$end_line + 1
            if (next_line <= length(lines)) {
              shinyjs::runjs(paste0("ace.edit('", id, "-", editor_id, "').gotoLine(", next_line, ", 0, true);"))
            }
          }
        } else {
          # If no valid block found, execute the current line as-is
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