#' Monitor performances
#' 
#' @description Monitor performances of the ShinyApp, with calculating time each step takes
#' @param r Shiny reactive value, used to communicate between modules (reactiveValue)
#' @param action If the function is called at the beginning of the end of a task, takes "start" or "stop" values (character)
#' @param task Name of the task (character)
#' @examples 
#' \dontrun{
#'  monitor_perf(r = r, action = "start", task = "Load database")
#' }
monitor_perf <- function(r = shiny::reactiveValues(), action = "stop", task = character()){
  
  # if (!r$perf_monitoring) return()
  if (action == "start") datetime_start <<- now()
  
  if (action == "stop"){
    datetime_stop <<- now()
    
    r$perf_monitoring_table <- 
      isolate(r$perf_monitoring_table) %>% 
      dplyr::bind_rows(tibble::tribble(
        ~task, ~datetime_start, ~datetime_stop, 
        task, datetime_start, datetime_stop))
    
    datetime_start <<- now() 
  }
}

#' Capture python output
#'
#' @description A function to get Python console result
#' @param code Python code that you want to run (character)
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