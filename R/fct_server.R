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