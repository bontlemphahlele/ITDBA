
# HADOOP MAPREDUCE - REDUCER


# Read Mapper output from standard input
input_lines <- readLines("stdin", warn = FALSE)

# Check whether there are any results
if (length(input_lines) > 0) {
  
  # Remove duplicate results
  results <- unique(input_lines)
  
  # Sort the results
  results <- sort(results)
  
  # Output the final matching locations
  cat(results, sep = "\n")
}
