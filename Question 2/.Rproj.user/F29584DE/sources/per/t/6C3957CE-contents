
# MAPREDUCE PROJECT
# 1.3: REDUCER

# Define the Reducer function
reduce_results <- function(mapper_results) {
  
  # Combine Mapper outputs
  combined_results <- do.call(rbind, mapper_results)
  
  # Handle cases where no matches were found
  if (is.null(combined_results) || nrow(combined_results) == 0) {
    return(data.frame(
      Document = character(),
      Row = integer(),
      Column = integer()
    ))
  }
  
  # Sort results by document, row and column
  final_results <- combined_results[
    order(
      combined_results$Document,
      combined_results$Row,
      combined_results$Column
    ),
  ]
  
  return(final_results)
}

print("REDUCER FUNCTION LOADED")

readLines("Reducer.R")



# ============================================================
# STEP 4: RUN THE REDUCER
# ============================================================

# Apply the Reducer to the Mapper outputs
reducer_results <- reduce_results(mapper_results)

# Display the final search results
cat("\nFINAL REDUCER RESULTS\n")
print(reducer_results)

# Display the total number of matches
cat(
  "\nTotal keyword occurrences:",
  nrow(reducer_results),
  "\n"
)
