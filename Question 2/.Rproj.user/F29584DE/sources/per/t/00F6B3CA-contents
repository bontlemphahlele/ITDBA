
# MAPREDUCE PROJECT
#  1.2 : MAPPER

# 1. Define the Mapper function
map_document <- function(file_path, keyword) {
  
  # Read the document
  document_lines <- readLines(file_path, warn = FALSE)
  
  # Convert keyword to lowercase
  search_keyword <- tolower(keyword)
  
  # Store matching locations
  results <- list()
  
  # Process each line
  for (row_number in seq_along(document_lines)) {
    
    current_line <- tolower(document_lines[row_number])
    
    # Find keyword occurrences
    matches <- gregexpr(
      search_keyword,
      current_line,
      fixed = TRUE
    )[[1]]
    
    # Record matching positions
    if (matches[1] != -1) {
      
      for (column_number in matches) {
        
        results[[length(results) + 1]] <- data.frame(
          Document = basename(file_path),
          Row = row_number,
          Column = column_number
        )
      }
    }
  }
  
  # Return matching locations
  if (length(results) == 0) {
    return(data.frame(
      Document = character(),
      Row = integer(),
      Column = integer()
    ))
  }
  
  do.call(rbind, results)
}

# Confirm that the Mapper has loaded
print("MAPPER FUNCTION LOADED")

# STEP 3: TEST THE MAPPER


# 1. Define the search keyword
keyword <- "africa"

# 2. Identify all seven text documents
files <- list.files(
  path = getwd(),
  pattern = "\\.txt$",
  full.names = TRUE
)

# 3. Apply the Mapper to all documents
mapper_results <- lapply(
  files,
  map_document,
  keyword = keyword
)

# 4. Combine all matching locations
final_results <- do.call(rbind, mapper_results)

# 5. Display the results
cat("\nMAPPER RESULTS\n")
print(final_results)

# 6. Display total occurrences
cat("\nTotal occurrences:", nrow(final_results), "\n")

