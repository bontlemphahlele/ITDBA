
# MAPREDUCE PROJECT
# STEP 4: PREPARE HADOOP INPUT

# 1. Locate the seven original text documents
files <- list.files(
  path = getwd(),
  pattern = "^doc[0-9]+\\.txt$",
  full.names = TRUE
)

# 2. Create an empty list for the prepared records
hadoop_records <- list()

# 3. Read each document
for (file in files) {
  
  document_name <- basename(file)
  
  document_lines <- readLines(
    file,
    warn = FALSE
  )
  
  # 4. Attach document name and row number to each line
  document_data <- data.frame(
    Document = document_name,
    Row = seq_along(document_lines),
    Content = gsub("\t", " ", document_lines),
    stringsAsFactors = FALSE
  )
  
  # Store the prepared document
  hadoop_records[[length(hadoop_records) + 1]] <- document_data
}

# 5. Combine all documents
hadoop_input <- do.call(rbind, hadoop_records)

# 6. Save the prepared input file
write.table(
  hadoop_input,
  file = "hadoop_input.tsv",
  sep = "\t",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE
)

# 7. Display preparation summary
cat("HADOOP INPUT PREPARATION\n")
cat("Documents processed:", length(files), "\n")
cat("Total records:", nrow(hadoop_input), "\n")
cat("Output file: hadoop_input.tsv\n")

