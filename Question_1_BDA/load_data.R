
# MAPREDUCE PROJECT
# Step 1: Load and Inspect the News Article Documents

# 1. Identify all text documents
files <- list.files(
  path = getwd(),
  pattern = "\\.txt$",
  full.names = TRUE
)

# 2. Display document names
print(files)

# 3. Count the documents
cat("Number of articles:", length(files), "\n")

# 4. Read the first article

article1 <- readLines(files[1], warn = FALSE)

cat("\nFirst 20 lines of doc1:\n")
print(head(article1, 20))

cat("\nTotal lines in doc1:", length(article1), "\n")
