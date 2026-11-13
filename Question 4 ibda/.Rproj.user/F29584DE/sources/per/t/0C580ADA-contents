packages <- c(
  "DBI",
  "RPostgres",
  "rstudioapi",
  "dplyr"
)

# Install missing packages
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

# Load packages
library(DBI)
library(RPostgres)
library(rstudioapi)
library(dplyr)

cat("All required packages are installed and loaded.\n")



# STEP 2: CONNECT TO LAYERBASE POSTGRESQL DATABASE

password <- rstudioapi::askForPassword(
  "Enter your Layerbase database password"
)

con <- dbConnect(
  RPostgres::Postgres(),
  host = "q4-3-very-bath.sage.cloud.layerbase.dev",
  port = 5432,
  dbname = "q4_3",
  user = "postgres",
  password = password,
  sslmode = "require"
)

# Verify database connection
dbIsValid(con)



# STEP 3: CREATE POSTGRESQL TABLE

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS plos_articles (
    article_title TEXT NOT NULL,
    authors TEXT,
    publication_date DATE,
    article_url TEXT,
    article_doi TEXT PRIMARY KEY,
    title_word_count INTEGER,
    publication_year INTEGER
  );
")

cat("PostgreSQL table is ready.\n")



# STEP 4: IMPORT CLEANED CSV DATASET
# ============================================================

# Read cleaned dataset
data <- read.csv(
  "C:/Users/27661/OneDrive - Eduvos/Question 4 ibda/artificial_intelligence_cleaned.csv",
  stringsAsFactors = FALSE
)

# Inspect dataset
head(data)

# Check dimensions
dim(data)

# Import records only if the table is empty
existing_records <- dbGetQuery(
  con,
  "SELECT COUNT(*) AS total FROM plos_articles;"
)$total

if (existing_records == 0) {
  dbAppendTable(con, "plos_articles", data)
  cat("Dataset successfully imported.\n")
} else {
  cat("Table already contains", existing_records,
      "records. Import skipped to avoid duplicates.\n")
}



# STEP 5: VERIFY DATABASE IMPORT

total_articles <- dbGetQuery(con, "
  SELECT COUNT(*) AS total_articles
  FROM plos_articles;
")

print(total_articles)

# Inspect imported records
dbGetQuery(con, "
  SELECT *
  FROM plos_articles
  LIMIT 5;
")



