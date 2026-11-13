#install and load data
install.packages(c(
  "httr2",
  "jsonlite",
  "dplyr",
  "stringr",
  "rvest",
  "tidytext",
  "wordcloud",
  "RColorBrewer",
  "purrr",
  "lubridate",
  "ggplot2"
))

library(httr2)
library(jsonlite)
library(dplyr)
library(stringr)
library(rvest)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(purrr)
library(lubridate) 
# Question 4.1 - Load data

# Website URL
website_url <- paste0(
  "https://journals.plos.org/ploscompbiol/",
  "issue?id=10.1371/issue.pcbi.v20.i09"
)

# 3. Download webpage
response <- httr2::request(website_url) |>
  httr2::req_user_agent(
    "Academic NLP project (student research)"
  ) |>
  httr2::req_perform()

# 4. Check response status
print(httr2::resp_status(response))

# 5. Parse webpage
webpage <- xml2::read_html(
  httr2::resp_body_string(response)
)

# 6. Extract article links
article_links <- webpage %>%
  html_elements("a[href*='article?id=']")

# 7. Display number of links
cat("Article links extracted:", length(article_links))

# 8. Display first 10 titles

#Question 4.2
# Extract article titles and URLs
links_data <- tibble(
  article_title = html_text2(article_links),
  article_url = html_attr(article_links, "href")
)

# Clean extracted links
links_data <- links_data %>%
  mutate(
    # Remove unwanted XML declaration text
    article_title = str_remove(
      article_title,
      '^version="1\\.0" encoding="UTF-8"\\?'
    ),
    
    # Standardise whitespace
    article_title = str_squish(article_title),
    
    # Extract DOI using regex
    article_doi = str_extract(
      article_url,
      "10\\.1371/journal\\.pcbi\\.[A-Za-z0-9.]+"
    ),
    
    # Convert relative URLs into absolute URLs
    article_url = url_absolute(
      article_url,
      website_url
    )
  ) %>%
  filter(
    !is.na(article_doi),
    !is.na(article_title),
    article_title != ""
  ) %>%
  distinct(article_doi, .keep_all = TRUE)

# Display results
print(head(links_data, 10))

# Count unique articles
cat(
  "\nUnique articles extracted:",
  nrow(links_data)
)

nrow(links_data)
head(links_data, 10)

#Extract 25 Articles
# QUESTION 4.1 - EXTRACT ARTICLE METADATA

# 1. DEFINE FUNCTION TO EXTRACT ARTICLE METADATA

extract_article_metadata <- function(title, article_url, doi) {
  
  tryCatch({
    
    # Download individual article page
    article_response <- httr2::request(article_url) %>%
      httr2::req_user_agent(
        "Academic NLP project (student research)"
      ) %>%
      httr2::req_perform()
    
    # Parse HTML content
    article_page <- xml2::read_html(
      httr2::resp_body_string(article_response)
    )
    
    # Extract authors
    authors <- article_page %>%
      html_elements("meta[name='citation_author']") %>%
      html_attr("content")
    
    authors <- if (length(authors) > 0) {
      paste(authors, collapse = "; ")
    } else {
      NA_character_
    }
    
    # Extract publication date
    publication_date <- article_page %>%
      html_element("meta[name='citation_date']") %>%
      html_attr("content")
    
    # Return extracted metadata
    tibble(
      article_title = title,
      authors = authors,
      publication_date = publication_date,
      article_url = article_url,
      article_doi = doi
    )
    
  }, error = function(e) {
    
    # Return record even if extraction fails
    tibble(
      article_title = title,
      authors = NA_character_,
      publication_date = NA_character_,
      article_url = article_url,
      article_doi = doi
    )
    
  })
  
}


# 2. SCRAPE THE 25 SELECTED ARTICLES

article_details <- purrr::map_dfr(
  seq_len(nrow(articles_to_scrape)),
  
  function(i) {
    
    result <- extract_article_metadata(
      title = articles_to_scrape$article_title[i],
      article_url = articles_to_scrape$article_url[i],
      doi = articles_to_scrape$article_doi[i]
    )
    
    # Pause between requests
    Sys.sleep(1)
    
    result
  }
)


# 3. INSPECT EXTRACTED DATA

print(head(article_details, 10))

# Check number of records
nrow(article_details)

# Check missing values
colSums(is.na(article_details))


# 4. CREATE INITIAL RAW DATASET

ai_research_raw <- article_details %>%
  mutate(
    
    # Count words in article titles using regex
    title_word_count = str_count(
      article_title,
      "\\b[[:alnum:]]+\\b"
    ),
    
    # Convert publication date into Date format
    publication_date = lubridate::mdy(
      publication_date,
      quiet = TRUE
    ),
    
    # Extract publication year
    publication_year = lubridate::year(
      publication_date
    )
    
  )


# 5. INSPECT RAW DATASET

# Dataset structure
str(ai_research_raw)

# First 10 records
print(head(ai_research_raw, 10))

# Dataset dimensions
dim(ai_research_raw)

# Missing values per column
colSums(is.na(ai_research_raw))


# 7. EXPORT INITIAL RAW DATASET

write.csv(
  ai_research_raw,
  "artificial_intelligence_raw.csv",
  row.names = FALSE,
  na = ""
)

# Save an RDS copy
saveRDS(
  ai_research_raw,
  "artificial_intelligence_raw.rds"
)


# QUESTION 4.1: RAW DATA QUALITY CHECKS


# Dataset dimensions
dim(ai_research_raw)

# Missing values in each column
colSums(is.na(ai_research_raw))

# Check duplicate DOIs
sum(duplicated(ai_research_raw$article_doi))

# Check duplicate article titles
sum(duplicated(ai_research_raw$article_title))

# Summary of numerical attributes
summary(
  ai_research_raw %>%
    select(title_word_count, publication_year)
)

# Check publication dates
range(
  ai_research_raw$publication_date,
  na.rm = TRUE
)

# Check for blank titles or author fields
sum(str_trim(ai_research_raw$article_title) == "")
sum(
  !is.na(ai_research_raw$authors) &
    str_trim(ai_research_raw$authors) == ""
)



# QUESTION 4.2: DATA CLEANING AND PREPARATION


# 1. CREATE A COPY OF THE RAW DATASET

ai_research_clean <- ai_research_raw


# 2. REMOVE LEADING AND TRAILING WHITESPACE

ai_research_clean <- ai_research_clean %>%
  mutate(
    article_title = str_trim(article_title),
    authors = str_trim(authors),
    article_url = str_trim(article_url),
    article_doi = str_trim(article_doi)
  )


# 3. STANDARDISE TEXT FORMATTING

ai_research_clean <- ai_research_clean %>%
  mutate(
    article_title = str_squish(article_title),
    authors = str_squish(authors),
    article_doi = str_to_lower(article_doi)
  )


# 4. HANDLE MISSING AND BLANK VALUES

# Convert blank text fields into NA
ai_research_clean <- ai_research_clean %>%
  mutate(
    article_title = na_if(article_title, ""),
    authors = na_if(authors, ""),
    article_url = na_if(article_url, ""),
    article_doi = na_if(article_doi, "")
  )


# 5. VALIDATE NUMERICAL ATTRIBUTES

# Recalculate title word count using regex
ai_research_clean <- ai_research_clean %>%
  mutate(
    title_word_count = str_count(
      article_title,
      "\\b[[:alnum:]]+\\b"
    )
  )


# 6. VALIDATE PUBLICATION DATES

ai_research_clean <- ai_research_clean %>%
  mutate(
    publication_year = year(publication_date)
  )


# 7. REMOVE DUPLICATE RECORDS

ai_research_clean <- ai_research_clean %>%
  distinct(article_doi, .keep_all = TRUE)


# 8. CHECK FOR INVALID VALUES

# Check missing values
colSums(is.na(ai_research_clean))

# Check duplicate DOIs
sum(duplicated(ai_research_clean$article_doi))

# Check blank author fields
sum(
  !is.na(ai_research_clean$authors) &
    str_trim(ai_research_clean$authors) == ""
)

# Check title word counts
summary(ai_research_clean$title_word_count)

# Check publication years
table(ai_research_clean$publication_year)


# 9. INSPECT CLEANED DATASET

str(ai_research_clean)

head(ai_research_clean, 10)

dim(ai_research_clean)


# 10. EXPORT CLEANED DATASET TO CSV

write.csv(
  ai_research_clean,
  "artificial_intelligence_cleaned.csv",
  row.names = FALSE,
  na = ""
)

# Save an RDS copy
saveRDS(
  ai_research_clean,
  "artificial_intelligence_cleaned.rds"
)

#Confirmation:
# Confirm number of records and columns
dim(ai_research_clean)

# Confirm missing values
colSums(is.na(ai_research_clean))

# Confirm duplicate DOIs
sum(duplicated(ai_research_clean$article_doi))

# Confirm CSV was exported
file.exists("artificial_intelligence_cleaned.csv")

# Confirm cleaned data structure
str(ai_research_clean)
