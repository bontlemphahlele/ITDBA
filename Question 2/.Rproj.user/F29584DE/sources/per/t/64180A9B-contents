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
library(ggplot2)

#Question 3.1 – Website selection and web scraping

# Define the website
url <- "https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1014408"

response <- httr2::request(url) |>
  httr2::req_user_agent("Academic NLP project (student research)") |>
  httr2::req_perform()

httr2::resp_status(response)

# Extract HTML response
article_html <- xml2::read_html(
  httr2::resp_body_raw(response)
)

# Inspect the page title
rvest::html_element(
  article_html,
  "title"
) |>
  rvest::html_text2()

# Extract readable text from the existing HTML document
article_text <- article_html |>
  rvest::html_elements("#artText p") |>
  rvest::html_text2()

# Remove empty paragraphs
article_text <- article_text[
  stringr::str_trim(article_text) != ""
]

# Combine paragraphs into one document
article_content <- paste(
  article_text,
  collapse = " "
)

# Remove unnecessary whitespace
article_content <- stringr::str_squish(
  article_content
)

# Inspect extraction
cat("Paragraphs extracted:", length(article_text), "\n")

cat("Total characters:", nchar(article_content), "\n")

# Display first 1000 characters
substr(article_content, 1, 1000)

plos_data <- data.frame(
  file_name = "Towards_Modeling_Phage_Therapy",
  content = article_content,
  stringsAsFactors = FALSE
)

# Inspect results
str(plos_data)
nchar(plos_data$content)

# Save extracted data
saveRDS(plos_data, "Towards_Modeling_Phage_Therapy.rds")

# Save a CSV copy
write.csv(plos_data,
          "Towards_Modeling_Phage_Therapy..csv",
          row.names = FALSE)

#Question 3.2 - Text preprocessing 


# Create a copy of the original content
cleaned_text <- plos_data$content

# Convert text to lowercase
cleaned_text <- stringr::str_to_lower(cleaned_text)

# Remove citation references
cleaned_text <- stringr::str_remove_all(
  cleaned_text,
  "\\[[0-9]+\\]|\\[[a-z]\\]"
)

# Remove URLs
cleaned_text <- stringr::str_remove_all(
  cleaned_text,
  "https?://\\S+|www\\.\\S+"
)

# Remove punctuation
cleaned_text <- stringr::str_remove_all(
  cleaned_text,
  "[[:punct:]]"
)

# Remove numbers
cleaned_text <- stringr::str_remove_all(
  cleaned_text,
  "\\b[0-9]+\\b"
)

# Remove unnecessary whitespace
cleaned_text <- stringr::str_squish(cleaned_text)

# Display cleaned text
cat(substr(cleaned_text, 1, 2000))

# Convert cleaned text into individual words
plos_tokens <- data.frame(text = cleaned_text) |>
  tidytext::unnest_tokens(word, text)

# Remove stop words
plos_tokens <- plos_tokens |>
  dplyr::anti_join(tidytext::stop_words, by = "word")

# Remove single-character tokens
plos_tokens <- plos_tokens |>
  dplyr::filter(stringr::str_length(word) > 1)

# Display first 20 tokens
head(plos_tokens, 20)

# Count remaining words
nrow(plos_tokens)


# Display the first 20 tokens
head(plos_tokens, 20)

# Combine the remaining words
final_cleaned_text <- paste(
  plos_tokens$word,
  collapse = " "
)

# Store the cleaned content
plos_data_cleaned <- data.frame(
  file_name = plos_data$file_name,
  content = final_cleaned_text,
  stringsAsFactors = FALSE
)

# Inspect the cleaned data
str(plos_data_cleaned)

# Original character count
original_characters <- nchar(plos_data$content)

# Cleaned character count
cleaned_characters <- nchar(plos_data_cleaned$content)

# Number of remaining words
remaining_words <- nrow(plos_tokens)

# Display comparison
cat("Original characters:", original_characters, "\n")
cat("Cleaned characters:", cleaned_characters, "\n")
cat("Remaining words:", remaining_words, "\n")

# Original character count
original_characters <- nchar(plos_data$content)

# Cleaned character count
cleaned_characters <- nchar(plos_data_cleaned$content)

# Number of remaining words
remaining_words <- nrow(plos_tokens)

# Display comparison
cat("Original characters:", original_characters, "\n")
cat("Cleaned characters:", cleaned_characters, "\n")
cat("Remaining words:", remaining_words, "\n")

head(plos_tokens, 20)
nrow(plos_tokens)

saveRDS(
  plos_data_cleaned,
  "Towards_Modeling_Phage_Therapy_cleaned.rds"
)

write.csv(
  plos_data_cleaned,
  "Towards_Modeling_Phage_Therapy_cleaned.csv",
  row.names = FALSE
)

# Question 3.3 – Text summarisation and visualisation


# Split original article into individual sentences
plos_sentences <- tokenizers::tokenize_sentences(
  plos_data$content
)[[1]]

# Create sentence data frame
sentence_df <- data.frame(
  sentence_id = seq_along(plos_sentences),
  sentence = plos_sentences,
  stringsAsFactors = FALSE
)

# Remove empty sentences
sentence_df <- sentence_df %>%
  filter(
    stringr::str_trim(sentence) != ""
  )

# Inspect sentences
head(sentence_df, 10)

# Count total sentences
cat(
  "Total sentences:",
  nrow(sentence_df),
  "\n"
)
# Calculate frequency of each word
word_frequency <- plos_tokens %>%
  count(
    word,
    sort = TRUE
  )

# Calculate word importance weights
word_frequency <- word_frequency %>%
  mutate(
    weight = n / sum(n)
  )

# Inspect the most frequent words
head(word_frequency, 15)
# Convert sentences into individual words
sentence_words <- sentence_df %>%
  unnest_tokens(
    word,
    sentence
  )

# Remove stop words
sentence_words <- sentence_words %>%
  anti_join(
    tidytext::stop_words,
    by = "word"
  ) %>%
  filter(
    stringr::str_length(word) > 1
  )

# Inspect sentence tokens
head(sentence_words, 20)

# Attach word importance weights to sentence words
sentence_scores <- sentence_words %>%
  inner_join(
    word_frequency,
    by = "word"
  ) %>%
  group_by(sentence_id) %>%
  summarise(
    score = mean(weight),
    word_count = n(),
    .groups = "drop"
  )

# Inspect sentence scores
head(sentence_scores)

# Combine sentence scores with original sentences
frequency_summary <- sentence_df %>%
  inner_join(
    sentence_scores,
    by = "sentence_id"
  ) %>%
  filter(
    word_count >= 8
  ) %>%
  arrange(
    desc(score)
  )

# Select the five highest-scoring sentences
frequency_summary <- frequency_summary %>%
  slice_head(n = 5)

# Display summary
print(frequency_summary)

# Save summary
write.csv(
  frequency_summary,
  "Frequency_Based_Summary.csv",
  row.names = FALSE
)

# Refine the TF-IDF summary
tfidf_summary <- sentence_df %>%
  filter(
    !stringr::str_detect(
      stringr::str_to_lower(sentence),
      "funding:|black boxes|green boxes|red boxes|bound receptors|free receptors|estimates of|equation|eq\\. \\("
    )
  ) %>%
  inner_join(sentence_tfidf, by = "sentence_id") %>%
  filter(word_count >= 8) %>%
  arrange(desc(tfidf_score)) %>%
  slice_head(n = 5)

# Display the results
print(tfidf_summary)

# Save the final summary
write.csv(
  tfidf_summary,
  "TFIDF_Based_Summary.csv",
  row.names = FALSE
)
#Question 3.3
# Select the 100 most frequent words
top_words <- word_frequency %>%
  slice_max(
    order_by = n,
    n = 100
  )

# Generate word cloud
set.seed(123)

wordcloud(
  words = top_words$word,
  freq = top_words$n,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.2,
  colors = brewer.pal(8, "Dark2")
)

# Add title
title(
  main = "Word Cloud of the Phage Therapy Article"
)

# Save graph
png(
  "Figure_1_Word_Cloud.png",
  width = 1200,
  height = 900,
  res = 150
)

set.seed(123)

wordcloud(
  words = top_words$word,
  freq = top_words$n,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  rot.per = 0.2,
  colors = brewer.pal(8, "Dark2")
)

title(main = "Word Cloud of the Phage Therapy Article")
dev.off()

## Select the top 15 words
top_15_words <- word_frequency %>%
  slice_max(
    order_by = n,
    n = 15
  ) %>%
  arrange(n)

# Create bar chart
word_frequency_plot <- ggplot(
  top_15_words,
  aes(
    x = reorder(word, n),
    y = n
  )
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 15 Most Frequent Words in the Article",
    subtitle = "Towards Modeling Phage Therapy",
    x = "Words",
    y = "Frequency",
    caption = "Source: Author's analysis of the PLOS Computational Biology article"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    plot.caption = element_text(size = 9)
  )

# Display graph
print(word_frequency_plot)

# Save graph
ggsave(
  "Figure_2_Top_15_Word_Frequencies.png",
  plot = word_frequency_plot,
  width = 10,
  height = 7,
  dpi = 300
)

# Prepare sentence ranking data
sentence_plot_data <- frequency_summary %>%
  mutate(
    sentence_label = paste(
      "Sentence",
      sentence_id
    )
  )

# Create sentence ranking graph
sentence_ranking_plot <- ggplot(
  sentence_plot_data,
  aes(
    x = reorder(sentence_label, score),
    y = score
  )
) +
  geom_col(fill = "darkseagreen4") +
  coord_flip() +
  labs(
    title = "Top 5 Sentences Ranked by Word Importance",
    subtitle = "Frequency-Based Extractive Summarisation",
    x = "Sentence ID",
    y = "Average Word Importance Score",
    caption = "Source: Author's analysis of the PLOS Computational Biology article"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    plot.caption = element_text(size = 9)
  )

# Display graph
print(sentence_ranking_plot)

# Save graph
ggsave(
  "Figure_3_Sentence_Ranking.png",
  plot = sentence_ranking_plot,
  width = 10,
  height = 7,
  dpi = 300
)



# QUESTION 3.3: RESULTS FOR WRITTEN ANALYSIS

# 1. MOST FREQUENT WORDS AND MAIN THEMES

cat("\n1. TOP 15 MOST FREQUENT WORDS\n")

top_15_words <- word_frequency %>%
  slice_max(order_by = n, n = 15)

print(top_15_words)

cat("\n2. TOP 10 WORDS AND THEIR FREQUENCIES\n")

print(
  word_frequency %>%
    slice_max(order_by = n, n = 10) %>%
    select(word, n)
)


# 2. FREQUENCY-BASED SUMMARISATION

cat("\n3. FREQUENCY-BASED SUMMARY\n")

print(
  frequency_summary %>%
    select(sentence_id, sentence, score, word_count)
)


# 3. TF-IDF SUMMARISATION

cat("\n4. TF-IDF-BASED SUMMARY\n")

print(
  tfidf_summary %>%
    select(sentence_id, sentence, tfidf_score, word_count)
)


# 4. COMPARE THE TWO SUMMARISATION TECHNIQUES

cat("\n5. COMPARISON OF THE TWO METHODS\n")

comparison <- data.frame(
  Method = c("Frequency-based", "TF-IDF"),
  Number_of_sentences = c(
    nrow(frequency_summary),
    nrow(tfidf_summary)
  ),
  Average_score = c(
    mean(frequency_summary$score, na.rm = TRUE),
    mean(tfidf_summary$tfidf_score, na.rm = TRUE)
  )
)

print(comparison)


# 5. WORD FREQUENCY STATISTICS

cat("\n6. WORD FREQUENCY STATISTICS\n")

frequency_statistics <- word_frequency %>%
  summarise(
    Total_unique_words = n(),
    Total_word_occurrences = sum(n),
    Highest_frequency = max(n),
    Average_frequency = round(mean(n), 2)
  )

print(frequency_statistics)


# 6. SENTENCE STATISTICS

cat("\n7. SENTENCE STATISTICS\n")

sentence_statistics <- data.frame(
  Total_sentences = nrow(sentence_df),
  Average_sentence_length = round(
    mean(stringr::str_count(sentence_df$sentence, "\\S+")),
    2
  )
)

print(sentence_statistics)


