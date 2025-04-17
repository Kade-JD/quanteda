# getting set up

#install.packages("rvest")
#install.packages("quanteda.textmodels")
#install.packages("quanteda.textplots")
#install.packages("quanteda.textstats")
library(pdftools)
library(quanteda)
library(readtext)
library(stringr)
library(rvest)
library(tidyverse)
library(tidycensus)
library(dplyr)
library(quanteda.textmodels)
library(ggplot2)
library(sf)
library(viridis)
library(mapview)
library(maps)
library(quanteda.textplots)
library(quanteda.textstats)
library(curl)
library(here)
here::i_am("./code/functions-import.R")
getwd()

# importing objects for analysis

# CONFIGURATION ----------------------------------------------------------
article_urls <- c(
  sonya_cnn1 = "https://www.cnn.com/2024/07/22/us/sonya-massey-police-shooting",
  sonya_cnn2 = "https://www.cnn.com/2024/07/23/us/sonya-massey-police-shooting-what-went-wrong/index.html",
  sonya_fox1 = "https://www.foxnews.com/us/bodycam-video-reveals-chaotic-scene-deputy-fatally-shooting-sonya-massey-called-911-help",
  sonya_fox2 = "https://www.foxnews.com/sports/yankees-marcus-stroman-speaks-out-following-fatal-shooting-sonya-massey-sad-society-were-living-in",
  sonya_abc1 = "https://abcnews.go.com/US/illinois-woman-dies-after-shot-deputy-involved-incident/story?id=111880175",
  sonya_abc2 = "https://abcnews.go.com/US/deputy-fatally-shot-sonya-massey-discharged-army-misconduct/story?id=112264355",
  sonya_nbc1 = "https://www.nbcnews.com/news/us-news/illinois-woman-called-police-possible-intruder-killed-deputies-attorne-rcna161673",
  sonya_nbc2 = "https://www.nbcnews.com/news/us-news/charges-filed-illinois-deputy-death-sonya-massey-rcna162456"
)

# 2. PROCESSING FUNCTIONS ---------------------------------------------------
clean_file <- function(file) {
  tryCatch({
    read_html(file) %>% 
      html_elements("p, article") %>%
      html_text() %>%
      paste(collapse = "\n")
  }, error = function(e) {
    paste(readLines(file, warn = FALSE), collapse = "\n")
  })
}

# DATA ACQUISITION -------------------------------------------------------
# Process existing files + new downloads
data_dir <- "data"
url_dir <- file.path(data_dir, "url")

all_files <- c(
  list.files(data_dir, pattern = "\\.txt$", full.names = TRUE, recursive = FALSE),
  list.files(url_dir, pattern = "\\.txt$", full.names = TRUE, recursive = FALSE)
) %>% unique()

corpus_texts <- sapply(all_files, clean_file)

# CORPUS CREATION --------------------------------------------------------
doc_corpus <- corpus(
  unname(corpus_texts),
  docnames = basename(all_files)
)

# Metadata
docvars(doc_corpus, "source") <- str_extract(docnames(doc_corpus), 
                                             "(cnn|fox|abc|nbc|nytimes|wapo|comments)"
)
docvars(doc_corpus, "type") <- ifelse(
  grepl("comments", docnames(doc_corpus)),
  "user_comments",
  "news_article"
)

# TEXT PROCESSING --------------------------------------------
library(quanteda)
# install.packages("textclean")
library(textclean)

tokens_obj <- doc_corpus %>%
  # 1. Pre-Cleaning (BEFORE tokenization)
  textclean::replace_non_ascii() %>%
  textclean::replace_contraction() %>%
  textclean::replace_word_elongation() %>%
  
  # 2. Tokenization
  tokens(
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove_url = TRUE,
    padding = FALSE  # Critical for phrase protection
  ) %>%
  
  # 3. Post-Tokenization Cleaning
  tokens_remove(
    pattern = c(
      "\\p{So}", 
      "\\p{C}",
      "^\\p{Pd}$",
      "\\b\\w{1}\\b",  # Remove single letters
      stopwords("en"),
      "comment", "said", "say", "share", "show", "just"
    ),
    valuetype = "regex"
  ) %>%
  
  # 4. Stemming Protection
  tokens_replace(
    pattern = lexicon::hash_lemmas$token,
    replacement = lexicon::hash_lemmas$lemma
  ) %>%
  
  # 5. Targeted Fixes
  tokens_replace(
    pattern = c("polic", "deputi", "repli", "juli"),
    replacement = c("police", "deputy", "reply", "julie")  # Verify context
  )


# ANALYSIS ---------------------------------------------------
news_dfm <- dfm(tokens_obj) %>%
  dfm_trim(min_termfreq = 5)

top_features <- topfeatures(news_dfm, 20)
top_features

# After creating tokens_obj and top_features
saveRDS(tokens_obj, "./data/analysis_objects/tokens.rds")
saveRDS(top_features, "./data/analysis_objects/top_features.rds")
saveRDS(doc_corpus, "./data/analysis_objects/corpus.rds")

# For full reproducibility
save(
  tokens_obj, top_features, doc_corpus,
  file = "./data/analysis_objects/full_analysis.RData"
)

