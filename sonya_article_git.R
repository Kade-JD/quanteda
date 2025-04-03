install.packages("rvest")
install.packages("quanteda.textmodels")
install.packages("quanteda.textplots")
install.packages("quanteda.textstats")
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
getwd()

#cnn1 pre-processing phase
sonya_cnn1 <- "https://www.cnn.com/2024/07/22/us/sonya-massey-police-shooting" 
sonyacnn1page <- read_html(sonya_cnn1) 
text_data_cnn1 <- html_text(html_nodes(sonyacnn1page, "p")) # Extract all text within the <p> tags
print(text_data_cnn1)  #Print the extracted text
#make corpus
corpus_sonyacnn1 <- corpus(text_data_cnn1)
corpus_sonyacnn1 <- # REMOVING LAST THREE TEXTS DUE TO CONTENT
  corpus_subset(corpus_sonyacnn1, 
                !(docnames(corpus_sonyacnn1) %in% c(paste("text",60:62,sep=""))))
tail(corpus_sonyacnn1)
head(corpus_sonyacnn1)
print(corpus_sonyacnn1)
summary(corpus_sonyacnn1)
head(docvars(corpus_sonyacnn1))
#token
sonya_tokencnn1 <- tokens(corpus_sonyacnn1) #tokenized the pdf text
sonya_nonpunc_cnn1 <- tokens(sonya_tokencnn1, remove_punct = TRUE) #remove punctuation
print(sonya_nonpunc_cnn1)
sonya_nostop_cnn1 <- tokens_select(sonya_nonpunc_cnn1, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_cnn1)
cnn_custom_stop_words <- c("said", "says")
sonya_custom_cnn1 <- tokens_select(sonya_nonpunc_cnn1, pattern = c((stopwords("en")),(cnn_custom_stop_words)), selection = "remove") #remove custom words
print(sonya_custom_cnn1)
#dataframe
sonya.dfm.cnn1 <- dfm(sonya_nostop_cnn1) #put the text into a dataframe matrix
sonya.cnn1.frequency <- topfeatures(sonya.dfm.cnn1, n = 10) #asked for the top 10 words in article
print(sonya.cnn1.frequency)
sonya.cnn1.dfm.custom <- dfm(sonya_custom_cnn1)
sonya.cnn1.frequency.custom <- topfeatures(sonya.cnn1.dfm.custom, n = 10)
print(sonya.cnn1.frequency.custom)
#keywords
kw_water_cnn1 <- kwic(sonya_tokencnn1, pattern =  "water") #searching for the word in association with other words
print(kw_water_cnn1)
kw_rebuke_cnn1 <- kwic(sonya_tokencnn1, pattern =  "rebuke")
print(kw_rebuke_cnn1)
kw_justification_cnn1 <- kwic(sonya_tokencnn1, pattern = phrase("boiling water*")) #phrase meaning
print(kw_justification_cnn1)
sonya_threat_cnn1 <- tokens_select(sonya_nostop_cnn1, pattern = c("speak*","deputy*"), padding = TRUE, window = 5) #view words associated with key words
print(sonya_threat_cnn1)

#cnn has blocked all comments on articles regarding sonya massey

#cnn2 article preprocessing phase
sonya_cnn2 <- "https://www.cnn.com/2024/07/23/us/sonya-massey-police-shooting-what-went-wrong/index.html" 
sonyacnn2page <- read_html(sonya_cnn2) 
text_data_cnn2 <- html_text(html_nodes(sonyacnn2page, "p")) # Extract all text within the <p> tags
print(text_data_cnn2)  #Print the extracted text
#make corpus
corpus_sonyacnn2 <- corpus(text_data_cnn2)
corpus_sonyacnn2 <- # REMOVING LAST THREE TEXTS DUE TO CONTENT
  corpus_subset(corpus_sonyacnn2, 
                !(docnames(corpus_sonyacnn2) %in% c("text57", "text58")))
tail(corpus_sonyacnn2)
print(corpus_sonyacnn2)
summary(corpus_sonyacnn2)
head(docvars(corpus_sonyacnn2))
#token
sonya_tokencnn2 <- tokens(corpus_sonyacnn2) #tokenized the pdf text
sonya_nonpunc_cnn2 <- tokens(sonya_tokencnn2, remove_punct = TRUE) #remove punctuation
print(sonya_nonpunc_cnn2)
sonya_nostop_cnn2 <- tokens_select(sonya_nonpunc_cnn2, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_cnn2)
cnn_custom_stop_words <- c("said", "says")
sonya_custom_cnn2 <- tokens_select(sonya_nonpunc_cnn2, pattern = c((stopwords("en")),(cnn_custom_stop_words)), selection = "remove") #remove custom words
print(sonya_custom_cnn2)
#dataframe
sonya.dfm.cnn2 <- dfm(sonya_nostop_cnn2) #put the text into a dataframe matrix
sonya.cnn2.frequency <- topfeatures(sonya.dfm.cnn2, n = 10) #asked for the top 10 words in article
print(sonya.cnn2.frequency)
sonya.cnn2.dfm.custom <- dfm(sonya_custom_cnn2)
sonya.cnn2.frequency.custom <- topfeatures(sonya.cnn2.dfm.custom, n = 10)
print(sonya.cnn2.frequency.custom)
#keywords
kw_water_cnn2 <- kwic(sonya_tokencnn2, pattern =  "water") #searching for the word in association with other words
print(kw_water_cnn2)
kw_rebuke_cnn2 <- kwic(sonya_tokencnn2, pattern =  "rebuke")
print(kw_rebuke_cnn2)
kw_justification_cnn2 <- kwic(sonya_tokencnn2, pattern = phrase("boiling water*")) #phrase meaning
print(kw_justification_cnn2)
sonya_threat_cnn2 <- tokens_select(sonya_nostop_cnn2, pattern = c("speak*","deputy*"), padding = TRUE, window = 5) #view words associated with key words
print(sonya_threat_cnn2)

#cnn2 removed comments on all articles regarding sonya massey

#fox1 preprocessing phase
sonya_fox1 <- "https://www.foxnews.com/us/bodycam-video-reveals-chaotic-scene-deputy-fatally-shooting-sonya-massey-called-911-help" 
sonya_fox1_page <- read_html(sonya_fox1) 
text_data_fox1 <- html_text(html_nodes(sonya_fox1_page, "p")) # Extract all text within the <p> tags
print(text_data_fox1)
#make corpus
corpus_sonyafox1 <- corpus(text_data_fox1)
corpus_sonyafox1 <- # REMOVING LAST THREE TEXTS DUE TO CONTENT
  corpus_subset(corpus_sonyafox1, 
                !(docnames(corpus_sonyafox1) %in% c(paste("text",33:42,sep=""))))
tail(corpus_sonyafox1)
print(corpus_sonyafox1)
summary(corpus_sonyafox1)
head(docvars(corpus_sonyafox1))
#token
sonya_token_fox1 <- tokens(corpus_sonyafox1) #tokenized the pdf text
sonya_nonpunc_fox1 <- tokens(text_data_fox1, remove_punct = TRUE) #remove punctuation
print(sonya_nonpunc_fox1)
sonya_nostop_fox1 <- tokens_select(sonya_nonpunc_fox1, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_fox1)
fox_custom_stop_words <- c("said", "says","material","may","published","broadcast","rewritten","redistributed","2025","fox","news","network","llc","rights","reserved","quotes","displayed","real-time","delayed","least","15","foxnews.com","app","newsletter","successfully","subscribed","minutes","market","data","provided","factset","powered","etf","refinitiv","lipper","30","postal","say","36")
sonya_custom_fox1 <- tokens_select(sonya_nonpunc_fox1, pattern = c((stopwords("en")),(fox_custom_stop_words)), selection = "remove") #remove custom words
print(sonya_custom_fox1)
#dataframe
sonya.dfm.fox1 <- dfm(sonya_nostop_fox1) #put the text into a dataframe matrix
sonya.fox1.frequency <- topfeatures(sonya.dfm.fox1, n = 15) #asked for the top 10 words in article
print(sonya.fox1.frequency)
sonya.fox1.dfm.custom <- dfm(sonya_custom_fox1)
sonya.fox1.frequency.custom <- topfeatures(sonya.fox1.dfm.custom, n = 15)
print(sonya.fox1.frequency.custom)
#table
dataframe.fox1 <- data.frame(sonya.fox1.frequency.custom)
colnames(dataframe.fox1) <- "frequency"
view(dataframe.fox1)
dataframe.fox1.organized <- tibble::rownames_to_column(dataframe.fox1, var = "topwords")
view(dataframe.fox1.organized)
dataframe.fox1.categories <- dataframe.fox1.organized %>%
  mutate(Group = case_when(
    topwords %in% c("massey", "water", "pot", "black") ~ "victim",  # For A, B, C -> "first"
    topwords %in% c("grayson", "officer", "deputy","police") ~ "officer",   # For X, Y, Z -> "last"
    TRUE ~ "other"                            # Default case
  )) 
view(dataframe.fox1.categories)
filter.fox1.categories <- filter(dataframe.fox1.categories) #filter variables
filter.fox1.categories 
view(filter.fox1.categories)
#semantic analysis 
fox_custom_key_words <- c("sonya","massey","grayson","officer","kill*","justified*","just*","fault*","cop*","officer*","violation*","policy","evidence*","character*","value*","interpret*")
sonya_key_custom_fox1 <- tokens_select(sonya_nostop_fox1, pattern = fox_custom_key_words) #choose keywords to analyze
sonya.dfm.fox1.key <- dfm(sonya_key_custom_fox1)
sonya.fox1.lsa <- textmodel_lsa(sonya.fox1.dfm.custom)
sonya.fox1.lsa$matrix_low_rank
sonya.fox1.lsa$features
dataframe.fox1.lsa.features <- data.frame(sonya.fox1.lsa$features)
view(dataframe.fox1.lsa.features)
barplot(sonya.fox1.lsa$features)
network.sonya.fox1 <- textplot_network(sonya.dfm.fox1.key)
sonya.fox1.textstat <- quanteda.textstats::textstat_keyness(sonya.fox1.dfm.custom)
textplot_keyness(sonya.fox1.textstat, show_reference = TRUE, show_legend = TRUE, n=20L, min_count = 2L, margin = 0.05, color = c("gray","darkblue"), labelcolor = "gray30", labelsize = 4, font = NULL)
textplot_keyness()
textplot_influence()
predict.fox1 <- predict(sonya.fox1.lsa)
predict.fox1
#keywords
kw_water_fox1 <- kwic(sonya_token_fox1, pattern =  "water") #searching for the word in association with other words
print(kw_water_fox1)
kw_rebuke_fox1 <- kwic(sonya_token_fox1, pattern =  "rebuke")
print(kw_rebuke_fox1)
kw_justification_fox1 <- kwic(sonya_token_fox1, pattern = phrase("boiling water*")) #phrase meaning
print(kw_justification_fox1)
sonya_nostop_fox1 <- tokens_select(sonya_nonpunc_fox1, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_fox1)
sonya_threat_fox1 <- tokens_select(sonya_nostop_fox1, pattern = c("speak*","deputy*"), padding = TRUE, window = 5) #view words associated with key words
print(sonya_threat_fox1)
kw_fox1 <- kwic((sonya_token_fox1), pattern = c("water","rebuke","boil*","speak*","deputy*","cop*","because*","just*"))

#fox 1 comments preprocessing
sonya_fox1_comments <- "C:/Users/kadej/Dropbox/Quanteda/fox1comments.txt"
sonya_fox1_page_comments <- readtext(sonya_fox1_comments) 
print(sonya_fox1_page_comments)
#Generate Corpus
corpus_sonya_fox1_comments <- (corpus(sonya_fox1_page_comments)) #make corpus
print(corpus_sonya_fox1_comments)
summary(corpus_sonya_fox1_comments)
head(docvars(corpus_sonya_fox1_comments))
#token
sonya_token_fox1_comments <- tokens(corpus_sonya_fox1_comments) #tokenized the pdf text
sonya_nonpunc_fox1_comments <- tokens(corpus_sonya_fox1_comments, remove_punct = TRUE, remove_numbers = TRUE) #remove punctuation and numbers
print(sonya_nonpunc_fox1_comments)
sonya_nostop_fox1_comments <- tokens_select(sonya_nonpunc_fox1_comments, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_fox1_comments)
fox1_comments_custom_stop_words <- c("reply","july","replies", "share","see")
sonya_custom_fox1_comments <- tokens_select(sonya_nonpunc_fox1_comments, pattern = c((stopwords("en")),(fox1_comments_custom_stop_words)), selection = "remove") #remove custom words
print(sonya_custom_fox1_comments)
#dataframe
sonya.dfm.fox1.comments <- dfm(sonya_nostop_fox1_comments) #put the text into a dataframe matrix
sonya.dfm.fox1.comments
sonya.fox1.comments.frequency <- topfeatures(sonya.dfm.fox1.comments, n = 10) #asked for the top 10 words in article
print(sonya.fox1.comments.frequency)
sonya.fox1.comments.dfm.custom <- dfm(sonya_custom_fox1_comments)
sonya.fox1.comments.frequency.custom <- topfeatures(sonya.fox1.comments.dfm.custom, n = 10)
print(sonya.fox1.comments.frequency.custom)
#table
dataframe.fox1.comments <- data.frame(sonya.fox1.comments.frequency.custom)
colnames(dataframe.fox1.comments) <- "frequency"
view(dataframe.fox1.comments)
dataframe.fox1.comments.organized <- tibble::rownames_to_column(dataframe.fox1.comments, var = "topwords")
view(dataframe.fox1.comments.organized)
dataframe.fox1.comments.categories <- dataframe.fox1.comments.organized %>%
  mutate(Group = case_when(
    topwords %in% c("massey", "water", "pot", "black") ~ "victim",  # For A, B, C -> "first"
    topwords %in% c("grayson", "officer", "deputy","police") ~ "officer",   # For X, Y, Z -> "last"
    TRUE ~ "other"                            # Default case
  )) 
view(dataframe.fox1.comments.categories)
filter.fox1.comments.categories <- filter(dataframe.fox1.comments.categories) #filter variables
filter.fox1.comments.categories 
view(filter.fox1.comments.categories)
#semantic analysis
fox_custom_key_words <- c("sonya","massey","grayson","officer","kill*","justified*","just*","fault*","cop*","officer*","violation*","policy","evidence*","character*","value*","interpret*")
sonya_key_custom_fox1_comments <- tokens_select(sonya_nostop_fox1_comments, pattern = fox_custom_key_words) #choose keywords to analyze
sonya.dfm.fox1.comments.key <- dfm(sonya_key_custom_fox1_comments)
sonya.dfm.fox1.comments.key
sonya.fox1.comments.lsa <- textmodel_lsa(sonya.dfm.fox1.comments.key)
sonya.fox1.comments.lsa$matrix_low_rank
sonya.fox1.comments.lsa$features
dataframe.fox1.comments.lsa.features <- data.frame(sonya.fox1.comments.lsa$features)
view(dataframe.fox1.comments.lsa.features)
barplot(sonya.fox1.comments.lsa$features)
network.sonya.comments.fox1 <- textplot_network(sonya.dfm.fox1.comments.key)
sonya.fox1.comments.textstat <- quanteda.textstats::textstat_keyness(sonya.fox1.comments.dfm.custom)
textplot_keyness(sonya.fox1.comments.textstat, show_reference = TRUE, show_legend = TRUE, n=20L, min_count = 2L, margin = 0.05, color = c("gray","darkblue"), labelcolor = "gray30", labelsize = 4, font = NULL)
#keywords
kw_water_fox1_comments <- kwic(sonya_token_fox1_comments, pattern =  "water") #searching for the word in association with other words
print(kw_water_fox1_comments)
kw_rebuke_fox1_comments <- kwic(sonya_token_fox1_comments, pattern =  "rebuke")
print(kw_rebuke_fox1_comments)
kw_justification_fox1_comments <- kwic(sonya_token_fox1_comments, pattern = phrase("boiling water*")) #phrase meaning
print(kw_justification_fox1_comments)
sonya_nostop_fox1_comments <- tokens_select(sonya_nonpunc_fox1_comments, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_fox1)
sonya_threat_fox1_comments <- tokens_select(sonya_nostop_fox1_comments, pattern = c("speak*","deputy*"), padding = TRUE, window = 5) #view words associated with key words
print(sonya_threat_fox1_comments)

#fox 2 article preprocessing
sonya_fox2 <- "https://www.foxnews.com/sports/yankees-marcus-stroman-speaks-out-following-fatal-shooting-sonya-massey-sad-society-were-living-in" 
sonya_fox2_page <- read_html(sonya_fox2) 
text_data_fox2 <- html_text(html_nodes(sonya_fox2_page, "p")) # Extract all text within the <p> tags
print(text_data_fox2)
#make corpus
corpus_sonyafox2 <- corpus(text_data_fox2)
corpus_sonyafox2 <- # REMOVING LAST THREE TEXTS DUE TO CONTENT
  corpus_subset(corpus_sonyafox2, 
                !(docnames(corpus_sonyafox2) %in% c(paste("text",1:2,sep=""),paste("text",19:28,sep=""))))
tail(corpus_sonyafox2)
head(corpus_sonyafox2)
print(corpus_sonyafox2)
summary(corpus_sonyafox2)
head(docvars(corpus_sonyafox2))
#token
sonya_token_fox2 <- tokens(corpus_sonyafox2) #tokenized the pdf text
sonya_nonpunc_fox2 <- tokens(text_data_fox2, remove_punct = TRUE) #remove punctuation
print(sonya_nonpunc_fox2)
sonya_nostop_fox2 <- tokens_select(sonya_nonpunc_fox2, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_fox2)
fox_custom_stop_words <- c("said", "says")
sonya_custom_fox2 <- tokens_select(sonya_nonpunc_fox2, pattern = c((stopwords("en")),(fox_custom_stop_words)), selection = "remove") #remove custom words
print(sonya_custom_fox2)
#dataframe
sonya.dfm.fox2 <- dfm(sonya_nostop_fox2) #put the text into a dataframe matrix
sonya.fox2.frequency <- topfeatures(sonya.dfm.fox2, n = 10) #asked for the top 10 words in article
print(sonya.fox2.frequency)
sonya.fox2.dfm.custom <- dfm(sonya_custom_fox2)
sonya.fox2.frequency.custom <- topfeatures(sonya.fox2.dfm.custom, n = 10)
print(sonya.fox2.frequency.custom)
#keywords
kw_water_fox2 <- kwic(sonya_token_fox2, pattern =  "water") #searching for the word in association with other words
print(kw_water_fox2)
kw_rebuke_fox2 <- kwic(sonya_token_fox2, pattern =  "rebuke")
print(kw_rebuke_fox2)
kw_justification_fox2 <- kwic(sonya_token_fox2, pattern = phrase("boiling water*")) #phrase meaning
print(kw_justification_fox2)
sonya_nostop_fox2 <- tokens_select(sonya_nonpunc_fox2, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_fox2)
sonya_threat_fox2 <- tokens_select(sonya_nostop_fox2, pattern = c("speak*","deputy*"), padding = TRUE, window = 5) #view words associated with key words
print(sonya_threat_fox2)

#fox 2 comments
sonya_fox2_comments <- "C:/Users/kadej/Dropbox/Quanteda/fox2comments.txt"
sonya_fox2_page_comments <- readtext(sonya_fox2_comments) 
print(sonya_fox2_page_comments)
#Generate Corpus
corpus_sonya_fox2_comments <- corpus(sonya_fox2_page_comments) #make corpus
print(corpus_sonya_fox2_comments)
summary(corpus_sonya_fox2_comments)
head(docvars(corpus_sonya_fox2_comments))
#token
sonya_token_fox2_comments <- tokens(corpus_sonya_fox2_comments) #tokenized the pdf text
sonya_nonpunc_fox2_comments <- tokens(corpus_sonya_fox2_comments, remove_punct = TRUE, remove_numbers = TRUE) #remove punctuation and numbers
print(sonya_nonpunc_fox2_comments)
sonya_nostop_fox2_comments <- tokens_select(sonya_nonpunc_fox2_comments, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_fox2_comments)
fox2_comments_custom_stop_words <- c("reply","july","replies", "share","see")
sonya_custom_fox2_comments <- tokens_select(sonya_nonpunc_fox2_comments, pattern = c((stopwords("en")),(fox1_comments_custom_stop_words)), selection = "remove") #remove custom words
print(sonya_custom_fox2_comments)
#dataframe
sonya.dfm.fox2.comments <- dfm(sonya_nostop_fox2_comments) #put the text into a dataframe matrix
sonya.fox2.comments.frequency <- topfeatures(sonya.dfm.fox2.comments, n = 10) #asked for the top 10 words in article
print(sonya.fox2.comments.frequency)
sonya.fox2.comments.dfm.custom <- dfm(sonya_custom_fox2_comments)
sonya.fox2.comments.frequency.custom <- topfeatures(sonya.fox2.comments.dfm.custom, n = 10)
print(sonya.fox2.comments.frequency.custom)
#keywords
kw_water_fox1_comments <- kwic(sonya_token_fox1_comments, pattern =  "water") #searching for the word in association with other words
print(kw_water_fox1_comments)
kw_rebuke_fox1_comments <- kwic(sonya_token_fox1_comments, pattern =  "rebuke")
print(kw_rebuke_fox1_comments)
kw_justification_fox1_comments <- kwic(sonya_token_fox1_comments, pattern = phrase("boiling water*")) #phrase meaning
print(kw_justification_fox1_comments)
sonya_nostop_fox1_comments <- tokens_select(sonya_nonpunc_fox1_comments, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_fox1)
sonya_threat_fox1_comments <- tokens_select(sonya_nostop_fox1_comments, pattern = c("speak*","deputy*"), padding = TRUE, window = 5) #view words associated with key words
print(sonya_threat_fox1_comments)

#wapo1 preprocessing phase
sonya_wapo1 <- "C:/Users/kadej/Dropbox/Quanteda/sonyamassey_wapo1.txt"
sonya_wapo1_page <- readtext(sonya_wapo1) 
print(sonya_wapo1_page)
#Generate Corpus
corpus_sonya_wapo1 <- corpus(sonya_wapo1_page) #make corpus
print(corpus_sonya_wapo1)
summary(corpus_sonya_wapo1)
head(docvars(corpus_sonya_wapo1))
#token
sonya_token_wapo1 <- tokens(corpus_sonya_wapo1) #tokenized the pdf text
sonya_nonpunc_wapo1 <- tokens(corpus_sonya_wapo1, remove_punct = TRUE) #remove punctuation
print(sonya_nonpunc_wapo1)
sonya_nostop_wapo1 <- tokens_select(sonya_nonpunc_wapo1, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_wapo1)
wapo_custom_stop_words <- c("said", "says")
sonya_custom_wapo1 <- tokens_select(sonya_nonpunc_wapo1, pattern = c((stopwords("en")),(wapo_custom_stop_words)), selection = "remove") #remove custom words
print(sonya_custom_wapo1)
#dataframe
sonya.dfm.wapo1 <- dfm(sonya_nostop_wapo1) #put the text into a dataframe matrix
sonya.wapo1.frequency <- topfeatures(sonya.dfm.wapo1, n = 10) #asked for the top 10 words in article
print(sonya.wapo1.frequency)
sonya.wapo1.dfm.custom <- dfm(sonya_custom_wapo1)
sonya.wapo1.frequency.custom <- topfeatures(sonya.wapo1.dfm.custom, n = 10)
print(sonya.wapo1.frequency.custom)
#keyword
kw_water_wapo1 <- kwic(sonya_token_wapo1, pattern =  "water") #searching for the word in association with other words
print(kw_water_wapo1)
kw_justification_wapo1 <- kwic(sonya_token_wapo1, pattern = phrase("boiling water*")) #phrase meaning
print(kw_justification_wapo1)
sonya_nostop_wapo1 <- tokens_select(sonya_nonpunc_wapo1, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_wapo1)
sonya_threat_wapo1 <- tokens_select(sonya_nostop_wapo1, pattern = c("speak*","deputy*"), padding = TRUE, window = 5) #view words associated with key words
print(sonya_threat_wapo1)

#wapo1 comments
sonya_wapo1_comments <- "C:/Users/kadej/Dropbox/Quanteda/wapo1comments.txt"
sonya_wapo1_page_comments <- readtext(sonya_wapo1_comments) 
print(sonya_wapo1_page_comments)
#Generate Corpus
corpus_sonya_wapo1_comments <- corpus(sonya_wapo1_page_comments) #make corpus
print(corpus_sonya_wapo1_comments)
summary(corpus_sonya_wapo1_comments)
head(docvars(corpus_sonya_wapo1_comments))
#token
sonya_token_wapo1_comments <- tokens(corpus_sonya_wapo1_comments) #tokenized the pdf text
sonya_nonpunc_wapo1_comments <- tokens(corpus_sonya_wapo1_comments, remove_punct = TRUE, remove_numbers = TRUE) #remove punctuation and numbers
print(sonya_nonpunc_wapo1_comments)
sonya_nostop_wapo1_comments <- tokens_select(sonya_nonpunc_wapo1_comments, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_wapo1_comments)
wapo1_comments_custom_stop_words <- c("reply","july","replies", "share","comments","recommended","expand_more","wrre","like")
sonya_custom_wapo1_comments <- tokens_select(sonya_nonpunc_wapo1_comments, pattern = c((stopwords("en")),(wapo1_comments_custom_stop_words)), selection = "remove") #remove custom words
print(sonya_custom_wapo1_comments)
#dataframe
sonya.dfm.wapo1.comments <- dfm(sonya_nostop_wapo1_comments) #put the text into a dataframe matrix
sonya.wapo1.comments.frequency <- topfeatures(sonya.dfm.wapo1.comments, n = 10) #asked for the top 10 words in article
print(sonya.wapo1.comments.frequency)
sonya.wapo1.comments.dfm.custom <- dfm(sonya_custom_wapo1_comments)
sonya.wapo1.comments.frequency.custom <- topfeatures(sonya.wapo1.comments.dfm.custom, n = 10)
print(sonya.wapo1.comments.frequency.custom)

#wapo 2 article preprocessing
sonya_wapo2 <- "C:/Users/kadej/Dropbox/Quanteda/sonyamassey_wapo2.txt"
sonya_wapo2_page <- readtext(sonya_wapo2) 
print(sonya_wapo2_page)
#Generate Corpus
corpus_sonya_wapo2 <- corpus(sonya_wapo2_page) #make corpus
print(corpus_sonya_wapo2)
summary(corpus_sonya_wapo2)
head(docvars(corpus_sonya_wapo2))
#token
sonya_token_wapo2 <- tokens(corpus_sonya_wapo2) #tokenized the pdf text
sonya_nonpunc_wapo2 <- tokens(corpus_sonya_wapo2, remove_punct = TRUE) #remove punctuation
print(sonya_nonpunc_wapo2)
sonya_nostop_wapo2 <- tokens_select(sonya_nonpunc_wapo2, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_wapo2)
wapo_custom_stop_words <- c("said", "says")
sonya_custom_wapo2 <- tokens_select(sonya_nonpunc_wapo2, pattern = c((stopwords("en")),(wapo_custom_stop_words)), selection = "remove") #remove custom words
print(sonya_custom_wapo2)
#dataframe
sonya.dfm.wapo2 <- dfm(sonya_nostop_wapo2) #put the text into a dataframe matrix
sonya.wapo2.frequency <- topfeatures(sonya.dfm.wapo2, n = 10) #asked for the top 10 words in article
print(sonya.wapo2.frequency)
sonya.wapo2.dfm.custom <- dfm(sonya_custom_wapo2)
sonya.wapo2.frequency.custom <- topfeatures(sonya.wapo2.dfm.custom, n = 10)
print(sonya.wapo2.frequency.custom)
#keyword
kw_water_wapo2 <- kwic(sonya_token_wapo2, pattern =  "water") #searching for the word in association with other words
print(kw_water_wapo2)
kw_justification_wapo2 <- kwic(sonya_token_wapo2, pattern = phrase("boiling water*")) #phrase meaning
print(kw_justification_wapo2)
sonya_nostop_wapo2 <- tokens_select(sonya_nonpunc_wapo2, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_wapo2)
sonya_threat_wapo2 <- tokens_select(sonya_nostop_wapo2, pattern = c("speak*","deputy*"), padding = TRUE, window = 5) #view words associated with key words
print(sonya_threat_wapo2)

#wapo2 comments
sonya_wapo2_comments <- "C:/Users/kadej/Dropbox/Quanteda/wapo2comments.txt"
sonya_wapo2_page_comments <- readtext(sonya_wapo2_comments) 
print(sonya_wapo2_page_comments)
#Generate Corpus
corpus_sonya_wapo2_comments <- corpus(sonya_wapo2_page_comments) #make corpus
print(corpus_sonya_wapo2_comments)
summary(corpus_sonya_wapo2_comments)
head(docvars(corpus_sonya_wapo2_comments))
#token
sonya_token_wapo2_comments <- tokens(corpus_sonya_wapo2_comments) #tokenized the pdf text
sonya_nonpunc_wapo2_comments <- tokens(corpus_sonya_wapo2_comments, remove_punct = TRUE, remove_numbers = TRUE) #remove punctuation and numbers
print(sonya_nonpunc_wapo2_comments)
sonya_nostop_wapo2_comments <- tokens_select(sonya_nonpunc_wapo2_comments, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_wapo2_comments)
wapo2_comments_custom_stop_words <- c("reply","july","replies", "share","comments","recommended","expand_more","wrre")
sonya_custom_wapo2_comments <- tokens_select(sonya_nonpunc_wapo2_comments, pattern = c((stopwords("en")),(wapo2_comments_custom_stop_words)), selection = "remove") #remove custom words
print(sonya_custom_wapo2_comments)
#dataframe
sonya.dfm.wapo2.comments <- dfm(sonya_nostop_wapo2_comments) #put the text into a dataframe matrix
sonya.wapo2.comments.frequency <- topfeatures(sonya.dfm.wapo2.comments, n = 10) #asked for the top 10 words in article
print(sonya.wapo2.comments.frequency)
sonya.wapo2.comments.dfm.custom <- dfm(sonya_custom_wapo2_comments)
sonya.wapo2.comments.frequency.custom <- topfeatures(sonya.wapo2.comments.dfm.custom, n = 10)
print(sonya.wapo2.comments.frequency.custom)

#abc1 preprocessing
sonya_abc1 <- "https://abcnews.go.com/US/illinois-woman-dies-after-shot-deputy-involved-incident/story?id=111880175" 
sonya_abc1_page <- read_html(sonya_abc1) 
text_data_abc1 <- html_text(html_nodes(sonya_abc1_page, "p")) # Extract all text within the <p> tags
print(text_data_abc1)
#make corpus
corpus_sonyaabc1 <- corpus(text_data_abc1)
corpus_sonyaabc1 <- # REMOVING LAST THREE TEXTS DUE TO CONTENT
  corpus_subset(corpus_sonyaabc1, 
                !(docnames(corpus_sonyaabc1) %in% c("text16")))
tail(corpus_sonyaabc1)
head(corpus_sonyaabc1)
print(corpus_sonyaabc1)
summary(corpus_sonyaabc1)
head(docvars(corpus_sonyaabc1))
#token
sonya_token_abc1 <- tokens(corpus_sonyaabc1) #tokenized the pdf text
sonya_nonpunc_abc1 <- tokens(text_data_abc1, remove_punct = TRUE) #remove punctuation
print(sonya_nonpunc_abc1)
sonya_nostop_abc1 <- tokens_select(sonya_nonpunc_abc1, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_abc1)
abc_custom_stop_words <- c("statement", "news","abc")
sonya_custom_abc1 <- tokens_select(sonya_nonpunc_abc1, pattern = c((stopwords("en")),(abc_custom_stop_words)), selection = "remove") #remove custom words
print(sonya_custom_abc1)
#dataframe
sonya.dfm.abc1 <- dfm(sonya_nostop_abc1) #put the text into a dataframe matrix
sonya.abc1.frequency <- topfeatures(sonya.dfm.abc1, n = 10) #asked for the top 10 words in article
print(sonya.abc1.frequency)
sonya.abc1.dfm.custom <- dfm(sonya_custom_abc1)
sonya.abc1.frequency.custom <- topfeatures(sonya.abc1.dfm.custom, n = 10)
print(sonya.abc1.frequency.custom)
#keywords
kw_water_abc1 <- kwic(sonya_token_abc1, pattern =  "water") #searching for the word in association with other words
print(kw_water_abc1)
kw_rebuke_abc1 <- kwic(sonya_token_abc1, pattern =  "rebuke")
print(kw_rebuke_abc1)
kw_justification_abc1 <- kwic(sonya_token_abc1, pattern = phrase("boiling water*")) #phrase meaning
print(kw_justification_abc1)
sonya_nostop_abc1 <- tokens_select(sonya_nonpunc_abc1, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_abc1)
sonya_threat_abc1 <- tokens_select(sonya_nostop_abc1, pattern = c("said*","officer*"), padding = TRUE, window = 5) #view words associated with key words
print(sonya_threat_abc1)

#abc 1 comments not available due to suspension in August 2023

#abc 2 article
sonya_abc2 <- "https://abcnews.go.com/US/deputy-fatally-shot-sonya-massey-discharged-army-misconduct/story?id=112264355" 
sonya_abc2_page <- read_html(sonya_abc2) 
text_data_abc2 <- html_text(html_nodes(sonya_abc2_page, "p")) # Extract all text within the <p> tags
print(text_data_abc2)
#make corpus
corpus_sonyaabc2 <- corpus(text_data_abc2)
corpus_sonyaabc2 <- # REMOVING LAST THREE TEXTS DUE TO CONTENT
  corpus_subset(corpus_sonyaabc2, 
                !(docnames(corpus_sonyaabc2) %in% c("text28")))
tail(corpus_sonyaabc2)
head(corpus_sonyaabc2)
print(corpus_sonyaabc2)
summary(corpus_sonyaabc2)
head(docvars(corpus_sonyaabc2))
#token
sonya_token_abc2 <- tokens(corpus_sonyaabc2) #tokenized the pdf text
sonya_nonpunc_abc2 <- tokens(text_data_abc2, remove_punct = TRUE) #remove punctuation
print(sonya_nonpunc_abc2)
sonya_nostop_abc2 <- tokens_select(sonya_nonpunc_abc2, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_abc2)
abc_custom_stop_words <- c("statement", "news","abc")
sonya_custom_abc2 <- tokens_select(sonya_nonpunc_abc2, pattern = c((stopwords("en")),(abc_custom_stop_words)), selection = "remove") #remove custom words
print(sonya_custom_abc2)
#dataframe
sonya.dfm.abc2 <- dfm(sonya_nostop_abc2) #put the text into a dataframe matrix
sonya.abc2.frequency <- topfeatures(sonya.dfm.abc2, n = 10) #asked for the top 10 words in article
print(sonya.abc2.frequency)
sonya.abc2.dfm.custom <- dfm(sonya_custom_abc2)
sonya.abc2.frequency.custom <- topfeatures(sonya.abc2.dfm.custom, n = 10)
print(sonya.abc2.frequency.custom)
#keywords
kw_water_abc2 <- kwic(sonya_token_abc2, pattern =  "water") #searching for the word in association with other words
print(kw_water_abc2)
kw_rebuke_abc2 <- kwic(sonya_token_abc2, pattern =  "rebuke")
print(kw_rebuke_abc2)
kw_justification_abc2 <- kwic(sonya_token_abc2, pattern = phrase("boiling water*")) #phrase meaning
print(kw_justification_abc2)
sonya_nostop_abc2 <- tokens_select(sonya_nonpunc_abc2, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_abc2)
sonya_threat_abc2 <- tokens_select(sonya_nostop_abc2, pattern = c("said*","officer*"), padding = TRUE, window = 5) #view words associated with key words
print(sonya_threat_abc2)

#nbc1 pre-processing 
sonya_nbc1 <- "https://www.nbcnews.com/news/us-news/illinois-woman-called-police-possible-intruder-killed-deputies-attorne-rcna161673"
sonya_nbc1_page <- read_html(sonya_nbc1) 
text_data_nbc1 <- html_text(html_nodes(sonya_nbc1_page, "p")) # Extract all text within the <p> tags
print(text_data_nbc1)
#make corpus
corpus_sonyanbc1 <- corpus(text_data_nbc1)
corpus_sonyanbc1 <- # REMOVING LAST THREE TEXTS DUE TO CONTENT
  corpus_subset(corpus_sonyanbc1, 
                !(docnames(corpus_sonyanbc1) %in% c(paste("text",1:10,sep=""),paste("text",26:27,sep=""))))
tail(corpus_sonyanbc1)
head(corpus_sonyanbc1)
print(corpus_sonyanbc1)
summary(corpus_sonyanbc1)
head(docvars(corpus_sonyanbc1))
#token
sonya_token_nbc1 <- tokens(corpus_sonyanbc1) #tokenized the pdf text
sonya_nonpunc_nbc1 <- tokens(text_data_nbc1, remove_punct = TRUE) #remove punctuation
print(sonya_nonpunc_nbc1)
sonya_nostop_nbc1 <- tokens_select(sonya_nonpunc_nbc1, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_nbc1)
nbc_custom_stop_words <- c("said")
sonya_custom_nbc1 <- tokens_select(sonya_nonpunc_nbc1, pattern = c((stopwords("en")),(nbc_custom_stop_words)), selection = "remove") #remove custom words
print(sonya_custom_nbc1)
#dataframe
sonya.dfm.nbc1 <- dfm(sonya_nostop_nbc1) #put the text into a dataframe matrix
sonya.nbc1.frequency <- topfeatures(sonya.dfm.nbc1, n = 10) #asked for the top 10 words in article
print(sonya.nbc1.frequency)
sonya.nbc1.dfm.custom <- dfm(sonya_custom_nbc1)
sonya.nbc1.frequency.custom <- topfeatures(sonya.nbc1.dfm.custom, n = 10)
print(sonya.nbc1.frequency.custom)
#keyword
kw_water_nbc1 <- kwic(sonya_token_nbc1, pattern =  "just*") #searching for the word in association with other words
print(kw_water_nbc1)
kw_rebuke_nbc1 <- kwic(sonya_token_nbc1, pattern =  "Grayson")
print(kw_rebuke_nbc1)
kw_justification_nbc1 <- kwic(sonya_token_nbc1, pattern = phrase("kill*")) #phrase meaning
print(kw_justification_nbc1)
sonya_nostop_nbc1 <- tokens_select(sonya_nonpunc_nbc1, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_nbc1)
sonya_threat_nbc1 <- tokens_select(sonya_nostop_nbc1, pattern = c("threat*"), padding = TRUE, window = 5) #view words associated with key words
print(sonya_threat_nbc1)

#nbc comments have been removed as of 2020 following George Floydd

#NBC Article 2 
sonya_nbc2 <- "https://www.nbcnews.com/news/us-news/charges-filed-illinois-deputy-death-sonya-massey-rcna162456"
sonya_nbc2_page <- read_html(sonya_nbc2) 
text_data_nbc2 <- html_text(html_nodes(sonya_nbc2_page, "p")) # Extract all text within the <p> tags
print(text_data_nbc2)
#make corpus
corpus_sonyanbc2 <- corpus(text_data_nbc2)
corpus_sonyanbc2 <- # REMOVING LAST THREE TEXTS DUE TO CONTENT
  corpus_subset(corpus_sonyanbc2, 
                !(docnames(corpus_sonyanbc2) %in% c(paste("text",1:11,sep=""),paste("text",36:38,sep=""))))
tail(corpus_sonyanbc2)
head(corpus_sonyanbc2)
print(corpus_sonyanbc2)
summary(corpus_sonyanbc2)
head(docvars(corpus_sonyanbc2))
#token
sonya_token_nbc2 <- tokens(corpus_sonyanbc2) #tokenized the pdf text
sonya_nonpunc_nbc2 <- tokens(text_data_nbc2, remove_punct = TRUE) #remove punctuation
print(sonya_nonpunc_nbc2)
sonya_nostop_nbc2 <- tokens_select(sonya_nonpunc_nbc2, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_nbc2)
nbc_custom_stop_words <- c("said")
sonya_custom_nbc2 <- tokens_select(sonya_nonpunc_nbc2, pattern = c((stopwords("en")),(nbc_custom_stop_words)), selection = "remove") #remove custom words
print(sonya_custom_nbc2)
#dataframe
sonya.dfm.nbc2 <- dfm(sonya_nostop_nbc2) #put the text into a dataframe matrix
sonya.nbc2.frequency <- topfeatures(sonya.dfm.nbc2, n = 10) #asked for the top 10 words in article
print(sonya.nbc2.frequency)
sonya.nbc2.dfm.custom <- dfm(sonya_custom_nbc2)
sonya.nbc2.frequency.custom <- topfeatures(sonya.nbc2.dfm.custom, n = 10)
print(sonya.nbc2.frequency.custom)
#keyword
kw_water_nbc2 <- kwic(sonya_token_nbc2, pattern =  "just*") #searching for the word in association with other words
print(kw_water_nbc2)
kw_rebuke_nbc2 <- kwic(sonya_token_nbc2, pattern =  "Grayson")
print(kw_rebuke_nbc2)
kw_justification_nbc2 <- kwic(sonya_token_nbc2, pattern = phrase("kill*")) #phrase meaning
print(kw_justification_nbc2)
sonya_nostop_nbc2 <- tokens_select(sonya_nonpunc_nbc2, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_nbc2)
sonya_threat_nbc2 <- tokens_select(sonya_nostop_nbc2, pattern = c("threat*"), padding = TRUE, window = 5) #view words associated with key words
print(sonya_threat_nbc2)

#nytimes1 preprocessing phase
sonya_nytimes1 <- "C:/Users/kadej/Dropbox/Quanteda/sonyamassey_nytimes.txt"
sonya_nytimes1_page <- readtext(sonya_nytimes1) 
print(sonya_nytimes1_page)
#Generate Corpus
corpus_sonyanytimes1 <- corpus(sonya_nytimes1_page) #make corpus
print(corpus_sonyanytimes1)
summary(corpus_sonyanytimes1)
head(docvars(corpus_sonyanytimes1))
#token
sonya_token_nytimes1 <- tokens(corpus_sonyanytimes1) #tokenized the pdf text
sonya_nonpunc_nytimes1 <- tokens(corpus_sonyanytimes1, remove_punct = TRUE) #remove punctuation
print(sonya_nonpunc_nytimes1)
sonya_nostop_nytimes1 <- tokens_select(sonya_nonpunc_nytimes1, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_nytimes1)
nytimes_custom_stop_words <- c("said","mr","ms")
sonya_custom_nytimes1 <- tokens_select(sonya_nonpunc_nytimes1, pattern = c((stopwords("en")),(nytimes_custom_stop_words)), selection = "remove") #remove custom words
print(sonya_custom_nytimes1)
#dataframe
sonya.dfm.nytimes1 <- dfm(sonya_nostop_nytimes1) #put the text into a dataframe matrix
sonya.nytimes1.frequency <- topfeatures(sonya.dfm.nytimes1, n = 10) #asked for the top 10 words in article
print(sonya.nytimes1.frequency)
sonya.nytimes1.dfm.custom <- dfm(sonya_custom_nytimes1)
sonya.nytimes1.frequency.custom <- topfeatures(sonya.nytimes1.dfm.custom, n = 10)
print(sonya.nytimes1.frequency.custom)
#keywords
kw_water_nytimes1 <- kwic(sonya_token_nytimes1, pattern =  "water") #searching for the word in association with other words
print(kw_water_nytimes1)
kw_justification_nytimes1 <- kwic(sonya_token_nytimes1, pattern = phrase("boiling water*")) #phrase meaning
print(kw_justification_nytimes1)
sonya_nostop_nytimes1 <- tokens_select(sonya_nonpunc_nytimes1, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_nytimes1)
sonya_threat_nytimes1 <- tokens_select(sonya_nostop_nytimes1, pattern = c("speak*","deputy*"), padding = TRUE, window = 5) #view words associated with key words
print(sonya_threat_nytimes1)

#nytimes1 comments have been closed

#nytimes 2 article preprocessing
sonya_nytimes2 <- "C:/Users/kadej/Dropbox/Quanteda/sonyamassey_nytimes2.txt"
sonya_nytimes2_page <- readtext(sonya_nytimes2) 
print(sonya_nytimes2_page)
#Generate Corpus
corpus_sonyanytimes2 <- corpus(sonya_nytimes2_page) #make corpus
print(corpus_sonyanytimes2)
summary(corpus_sonyanytimes2)
head(docvars(corpus_sonyanytimes2))
#token
sonya_token_nytimes2 <- tokens(corpus_sonyanytimes2) #tokenized the pdf text
sonya_nonpunc_nytimes2 <- tokens(corpus_sonyanytimes2, remove_punct = TRUE) #remove punctuation
print(sonya_nonpunc_nytimes2)
sonya_nostop_nytimes2 <- tokens_select(sonya_nonpunc_nytimes2, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_nytimes2)
nytimes_custom_stop_words <- c("said","mr","ms")
sonya_custom_nytimes2 <- tokens_select(sonya_nonpunc_nytimes2, pattern = c((stopwords("en")),(nytimes_custom_stop_words)), selection = "remove") #remove custom words
print(sonya_custom_nytimes2)
#dataframe
sonya.dfm.nytimes2 <- dfm(sonya_nostop_nytimes2) #put the text into a dataframe matrix
sonya.nytimes2.frequency <- topfeatures(sonya.dfm.nytimes2, n = 10) #asked for the top 10 words in article
print(sonya.nytimes2.frequency)
sonya.nytimes2.dfm.custom <- dfm(sonya_custom_nytimes2)
sonya.nytimes2.frequency.custom <- topfeatures(sonya.nytimes2.dfm.custom, n = 10)
print(sonya.nytimes2.frequency.custom)
#keywords
kw_water_nytimes2 <- kwic(sonya_token_nytimes2, pattern =  "water") #searching for the word in association with other words
print(kw_water_nytimes2)
kw_justification_nytimes2 <- kwic(sonya_token_nytimes2, pattern = phrase("boiling water*")) #phrase meaning
print(kw_justification_nytimes2)
sonya_nostop_nytimes2 <- tokens_select(sonya_nonpunc_nytimes2, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_nytimes2)
sonya_threat_nytimes2 <- tokens_select(sonya_nostop_nytimes2, pattern = c("speak*","deputy*"), padding = TRUE, window = 5) #view words associated with key words
print(sonya_threat_nytimes2)

#nytimes 2 comments
sonya_nytimes2_comments <- "C:/Users/kadej/Dropbox/Quanteda/nytimes2comments.txt"
sonya_nytimes2_page_comments <- readtext(sonya_nytimes2_comments) 
print(sonya_nytimes2_page_comments)
#Generate Corpus
corpus_sonya_nytimes2_comments <- corpus(sonya_nytimes2_page_comments) #make corpus
print(corpus_sonya_nytimes2_comments)
summary(corpus_sonya_nytimes2_comments)
head(docvars(corpus_sonya_nytimes2_comments))
#token
sonya_token_nytimes2_comments <- tokens(corpus_sonya_nytimes2_comments) #tokenized the pdf text
sonya_nonpunc_nytimes2_comments <- tokens(corpus_sonya_nytimes2_comments, remove_punct = TRUE, remove_numbers = TRUE) #remove punctuation and numbers
print(sonya_nonpunc_nytimes2_comments)
sonya_nostop_nytimes2_comments <- tokens_select(sonya_nonpunc_nytimes2_comments, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_nytimes2_comments)
nytimes2_comments_custom_stop_words <- c("reply","july","replies", "share","comments","recommended","expand_more","wrre","recommendshare","flag","commented","s","m")
sonya_custom_nytimes2_comments <- tokens_select(sonya_nonpunc_nytimes2_comments, pattern = c((stopwords("en")),(nytimes2_comments_custom_stop_words)), selection = "remove") #remove custom words
print(sonya_custom_nytimes2_comments)
#dataframe
sonya.dfm.nytimes2.comments <- dfm(sonya_nostop_nytimes2_comments) #put the text into a dataframe matrix
sonya.nytimes2.comments.frequency <- topfeatures(sonya.dfm.nytimes2.comments, n = 10) #asked for the top 10 words in article
print(sonya.nytimes2.comments.frequency)
sonya.nytimes2.comments.dfm.custom <- dfm(sonya_custom_nytimes2_comments)
sonya.nytimes2.comments.frequency.custom <- topfeatures(sonya.nytimes2.comments.dfm.custom, n = 10)
print(sonya.nytimes2.comments.frequency.custom)
