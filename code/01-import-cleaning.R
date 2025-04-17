
```{r, include=F}
sonya_cnn1 <- "https://www.cnn.com/2024/07/22/us/sonya-massey-police-shooting" 
sonyacnn1page <- read_html(sonya_cnn1) 
text_data_cnn1 <- html_text(html_nodes(sonyacnn1page, "p")) # Extract all text within the <p> tags
print(text_data_cnn1)  #Print the extracted text
```



```{r, include=F}
corpus_sonyacnn1 <- corpus(text_data_cnn1)
corpus_sonyacnn1 <- # REMOVING LAST THREE TEXTS DUE TO CONTENT
  corpus_subset(corpus_sonyacnn1, 
                !(docnames(corpus_sonyacnn1) %in% c(paste("text",60:62,sep=""))))
tail(corpus_sonyacnn1)
head(corpus_sonyacnn1)
head(corpus_sonyacnn1)
print(corpus_sonyacnn1)
summary(corpus_sonyacnn1)
head(docvars(corpus_sonyacnn1))
```



```{r, include=F}
sonya_tokencnn1 <- tokens(corpus_sonyacnn1) #tokenized the pdf text
sonya_nonpunc_cnn1 <- tokens(text_data_cnn1, remove_punct = TRUE) #remove punctuation
print(sonya_nonpunc_cnn1)
```


```{r, include=F}
sonya_nostop_cnn1 <- tokens_select(sonya_nonpunc_cnn1, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_cnn1)
cnn_custom_stop_words <- c("said", "says")
sonya_custom_cnn1 <- tokens_select(sonya_nonpunc_cnn1, pattern = c((stopwords("en")),(cnn_custom_stop_words)), selection = "remove") #remove custom words
print(sonya_custom_cnn1)
```



```{r,  include=F}
sonya.dfm.cnn1 <- dfm(sonya_nostop_cnn1) #put the text into a dataframe matrix
sonya.cnn1.frequency <- topfeatures(sonya.dfm.cnn1, n = 10) #asked for the top 10 words in article
print(sonya.cnn1.frequency)
sonya.cnn1.dfm.custom <- dfm(sonya_custom_cnn1)
sonya.cnn1.frequency.custom <- topfeatures(sonya.cnn1.dfm.custom, n = 10)
print(sonya.cnn1.frequency.custom)
```


```{r,  include=F}
dataframe.cnn1 <- data.frame(sonya.cnn1.frequency.custom)
colnames(dataframe.cnn1) <- "frequency"
#view(dataframe.cnn1)
dataframe.cnn1.organized <- tibble::rownames_to_column(dataframe.cnn1, var = "topwords")
#view(dataframe.cnn1.organized)
dataframe.cnn1.categories <- dataframe.cnn1.organized %>%
  mutate(Group = case_when(
    topwords %in% c("massey", "water", "pot", "black","woman","unarmed","boiling","boil","sonya") ~ "victim",  # For A, B, C -> "first"
    topwords %in% c("grayson", "officer", "deputy","police","deputy","enforcement","officers","deputies","cop","cops","violated","policy") ~ "officer",   # For X, Y, Z -> "last"
    TRUE ~ "other"                            # Default case
  )) 
#view(dataframe.cnn1.categories)
filter.cnn1.categories <- filter(dataframe.cnn1.categories) #filter variables
#filter.cnn1.categories 
#View(filter.cnn1.categories)
```


```{r,  include=F}
kw_water_cnn1 <- kwic(sonya_tokencnn1, pattern =  "water") #searching for the word in association with other words
print(kw_water_cnn1)
kw_justification_cnn1 <- kwic(sonya_tokencnn1, pattern = phrase("boiling water*")) #phrase meaning
print(kw_justification_cnn1)
sonya_nostop_cnn1 <- tokens_select(sonya_nonpunc_cnn1, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_cnn1)
sonya_threat_cnn1 <- tokens_select(sonya_nostop_cnn1, pattern = c("speak*","deputy*"), padding = TRUE, window = 5) #view words associated with key words
print(sonya_threat_cnn1)
```


```{r,  include=F}
sonya_cnn2 <- "https://www.cnn.com/2024/07/23/us/sonya-massey-police-shooting-what-went-wrong/index.html" 
sonyacnn2page <- read_html(sonya_cnn2) 
text_data_cnn2 <- html_text(html_nodes(sonyacnn2page, "p")) # Extract all text within the <p> tags
print(text_data_cnn2)  #Print the extracted text
```


```{r,  include=F}
corpus_sonyacnn2 <- corpus(text_data_cnn2)
corpus_sonyacnn2 <- # REMOVING LAST THREE TEXTS DUE TO CONTENT
  corpus_subset(corpus_sonyacnn2, 
                !(docnames(corpus_sonyacnn2) %in% c("text57", "text58")))
tail(corpus_sonyacnn2)
print(corpus_sonyacnn2)
summary(corpus_sonyacnn2)
head(docvars(corpus_sonyacnn2))
```


```{r,  include=F}
sonya_tokencnn2 <- tokens(corpus_sonyacnn2) #tokenized the pdf text
sonya_nonpunc_cnn2 <- tokens(sonya_tokencnn2, remove_punct = TRUE) #remove punctuation
print(sonya_nonpunc_cnn2)
```


```{r,  include=F}
sonya_nostop_cnn2 <- tokens_select(sonya_nonpunc_cnn2, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_cnn2)
cnn_custom_stop_words <- c("said", "says")
sonya_custom_cnn2 <- tokens_select(sonya_nonpunc_cnn2, pattern = c((stopwords("en")),(cnn_custom_stop_words)), selection = "remove") #remove custom words
print(sonya_custom_cnn2)
```


```{r,  include=F}
sonya.dfm.cnn2 <- dfm(sonya_nostop_cnn2) #put the text into a dataframe matrix
sonya.cnn2.frequency <- topfeatures(sonya.dfm.cnn2, n = 10) #asked for the top 10 words in article
print(sonya.cnn2.frequency)
sonya.cnn2.dfm.custom <- dfm(sonya_custom_cnn2)
sonya.cnn2.frequency.custom <- topfeatures(sonya.cnn2.dfm.custom, n = 10)
print(sonya.cnn2.frequency.custom)
```


```{r,  include=F}
dataframe.cnn2 <- data.frame(sonya.cnn2.frequency.custom)
colnames(dataframe.cnn2) <- "frequency"
#view(dataframe.cnn2)
dataframe.cnn2.organized <- tibble::rownames_to_column(dataframe.cnn2, var = "topwords")
#view(dataframe.cnn2.organized)
dataframe.cnn2.categories <- dataframe.cnn2.organized %>%
  mutate(Group = case_when(
    topwords %in% c("massey", "water", "pot", "black","woman","unarmed","boiling","boil","sonya") ~ "victim",  # For A, B, C -> "first"
    topwords %in% c("grayson", "officer", "deputy","police","deputy","enforcement","officers","deputies","cop","cops","violated","policy") ~ "officer",   # For X, Y, Z -> "last"
    TRUE ~ "other"                            # Default case
  )) 
#view(dataframe.cnn2.categories)
filter.cnn2.categories <- filter(dataframe.cnn2.categories) #filter variables
#filter.cnn2.categories 
#View(filter.cnn2.categories)
```


```{r,  include=F}
kw_water_cnn2 <- kwic(sonya_tokencnn2, pattern =  "water") #searching for the word in association with other words
print(kw_water_cnn2)
kw_rebuke_cnn2 <- kwic(sonya_tokencnn2, pattern =  "rebuke")
print(kw_rebuke_cnn2)
kw_justification_cnn2 <- kwic(sonya_tokencnn2, pattern = phrase("boiling water*")) #phrase meaning
print(kw_justification_cnn2)
sonya_threat_cnn2 <- tokens_select(sonya_nostop_cnn2, pattern = c("speak*","deputy*"), padding = TRUE, window = 5) #view words associated with key words
print(sonya_threat_cnn2)
```


```{r,  include=F}
sonya_fox1 <- "https://www.foxnews.com/us/bodycam-video-reveals-chaotic-scene-deputy-fatally-shooting-sonya-massey-called-911-help" 
sonya_fox1_page <- read_html(sonya_fox1) 
text_data_fox1 <- html_text(html_nodes(sonya_fox1_page, "p")) # Extract all text within the <p> tags
print(text_data_fox1)
```


```{r,  include=F}
corpus_sonyafox1 <- corpus(text_data_fox1)
corpus_sonyafox1 <- # REMOVING LAST THREE TEXTS DUE TO CONTENT
  corpus_subset(corpus_sonyafox1, 
                !(docnames(corpus_sonyafox1) %in% c(paste("text",33:42,sep=""))))
tail(corpus_sonyafox1)
head(corpus_sonyafox1)
print(corpus_sonyafox1)
summary(corpus_sonyafox1)
head(docvars(corpus_sonyafox1))
```


```{r,  include=F}
sonya_token_fox1 <- tokens(corpus_sonyafox1) #tokenized the pdf text
sonya_nonpunc_fox1 <- tokens(text_data_fox1, remove_punct = TRUE) #remove punctuation
print(sonya_nonpunc_fox1)

```


```{r,  include=F}
sonya_nostop_fox1 <- tokens_select(sonya_nonpunc_fox1, pattern = stopwords("en"), selection = "remove") #remove articles
print(sonya_nostop_fox1)
fox_custom_stop_words <- c("said", "says")
sonya_custom_fox1 <- tokens_select(sonya_nonpunc_fox1, pattern = c((stopwords("en")),(fox_custom_stop_words)), selection = "remove") #remove custom words
print(sonya_custom_fox1)
```


```{r,  include=F}
sonya.dfm.fox1 <- dfm(sonya_nostop_fox1) #put the text into a dataframe matrix
sonya.fox1.frequency <- topfeatures(sonya.dfm.fox1, n = 10) #asked for the top 10 words in article
print(sonya.fox1.frequency)
sonya.fox1.dfm.custom <- dfm(sonya_custom_fox1)
sonya.fox1.frequency.custom <- topfeatures(sonya.fox1.dfm.custom, n = 10)
print(sonya.fox1.frequency.custom)
```


```{r,  include=F}
dataframe.fox1 <- data.frame(sonya.fox1.frequency.custom)
colnames(dataframe.fox1) <- "frequency"
#view(dataframe.fox1)
dataframe.fox1.organized <- tibble::rownames_to_column(dataframe.fox1, var = "topwords")
#view(dataframe.fox1.organized)
dataframe.fox1.categories <- dataframe.fox1.organized %>%
  mutate(Group = case_when(
    topwords %in% c("massey", "water", "pot", "black","woman","unarmed","boiling","boil","sonya") ~ "victim",  # For A, B, C -> "first"
    topwords %in% c("grayson", "officer", "deputy","police","deputy","enforcement","officers","deputies","cop","cops","violated","policy") ~ "officer",   # For X, Y, Z -> "last"
    TRUE ~ "other"                            # Default case
  )) 
#view(dataframe.fox1.categories)
filter.fox1.categories <- filter(dataframe.fox1.categories) #filter variables
#filter.fox1.categories 
#View(filter.fox1.categories)
```


```{r,  include=F}
fox_custom_key_words <- c("sonya","massey","grayson","officer","kill*","justified*","just*","fault*","cop*","officer*","violation*","policy")
sonya_key_custom_fox1 <- tokens_select(sonya_nostop_fox1, pattern = fox_custom_key_words) #choose keywords to analyze
sonya.dfm.fox1.key <- dfm(sonya_key_custom_fox1)
sonya.fox1.lsa <- textmodel_lsa(sonya.fox1.dfm.custom)
sonya.fox1.lsa$matrix_low_rank
sonya.fox1.lsa$features
dataframe.fox1.lsa.features <- data.frame(sonya.fox1.lsa$features)
view(dataframe.fox1.lsa.features)
barplot(sonya.fox1.lsa$features)
predict.fox1 <- predict(sonya.fox1.lsa)
predict.fox1
textplot_network(sonya.dfm.fox1.key)
```


```{r,  include=F}
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
```


