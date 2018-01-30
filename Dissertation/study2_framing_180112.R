##################################################
## Project: Dissertation Study 2
## Script purpose:  
## Date Created:
## Last Updated:
## Author:
##################################################
load(here::here("fb_corpus_2018-01-27.RData"))

library(tidyverse)
library(magrittr)
library(stringr)
library(stm)

# Keep only posts with 25 or more words
to_clean <- fb_df %>% 
  mutate(n_words = stringr::str_count(message, "\\S+")) %>%
  filter(
    n_words >= 50
  )

#### 1) Preprocess facebook posts ####
# Define a function to clean the text data. This will:
#     1) Remove non-ASCII characters
#     2) Remove hyperlinks
#     3) Remove numbers
#     4) Remove line breaks
#     5) Convert words to lower case
#     6) Remove stop words
#     7) Remove emails
#     8) Remove punctuation
#     9) Stip excess white space
#     10) Get rid of any words with 2 or fewer characters
clean_text <- function(data, text_variable, keep_stop = NULL, stop_list = NULL,
                       remove_common = TRUE, top_n_common = 25,
                       keep_common = NULL, stem = FALSE,
                       filter = NULL, min_n_words = NULL) {
  require(tm)
  names(data)[which(names(data) == text_variable)] <- "text"

  # Get stop words from the tidytext package
  if (!is.null(stop_list)) {
    stoplist <- stop_list
  } else {
    data("stop_words", package = "tidytext")
    stoplist <- c(stop_words$word)
  }

  if (!is.null(keep_stop)) {
    stoplist <- stoplist[which(!stoplist %in% keep_stop)]
  }

  textdata <- data %>%
    mutate(text = iconv(text, "latin1", "ASCII", sub = " ")) %>% # Remove non-ASCII characters
    mutate(text = str_replace_all(
      text,
      "https?://t\\.co/[A-Za-z\\d]+|https?://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https?",
      ""
    )) %>% # Remove hyperlinks
    mutate(text = removeNumbers(text)) %>% # Get rid of numbers
    mutate(text = str_replace_all(text, "[\r\n]", " ")) %>% # Remove line breaks
    mutate(text = tolower(text)) %>% # Convert all to lower case
    mutate(text = removeWords(text, stoplist)) %>% # Remove stopwords
    mutate(text = gsub("\\S+@\\S+", " ", text)) %>% # Remove email addresses
    mutate(text = gsub("[[:punct:][:blank:]]+", " ", text)) %>% # Remove punctuation
    mutate(text = removeWords(text, stoplist)) %>% # Remove stopwords, again
    mutate(text = gsub("\\b\\w{1,2}\\b", "", text)) # Get rid of any words with 2 or fewer characters

  if (!is.null(filter)) {
    textdata <- textdata %>%
      filter(str_detect(
        text,
        filter
      ))
  }

  if (remove_common) {
    if (!is.null(keep_common)) {
      common_words <- textdata %>%
        unnest_tokens(word, text) %>%
        count(word, sort = TRUE) %>%
        select(word) %>%
        slice(1:top_n_common) %>%
        filter(!word %in% keep_common)
    } else {
      common_words <- textdata %>%
        unnest_tokens(word, text) %>%
        count(word, sort = TRUE) %>%
        slice(1:top_n_common) %>%
        select(word)
    }
    textdata <- textdata %>%
      mutate(text = removeWords(text, common_words$word))
  }
  if (!is.null(min_n_words)) {
    textdata <- textdata %>%
      mutate(n_words = stringr::str_count(text, "\\S+")) %>%
      filter(n_words > min_n_words) %>%
      select(-n_words)
  }
  textdata <- textdata %>%
    mutate(text = gsub("\\s+", " ", text)) # Stip any excess white space

  if (stem == TRUE) {
    textdata <- textdata %>%
      mutate(text = stemDocument(text, language = "english"))
  }

  return(textdata)
}

# Create stop words vector #
data("stop_words", package = "tidytext")
stop_words <- stop_words %>%
  select(word) %>%
  filter(!duplicated(word))
stop_words <- c(
  stop_words$word, "nys", "nyc",
  "school", "education", "parents", "parent",
  "district", "grade", "people", "regents",
  "principal", "question",
  "child", "children",
  "kid", "kids",
  "student", "students",
  "son", "daughter",
  "teacher", "teachers",
  "school", "schools",
  "board", "learning", "day",
  "math", "ela",
  "york", "island", "florida"
)

clean_fb_df <- clean_text(fb_df, 
                          "message",
                          remove_common = F,
                          stem = T)

#clean_fb_df <- clean_fb_df %>% 
#  filter(str_detect(text,
#  "\\btest|\\bassessment"))

temp <- textProcessor(
  documents = clean_fb_df$text,
  metadata = clean_fb_df,
  lowercase = F,
  removestopwords = F,
  removenumbers = F,
  removepunctuation = F,
  stem = F
)
meta <- temp$meta
vocab <- temp$vocab
docs <- temp$documents
out <- prepDocuments(docs, vocab, meta,
                     lower.thres = 50)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

K <- c(10, 20, 30, 40, 50, 75, 100)
kresult <- searchK(out$documents, 
                   out$vocab, 
                   K, 
                   prevalence = ~ group_name + month_year, 
                   data = out$meta, 
                   init.type = "Spectral")
plot(kresult)

k <- 80
stmFit <- stm(out$documents, 
              out$vocab, 
              K = k, 
              prevalence = ~ group_name + month_year, 
              max.em.its = 300, 
              data = out$meta, 
              init.type = "Spectral", 
              seed = 300,
              control = list(alpha = 50/k,
                             eta = 0.01))

plot(stmFit, 
     type = "summary", 
     xlim = c(0,.14), 
     n = 10, 
     labeltype = "frex", 
     text.cex = 0.65)

## LDA model
library(topicmodels)
library(tidytext)

clean_fb_df_word <- clean_fb_df %>%
  unnest_tokens(word, text)

word_counts <- clean_fb_df_word %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

fb_dtm <- word_counts %>%
  cast_dtm(id, word, n)

fb_dtm <- removeSparseTerms(fb_dtm, 0.999)
rowTotals <- apply(fb_dtm , 1, sum)
fb_dtm   <- fb_dtm[rowTotals> 0, ] 

fb_lda <- LDA(fb_dtm, k = 80, control = list(seed = 1234))

fb_tfidf <- word_counts %>% bind_tf_idf(word, group, count)

#### GET LINKS ####
opt_out_groups <- readxl::read_excel("/Users/richardmorel/Box Sync/Paquin-Morel Lab/optout_group_list_171010.xlsx") %>%
  mutate(URL = str_replace(URL, "https://", "http://")) %>%
  filter(!duplicated(URL))

fb_links <- fb_df %>%
  mutate(links = if_else(is.na(link),
    str_extract(message, " ?(f|ht)tp(s?)://(.*)[.][a-z]+"),
    link
  )) %>%
  select("id", "group_id", "links", "year") %>%
  filter(!is.na(links)) %>%
  mutate(
    domain = urltools::url_parse(links)$domain,
    url = if_else(urltools::url_parse(links)$domain == "www.facebook.com",
      paste0(
        "http://", urltools::url_parse(links)$domain, "/",
        urltools::url_parse(links)$path
      ),
      paste0("http://", urltools::url_parse(links)$domain, "/")
    )
  ) %>%
  filter(!grepl(".biz|.gov|.us|.tv", domain))

fb_links_urls <- fb_links$url %>%
  map_chr(~ ifelse(ncol(str_split(.x, "/", simplify = T)) > 5,
    paste0(
      "http://",
      str_split(.x, "/", simplify = T)[, 3], "/",
      str_split(.x, "/", simplify = T)[, 4], "/",
      str_split(.x, "/", simplify = T)[, 5], "/"
    ),
    .x
  ))
fb_links_urls <- if_else(str_split(fb_links_urls, "/", simplify = T)[, 3] == "www.facebook.com" &
  str_split(fb_links_urls, "/", simplify = T)[, 4] %in% c("groups", "pages", "pg"),
paste0(
  "http://",
  str_split(fb_links_urls, "/", simplify = T)[, 3], "/",
  str_split(fb_links_urls, "/", simplify = T)[, 5],
  "/"
),
fb_links_urls
)
fb_links <- fb_links %>%
  mutate(url = fb_links_urls) %>%
  filter(group_id %in% fb_attributes$ID)

matched_links <- opt_out_groups$URL %>%
  map(~ agrep(.x, fb_links$url, max.distance = 0.01))

matched_links <- sort(unlist(matched_links))

fb_edgelist <- data.frame(
  FROM = fb_links[matched_links, "group_id"],
  TO = fb_links[matched_links, "url"],
  year = fb_links[matched_links, "year"],
  stringsAsFactors = F
)

fb_edgelist$FROM <- fb_attributes$ABBREVIATION[match(fb_edgelist$FROM, fb_attributes$ID)]
to_url <- fb_edgelist$TO %>%
  map_chr(~ ifelse(ncol(str_split(.x, "/", simplify = T)) > 4 &
    str_split(.x, "/", simplify = T)[, 3] == "www.facebook.com",
  paste0(
    "http://",
    str_split(.x, "/", simplify = T)[, 3], "/",
    str_split(.x, "/", simplify = T)[, 4], "/"
  ),
  .x
  ))
fb_edgelist <- fb_edgelist %>%
  mutate(TO = to_url)
fb_edgelist <- fb_edgelist %>%
  filter(TO != "http://www.donotlink.com/")

to_abbr <- fb_edgelist$TO %>%
  map(~ opt_out_groups$ABBREVIATION[agrep(.x, opt_out_groups$URL, max.distance = 0.01)])

to_abbr <- unlist(to_abbr)

fb_edgelist <- fb_edgelist %>%
  mutate(TO = to_abbr)

fb_edgelist <- fb_edgelist %>%
  group_by(FROM, TO, year) %>%
  summarize(n = n())

## CREATE ATTRIBUTES ##
first_year_to <- fb_edgelist %>%
  group_by(TO) %>%
  summarize(first_year = min(year)) %>%
  rename(GROUP = TO)
first_year_from <- fb_edgelist %>%
  group_by(FROM) %>%
  summarize(first_year = min(year)) %>%
  rename(GROUP = FROM)

first_year <- full_join(first_year_from, first_year_to)

library(igraph)
fb_groups_network <- graph_from_data_frame(fb_edgelist, vertices = first_year)

library(ggraph)
ggraph(fb_groups_network, layout = "kk") +
  geom_edge_link(aes(colour = year)) +
  geom_node_point(aes(colour = first_year)) +
  theme_void() +
  facet_edges(~ year)

ggraph(fb_groups_network, layout = "fr") +
  geom_edge_link(aes(colour = year)) +
  geom_node_point(aes(colour = first_year)) +
  geom_node_text(aes(label = name)) +
  theme_void()