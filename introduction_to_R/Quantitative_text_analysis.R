# Install packages
# install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("tidytext")
# install.packages("quanteda")
# install.packages("quanteda.textstats")
# install.packages("quanteda.textmodels")
# install.packages("ldatuning")
# install.packages("seededlda")
# install.packages("udpipe")
# install.packages("parallel")

# Load packages
library(parallel)
library(tidyverse)
library(lubridate)
library(tidytext)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)
library(ldatuning)
library(seededlda)
library(udpipe)

# Load data dynamically from a github page (should've done this earlier)
load(url("https://github.com/jfjelstul/law-and-courts-workshop/raw/master/data/text_corpus.RData")) %>% as_tibble()


# Regex
# Matching literally
string = "courts and judges"
string %>%
  str_extract(pattern = "courts")

string %>%
  str_detect(pattern = "duck")

# Getting a name
# Run the first
wiki = data.frame(wiki_text = c("Lee Epstein is a political scientist", "Arnold Schwarzenegger is an american actor"))

# And then run this and try to understand what just happened
wiki = string %>%
  mutate(
    name = wiki_text %>%
      str_extract(pattern = "[A-Z][a-z]+\\s[A-Z][a-z]+")
  )

# Regex exercise 1 - replace the # with quantifiers
# Run the first
wiki = data.frame(wiki_text = c("Lee Epstein is a political scientist", "Arnold Schwarzenegger is an american actor", "D. Trump is a former US president"))

# And then run this and try to understand what just happened
wiki = wiki %>%
  mutate(
    name = wiki_text %>%
      str_extract(pattern = "[A-Z]\\.#[a-z]#\\s[A-Z][a-z]#")
  )

# Exercise 2
# This is the data you will be working with
judge_rapporteur = text_corpus %>%
  select(ecli, text) %>%
  filter(str_detect(text, "^composed of")) %>%
  as_tibble() %>%
  slice_tail(n = 3)

# Your code goes here

# Exercise 3
NSS_df = readRDS(url("https://github.com/stepanpaulik/courts_and_judges/raw/main/data/NSS_df.rds"))

# Check the ability distribution
NSS_df %>% 
  ggplot() +
  geom_density(aes(x = ability, group = phd, color = phd))

# Your transformation code goes here

# The model
logit <- glm(outcome ~ phd, data = NSS_df, family = "binomial")

plot(logit)

# UDPipe
bvg_text_corpus = readRDS(url("https://github.com/stepanpaulik/courts_and_judges/raw/main/data/bvg_text_corpus.rds")) %>% 
  as_tibble() %>%
  slice_head(n = 100)

# Download the language model and determine the number of cores used for the process
n.cores = detectCores() - 2
ud_model = udpipe_download_model(language = "german")
ud_model = udpipe_load_model(ud_model$file_model)

start = Sys.time()
bvg_udpipe = udpipe(x = bvg_text_corpus, object = ud_model, parallel.cores = n.cores) %>%
  as_tibble() %>% 
  select(c(doc_id, paragraph_id, start, end, token, lemma, upos))
  select()
  
end = Sys.time()

start = Sys.time()
bvg_udpipe = udpipe(x = bvg_text_corpus, object = ud_model, parallel.cores = 1) %>%
  as_tibble() %>%
  select(c(doc_id, paragraph_id, start, end, token, lemma, upos))
end = Sys.time()

# Creating a dtm/dfm multiple ways---

# Stop words - they are words that you think carry little to zero meaning in terms of distinguishing topics from each other
to_remove = tibble(
  word = c(
    "article", "court", "paragraph", "judgment", "case",
    "proceedings", "apeal", "application",
    "directive", "regulation", "law",
    "member", "state", "states", "commission", "european", "union", "eu", "eu:c"
  )
)

# Add the typical english stop words
to_remove = bind_rows(
  get_stopwords() %>% select(word),
  to_remove
)
# Quanteda
quanteda_corpus = text_corpus %>%
  select(ecli, text) %>% 
  group_by(ecli) %>%
  summarise(text = paste(text, collapse = " ")) %>%
  corpus(
    docid_field = "ecli",
    text_field = "text"
  )

CJEU_dfm = tokens(quanteda_corpus, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) %>% 
  tokens_select(pattern = to_remove$word, valuetype = "regex", selection = "remove", min_nchar=2L) %>%   dfm() %>% 
  dfm_trim(min_termfreq = 5)

# The tidy text way
# Create tidy text corpus
tidy_text_corpus = text_corpus %>%
  group_by(ecli) %>%
  unnest_tokens(
    output = "word",
    input = text,
    token = "words",
    to_lower = TRUE
  )

# Remove stop words, short words, numbers, and words with punctuation
tidy_text_corpus = tidy_text_corpus %>%
  anti_join(
    get_stopwords(),
    by = "word"
  ) %>%
  filter(
    !str_detect(word, "[0-9]")
  ) %>%
  filter(
    str_length(word) >= 2
  ) %>%
  filter(
    !str_detect(word, "[[:punct:]]")
  )

# Remove words
tidy_text_corpus = tidy_text_corpus %>%
  anti_join(
    to_remove,
    by = "word"
  )

# Plot frequency by rank
# Zipf's law: A word's frequency is inversely proporational to its rank.
# The word at rank n appears 1/n times as often as the most frequent one.
plot <- tidy_text_corpus |>
  group_by(word) |>
  summarize(count = n()) |>
  arrange(desc(count)) |>
  ungroup() |>
  mutate(
    rank = row_number(),
    frequency = count / sum(count)
  ) |>
  ggplot() +
  geom_line(aes(x = rank, y = frequency), size = 1, color = "#3498db") +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Zipf's law for CJEU judgments",
    x = "Rank",
    y = "Frequency"
  ) +
  theme_minimal()

# Document feature matrix ------------------------------------------------------

# Create a dfm
CJEU_dfm_tidy = tidy_text_corpus %>%
  group_by(word, ecli) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  cast_dfm(
    document = ecli,
    term = word,
    value = count
  )

# Check the dimensions
dim(CJEU_dfm_tidy)

# Trim the DF
CJEU_dfm_tidy_trimmed = CJEU_dfm_tidy %>%
  dfm_trim(min_termfreq = 5)

# Check the dimensions
dim(CJEU_dfm_tidy_trimmed)
dim(CJEU_dfm)

# LDA
# hyperparameter = ldatuning::FindTopicsNumber(
#   CJEU_dfm,
#   topics = seq(from = 14, to = 22, by = 2),
#   metrics = c("CaoJuan2009",  "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 77),
#   verbose = TRUE
# )
# 
# FindTopicsNumber_plot(hyperparameter)


# number of topics
K = 10

# set random number generator seed
set.seed(9161)

# compute the LDA model, inference via 1000 iterations of Gibbs sampling
cjeu_topic_model = textmodel_lda(CJEU_dfm, K)
# saveRDS(cjeu_topic_model, file = "../data/cjeu_topic_model.rds")
cjeu_topic_model = readRDS(url("https://github.com/stepanpaulik/courts_and_judges/raw/main/data/cjeu_topic_model.rds"))

# List 20 terms that define each topic
terms(cjeu_topic_model, n = 20)

# How the NSS dataset was generated
# Random prep for courts_and_judges
NSS_df = NSS_metadata %>%
  filter(type_decision == "Rozsudek") %>%
  mutate(outcome = case_when(
    grepl("zrušeno", type_verdict) ~ "granted",
    .default = "rejected"
  ),
  outcome = factor(outcome),
  phd = case_when(
    grepl("Ph.D.", lawyer) ~ TRUE,
    .default = FALSE)) %>%
  select(c(doc_id, lawyer, outcome, phd)) %>%
  group_by(phd) %>%
  mutate(ability = rnorm(n(), mean = phd + 4, sd = 1)) %>%
  ungroup() %>%
  drop_na()

# Check the ability distribution
NSS_df %>% 
  ggplot() +
  geom_density(aes(x = ability, group = phd, color = phd))



#### Scaling

### Solutions



# Regex
# Run the first
wiki = data.frame(wiki_text = c("Lee Epstein is a political scientist", "Arnold Schwarzenegger is an american actor", "D. Trump is a former US president"))

# And then run this and try to understand what just happened
wiki = wiki %>%
  mutate(
    name = wiki_text %>%
      str_extract(pattern = "[A-Z]\\.?[a-z]*\\s[A-Z][a-z]+")
  )

# Exercise 1
judge_rapporteur = judge_rapporteur %>% 
  mutate(
    judge_rapporteur = text |>
      str_extract("[A-Z]\\. [[:alpha:]]+ \\(Rapporteur\\)") |>
      str_remove("\\([Rr]apporteur\\)") |>
      str_remove("^[A-Z]\\.") |>
      str_squish()
  )