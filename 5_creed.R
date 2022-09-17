
library(tidytext)
library(schrute)
library(stringr)
library(tidyverse)

schrute::theoffice %>%
  mutate(character = stringr::str_trim(character) %>% str_remove('\\"')) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(character, word) %>% 
  group_by(character) %>% 
  filter(n() >= 400) %>% 
  ungroup() -> wordz

wordz %>% 
  filter(!str_detect(word, "^[0-9]")) %>% 
  group_by(word) %>% 
  mutate(n_total = sum(n),
         n_other = n_total - n) %>% 
  ungroup() %>% 
  pivot_wider()
  filter(n_total > 1) %>% 
  mutate(prop = log(n+1)/log(n_other + 1)) %>% 
  group_by()

  filter(character %in% c("Dwight", "Creed"))
  filter(str_detect(tolower(text), "strangler")) %>% 
  View()
