library(tidyverse)
library(tidytext)

#1 - Try Lyric Genius
devtools::install_github("josiahparry/geniusR")
library(geniusR)

#Only 2 incomplete seasons....
gen_s1 <- genius_album(artist = "The Office (US)", album = "Season 1")
gen_s2 <- genius_album(artist = "The Office (US)", album = "Season 2")

#2 - data.world. Doesnt have cues

dw_office <- read_csv("data/the-office-lines - scripts.csv") %>% 
  mutate(line = str_remove_all(line_text, "\\[(.*?)\\]"),
         direction = str_extract_all(line_text, "\\[(.*?)\\]"))

speakers <- dw_office %>% 
  count(speaker, sort=TRUE) %>% 
  #Create 'other' speaker for anyone less than 400 lines
  mutate(speaker2 = ifelse(n < 400, "Speaker", speaker)) %>% 
  select(-n)

dw_office2 <- dw_office %>% 
  left_join(speakers) %>% 
  #Clean text
  mutate(line_text = str_remove_all(line_text, "\\t|@|\\*|\\n|\\+|\\_|=|\\#|�|\\(|\\)|\\{|\\}|\\/|\\;"),
         line_text = str_replace_all(line_text, "&", "and")) %>% 
  mutate(end_of_scene = ifelse(scene != lead(scene) | is.na(lead(scene)), "=", ">")) %>% 
  mutate(text = paste0(speaker2, ": ", line_text, end_of_scene))

saveRDS(dw_office2, file = "data/Pro_TheOffice.rds")

#Chars
dw_chars <- dw_office2 %>% 
  select(text) %>% 
  unnest_tokens(character, text, to_lower = FALSE, 
                token = "characters", strip_non_alphanum = FALSE) %>% 
  count(character, sort = TRUE)

dw_chars %>% pull(character)

