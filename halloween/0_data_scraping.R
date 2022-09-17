library(rvest)
library(tidyverse)

#Grab all list items from the wiki

off_raw <- read_html("https://theoffice.fandom.com/wiki/Halloween_Costumes")

off_raw %>% 
  html_elements("li") %>% 
  html_text2() %>% 
  str_subset("- |as ") %>% 
  tibble::tibble(cost_raw = .) %>% 
  separate(cost_raw, c("character", "costume"), 
           sep="as |- ", extra = "merge") %>% 
  separate(costume, c("costume_detai", "costume_source"),
           sep = "from", extra = "warn") %>% 
  mutate(across(where(is.character), str_trim)) %>% 
  write_tsv("office_costumes_raw.tsv", na="")
  


off_raw %>% 
  html_elements("li") %>% 
  html_elements("img") %>% 
  as.character() %>% 
  str_remove_all(fixed("<img data-src"))
