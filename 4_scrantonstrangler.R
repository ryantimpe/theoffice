
library(tidytext)
library(schrute)
library(stringr)
library(tidyverse)

schrute::theoffice %>% 
  as.data.frame() %>% 
  filter(str_detect(tolower(text), "strangler")) %>% 
  View()
