library(tidyverse)
library(gt)
library(gtExtras)
library(schrute)

# Raw data

schrute::theoffice %>% View()

office_costumes <- readxl::read_xlsx("halloween/office_costumes.xlsx")

hw_ep_list <- office_costumes %>% 
  select(ep_num, ep_season) %>% 
  distinct()
