library(tidyverse)
library(gt)
# remotes::install_github("jthomasmock/gtExtras")
library(gtExtras)


## Office data

office_costumes <- readxl::read_xlsx("halloween/office_costumes.xlsx")


## Boring table

office_costumes %>% 
  count(character, costume_category, sort = TRUE) %>% 
  filter(character %in% c("Michael", "Dwight", "Angela", "Pam"),
         costume_category %in% c("Animal", "Fictional character", "Public figure")) %>% 
  # group_by(costume_category) %>% 
  gt() %>% 
  gt_theme_dot_matrix()

# Sort out gt_theme_paper()

office_costumes %>% 
  count(character, costume_category, sort = TRUE) %>% 
  filter(character %in% c("Michael", "Dwight", "Angela", "Pam"),
         costume_category %in% c("Animal", "Fictional character", "Public figure")) %>% 
  group_by(costume_category) %>%
  gt() %>%
  tab_header(title = "Halloween at The Office",
             subtitle = "It is your costume") %>% 
  cols_label(character = "Character",
             costume_category = "Costume type",
             n = "#") %>% 
  gt_highlight_rows(rows = 3, fill = "#FBF719", alpha = 0.5, font_weight = "normal") %>% 
  tab_footnote(
    footnote = "Arbitrarily chosen by Ryan.",
    location = cells_column_labels(columns = "costume_category")
  ) %>% 
  gt_theme_paper()


office_costumes %>% 
  filter(ep_season != 0) %>% 
  count(costume_category, sort = TRUE) %>% 
  gt() %>% 
  tab_header(title = "Halloween in Scranton",
             subtitle = "Costume categories over the course of the series") %>% 
  cols_label(costume_category = "Costume category", 
             n = "Count") %>% 
  summary_rows(
    columns = n,
    fns = list(
      Total = "sum"
    ),
    formatter = fmt_integer
  ) %>% 
  gt_theme_paper()


office_costumes %>% 
  filter(ep_season != 0) %>%
  filter(costume_category %in% c("Other", "Low-effort", "The Office universe")) %>% View()
  group_by(costume_category) %>% 
  gt() %>%
  tab_header(title = "Halloween at The Office",
             subtitle = "Strangest costumes") %>% 
  cols_label(character = "Character",
             costume_category = "Costume type",
             n = "#") %>% 
  gt_highlight_rows(rows = 3, fill = "#FBF719", alpha = 0.5, font_weight = "normal") %>% 
  tab_footnote(
    footnote = "Arbitrarily chosen by Ryan.",
    location = cells_column_labels(columns = "costume_category")
  ) %>% 
  gt_theme_paper()
