library(tidyverse)
library(gt)
# remotes::install_github("jthomasmock/gtExtras")
library(gtExtras)


## Office data

office_costumes <- readxl::read_xlsx("halloween/office_costumes.xlsx")

## Office characters as ordered factors
office_costumes$character %>% unique() %>% dput()


character_order <- tibble::tribble(
  ~character, ~char_category,
  "Michael", "primary", "manager",
  "Dwight", "primary", "sales",
  "Jim", "primary", "sales",
  "Pam", "primary", "other",
  "Angela", "primary", "accounting",
  "Oscar", "primary", "accounting",
  "Kevin", "primary", "accounting",
  "Stanley", "primary", "sales",
  "Phyllis", "primary", "sales",
  "Kelly", "primary", "other",
  "Ryan", "primary", "other",
  "Meredith", "primary", "other",
  "Creed", "primary", "other",
  "Devon", "secondary", "other",
  "Andy", "primary", "sales",
  "Erin", "primary", "other",
  "Darryl", "primary", "other",
  "Toby", "primary", "other",
  "Gabe", "secondary", "other",
  "Todd Packer", "secondary", "sales",
  "Bob Vance", "guest", "family",
  "Nellie", "secondary", "other",
  "Carol", "guest", "family",
  "Cece", "guest", "family",
  "Robert Lipton", "guest", "family",
  )

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


# Who wore a costume each year

office_costumes %>% 
  filter(ep_season > 0) %>% 
  select(ep_season, character) %>% 
  distinct() %>% #some characters have multiple costumes
  mutate(headshot = paste0("https://raw.githubusercontent.com/ryantimpe/theoffice/master/headshots/",
                           tolower(character), ".png")) %>% 
  select(-character) %>% 
  nest(data = headshot) %>%
  rename(characters = data) %>% 
  gt() %>% 
  tab_header(title = "Halloween at The Office",
             subtitle = "characters dressed up each season") %>% 
  cols_label(characters = "character",
             ep_season = "season") %>% 
  cols_align(
    align = "left",
    columns = characters
  ) %>% 
  gtExtras::gt_img_multi_rows(characters) %>% 
  gt_theme_paper()
