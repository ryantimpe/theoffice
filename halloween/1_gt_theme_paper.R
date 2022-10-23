library(tidyverse)
library(gt)
# remotes::install_github("jthomasmock/gtExtras")
library(gtExtras)
library(schrute)

## Office data

office_costumes <- readxl::read_xlsx("halloween/office_costumes.xlsx")

schrute <- schrute::theoffice

## Office characters as ordered factors
office_costumes$character %>% unique() %>% dput()


character_order <- tibble::tribble(
  ~character, ~char_category, ~char_role,
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
  "Todd", "secondary", "sales",
  "Bob Vance", "guest", "family",
  "Nellie", "secondary", "other",
  "Carol", "guest", "family",
  "Cece", "guest", "family",
  "Robert Lipton", "guest", "family",
)

office_costumes2 <- office_costumes %>% 
  left_join(character_order) %>% 
  mutate(character = factor(character, levels = character_order$character))

## Boring table

office_costumes2 %>% 
  count(character, costume_category, sort = TRUE) %>% 
  filter(character %in% c("Michael", "Dwight", "Angela", "Pam"),
         costume_category %in% c("Animal", "Fictional character", "Public figure")) %>% 
  # group_by(costume_category) %>% 
  gt() 

## Episode list

epdata <- office_costumes2 %>% 
  select(season = ep_season, episode = ep_num) %>% 
  distinct() %>% 
  left_join(schrute) %>% 
  drop_na(index) %>% 
  select(season, episode, episode_name, imdb_rating, air_date) %>% 
  distinct() %>% 
  dplyr::relocate(air_date, .after = episode_name) %>% 
  mutate(note = case_when(
    season == 3 ~ "Diwali is 'essentially a Hindu Halloween'",
    season %in% 5 ~ "Cold open",
    season %in% 6 ~ "Cold open. Deleted after original airing.",
    TRUE ~ ""
  ))


# Table - summary of Halloween episodes
epdata %>% 
  mutate(imdb_rating = floor(imdb_rating/2), #Turn into a 5 star rating, no fractions
         seas_ep = paste0("S", season, " ep", str_pad(episode, 2, pad = "0")),
         air_date = lubridate::as_date(air_date)
  ) %>%  
  select(-season, -episode) %>% 
  dplyr::relocate(seas_ep, .before =1) %>% 
  bind_rows(tibble::tibble(seas_ep = c("S1", "S4"))) %>% 
  arrange(seas_ep) %>% 
  gt() %>% 
  tab_header(title = "The Office (U.S.) Halloween episodes") %>%
  cols_label(seas_ep = "Episode",
             episode_name = "Name",
             air_date = "Aired",
             imdb_rating = "Rating") %>% 
  tab_footnote(
    md("IMBD rating is an score between 1 and 10. Represented as the 'floor(**imdb_rating**/2)'"),
    location = cells_column_labels(columns = "imdb_rating")
  ) %>% 
  fmt_date(air_date, date_style = "m_day_year") %>% 
  gt_fa_rating(imdb_rating, icon = "ghost", color = "#FE9A0A") %>% 
  sub_missing() %>% 
  text_transform(location = cells_body(columns = note) , 
                 fn = function(x){
                   paste0("<span style='font-size:75%'>", x, "</span>")
                 }) %>% 
  gt_theme_paper()



#Dumb test table
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

office_costumes2 %>% 
  filter(char_role != "family") %>% 
  arrange(ep_season, character) %>% 
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

#Same chart but grid the employees

gt_oc2 <- office_costumes2 %>% 
  filter(char_role != "family") %>% 
  arrange(ep_season, character) %>% 
  filter(ep_season > 0) %>%
  select(ep_season, character) %>% 
  mutate(headshot = paste0("https://raw.githubusercontent.com/ryantimpe/theoffice/master/headshots/",
                           tolower(character), ".png")) %>% 
  complete(ep_season = 1:9, character, fill = list(headshot = "")) %>% 
  distinct() %>% #some characters have multiple costumes
  pivot_wider(names_from = character, values_from = headshot,
              values_fill = "") %>% 
  gt() %>% 
  tab_header(title = "Halloween at The Office",
             subtitle = "characters dressed up each season") %>% 
  cols_label(ep_season = "season") %>% 
  cols_align(
    align = "left"
  ) %>% 
  # gt_img_rows(columns = c("Michael", "Dwight")) %>% 
  gt_theme_paper() %>% 
  tab_options(column_labels.hidden = TRUE) %>% 
  opt_css(
    "      
      tbody tr {
        height:38px;
      }
"
  )

for(ii in office_costumes2 %>% 
    filter(char_role != "family") %>% 
    pull(character) %>% 
    unique()){
  gt_oc2 <- gt_oc2 %>% 
    gt_img_rows(ii)
}
gt_oc2


# Mentions of Halloween

schrute %>% 
  filter(str_detect(text, "Halloween")) %>% 
  count(character, sort = TRUE)

schrute %>% 
  filter(str_detect(tolower(text), "ghost")) %>% 
  count(character, sort = TRUE)


#Character & Seasons * Halloweer

schrute %>% 
  filter(str_detect(tolower(text), "halloween")) %>% 
  count(character, sort = TRUE) %>% 
  filter(n > 1) %>%
  rename(metric = character)  %>% 
  mutate(group = "Characters with more than 1 mention") %>% 
  mutate(headshot = paste0("https://raw.githubusercontent.com/ryantimpe/theoffice/master/headshots/",
                           tolower(metric), ".png")) %>% 
  bind_rows(
    schrute %>% 
      filter(str_detect(tolower(text), "halloween")) %>% 
      count(season) %>% 
      rename(metric=season)%>% 
      mutate(group = "mentions each season",
             metric = paste0("S", metric),
             headshot = NA)
  ) %>% 
  group_by(group) %>% 
  gt() %>% 
  tab_header(title = "Halloween in Scranton",
             subtitle = "mentions of 'Halloween' in The Office") %>% 
  gt::cols_move_to_start(headshot) %>% 
  cols_align(
    align = "left",
    columns = metric
  ) %>% 
  tab_options(column_labels.hidden = TRUE) %>% 
  gtExtras::gt_img_rows(headshot, height = 30) %>% 
  gtExtras::gt_fa_repeats(n, name = "spider", palette = "#728700") %>% 
  gt_theme_paper()



# Characters * Scranton Strangler
schrute %>% 
  filter(str_detect(tolower(text), "scranton strangler")) %>% 
  count(character, sort = TRUE) %>% 
  filter(character != "Isabel") %>%
  rename(metric = character)  %>% 
  mutate(headshot = paste0("https://raw.githubusercontent.com/ryantimpe/theoffice/master/headshots/",
                           tolower(metric), ".png")) %>% 
  gt() %>% 
  tab_header(title = "The Scranton Strangler",
             subtitle = "mentions of The Office's resident serial killer") %>% 
  gt::cols_move_to_start(headshot) %>% 
  cols_label(metric = "character",
             n = "mentions",
             headshot = "") %>% 
  cols_align(
    align = "left",
    columns = metric
  ) %>% 
  gtExtras::gt_img_rows(headshot, height = 40) %>% 
  gtExtras::gt_fa_repeats(n, name = "skull", palette = "#991111") %>% 
  gt_theme_paper()%>% 
  opt_css(
    '      
      table {
      background-image: url("https://raw.githubusercontent.com/ryantimpe/theoffice/master/halloween/bloodsplatter.png");
      background-repeat: no-repeat;
      background-size: 70%;
      background-position: 100% 80%;
      }
'
  )

#Character & Seasons * Scranton strnagler

schrute %>% 
  filter(str_detect(tolower(text), "scranton strangler")) %>% 
  count(character, sort = TRUE) %>% 
  filter(character != "Isabel") %>%
  rename(metric = character)  %>% 
  mutate(group = "character split") %>% 
  bind_rows(
    schrute %>% 
      filter(str_detect(tolower(text), "scranton strangler")) %>% 
      count(season) %>% 
      rename(metric=season)%>% 
      mutate(group = "season split",
             metric = paste0("S", metric))
  ) %>% 
  mutate(headshot = paste0("https://raw.githubusercontent.com/ryantimpe/theoffice/master/headshots/",
                           tolower(metric), ".png")) %>% 
  group_by(group) %>% 
  gt() %>% 
  tab_header(title = "The Scranton Strangler",
             subtitle = "mentions of The Office's resident serial killer") %>% 
  gt::cols_move_to_start(headshot) %>% 
  cols_label(metric = "",
             n = "mentions",
             headshot = "") %>% 
  cols_align(
    align = "left",
    columns = metric
  ) %>% 
  gtExtras::gt_img_rows(headshot, height = 40) %>% 
  gtExtras::gt_fa_repeats(n, name = "skull", palette = "#991111") %>% 
  gt_theme_paper()%>% 
  opt_css(
    '      
      table {
      background-image: url("https://raw.githubusercontent.com/ryantimpe/theoffice/master/halloween/bloodsplatter.png");
      background-repeat: no-repeat;
      background-size: 70%;
      background-position: 50% 50%;
      }
'
  )




#Spookiest season

spooky_words <- c("ghost", "halloween", "strangler", "blood", "murder",
                  "death", "died", "decapitated", "monster", "scary", 
                  "dead", "vampire", "creep", "zombie", "bat",  "witch",
                  "killed", "evil", "horror", "psycho")

schrute %>% 
  filter(str_detect(tolower(text), spooky_words %>% paste0(collapse="|"))) %>% 
  tidytext::unnest_tokens(words, text) %>% 
  filter(words %in% spooky_words) %>% 
  count(season, words, name = "count_spooky_word") %>% 
  group_by(season) %>% 
  mutate(most_frequent = words[count_spooky_word == max(count_spooky_word)]) %>% 
  ungroup() %>% 
  count(season, most_frequent, name = "count_spooky") %>% 
  left_join(
    #Add number of lines per season
    schrute %>% 
      count(season, name = "count_lines")
  ) %>% 
  mutate(spooky_percent = count_spooky/count_lines) %>% 
  select(-starts_with("count_")) %>% 
  gt() %>% 
  tab_header(title = "Spooky in Scranton",
             subtitle = "spooky words per line spoken in The Office") %>% 
  cols_label(spooky_percent = "Spookiness") %>% 
  gt::fmt_percent(spooky_percent, decimals = 1) %>% 
  # gtExtras::gt_plt_bar_pct(spooky_percent, scaled = TRUE) %>% 
  tab_style(style = css("background-image" = "url('http://raw.githubusercontent.com/ryantimpe/theoffice/master/halloween/bloodsplatter.png')",
                        "background-repeat" = 'no-repeat',
                        "background-size" = '70%',
                        "background-position" = '50% 50%'
                        ),
            locations =cells_body(rows = 5)) %>% 
  # gt_theme_paper() %>%
  as_raw_html()



#Reprex of the background image space issue

library(dplyr)
library(gt)

gtcars %>%
  dplyr::select(mfr, model, msrp) %>%
  dplyr::slice(1:2) %>%
  gt() %>% 
  tab_style(style = 
              css(
                "background-image" = 
                  "url('http://raw.githubusercontent.com/ryantimpe/theoffice/master/halloween/bloodsplatter.png')",
                "background-repeat" = 'no-repeat',
                "background-size" = '70%',
                "background-position" = '50% 50%'
  ),
  locations =cells_body(rows = 2)) %>% 
  gt::as_raw_html()

css(
  "background-image" = 
    "url('http://raw.githubusercontent.com/ryantimpe/theoffice/master/halloween/bloodsplatter.png')",
  "background-repeat" = 'no-repeat',
  "background-size" = '70%',
  "background-position" = '50% 50%'
)