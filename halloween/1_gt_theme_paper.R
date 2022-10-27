library(tidyverse)
library(gt)
# remotes::install_github("jthomasmock/gtExtras")
library(gtExtras)
library(schrute)

## Office data

office_costumes <- read_csv("halloween/office_costumes.csv")

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
  "Andy", "primary", "sales",
  "Erin", "primary", "other",
  "Darryl", "primary", "other",
  "Toby", "primary", "other",
  "Devon", "secondary", "other",
  "Gabe", "secondary", "other",
  "Todd", "secondary", "sales",
  "Jo", "secondary", "other",
  "Bob Vance", "guest", "family",
  "Nellie", "secondary", "other",
  "Carol", "guest", "family",
  "Cece", "guest", "family",
  "Robert Lipton", "guest", "family"
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

# Who dressed up as each other

office_within_u <- read_csv("Halloween/office_within_universe.csv")

## Grid version

gt_oc3 <- office_within_u %>% 
  #Set characters as factor to have an order
  mutate(character = factor(character, levels = character_order$character),
         dressed_as = factor(dressed_as, levels = character_order$character)) %>% 
  #Check if halloween
  left_join(epdata) %>% 
  # mutate(halloween = !is.na(title)) %>% 
  arrange(dressed_as, character) %>% 
  mutate(headshot = paste0("https://raw.githubusercontent.com/ryantimpe/theoffice/master/headshots/",
                           tolower(character), ".png")) %>% 
  mutate(dress_up = paste0("https://raw.githubusercontent.com/ryantimpe/theoffice/master/", 
                           "headshots/halloween/",
                           tolower(character), "_", 
                           season, "_", episode, "_",
                           tolower(dressed_as), ".png")) %>%
  select(headshot, character, dressed_as, dress_up) %>% 
  pivot_wider(names_from = dressed_as, values_from = dress_up,
              values_fill = "") %>% 
  gt() %>% 
  tab_header(title = "Identity theft is note a joke, Jim",
             subtitle = "characters dressed up as each other") %>% 
  cols_label(
    headshot = "dressed as..."
  ) %>% 
  gt_merge_stack_image(headshot, character, 
                       img_height = 50,
                       img_css = css(background.color = "#ffff88",
                                     border = "solid black")) %>% 
  cols_align(
    align = "center",
    columns = headshot
  ) %>% 
  # gt_img_rows(headshot, height = 50) %>% 
  gt_theme_paper()

for(ii in unique(office_within_u$dressed_as)){
  gt_oc3 <- gt_oc3 %>% 
    gt_img_rows(ii, height = 60)
}

gt_oc3 





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
  tab_options(
    column_labels.hidden = TRUE
  ) %>% 
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
  gt_theme_paper()




# Who dressed up each season... simpler table

office_costumes2 %>% 
  filter(ep_season >0 ) %>% 
  filter(char_category == "primary") %>% 
  select(ep_season, character) %>% 
  count(ep_season, character) %>% 
  complete(ep_season = 1:9) %>% 
  mutate(ep_season = paste0("s", ep_season)) %>% 
  bind_rows(
    group_by(., character) %>% 
      summarize(n=sum(n, na.rm=TRUE), .groups = "drop") %>% 
      ungroup() %>% 
      drop_na(character) %>% 
      mutate(ep_season = "series")
  ) %>% 
  pivot_wider(names_from = ep_season, values_from = n,
              values_fill = NA) %>% 
  drop_na(character) %>% 
  arrange(character) %>% 
  mutate(headshot = paste0("https://raw.githubusercontent.com/ryantimpe/theoffice/master/headshots/",
                           tolower(character), ".png"))%>%
  dplyr::relocate(headshot) %>% 
  gt(rowname_col = "headshot") %>% 
  tab_header(title = "Who dressed up in the Halloween episodes?",
             subtitle = "number of costumes for each character each season") %>% 
  # gt_img_rows(headshot, height = 25) %>%
  text_transform(
    locations = cells_stub(),
    fn = function(x) {
      web_image(
        url = x,
        height = 25
      )}
  ) %>% 
  tab_style(locations = cells_stub(),
            style = css(text.align = "center")) %>% 
  cols_hide(character) %>% 
  cols_label(headshot = "") %>%
  cols_align("center") %>% 
  tab_style(locations = cells_body(series),
            style = list(#cell_fill("#99999966"),
              css(font.weight = "bold"))) %>%
  tab_style(locations = cells_column_labels(series),
            style = list(#cell_fill("#FBF71966"),
              css(padding="5px 0px 5px 0px",
                  font.size = "80%"))) %>% 
  gt::summary_rows(columns = c(num_range("s", 1:9), series),
                   fns = list("Seas" = ~sum(., na.rm=TRUE)),
                   formatter = fmt_integer,
                   missing_text = "ALL") %>% 
  tab_source_note(html('A single 
                       <svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;fill:currentColor;overflow:visible;position:relative;"><path d="M438.6 105.4C451.1 117.9 451.1 138.1 438.6 150.6L182.6 406.6C170.1 419.1 149.9 419.1 137.4 406.6L9.372 278.6C-3.124 266.1-3.124 245.9 9.372 233.4C21.87 220.9 42.13 220.9 54.63 233.4L159.1 338.7L393.4 105.4C405.9 92.88 426.1 92.88 438.6 105.4H438.6z"/></svg> 
                       denotes 1 costume. <br/>
                       A double <svg aria-hidden="true" role="img" viewBox="0 0 448 512" style="height:1em;width:0.88em;vertical-align:-0.125em;margin-left:auto;margin-right:auto;font-size:inherit;color:"#b40000";overflow:visible;position:relative;"><path d="M182.6 246.6C170.1 259.1 149.9 259.1 137.4 246.6L57.37 166.6C44.88 154.1 44.88 133.9 57.37 121.4C69.87 108.9 90.13 108.9 102.6 121.4L159.1 178.7L297.4 41.37C309.9 28.88 330.1 28.88 342.6 41.37C355.1 53.87 355.1 74.13 342.6 86.63L182.6 246.6zM182.6 470.6C170.1 483.1 149.9 483.1 137.4 470.6L9.372 342.6C-3.124 330.1-3.124 309.9 9.372 297.4C21.87 284.9 42.13 284.9 54.63 297.4L159.1 402.7L393.4 169.4C405.9 156.9 426.1 156.9 438.6 169.4C451.1 181.9 451.1 202.1 438.6 214.6L182.6 470.6z"/></svg> 
                       denotes more than 1.')) %>% 
  gt_theme_paper() %>% 
  gt_int_as_fa(s1:s9,
               palette = c("#666666", "#b40000"))



# Costume category

unique(office_costumes2$costume_category)


category_icons <- tibble::tribble(
  ~costume_category, ~fa_icon, ~color,
  "Classic", "ghost", "#bdda57", #green
  "Animal", "cat", "#005452", #teal
  "Fictional character", "crown", "#d4af37", #gold
  "Real person", "user", "#4040FF", #blue
  "Occupation", "user-nurse", "#561D5E", #purple
  "Low-effort", "face-meh", "#afafaf", #light grey
  "The Office universe", "paperclip", "#000000",
  "Other", "mask", "#444444", #grey
)

office_costume_categories <- office_costumes2 %>% 
  filter(ep_season >0 ) %>% 
  filter(char_category != "guest") %>% 
  mutate(costume_category = case_when(
    costume_category %in% c("Performer", "Public figure") ~ "Real person",
    TRUE ~ costume_category
  )) %>% 
  select(ep_season, character, costume_category) %>% 
  complete(ep_season = 1:9) %>% 
  mutate(ep_season = paste0("s", ep_season)) %>% 
  group_by(ep_season, character) %>% 
  mutate(ep_season2 = ifelse(row_number() == 2, 
                             paste0(ep_season, "*"),
                             ep_season)) %>% 
  ungroup() %>% 
  select(-ep_season) 

ci2 <- category_icons %>% 
  left_join(
    office_costume_categories %>% 
      count(costume_category), by = "costume_category"
  )

office_costume_categories %>% 
  pivot_wider(names_from = ep_season2, values_from = costume_category,
              values_fill = NA) %>%
  drop_na(character) %>% 
  arrange(character) %>% 
  mutate(headshot = paste0("https://raw.githubusercontent.com/ryantimpe/theoffice/master/headshots/",
                           tolower(character), ".png"))%>%
  dplyr::relocate(headshot) %>% 
  gt(rowname_col = "headshot")  %>% 
  tab_header(title = "Costumes worn in Halloween episodes, by theme",
             subtitle = html(glue::glue(
               "
               {fontawesome::fa(ci2$fa_icon[1], fill = ci2$color[1])} {ci2$costume_category[1]}: <b>{ci2$n[1]}</b> total <br/>
               {fontawesome::fa(ci2$fa_icon[2], fill = ci2$color[2])} {ci2$costume_category[2]}: <b>{ci2$n[2]}</b> <br/>
               {fontawesome::fa(ci2$fa_icon[3], fill = ci2$color[3])} {ci2$costume_category[3]}: <b>{ci2$n[3]}</b> <br/>
               {fontawesome::fa(ci2$fa_icon[4], fill = ci2$color[4])} {ci2$costume_category[4]}: <b>{ci2$n[4]}</b> <br/>
               {fontawesome::fa(ci2$fa_icon[5], fill = ci2$color[5])} {ci2$costume_category[5]}: <b>{ci2$n[5]}</b> <br/>
               {fontawesome::fa(ci2$fa_icon[6], fill = ci2$color[6])} {ci2$costume_category[6]}: <b>{ci2$n[6]}</b> <br/>
               {fontawesome::fa(ci2$fa_icon[7], fill = ci2$color[7])} {ci2$costume_category[7]}: <b>{ci2$n[7]}</b> <br/>
               {fontawesome::fa(ci2$fa_icon[8], fill = ci2$color[8])} {ci2$costume_category[8]}: <b>{ci2$n[8]}</b> <br/>
               "
             ))) %>% 
  tab_source_note("Some characters in seasons 7 and 9 wore multiple costumes.") %>% 
  cols_label(`s7*` = "s7",
             `s9*` = "s9") %>% 
  text_transform(
    locations = cells_stub(),
    fn = function(x) {
      web_image(
        url = x,
        height = 25
      )}
  ) %>% 
  tab_style(locations = cells_stub(),
            style = css(text.align = "center")) %>% 
  tab_style(locations = cells_body(c("s7*", "s9*")),
            style = cell_fill(colorspace::darken("#F5F5F5"))) %>% 
  cols_hide(character) %>% 
  cols_label(headshot = "") %>%
  cols_align("center") %>% 
  gt_theme_paper() %>% 
  gt_text_as_fa(s1:`s9*`,
                category_icons)
