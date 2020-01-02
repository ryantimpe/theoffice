library(tidyverse)
library(tidytext)
library(schrute)

library(extrafont)
loadfonts(device = "win")

dat <- schrute::theoffice

dg_rules <- list()

#Drinking Game

# 1. That's what she said
# 2. Pam: 'Dunder Mifflin, this is Pam'
# 3. Dwight: 'Question.', beets or Mose
# 4. Jim: ~looks to camera~
# 5. Kelly: 'Oh My God!'
# 6. Michael: breasts, boobs
# 7. Angela: cats or Sprinkles
# 8. Andy: Cornell or Big Tuna
# 9. Stanley: Laughs
# 10. Meredith: Drinks
# 11. Phyllis: Bob (Vance, Vance Refrigeration)
# 12. Ryan: actually
# 13. Anyone: 'Assistant to the Regional Manager'

dg_rule_list <- tibble::tribble(
  ~rule, ~character, ~trigger, ~rule_group,
  1., "Anyone",    "That's what she said", "Anyone", 
  2., "Pam",       "'Dunder Mifflin, this is Pam'", "Pam",
  4., "Jim",       "~looks to camera~", "Jim",
  3., "Dwight",    "'Question.', beets or Mose", "Dwight",
  6., "Michael",   "breasts, boobs", "Michael",
  7., "Angela",    "cats or Sprinkles", "Angela & Andy",
  8., "Andy",      "Cornell or Big Tuna", "Angela & Andy",
  9., "Stanley",   "Laughs", "Phyllis, Stanley, & Meredith" ,
  10, "Meredith",  "Drinks", "Phyllis, Stanley, & Meredith" , 
  11, "Phyllis",   "Bob (Vance)\n(Vance Refrigeration)","Phyllis, Stanley, & Meredith" ,
  5., "Kelly",     "'Oh My God!'", "Kelly & Ryan",
  12, "Ryan",      "actually", "Kelly & Ryan", 
  13, "Anyone",    "'Assistant to the\nRegional Manager'", "Anyone"
)


# 1. That's what she said ----
rr = 1
dg_rules[[rr]] <- dat %>% 
  filter(str_detect(tolower(text), "that's what she said")) %>% 
  mutate(rule = rr)

# 2. Pam: Dunder Mifflin, this is Pam ----    
rr = 2
dg_rules[[rr]]<- dat %>% 
  filter(character == "Pam") %>% 
  filter(str_detect(tolower(text), "dunder mifflin")) %>% 
  filter(str_detect(tolower(text), "this is pam")) %>% 
  mutate(rule = rr)

# 3. Dwight: Question of Fact ----   
rr = 3
dg_rules[[rr]] <- dat %>% 
  filter(character == "Dwight") %>% 
  filter(str_detect(tolower(str_trim(text)), "^question|beets|mose")) %>% 
  mutate(rule = rr)

# 4. Jim: Looks to camera ----
rr = 4
dg_rules[[4]] <- dat %>% 
  filter(character == "Jim") %>% 
  filter(str_detect(str_trim(text_w_direction), "to camera")) %>% 
  mutate(rule = rr)

# 5. Kelly: Oh My God! ----
rr = 5
dg_rules[[rr]]<- dat %>% 
  filter(character == "Kelly") %>% 
  filter(str_detect(tolower(text), "oh my god")) %>% 
  mutate(rule = rr)

# 6. Michael: Breasts, boobs ----
rr = 6
dg_rules[[rr]] <- dat %>% 
  filter(character == "Michael") %>% 
  filter(str_detect(tolower(text), "boob|breast")) %>% 
  mutate(rule = rr)

# 7. Angela: cats or Sprinkles ----
rr = 7
dg_rules[[rr]]<- dat %>% 
  filter(character == "Angela") %>% 
  filter(str_detect(tolower(text), "cat[ s\\.\\?]|sprinkles")) %>% 
  mutate(rule = rr)

# 8. Andy: Cornell or Big Tuna ----
rr = 8
dg_rules[[rr]] <- dat %>% 
  filter(character == "Andy") %>% 
  filter(str_detect(tolower(text), "cornell|big tuna")) %>% 
  mutate(rule = rr)

# 9. Stanley: Laughs ----
rr = 9
dg_rules[[rr]]<- dat %>% 
  filter(character == "Stanley") %>% 
  filter(str_detect(tolower(text_w_direction), "laughs")) %>% 
  mutate(rule = rr)

# 10. Meredith: Drinks ----
rr = 10
dg_rules[[rr]] <- dat %>% 
  filter(character == "Meredith") %>% 
  filter(str_detect(tolower(text_w_direction), "drink"))%>% 
  mutate(rule = rr)

# 11. Phyllis: Phyllis: Bob (Vance, Vance Refrigeration) ----
rr = 11
dg_rules[[rr]] <- dat %>% 
  filter(character == "Phyllis") %>% 
  filter(str_detect(tolower(text_w_direction), "bob"))%>% 
  mutate(rule = rr)

# 12. Ryan: actualy ----
rr = 12
dg_rules[[rr]] <- dat %>% 
  filter(character == "Ryan") %>% 
  filter(str_detect(tolower(text), "actually")) %>% 
  mutate(rule = rr)

# 13. Anyone: 'Assistant to the Regional Manager' ----
rr = 13
dg_rules[[rr]] <- dat %>% 
  filter(str_detect(tolower(text), "assistant to the regional manager")) %>% 
  mutate(rule = rr)


# Analysis ----

dg <- bind_rows(dg_rules)


dg %>% 
  count(rule) %>% 
  left_join(dg_rule_list)

office_colors <- c("Michael" = "#1f497d",
                   "Dwight"  = "#f79646",
                   "Jim"     = "#9bbb59",
                   "Pam"     = "#c0504d",
                   "Kelly & Ryan"     = "#8064a2",
                   "Angela & Andy"   = "#4bacc5",
                   "Phyllis, Stanley, & Meredith"   = "#948b54",
                   "Anyone"= "#666666")


plot_chart <- dg %>% 
  count(season, episode, rule) %>%
  full_join(dat %>% select(season, episode, episode_name) %>% distinct()) %>% 
  left_join(dg_rule_list) %>% 
  arrange(season, episode) %>% 
  mutate(episode = as.numeric(episode),
         season = paste0("Season ", as.numeric(season))) %>%
  mutate(rule_group = factor(rule_group,
                             levels = names(office_colors))) %>% 
  ggplot(aes(x = episode, y = n)) +
  geom_col(aes(fill= rule_group)) +
  scale_fill_manual(values = office_colors) +
  scale_x_continuous(breaks = seq(1, 26, by = 5)) +
  scale_y_continuous(breaks = seq(0, 15, by  = 3)) +
  facet_wrap(~season, scales = "free_x") +
  labs(
    title = "The Office Drinking Game.",
    subtitle = "Drink consumption by season & trigger",
    caption = NULL,
    x = NULL,
    y = NULL
  ) +
  theme(
    legend.position = "none",
    legend.background = element_rect(fill = "#ffff88"),
    legend.title = element_blank(),
    text = element_text(family = "mono"),
    plot.title = element_text(size = 16, face ="bold"),
    panel.background = element_rect(fill = "#F5F5F5"),
    plot.background =  element_rect(fill = "#F5F5F5"),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(color = "#0099cc"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 10, color = "black"),
    strip.background = element_rect(fill = "#ffff88"),
    strip.text = element_text(family = "Comic Sans MS", color = "#000F55")
  )

plot_list <- dg_rule_list %>% 
  left_join(tibble(rule_group = names(office_colors), color = office_colors)) %>% 
  mutate(rule_group = factor(rule_group,
                             levels = names(office_colors))) %>% 
  arrange(rule_group) %>% 
  mutate(index = row_number()) %>% 
  ggplot(aes(y=index), x = 1) +
  geom_tile(aes(fill = color, x = -0.2, width = 0.2, height = 0.8), color = "#f5f5f5") +
  scale_fill_identity() +
  scale_size_identity() +
  geom_vline(xintercept = 1.2, color = "#F05136") +
  geom_text(aes(x=1, label = character), hjust = 1,
            family = "mono", size = 3.1) +
  geom_text(aes(x=1.4, label = trigger), hjust = 0,
            family = "Comic Sans MS", color = "#000F55", alpha = 0.8, size = 3) +
  scale_y_reverse(breaks =2:13 - 0.5) +
  coord_cartesian(xlim = c(-0.3, 4)) +
  labs(title = "Drink Triggers",
       caption = "data from {schrute} | @ ryantimpe .com") +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "#ffff88"),
    legend.title = element_blank(),
    text = element_text(family = "mono"),
    plot.title = element_text(size = 14, face ="bold"),
    panel.background = element_rect(fill = "#F5F5F5"),
    plot.background =  element_rect(fill = "#F5F5F5"),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(color = "#0099cc"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    strip.background = element_rect(fill = "#ffff88"),
    strip.text = element_text(family = "Comic Sans MS", color = "#000F55")
  )

library(gridExtra)
m1 <- grid.arrange(arrangeGrob(grobs = list(plot_chart, plot_list),
                         ncol = 2, widths = c(2.5, 1)))

sz = 10
ggsave(filename = "DrinkingGame.png", m1, width = sz, height = sz*(9/16), device = "png")
