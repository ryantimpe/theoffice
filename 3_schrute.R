library(tidyverse)
library(tidytext)
library(schrute)

library(extrafont)
loadfonts(device = "win")

dat <- schrute::theoffice

twss_reg <- "that's what she said"
twss <- dat %>% 
  arrange(season, episode, index) %>% 
  filter(str_detect(tolower(text), twss_reg) | 
           lead(str_detect(tolower(text), twss_reg)) | 
           lead(str_detect(tolower(text), twss_reg), 2)) %>% 
  # unnest_tokens(sent, text, token = "sentences", drop=FALSE)%>% 
  arrange(season, episode, index)

write_delim(twss, "twss.txt", delim="\t")

# Bar chart ----

twss_caps <- tibble::tribble(
  ~character, ~twss,
  "Holly", "I'm not saying it won't be hard. But we can make it work.\nThat's what she said.",
  "Jan", "Why is this so hard? That's what she said.\nOh my God. What am I saying?",
  "Dwight", "Alright Dwight. This is huge.",
  "Michael", "	 I never know. I just say it. I say stuff like that, you know, to lighten the tension.\nWhen things sort of get hard.",
  "Creed", "Wait! Wait. Hold on. Where's the band?\n‘Cause there's just no way you guys are making this magic with just your mouths"
)

dat %>% 
  arrange(season, episode, index) %>% 
  filter(str_detect(tolower(text), twss_reg) ) %>% 
  count(character, sort = T) %>%
  left_join(twss_caps) %>% 
  mutate(character = fct_relevel(factor(character, levels = .$character),
                                 "Jan", "Pam", "Holly", "David", "Creed", "Jim", "Dwight", "Michael"),
         n2 = ifelse(n == 1, NA, n)) %>% 
  ggplot(aes(x = character, y = n)) +
  geom_col(fill = "#999999") +
  geom_vline(xintercept = (2:8 - 0.5), color = "#0099cc") +
  geom_hline(yintercept = 0, color = "#F05136") +
  geom_text(aes(label = n2), hjust = 1, family = "mono",
            size = 3, color = "#F5F5F5")+
  geom_text(aes(label = twss), y = 3.25, hjust = 0,
           family = "Comic Sans MS", color = "#000F55", size = 2.85, alpha = 0.75) +
  coord_flip() +
  labs(
    title = "That's what she said.",
    subtitle = "Jokes landed by characters in The Office (US)",
    caption = "data from {schrute} 
    @ ryantimpe .com",
    x = NULL,
    y = "That's what she said, unique occurences"
  ) +
  theme(
    text = element_text(family = "mono"),
    plot.title = element_text(size = 16, face ="bold"),
    panel.background = element_rect(fill = "#F5F5F5"),
    plot.background =  element_rect(fill = "#F5F5F5"),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(face ="bold")
  )

ggsave(filename = "TWSS.png", width = 6.5, height = 6.5*(9/16), device = "png")

#Sankey ----

library(ggalluvial)
twss_snk <- read_delim("twss.txt", delim="\t") %>% 
  drop_na()

twss_snk2 <- twss_snk %>% 
  mutate_at(vars(setup, twss),
            list(~case_when(
              . %in% c("David Brent", "Deposition", "Doctor", "Waitress") ~ "Other",
              . %in% c("Angela", "Clark", "Darryl", "David", "Gabe", "Kelly", "Kevin") ~ "Other\nEmployee",
              TRUE ~ .))) %>% 
  mutate_at(vars(setup, twss),
            list(~factor(., levels = c("Michael", "Dwight", "Jim", "Pam",
                                       "Jan", "Holly", "Creed",
                                       "Other\nEmployee", "Other")))) %>%
  count(setup, twss) %>% 
  mutate(self = setup == twss) %>% 
  rename(`Setup` = setup, `Delivery` = twss)

office_colors <- c("Michael" = "#1f497d",
                   "Dwight"  = "#f79646",
                   "Jim"     = "#00b0f0",
                   "Pam"     = "#c0504d",
                   "Jan"     = "#8064a2",
                   "Holly"   = "#4bacc5",
                   "Creed"   = "#9bbb59",
                   "Other\nEmployee"= "#948b54",
                   "Other" = "#808080")

ggplot(twss_snk2,
       aes(y = n, axis1 = Setup, axis2 = Delivery)) +
  geom_hline(yintercept = seq(2, 26, by = 2), color = "#0099cc", alpha = 0.5) +
  geom_vline(xintercept = 5/4, color = "#F05136") +
  geom_alluvium(aes(fill = Delivery), width = 1/4, alpha = 0.75) +
  geom_stratum(width = 1/4, alpha = 1, fill = "#ffff88", color = "#333333") +
  geom_text(stat = "stratum", infer.label = TRUE, 
            family = "Comic Sans MS", color = "#000F55", alpha = 0.8, size = 3.25) +
  scale_fill_manual(values = office_colors) +
  scale_x_discrete(limits = c("Setup", "Delivery"), expand = c(.05, .05)) +
  coord_cartesian(expand = F) +
  # coord_flip() +
  labs(
    title = "That's what she said.",
    subtitle = "Setup and Delivery by characters in The Office (US)",
    caption = "data from {schrute} |  @ ryantimpe .com",
    x = NULL,
    y = NULL
  ) +
  # theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(family = "mono"),
    plot.title = element_text(size = 16, face ="bold"),
    panel.background = element_rect(fill = "#F5F5F5"),
    plot.background =  element_rect(fill = "#F5F5F5"),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12, face="bold", color = "black")
  )

sz = 8
ggsave(filename = "TWSS_Sankey.png", width = sz, height = sz*(9/16), device = "png")

