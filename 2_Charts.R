script <- readRDS("data/Pro_TheOffice.rds") %>% 
  select(-line_text, -text, -end_of_scene) %>% 
  mutate(line = str_remove_all(line, "�"))

#That's what she said ----
twss_detect <- "hat she sa"

twss <- script %>% 
  filter(str_detect(line, twss_detect) | str_detect(lead(line), twss_detect)) %>% 
  #These steps drop the preceding lines if they are in a different scene...
  #... usually the speaker does the joke solo
  mutate(twss = ifelse(str_detect(line, "hat she sa"), id, NA)) %>% 
  fill(twss, .direction = "up") %>% 
  group_by(twss) %>% 
  filter(scene == last(scene)) %>% 
  ungroup()

twss %>% 
  filter(id == twss) %>% 
  count(speaker, sort = TRUE)

twss %>% 
  filter(id == twss) %>% 
  count(speaker, season, sort = TRUE) %>% 
  complete(season = 1:9) %>% 
  spread(season, n, fill = "") %>% 
  drop_na()

#That's what * said ----
twqs_detect <- "hat's what(.*)said"

twqs <- script %>% 
  filter(str_detect(line, twqs_detect) | str_detect(lead(line), twqs_detect)) %>% 
  #These ones arent usually jokes
  filter(!(str_detect(line, "what I said") | str_detect(lead(line), "what I said"))) %>% 
  filter(!(str_detect(line, "what you said") | str_detect(lead(line), "what you said"))) %>% 
  #These steps drop the preceding lines if they are in a different scene...
  #... usually the speaker does the joke solo
  mutate(twss = ifelse(str_detect(line, twqs_detect), id, NA)) %>% 
  fill(twss, .direction = "up") %>% 
  group_by(twss) %>% 
  filter(scene == last(scene)) %>% 
  ungroup() %>% 
  group_by(twss) %>% 
  mutate(Speaker_setup = first(speaker),
         Speaker_punchline = last(speaker)) %>% 
  ungroup()


twqs_solo <- twqs %>% 
  filter(Speaker_punchline == "Michael") %>% 
  group_by(twss) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

# Boobs ----

bbs_detect <- c("boob", "breast", " chest", "knocker", "honker", " melon")
bbs_detect2 <- paste(bbs_detect, collapse = "|")

bbs <- script %>% 
  filter(str_detect(line, bbs_detect2))

bbs %>% 
  count(speaker, season, sort = TRUE) %>% 
  complete(season = 1:9) %>% 
  spread(season, n, fill = 0) %>% 
  drop_na() %>% 
  mutate(total = rowSums(.[-1])) %>% 
  arrange(desc(total)) %>% 
  select(-total)

# Season sentiment ----

seas_sent <- script %>% 
  unnest_tokens(word, line) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(season, sentiment) %>% 
  spread(sentiment, n) %>% 
  mutate(pos = positive / (positive + negative))

# Character sentiment ----

char_sent <- script %>% 
  group_by(speaker) %>%
  filter(n() > 400) %>% 
  ungroup() %>% 
  unnest_tokens(word, line) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(speaker, sentiment) %>% 
  spread(sentiment, n) %>% 
  mutate(pos = positive / (positive + negative))

main_chars <- char_sent$speaker

# What Michael says about others ----

mich_sent <- script %>% 
  filter(speaker == "Michael") %>% 
  #Find lines where Michael names people
  filter(str_detect(line, paste(main_chars, collapse = "|"))) %>% 
  mutate(about = str_extract_all(line, paste(main_chars, collapse = "|"))) %>% 
  unnest(about) %>% 
  distinct() %>% 
  #Get sentiments by subject
  unnest_tokens(word, line) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing"))  %>% 
  count(about, sentiment) %>% 
  spread(sentiment, n) %>% 
  mutate(pos = positive / (positive + negative)) %>% 
  drop_na() %>% 
  arrange(pos)

mich_sent_words <- script %>% 
  filter(speaker == "Michael") %>% 
  #Find lines where Michael names people
  filter(str_detect(line, paste(main_chars, collapse = "|"))) %>% 
  mutate(about = str_extract_all(line, paste(main_chars, collapse = "|"))) %>% 
  unnest(about) %>% 
  distinct() %>% 
  #Get sentiments by subject
  unnest_tokens(word, line) %>% 
  anti_join(stop_words) %>% 
  inner_join(get_sentiments("bing"))  %>% 
  count(about, word, sentiment) %>% 
  group_by(about) %>% 
  top_n(5, n) %>% 
  ungroup()

# Words more likely said by Michael ----

mich_words <- script %>% 
  mutate(speaker_m = ifelse(speaker == "Michael", speaker, "Other")) %>% 
  unnest_tokens(word, line) %>% 
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>% 
  #Only look at words said a min number of times
  group_by(word) %>% 
  filter(n() >= 30) %>% 
  ungroup() %>% 
  count(speaker_m, word) %>% 
  spread(speaker_m, n, fill = 0) %>% 
  filter(!str_detect(word, "^[0-9]")) %>% 
  mutate_if(is.numeric, funs((. + 1)/(sum(.) + 1))) %>% 
  mutate(Score = log(Michael / Other)) %>% 
  group_by(sign(Score)) %>% 
  top_n(10, abs(Score)) %>% 
  ungroup()

