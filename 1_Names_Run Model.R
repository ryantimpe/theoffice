# code that actually runs all the functions
library(readr)
library(stringr)
library(dplyr)
library(purrr)
library(tokenizers)
library(keras)

source("1_Names_Load Data Functions.R")
source("1_Names_Model Functions.R")

# Set the max length of the _saurs
max_length <- 100

# format the _saurs into the appropriate datapoints
data <- dw_office2 %>% 
  pull(text) %>%
  # add_stop() %>%
  split_into_subs() %>%
  fill_data(max_length = max_length)

# create the vector of characters in the data
characters <- dw_office2 %>% 
  select(text) %>% 
  unnest_tokens(character, text, to_lower = FALSE, 
                token = "characters", strip_non_alphanum = FALSE) %>% 
  pull(character) %>% unique()

# make the vector/3D-array as the y and x data for keras
vectors <- vectorize(data, characters, max_length)

# initialize the model
model <- create_model(characters, max_length)

# iterate the model
iterate_model(model, characters, max_length, diversity, vectors, 50)

# create the result
result <- 
  runif(500,0.5,1.5) %>% #randomly choose diversity for each plate
  map_chr(~ generate_saur(model, characters, max_length, .x))

result_df <- result %>% 
  data_frame(kerasaur = .) %>%
  distinct %>%
  filter(!is.na(kerasaur), kerasaur != "") %>%
  anti_join(data_frame(kerasaur = kerasaurs), by="kerasaur") %>%  # remove actual animals
  mutate(kerasaur = tools::toTitleCase(kerasaur))


write.csv(result_df, "Output/2_Names_Output_ptero_highertemp.csv", row.names = F, quote = F)
