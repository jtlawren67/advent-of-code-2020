library(tidyverse)

d6 <- read_csv('data/day6.txt', col_name = 'orig', skip_empty_rows = F) %>% 
  mutate(grp_id = cumsum(is.na(orig))) %>%  
  filter(!is.na(orig))


##Part 1 - What is the sum total of the number of unique questions answered yes per group
d6 %>% 
  group_by(grp_id) %>%
  summarise(
    all_answers = paste(orig, collapse = '') 
  ) %>% 
  mutate(unique_yes = map_int(all_answers, ~length(unique(unlist(strsplit(.x, '')))))) %>%
  pull(unique_yes) %>% 
  sum()

##Part 2 - What are the number of questions WHERE every person in the group answers yes
d6 %>%
  mutate(person_id = 1:n(),
         split_answers = strsplit(orig, '')) %>%
  unnest(split_answers) %>%
  group_by(grp_id) %>% 
  mutate(people_in_grp = n_distinct(person_id)) %>% 
  count(grp_id, people_in_grp, split_answers, name = 'people_with_answers') %>% 
  ungroup() %>% 
  summarize(
    all_answered_yes = sum(people_with_answers == people_in_grp)
  ) 