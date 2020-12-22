library(tidyverse)

d21 <- readLines('data/day21.txt')

ingrediants <- d21 %>%
  tibble(input = d21) %>%
  extract(input, into = c('ingrediants', 'allergens'), 
          regex = '(.+) \\(contains (.+)\\)') %>% 
  mutate(item_id = row_number()) %>% 
  separate_rows(ingrediants, sep = ' ') %>% 
  separate_rows(allergens, sep = ', ')

r_ing <- ingrediants %>% distinct(item_id, ingrediants)
r_all <- ingrediants %>% distinct(item_id, allergens)

###Part 1
to_solve <- unique(ingrediants$allergens)
ing_list <- r_ing
all_list <- r_all
solution <- tibble()
s_ix = 1
while(length(to_solve) > 0){
  
  #Grab An Allergen
  to_check <- all_list %>% 
    count(allergens, sort = T) %>% 
    slice(s_ix)

  recs <- ing_list %>% 
    filter(item_id %in% (all_list %>% 
                           filter(allergens == to_check$allergens) %>% 
                           pull(item_id))) %>% 
    count(ingrediants) %>%
    filter(n == to_check$n)
  
  if(nrow(recs) == 1){
  
    #Update Items
    solution <- solution %>% 
      bind_rows(
        tibble(
          ingrediants = recs$ingrediants,
          allergent = to_check$allergens
        )
      )
  
    ing_list <- ing_list %>% filter(ingrediants != recs$ingrediants)
    all_list <- all_list %>% filter(allergens != to_check$allergens)
    to_solve <- setdiff(to_solve, to_check$allergens)
    
    s_ix = 1
  }
  else{
    s_ix = s_ix + 1
  }
}

##Part 1
r_ing %>% 
  anti_join(solution, by = "ingrediants") %>% 
  nrow()

##Part 2
solution %>% 
  arrange(allergent) %>% 
  summarize(
    lst = paste(ingrediants, collapse = ',')
  )
  
  