library(tidyverse)

#Building Mapping of Bags
d7 <- tibble(orig = read_lines('data/day7.txt')) %>% 
  mutate(orig = str_remove_all(orig, '\\.') %>% str_replace_all('bags', 'bag')) %>% 
  separate(orig, c('bag', 'contains'), sep = ' contain ', remove = F) %>% 
  separate_rows(contains, sep = ',') %>% 
  mutate(
    quantity = if_else(
      str_detect(contains, 'no other bag'), 0, parse_number(contains)
      ),
    contents = str_trim(str_remove_all(contains, '\\d+'))
  ) %>% 
  select(bag, quantity, contents)

##Part 1 - How many bag colors can eventually contain at least one shiny gold bag?
##Needed to get a hint from David Robinson (https://twitter.com/drob/status/1336003816395845632)
##Not proud of it... but credit where credit is due

what_contains_me <- function(bags, ...){
  return(
    d7 %>% filter(contents %in% {{bags}}) %>% pull(bag)
  )
}


part1 <- accumulate(1:100, what_contains_me, .init = 'shiny gold bag') %>% 
  unlist() %>% 
  discard(~.x == 'shiny gold bag') %>% 
  n_distinct()

##Part 2 - Done without assistance!

how_many_bags <- function(bag){
  
  sub_grp <- d7 %>% filter(bag == {{bag}}) 
  
  if(bag == 'no other bag'){
    return(1)
  } else{
    
    total = 1
    
    for(i in 1:nrow(sub_grp)){
            total = total + 
        (sub_grp %>% slice(i) %>% pull(quantity)) * 
          how_many_bags(sub_grp %>% slice(i) %>% pull(contents))
    }
    
    return(total)
  }
}

how_many_bags('shiny gold bag')-1

