library(tidyverse)

input <- readLines('data/day24.txt')

###############
#### PART 1 ###
###############

tile_list <- map_dfr(1:length(input), function(i){
  tibble(
    instr = input[i] %>% str_replace_all('(e|w)', '\\10') %>% str_split('0')
  ) %>% unnest(instr) %>% 
    filter(instr != '') %>% 
    mutate(
     x = if_else(instr %in% c('se', 'e'), 1, if_else(instr %in% c('nw', 'w'), -1, 0)),
     y = if_else(instr %in% c('ne', 'e'), 1, if_else(instr %in% c('sw', 'w'), -1, 0)),
     z = if_else(instr %in% c('ne', 'nw'), 1, if_else(instr %in% c('se', 'sw'), -1, 0))
    ) %>% 
    summarise(across(c(x, y, z), sum)) %>%
    mutate(instruction_id = i)
  }
)

#How many black tiles are there?
tile_list %>% 
  count(x, y, z) %>%
  mutate(color = if_else(n %% 2 == 1, 'black', 'white'))  %>%
  filter(color == 'black') %>% 
  nrow()
     
##############
### PART 2
#############

neighbors <- tribble(
  ~x_delta, ~y_delta, ~z_delta,
  -1,0,1,
  0,1,1,
  0,-1,-1,
  1,1,0,
  1,0,-1,
  -1,-1,0
)

run_day <- function(black_tiles){
  #Get All Neighbor Tiles
  coords <- black_tiles %>% select(x0 = x, y0 = y, z0 =z) %>%
    crossing(neighbors) %>%
    mutate(
      x = x0 + x_delta,
      y = y0 + y_delta,
      z = z0 + z_delta
    ) %>% 
    left_join(black_tiles %>% mutate(color = 'black'), by = c('x', 'y', 'z')) %>% 
    replace_na(list(color = 'white')) %>% 
    count(x, y, z, color) %>% 
    filter((color == 'black' & n %in% c(1, 2)) | (color=='white' & n == 2))
}

black_tiles <- tile_list %>% 
  count(x, y, z) %>% 
  filter(n %% 2 ==1) 

for(i in 1:100){
  black_tiles <- run_day(black_tiles %>% select(-n))
  if(i %% 10 == 0){
    print(paste(Sys.time(), "Day ", i,":", nrow(black_tiles)))
  }
}
