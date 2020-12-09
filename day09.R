library(tidyverse)

d8 <- read_lines('data/day9.txt') %>% as.numeric

#Find a number where no pair from the prior 25 will add up to it

is_valid <- function(id, preamble, dt){
  
  #Don't Need a Number for the First Five Numbers
  if(id <= preamble) return(0)
  
  candidates <- crossing(
    val1 = dt[(id-preamble):(id-1)],
    val2 = dt[(id-preamble):(id-1)]
  ) %>% 
    filter(val1 != val2,
           val1 + val2 == dt[id])
  
  if(nrow(candidates) > 0){
    return(0)
  } 
  else {
    return(dt[id])
  }
}

part1 <- map_dbl(1:length(d8), ~is_valid(.x, 25, d8)) 
discard(part1, ~.x == 0)

# Part 2 - 
#you must find a contiguous set of at least two numbers in your list which sum 
#to the invalid number from step 1.

part2 <- crossing(
  starting_point = 1:length(d8),
  window_size   = 2:length(d8)
) %>% 
  mutate(vals = map2_dbl(starting_point,
                         window_size,
                         ~sum(d8[.x:(.x + .y - 1)]))) %>% 
  filter(vals == 69316178)

#To find the encryption weakness, 
#add together the smallest and largest number in this contiguous range
sum(range(d8[part2$starting_point:(part2$starting_point + part2$window_size-  1)]))