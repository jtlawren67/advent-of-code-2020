library(tidyverse)
library(tictoc)
options(scipen = 999)

d13 <- readLines('data/day13.txt')

#Part 1
d13_clean <- tibble(
  ts = d13[1],
  bus_id = strsplit(d13[2], ',') %>% unlist()
) %>%
  filter(bus_id != 'x') %>% 
  mutate(across(everything(), as.numeric)) %>% 
  mutate(waittime = bus_id - (ts %% bus_id)) %>%
  filter(waittime == min(waittime)) %>% 
  mutate(ans = waittime*bus_id) %>% 
  pull(ans)

## Part 2
pt2 <- tibble(bus = d13[2] %>% strsplit(',') %>% unlist) %>% 
  mutate(offset = row_number()-1) %>%
  filter(bus != 'x') %>%
  mutate(across(everything(), as.numeric))

##Initialize Sequence
inc = pt2 %>% slice(1) %>% pull(bus)
t = 0 #Starting Point

for(i in 2:nrow(pt2)){
  #Set Target Parameters
  target_bus <- pt2 %>% slice(i) %>% pull(bus)
  target_offset <- pt2 %>% slice(i) %>% pull(offset)
  
  #Iterate to Find Solution
  while((t+target_offset) %% target_bus !=0){
    t = t + inc
  }
  
  # Increase Step based on Solution
  inc = inc * target_bus
}

print(t)
