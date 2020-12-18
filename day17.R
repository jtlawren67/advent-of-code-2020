library(tidyverse)

input <- readLines('data/day17.txt')


# Create Coordinates for Actives
d17 <- input %>% tibble(val = .) %>% 
  mutate(z = 0,
         x = row_number()-1) %>% 
  separate_rows(val, sep = '') %>%
  filter(val != '') %>% 
  group_by(z, x) %>% 
  mutate(y = (1:n())-1) %>% 
  ungroup()


get_neighbors <- function(x, y, z){
  expand.grid(
    x = (x-1):(x+1),
    y = (y-1):(y+1),
    z = (z-1):(z+1)
  ) %>% 
    filter(pmax(abs(x-{{x}}), abs(y-{{y}}), abs(z-{{z}})) == 1) %>%
    inner_join(actives, by = c('x', 'y', 'z')) %>%
    nrow()
}

###Part 1
actives <- d17 %>% 
  filter(val == "#") 

n_x <- range(d17$x)
n_y <- range(d17$y)
n_z <- range(d17$z)

for(i in 1:6){
  print(paste(Sys.time(), ":", i))
  
  #Update Dimensions
  n_x = c(n_x[1]-1, n_x[2]+1)
  n_y = c(n_y[1]-1, n_y[2]+1)
  n_z = c(n_z[1]-1, n_z[2]+1)
  
  ##Build Next Step Candidate Grid
  candidate_grid <- expand.grid(
    x = seq(n_x[1], n_x[2]),
    y = seq(n_y[1], n_y[2]),
    z = seq(n_z[1], n_z[2])
  ) %>% 
    left_join(actives, by = c("x", "y", "z")) %>% 
    mutate(active_neighbors = pmap_int(
      list(x = x, y = y, z = z), get_neighbors
    )) %>% 
    mutate(
      val = case_when(
        val == "#" & active_neighbors %in% c(2, 3) ~ "#",
        is.na(val) & active_neighbors == 3 ~ "#",
        TRUE ~ NA_character_
      )
    ) %>%
    select(x, y, z, val) %>% 
    filter(val == "#")
  
  actives <- candidate_grid

}

nrow(actives)

###Part 2
get_neighbors2 <- function(x, y, z, w){
  expand.grid(
    x = (x-1):(x+1),
    y = (y-1):(y+1),
    z = (z-1):(z+1),
    w = (w-1):(w+1)
  ) %>% 
    filter(pmax(abs(x-{{x}}), abs(y-{{y}}), abs(z-{{z}}), abs(w-{{w}})) == 1) %>%
    inner_join(actives, by = c('x', 'y', 'z', 'w')) %>%
    nrow()
}

actives <- d17 %>% 
  mutate(w = 0) %>% 
  filter(val == "#") 

n_x <- range(d17$x)
n_y <- range(d17$y)
n_z <- range(d17$z)
n_w <- c(0, 0)

for(i in 1:6){
  print(paste(Sys.time(), ":", i))
  
  #Update Dimensions
  n_x = c(n_x[1]-1, n_x[2]+1)
  n_y = c(n_y[1]-1, n_y[2]+1)
  n_z = c(n_z[1]-1, n_z[2]+1)
  n_w = c(n_w[1]-1, n_w[2]+1)
  
  ##Build Next Step Candidate Grid
  candidate_grid <- expand.grid(
    x = seq(n_x[1], n_x[2]),
    y = seq(n_y[1], n_y[2]),
    z = seq(n_z[1], n_z[2]),
    w = seq(n_w[1], n_w[2])
  ) %>% 
    left_join(actives, by = c("x", "y", "z", "w")) %>% 
    mutate(active_neighbors = pmap_int(
      list(x = x, y = y, z = z, w= w), get_neighbors2
    )) %>% 
    mutate(
      val = case_when(
        val == "#" & active_neighbors %in% c(2, 3) ~ "#",
        is.na(val) & active_neighbors == 3 ~ "#",
        TRUE ~ NA_character_
      )
    ) %>%
    select(x, y, z, w, val) %>% 
    filter(val == "#")
  
  actives <- candidate_grid
  
}

nrow(actives)