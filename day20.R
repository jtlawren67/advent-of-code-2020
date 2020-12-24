library(tidyverse)
options(scipen = 999)

input <- readLines('data/day20.txt', skipNul = T)

###Structure Data

d20 <- tibble(orig = input) %>% 
  filter(!is.na(orig)) %>% 
  mutate(tile = str_remove_all(orig, '\\D+'),
         tile = na_if(tile, '')) %>% 
  fill(tile, .direction = 'down') %>% 
  filter(!str_detect(orig, 'Tile')) %>% 
  group_by(tile) %>% 
  mutate(row_id = row_number()) %>% 
  mutate(value = str_split(orig, '')) %>% 
  unnest(value) %>% 
  group_by(tile, row_id) %>% 
  mutate(col_id = 1:n()) %>% 
  ungroup() %>% 
  group_by(tile) %>% 
  summarize(
    top = paste(if_else(row_id == 1, value, ''), collapse = ''),
    bottom = paste(if_else(row_id == max(row_id), value, ''), collapse = ''),
    left = paste(if_else(col_id == 1, value, ''), collapse = ''),
    right = paste(if_else(col_id == max(col_id), value, ''), collapse = '')
  ) %>% 
  mutate(across(-tile, stringi::stri_reverse, .names = 'reverse_{col}')) %>% 
  pivot_longer(cols = -tile,
               names_to = 'location',
               values_to = 'value') 

all_tiles = crossing(
  d20,
  d20 %>% rename_with(~paste0('cmp_', .x))
) %>% 
  filter(tile != cmp_tile,
         value == cmp_value) %>% 
  group_by(tile) %>% 
  summarize(
    matches = n_distinct(cmp_tile)
  ) 

all_tiles %>% filter(matches == 2) %>% pull(tile) %>% as.numeric %>% reduce(`*`)


#######PART 2########
match_map <- crossing(
  d20,
  d20 %>% rename_with(~paste0('cmp_', .x))
) %>% 
  filter(tile != cmp_tile,
         value == cmp_value) %>% 
  group_by(tile) %>% 
  mutate(neighbors = n_distinct(cmp_tile),
         potential_matches = paste(unique(cmp_tile), collapse = ', ')
         )%>% 
  ungroup() %>% 
  select(tile, cmp_tile, neighbors) %>% 
  distinct()

all_values <- tibble(orig = input) %>% 
  filter(!is.na(orig)) %>% 
  mutate(tile = str_remove_all(orig, '\\D+'),
         tile = na_if(tile, '')) %>% 
  fill(tile, .direction = 'down') %>% 
  filter(!str_detect(orig, 'Tile')) %>% 
  group_by(tile) %>% 
  mutate(row_id = row_number()) %>% 
  mutate(value = str_split(orig, '')) %>% 
  unnest(value) %>% 
  group_by(tile, row_id) %>% 
  mutate(col_id = 1:n())


  
##Assemble Puzzle to figure out how tiles match to each other
tile_arrangement <- matrix(rep(NA_character_, nrow(all_tiles)), nrow=sqrt(nrow(all_tiles)))

###Figure out what pieces need to go where
for(i in 1:nrow(tile_arrangement)){
  for (j in 1:ncol(tile_arrangement)){

    if(i==1 & j == 1){
      #If Upper Left Corner... Pick a corner id... find its neighbors and start trying thing
      tile_arrangement[i, j] <- match_map %>% 
        filter(neighbors ==2) %>% 
        slice(1) %>% 
        pull(tile)
    }
    else if(i == 1){
      #Look for a Neighboring Piece with only 3 Potential Buddies and the piece to the left is what we picked
      tile_arrangement[i, j] <- match_map %>% 
        filter(neighbors <= 3, cmp_tile == (tile_arrangement[i, j-1])) %>%
        slice(1) %>% 
        pull(tile)
    }
    else {
      tile_arrangement[i, j] <- match_map %>% 
        filter(cmp_tile == (tile_arrangement[i-1, j])) %>%
        slice(1) %>% 
        pull(tile)
    }

    match_map <- match_map %>% filter(tile != tile_arrangement[i, j])
  }
}


get_matrix <- function(tile_id){
  sub <- all_values %>% 
    filter(tile == tile_id)
  
  return(
    matrix(sub %>% pull(value),
           nrow = sub %>% pull(row_id) %>% max(),
           byrow = T)
  )
}

rotate <- function(x) t(apply(x, 2, rev))
flip_y <- function(x) apply(x, 2, rev)

check_matrix <- function(m1, m2, direction){
  if(direction == 'up') return(all(m1[1, ] == m2[nrow(m2), ]))
  else if(direction == 'down') return(all(m1[nrow(m1), ] == m2[1, ]))
  else if(direction == 'left') return(all(m1[, 1] == m2[, ncol(m2)]))
  else if(direction == 'right') return(all(m1[, ncol(m1)]==m2[, 1]))
}

# Given 3 Matrices What is the Solvable State for the Target
find_rotation <- function(target, up = NA, down = NA, left = NA, right = NA){
  
  if(all(!is.na(up))){v_matrix = up}else{v_matrix = down}
  if(all(!is.na(left))){h_matrix = left}else{h_matrix = right}
  
  v_direction <- ifelse(all(!is.na(up)), 'up', 'down')
  h_direction <- ifelse(all(!is.na(left)), 'left', 'right')
  
  for(vr in 1:8){
    for(hr in 1:8){
      for(tr in 1:8){
        if(check_matrix(target, v_matrix, v_direction) & 
           check_matrix(target, h_matrix, h_direction)) return(target)
        
        target = rotate(target)
        if(tr == 4) target = flip_y(target)
      }
      h_matrix <- rotate(h_matrix)
      if(hr == 4) h_matrix <- flip_y(h_matrix)
    }
    v_matrix <- rotate(v_matrix)
    if(vr == 4) v_matrix <- flip_y(v_matrix)
  }
}


###Actually Build the Puzzle
for(i in 1:nrow(tile_arrangement)){
  for(j in 1:ncol(tile_arrangement)){
    if(i == 1 & j == 1){
      target = find_rotation(get_matrix(tile_arrangement[i, j]),
                             right = get_matrix(tile_arrangement[i, j+1]),
                             down = get_matrix(tile_arrangement[i+1, j])
      )
      r_solution <- target
    }
    else if(i == 1){
      target = find_rotation(get_matrix(tile_arrangement[i,j]),
                             left = get_matrix(tile_arrangement[i, j-1]),
                             down = get_matrix(tile_arrangement[i+1, j])
                             )
      r_solution <- cbind(r_solution, target)
    }
    else if(j == 1){
      target = find_rotation(get_matrix(tile_arrangement[i,j]),
                             up = get_matrix(tile_arrangement[i-1, j]),
                             right = get_matrix(tile_arrangement[i, j+1])
      )
      r_solution <- target
    }
    else{
      target = find_rotation(get_matrix(tile_arrangement[i,j]),
                             up = get_matrix(tile_arrangement[i-1, j]),
                             left = get_matrix(tile_arrangement[i, j-1])
      )
      r_solution <- cbind(r_solution, target)
    }
  }
  if(i == 1) c_solution <- r_solution
  else c_solution <- rbind(c_solution, r_solution)
}

#Kill The Borders
full <- c_solution[
  discard(1:nrow(c_solution), ~.x %% 10 %in% c(0, 1)),
  discard(1:ncol(c_solution), ~.x %% 10 %in% c(0, 1))
]

#Bring in the Monster String
monster_string <- c("                  # ",
                    "#    ##    ##    ###",
                    " #  #  #  #  #  #   "
) %>% str_replace_all(' ', '.')

###Check for Monsters by Doing the 8 Rotations of the Grid and Checkign
final_rotation <- function(full){
  for(r in 1:8){

    #Check for Monster
    for(i in 1:(nrow(full)-2)){
      for(j in 1:(ncol(full)-nchar(monster_string)[1])){
        sub_mtx <- paste(t(full[i:(i+2), j:(j+19)]), collapse = '')
        mstr_str <- paste(monster_string, collapse = '')
        chk = str_detect(sub_mtx, mstr_str)
        
        if(chk == T)return(full)
      }
    }
  
    #Else Transform
    full <- rotate(full)
    if(r == 4) full <- flip_y(full)
  }
}

final <- final_rotation(full)

mstr_grids <- tibble()
##Where are monster grids?
for(i in 1:(nrow(final)-2)){
  for(j in 1:(ncol(final)-nchar(monster_string)[1])){
    sub_mtx <- paste(t(final[i:(i+2), j:(j+19)]), collapse = '')
    mstr_str <- paste(monster_string, collapse = '')
    if(str_detect(sub_mtx, mstr_str)){
      mstr_grids <- mstr_grids %>% 
        bind_rows(tibble(r_start = i,r_end = i+2,c_start = j, c_end = j+19))
    }
  }
}

str_count(paste(t(final), collapse = ''), "#") - #All Hashs in Grid
(str_count(paste(monster_string, collapse = ''), "#") * #Hashes in Monster)
   nrow(mstr_grids)) #Times Instances of Monster
