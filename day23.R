library(tidyverse)
options(scipen = 999)

input <- '315679824'

circle <- input %>% str_split('') %>% unlist() %>% as.numeric
upper_limit <- length(circle)

##############
##### PART 1
##############

find_destination <- function(x){
  if((x-1) %in% circle){return(x-1)}
  else if((x-1) == 0){return(find_destination(upper_limit+1))}
  else {return(find_destination(x-1))}
}

current_position = 1

for(i in 1:100){
  
  current_cup <- circle[current_position]
  pickup_positions <- map_dbl(current_position+(1:3), 
                              ~ifelse(.x > upper_limit, .x %% upper_limit, .x))
  
  pickup <- circle[pickup_positions]
  circle <- circle[-pickup_positions]
  
  destination_cup <- find_destination(as.numeric(current_cup))
  destination_idx <- which(circle==destination_cup)

  circle <- append(circle, pickup, after = destination_idx)
  
  current_position <- ifelse(which(circle == current_cup)==upper_limit,
                             1,
                             which(circle==current_cup) + 1)

}

answer_order <- rep(NA_integer_, upper_limit)
order_idx = 1

while(any(is.na(answer_order))){
  if(all(is.na(answer_order))){
    insert_idx <- ifelse(which(circle == 1)==upper_limit, 1, which(circle==1)+1)
    answer_order[order_idx] <- circle[insert_idx]
  }
  else{
    insert_idx <- ifelse((insert_idx + 1) > upper_limit, 1, insert_idx + 1)
    answer_order[order_idx] <- circle[insert_idx]
  }
  order_idx = order_idx + 1
}

paste(answer_order[1:(length(answer_order)-1)], collapse = '')

###############
#######PART 2
###############

p2 <- "315679824" %>% str_split('') %>% unlist() %>% as.numeric()
circle <- c(p2, seq(max(p2)+1,1e6))
ncups <- length(circle)

#Build Structure for Neighbors
neighbors <- integer(ncups)
for(i in seq.int(1, ncups-1)){
  neighbors[circle[i]] <- circle[i+1]
}
neighbors[circle[ncups]] <- circle[1]

current_cup <- circle[1]

get_destination <- function(current_cup){
  if(current_cup == 1) return(ncups)
  else return(current_cup - 1)
}

for(i in 1:1e7){
  
  destination <- get_destination(current_cup)
  
  n1 <- neighbors[current_cup]
  n2 <- neighbors[n1]
  n3 <- neighbors[n2]
  
  while(destination %in% c(n1, n2, n3)){
    destination <- get_destination(destination)
  }
  
  neighbors[current_cup] <- neighbors[n3]
  neighbors[n3] <- neighbors[destination]
  neighbors[destination] <- n1
  
  current_cup <- neighbors[current_cup]
}

neighbors[1]*neighbors[neighbors[1]]