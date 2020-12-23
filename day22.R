library(tidyverse)

day22 <- readLines('data/day22.txt', skipNul = T)

setup <- day22 %>%
  tibble(raw = .) %>% 
  filter(!is.na(raw) & raw != '') %>% 
  mutate(player = if_else(str_detect(raw, 'Player'), str_remove_all(raw, '\\D+'), NA_character_),
         cards = if_else(is.na(player), as.integer(str_remove_all(raw, '\\D+')), NA_integer_)
         ) %>%
  fill(player, .direction = 'down') %>% 
  filter(!is.na(cards)) %>% 
  select(-raw)

##Assign Decks
for(i in 1:n_distinct(setup$player)){
  assign(paste0('p',i), setup %>% filter(player==i) %>% pull(cards))
}
  
###Part 1
round = 0
while(length(p1) > 0 & length(p2) > 0){
  round = round + 1
  p1_plays <- p1[1]; p1 <- p1[-1]
  p2_plays <- p2[1]; p2 <- p2[-1]
  
  if(p1_plays > p2_plays) {p1 <- append(p1, c(p1_plays, p2_plays))}
  else{p2 <- append(p2, c(p2_plays, p1_plays))}
  
  winner <- append(p1, p2)
}

sum(map2_int(length(winner):1, winner, ~.x*.y))



###Part 2: Recursive Combat
p1 <- setup %>% filter(player==1) %>% pull(cards)
p2 <- setup %>% filter(player==2) %>% pull(cards)

combat <- function(p1, p2, is_subgame){
  #Initialize Game
  p1_hist <- list()
  p2_hist <- list()
  round <- 1

  #Shortcut to End Subgame since P1 can't lose if they have the highest card
  if(is_subgame == 1 & (max(c(p1, p2)) %in% p1)) return(list(1, 0, 0))
  
  #Play Game
  while(length(p1) > 0 & length(p2) > 0){

    if(any(map2_lgl(p1_hist, p2_hist, ~identical(.x, p1) & identical(.y, p2)))){
      return(list(1, p1, p2))
    }
    
    #Add Values to History
    p1_hist[[round]] <- p1
    p2_hist[[round]] <- p2
    
    p1_plays <- p1[1]; p1 <- p1[-1]
    p2_plays <- p2[1]; p2 <- p2[-1]
    
    if(length(p1)>=p1_plays & length(p2)>=p2_plays){
      winner = combat(p1[1:p1_plays], p2[1:p2_plays], 1)
      if(winner[[1]] == 1){p1 <- append(p1, c(p1_plays, p2_plays))}
      else{p2 <- append(p2, c(p2_plays, p1_plays))}
      
    } else {
      #Regular Rules
      if(p1_plays > p2_plays) {p1 <- append(p1, c(p1_plays, p2_plays))}
      else{p2 <- append(p2, c(p2_plays, p1_plays))}
    }
    
    round = round + 1
    
  }
  
  #Game is Over
  if(length(p1) > 0){return(list(1, p1, p2))}
  else{return(list(2, p1, p2))}
}

results <- combat(p1, p2, 0)

winner <- append(results[[2]], results[[3]])
sum(map2_int(length(winner):1, winner, ~.x*.y))
