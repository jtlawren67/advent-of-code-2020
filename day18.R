library(stringr)
library(purrr)

options(scipen = 999)

eqs <- readLines('data/day18.txt') %>% 
  str_remove_all(' ')

## Solve String Assumes No Spaces and No Parens
solve_string <- function(x){
  if(str_detect(x, '^\\d+$'))return(as.character(x))
  
  nums <- str_split(x, '\\D')%>% unlist() %>% keep(~.x!='') %>% as.numeric()
  ops <- str_split(x, '\\d') %>% unlist() %>% keep(~.x!='') %>% str_trim()
  val = nums[1]
  for(i in 1:length(ops)){
    val = ifelse(ops[i] == "+", val + nums[i+1], val*nums[i+1])
  }
  return(as.character(val))
}

reduce_parens <- function(x){
  
  while(str_detect(x, '\\(')){
  
    # Find Paren Groups
    to_replace <- str_remove_all(x, ' ') %>% str_match_all('\\([\\d+*]+\\)') %>% unlist()
    
    #Solve Groups
    replace_with <- str_sub(to_replace, 2, -2) %>% map_chr(solve_string)
    
    #Update String
    for(i in seq_along(to_replace)){
      x <- str_replace_all(x, fixed(to_replace[i]), replace_with[i])
    }
  }
  
  return(as.numeric(solve_string(x)))
}

part1 <- sum(sapply(eqs, reduce_parens))

#############PART 2###############

reduce_parens2 <- function(x){
  
  while(str_detect(x, '\\(')){
    
    # Find Paren Groups
    to_replace <- str_remove_all(x, ' ') %>% str_match_all('\\([\\d+*]+\\)') %>% unlist()
    
    #Solve Groups
    replace_with <- str_sub(to_replace, 2, -2) %>% map_chr(handle_addition)
    
    #Update String
    for(i in seq_along(to_replace)){
      x <- str_replace_all(x, fixed(to_replace[i]), replace_with[i])
    }
  }
  
  return(as.numeric(handle_addition(x)))
}

handle_addition <- function(x){
  while(str_detect(x, '\\d+\\+\\d+')){
    to_replace <- str_remove_all(x, ' ') %>% str_match('\\d+\\+\\d+') %>% unlist()
    replace_with <- str_split(to_replace, '\\+') %>% map(., ~sum(as.numeric(.x))) %>% unlist %>% as.character()
    
    for(i in seq_along(to_replace)){
      if(i == 1) x <- str_replace(x, fixed(to_replace[i]), replace_with[i])
      else x <- gsub(paste0('([+-])',
                            str_replace(to_replace[i], fixed('+'), '\\+')), paste0('\\1',replace_with[i]), x)
    }
    
  }
  
  return(solve_string(x))
}

part2 <- sum(sapply(eqs, reduce_parens2))
