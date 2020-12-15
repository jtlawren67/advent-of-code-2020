library(tidyverse)
library(magrittr)
library(binaryLogic)
options(scipen = 999)

d14 <- readLines('data/day14.txt')

#Initialize memory
mem <- list()

for(i in 1:length(d14)){

  if(str_detect(d14[i], 'mask')){
    mask = str_replace(d14[i], 'mask = ', '') %>%
         str_split('') %>% 
         unlist()

  }
  else if (str_detect(d14[i], 'mem')){
    matches = str_extract_all(d14[i], '\\d+', simplify = T)
    
    #Convert Number to 36-bit Binary
    bit_array <- as.binary(as.numeric(matches[2]), n = 36) %>% str_split('') %>% unlist()
    
    #Apply Mask
    result <- ifelse(mask=='X', bit_array, mask)
    
    #Convert Back to Integer
    result_num <- sum(2^(which(rev(result) == 1)-1))
    
    #Store Result
    mem[matches[1]] = result_num
    
  }
  
}

##Part 1
purrr::reduce(mem, sum)

##Part 2
mem <- tibble::tibble(
  mem_id = NULL,
  value = NULL
)

for(i in 1:length(d14)){
  
  if(str_detect(d14[i], 'mask')){
    mask = str_replace(d14[i], 'mask = ', '') %>%
      str_split('') %>% 
      unlist()
    
  }
  else if(str_detect(d14[i], 'mem')){
    matches = str_extract_all(d14[i], '\\d+', simplify = T)
    
    #Get Binary of Memory Input
    bit_array <- as.binary(as.numeric(matches[1]), n = 36) %>% str_split('') %>% unlist()
    
    #Apply the Mask
    result <- dplyr::case_when(
      mask == '1' ~ '1',
      mask == '0' ~ bit_array,
      mask == 'X' ~ 'X'
    )
    
    #Generate Combinations for Floating
    floating_idx <- which(result == 'X', arr.ind = T)
    combinations <- gtools::permutations(n = 2, r = length(floating_idx), 
                         v = c('0', '1'), repeats.allowed = T) 
    
    insert_dt <- map_dfr(1:nrow(combinations), function(r){
      tmp <- result
      tmp[floating_idx] <- combinations[r, ]
      result_num <- sum(2^(which(rev(tmp) == 1)-1))
      return(tibble(mem_id = result_num, val = matches[2]))
    })
    
    mem <- rbind(mem, insert_dt)

  }
}

mem %>% 
  mutate(rid = row_number()) %>% 
  group_by(mem_id) %>% 
  slice_max(rid) %>% 
  ungroup() %>% 
  summarize(val = sum(as.numeric(val)))

