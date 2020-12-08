library(tidyverse)

d8 <- read_csv('data/day8.txt', col_names = 'orig') %>% 
  mutate(rule_id = row_number(),
         op = str_sub(orig, 1, 3),
         arg = parse_number(orig))

##Part 1 - What is the value before the program runs a duplicated row?

run_program <- function(instructions){
  #Initialize Values
  ids <- c()
  value = 0
  run_id = 1
  
  #while we're not running a duplicated id
  while(!run_id %in% ids){
    
    #Add the id to our list
    ids <- c(ids, run_id)
    
    #Subset Data
    sub_df <- instructions %>% slice(run_id)
    
    #Check Value and set next ID
    if(sub_df$op == 'acc'){
      value = value + sub_df$arg
      run_id = run_id + 1
    }
    else if (sub_df$op == 'jmp') {
      run_id = run_id + sub_df$arg
    } 
    else {
      run_id = run_id + 1
    }
    
    #If Reaches All Instructions Return the Value
    if(run_id == nrow(d8) + 1) return(c('done', value))
    
  }
  
  return(c('failed', value))
}

print(run_program(d8))

## Part 2 - Change one nop to jmp or one jmp to nop so that the program finishes
## What is the value when the program finishes

rules_to_investigate <- d8 %>% 
  filter(op != 'acc',
         !(op == 'nop' & arg == 0)) %>% pull(rule_id)


flip_rule <- function(op) {
  if(op == 'acc') return('acc')
  if(op == 'jmp') return('nop')
  if(op == 'nop') return('jmp')
}

for(target_rule in rules_to_investigate){
  value = run_program(
    d8 %>% mutate(op = if_else(rule_id == target_rule, flip_rule(op), op))
  )
  #If we found the answer we don't need to run more
  if(value[1] == 'done') break
}

print(value)
