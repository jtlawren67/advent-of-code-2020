library(tidyverse)

input <- readLines('data/day19.txt')

##Grab Messages
msgs <- keep(input, ~str_detect(.x, '^[ab]+'))
##Grab Rules
rules <- setdiff(input, msgs) %>% str_remove_all('\\"') %>% 
  tibble(x = .) %>% 
  extract(x, c("rule_number", "rule"), regex = '(\\d+): (.+)') %>% 
  filter(!is.na(rule_number)) %>% 
  mutate(solution = rule,
         is_solved = str_count(rule)==1)

#Iterate Until All Rules are Solved
while(!all(rules$is_solved)){
  #Get Rules That Have Currently Been Solved
  currently_solved <- rules %>% filter(is_solved == T) %>% pull(rule_number)
  replacement = rules %>% filter(is_solved == T) %>% pull(solution)
  
  #replace solvable solutions with known solutions
  for(i in seq_along(currently_solved)){
    
      rules$solution <- str_replace_all(
        rules$solution,
        paste0('\\b', currently_solved[i], '\\b'),
        paste0(' (', replacement[i], ') ')
      )

  }
  
  #update the is solvable field (SOLVED IF NO DIGITS ARE LEFT)
  rules$is_solved <- str_detect(rules$solution, '^\\D+$')
  #Collapse Out the Spaces if solved
  rules$solution <- if_else(rules$is_solved, 
                            str_remove_all(rules$solution, ' '),
                            rules$solution)
}

##Part 1
rule0 <- rules %>% filter(rule_number == '0') %>% pull(solution)
sum(map_lgl(msgs, ~str_detect(.x, paste0('^', rule0, '$'))))

###Part 2
#Original Rules
# 8: 42
# 11: 42 31 

#New Rules
# 8: 42 | 42 8 (at least 1 42 but can be infinite 42s)
# 11: 42 31 | 42 11 31 same number of 42s and 31s in a row 

# Rule 0 is 8 11

##Grabbing Rules That Need Changing
rule8 <-  rules %>% filter(rule_number == '8') %>% pull(solution)
rule42 <- rules %>% filter(rule_number == '42') %>% pull(solution)
rule31 <- rules %>% filter(rule_number == '31') %>% pull(solution)

# Updating New rules for the infinite loops
new_rule_8 <- paste0(rule8, '+')

###Just Keep Trying Things since I'm not sure how to enforce the same 
###number for rule 42s, rule 31s
solutions <- c()
for(i in 1:100){
  #Construct Rule 11 Variant
  new_rule_11 <- paste0('((', rule42, '){',i,'}(', rule31,'){',i,'})')
  #Construct Rule 0 Variance
  new_rule0 <- paste0('(', new_rule_8, ')(', new_rule_11, ')')
  #Get Solutions
  new_solutions <- append(solutions, 
                          keep(msgs, ~str_detect(.x, paste0('^', new_rule0, '$')))
  )
  
  if(length(new_solutions) == length(solutions)) break
  else solutions = new_solutions
}

n_distinct(solutions)
