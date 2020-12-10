library(magrittr)
library(memoise)
options(scipen = 999)

d10 <- readLines('data/day10.txt') %>% as.numeric() 

#Part 1 - What are all the 3 jolt differences times the 1 jolt differences
c(0, d10) %>% sort() %>% diff() %>% c(., 3) %>% table()

#Part 2 - How Many Distinct Ways Are There To Order the Adapters
part2 <- function(value){
  if(value == max(d10)) return(1)
  if(!value %in% d10 & value != 0) return(0)
  
  return(m_part2(value+1) + m_part2(value+2) + m_part2(value+3))
}

m_part2 <- memoise(part2)

m_part2(0) %>% as.numeric()
