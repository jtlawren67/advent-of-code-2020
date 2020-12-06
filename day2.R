library(tidyverse)

###PART 1

d2 <- read_csv('data/day2.txt', col_names = F)

d2_a <- d2 %>%
  separate(X1, c("mn", "mx", "char", "pattern"), sep = '-|:| ', extra = 'merge', remove = F) %>% 
  mutate(
    cln_string = str_remove_all(pattern, paste0('[^',char,']')),
    cnt = nchar(cln_string),
    valid = cnt >= as.integer(mn) & cnt <= as.integer(mx)
  ) 

d2_a %>% 
count(valid)

####PART 2

d2_b <- d2 %>%
  separate(X1, c("mn", "mx", "char", "pattern"), sep = '-|:| ', extra = 'merge', 
           remove = F) %>% 
  mutate(
    itm1 = str_sub(str_remove(pattern, " "), as.integer(mn), as.integer(mn)),
    itm2 = str_sub(str_remove(pattern, " "), as.integer(mx), as.integer(mx)),
    valid = xor(itm1 == char,  itm2 == char)
  ) 

count(d2_b, valid)
