library(tidyverse)
library(rvest)


expenses <-  read_lines('data/day1.txt') %>% as.numeric

####PART 1:
# For example, suppose your expense report contained the following:
#   
#   1721
# 979
# 366
# 299
# 675
# 1456
# In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.

crossing(
  item1 = expenses,
  item2 = expenses
) %>% 
  filter(item1 + item2 == 2020) %>%
  mutate(answer = item1 * item2)




#### PART 2 #$##
#In your expense report, what is the product of the three entries that sum to 2020?

crossing(
  item1 = expenses,
  item2 = expenses,
  item3 = expenses
) %>% 
  filter(item1 + item2 + item3 == 2020) %>%
  mutate(answer = item1 * item2 * item3)
