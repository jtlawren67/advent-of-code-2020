library(tidyverse)

d4 <- read_csv('data/day4.txt', col_names = 'orig', skip_empty_rows = F) %>% 
  mutate(tag = if_else(is.na(orig), 1, 0),
         id = cumsum(tag)) %>% 
  filter(!is.na(orig)) %>% 
  group_by(id) %>%
  summarize(newstring = paste0(orig, collapse = ' '))


# Count the number of valid passports - those that have all required fields. 
# Treat cid as optional. In your batch file, how many passports are valid?
d4 %>% 
  mutate(
    valid = str_count(newstring, ':') == 8 | 
      (str_count(newstring, ":") == 7 & !str_detect(newstring, 'cid:'))
  ) %>% 
  count(valid)

# You can continue to ignore the cid field, but each other field has strict rules about what values 
# are valid for automatic validation:
#   
# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
# If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.
# 
# Your job is to count the passports where all required fields are both present 
# and valid according to the above rules. Here are some example values:

d4b <- d4 %>% 
  separate_rows(newstring, sep = " ") %>% 
  separate(newstring, c('key', 'value'), sep = ":", convert = T) %>% 
  pivot_wider(names_from = 'key', values_from = 'value') %>%
  
  ### Run Validation Rules
  mutate(
    valid = 
      (between(as.numeric(byr), 1920, 2002)) & 
      (between(as.numeric(iyr), 2010, 2020)) &
      (between(as.numeric(eyr), 2020, 2030)) &
      case_when(
        !str_detect(hgt, '\\d+[cm|in]') ~ FALSE,
        str_detect(hgt, 'in') & between(parse_number(hgt), 59, 76) ~ TRUE,
        str_detect(hgt, 'cm') & between(parse_number(hgt), 150, 193) ~ TRUE,
        TRUE ~ FALSE
      ) & 
      str_detect(hcl, '#[0-9a-f]{6}') & 
      ecl %in% c('amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth') & 
      str_detect(pid, '^[\\d]{9}$') 
  )

count(d4b, valid)