library(tidyverse)

d5 <- read_csv('data/day5.txt', col_names = 'orig')

find_seat <- function(mn, mx, input){
  
  #If Only 1 Option Then Return It
  if(input=="") return(mn)
  
  #Check if we're doing lower half
  if(str_sub(input, 1, 1) %in% c('F', 'L')){
    return(find_seat(mn, mx - ceiling((mx-mn)/2), str_sub(input, 2)))
  } else{
    return(find_seat(mn + floor((mx-mn)/2 + 1), mx, str_sub(input, 2)))
  }
}

##Part 1
d5_a <- d5 %>%
  mutate(seat_id = map_dbl(orig,
                           ~(find_seat(0, 127, str_sub(.x, 1, 7))*8 + find_seat(0, 7, str_sub(.x, 8)))
                           )
  )

max(d5_a$seat_id)

##Part 2 
setdiff(0:1023, d5_a$seat_id) %>% keep(~!.x %in% c(setdiff(0:1023, d5_a$seat_id)+1,
                                                  setdiff(0:1023, d5_a$seat_id)-1))
