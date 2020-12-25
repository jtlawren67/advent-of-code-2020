library(tidyverse)
options(scipen = 999)

input <- readLines('data/day25.txt')

card_public_key = as.numeric(input[1])
door_public_key = as.numeric(input[2])

###########
####Part 1
###########

lp <- function(val, iteration, subject_no=7){
  return((val*subject_no)%%20201227)
}

ans = 1
it = 0
while(!ans %in% c(card_public_key, door_public_key)){
  it = it + 1
  ans <- lp(ans, 1)
}

if(ans == card_public_key){
  ans <- reduce2(1:it, rep(door_public_key, it), lp, .init = 1)
}else{
  ans <- reduce2(1:it, rep(card_public_key, it), lp, .init = 1)
}

print(ans)

