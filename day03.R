library(tidyverse)


# Read in File and Break Apart String into Vector of Characters
d3 <- read_csv('data/day3', col_names = 'orig') %>%
  mutate(broken = strsplit(orig, ""))

# Turn Vectors into a single matrix
m <- matrix(unlist(d3$broken), nrow = nrow(d3), byrow = T)

#The toboggan can only follow a few specific slopes (you opted for a cheaper model that 
# prefers rational numbers); start by counting all the trees you would encounter for the 
# slope right 3, down 1:

run_slope <- function(right, down) {
  
  #Initialize to zero to make MOD'ing easier, will adjust for the 1-index laster
  current_row = 0
  current_col = 0
  num_trees = 0

  #Run Slope (adjust for 1-indexing)
  while(current_row+1 < nrow(m)){
    
    #Take Next Step
    current_row = current_row + down
    current_col = (current_col + right) %% ncol(m)
      
    ##Check Current Location For Trees (marked by "X")
    if(m[current_row+1, current_col+1] == '#'){
      num_trees = num_trees + 1
    }
  }
  
  return(num_trees)
}

## Part 1  
run_slope(3, 1)

## Part 2
run_slope(1, 1) * run_slope(3, 1) * run_slope(5, 1) * run_slope(7, 1) * run_slope(1, 2)
