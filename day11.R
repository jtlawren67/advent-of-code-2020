library(tidyverse)
library(tictoc)

#Create Grid of Seats
d11 <- readLines('data/day11.txt')
grid <- d11 %>% strsplit('') %>% unlist %>% matrix(nrow = length(d11), byrow = T)

#Get Number of Occupied Neighbor Seats
count_seats <- function(grd, row_id, col_id){
  #Subset Matrix
  sub_mtx = grd[row_id:(row_id+2), col_id:(col_id+2)]
  sub_mtx[2, 2] = ''
  return(sum(sub_mtx == '#'))
}

seat_step <- function(mat){
  #Pad Zeroes on Border
  new_grid <- rbind(
    rep(0, ncol(mat)+2),
    cbind(0, mat, 0),
    rep(0, ncol(mat)+2)
  )
  
  #Build Output Grid
  end_grid <- matrix(rep('na', nrow(mat)*ncol(mat)), nrow = nrow(mat), ncol = ncol(mat))
  
  #Run Rules for Seats
  for(i in 1:nrow(mat)){
    for(j in 1:ncol(mat)){
      neighbors = count_seats(new_grid, i, j)
      
      if(mat[i, j] == 'L' & neighbors == 0){end_grid[i, j] = '#'}
      else if(mat[i, j] == '#' & neighbors >=4){end_grid[i, j] = 'L'}
      else {end_grid[i, j] = mat[i, j]}

    }
  }
  
return(end_grid)
}

##Run until things stabilize
isStable = F
start_grid = grid
tic()
while(!isStable){
  end_grid <- seat_step(start_grid)
  if(all(start_grid == end_grid)){
    isStable = T
  }
  else{
    start_grid = end_grid
  }
}
toc()
sum(end_grid=="#")


## Part 2
first_seats <- function(mat, rid, cid){
  #print(paste("NEW IDs", rid, cid))
  
  n<-s<-e<-w<-nw<-ne<-sw<-se<-'.'
  
  #North 
  nid = rid-1
  while(n=='.' & nid > 0){
    n <- mat[nid, cid]
    nid <- nid -1
  }
  
  #South
  sid = rid + 1
  while(s=='.' & sid <= nrow(mat)){
    s <- mat[sid, cid]
    sid <- sid + 1
  }
  
  #East
  eid = cid + 1
  while(e=='.' & eid <= ncol(mat)){
    e <- mat[rid, eid]
    eid <- eid + 1
  }
  
  #West
  wid = cid - 1
  while(w=='.' & wid > 0){
    w <- mat[rid, wid]
    wid <- wid - 1
  }
  
  #NE
  ne_id_n = rid - 1
  ne_id_e = cid + 1
  while(ne == '.' & ne_id_n > 0 & ne_id_e <= ncol(mat)){
    ne <- mat[ne_id_n, ne_id_e]
    ne_id_n = ne_id_n - 1
    ne_id_e = ne_id_e + 1
  }
  
  #NW
  nw_id_n = rid - 1
  nw_id_w = cid - 1
  while(nw == '.' & nw_id_n > 0 & nw_id_w > 0){
    nw <- mat[nw_id_n, nw_id_w]
    nw_id_n = nw_id_n - 1
    nw_id_w = nw_id_w - 1
  }
  
  #SE
  se_id_s = rid + 1
  se_id_e = cid + 1
  while(se == '.' & se_id_s <= nrow(mat) & se_id_e <= ncol(mat)){
    se <- mat[se_id_s, se_id_e]
    se_id_s = se_id_s + 1
    se_id_e = se_id_e + 1
  }
  
  
  #SW
  sw_id_s = rid + 1
  sw_id_w = cid - 1
  while(sw == '.' & sw_id_s <= nrow(mat) & sw_id_w > 0){
    #print(paste(sw_id_s, sw_id_w))
    sw <- mat[sw_id_s, sw_id_w]
    sw_id_s = sw_id_s + 1
    sw_id_w = sw_id_w - 1
  }
  
  return(sum(c(n,e,s,w,se,sw,ne,nw)=='#'))
  
}

seat_step2 <- function(mat){
  #Build Output Grid
  end_grid <- matrix(rep('na', nrow(mat)*ncol(mat)), nrow = nrow(mat), ncol = ncol(mat))
  
  #Run Rules for Seats
  for(i in 1:nrow(mat)){
    for(j in 1:ncol(mat)){
      
      neighbors = first_seats(mat, i, j)
      
      if(mat[i, j] == 'L' & neighbors == 0){end_grid[i, j] = '#'}
      else if(mat[i, j] == '#' & neighbors >=5){end_grid[i, j] = 'L'}
      else {end_grid[i, j] = mat[i, j]}
    }
  }
  return(end_grid)
}

##Run until things stabilize
isStable = F
start_grid = grid

tic()
while(!isStable){
  end_grid <- seat_step2(start_grid)
  if(all(start_grid == end_grid)){
    isStable = T
  }
  else{
    start_grid = end_grid
  }
}
toc()

sum(end_grid=="#")