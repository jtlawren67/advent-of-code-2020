input <- c(0,8,15,2,12,1,4)


###Part 1 

while(length(input) < 2020){
  last_spoken = input[length(input)]
  num_spoken = sum(input == last_spoken)
  if(num_spoken == 1){input <- append(input, 0)}
  else if(num_spoken > 1){
    last2 <- rev(which(input == last_spoken, arr.ind = T))[1:2]
    input <- append(input, last2[1]-last2[2])
    
  }
}


## Part 2
input <- c(0,8,15,2,12,1,4)

ht <- new.env(hash=TRUE)

i = 1

#Initialize Existing Input
for(i in seq_len(length(input))){
  ht[[as.character(input[i])]] <- c(i)
}

i = i + 1
last_number = input[length(input)]

while(i <= 30000000){
  if(i %% 5e5 == 0){ print(paste(Sys.time(), ":", i))}
  
  if(length(ht[[as.character(last_number)]]) == 1){
    last_number = 0
  }
  else{
    last_number = ht[[as.character(last_number)]][1] - ht[[as.character(last_number)]][2]
  }
  
  if(length(ht[[as.character(last_number)]]) == 0){ht[[as.character(last_number)]] <- c(i)}
  else if (length(ht[[as.character(last_number)]]) == 1){
    ht[[as.character(last_number)]] <- append(ht[[as.character(last_number)]], i, 0)
  }
  else {
    ht[[as.character(last_number)]][2] <- ht[[as.character(last_number)]][1]
    ht[[as.character(last_number)]][1] <- i
  }
  
  i = i + 1
}

print(last_number)
