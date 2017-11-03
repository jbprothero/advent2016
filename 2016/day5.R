library(digest)

d = "ffykfhsq"

################ Part 1

index = 0
passlength = 8
password = character(passlength)

slot = 1
while(slot <= passlength){
  
  temp = paste(d, as.character(index), sep="")
  hash = digest(temp, serialize=F)
  
  if(substr(hash, 1, 5) == '00000'){
    password[slot] = substr(hash, 6, 6)
    slot = slot + 1
  }
  
  index = index + 1
}

################ Part 2

index = 0
passlength = 8
password = character(passlength)

positions = c('0', '1', '2', '3', '4', '5', '6', '7')
while(length(positions) > 0){
  temp = paste(d, as.character(index), sep="")
  hash = digest(temp, serialize=F)
  
  if(substr(hash, 1, 5) == '00000'){
    print(hash)
    avail = paste(positions, sep="", collapse="")
    pos = grep(paste('[', avail, ']', sep="", collapse=""), substr(hash, 6, 6), value=T)
    if(length(pos)>0){
      password[as.numeric(pos)+1] = substr(hash, 7, 7)
      positions = setdiff(positions, pos)
    }
  }
  
  index = index + 1
}

