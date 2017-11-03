library(stringr)

d = readLines('day4.txt')

################ Part 1

idsum = 0

for(i in d){
  id = str_extract(i, '[0-9]+')
  lets = str_extract_all(i, '[a-z]+')[[1]]
  numwords = length(lets)
  checksum = lets[numwords]
  f = paste(lets[1:(numwords-1)], sep="", collapse="")
  n = as.numeric(charToRaw(f))
  
  counts = numeric(26)
  for(j in n){
    counts[j-96] = counts[j-96]+1
  }
  
  topfive = letters[order(-counts, letters)][1:5]
  topfive = paste(topfive, sep="", collapse="")
  if(topfive == checksum){idsum = idsum+as.numeric(id)}
}

################ Part 2

rooms = data.frame(id=character(1), name=character(1), stringsAsFactors=F)

for(i in d){
  id = str_extract(i, '[0-9]+')
  lets = str_extract_all(i, '[a-z]+')[[1]]
  numwords = length(lets)
  checksum = lets[numwords]
  f = paste(lets[1:(numwords-1)], sep="", collapse="")
  n = as.numeric(charToRaw(f))
  
  counts = numeric(26)
  for(j in n){
    counts[j-96] = counts[j-96]+1
  }
  
  topfive = letters[order(-counts, letters)][1:5]
  topfive = paste(topfive, sep="", collapse="")
  if(topfive == checksum){
    rooms = rbind( rooms, c( id, intToUtf8( (((n-97) + as.numeric(id)) %% 26) + 97 ) ) )
  }
}

rooms[grep('north', rooms$name),]
