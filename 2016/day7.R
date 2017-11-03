library(stringr)

d = readLines('day7.txt')

################ Part 1

# Takes in a word and determines whether that word contains an ABBA pattern 
findabba <- function(word){
  
  n = nchar(word)
  
  candidates = c(substr(word,1,2), substr(word,2,3))
  
  for(k in 3:(n-1)){
    
    teststr = substr(word,k,k+1)
    revteststr = paste(substr(teststr,2,2), substr(teststr,1,1), sep = "", collapse = "")
    
    samechartest = substr(candidates[1],1,1)==substr(candidates[1],2,2)
    
    if(revteststr == candidates[1] & !samechartest){
      return(T)
    } else {
      candidates[1] = candidates[2]
      candidates[2] = teststr
    }
    
  }
  
  return(F)
}

tlsips = 0

for(i in d){
  
  words = str_extract_all(i, '[a-z]+')[[1]]
  lwords = length(words)
  
  goodabba = F
  badabba = F
  
  for(j in 1:lwords){
    word = words[j]
    
    if(j%%2==1 & !goodabba & !badabba){goodabba = findabba(word)}
    if(j%%2==0 & !badabba){badabba = findabba(word)}
  }
  
  if(goodabba & !badabba){tlsips = tlsips + 1}
}

################ Part 2

# Takes in a word and returns all ABA patterns within that word
findaba <- function(word){
  
  n = nchar(word)
  returnlist = character(0)
  
  candidate = substr(word,1,2)
  for(k in 2:(n-1)){
    teststr = substr(word,k+1,k+1)
    
    samechartest = substr(candidate,1,1)==substr(candidate,2,2)
    
    if(teststr == substr(candidate,1,1) & !samechartest){
      returnlist = append(returnlist, paste(candidate, teststr, sep=""))
    }
    
    candidate = substr(word,k,k+1)
  }
  
  return(returnlist)
}

# Takes in a word & a list of BAB patterns and determines whether the word contains any of those patterns
checkbab <- function(word, babs){
  
  for(bab in babs){
    if(grepl(bab, word)){return(T)}
  }
  
  return(F)
}

# Takes in a list of ABA patterns and converts them into their corresponding BAB patterns
abatobab <- function(abas){
  
  char1 = substr(abas,1,1)
  char2 = substr(abas,2,2)
  return(paste(char2, char1, char2, sep=""))
}

sslips = 0

for(i in d){
  words = str_extract_all(i, '[a-z]+')[[1]]
  lwords = length(words)
  
  ababab = F
  oddbabs = character(0)
  evenbabs = character(0)
  for(j in 1:lwords){
    word = words[j]
    
    if(j%%2==1 & ababab==F){
      ababab = checkbab(word, oddbabs)
      evenbabs = append(evenbabs, abatobab(findaba(word)))
    }
    if(j%%2==0 & ababab==F){
      ababab = checkbab(word, evenbabs)
      oddbabs = append(oddbabs, abatobab(findaba(word)))
    }
    
  }
  
  if(ababab){sslips = sslips + 1}
}
