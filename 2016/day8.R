library(stringr)

d = readLines('day8.txt')

################ Part 1

# Takes in a vector and shifts its entries forward by 'by'.
# Entries that fall off the edge reappear at the beginning
rotate <- function(v, by){
  n = length(v)
  out = numeric(n)
  out[(by+1):n] = v[1:(n-by)]
  out[1:by] = v[(n+1-by):n]
  return(out)
}

screen = matrix(0, nrow=6, ncol=50)

for(i in d){
  words = str_extract_all(i, '[a-z]+')[[1]]
  nums = as.numeric(str_extract_all(i, '[0-9]+')[[1]])
  
  if(words[1] == 'rect'){
    A = nums[1]
    B = nums[2]
    screen[1:B, 1:A] = matrix(1, nrow=B, ncol=A)
  }
  
  if(words[1] == 'rotate'){
    if(words[2] == 'row'){
      screen[nums[1]+1,] = rotate(screen[nums[1]+1,], nums[2])
    }
    
    if(words[2] == 'column'){
      screen[,nums[1]+1] = rotate(screen[,nums[1]+1], nums[2])
    }
  }
}

sum(screen)

################ Part 2

display = ifelse(screen==1, NaN, screen)

for(j in 1:10){
  beg = (j-1)*5+1
  end = j*5
  print(display[,beg:end])
}

