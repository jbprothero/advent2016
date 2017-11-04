library(stringr)

d = readLines('day9.txt')

################ Part 1

n = nchar(d)

outstr = character(1)

i = 1
while(i <= n){
  if(substr(d,i,i)=='('){
    j = regexpr( '\\)', str_sub(d, i)) + i - 1
    nums = as.numeric(str_extract_all(substr(d,i,j), '[0-9]+')[[1]])
    
    chain = substr(d, j+1, j+nums[1])
    dchain = str_dup(chain, nums[2])
    
    outstr = paste(outstr, dchain, sep="")
    i = j+nums[1]+1
  } else {
    outstr = paste(outstr, substr(d,i,i), sep="")
    i = i+1
  }
}

nchar(outstr)

################ Part 2

decompress <- function(chain){
  
  subn = nchar(chain)
  
  out = 0
  
  i = 1
  while(i <= subn){
    if(substr(chain,i,i)=='('){
    
      j = regexpr( '\\)', str_sub(chain, i)) + i - 1
      nums = as.numeric(str_extract_all(substr(chain,i,j), '[0-9]+')[[1]])
    
      subchain = substr(chain, j+1, j+nums[1])
      out = out + decompress(subchain)*nums[2]
      i = j+nums[1]+1
    
    } else {
      out = out+1
      i = i+1
    }
  }
  
  return(out)
}

decompress(d)
