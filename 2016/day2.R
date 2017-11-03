library(stringr)

d = readLines('day2.txt')

################ Part 1

pos = c(1,1)
code = numeric(0)
for(i in d){
  
  l = nchar(i)
  for(j in 1:l){
    dir = substr(i,j,j)
    if(dir=='R'){pos[2] = pos[2]+1}
    if(dir=='U'){pos[1] = pos[1]-1}
    if(dir=='L'){pos[2] = pos[2]-1}
    if(dir=='D'){pos[1] = pos[1]+1}
    
    pos = ifelse(pos>2, 2, pos)
    pos = ifelse(pos<0, 0, pos)
  }
  
  code = c(code, pos[1]*3 + pos[2] + 1)  
}

# 1 2 3
# 4 5 6
# 7 8 9

################ Part 2

pos = c(2,0)
code = numeric(0)
for(i in d){
  
  l = nchar(i)
  for(j in 1:l){
    dir = substr(i,j,j)
    if(dir=='R' & pos[1]+pos[2]<6 & pos[2]-pos[1]<2){pos[2] = pos[2]+1}
    if(dir=='U' & pos[1]+pos[2]>2 & pos[2]-pos[1]<2){pos[1] = pos[1]-1}
    if(dir=='L' & pos[1]+pos[2]>2 & pos[2]-pos[1]>-2){pos[2] = pos[2]-1}
    if(dir=='D' & pos[1]+pos[2]<6 & pos[2]-pos[1]>-2){pos[1] = pos[1]+1}
  }
  
  code = c(code, pos[1]*5 + pos[2] + 1)
}

#       3    
#    7  8  9
# 11 12 13 14 15
#    17 18 19 
#       23
