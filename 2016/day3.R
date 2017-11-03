

d = read.table('day3.txt', header=F)

################ Part 1

n = length(d[,1])
triangles = numeric(n)

for(i in 1:n){
  set = d[i,]
  bigside = max(set)
  others = sum(set) - bigside
  if(others > bigside){triangles[i] = 1}
}

ans = sum(triangles)

################ Part 2

triangles2 = numeric(n)

for(i in 1:3){
  for(j in 1:(n/3)){
    start = (j-1)*3 + 1
    set = d[start:(start+2),i]
    bigside = max(set)
    others = sum(set) - bigside
    if(others > bigside){triangles2[n*(i-1)/3 + j] = 1}
  }
}

ans2 = sum(triangles2)