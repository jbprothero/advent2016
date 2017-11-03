

d = readLines('day6.txt')

################ Part 1

counts = matrix(0, nrow=26, ncol=8)

for(i in d){
  for(j in 1:8){
    letter = substr(i, j, j)
    number = utf8ToInt(letter) - 96
    counts[number, j] = counts[number, j] + 1
  }
}

maxes = apply(counts, 2, max)

for(i in 1:8){
  for(j in 1:26){
    if(counts[j,i] == maxes[i]){print(intToUtf8(j+96))}
  }
}

################ Part 2

counts = matrix(0, nrow=26, ncol=8)

for(i in d){
  for(j in 1:8){
    letter = substr(i, j, j)
    number = utf8ToInt(letter) - 96
    counts[number, j] = counts[number, j] + 1
  }
}

mins = apply(counts, 2, min)

for(i in 1:8){
  for(j in 1:26){
    if(counts[j,i] == mins[i]){print(intToUtf8(j+96))}
  }
}