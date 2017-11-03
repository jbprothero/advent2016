library(stringr)

d = read.csv('day1.txt', header=F, stringsAsFactors=F)
d = str_trim(d)

################ Part 1

nstotal = 0 # N is positive, S is negative
ewtotal = 0 # E is positive, W is negative
direc = 0 # N=0, E=1, S=2, W=3

for(s in d){
  if(substr(s, 1, 1) == 'R'){direc=direc+1}
  if(substr(s, 1, 1) == 'L'){direc=direc-1}
  direc = direc %% 4
  amt = as.numeric(substring(s, 2))
  if(direc==0){nstotal = nstotal + amt}
  if(direc==1){ewtotal = ewtotal + amt}
  if(direc==2){nstotal = nstotal - amt}
  if(direc==3){ewtotal = ewtotal - amt}
}

ans = abs(nstotal) + abs(ewtotal)

################ Part 2

hpos = 0
vpos = 0
hseg = list() # list of traversed horizontal segments c(leftendpt, rightendpt, vertpos)
vseg = list() # list of traversed vertical segments c(bottomendpt, topendpt, horizpos)
crossings = numeric(0)
direc = 0 # N=0, E=1, S=2, W=3

for(s in d){
  if(substr(s, 1, 1) == 'R'){direc=direc+1}
  if(substr(s, 1, 1) == 'L'){direc=direc-1}
  direc = direc %% 4
  amt = as.numeric(substring(s, 2))
  hsegnum = length(hseg)
  vsegnum = length(vseg)
  
  if(direc == 0){
    for(seg in hseg){
      if(hpos >= seg[1] & hpos <= seg[2] & vpos < seg[3] & vpos + amt >= seg[3]){
        crossings = c(crossings, seg[3])
      }
    }
    
    for(seg in vseg){
      if(hpos == seg[3] & vpos < seg[1] & vpos + amt >= seg[1]){
        crossings = c(crossings, seg[1])
      }
    }
    
    if(length(crossings)>0){
      goal = c(hpos, min(crossings))
      break
    }else{
      vseg[[vsegnum+1]] = c(vpos, vpos+amt, hpos)
      vpos = vpos+amt
    }
  }
  
  if(direc == 1){
    for(seg in vseg){
      if(vpos >= seg[1] & vpos <= seg[2] & hpos < seg[3] & hpos + amt >= seg[3]){
        crossings = c(crossings, seg[3])
      }
    }
    
    for(seg in hseg){
      if(vpos == seg[3] & hpos < seg[1] & hpos + amt >= seg[1]){
        crossings = c(crossings, seg[1])
      }
    }
    
    if(length(crossings)>0){
      goal = c(min(crossings), vpos)
      break
    }else{
      hseg[[hsegnum+1]] = c(hpos, hpos+amt, vpos)
      hpos = hpos+amt
    }
  }
  
  
  
  
  if(direc == 2){
    for(seg in hseg){
      if(hpos >= seg[1] & hpos <= seg[2] & vpos > seg[3] & vpos - amt <= seg[3]){
        crossings = c(crossings, seg[3])
      }
    }
    
    for(seg in vseg){
      if(hpos == seg[3] & vpos > seg[2] & vpos - amt <= seg[2]){
        crossings = c(crossings, seg[2])
      }
    }
    
    if(length(crossings)>0){
      goal = c(hpos, max(crossings))
      break
    }else{
      vseg[[vsegnum+1]] = c(vpos-amt, vpos, hpos)
      vpos = vpos-amt
    }
  }
  
  if(direc == 3){
    for(seg in vseg){
      if(vpos >= seg[1] & vpos <= seg[2] & hpos > seg[3] & hpos - amt <= seg[3]){
        crossings = c(crossings, seg[3])
      }
    }
    
    for(seg in hseg){
      if(vpos == seg[3] & hpos > seg[2] & hpos - amt <= seg[2]){
        crossings = c(crossings, seg[2])
      }
    }
    
    if(length(crossings)>0){
      goal = c(max(crossings), vpos)
      break
    }else{
      hseg[[hsegnum+1]] = c(hpos-amt, hpos, vpos)
      hpos = hpos-amt
    }
  }
  
  goal = c(0,0)
}

ans2 = sum(abs(goal))
