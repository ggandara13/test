


linear.congruence <- function(a=22695477, 
                              b=1, 
                              m=2**32, 
                              seed=1234){
  random.number <- (a * seed + b) %% m
  if (random.number <= 2**31){
    return(c(0,random.number))
  } else{
    return(c(1,random.number))
  }
}

save(list = ls(all = TRUE), file= "all.rda")

#rm(list=ls()) 
# restore the saved values to the user's workspace
#load("all.rda")


