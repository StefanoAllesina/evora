nn <- read_csv("nobel.csv")
for (i in 1:nrow(nn)){
  if(is.na(nn$year[i])) nn$year[i] <- nn$year[i-1]
  if(is.na(nn$category[i])) nn$category[i] <- nn$category[i-1]
}

write.csv(nn, "nobel.csv")
