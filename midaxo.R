# clear memory for this session
rm(list=ls())

source("glove.R")
gmod <- glove$model
class(gmod)
colnames(gmod)[100:110]

# run the crunchbase script, extract the cbase data frame
source('crunchbase.R')
cbase <- crunchbase$cbase

glimpse(cbase)



gdf <-rep(0.0, 300*nrow(cbase))
dim(gdf) <- c(300, nrow(cbase))
colnames(gdf) <- cbase$Company
for (i in 1:nrow(cbase)){
  #i <- 1
  company <- cbase$Company[i]  
  print(paste(i,company))
  vec <- rep(0L, 300)
  dim(vec) <- c(300,1)
  k <- 0.0
  for (j in seq(1:length(cbase$DWords[i][[1]]))) {
   # j <- 2
    word <- cbase$DWords[i][[1]][j]
    #print (word)
    if (word %in% colnames(gmod)){
      thisword <- word
      thisvec <- gmod[,word]
      dim(thisvec) <- c(300,1)
      #print(paste(k, word))
      vec <- vec + thisvec
      k <- k + 1
    }
    #print(vec)
  }
  vec <- vec / k
  gdf[,i] <- vec   
}

rm(list=c("msg"))
msg <- ""
for (i in 5:6){#:nrow(cbase)){
  #i <- 1
  cmsg <- paste("\n")
  cmsg <- paste(cmsg, "#",  i, "Company: ", cbase$Company[i], "\n")
  cmsg <- paste(cmsg, "Descr: ",cbase$Desc2[i], "\n")
  
  x <- find_sim_wvs(this_wv=gdf[,i], all_wvs=gdf, top_n_res=5)
  
  if (length(x) == 0 || is.na(x)){
    print(paste("ERROR!"))
    next
  }
  for (j in 2:4){
    cmsg <- paste(cmsg, "match #", j-1 )
    other <- names(x)[j]    
    cmsg <- paste(cmsg, "Company: ", other, "Score= ", x[j], "\n")
    cmsg <- paste(cmsg, "\tClosestDescr: ",cbase$Desc2[cbase$Company == other], "\n")
  }
    print(i)
  msg <- paste(msg, cmsg)
}

cat(msg)
write(msg, 'crunchbase_matches.txt')



names(x)[2]
cbase[cbase$Company =="Netflix", "Desc2"]
cbase[cbase$Company =="Daraz.pk", "Desc2"]
cbase[cbase$Company =="Myntra", "Desc2"]
cbase[cbase$Company =="Weebly", "Desc2"]

