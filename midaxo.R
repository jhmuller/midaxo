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



desc_df <-rep(0.0, 300*nrow(cbase))
dim(desc_df) <- c(300, nrow(cbase))
colnames(desc_df) <- cbase$Company

cat_df <-rep(0.0, 300*nrow(cbase))
dim(cat_df) <- c(300, nrow(cbase))
colnames(cat_df) <- cbase$Company

for (i in 1:nrow(cbase)){
  #i <- 1
  company <- cbase$Company[i]  
  print(paste(i,company))

  vec <- rep(0, 300)
  dim(vec) <- c(300,1)
  sumwts <- 0.0
  for (j in seq(1:length(cbase$Dwords[i][[1]]))) {
    word <- cbase$Dwords[i][[1]][j]
    if (word %in% colnames(gmod)){
      thisword <- word
      thiswt = cbase$Dword_wts[i][[1]][j]
      sumwts <- sumwts + thiswt
      thisvec <- gmod[,word] * thiswt
      dim(thisvec) <- c(300,1)
      vec <- vec + thisvec
    }
  }
  desc_vec <- vec / sumwts
  desc_df[,i] <- desc_vec  
  
  vec <- rep(0, 300)
  dim(vec) <- c(300,1)  
  sumwts = 0
  for (j in seq(1:length(cbase$Catwords[i][[1]]))) {
    word <- cbase$Catwords[i][[1]][j]
    if (word %in% colnames(gmod)){
      thisword <- word
      sumwts <- sumwts + 1
      thisvec <- gmod[,word] * thiswt
      dim(thisvec) <- c(300,1)
      vec <- vec + thisvec
    }
  }  
  cat_vec <- vec / sumwts  
  cat_df[,i] <- cat_vec  
}


vdf <- cat_df*.01 + desc_df*.99

rm(list=c("msg"))
msg1 <- ""
for (i in 2:2){#:nrow(cbase)){
  #i <- 1
  msg1 <- paste("\n")
  msg1 <- paste(msg1, "#",  i, "Company: ", cbase$Company[i], "\n")
  msg1 <- paste(msg1, "Dwords: ",cbase$Dwords[i], "\n")
  msg1 <- paste(msg1, "Dword_wts: ",cbase$Dword_wts[i], "\n")  
  msg1 <- paste(msg1, "Catwords: ",cbase$Catwords[i], "\n")  
  x <- find_sim_wvs(this_wv=vdf[,i], all_wvs=vdf, top_n_res=5)
  
  if (length(x) == 0 || is.na(x)){
    print(paste("ERROR!"))
    next
  }
  msg2 <- " "
  for (j in 2:6){
    msg2 <- paste(msg2, "\nmatch #\n", j-1 )
    other <- names(x)[j]    
    msg2 <- paste(msg2, "Company: ", other, "Score= ", x[j], "\n")
    msg2 <- paste(msg2, "\tClosestDwords: ",cbase$Dwords[cbase$Company == other], "\n")
    msg2 <- paste(msg2, "\tClosestDwords: ",cbase$Dword_wts[cbase$Company == other], "\n")
    msg2 <- paste(msg2, "Catwords: ",cbase$Catwords[cbase$Company == other], "\n")     
  }
  print(i)
  msg <- paste(msg1, msg2)
  cat(msg)
}

library(MASS)
odir <- "data_derived"
dir.create(odir, showWarnings = TRUE)
write.matrix(cat_df, "data_derived/catmat.csv", sep="|")
write.matrix(desc_df, "data_derived/descmat.csv", sep="|")

typeof(cat_df)
cat(msg)
write(msg, 'crunchbase_matches.txt')



names(x)[2]
cbase[cbase$Company =="Netflix", "Desc2"]
cbase[cbase$Company =="Daraz.pk", "Desc2"]
cbase[cbase$Company =="Myntra", "Desc2"]
cbase[cbase$Company =="Weebly", "Desc2"]

