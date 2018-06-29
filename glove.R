
# input .txt file, exports list of list of values and character vector of names (words)
proc_pretrained_vec <- function(p_vec) {
  
  
  # initialize space for values and the names of each word in vocab
  vals <- vector(mode = "list", length(p_vec))
  names <- character(length(p_vec))
  
  # loop through to gather values and names of each word
  for(i in 1:length(p_vec)) {
    if(i %% 1000 == 0) {print(i)}
    this_vec <- p_vec[i]
    this_vec_unlisted <- unlist(strsplit(this_vec, " "))
    this_vec_values <- as.numeric(this_vec_unlisted[-1])  # this needs testing, does it become numeric?
    this_vec_name <- this_vec_unlisted[1]
    
    vals[[i]] <- this_vec_values
    names[[i]] <- this_vec_name
  }
  
  # convert lists to data.frame and attach the names
  glove <- data.frame(vals)
  names(glove) <- names
  
  return(glove)
}



find_sim_wvs <- function(this_wv, all_wvs, top_n_res=40, verbose=FALSE) {
  # this_wv will be a numeric vector; all_wvs will be a data.frame with words as columns and dimesions as rows
  require(text2vec)
  this_wv_mat <- matrix(this_wv, ncol=length(this_wv), nrow=1)
  all_wvs_mat <- as.matrix(all_wvs)
  
  if(dim(this_wv_mat)[[2]] != dim(all_wvs_mat)[[2]]) {
    if (verbose) {
      print("switching dimensions on the all_wvs_matrix")
    }
    all_wvs_mat <- t(all_wvs_mat)
  }
  
  cos_sim = sim2(x=all_wvs_mat, y=this_wv_mat, method="cosine", norm="l2")
  sorted_cos_sim <- sort(cos_sim[,1], decreasing = T) 
  return(head(sorted_cos_sim, top_n_res))
  
}
# here we are reading in the unzipped, raw, GloVe pre-trained word vector object (.txt)
# all you have to change is the file path to where you GloVe object has been unzipped
g6b_300 <- scan(file = "data_orig/glove.6B.300d.txt", what="", sep="\n")
  
  
# call the function to convert the raw GloVe vector to data.frame (extra lines are for wall-time reporting)
t_temp <- Sys.time()
model <- proc_pretrained_vec(g6b_300)  # this is the actual function call
(t_elap_temp <- paste0(round(as.numeric(Sys.time() - t_temp, units="mins"), digits = 2), " minutes"))

crunchbase = list()
glove <- list()
glove$model <- model
rm(list=c(g6b_300))
