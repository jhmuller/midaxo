
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




# using the function -------------------------------------------------------------------------

# here we are reading in the unzipped, raw, GloVe pre-trained word vector object (.txt)
# all you have to change is the file path to where you GloVe object has been unzipped
g6b_300 <- scan(file = "data/glove.6B.300d.txt", what="", sep="\n")


# call the function to convert the raw GloVe vector to data.frame (extra lines are for wall-time reporting)
t_temp <- Sys.time()
glove.300 <- proc_pretrained_vec(g6b_300)  # this is the actual function call
(t_elap_temp <- paste0(round(as.numeric(Sys.time() - t_temp, units="mins"), digits = 2), " minutes"))
