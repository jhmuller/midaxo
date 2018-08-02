library(readr) # reading in a tibble
library(tm) # text mining
library(purrr) # for reduce
library(stringr) # string matching
library(tibble) # for glimpse
library(dplyr)

#' @title Read in crunchbase company description CSV file.
#'
#' @description
#' \code{read_crunchbase} reads in a crunchbase csv file and returns
#' the resulting data.frame .
#'
#' @details
#' Minor detail, creates a new column called 'Company' for the company name
#'  methods can be defined for it directly
#' @param fdir the directory for the file
#' @param fname the name of the file
#'
#' @return a data.frame containing the data
read_1_crunchbase <- function(fdir='data_orig',
                            fname="Crunchbasecompanies-25-05-2018.csv"){
  
  fpath <- file.path(fdir, fname)
  df <- readr::read_delim(fpath, delim=',', escape_double=TRUE,col_types = 
                            cols(
                              `Organization Name` = col_character(),
                              `Organization Name URL` = col_character(),
                              Categories = col_character(),
                              `Headquarters Location` = col_character(),
                              Description = col_character(),
                              `CB Rank (Company)` = col_character()
                            ))
  df['Company'] <- df['Organization Name'] 
  return (df)
} 

read_crunchbase_files <- function(){
  all_files <- list.files("data_orig")
  csv_files <- all_files[grep("companies-26-06-2018.*.csv", all_files)]
  df <- data.frame()
  for (fi in seq_along(csv_files)){
    print(csv_files[fi])
    temp <- read_1_crunchbase(fdir="data_orig", fname=csv_files[fi])
    if (nrow(df) == 0){
      df <- temp
    } else{
      df <- rbind(df, temp)
    }
  } 
  return (df)
}

drop_first <- function(x){
  if (length(x) > 1){
    res <- x[2:length(x)]
  } else {
    res <- list()
  }
  return (res)
}

drop_empty <- function(x){
  if (length(x) > 0){
    temp <- unlist(x)
    n_elems <- 0
    for (i in seq(1, length(x))){
      if (nchar(x[[i]]) > 0){
        n_elems <- n_elems + 1        
        temp[n_elems] <- x[[i]]
      }
    }
    res <- list(temp[1:n_elems])
  } else {
    res <- list()
  }
  return (res)
}

#' @title Process descriptions, i.e. prepare for vectorizing.
#'
#' @name process_descriptions
#' 
#' @description
#' \code{process_descriptions} creates a word list from the original description strings.
#' 
#'
#' @details
#' Minor detail, creates a new column called 'Company' for the company name
#'  methods can be defined for it directly
#' @param df  data.frame with the original descriptions 
#'
#' @return a data.frame with new columns for processed descriptions
process_descriptions <- function(df){
  stop_words <- stopwords("english")
  regex_special <- "\\$|\\*|\\+|\\.|\\?|\\[|\\]|\\^|\\{|\\}|\\|\\(|\\)|\\$"  
  df$Company = sapply(df$Company, str_remove_all, regex_special) 
  df$company_lower <-  sapply(df$Company, str_to_lower)
  df$Desc_mod <- sapply(df$Description, str_to_lower) 
  df$Desc_mod <- sapply(df$Desc_mod, iconv, "latin1", "ASCII", sub=' ')
  df$Desc_mod <- sapply(df$Desc_mod, str_remove_all, "\\.|,")   
  
  stop_words_regex <- paste0(" ", paste0(stop_words, collapse=" | "), " ")
  df$Desc_mod <- sapply(df$Desc_mod, str_replace_all, stop_words_regex, " ") 

  df$Categories_lower <- sapply(df$Categories, str_to_lower)
  #df <- select(df, Company, company_lower,  Dwords, Desc_mod,  Description, everything())
  return (df)
}

df <-  read_crunchbase_files()

df <- process_descriptions(df)

Dwords <- strsplit(x=df$Desc_mod, split=' ')
Dwords <- sapply(df$Dwords, drop_first )
Dwords <- sapply(df$Dwords, drop_empty )
unique_dwords = unique(unlist(df$Dwords))
cols = c("cnt")
dwordsdf <- data.frame(matrix(nrow=length(unique_dwords), ncol=length(cols)))
colnames(dwordsdf) <- cols
dwordsdf$cnt <- rep(0, nrow(dwordsdf))
row.names(dwordsdf) <- unique_dwords

for (i in seq(1, nrow(df))){
  dwords <- unique(df$Dwords[i][[1]])
  if (length(dwords) == 0){
    next
  }
  for (j in seq_along(dwords[[1]])){
    dword <- dwords[[1]][j]
    print (paste(j, dword))
    dwordsdf[dword, "cnt"] <- dwordsdf[dword, "cnt"] + 1
  }
}

df$Dword_wts = rep(0, nrow(df))
for (i in seq(1, nrow(df))){
  print (i)
  
  dwords_list <- df$Dwords[i][[1]]
  if (length(dwords_list) == 0){
    next
  }
  dwords <- dwords_list[[1]]
  word_wts = 1.0/dwordsdf[dwords,] 
  word_wts =  round(word_wts/sum(word_wts), 3)
  print(word_wts)
  df$Dword_wts[i] = list(word_wts)
}

#
write.csv

#

crunchbase = list()
crunchbase$cbase <- df
rm(list = c('df'))
