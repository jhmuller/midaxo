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
read_crunchbase <- function(fdir='data_orig',
           fname="Crunchbasecompanies-25-05-2018.csv"){
  
  fpath <- file.path(fdir, fname)
  df <- readr::read_delim(fpath, delim=';', col_types = 
                                cols(
                                  `Organization Name` = col_character(),
                                  `Organization Name URL` = col_character(),
                                  Categories = col_character(),
                                  `Headquarters Location` = col_character(),
                                  Description = col_character(),
                                  `CB Rank (Company)` = col_integer(),
                                  Website = col_character(),
                                  `Founded Date` = col_date(format="%d/%m/%Y"),
                                  `Company Type` = col_character(),
                                  `Number of Employees` = col_character(),
                                  `Number of Founders` = col_integer(),
                                  `Number of Funding Rounds` = col_integer(),
                                  `Last Funding Date` = col_date(format="%d/%m/%Y"),
                                  `Last Funding Amount` = col_character(),
                                  `Acquisition Status` = col_character(),
                                  X16 = col_character()
                                ))
  df['Company'] <- df['Organization Name'] 
  #df = df[, -c("X16")]
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

  df$Dwords <- strsplit(x=df$Desc_mod, split=' ')
  df$Dwords <- sapply(df$Dwords, drop_first )
  df$Dwords <- sapply(df$Dwords, drop_empty )
  df$Categories_lower <- sapply(df$Categories, str_to_lower)
  df$Catwords <- strsplit(x=df$Categories_lower, split=' ')  
  #df <- select(df, Company, company_lower,  Dwords, Desc_mod,  Description, everything())
  return (df)
}

#res = df[, c('Company', 'Description', "Desc2", "Categories")]  

df = read_crunchbase()
df <-  subset(df, select= - c(X16))
glimpse(df)
df <- process_descriptions(df)
glimpse(df)

unique_dwords = unique(unlist(df$Dwords))
cols = c("cnt")
dwordsdf <- data.frame(matrix(nrow=length(unique_dwords), ncol=length(cols)))
colnames(dwordsdf) <- cols
dwordsdf$cnt <- rep(0, nrow(dwordsdf))
row.names(dwordsdf) <- unique_dwords
for (i in seq(1, nrow(df))){
  dwords <- unique(df$Dwords[i][[1]])
  for (j in seq_along(dwords)){
    dword <- dwords[j]
    print (paste(j, dword))
    dwordsdf[dword, "cnt"] <- dwordsdf[dword, "cnt"] + 1
  }
}

df$Dword_wts = rep(0, nrow(df))
for (i in seq(1, nrow(df))){
  print (i)
  dwords <- df$Dwords[i][[1]]
  word_wts = 1.0/dwordsdf[df$Dwords[i][[1]],] 
  word_wts =  round(word_wts/sum(word_wts), 3)
  print(word_wts)
  df$Dword_wts[i] = list(word_wts)
}

#


#

crunchbase = list()
crunchbase$cbase <- df
rm(list = c('df'))
