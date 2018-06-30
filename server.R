#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)

indir <- file.path( "data_derived")
print(indir)
catmat <- as.matrix(read.csv(file.path( indir, "catmat.csv"), sep="|"))
descmat <- as.matrix(read.csv(file.path( indir, "descmat.csv"), sep="|"))

cbase <- readr::read_delim(file.path("data_orig", "Crunchbasecompanies-25-05-2018.csv"), delim=';')
cbase['Company'] <- cbase['Organization Name']



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$matchTable <- DT::renderDataTable({
    cName = input$companySelect
    catWt <- as.double(input$catWt)
    if (is.matrix(catmat) && is.matrix(descmat)){
      vmat <- (catmat*catWt + (1-catWt)*descmat)
    } else {
      vmat <- catmat
    }
    simmat <- find_sim_wvs(this_wv=vmat[,cName], all_wvs=vmat, top_n_res=input$numMatches)
    if (is.null(simmat) || is.na(simmat)){
      return (data.frame(Name=cname))
    } 
    simdf <- data.frame(simmat)
    simdf$Description <- rep(1, input$numMatches)
    colnames(simdf) <- c("Score", "Description")     
    simdf$Name <- row.names(simdf)
   
    for (si in 1:nrow(simdf)){
      if (simdf$Name[si] %in% cbase$Company){
        simdf$Description[si] <- cbase$Description[cbase$Company == simdf$Name[si]]
      }else{
        simdf$Description[si] <- "Unknown"
      }
    }
    simdf$Score <- sapply(simdf$Score, round, 3)
    simdf
  }, options = list(sDom  = '<"top">t<"bottom">'))
  
})
