#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(DT)
library(dplyr)

indir <- file.path( "data_derived")
print(indir)
tempdf <- read.csv(file.path( indir, "catmat.csv"), sep="|",nrows=1)
cnames <- colnames(tempdf)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("similar Companies"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "catWt",
                  label = "Category Weight:",
                  min = 0.0,
                  max = 1.0,
                  value = 50),      
      selectizeInput("numMatches", "Number of Matches", 
                     choices=c(1, 2, 5, 10, 20, 30),
                     selected = 1, multiple = FALSE,
                     options = NULL),       
      selectizeInput("companySelect", "Company", 
                     choices=cnames,
                     selected = NULL, multiple = FALSE,
                     options = NULL)), 
    mainPanel(
       DT::dataTableOutput("matchTable")       
    )
  )
))
