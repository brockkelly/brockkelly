# This function triangulararbitrage() takes an input of a pair of currencies and
# calculates the profit from all possible triangular arbitrage trades from a list
# of 14 currencies

# importing libraries
library(quantmod)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(BatchGetSymbols)

rm(list = ls())

# currencies

currencies <- as.data.frame(read_csv("Desktop/Personal Projects/export.csv"))
currencies <- filter(currencies, CODE != "CNH")

# pair

pair <- c("USD", "JPY")

triangulararbitrage <- function(pair) {

# create valid currency pairs

  basetoalternate <- filter(currencies, CODE != pair[1] & CODE != pair[2])
  basetoalternate <- paste(pair[1], basetoalternate$CODE, "=X", sep = "")

  alternatetoquote <- filter(currencies, CODE != pair[2] & CODE != pair[1])
  alternatetoquote <- paste(alternatetoquote$CODE, pair[2], "=X", sep = "")

# organizing them into a dataframe and sorting them to create valid triangle
# arbitrage trades
  triangles <- data.frame(basealt = as.matrix(sort(basetoalternate)), 
                        altquote = as.matrix(sort(alternatetoquote)))

# defining an empty matrix to place the profit from each triangle arbitrage trade
# into

  profit <- matrix(NA, nrow = nrow(triangles))

# need to loop over data frame and collect the profit for each possible triangle
# trade

  for (i in 1:nrow(triangles)) {
  
  # getting quotes from beginning of year of for BASE/ALT, ALT/QUOTE, BASE/QUOTE
  
  # BASE/ALT ex. CAD/AUD
    ex1 <- getSymbols(Symbol = triangles[i, 1], src = "yahoo", 
                    from = "2021-01-01", auto.assign = FALSE)
  
  # ALT/QUOTE ex. AUD/USD
    ex2 <- getSymbols(Symbol = triangles[i, 2], src = "yahoo", 
                    from = "2021-01-01", auto.assign = FALSE)
  
  # BASE/QUOTE ex. CAD/USD
    x <- paste(pair[1], pair[2], "=X", sep = "")
    ex3 <- getSymbols(Symbol = x, src = "yahoo", from = "2021-01-01", auto.assign = FALSE)
  
  # storing it in a data frame
    data <- data.frame(basetoalt=as.matrix(ex1[,4]), 
                     alttoquote=as.matrix(ex2[,4]), 
                     basetoquote = as.matrix(ex3[,4]), 
                     date=time(ex1))
    colnames(data) = c("basetoalt", "alttoquote", "basetoquote", "date")
  
  # profit calculation
  # ex. profit = $1000 [( CAD/AUD * AUD/USD ) - CAD/USD]
    data <- mutate(data, profit = 1000*abs((basetoalt*alttoquote) - basetoquote))
    profit[i,] <- sum(data$profit)
    
    #profit$basealt <- as.vector(basetoalternate)
    #profit$altbase <- as.vector(alternatetobase)
  }
  
  profit <- data.frame(profit = as.matrix(profit),
                       basetoalt=as.matrix(basetoalternate), 
                       alttoquote=as.matrix(alternatetoquote))
  colnames(profit) <- c("Potential Profit", "BASE/ALT", "ALT/QUOTE")
  return(profit)
  
}

triangulararbitrage(pair)








