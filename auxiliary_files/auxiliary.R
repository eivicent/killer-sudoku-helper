rm(list = ls())
library(shiny)
library(shinydashboard)
library(tidyverse)
library(reactable)
library(shinythemes)

combi <- function(value, cells){
  x    <- combn(c(1:9), cells)
  x2 <- colSums(x)
  possible_combinations <- t(x[,which(x2 == value)])
  return(possible_combinations)
}

possible_combinations <- expand.grid(totValue = c(3:45), cells =  c(2:9)) %>%
  unite(key, totValue, cells, remove = F, sep="")

combinations <- apply(possible_combinations[,c("totValue", "cells")], 1, 
                      function(x) combi(x[1], x[2]))
names(combinations) <- possible_combinations$key

valid_combinations <- Filter(function(x) nrow(x) > 0 ,combinations)
amount_of_combinations <- enframe(sapply(valid_combinations, nrow),
                                  name = "key", value = "combinations")

combinations_summary <- amount_of_combinations %>% 
  left_join(possible_combinations, by = "key")

 
  reactable(combi(15,3),
            defaultColDef = 
              colDef(name = NULL,
                     align = "center"),
            bordered = TRUE,
            filterable = F)
  