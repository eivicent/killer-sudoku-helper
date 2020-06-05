rm(list = ls())
library(shiny)
library(shinydashboard)
library(tidyverse)
library(reactable)
library(shinythemes)
library(crosstalk)

combi <- function(value, cells){
  x    <- combn(c(1:9), cells)
  x2 <- colSums(x)
  possible_combinations <- t(x[,which(x2 == value)])
  return(possible_combinations)
}

convert_wide_combi <- function(combi){
  output <- combi %>% as.data.frame %>% mutate(combination = row_number()) %>%
    gather(key, value, -combination) %>%
    mutate(key = value) %>% 
    spread(value, key)
  
  return(output)
}

possible_combinations <- expand.grid(totValue = c(3:45), cells =  c(2:9)) %>%
  unite(key, totValue, cells, remove = F, sep="_")

combinations <- apply(possible_combinations[,c("totValue", "cells")], 1, 
                      function(x) combi(x[1], x[2]))
names(combinations) <- possible_combinations$key

valid_combinations_wide <- lapply(combinations, convert_wide_combi) %>%
  bind_rows(.id = "key") %>%
  group_by(key) %>%
  mutate(combinations = max(combination)) %>%
  separate(key, c("totValue", "cells"), sep = "_")

data <- SharedData$new(valid_combinations_wide)


  reactable(combi(15,3),
            defaultColDef = 
              colDef(name = NULL,
                     align = "center"),
            bordered = TRUE,
            filterable = F)
  