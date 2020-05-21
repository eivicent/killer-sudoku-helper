library(tidyverse)

combi <- function(value, cells){
  x    <- combn(c(1:9), cells)
  x2 <- colSums(x)
  possible_combinations <- t(x[,which(x2 == value)])
  return(possible_combinations)
}

possible_combinations <- expand.grid(c(3:45), c(2:9))
names(possible_combinations) <- c("totValue", "cells")
combinations <- apply(possible_combinations, 1, function(x) combi(x[1], x[2]))

combinations_summary <- cbind(possible_combinations, 
                               "combinations" = sapply(combinations, nrow)) %>%
  filter(combinations > 0) %>%
  arrange(combinations)


