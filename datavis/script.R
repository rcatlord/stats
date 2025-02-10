# Anscombe's quartet
# Source: Anscombe, F.J. (1973). Graphs in Statistical Analysis. The American Statistician 27, 1, 17-21.
# URL: https://www.jstor.org/stable/2682899

library(tidyverse)

# tidy data
anscombe_tidy <- anscombe |> 
  pivot_longer(
    cols = everything(), 
    cols_vary = "slowest",
    names_to = c(".value", "set"), 
    names_pattern = "(.)(.)"
  )

# summary statistics
anscombe_tidy |> 
  group_by(set) |> 
  summarise(
    mean_x    = mean(x),
    mean_y    = mean(y),
    std_dev_x = sd(x),
    std_dev_y = sd(y),
    corr_x_y  = cor(x, y)
  )

