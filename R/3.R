source('R/helpers.R')
library(stringr)
library(purrr)

input_data <- aoc_get_data(3)

# Problem 1 -----------------------------------------------------------------------------------

n <- nchar(input_data[1])
tree_locations <- str_locate_all(input_data, '#')
tree_locations %>%
