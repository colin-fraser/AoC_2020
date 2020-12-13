source("R/helpers.R")
library(stringr)
library(purrr)

input_data <- aoc_get_data(3)

# Problem 1 -----------------------------------------------------------------------------------

n <- nchar(input_data[1])
# for indices y = 1..n and tree locations x, you hit a tree when
# 3(y-1)+1 = x mod n
input_data %>%
  str_locate_all("#") %>%
  map(~ .x[, 1]) %>%
  imap_lgl(~ any((3 * (.y - 1) + 1 - .x) %% n == 0)) %>%
  sum()

# Problem 2 -----------------------------------------------------------------------------------
f <- function(slope, idata = input_data) {
  n <- nchar(idata)
  idata %>%
    str_locate_all("#") %>%
    map(~ .x[, 1]) %>%
    imap_lgl(~ any((slope * (.y - 1) + 1 - .x) %% n == 0)) %>%
    sum()
}

prod(sapply(c(1, 3, 5, 7, .5), f))
