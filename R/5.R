source("R/helpers.R")

seats <- aoc_get_data(5)


# Problem 1 -----------------------------------------------------------------------------------

row <- function(x) {
  x <- strsplit(x, '')[[1]][1:7]
  sum(2^(6:0)[x == 'B'])
}
col <- function(x) {
  x <- strsplit(x, '')[[1]][8:10]
  sum(2^(2:0)[x == 'R'])
}
read_id <- function(x) {
  row <- row(x)
  col <- col(x)
  row * 8 + col
}

max(sapply(seats, read_id))

# Problem 2 -----------------------------------------------------------------------------------

ids <- sort(sapply(seats, read_id))
ids[which(diff(ids) != 1)]+1
