source("2022/helpers.R")
raw_input <- get_aoc_data(3)


# Part 1 --------------------------------------------------------------------------------------

priority <- function(x) {
  if (x == tolower(x)) which(letters == x)
  else which(LETTERS == x) + 26
}

split_sack <- function(x) {
  x <- strsplit(x, '')[[1]]
  list(x[1:(length(x)/2)], x[length(x)/2 + 1:(length(x)/2)])
}

items_in_both <- function(x) {
  intersect(x[[1]], x[[2]])
}

splitby("\n", TRUE)(raw_input) |>
  map_dbl(fcomp(split_sack, items_in_both, priority)) |>
  sum()


# Part 2 --------------------------------------------------------------------------------------

split_n <- function(n) \(x) split(x, ceiling(seq_along(x)/n))
load_in <- fcomp(splitby("\n", TRUE), split_n(3))
prio <- fcomp(splitby(""), \(x) Reduce(intersect, x), priority)
map_prios <- function(x) map_dbl(x, prios)

raw_input |>
  load_in() |>
  map(prio) |>
  reduce(sum)
