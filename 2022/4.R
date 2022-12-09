source("2022/helpers.R")
data <- get_aoc_data(4)


# Part 1 --------------------------------------------------------------------------------------

# parse_line :: character(1) -> integer(2)
parse_line <- fcomp(
  splitby(",", TRUE),
  splitby("-"),
  fmap(as.numeric)
)

# one_contains :: character(1) -> logical(1)
one_contains <- fcomp(
  parse_line,
  freduce(\(x, y) x-y),
  \(x) any(x == 0) || do.call(xor, as.list(x>0))
)

# parse_data :: character(1) -> list(character)
parse_data <- splitby("\n", T)

data |>
  parse_data() |>
  map_lgl(one_contains) |>
  sum()


# Part 2 --------------------------------------------------------------------------------------

# arrange_pair :: list(numeric(2), numeric(2)) -> list(numeric(2), numeric(2))
arrange_pair <- function(x) {
  if (x[[1]][1] <= x[[2]][1]) {
    x
  } else {
    list(x[[2]], x[[1]])
  }
}

# assume x[1]<y[1]
overlaps_sorted <- function(x) {
  (x[[1]][2] >= x[[2]][1])
}

overlaps <- fcomp(arrange_pair, overlaps_sorted)

data |>
  parse_data() |>
  map(parse_line) |>
  map_lgl(overlaps) |>
  sum()
