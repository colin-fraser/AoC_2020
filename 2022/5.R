source("2022/helpers.R")

data <- get_aoc_data(5)

example_data <- "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"

move_crates <- function(crates, dirs) {
  n <- dirs[[1]]
  from <- dirs[[2]]
  to <- dirs[[3]]
  stack <- crates[[from]][n:1]
  crates[[from]] <- crates[[from]][-(1:n)]
  crates[[to]] <- c(stack, crates[[to]])
  crates
}


# Read Crates ---------------------------------------------------------------------------------

crate_block <- fcomp(
  splitby("\n\n", T),
  as_mapper(1),
  splitby("\n", T),
  \(x) x[-length(x)]
)

read_crate_line <- function(x) {
  nchars <- nchar(x)
  n <- (nchars + 1)/4
  x <- strsplit(x, "")[[1]][seq(2, nchars-1, by = 4)]
  x
}

read_raw_crates <- fcomp(
  crate_block,
  fmap(read_crate_line))

crate_lines_to_crate_cols <- fcomp(
  transpose,
  fmap(\(x) x[x != ' '])
)

read_crates <- fcomp(
  read_raw_crates,
  crate_lines_to_crate_cols
)

# Instructions --------------------------------------------------------------------------------

parse_instruction <- fcomp(
  partial(stringr::str_extract_all, pattern = "\\d+"),
  unlist,
  as.numeric,
  as.list
)

read_raw_instructions <- fcomp(
  splitby("\n\n", T),
  as_mapper(2)
)

read_instructions <- fcomp(
  read_raw_instructions,
  partial(stringr::str_extract_all, pattern = "\\d+"),
  unlist,
  as.numeric,
  \(x) split(x, ceiling(seq_along(x)/3)),
  fmap(as.list)
)


# Solution ------------------------------------------------------------------------------------


instructions <- read_instructions(data)
crates <- read_crates(data)
for (ins in instructions) {
  crates <- move_crates(crates, ins)
}
paste(map_chr(crates, 1), collapse = "")


# Part 2 --------------------------------------------------------------------------------------

move_crates2 <- function(crates, dirs) {
  n <- dirs[[1]]
  from <- dirs[[2]]
  to <- dirs[[3]]
  stack <- crates[[from]][1:n]
  crates[[from]] <- crates[[from]][-(1:n)]
  crates[[to]] <- c(stack, crates[[to]])
  crates
}

instructions <- read_instructions(data)
crates <- read_crates(data)
for (ins in instructions) {
  crates <- move_crates2(crates, ins)
}
paste(map_chr(crates, 1), collapse = "")


