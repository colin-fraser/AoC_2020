source("2022/helpers.R")
raw_input <- get_aoc_data(2)


# Part 1 --------------------------------------------------------------------------------------
library(purrr)
fcomp <- \(...) compose(..., .dir = 'forward')  # forward composition
map_funcs <- \(...) function(l) map2(list(...), l, \(f, x) f(x))

# translator :: (a = b, ..., a = b) -> a -> b
translator <- function(...) \(x) switch(x, ..., stop("No translation provided for ", x))

# from_abc :: character(1) -> character(1)
from_abc <- translator(A = 'Rock', B = 'Paper', C = 'Scissors')

# from_xyz :: character(1) -> character(1)
from_xyz <- translator(X = 'Rock', Y = 'Paper', Z = 'Scissors')

# to_numeric :: character(1) -> numeric(1)
to_numeric <- translator(Rock = 0L, Paper = 1L, Scissors = 2L)

# move_pair_to_numeric -> character(2) -> numeric(2)
move_pair_to_numeric <- fcomp(
  map_funcs(fcomp(from_abc, to_numeric), fcomp(from_xyz, to_numeric)),
  unlist)

# numeric_outcome_from_numeric :: numeric(2) -> numeric(1)
numeric_outcome_from_numeric <- \(x) (x[2] - x[1] - 1) %% 3 + 1L
# numeric_outcome_to_english :: numeric(1) -> character(1)
numeric_outcome_to_english <- translator("Win", "Lose", "Draw")

# shape_score_from_numeric :: numeric(2) -> numeric(1)
shape_score_from_numeric <- \(x) x[2] + 1L

# outcome_score_from_numeric_game :: numeric(2) -> numeric(1)
outcome_score_from_numeric_game <- fcomp(
  numeric_outcome_from_numeric,
  numeric_outcome_to_english,
  translator("Lose" = 0L, "Draw" = 3L, "Win" = 6L)
)

# outcome_score_from_numeric_game :: numeric(2) -> numeric(1)
game_score <- \(x) shape_score_from_numeric(x) + outcome_score_from_numeric_game(x)

# outcome_score_from_numeric_game :: character(1) -> list(list(character()))
splitby <- function(sep) \(x) strsplit(x, sep)

# raw_in_to_movelist :: character(1) -> list(character(2))
raw_in_to_movelist <- fcomp(splitby("\n"), unlist, splitby(" "))

raw_input |>
  raw_in_to_movelist() |>
  map_dbl(
    fcomp(
      move_pair_to_numeric,
      game_score
    )
  ) |>
  sum()

# Part 2 --------------------------------------------------------------------------------------

# win_instruction :: character(1) -> numeric(1)
win_instruction_to_numeric <- fcomp(translator(X = 'Lose', Y = 'Draw', Z = 'Win'),
                                    translator(Draw = 0, Win = 1, Lose = -1))

# move_pair_from_raw :: character(2) -> numeric(2)
move_pair_from_character <- fcomp(
  map_funcs(fcomp(from_abc, to_numeric), win_instruction_to_numeric),
  unlist)

# choose_move_from_numeric :: numeric(2) ->
choose_move_from_numeric <- \(x) c(x[1], (x[1] + x[2]) %% 3)


raw_input |>
  raw_in_to_movelist() |>
  map_dbl(fcomp(move_pair_from_character,
            choose_move_from_numeric,
            game_score)) |>
  sum()
