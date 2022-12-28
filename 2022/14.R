source("2022/helpers.R")
library(tibble)
library(ggplot2)
library(dplyr)
mapper <- \(f) \()

data <- get_aoc_data(14)
test_in <- "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9"


parse_in <- fcomp(splitby("\n", TRUE), splitby(" -> "),
                  fmap(
                  fcomp(
                    splitby(","),
                    fmap(as.numeric)
                  )
  )
)

rep_or_seq <- \(n) \(x,y) if (x == y) rep(x, n) else (x:y)[-1]

expand_coord_pair <- function(l, x) {
  if (is.numeric(l)) {
    l <- list(l)
  }
  x0 <- l[[length(l)]]
  rep <- rep_or_seq(max(abs(x0 - x)))
  c(l, map(transpose(list(rep(x0[1], x[1]), rep(x0[2], x[2]))), unlist))
}

expand_coords <- \(l) reduce(l, expand_coord_pair)
expand_all_in <- fcomp(fmap(expand_coords), squash)

check_empty <- function(x, cd) {
  !(x[2] %in% cd[[as.character(x[1])]])
}

update_coord_dict <- function(l, x) {
  if (check_empty(x, l)) {
    l[[as.character(x[1])]] <- sort(c(l[[as.character(x[1])]], x[2]))
  }
  l
}

key_range <- fcomp(names, as.numeric, range)

lookup <- function(dict, what) {
  dict[[as.character(what)]] %||% Inf
}

make_coord_dict <- \(l) reduce(l, update_coord_dict, .init = list())

move_down <- \(x, steps_down = 1, steps_right = 0) x + c(steps_right, steps_down)

move_down_to <- \(x, to, steps_right = 0) c(x[1] + steps_right, to)

move_sand <- function(x, rocks_dict, sand_dict) {
  if (is.infinite(x[2])) {
    return (x)
  }
  spaces_below <- map(list(c(0, 1), c(-1, 1), c(1, 1)), \(y) x + y)
  column <- detect(spaces_below, \(x) check_empty(x, rocks_dict) && check_empty(x, sand_dict))[1]
  if (!is.null(column)) {
    obstacles <- c(lookup(rocks_dict, column), lookup(sand_dict, column))
    if (!is.null(obstacles)) {
      move_sand(c(column, min(obstacles[obstacles > x[2]]) - 1), rocks_dict, sand_dict)
    } else {
      x
    }
  } else {
    x
  }
}

sim <- function(rocks_dict, max_iter = 5000) {
  sand_hist <- list()
  sand_dict <- list()
  counter <- 0
  y_value <- 0
  while (y_value < Inf) {
    sand_loc <- move_sand(c(500, 0), rocks_dict, sand_dict)
    y_value <- sand_loc[2]
    counter <- counter + 1
    sand_dict <- update_coord_dict(sand_dict, sand_loc)
    sand_hist <- c(sand_hist, list(sand_dict))
    if (counter > max_iter) {
      warn("Passed max iter")
      break
    }
  }
  sand_hist
}

rocks <- parse_in(get_aoc_data(14)) |>
  expand_all_in() |>
  make_coord_dict()

s <- sim(rocks)
length(s) - 1


coord_df <- function(cdf, itemtype) {
  imap_dfr(cdf, ~ tibble(x = as.numeric(.y), y = .x, item_type = itemtype))
}

plot_frame <- function(rocks = x, sand = list()) {
  rockdf <- coord_df(rocks, itemtype = 'rock')
  sanddf <- coord_df(sand, itemtype = 'sand')
  bind_rows(rockdf, sanddf) |>
    ggplot(aes(x = x, y = y, color = item_type)) +
    geom_point(show.legend = FALSE) +
    scale_y_reverse(limits = c(NA, 0)) +
    colinlib::theme1('both')
}


sh <- s |>
  map_df(coord_df, 'sand', .id = 'time') |>
  mutate(time = as.numeric(time))
library(gganimate)
sh |>
  ggplot(aes(x = x, y = y)) +
  geom_point(show.legend = FALSE, color = 'gold') +
  geom_tile(data = coord_df(rocks, 'rock'), fill = 'grey30') +
  scale_y_reverse(limits = c(NA, 0)) +
  theme_void() +
  transition_time(time)


# part 2 ----------------------------------------------------------------------------------------------------------

move_sand2 <- function(x, rocks_dict, sand_dict, floor) {
  if (x[2] == floor - 1) return(x)
  spaces_below <- map(list(c(0, 1), c(-1, 1), c(1, 1)), \(y) x + y)
  column <- detect(spaces_below, \(x) check_empty(x, rocks_dict) && check_empty(x, sand_dict))[1]
  if (!is.null(column)) {
    obstacles <- c(lookup(rocks_dict, column), lookup(sand_dict, column), floor)
    if (!is.null(obstacles)) {
      move_sand2(c(column, min(obstacles[obstacles > x[2]]) - 1), rocks_dict, sand_dict, floor)
    } else {
      x
    }
  } else {
    x
  }
}

sim2 <- function(rocks_dict, max_iter = 5000, floor = Inf) {
  sand_hist <- list()
  sand_dict <- list()
  counter <- 0
  y_value <- 1
  while (y_value > 0 && y_value < floor) {
    sand_loc <- move_sand2(c(500, 0), rocks_dict, sand_dict, floor)
    y_value <- sand_loc[2]
    counter <- counter + 1
    sand_dict <- update_coord_dict(sand_dict, sand_loc)
    sand_hist <- c(sand_hist, list(sand_dict))
    if (counter > max_iter) {
      warn("Passed max iter")
      break
    }
  }
  sand_hist
}

rocks <- parse_in(get_aoc_data(14)) |>
  expand_all_in() |>
  make_coord_dict()
s <- sim2(rocks, floor = max(unlist(rocks)) + 2, max_iter = 100000)


sh <- s |>
  map_df(coord_df, 'sand', .id = 'time') |>
  mutate(time = as.numeric(time))
