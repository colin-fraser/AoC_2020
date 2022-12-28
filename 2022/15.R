source("2022/helpers.R")
library(tibble)
library(dplyr)


# Part 1 ----------------------------------------------------------------------------------------------------------

parse_in <- fcomp(
  splitby("\n", TRUE),
  \(x) stringr::str_extract_all(x, "(?<=[xy]=)-?\\d+"),
  fmap(as.numeric),
  fmap(\(x) setNames(x, c('sensor_x', 'sensor_y', 'beacon_x', 'beacon_y'))),
  dplyr::bind_rows
)

man_dist <- \(x, y) sum(abs(x-y))
xrange <- \(df) range(c(df$sensor_x, df$beacon_x))
yrange <- \(df) range(c(df$sensor_y, df$beacon_y))

add_distance <- function(df) {
  df |>
    rowwise() |>
    mutate(dist = man_dist(c(sensor_x, sensor_y), c(beacon_x, beacon_y))) |>
    ungroup()
}

get_blocs <- function(df) distinct(df, beacon_x, beacon_y)
get_beacons_in_row <- function(df, row) {
  filter(get_blocs(df), beacon_y == row) |>
    pull(beacon_x)
}

circle_intersection_with_row <- function(row, y) {
  radius <- row$dist
  sensor_loc <- c(row$sensor_x, row$sensor_y)
  slack <- radius - abs(sensor_loc[2] - y)
  if (slack < 0) {
    return(NULL)
  }
  sensor_loc[1] + (-slack):slack
}

row_locs_with_no_beacon <- function(df, y) {
  transpose(df[c('sensor_x', 'sensor_y', 'dist')]) |>
    map(circle_intersection_with_row, y = y) |>
    reduce(union)
}


solve <- function(x, y) {
  df <- parse_in(x) |>
    add_distance()
  blocs <- get_beacons_in_row(df, y)

  potential_locs <- row_locs_with_no_beacon(df, y)

  length(setdiff(potential_locs, blocs))
}

# solve(get_aoc_data(15), 2000000)


# Part 2 ----------------------------------------------------------------------------------------------------------

get_boundary <- function(ctr, radius, xrange, yrange) {
  cap <- function(r) \(x) ifelse(x < r[1], r[1], ifelse(x > r[2], r[2], x))
  cap_x <- cap(xrange)
  cap_y <-cap(yrange)
  unique(rbind(
    data.frame(x = cap_x(ctr[1] + 0:radius), y = cap_y(ctr[2] + radius:0)),
    data.frame(x = cap_x(ctr[1] + (radius-1):0), y = cap_y(ctr[2] + -(1:radius))),
    data.frame(x = cap_x(ctr[1] + -(1:radius)), y = cap_y(ctr[2] + -(radius-1):0)),
    data.frame(x = cap_x(ctr[1] + -(radius-1):(-1)), y = cap_y(ctr[2] + 1:(radius-1)))
  ))
}

location_not_in_circ <- function(x, y, df) {
  none(df, \(r) man_dist(c(r$sensor_x, r$sensor_y), c(x, y)) <= r$dist)
}

search_points <- function(bdf, locs) {
  out <- detect(transpose(bdf), \(z) location_not_in_circ(z$x, z$y, locs))
  if (!is.null(out)) {
    done(out)
  } else {
    out
  }
}

search_circle <- function(row, locs, xrange, yrange) {
  boundary <- get_boundary(c(row$sensor_x, row$sensor_y), row$dist + 1, xrange, yrange)
  search_points(boundary, locs)
}

search_circles <- function(locs, xrange, yrange) {
  transposed_locs <- transpose(locs)
  transposed_locs |>
    reduce(\(., r) search_circle(r, transposed_locs, xrange, yrange), .init = NULL)
}

test_in <- "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"

sol <- parse_in(get_aoc_data(15)) |>
  add_distance() |>
  search_circles(c(0, 4000000), c(0, 4000000))

sol$x * 4000000 + sol$y
