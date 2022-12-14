source("2022/helpers.R")


# Parsing -------------------------------------------------------------------------------------

parse_data <- function(x) {
  x <- x |> then(fcomp(splitby("\n", TRUE), splitby("")))
  n <- length(x)
  matrix(unlist(x), byrow = TRUE, nrow = n)
}

letters_to_numeric <- \(x) apply(x, c(1, 2), \(y) switch(y,
  S = 1,
  E = 26,
  which(letters == y)
))


# Problem 1 -----------------------------------------------------------------------------------

# return unvisited neighbors
available_neighbors <- function(x, m, visited = 0) {
  rows <- nrow(m)
  cols <- ncol(m)
  out <- c(x - rows, x + rows, x - 1, x + 1)
  out <- out[(out <= rows * cols) & (out >= 1) & !(out %in% visited)]
  out[(m[out] <= m[x] + 1)]
}

# D will keep track of the best distances from start at each iteration.
# Initialize it with all Infinity.
initialize_D <- function(m, start) {
  m[] <- Inf
  m[start] <- 0
  m
}

# One iteration - from a position x in M, compute current best distances to x's unvisited neighbors
iteration <- function(M, x, end, D = initialize_D(M, x), visited = 0) {
  Reduce(\(D, nb) {
    D[nb] <- min(D[nb], D[x] + 1)
    D
  }, available_neighbors(x, M, visited), init = D)
}

# Basically Dijkstra's algorithm
find_paths <- function(M, start, end) {
  D <- initialize_D(M, start)
  x <- start
  visited <- 0

  while (x != end) {
    D <- iteration(M, x, end, D, visited)
    visited <- c(visited, x)
    if (all(is.infinite(D[-visited]))) {
      return(D)
    }
    x <- which.min(replace(D, visited, Inf))
  }

  D
}

# Solve

data <- get_aoc_data(12)
L <- parse_data(data)
start <- which(L == "S")
end <- which(L == "E")
M <- letters_to_numeric(L)


sols <- find_paths(M, start, end)
sols[end]

# Part 2 --------------------------------------------------------------------------------------
# Incredibly short! Just run Djikstra's in reverse from end!

M2 <- -M
sols2 <- find_paths(M2, end, 0)
min(sols2[L == "a"])

# Plotting ------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

path_from_dist_mat <- function(D, end) {
  path <- end
  while (D[path[1]] > 0) {
    neighbors <- available_neighbors(path[1], D)
    path <- c(neighbors[which(D[path[1]] - D[neighbors] == 1)[1]], path)
  }
  path
}

m_to_df <- function(m, value_col = "value") {
  tibble::as_tibble(m, rownames = "Row") |>
    tidyr::pivot_longer(cols = -Row, names_to = "Col", values_to = value_col) |>
    mutate(Col = stringr::str_sub(Col, 2)) |>
    mutate(across(c(Row, Col), as.numeric))
}

l_to_df <- function(L) {
  m_to_df(L, "letter") |>
    left_join(m_to_df(letters_to_numeric(L)))
}

path_df <- function(path, dim) {
  arrayInd(path, dim) |>
    as_tibble() |>
    transmute(Row_to = V1, Col_to = V2, Row = lag(V1), Col = lag(V2)) |>
    dplyr::slice(-1)
}

plot_L <- function(Ldf) {
  ggplot(Ldf, aes(x = Col, y = Row, label = letter, color = value)) +
    geom_text(show.legend = FALSE) +
    scale_color_gradient2(low = "green", mid = "yellow", high = "red") +
    coord_fixed() +
    theme_void() +
    theme(plot.background = element_rect(fill = "grey20")) +
    scale_y_reverse()
}

plot_sol <- function(path, L) {
  Ldf <- l_to_df(L)
  pdf <- path_df(path, dim(L))
  p <- plot_L(Ldf)
  p <- p + geom_segment(
    data = pdf,
    aes(xend = Col_to, yend = Row_to, x = Col, y = Row),
    inherit.aes = FALSE,
    linewidth = 2,
    color = "grey90", alpha = 0.9
  )

  p
}

# Part 1 plot
plot_sol(path_from_dist_mat(sols, end), L)

# Part 2 plot
m_to_df(sols2) |>
  left_join(l_to_df(L), by = c("Row", "Col")) |>
  mutate(value = value.x) |>
  plot_L()
