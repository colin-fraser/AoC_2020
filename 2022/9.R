source("2022/helpers.R")
data <- get_aoc_data(9)

reducer <- \(f, ...) \(x) reduce(x, f, ...)
mapper <- \(f, ...) \(x) map(x, f, ...)

data <- get_aoc_data(9)

# Part 1 --------------------------------------------------------------------------------------


tl <- \(x) tail(x, 1)[[1]]
parse_input_data <- fcomp(splitby("\n", TRUE), splitby(" "))

parse_cmd <- \(x) list(dir = x[1], steps = as.numeric(x[2]))
new_cmd <- function(dir, steps) list(dir = dir, steps = steps)
origin <- \(x=0, y=0) list(c(x,y))

# loc = current location
# move_head :: (numeric(2), chr(1), numeric(1)) -> List(numeric(2))
head_segment <- function(cmd) {
  vec <- switch(cmd$dir, 'U' = c(0, 1), 'D' = c(0, -1), 'L' = c(-1,0), 'R' = c(1, 0))
  map(1:cmd$steps, \(x) x * vec)
}

# add two head segments
add_segments <- \(x, y) c(x, map(y, \(z) z + tl(x)))

head_history <- fcomp(mapper(head_segment), reducer(add_segments))

tail_dir <- \(hloc, tloc) hloc - tloc
tail_dist <- fcomp(tail_dir, abs, max)
move_tail <- \(tloc, hloc) tloc + (tail_dist(hloc, tloc) > 1) * sign(tail_dir(hloc, tloc))
tail_moves_from_head_hist <- \(hh) accumulate(hh, move_tail, .init = c(0, 0))[-1]

parse_to_hh <- fcomp(parse_input_data, mapper(parse_cmd), head_history)


parse_to_hh(data) |>
  tail_moves_from_head_hist() |>
  unique() |>
  length()

#  Plotting for fun ------------------------------------------------------------------------------

histories <- \(cmds) {out <- list(hh = head_history(cmds)); list(hh = hh, th = tail_moves_from_head_hist(out$hh))}
history_to_df <- \(h) do.call("rbind", h) |> as_tibble() |> mutate(step = row_number())
histories <- \(hh, th) map(list(head = hh, tail = th), history_to_df) |> bind_rows(.id = "obj")
add_box <- \(x, y) tibble::tibble(x = c(0, 0, 1, 1)-.5 + x, y = c(0, 1, 1, 0)-.5 + y)

library(gganimate)
hh <- parse_input_data(data) |>
  map(parse_cmd) |>
  head_history()
th <- tail_moves_from_head_hist(hh)
steps <- 250
new_spots <- th |> history_to_df() |>
  filter(step <= steps) |>
  distinct(V1, V2, .keep_all = TRUE) |>
  transmute(step, rn = row_number()) |>
  tidyr::complete(step = 1:steps) |>
  tidyr::fill(rn, .direction = 'down')

hdf <- histories(hh, th) |>
  filter(step <= steps)
hdf |>
  group_by(step, obj, V1, V2) |>
  summarise(add_box(V1, V2)) |>
  left_join(new_spots, by = 'step') |>
  ggplot(aes(x = x, y = y, group = interaction(step, obj), fill = obj)) +
  geom_polygon(show.legend = FALSE, alpha = 0.8, position = position_jitter(width = .05, height = .05)) +
  coord_fixed() +
  theme_void() +
  geom_text(x = min(hdf$V1), y = min(hdf$V2), aes(label = glue::glue("new positions: {rn}")), hjust= 0) +
  scale_fill_manual(values = c('#BB2528', '#165B33')) +
  transition_time(step) +
  ease_aes('linear') +
  shadow_mark(alpha = 0.1, exclude_layer = 2) -> p
p
animate(p, nframes = 50 * 10, fps = 25)

# Part 2 --------------------------------------------------------------------------------------

n_tail_moves_from_head_hist <- \(hh, n) accumulate(1:n, \(x, .) tail_moves_from_head_hist(x), .init = hh)
parse_to_hh(data) |>
  n_tail_moves_from_head_hist(9) |>
  pluck(10) |>
  unique() |>
  length()

hist <- parse_to_hh(data) |>
  n_tail_moves_from_head_hist(9)

steps <- 2000
plot_data <- hist |>
  map_df(fcomp(lift_dl(rbind), as_tibble, \(x) mutate(x, step = row_number())), .id = 'pos') |>
  mutate(pos = as.numeric(pos)) |>
  mutate(obj = case_when(pos == 1 ~ 'head', pos == 10 ~ 'tail', TRUE ~ 'mid')) |>
  filter(step <= steps) |>
  arrange(step, pos)

counts <- plot_data |>
  filter(obj == 'tail') |>
  distinct(V1, V2, .keep_all = TRUE) |>
  mutate(rn = row_number()) |>
  tidyr::complete(step = 1:steps) |>
  tidyr::fill(pos, V1, V2, obj, rn, .direction = 'down')

p <- plot_data |>
  ggplot(aes(x = V1, y = V2, group = interaction(obj, pos))) +
  geom_tile(show.legend = FALSE, data = filter(plot_data, obj != 'tail'), fill = '#165B33', width=1, height = 1) +
  geom_tile(show.legend = FALSE, data = filter(plot_data, obj == 'tail'), fill = '#BB2528', width=1, height = 1) +
  geom_text(x = min(plot_data$V1), y = min(plot_data$V2),
            aes(label = glue::glue("unique positions: {rn}")), hjust= 0,
            data = counts) +
  theme_void() +
  coord_fixed() +
  ease_aes('linear') +
  transition_time(step) +
  shadow_mark(fill = '#d9a5a6', exclude_layer = c(1, 3))
animate(p, nframes = steps, fps = 25)
