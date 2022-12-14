source("2022/helpers.R")

mapper <- \(f, ...) \(x) map(x, f, ...)
getter <- \(...) \(l) pluck(l, ...)

extract_number <- \(x) as.numeric(stringr::str_extract(x, "\\d+"))

parse_data <- fcomp(splitby("\n\n", TRUE), splitby("\n"), mapper(trimws))

# parse monkey number
parse_monkey_no <- extract_number

# parse starting  items
parse_starting_items <- fcomp(splitby(": ", T), \(x) x[-1], splitby(", ", T), as.numeric)
parse_starting_items("Starting items: 79, 98")

# parse operation
op_line_function <- \(s) new_function(pairlist2(old = ), parse_expr(s))
parse_operation_line <- fcomp(splitby("= ", T), getter(2), op_line_function)

# parse test
parse_test <- \(test, true, false) \(x) if (x %% extract_number(test) == 0) extract_number(true) else extract_number(false)


new_monkey_config <- \(mn, items, op, test, inspection_record) list(mn = mn, items = items, op = op, test = test, inspection_record = inspection_record)

parse_chunk <- \(x) new_monkey_config(parse_monkey_no(x[1]),
                                      parse_starting_items(x[2]),
                                      parse_operation_line(x[3]),
                                      parse_test(x[4], x[5], x[6]),
                                      0
                                      )


# states --------------------------------------------------------------------------------------

throw <- function(state, i, item, from, to) {
  state[[from+1]][['items']] <- state[[from+1]][['items']][-1]
  state[[to+1]][['items']] <- c(state[[to+1]][['items']], item)
  state
}

add_count_to_monkey <- function(state, mn, n) {
  state[[mn+1]][['inspection_record']] <- state[[mn+1]][['inspection_record']] + n
  state
}


turn <- function(mn, state, noisy = FALSE) {
  monkey <- state[[mn+1]]
  state <- add_count_to_monkey(state, mn, length(monkey$items))
  for (i in seq_along(monkey$items)) {
    item <- fcomp(getter('items', i), monkey$op, \(x) floor(x/3))(monkey)
    state <- throw(state, i, item, from = mn, to = monkey$test(item))
  }
  state
}


round <- function(state, turn_function = turn, noisy = FALSE, ...) {
  for (i in seq_along(state) - 1) {
    state <- turn_function(i, state, noisy, ...)
  }
  state
}



rounds <- function(initial_state, n = 20, turn_function = turn, ...) {
  state <- initial_state
  for (i in 1:n) {
    state <- round(state, turn_function, ...)
  }
  state
}

library(tibble)
library(dplyr)

get_monkey_business <- function(state) {
  state |>
    map_df(\(x) tibble(mn = x$mn, record = x$inspection_record)) |>
    slice_max(record, n = 2) |>
    summarise(prod(record)) |>
    pull()
}


get_aoc_data(11) |>
  parse_data() |>
  map(parse_chunk) |>
  rounds(20, turn_function = turn) |>
  get_monkey_business()

# Part 2 --------------------------------------------------------------------------------------

new_monkey_config <- \(mn, items, op, test, inspection_record, modulus, ...) {
  list(mn = mn, items = items, op = op, test = test, inspection_record = inspection_record,
       modulus = modulus, ...)
  }

parse_test_human <- function(x) {
  paste(extract_number(x[1]), ':', extract_number(x[2]), '|', extract_number(x[3]))
}

get_mod <- \(s) prod(map_dbl(s, 'modulus'))

# shrink the number to the mod of the new monkey in order to prevent integer overflow
throw2 <- function(state, i, item, from, to, noisy = FALSE, debug_mod = FALSE, mod = get_mod(state)) {
  item <- item %% mod
  if (noisy) {
    cat("Throwing item", item, "from", from, "to", to, '\n')
  }
  state[[from+1]][['items']] <- state[[from+1]][['items']][-1]
  state[[to+1]][['items']] <- c(state[[to+1]][['items']], item)
  state
}

# rewrite parse_chunk
parse_chunk <- \(x) new_monkey_config(parse_monkey_no(x[1]),
                                      as.integer(parse_starting_items(x[2])),
                                      parse_operation_line(x[3]),
                                      parse_test(x[4], x[5], x[6]),
                                      0,
                                      extract_number(x[4]),
                                      test_human_readable = parse_test_human(x[4:6]),
                                      op_human_readable = x[3])

turn2 <- function(mn, state, noisy = FALSE, update_worry = identity, mod, ...) {
  monkey <- state[[mn+1]]
  state <- add_count_to_monkey(state, mn, length(monkey$items))
  for (i in seq_along(monkey$items)) {
    item <- fcomp(getter('items', i), monkey$op, update_worry)(monkey)
    state <- throw2(state, i, item, from = mn, to = monkey$test(item), noisy, mod = mod, ...)
  }
  state
}


state_to_df <- \(s) map_df(s, \(x) {
  as_tibble(x[c('mn', 'inspection_record', 'test_human_readable', 'op_human_readable')]) |>
    mutate(items = paste(x$items, collapse = ','))
  }
  )

rounds_to_df <- function(initial_state, n, turn_function, noisy = FALSE, ...) {
  state <- initial_state
  mod <- get_mod(initial_state)
  df <- state_to_df(state) |> mutate(round = 0)
  for (i in seq_len(n)) {
    state <- round(state, turn_function, noisy = noisy, mod = mod, ...)
    df <- bind_rows(df, state_to_df(state) |> mutate(round = i))
  }
  df
}

initstate <- get_aoc_data(11) |>
  parse_data() |>
  map(parse_chunk)


rounds(initstate, 20, turn2, mod = get_mod(initstate)) |>
  state_to_df()
