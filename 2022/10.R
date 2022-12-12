source("2022/helpers.R")
data <- get_aoc_data(10)


# PART 1 --------------------------------------------------------------------------------------


data_to_instructions <- fcomp(splitby("\n", TRUE),
                              splitby(" "))

new_state <- \(time,
               register,
               current_instruction,
               instruction_queue,
               record) {
  list(time = time,
       register = register,
       current_instruction = current_instruction,
       instruction_queue = instruction_queue,
       record = record)
}
instructions_remaining <- \(s) length(s$instruction_queue)


new_status <- \(s, v) list(status = s, value = v)

inst <- function(value, duration, init_value, init_time) {
  function(t) {
    if (t - init_time == duration) {
      new_status('done', value)
    } else {
      new_status('processing', init_value)
    }
  }
}

addx <- \(x) \(r, t) inst(as.numeric(x)+r, 2, r, t)
noop <- \(r, t) inst(r, 1, r, t)

is_done <- \(x) x$status == 'done'

execute_current_instruction <- function(state) {
  with(state, current_instruction(time))
}

parse_cmd <- \(x) switch(x[1], "addx" = addx(x[2]), "noop" = noop)
cmd_to_instruction <- \(cmd, r, t) parse_cmd(cmd)(r, t)

new_record <- \(t, state) list(t = t, r = state$register, strength = t * state$register)

tick <- function(state) {

  t <- state$time + 1
  record <- new_record(t, state)

  cs <- state$current_instruction(t)

  r <- cs$value

  if (is_done(cs)) {
    if (length(state$instruction_queue) == 0) {
      return(new_state(t, r, NULL, list(), record))
    }
    next_instruction <- cmd_to_instruction(pluck(state$instruction_queue, 1), r, t)
    instruction_queue <- state$instruction_queue[-1]
  } else {
    next_instruction <- state$current_instruction
    instruction_queue <- state$instruction_queue
  }
  new_state(t,
            cs$value,
            next_instruction,
            instruction_queue,
            record)
}

has_current_instruction <- \(state) !is.null(state$current_instruction)

inss <- data_to_instructions(td)

initial_state <- function(i, t = 0, r = 1) {
  new_state(t, r, cmd_to_instruction(i[[1]], r, t), i[-1], list())
}

execute_instruction_list <- function(i) {
  state <- initial_state(i)
  states <- list(state)
  while (has_current_instruction(state)) {
    state <- tick(state)
    states <- append(states, list(state))
  }
  states
}


data_to_instructions(test_data) |>
  execute_instruction_list() |>
  map_df(fcomp(as_mapper("record"), as_tibble)) |>
  filter(t %in% c(20, 60, 100, 140, 180, 220)) |>
  summarise(sum(strength))


# PART 2 --------------------------------------------------------------------------------------


data_to_instructions(data) |>
  execute_instruction_list() |>
  map_df(fcomp(as_mapper("record"), as_tibble)) |>
  mutate(row = ceiling(t/40), col = (t - 1) %% 40 ) |>
  mutate(lit = (r >= col-1) & (r <= col + 1)) |>
  filter(lit) |>
  ggplot(aes(x = col, y = -row)) +
  geom_point(color = 'orange') +
  coord_fixed() +
  theme_void()
