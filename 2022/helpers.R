library(purrr)
library(rlang)

# advent_of_code_input :: integer -> character
advent_of_code_input <- function(x) {
  paste0("https://adventofcode.com/2022/day/", x, "/input")
}

as_list_arg_fn <- function(.f) \(x) do.call(.f, x)

# get_cookie :: character -> character
get_session_cookie <- memoise::memoise(function(x = "advent of code") rstudioapi::askForSecret(x))

# set_session_cookie :: character -> request
set_session_cookie <- function(x) httr::set_cookies(session = x)

session_cookie <- compose(set_session_cookie, get_session_cookie)

# make_get_args :: (integer, character) -> list(character, character)
make_get_args <- function(i, x = "advent of code") {
  list(url = advent_of_code_input(i), config = session_cookie(x))
}

# get_aoc_data :: integer -> character
get_aoc_data <- compose(
  partial(httr::content, encoding = "UTF-8"),
  as_list_arg_fn(httr::GET),
  make_get_args
  )

fcomp <- partial(compose, .dir = 'forward')
fmap <- function(.f, ...) \(.x) map(.x, .f, ...)
freduce <- function(.f, ...) \(.x) reduce(.x, .f, ...)

# enables putting a function on the right side of a pipe,
# e.g. c(2, 3) |> then(\(x) sum(x))
then <- function(.x, .f, ...) exec(.f, !!!list2(.x, ...))

# create a string splitting function that splits by `by`
splitby <- function(by, unlist = FALSE) {
  function(x) {
    out <- strsplit(x, by, perl = TRUE)
    if (unlist) {
      unlist(out)
    } else {
      out
    }
  }
}
