library(glue)
library(httr)
library(magrittr)

aoc_build_url <- function(day, year = 2020) {
  formatted_url <- glue("adventofcode.com/{year}/day/{day}/input")
  return(formatted_url)
}

.aoc_get_response <- function(day,
                              session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie"),
                              year = 2020) {
  aoc_url <- aoc_build_url(day, year)
  cookie <- set_cookies(session = session_cookie)
  response <- GET(aoc_url, cookie)
  return(response)
}

aoc_get_response <- memoise::memoise(.aoc_get_response)

aoc_get_data <- function(day, session_cookie = keyring::key_get("RStudio Keyring Secrets", "Advent of Code Session Cookie"),
                         year = 2020) {
  aoc_get_response(day, session_cookie, year) %>%
    content(encoding = 'UTF-8') %>%
    readr::read_lines()
}
