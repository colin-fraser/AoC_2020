library(tidyverse)
library(httr)

# get data ----------------------------------------------------------------

session_cookie <- rstudioapi::askForSecret('aoc_session')
data <- httr::GET("https://adventofcode.com/2020/day/1/input",
                  set_cookies(session = session_cookie)) %>%
  content() %>%
  read_lines() %>%
  as.numeric()

# Part 1 ------------------------------------------------------------------

crossing(c1 = data, c2 = data) %>%
  filter(c1 <= c2) %>%
  filter(c1 + c2 == 2020) %>%
  summarise(solution = c1 * c2)


# Part 2 ------------------------------------------------------------------

crossing(c1 = data, c2 = data, c3 = data) %>%
  filter(c1 <= c2, c2 <= c3) %>%
  filter(c1 + c2 + c3 == 2020) %>%
  summarise(solution = c1 * c2 * c3)

