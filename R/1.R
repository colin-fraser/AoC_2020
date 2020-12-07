library(tidyverse)

session_cookie <- rstudioapi::askForSecret('aoc_session')
data <- httr::GET("https://adventofcode.com/2020/day/1/input",
                  set_cookies(session = session_cookie)) %>%
  content() %>%
  read_lines() %>%
  as.numeric()

crossing(c1 = data, c2 = data) %>%
  filter(c1 <= c2) %>%
  filter(c1 + c2 == 2000) %>%
  summarise(solution = c1 * c2)
