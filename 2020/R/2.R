library(httr)
library(tidyverse)


# get data ----------------------------------------------------------------

session_cookie <- rstudioapi::askForSecret("aoc_session")
data <- httr::GET(
  "https://adventofcode.com/2020/day/2/input",
  set_cookies(session = session_cookie)
) %>%
  content() %>%
  read_lines()

pw_df <- tibble(raw = data) %>%
  mutate(
    left = as.numeric(str_extract(raw, "\\d+")),
    right = as.numeric(str_extract(raw, "(?<=\\-)\\d+")),
    letter = str_extract(raw, "[a-z]"),
    password = str_extract(raw, "(?<=: ).+")
  )

# Problem 1 --------------------------------------------------------------

pw_df %>%
  mutate(letter_count = str_count(password, letter)) %>%
  summarise(solution = sum(letter_count >= left & letter_count <= right))

# Problem 2 ---------------------------------------------------------------

pw_df %>%
  mutate(
    password_substring =
      paste0(str_sub(password, left, left), str_sub(password, right, right))
  ) %>%
  summarise(solution = sum(str_count(password_substring, letter) == 1))
