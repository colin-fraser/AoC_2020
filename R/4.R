source("R/helpers.R")
library(stringr)
library(readr)
library(tidyr)
library(dplyr)

idata <- aoc_get_response(4) %>%
  content(encoding = 'UTF-8') %>%
  str_split("\n\n") %>%
  .[[1]] %>%
  str_replace_all('\n', " ")

# Problem 1 -----------------------------------------------------------------------------------

sum(str_count(idata, "(byr|iyr|eyr|hgt|hcl|ecl|pid):")==7)

# Problem 2 -----------------------------------------------------------------------------------

ppts <- idata %>%
  imap( ~ .x %>%
    str_replace_all(" ", "\n") %>%
    read_csv(col_names = "raw") %>%
    separate(raw, into = c("key", "value"), sep = ":") %>%
      pivot_wider(names_from = key, values_from = value) %>%
      mutate(id = .y)
    ) %>%
  bind_rows

check_byr <- function(x) !is.na(x) & (str_length(x) == 4) & (x >= '1920') & (x <= '2002')
check_iyr <- function(x) !is.na(x) & (str_length(x) == 4) & (x >= '2010') & (x <= '2020')
check_eyr <- function(x) !is.na(x) & (str_length(x) == 4) & (x >= '2020') & (x <= '2030')
check_hgt <- function(x) {
  case_when(
    is.na(x) ~ FALSE,
    str_detect(x, '\\dcm') ~ '150cm' <= x & x <= '193cm',
    str_detect(x, '\\din') ~ '59in' <= x & x <= '76in',
    TRUE ~ FALSE
  )
}
check_hcl <- function(x) !is.na(x) & str_detect(x, '^#[[0-9][a-f]]{6}$')
check_ecl <- function(x) !is.na(x) & str_detect(x, "^(amb|blu|brn|gry|grn|hzl|oth)$")
check_pid <- function(x) !is.na(x) & str_detect(x, "^\\d{9}$")
ppts %>%
  filter(check_byr(byr),
         check_iyr(iyr),
         check_eyr(eyr),
         check_hgt(hgt),
         check_hcl(hcl),
         check_ecl(ecl),
         check_pid(pid)
  ) %>%
  nrow
