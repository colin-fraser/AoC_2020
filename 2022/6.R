source("2022/helpers.R")
raw_data <- get_aoc_data(6)

no_duplicates <- \(x) anyDuplicated(x) == 0

# (numeric(1), character(*)) -> numeric(1) -> character(n)
n_extractor <- \(n, x) \(i) x[i:(i+n-1)]

# (numeric(1), character(*)) -> (numeric(1), character(*)) -> lgl(1)
checker <- \(n, x) fcomp(n_extractor(n, x), no_duplicates)

data <- strsplit(raw_data, "")[[1]]

find_signal <- \(n, x) n - 1 + detect_index(seq_along(x), checker(n, x))
find_signal(4, data)
find_signal(14, data)
