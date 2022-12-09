source("2022/helpers.R")


# Part 1 --------------------------------------------------------------------------------------

raw_input <- get_aoc_data(1)
splitby <- function(by, unlist = FALSE) {
  function(x) {
    out <- strsplit(x, by)
    if (unlist) {
      out <- unlist(out)
    }
    out
  }
}
cal_lists <- fcomp(splitby('\n\n'), unlist, splitby('\n'))
process_list_item <- fcomp(as.numeric, sum)

raw_input |>
  cal_lists() |>
  map_dbl(process_list_item) |>
  then(fcomp(\(x) sort(x, decreasing = TRUE), slice(1)))


# Part 2 --------------------------------------------------------------------------------------


raw_input |>
  cal_lists() |>
  map_dbl(process_list_item) |>
  then(fcomp(\(x) sort(x, decreasing = TRUE), slice(1,3))) |>
  sum()
