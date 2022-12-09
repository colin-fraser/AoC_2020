source("2022/helpers.R")
raw_input <- get_aoc_data(7)

getter <- function(...) \(y) pluck(y, ...)
mapper <- function(f) \(x) map(x, f)
regex_extractor <- function(pattern) \(x) stringr::str_extract(x, pattern)
chr_slicer <- function(start = NULL, end = NULL) \(x) substr(x, start %||% 1, end %||% nchar(x))

# Part 1 --------------------------------------------------------------------------------------

parse_input <- fcomp(splitby("\n(?=\\$)", unlist = TRUE), splitby("\n"))
chunks_to_cmd_out_pairs <- mapper(\(x) list(cmd = fcomp(chr_slicer(3), splitby(' ', TRUE))(x[1]), out = x[-1]))

# cd :: (chr(*), chr(1)) -> chr(*)
# takes a path and an argument to cd and returns the new path
cd <- \(p, arg) switch(arg, '/' = '/', '..' = p[-length(p)], c(p, arg))

# path_update_from_command :: (chr(*), chr(*)) -> chr(*)
path_update_from_command <- \(x, path) switch(x[1], 'cd' = cd(path, x[2]), path)

# accum :: (chr(*), list(ch))
accum <- \(path, pair) path_update_from_command(pair$cmd, path)
path_history_from_pairs <- \(pairs) accumulate(pairs, accum, .init = '/')[-length(pairs)]
append_path_history <- \(pairs) map2(path_history_from_pairs(pairs), pairs, \(x,y) {y$path <- x; y})
chunks_to_triples <- fcomp(chunks_to_cmd_out_pairs, append_path_history)

filter_to_cmd <- \(cmd) \(triples) keep(triples, \(x) x$cmd[1] == cmd)
dir_size <- fcomp(getter("out"), regex_extractor("\\d+"), as.numeric, partial(sum, na.rm=T))

parent_paths <- function(path) map(seq_along(path), \(x) path[1:x])
paste_path <- \(x) paste0(x[1], paste0(x[-1], collapse = '/'))
increment_accumulator <- function(l, x, n) {
  reduce(parent_paths(x), \(l,y){
    y <- paste_path(y)
    if (y %in% names(l)) l[[y]] <- l[[y]] + n
    else l[[y]] <- n
    l
  }, .init = l)
}
increment_accumulator_from_triple <- \(acc, trp) increment_accumulator(acc, trp$path, dir_size(trp))
dir_sizes <- \(triples) reduce(triples, increment_accumulator_from_triple, .init = list())

parse_input(raw_input) |>
  chunks_to_triples() |>
  then(filter_to_cmd("ls")) |>
  dir_sizes() |>
  discard(\(x) x > 100000) |>
  reduce(sum)


# Part 2 --------------------------------------------------------------------------------------

dirs_by_size <- fcomp(parse_input, chunks_to_triples, filter_to_cmd('ls'), dir_sizes)
free_space_from_dir_sizes <- \(ds) 70000000 - ds[["/"]]
space_needed <- \(fs) 30000000 - fs
space_needed_from_ds <- fcomp(free_space_from_dir_sizes, space_needed)
find_dir <- function(input) {
  ds <- dirs_by_size(input)
  space_needed <- space_needed_from_ds(ds)
  dirs_by_size(input) |>
    discard(\(x) x < space_needed) |>
    reduce(min)
}
find_dir(raw_input)
