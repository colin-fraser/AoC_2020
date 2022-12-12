source("2022/helpers.R")

mapper <- \(f) \(x) map(x, f)
row_apply <- \(f) \(m) t(apply(m, 1, f))
then <- \(x, f) f(x)
fcomp <- \(...) compose(..., .dir = "forward")
# Part 1 --------------------------------------------------------------------------------------

# parse the raw input into a matrix
input_to_matrix <- fcomp(
  splitby("\n", T), splitby(""), mapper(as.numeric),
  \(x) matrix(unlist(x), nrow = length(x), byrow = TRUE)
)

# the plan: build a function that computes all the left visibilities, and simply
# apply it to m, m', flip(m), and (flip(m))'
left_visibility_mat <- row_apply(\(x) x > lag(cummax(x), 1, default = -1))

# flips
flp <- \(x) x[, ncol(x):1]

# returns a function that returns n copies of x in a list
copy <- \(n) \(x) lapply(1:n, \(z) x)

# given a list of n functions, return a function that takes a list of length n
# and returns list(f1(l[[1]]), ..., fn(l[[n]]))
applicator <- \(funcs) \(l) map2(l, funcs, \(x, f) f(x))

copy_apply <- \(f) fcomp(
  copy(4),
  applicator(list(identity, flp, t, fcomp(t, flp))), # transforms
  mapper(f),
  applicator(list(identity, flp, t, fcomp(flp, t))) # inverses
)
map_reduce_agg <- \(mfunc, rfunc, afunc) fcomp(
  copy_apply(mfunc),
  \(x) reduce(x, rfunc),
  afunc
)

solution1 <- fcomp(input_to_matrix, map_reduce_agg(left_visibility_mat, \(x, y) x | y, sum))

solution1(get_aoc_data(8))


# Part 2 --------------------------------------------------------------------------------------
scenic_right <- \(x) {
  if (length(x) == 1) 0
  else if (all(x[-1] < x[1])) length(x) - 1
  else  which.max(x[-1] >= x[1])
}
scenic_mat <- row_apply(\(v) sapply(seq_along(v), \(x) scenic_right(v[x:length(v)])))
solution2 <- fcomp(input_to_matrix, map_reduce_agg(scenic_mat, \(x, y) x * y, max))

solution2(get_aoc_data(8))
