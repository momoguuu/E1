# E1
# Solve these exercises by filling in the function bodies, and try to read up on
# things that you didn't know or surprise you.

# write a function that multiplies x by y
# x and y are both single numbers.
ex01Multiply <- function(x, y) {
  # your code
  x * y
}

# write a function that multiplies x by y
# x and y are numeric vectors. Does this function need to
# differ from the one above?
ex02MultiplyVectors <- function(x, y) {
  # your code
  x * y
}

# write a function that checks if a "logical", a "numeric", or a "character" vector or something else.
# the function should return "lg" if the input is logical, "nm" if it is numeric, "ch" for characters,
# and otherwise "x".
ex03VectorType <- function(x) {
  # your code
  switch(mode(x), numeric = "nm", logical = "lg", character = "ch", "x")
}

# write a function that takes a vector of integers and returns all numbers that are odd.
# an empty vector should be returned when no odd numbers are found.
ex04Odd <- function(x) {
  # your code
  x[x %% 2 == 1]
}

# E 2
 This exercise is concerned with vectors and basic operations on them.


# Suppose you have a vector "numbers" of ages of people, and you want to know how many of them are "Youth" (younger
# than 18 years), "Young Adult" (at least 18, less than 36), "Adult" (at least 36, less than 56) or "Senior" (56 and
# up). Or another problem: you have a vector of people's BMI and you want to know how many are "Severely underweight"
# (BMI < 16), "Underweight" (BMI between 16 and 18.5), "Healthy" (18.5 to 25) or "Overweight" (BMI > 25).
#
# For these problems, you get as input a vector of numbers, a vector of bin cutoffs, and a vector of bin names.
# The bin cutoffs indicate the upper bounds of each bin (the upper bound of the last bin is `Inf` (infinity) and not
# provided). Your task is to count the numbers in each bin and return them in a *named* vector, in order of increasing
# cutoff value.
#
# The inputs for the first problem described above could be
# > numbers = c(10.3, 32.7, 50.5, 62.4, 32.0, 50.4, 19.7, 60.1, 69.0, 50.6, 11.1, 48.6, 17.4, 34.3, 78.7)
# > cutoffs = c(18, 36, 56)
# > binnames = c("Youth", "Young Adult", "Adult", "Senior")
# and the expected result would be
# > c(Youth = 3, "Young Adult" = 4, Adult = 4, Senior = 4)
# The second problem could have input
# > numbers = c(20, 23, 28)
# > cutoffs = c(16, 18.5, 25)
# > binnames = c("Severely underweight", "Underweight", "Healthy", "Overweight")
# and the result should be
# > c("Severely underweight" = 0, Underweight = 0, Healthy = 2, Overweight = 1)
#
# You may assume that the cutoff values are already sorted.
# Cutoff bounds are exclusive, so a data point of 18.0 in the first example is counted as "Young Adult".
#
# There are different ways to solve this; among others you could solve this task by itself, you could also solve the
# following exercise (ex02Binning) first and call it here using the `table()` method, or you could
# try if the `hist()` function is useful. (If you use `table()`, you will have to wrap the result in `c()` to get
# a named vector from the table: `c(table(...))`).
ex01BinCounting <- function(numbers, cutoffs, binnames) {
  # your code
  cumulative <- vapply(c(cutoffs, Inf), function(x) sum(numbers < x), 0)
  result <- diff(c(0, cumulative))
  names(result) <- binnames
  result
}

# Related to the last exercise: Now you should not only *count* the number of data points in each bin, you
# should return, for each data point, the bin that it belongs to, as an *ordered factor* variable. This is called data
# binning. For the first input, this would be
# > numbers = c(10.3, 32.7, 50.5, 62.4, 32.0, 50.4, 19.7, 60.1, 69.0, 50.6, 11.1, 48.6, 17.4, 34.3, 78.7)
# > cutoffs = c(18, 36, 56)
# > binnames = c("Youth", "Young Adult", "Adult", "Senior")
# return:
# > ordered(c("Youth", "Young Adult", "Adult", "Senior", "Young Adult", "Adult",  "Young Adult", "Senior",
#             "Senior", "Adult", "Youth", "Adult",  "Youth", "Young Adult", "Senior"),
#           levels = c("Youth", "Young Adult", "Adult", "Senior"))
# The second problem would be
# > numbers = c(20, 23, 28)
# > cutoffs = c(16, 18.5, 25)
# > binnames = c("Severely underweight", "Underweight", "Healthy", "Overweight")
# and the result should be
# > ordered(c("Healthy", "Healthy", "Overweight"),
#   levels = c("Severely underweight", "Underweight", "Healthy", "Overweight"))
#
# The `cut()` function could be helpful here.
ex02Binning <- function(numbers, cutoffs, binnames) {
  # your code
  cut(numbers, breaks = c(-Inf, cutoffs, Inf), labels = binnames, right = FALSE, ordered_result = TRUE)
}


# "Fizz Buzz" is a game for children where players take turns saying numbers
# counting up, but say "Fizz" instead if a number is divisible by 3 and "Buzz"
# if a number is divisible by 5, saying "Fizz Buzz" if both is the case.
# Read about it at Wikipdia: <https://en.wikipedia.org/wiki/Fizz_buzz>.
#
# Write a function that plays this game with itself. The function should
# return a `character` vector containing the numbers from 1 up to `up.to`, or
# "Fizz", "Buzz" or "Fizz Buzz" at the appropriate places. The numbers at which
# "Fizz" and "Buzz" should be returned are optional arguments.
#
# Example:
# up.to = 10, fizz.number = 3, buzz.number = 5
# --> returns c("1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz")
# up.to = 6, fizz.number = 2, buzz.number = 4
# --> returns c("1", "Fizz", "3", "Fizz Buzz", "5", "Fizz")
#
# You can rely on fizz.number, buzz.number and up.to being integer values greater
# than 0, though not necessarily greater than 1 and not necessarily different from
# each other.
#
# Although you are allowed to use loops, it is probably simpler to solve this
# without loops.
ex03FizzBuzz <- function(up.to, fizz.number = 3, buzz.number = 5) {
  # your code
  sequence <- seq_len(up.to)
  ret <- as.character(sequence)
  is.fizz <- sequence %% fizz.number == 0
  is.buzz <- sequence %% buzz.number == 0
  ret[is.fizz] <- "Fizz"
  ret[is.buzz] <- "Buzz"
  ret[is.fizz & is.buzz] <- "Fizz Buzz"
  ret
}


# E 3
# This exercise is concerned with basic operations on matrices and data frames.
# We are using only methods contained in the standard R installation; don't use
# tidy here.

# Imagine a square matrix as a chess board, where the top left field is white, the
# second row left field as well as the first row, second column field are black, etc.
#
# Given a square matrix as input, return the sum of the values of all elements that
# would be on black fields:
#
# 1 2
# 3 4  --> 2 and 3 are on 'black' fields --> result 5
#
# 1 2 3
# 4 5 6 --> 2 + 4 + 6 + 8 --> 20
# 7 8 9
#
# 1 0 0 0
# 0 1 0 0
# 0 0 1 0 --> 0
# 0 0 0 1
#
# Make sure this works with the trivial matrix with only one element!
# The `row()` and `column()` function are useful here.
ex01ChessboardSum <- function(mat) {
  # your code
  sum(mat[(row(mat) + col(mat)) %% 2 == 1])
}

# Given a matrix as input, return the coordinates (row and column) of the minimum element,
# as a two element numeric vector. If multiple elements are tied for minimum, you
# can return the coordinates of any of them.
#
# E.g.
#
# 1 2 3
# 4 5 6 --> c(1, 1)
# 7 8 9
#
#  0  0
#  0  0 --> c(3, 1)
# -3  0
#
# 1 0 --> both c(2, 1) and c(1, 2) would be correct
# 0 1
ex02MatrixWhichMin <- function(mat) {
  # your code
  wm <- which.min(mat)
  as.numeric(c(row(mat)[wm], col(mat)[wm]))
}

# Your function is given a `data.frame` with columns that can each be of any of numeric, logical
# or factor type. You are supposed to return a `data.frame` containing only columns selected
# by type. For this you are given the "type" argument: a character vector containing a single
# argument containing one of c("numeric", "logical", "character").
#
# E.g. * data = iris (the "iris" dataset as contained in R)
#        type = "numeric"
#        --> return
#                     Sepal.Length Sepal.Width Petal.Length Petal.Width
#                   1          5.1         3.5          1.4         0.2
#                   2          4.9         3.0          1.4         0.2
#                   3          4.7         3.2          1.3         0.2
#                   .........
#      * data = iris
#        type = "logical"
#        --> return a data frame with 0 columns and 150 rows
#      * data = data.frame(a = c(1, 2, 3), b = c("alpha", "beta", "gamma"))
#        type = "character"
#        --> return
#                        b
#                  1 alpha
#                  2  beta
#                  3 gamma
#    ... etc.
# The order of all included columns should not be changed with respect to their order in the input.
# Be aware that there are could be other columns that are not one of numeric, logical, or character,
# in particular "factor" columns; they should always be discarded.
ex03SelectDF <- function(data, type) {
  # your code
  sel.col <- vapply(data, function(col) {
    type %in% class(col)
  }, TRUE)
  data[sel.col]
}


# You are given a `data.frame` with some data about study participants. Its columns
# are numeric, some of which have missing values (NA). Your task is to *impute* the missing values,
# i.e. set them to putative numeric values inferred from the other participants. In particular,
# you are to set the missing values in each variable to the *mean value* of all the *non-missing values*
# in that variable.
#
# E.g. for the input
# > data.frame(
#     height = c(178, 185, NA, 157, NA, 174), weight = c(95, 90, 99, 70, 77, NA),
#     age = c(23, NA, NA, NA, 21, 22))
# the returned value should be
# > data.frame(
#     height = c(178, 185, 173.5, 157, 173.5, 174), weight = c(95, 90, 99, 70, 77, 86.2),
#     age = c(23, 22, 22, 22, 21, 22))
#
# The return value should be a data.frame with the same columns as the input and with the missing values
# imputed. You can assume that there is at least one value present in each variable.
# Your input may have different column names.
ex04Imputation <- function(data) {
  # your code
  for (cn in colnames(data)) {
    col <- data[[cn]]
    data[[cn]][is.na(col)] <- mean(col, na.rm = TRUE)
  }
  data
}

# E 4
# This exercise is concerned with control structures: conditional execution and loops.
# (we will skip functions for now).

# Consider the following `data.frame` that records "opposites" of concepts:
#
#        concept           opposite
#      1   black              white
#      2    slow               fast
#      3     big              small
#      4     man              woman
#      5    many                few
#      6     man              woman
#      7    fast               slow
#
# indicating that "many" is the opposite of  "few" etc.
#
# Write a function that takes a database of this form, as well as a concept, as input and returns
# the opposite of the given concept. Note, however, that the opposite of an opposite is the concept
# itself, so with the database above, your function should report "white" as the opposite of "black",
# but also "black" as the opposite of "white".
#
# The function should now take two inputs, `database` and `concept` and return a `character(1)` string.
#
# Example inputs using the the knowledge database from above:
# > database = data.frame(concept = c("black", "slow", "big", "man", "many", "man", "fast"),
#     opposite = c("white", "fast", "small", "woman", "few", "woman", "slow"), stringsAsFactors = FALSE)
# The function should give the following outputs, as examples:
# > ex01Opposite(database, "black")
# --> "white"
# > ex01Opposite(database, "slow")
# --> "fast"
# > ex01Opposite(database, "small")
# --> "big"
# > ex01Opposite(database, "man")
# --> "woman"
# > ex01Opposite(database, "woman")
# --> "man"
#
# You may assume that the `concept` input can always be found in at least one of the
# two columns of `database`. Some of the concepts and their opposites may occur more than once.
# However, there are never any contradictions: if a concept occurs more than once, its opposite
# is the same, and if an entry of the `opposite` column is also in the `concept` column, then the
# corresponding `concept` entry is in the `opposite` column (see the "fast"/"slow" example above).
ex01Opposite <- function(database, concept) {
  # your code

  # the solution checks if the "concept" item is in the `concept` column
  # of the database first, and if not, defaults to the `opposite` column.
  if (concept %in% database$concept) {
    position <- which(database$concept == concept)[[1]]
    database$opposite[[position]]
  } else {
    position <- which(database$opposite == concept)[[1]]
    database$concept[[position]]
  }
}

# A "cellular automaton" is a discrete model of state evolution. We consider a state
# of a vector with "cells" of values 0 or 1, for example the vector c(0, 1, 0, 1, 0, 0, 0).
# The state now changes according to a special rule: Every cell changes to state 1
# whenever its immediate left neighbour has state 1. A cell that has state 1 remains at state 1.
# Each cell that has no left neighbour or a left neighbour that is 0, and that is also 0 itself, remains 0 otherwise.
# The evolution of the vector above would be
# > c(0, 1, 0, 1, 0, 0, 0)
# > c(0, 1, 1, 1, 1, 0, 0)
# > c(0, 1, 1, 1, 1, 1, 0)
# > c(0, 1, 1, 1, 1, 1, 1)
#
# Write a function that takes as input a vector of 0s and 1s and "evolves" the state until no
# more changes occur. The return value should be a
# list of states, with the first entry being the input vector.
# The upper example would look like this:
# > initial.state = c(0, 1, 0, 1, 0, 0, 0)
# --> returns
# list(c(0, 1, 0, 1, 0, 0, 0),
#      c(0, 1, 1, 1, 1, 0, 0),
#      c(0, 1, 1, 1, 1, 1, 0),
#      c(0, 1, 1, 1, 1, 1, 1))
#
# Other examples:
#
# > initial.state = c(0, 1, 0, 0, 0, 1)
# --> returns
# list(c(0, 1, 0, 0, 0, 1),
#      c(0, 1, 1, 0, 0, 1),
#      c(0, 1, 1, 1, 0, 1),
#      c(0, 1, 1, 1, 1, 1))
#
# > initial.state = c(0, 0, 0, 0, 0)
# --> returns the value as of
# list(c(0, 0, 0, 0, 0))
# (i.e. a single entry, since this is already a state that will not change.)
#
ex02CellularAutomaton <- function(initial.state) {
  # your code here
  results <- list(initial.state)
  state <- initial.state
  repeat {
    # shift the state to the right by 1
    state.shifted <- c(0, head(state, -1))
    # every state where state or state.shifted is 1 should be 1, others 0
    state.next <- ifelse(state == 1 | state.shifted == 1, 1, 0)
    # if nothing changed, we are done
    if (all(state.next == state)) break
    results[[length(results) + 1]] <- state <- state.next
  }
  results
}
