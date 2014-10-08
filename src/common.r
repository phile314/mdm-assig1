# This script contains utility functions common to all the other scripts.

# Function: split(s, x, y)
# Splits the vectors x and y according to the value of s.
#
# Arguments
#   s : A number representing the threshold value to be used to apply the split
#   x : A vector containing the values for some binary or numerical attribute
#   y : A vector containing the respecitve binary class labels for the observations contained in x
#
# x and y have the same length.
#
# Result:
# A list containing the following named fields : left, right, isRight.
# left and right are also lists containing two named fields (x and y).
# right$x and right$y are numerical vectors containing the portion of the input
# vectors x and y for which the x-values are greater than s.
# right$x and right$y contain the remaining values from x and y.
# isRight is a logical vector resulting from x > s
split <- function(s, x, y){
  isRight = x > s
  l = list(x = x[! isRight], y = y[! isRight])
  r = list(x = x[isRight]  , y = y[isRight])
  return(list(left = l, right = r, isRight = isRight))
}

# Function: partition(isRight, x, y)
# Partition the attributes matrix x and the class labels y according to the
# logical vector isRight.
#
# Arguments:
#   isRight : A logical vector
#   x       : A numerical matrix
#   y       : A numerical (binary) vector
#
# The number of rows of x and the length of isRight and y are the same.
#
# Result
# A list containing the following named fields:
#   left.x : left rows of x
#   right.x : right rows of x
#   left.y : left rows of y
#   right.y : right rows of y
# Where left or right is determined by the logical value of the correspondent
# element of isRight.
partition <- function(isRight, x, y) {
  rx = x[isRight, , drop = FALSE]
  lx = x[! isRight, , drop = FALSE]
  ry = y[isRight]
  ly = y[! isRight]
  return(list(left.x = lx, right.x = rx, left.y = ly, right.y = ry))
}

# Function: reduction(s, x, y, i)
# Arguments
#   s : A number representing a treshold value for a split
#   x : A numerical vector containing values for a binary/numerical attribute
#   y : A numerical (binary) vector containing the class labels related to x
#   i : The impurity function to be used (default = gini_index)
#
# The vectors x and y have the same length.
#
# Result: A number representing the reduction obtained applying the split s.
reduction <- function (s, x, y, i = gini_index){
  nodes = split(s, x, y)
  l = nodes$left$y
  r = nodes$right$y
  pl = length(l) / length(x)
  pr = 1 - pl
  return((i(l) * pl) + (i(r) * pr))
}

# Function: impurity_reduction(s, x, y, i)
#
# Arguments:
#   s : A number representing the split on the numerical attributes
#   x : A vector containing the numerical attributes
#   y : A vector containing the binary class labels
#   i : The impurity function used (default = gini_index)
#
# The vectors x and y have the same length.
#
# Result
# A number representing the impurity reduction obtained using the
# split s on x and y and the impurity function x.
#
impurity_reduction <- function (s, x, y, i = gini_index)
  return(i(y) - reduction(s, x, y, i))

# Function: read.data(fileName, test, header)
# Reads a dataset and split trainining and testing data
#
# Arguments:
#   fileName : The csv file to load. Last column is assumed to be class label.
#   test     : A number in [0,1] that represents the percent of rows to use for testing
#   header   : A logic value: does the file has an header?
#
# Result
#   A list of
#     data : a data frame that contains the whole dataset
#     train.x :  attribute values as matrix (training dataset)
#     train.y :  vector of class labels (training dataset)
#     test.x  :  attribute values as matrix, testing dataset)
#     test.y  :  vector of class labels (testing dataset)
read.data <- function(fileName, test, header = FALSE) {
    r.data <- read.csv(fileName, header)

    isTest <- sample(nrow(r.data), test * nrow(r.data))
    r.test <- r.data[isTest, ]
    r.train <- if (is.integer(isTest)) r.data else r.data[-isTest, ]

    nc <- dim(r.data)[2]
    r.train.x <- r.train[, 1:(nc - 1), drop = FALSE]
    r.train.y <- r.train[,nc]
    r.test.x <- r.test[, 1:(nc - 1), drop = FALSE]
    r.test.y <- r.test[, nc]
    return(list(data = r.data, train.x = r.train.x, train.y = r.train.y,
                 test.x = r.test.x, test.y = r.test.y))
}

# Function: gini_index(y)
# The gini index impurity function for the two-class case
#
# Arguments:
#   y : A binary (numerical) vector, with class labels 0 or 1.
#
# Result: The value of the gini index impurity function for the given class label vector
gini_index <- function(y) {
    n1 <- sum(y)
    n <- NROW(y)
    return((n1 / n) * (1 - (n1 / n)))
}

# majority_class(y)
# Computes the majority vote for the given vector of class labels.
#
# Arguments
#   y : A binary (numerical) vector, with class labels 0 or 1
#
# Result
#   The class label 0 or 1 which is more frequent in the input vector.
#   Ties are broken at random.
majority_class <- function(y) {
    n1 <- sum(y)
    n <- NROW(y)
    if (n1 * 2 > n)
      return(1)
    if (n1 * 2 < n)
      return(0)
    return (sample(0:1, 1))
}

# Function: candidate_splits(x, y, impurity)
# Computes the possible splits for the attribute vector x, with class label y and
# their impurity reduction.
#
# Arguments:
#   x : A numerical vector containing values of a numerical/binary attribute
#   y : A binary (numerical) vector containing the corresponding class labels for x
#   impurity : The impurity function to be used when computing the impurity reduction.
#
# Result:
#   A 2-columns matrix containing all the possible splits on x and their impurity reduction.
candidate_splits <- function(x, y, impurity = gini_index){
  xy <- data.frame(x,y)
  sorted <- xy[order(xy$x), ]
  x <- sorted$x
  y <- sorted$y
  x.distinct <- unique(x)
  splits <- lapply(seq_len(length(x.distinct) - 1),
                   function(i) mean(x.distinct[i : (i + 1)]))

  with.impurity <- function(s) c(s, impurity_reduction(s, x, y, impurity))
  candidates <- t(vapply(splits, with.impurity , c(1,2)))
  return(candidates)
}

# Function: is_good_split(nodes, minleaf)
# Determine whether a split meet the minleaf constraint.
#
# Arguments:
#   nodes : A list obtained by the split function
#   minleaf : An integer number
#
# Result: TRUE if both the leaves contained in nodes contain minleaf observations,
#         FALSE otherwise.
is_good_split <- function (nodes, minleaf) {
  length(nodes$left$x) >= minleaf && length(nodes$right$x) >= minleaf
}

# Function: best.split(s1, s2)
# Returns the best of the two splits given.
#
# Arguments
#   s1 s2 : A split object representing a split
#           The comparison is done on the reduction field.
best.split <-function(s1, s2){
  if (!"reduction" %in% names(s1))
    return(s2)
  if(!"reduction" %in% names(s2))
    return(s1)
  best <- if (s1$reduction >= s2$reduction) s1 else s2
  return(best)
}

# Function: best.split.among(splits)
#
# Arguments:
#   splits: A list of split objects.
# Output
#   The split in the list with greater reduction, or NULL
#   if the list is empty.
#   The returned split is extended with a new field index containing its
#   position in the list.
best.split.among <- function(splits){
  best <- NULL
  for (i in seq(splits)){
    splits[[i]]$index <- i
    best <- best.split(best, splits[[i]])
  }
  return(best)
}

# Function: best.split.on(x, y, minleaf, impurity)
# Computes the best possible split on the attribute vector x.
#
# Arguments
#   x : Vector containing numerical/binary values
#   y : Vector containing binary class labels
#   minleaf : The minimum number of observations required to consider a split acceptable (defaut = 0)
#   impurity : The impurity function that will be used for computing the impurity reduction (default = gini_index)
#
# The two vectors have the same length.
#
# Result
#   A split object representing the best possible split on x
#   NULL if no possible split satisfy the minleaf constraint.
best.split.on <- function (x, y, minleaf = 0, impurity = gini_index){
  cs <- as.data.frame(candidate_splits(x, y, impurity))
  colnames(cs) <- c('split', 'reduction')
  candidates <- unique(cs[order(cs$reduction),])
  splits <- list()
  for (r in seq_len(nrow(candidates))){
    current <- as.list(cs[r, ])
    nodes <- split(current$split, x, y)
    if (is_good_split(nodes, minleaf))
      splits[[r]] <- c(current, isRight = list(nodes$isRight))
  }
  return(best.split.among(splits))
}

# Result
#   A list containing the following elements or NULL:
#     col : The index of the column to be splitted.
#     split : The numerical value that seperates the observations.
best.split.of.all <- function(attrs, ys, minleaf = 0, impurity = gini_index){
  fbest <- function(a, b) best.split.on(a, b, minleaf, impurity)
  candidates <- apply(attrs, 2, fbest, ys)
  return(best.split.among(candidates))
}
