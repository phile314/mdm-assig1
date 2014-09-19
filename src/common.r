split <- function(s, x, y){
  isRight = x > s
  l = list(x = x[! isRight], y = y[! isRight])
  r = list(x = x[isRight]  , y = y[isRight])
  return(list(left = l, right = r, isRight = isRight))
}

# Result
# A list containing the following named fields:
#   xsl : left rows of attrs
#   xsr : right rows of attrs
#   ysl : left rows of ys
#   ysr : right rows of ys
partition <- function(isRight, xs, ys) {
  xsr = xs[isRight, , drop = FALSE]
  xsl = xs[! isRight, , drop = FALSE]
  ysr = ys[isRight]
  ysl = ys[! isRight]
  return(list("xsl" = xsl, "xsr" = xsr, "ysl"=ysl, "ysr"=ysr))
}

reduction <- function (s, x, y, i = gini_index){
  nodes = split(s, x, y)
  l = nodes$left$y
  r = nodes$right$y
  pl = length(l) / length(x)
  pr = 1 - pl
  return((i(l) * pl) + (i(r) * pr))
}

# Function: impurity_reduction
#
# Arguments:
#   s : A number representing the split on the numerical attributes
#   x : A vector containing the numerical attributes
#   y : A vector containing the binary class labels
#   i : The impurity function used
#   
# The vectors x and y have the same length.
#
# Result
# A number representing the impurity reduction obtained using the
# split s on x and y and the impurity function x.
#
impurity_reduction <- function (s, x, y, i = gini_index)
  return(i(y) - reduction(s, x, y, i))

# Function: read_data
#
# Arguments:
#   fileName : The csv file to load. Last column is assumed to be class label.
#
# Result
#   A list of
#     dat : full dataset
#     xs :  attribute values as matrix
#     ys :  vector of class labels
read_data <- function(fileName) {
    r.dat <- read.csv(fileName)
    nc <- dim(r.dat)[2]
    r.dat.xs <- r.dat[,1:(nc - 1)]
    r.dat.ys <- r.dat[,nc]
    return(list(dat=r.dat,xs=r.dat.xs,ys=r.dat.ys))
}

gini_index <- function(y) {
    n1 <- sum(y)
    n <- NROW(y)
    return((n1 / n) * (1 - (n1 / n)))
}

majority_class <- function(y) {
    n1 <- sum(y)
    n <- NROW(y)
    if (n1 * 2 > n)
      return(1)
    if (n1 * 2 < n)
      return(0)
    return (sample(0:1, 1))
}

candidate_splits <- function(x, y){
  xy <- data.frame(x,y)
  sorted <- xy[order(xy$x), ]
  x <- sorted$x
  y <- sorted$y
  x.distinct <- unique(x)
  splits <- lapply(seq_len(length(x.distinct) - 1),
                   function(i) mean(x.distinct[i : (i + 1)]))
  candidates <- t(vapply(splits, function(s) c(s, impurity_reduction(s, x, y)), c(1,2)))
  return(candidates)
}

is_good_split <- function (nodes, minleaf) {
  length(nodes$left$x) >= minleaf && length(nodes$right$x) >= minleaf
}

best.split <-function(s1, s2){
  if (!"reduction" %in% names(s1))
    return(s2)
  if(!"reduction" %in% names(s2))
    return(s1)
  best <- if (s1$reduction >= s2$reduction) s1 else s2
  return(best)
}

# Input a list of split objects.
# Output 
#   the split in the list with greater reduction, or NULL
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

# TODO pass around implicity arguments

# Function: best_split
#
# Arguments
#   x : Vector containing numerical values
#   y : Vector containing binary class labels
#   minleaf : The minimum number of observations required to consider a split acceptable
#
# The two vectors have the same length.
# This version adopts the brute version approach.
#
# Result
#   A list containing three elements:
#     reduction : The impurity reduction produced by the returned split
#     split : The numerical value that separates the observations
#     isRight : A boolean vector, indicating for each row if
#               it belongs to the right subtree or not.
#   NULL if no possible split satisfy the minleaf constraint.
#
best.split.on <- function (x, y, minleaf = 0){
  cs <- as.data.frame(candidate_splits(x, y))
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
best.split.of.all <- function(attrs, ys, min_leaf = 0){
  fbest <- function(a, b) best.split.on(a, b, min_leaf)
  candidates <- apply(attrs, 2, fbest, ys)
  return(best.split.among(candidates))
}
