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

read_data <- function(fileName) {
    r.dat <- read.csv(fileName)
    r.dat.xs <- r.dat[,1:8]
    r.dat.ys <- r.dat[,9]
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

# Function: best_split
#
# Arguments
#   x : Vector containing numerical values
#   y : Vector containing binary class labels
#   minleaf : The minimum number of observations required to consider a split acceptable
#
# The two vectors have the same length greater or equal than 2.
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
best_split <- function (x, y, minleaf = 0){
  best.split = NULL
  best.reduction = 0
  cs <- as.data.frame(candidate_splits(x, y))
  colnames(cs) <- c('split', 'reduction')
  candidates <- unique(cs[order(cs$reduction),])
  
  for (r in seq_len(nrow(candidates))){
    row = cs[r, ]
    if (row$reduction >= best.reduction) {
      nodes <- split(row$split, x, y)
      if (is_good_split(nodes, minleaf)) {
        best.reduction <- row$reduction
        best.split <- row$split
        best.isRight = nodes$isRight
      }
    }
  }
  if (is.null(best.split))
    return(NULL)
  
  return(list("split" = best.split, 
              "reduction" = best.reduction,
              "isRight" = best.isRight))
}

# TODO better name
# Result
#   A list containing the following elements or NULL:
#     col : The index of the column to be splitted.
#     split : The numerical value that seperates the observations.
best_of_best <- function(attrs, ys, min_leaf = 0){
  fbest <- function(a, b) best_split(a, b, min_leaf)
  candidates <- apply(attrs, 2, fbest, ys)
  best <- list(reduction = 0, col = 0) 
  for (i in seq(candidates)){
    c <- candidates[[i]]
    if (is.null(c))
      next
    if (c$reduction >= best$reduction){
      best <- c
      best$col <- i
    }    
  }
  if (best$col == 0)
    return(NULL)
  return(best)
}
