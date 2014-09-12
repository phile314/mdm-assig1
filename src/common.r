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
impurity_reduction <- function (s, x, y, i = gini_index) {
  l = y[x <= s]
  r = y[x > s]
  pl = length(l) / length(x)
  pr = 1 - pl
  delta = i(y) - (i(l) * pl) - (i(r) * pr) 
  return (delta)
}

gini_index <- function(ys) {
    n1 <- sum(ys)
    n <- NROW(ys)
    return((n1/n) * (1 - (n1/n)))
}

majority_class <- function(ys) {
    n1 <- sum(ys)
    n <- NROW(ys)
    if (n1 * 2 < n) {
        return(0)
    } else {
        return(1)
    }
}

# Function: bestsplit
#
# Arguments
#   x : Vector containing numerical values
#   y : Vector containing binary class labels
#
# The two vectors have the same length.
# This version adopts the brute version approach.
#
# Result
#   A number representing the best split for the given input vectors,meaning 
#   the split that minimize the impurity function.
#   If the vectors have length less than 2, NULL is returned.
#
bestsplit <- function (x, y){
  # TODO is x and y sorted?
  best = NULL
  for (i in seq(x[1 : length(x)-1])){
    pivot = mean(x[i : (i+1)])
    candidate = impurity_reduction(pivot, x, y)
    best = max(best, candidate)
  }
  return (best)
} 
