# This script contains the implementation of classification trees using an
# imperative approach.

source("common.r")

# Function: tree.grow(x, y, nmin, minleaf, impurity)
# Trains a binary classification tree.
#
# Arguments
#   x : A numerical matrix. Each column contains observations for a
#       a numerical/binary attribute.
#   y : A numerical (binary) vector, containing the true class label
#       for each row of x
#   nmin : an integer representing the minimum number of observation that a node
#          must contain for it to be allowed to be split
#   minleaf : an integer representing the minimum number for a leaf node
#   impurity : The impurity function that will be used in the algorithm to
#              compute the impurity reduction (default = gini index)
#
# The number of rows of x is the same as the length of y.
#
# Result: A tree data structure, that can be used to predict class labels
#         with the classify function.
#         A tree is represented as a non-empty data frame containing the following
#         columns: left, right, label, split, splitCol
#         The first row represent the root of the tree.
tree.grow <- function(x, y, nmin = 0, minleaf = 0, impurity = gini_index){
  # Initialization
  N <- length(y)
  tree <- data.frame(left = rep(NA,N),
                     right = rep(NA, N),
                     label = rep(NA,N),
                     split = rep(NA, N),
                     splitCol = rep(NA, N))
  tree[1, ] <- mkLeaf(y)
  worklist <- list(1) # List of leaves indexes that still need to be split
  samples <- list() # Observations row indexes contained by each leaf
  samples[[1]] <- 1:length(y)
  freeRow <- 2

  while(length(worklist) != 0){
    current.index <- worklist[[1]]
    worklist <- worklist[-1]
    current.samples <- samples[[current.index]]
    y.current <- y[current.samples]
    x.current <- x[current.samples, , drop = FALSE]

    if(impurity(y.current) > 0 && length(current.samples) >= nmin){
      best <- best.split.of.all(x.current, y.current, minleaf, impurity)

      if (is.null(best))
        next

      # Make leaves
      left.index <- freeRow
      right.index <- freeRow + 1
      tree[left.index ,] <- mkLeaf(y.current[!best$isRight])
      tree[right.index,] <- mkLeaf(y.current[best$isRight])

      # Split samples
      samples[[left.index]] <- current.samples[!best$isRight]
      samples[[right.index]] <- current.samples[best$isRight]

      # Add children to current node
      tree[current.index, ] <- mkNode(left.index, right.index, best)

      # Control iteration
      worklist <- c(worklist, left.index, right.index)
      freeRow <- right.index + 1
    }
  }
  return(tree[1:(freeRow - 1), ])
}


# Function: tree.classify(x, tr)
# Predicts the class label for each row in the input attributes matrix.
#
# Arguments
#   x : A matrix with the same number of columns as the matrix used to train tr
#   tr : A tree object produced by the tree.grow function
#
# Result
#   A vector of binary class labels. It contains the predicted class label
#   for each row in x.
tree.classify <- function (x, tr) apply(x, 1, predict, tr)


# Function: mkLeaf(y)
# Proudces a leaf row
#
# Arguments
#   y : A vector of binary class labels
#
# Result
#   A row representing a leaf node in the tree data structure.
#   All the fields are set to NA, except for label, which is computed applying
#   majority vote to the class label vector y.
mkLeaf <- function(y) c(NA, NA, majority_class(y), NA, NA)


# Function: mkNode(left, right, best)
# Produces an internal node row.
#
# Arguments
#   left, right : Integer numbers representing the row index at which the left
#                 and right are stored in the tree data structure.
#   best : A split object containing the fields split (threshold value) and
#          index, which is the column number of the attribute matrix in which
#          the same attribute values are stored
#
# Result
#  A vector representing an internal node row in the tree data structure.
#  The label field is set to NA.
mkNode <- function (left.index, right.index, best) {
  c(left.index, right.index, NA, best$split, best$index)
}


# Function: predict(x, tr)
# Predicts the class label for a single attributes input vector
#
# Arguments:
#   x : A numerical vector containing the attributes observations
#   tr : A tree object produced by the tree.grow function
#
# Result
#  The binary class label predicted by the trained tree.
predict <- function(x, tr){
  i <- 1
  while (is.na(tr$label[i]))
    i <- if (x[tr$splitCol[i]] <= tr$split[i]) tr$left[i] else tr$right[i]
  return(tr$label[i])
}
