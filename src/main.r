# This script contains tests used to check the correctness of our implementation

source("common.r")
source("ctree.r")
source("functional.r")

# Grows a classification tree on the complete pima data set and uses it to
# predict the training sample itself and finally returns the resulting
# confusion matrix. The matrix is almost identical to the one reported in the
# assignment. Different runs returns slight different versions, as ties are
# broken at random.
test.pima <- function(){
  pima <- read.data("../data/pima.txt", 0)
  pima.tree <- tree.grow(pima$train.x, pima$train.y, nmin = 20, minleaf = 5)
  pima.actual <- tree.classify(pima$train.x, pima.tree)
  pima.expected <- pima$train.y
  return(table(pima.expected, pima.actual))
}

# Grows a classification tree on the credit data set used in the lectures.
# It returns the same classification tree. (The tree is actually symmetric to
# that one, because observations that satisfy a <= split are always assigned
# to the left child).
test.credit <- function(){
  credit <- read.data("../data/credit.txt", 0, header = TRUE)
  return(tree.grow(credit$train.x, credit$train.y, nmin = 2, minleaf = 1))
}

# Example with the dataset of our choice (spambase).
# A classification tree is grown on 70% of the data set and tested against
# the remaining 30%. The error rate is returned.
test.spambase <- function(){
  sb <- read.data("../data/spambase.data", 0.3)
  sb.tree <- tree.grow(sb$train.x, sb$train.y, nmin = 6, minleaf = 15)
  sb.actual <- tree.classify(sb$test.x, sb.tree)
  sb.expected <- sb$test.y
  sb.cm <- table(sb.expected, sb.actual)
  sb.error <- error_rate(sb.cm)
  return(sb.error)
}
