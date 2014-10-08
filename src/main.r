source("common.r")
source("ctree.r")
source("functional.r")

test.pima <- function(){
  pima <- read.data("../data/pima.txt", 0)
  pima.tree <- tree.grow(pima$train.x, pima$train.y, nmin = 20, minleaf = 5)
  pima.actual <- tree.classify(pima$train.x, pima.tree)
  pima.expected <- pima$train.y
  return(table(pima.expected, pima.actual))
}

test.credit <- function(){
  credit <- read.data("../data/credit.txt", 0, header = TRUE)
  return(tree.grow(credit$train.x, credit$train.y, nmin = 2, minleaf = 1))
}
