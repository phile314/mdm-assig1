
source("common.r")
source("ctree.r")

test.pima <- function(){
  pima <- read.data("../data/pima.txt")
  pima.tree <- tree.grow(pima$xs, pima$ys, nmin = 20, minleaf = 5)
  pima.actual <- tree.classify(pima$xs, pima.tree)
  pima.expected <- pima$ys
  return(table(pima.expected, pima.actual))
}

test <- function(filename, nmin, minleaf){
  data <- read.data(filename)
  tree <- tree.grow(data$xs, data$ys, nmin, minleaf)
  actual <- tree.classify(data$xs, tree)
  expected <- data$ys
  return(table(expected, actual))
}

credit.data <- read.data('../data/credit.txt')
credit.tree <- tree.grow(credit.data$xs, credit.data$ys, nmin = 2, minleaf = 1)