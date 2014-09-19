
source("common.r")
source("ctree.r")
source("functional.r")

test.pima <- function(){
  pima <- read_data("../data/pima.txt", 0)
  pima.tree <- tree.grow(pima$trxs, pima$trys, nmin = 20, minleaf = 5)
  pima.actual <- tree.classify(pima$trxs, pima.tree)
  pima.expected <- pima$trys
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