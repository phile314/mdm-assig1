
source("common.r")
source("ctree.r")

test.pima <- function(){
  pima <- read_data("../data/pima.txt")
  pima.tree <- tree.grow(pima$xs, pima$ys, nmin = 20, minleaf = 5)
  pima.actual <- tree.classify(pima$xs, pima.tree)
  pima.expected <- pima$ys
  return(table(pima.expected, pima.actual))
}