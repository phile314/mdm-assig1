
source("common.r")
source("ctree.r")

test.pima <- function(){
  pima <- read_data("../data/pima.txt", 0)
  pima.tree <- tree.grow(pima$trxs, pima$trys, nmin = 20, minleaf = 5)
  pima.actual <- tree.classify(pima$trxs, pima.tree)
  pima.expected <- pima$trys
  return(table(pima.expected, pima.actual))
}
