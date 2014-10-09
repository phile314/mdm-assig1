# This script contains code used to benchmark the imperative and functional
# implementation of classification trees and to find the best parameters
# configurations for the spambase data set.

library(microbenchmark)
library(parallel)
source("ctree.r")
source("functional.r")

################################################################################
# Benchmark
################################################################################

benchmark <- function(){
  pima <- read.data('../data/pima.txt', 0.3)
  bench.grow <- microbenchmark(tree.grow(pima$train.x, pima$train.y, 20, 5),
                               tree.functional.grow(pima$train.x, pima$train.y, 20, 5),
                               times = 5)

  imp.tree <- tree.grow(pima$train.x, pima$train.y, 20, 5)
  fun.tree <- tree.functional.grow(pima$train.x, pima$train.y, 20, 5)

  bench.classify <- microbenchmark(tree.classify(pima$test.x, imp.tree),
                                   tree.functional.classify(pima$test.x, fun.tree),
                                   times = 5)
  return(list(grow = bench.grow, classify = bench.classify))
}

################################################################################
# Parameter search
################################################################################

# Computes the error rate for a number of combinations of nmin and minleaf parameters
search.params <- function(){
  data <- read.data('../data/spambase.data', 0.3)
  nmins <- c(0,1,2,3,4,5,6,8,10,15,20,30,50,80,110,140,170,200,275,400,500,750,1000,1500,3000)
  minleafs <- c(0,1,2,3,4,5,6,8,10,15, 25, 50, 100, 200, 500)
  ps_pars <- list()

  for(i in 1:length(nmins))
    for(j in 1:length(minleafs))
      # The minleaf constraint implies all nmin constraints for which
      # nmins <= 2*minleaf holds. Just skip them.
      if(nmins[i] >= 2 * minleafs[j])
        ps_pars[[length(ps_pars) + 1]] <- list(nmin = nmins[i], minleaf = minleafs[j])

  ps_lbls <- rep(NA, length(ps_pars))   # we are not using the labels...
  e_ps <- eval_mthd(data, ps_lbls, ps_pars, eval_with_pars)
  return(e_ps)
}
# Function: error_rate
#
# Arguments
#   cm : The confusion matrix.
#
# Result
#   The error rate.
error_rate <- function(cm) {
  return(1 - (sum(diag(cm)) / sum(cm)))
}

# Function: cm
#
# Arguments
#   act : Vector of predicted class label.
#   exp : Vector of exptected class labels
#
# The two vectors have the same length.
#
# Result
#   The confusion matrix.
cm <- function(act, exp) {
  return(table(act, exp))
}

# Function: eval_mthd
#
# Arguments
#   data : The data set on which to evaluate the parameters.
#   lbls : Vector containing the descriptions of the parameters.
#   vals : The parameter combinations to evaluate.
#   r    : The evaluation function.
#
#
# Result
#   A list containing:
#     all : A list containing the results of calling the `r` function.
eval_mthd <- function(data, lbls, vals, r) {
  f <- function(lbl, v) list(par_lbl = lbl, model =r(data, v))
  all <- (mcmapply(f, lbls, vals, SIMPLIFY = FALSE, mc.cores = detectCores()))
  return (list(all = all, lbls = lbls, params = vals))
}

# Function: eval_to_df
#
# Arguments
#   eres : Result from the eval_mthd function.
#
#
# Result
#   A data frame representation of the result.
eval_to_df <- function(eres) {
  N <- length(eres$all)
  df <- data.frame(   nmin = rep(NA, N),
                      minLeaf = rep(NA, N),
                      error = rep(NA, N))
  for(i in 1:(length(eres$all))) {
    pr <- eres$params[[i]]
    df[i, ] <- c(pr$nmin, pr$minleaf, eres$all[[i]]$model$error)
  }
  return(df)
}

# Function: eval_with_pars
#
# Arguments:
#   data: The dataset on which to evaluate the algorithm.
#   par : A list of the parameters to use (nmin, minleaf)
#
# Result
# A list of
#   model : the fitted tree
#   pred  : the predicted class label
#   error : the error rate
#   cm    : the confusion matrix
#
# Evaluates the tree classification algorithm.
eval_with_pars <- function(data, par) {
  model <- tree.functional.grow(data$train.x, data$train.y, par$nmin, par$minleaf)
  prys <- tree.functional.classify(data$test.x, model)
  cm <- cm(prys, data$test.y)
  er <- error_rate(cm)
  return(list(model=model, pred=prys, error=er, cm=cm))
}

