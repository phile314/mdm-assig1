source("common.r")

tree.grow <- function(x, y, nmin = 0, minleaf = 0, impurity = gini_index){
  # Initialization
  N <- length(y)
  tree <- data.frame(left = rep(NA,N),
                     right = rep(NA, N),
                     label = rep(NA,N), # logical? (binary classification)
                     split = rep(NA, N),
                     splitCol = rep(NA, N))
  tree[1, ] <- mkLeaf(y)
  worklist <- list(1)
  samples <- list()
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
        next # TODO clean samples ? doesn't seem necessary

      obs <- partition(best$isRight, x.current, y.current)

      # Make leaves
      left.index <- freeRow
      right.index <- freeRow + 1
      tree[left.index ,] <- mkLeaf(obs$ysl)
      tree[right.index,] <- mkLeaf(obs$ysr) # TODO should we always enforce two different class labels?! 

      # Split samples
      samples[[left.index]] <- current.samples[x.current[, best$index] <= best$split]
      samples[[right.index]] <- current.samples[x.current[, best$index] > best$split]

      # Add children to current node
      tree[current.index, ] <- mkNode(left.index, right.index, best)
      
      # Control iteration
      worklist <- c(worklist, left.index, right.index)
      freeRow <- right.index + 1
    }
  }
  return(tree[1:(freeRow - 1), ])
}

mkLeaf <- function(y) c(NA, NA, majority_class(y), NA, NA)

mkNode <- function (left.index, right.index, best) {
  c(left.index, right.index, NA, best$split, best$index)
}

tree.classify <- function (x, tr) {
  apply(x, 1, predict, tr)
}

is.leaf <- function(node) (! is.na(node$label))

predict <- function(x, tr){
  node <- tr[1, ]
  while (!(is.leaf(node))){
    succ <- if (x[node$splitCol] <= node$split) node$left else node$right
    node <- tr[succ, ]
  }
  return(node$label)
}