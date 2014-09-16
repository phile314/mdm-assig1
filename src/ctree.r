source("common.r")

tree.grow <- function(x, y, nmin, minleaf, impurity = gini_index){
  N <- length(y)
  tree <- data.frame(left = rep(NA,N),
                     right = rep(NA, N),
                     label = rep(NA,N)) # logical? (binary classification)

  # Initialization
  tree[1, ] <- c(NA, NA, majority_class(y))
  worklist <- list(1)
  samples = list()
  samples[[1]] = 1:length(y)
  freeRow <- 2
  
  while(length(worklist) != 0){
    current.index <- worklist[[1]]
    worklist[[1]] <- NULL
    current.samples <- samples[[current.index]]
    y.current <- y[current.samples]
    x.current <- x[current.samples,]
    if(impurity(y.current) > 0) { # || length(samples[current]) < nmin ){
      best <- best_of_best(x.current, y.current)
      if (best == NULL)
        next # TODO clean samples ? doesn't seem necessary
      left <- best$nodes$left
      right <- best$nodes$right
            
      tree[freeRow,] <- c(NA, NA, majority_class(left$y))
      tree[freeRow + 1,] <- c(NA, NA, majority_class(right$y)) # TODO should we always enforce two different class labels?! 
      sample[[freeRow]] <- current.samples[x.current[best$col, ] <= best$split]
      sample[[freeRow + 1]] <- current.samples[- (x.current[best$col, ] <= best$split)]
      
      workinglist <- c(workinglist, freeRow, freeRow + 1)
      freeRow = freeRow + 2      
    }
  }
  return(tree[1:(freeRow - 1), ])
}

# TODO better name
# TODO repetition from best_split
best_of_best <- function(attrs, y){
  candidates <- apply(attrs, c(2), best_split, y)
  best <- list(reduction = 0, col = 0) 
  for (i in seq(candidates)){
    c <- candidates[[i]]
    if (is.null(c))
      next
    if (c$reduction >= best$reduction){
      best <- c
      best$col <- i
    }    
  }
  if (best.col == 0)
    return(NULL)
  return(best)
}
