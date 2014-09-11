source("common.r")

tree.functional.classify <- function(xs) UseMethod("tree.functional.classify")

tree.functional.grow <- function(xs, ys) {
    maj <- majority_class ys
    t <- tree.functional.growI(xs, ys, maj)
    return(t)
}

# either split the given rows, or create a leaf node
tree.functional.growI <- function(xs, ys, cls) {
    sp <- get_best_split(xs, ys)
    if () {
        return(mkLeaf(xs, ys, cls))
    } else {
        return(mkNode(....))
    }
}


#tree.functional.


mkLeaf <- function(xs, ys, cl) {
    l <- list(cl, xs, ys)
    class(l) <- "leaf"
    return(l)
}

mkNode <- function(l, r, attr, bnd) {
    n <- list(l, r, attr, bnd)
    class(n) <- "node"
    return(n)
}
