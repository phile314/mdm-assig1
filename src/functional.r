source("common.r")

tree.functional.classifyI <- function(tr,x) UseMethod("tree.functional.classifyI")

tree.functional.classify <- function(tr,xs) {
    apply(xs, 1, (tree.functional.classifyI tr)
}

tree.functional.grow <- function(xs, ys) {
    maj <- majority_class ys
    t <- tree.functional.growI(xs, ys, maj)
    return(t)
}

# either split the given rows, or create a leaf node
tree.functional.growI <- function(xs, ys, cls) {
    sp <- get_best_split(xs, ys)
    if (sp .... nil) {
        return(mkLeaf(xs, ys, cls))
    } else {
        # split data
        

        l <- tree.functional.growI(,, 0)
        r <- tree.functional.growI(,, 1)
        return(mkNode(l, r,..))
    }
}


tree.functional.classifyI.leaf <- function(lf,x) {
    return(nd...cls)
}

tree.functional.classifyI.node <- function(nd,x) {
    if(x...attr < nd...attr) {
        return(tree.functional.classifyI(nd...l, x))
    } else {
        return(tree.functional.classifyI(nd...r, x))
    }
}


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
