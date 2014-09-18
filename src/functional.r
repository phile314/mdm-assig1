source("common.r")

tree.functional.classifyI <- function(tr,x) UseMethod("tree.functional.classifyI")

tree.functional.classify <- function(tr,xs) {
    f <- function(x) tree.functional.classifyI(tr, x)
    apply(xs, 1, f)
}

tree.functional.grow <- function(xs, ys, minleaf = 0) {
    maj <- majority_class(ys)
    t <- tree.functional.growI(xs, ys, maj, minleaf)
    return(t)
}

# either split the given rows, or create a leaf node
tree.functional.growI <- function(xs, ys, cls, minleaf) {
    sp <- best_of_best(xs, ys, minleaf)
    if (is.null(sp)) {
        return(mkLeaf(xs, ys, cls))
    } else {
        obs <- sp[["nodes"]]
        l <- tree.functional.growI(obs[["xsl"]],  obs[["ysl"]], 0, minleaf)
        r <- tree.functional.growI(obs[["xsr"]], obs[["ysr"]], 1, minleaf)
        return(mkNode(l, r, sp$col, sp$split))
    }
}


tree.functional.classifyI.leaf <- function(lf,x) {
    return(lf[["cls"]])
}

tree.functional.classifyI.node <- function(nd,x) {
    if(x[nd[["attr"]]] <= nd[["bnd"]]) {
        return(tree.functional.classifyI(nd[["chldl"]], x))
    } else {
        return(tree.functional.classifyI(nd[["chldr"]], x))
    }
}


mkLeaf <- function(xs, ys, cl) {
    l <- list(cls=cl, datx=xs, daty=ys)
    class(l) <- "leaf"
    return(l)
}

mkNode <- function(l, r, attr, bnd) {
    n <- list(chldl=l, chldr=r, attr=attr, bnd=bnd)
    class(n) <- "node"
    return(n)
}
