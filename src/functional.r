source("common.r")

tree.functional.classifyI <- function(tr,x) UseMethod("tree.functional.classifyI")

tree.functional.classify <- function(tr,xs) {
    f <- function(x) tree.functional.classifyI(tr, x)
    apply(xs, 1, f)
}

tree.functional.grow <- function(xs, ys, nmin = 0, minleaf = 0) {
    maj <- majority_class(ys)
    t <- tree.functional.growI(xs, ys, maj, nmin, minleaf)
    return(t)
}

# either split the given rows, or create a leaf node
tree.functional.growI <- function(xs, ys, cls, nmin, minleaf) {
    if (gini_index(ys) > 0 && length(ys) >= nmin) {
        sp <- best.split.of.all(xs, ys, minleaf)
        if (! is.null(sp)) {
            obs <- partition(sp$isRight, xs, ys)
            l <- tree.functional.growI(obs$xsl, obs$ysl, majority_class(obs$ysl), nmin, minleaf)
            r <- tree.functional.growI(obs$xsr, obs$ysr, majority_class(obs$ysr), nmin, minleaf)
            return(f_mkNode(l, r, sp$index, sp$split))
        }
    }
    return(f_mkLeaf(xs, ys, cls))
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


f_mkLeaf <- function(xs, ys, cl) {
    l <- list(cls=cl, datx=xs, daty=ys)
    class(l) <- "leaf"
    return(l)
}

f_mkNode <- function(l, r, attr, bnd) {
    n <- list(chldl=l, chldr=r, attr=attr, bnd=bnd)
    class(n) <- "node"
    return(n)
}
