source("common.r")

# Function: tree.functional.classifyI
#
# Arguments:
#   tr : The tree to use for predicting.
#   x  : A vector containing the attribute values.
#
# Result
# The predicted class (either 0 or 1).
#
# Predicts the class label of one observation.
tree.functional.classifyI <- function(tr,x) UseMethod("tree.functional.classifyI")

# Function: tree.functional.classify
#
# Arguments:
#   tr : The tree to use for predicting.
#   xs  : A matrix containing the attribute values, one row for each observation.
#
# Result
# Vector of the predicted class labels (either 0 or 1).
#
# Predicts the class label of some observations.
tree.functional.classify <- function(tr,xs) {
    f <- function(x) tree.functional.classifyI(tr, x)
    apply(xs, 1, f)
}

# Function: tree.functional.grow
#
# Arguments:
#   xs  : A matrix containing the attribute values, one row for each observation.
#   ys  : Vector with the class labels.
#   nmin: Minimal number of obs. required to split a leaf.
#   minleaf: Minimal number of obs. required in new leafs.
#
# Result
# The tree.
#
# Grows a tree from a data set.
tree.functional.grow <- function(xs, ys, nmin = 0, minleaf = 0) {
    maj <- majority_class(ys)
    t <- tree.functional.growI(xs, ys, maj, nmin, minleaf)
    return(t)
}

# Function: tree.functional.growI
#
# Arguments:
#   xs  : A matrix containing the attribute values, one row for each observation.
#   ys  : Vector with the class labels.
#   cls : The class label to predict for the given data set, if no split can be made.
#   nmin: Minimal number of obs. required to split a leaf.
#   minleaf: Minimal number of obs. required in new leafs.
#
# Result
# The subtree for the given observations. Either a node,
# when a split occured, else a leaf with the class passed
# in a parameter cls.
#
# Grows a (sub)tree from a data set.
tree.functional.growI <- function(xs, ys, cls, nmin, minleaf) {
    if (gini_index(ys) > 0 && length(ys) >= nmin) {
        sp <- best.split.of.all(xs, ys, minleaf)
        if (! is.null(sp)) {
            obs <- partition(sp$isRight, xs, ys)
            # the left/right subtree could be visited in parallel
            l <- tree.functional.growI(obs$left.x, obs$left.y, majority_class(obs$left.y), nmin, minleaf)
            r <- tree.functional.growI(obs$right.x, obs$right.y, majority_class(obs$right.y), nmin, minleaf)
            return(f_mkNode(l, r, sp$index, sp$split))
        }
    }
    return(f_mkLeaf(xs, ys, cls))
}

# Function: tree.functional.classifyI.leaf
#
# Arguments:
#   lf  : The leaf.
#   x   : The attribute values.
#
# Result
# The class label.
#
# Returns the class label for a leaf.
tree.functional.classifyI.leaf <- function(lf,x) {
    return(lf[["cls"]])
}

# Function: tree.functional.classifyI.node
#
# Arguments:
#   nd  : The node.
#   x   : The attribute values.
#
# Result
# The class label.
#
# Returns the class label predicted by this (sub)tree.
tree.functional.classifyI.node <- function(nd,x) {
    if(x[nd[["attr"]]] <= nd[["bnd"]]) {
        return(tree.functional.classifyI(nd[["chldl"]], x))
    } else {
        return(tree.functional.classifyI(nd[["chldr"]], x))
    }
}

# Function: f_mkLeaf
#
# Arguments:
#   xs  : A matrix containing the attribute values, one row for each observation.
#   ys  : Vector with the class labels.
#   cl  : Class of the leaf node.
#
# Result
# A leaf node.
#
# Creates a leaf.
f_mkLeaf <- function(xs, ys, cl) {
    l <- list(cls=cl, datx=xs, daty=ys)
    class(l) <- "leaf"
    return(l)
}

# Function: f_mkNode
#
# Arguments:
#   l   : The left subtree.
#   r   : The right subtree.
#   attr: The index of the attribute to split on.
#   bnd : The (numeric) treshold where to split.
#
# Result
# A node.
#
# Creates a node.

f_mkNode <- function(l, r, attr, bnd) {
    n <- list(chldl=l, chldr=r, attr=attr, bnd=bnd)
    class(n) <- "node"
    return(n)
}
