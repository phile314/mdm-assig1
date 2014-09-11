

gini_index <- function(ys) {
    n1 <- sum(ys)
    n <- NROW(ys)
    return((n1/n) * (1 - (n1/n)))
}

majority_class <- function(ys) {
    n1 <- sum(ys)
    n <- NROW(ys)
    if (n1 * 2 < n) {
        return(0)
    } else {
        return(1)
    }
}

# Empty list means there exists no valid split,
# else return an optimal split.
# Only return splits which improve the impurity, else we could loop forever (different class with same data)
get_best_split <- function(xs, ys) {
    return(list())
    # return(list(attr=,bnd=,xsl=,xsr=,ysl=,ysr=))
}
