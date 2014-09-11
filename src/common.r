gini_index <- function(xs) {
    n1 <- sum(xs)
    n <- NROW(xs)
    return((n1/n) * (1 - (n1/n)))
}
