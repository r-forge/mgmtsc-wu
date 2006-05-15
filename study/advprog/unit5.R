print.relation <- function(x, ...) {
    cat("A binary relation on", nrow(x$incidences), "objects.\n")
}

incidences <- function(x) {
    if(!inherits(x, "relation")) stop("Wrong class.")
    x $ incidences
}

as.relation <- function(x) UseMethod("as.relation")

as.relation.matrix <- function(x)
    .relation_from_incidences(x)

as.relation.numeric <- function(x)
    .relation_from_incidences(outer(x, x, "<=") * 1)

.relation_from_incidences <- function(x) {
    y <- list(incidences = x)
    class(y) <- "relation"
    y
}   

dual <- function(x) {
    .relation_from_incidences(t(incidences(x)))
}
