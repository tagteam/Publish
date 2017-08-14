canbe.numeric <- function(x){
    if (!is.character(x)) x <- as.character(x)
    u <- x[!is.na(x) & x!="NA"]
    test <- suppressWarnings(as.numeric(u))
    if (any(is.na(test)))
        FALSE
    else
        TRUE
}
