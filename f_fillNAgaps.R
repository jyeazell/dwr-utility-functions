fillNAgaps <- function(x, firstBack=FALSE) {
    ## NA's in a vector or factor are replaced with last non-NA values
    ## If firstBack is TRUE, it will fill in leading NA's with the first
    ## non-NA value. If FALSE, it will not change leading NA's.
    
    # If it's a factor, store the level labels and convert to integer
    
    lvls <- NULL
    if (is.factor(x)) {
        lvls <- levels(x)
        x    <- as.integer(x)
    }
    
    goodIdx <- !is.na(x)
    
    # These are the non-NA values from x only
    # Add a leading NA or take the first good value, depending on firstBack   
    if (firstBack)   goodVals <- c(x[goodIdx][1], x[goodIdx])
    else             goodVals <- c(NA,            x[goodIdx])
    
    # Fill the indices of the output vector with the indices pulled from
    # these offsets of goodVals. Add 1 to avoid indexing to zero.
    fillIdx <- cumsum(goodIdx)+1
    
    x <- goodVals[fillIdx]
    
    # If it was originally a factor, convert it back
    if (!is.null(lvls)) {
        x <- factor(x, levels=seq_along(lvls), labels=lvls)
    }
    
    x
}