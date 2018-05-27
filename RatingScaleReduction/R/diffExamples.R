diffExamples <-
function(attribute) {
    # select the number of unique examples
    m1 <- unique(attribute)
    # number of unique examples
    u <- nrow(m1)
    # compute the number of repetitive examples according to attributes
    d <- nrow(as.matrix(attribute)) - u
    
    return(list(total.examples = nrow(as.matrix(attribute)), diff.examples = u, dup.examples = d))
}
