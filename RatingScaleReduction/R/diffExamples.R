###############################################################################################
# Function description
#	The number of different (unique) examples in a dataset
#	Datasets may contain replications. In particular, one example may be replicated n times, where n
#	is the total number of examples. Such situation would deviate computations and should be early detected. 
#
# Parameters
#	attribute: a matrix or data.frame containing attributes
#
# Return the list of
#	total.examples: a number of examples in a data
#	diff.examples: a number of different examples in a data
#	dup.exapmles: a number of duplicate examples in a data
###############################################################################################


diffExamples <- function(attribute) {
    # select the number of unique examples
    m1 <- unique(attribute)
    # number of unique examples
    u <- nrow(m1)
    # compute the number of repetitive examples according to attributes
    d <- nrow(as.matrix(attribute)) - u
    
    return(list(total.examples = nrow(as.matrix(attribute)), diff.examples = u, dup.examples = d))
}

