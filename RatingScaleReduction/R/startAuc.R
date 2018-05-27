startAuc <-
function(attribute, D) {
    
    # attribute - the matix of attributes, every attribute must have headers D - the decision vector
    # testing for null length 
    if (length(names(attribute)) == 0) {
        outlist <- list(message("names(attribute)==NULL Create attribute LABELS (e.g. using colnames)"))
    } 
    else {
        
        # set start.auc of the length number of column of attribute
        start.auc <- vector(length = ncol(attribute))
        # iterate for all attributes
        for (i in 1:(ncol(attribute))) {
            # compute AUC for every attribute
            start.auc[i] <- roc(D, attribute[, i], plotROC = FALSE)$auc
        }
        
        # create AUC one variable and AUC running total of data table
        tab <- data.frame(names(attribute)[1:(ncol(attribute))], start.auc)
        # header of date table
        colnames(tab) = c("item", "auc")
        
        # create output list
        outlist <- list(auc = start.auc, item = names(attribute)[1:(ncol(attribute))], summary = tab)
    }
    class(outlist) = "startAuc"
    return(outlist)
}
