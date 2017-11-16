###############################################################################################
# Function description
#   AUC of the running total of attributes
#   AUC values are computed for all individual attributes. We sort them in an ascending order. We
#   beging with the attribute having the largest AUC and add to it the second, third,... attribute until
#   AUC of the total of them decreases.
#
# Parameters
#   attribute: a matrix or data.frame containing attributes
#   D: the decision vector
#   plotT: If TRUE the plot is created: x - labels of atrributes, y - total AUC in ascending order
#
# Return the list of
#   ordered.attribute:ordered attribute matrix
#   total.auc: total AUC
#   item: ordered attribute labels
#   summary: a summary table
###############################################################################################

totalAuc <- function(attribute, D, plotT = FALSE) {
     # testing for null length of names of attribute
    if (length(names(attribute)) == 0) {
        outlist = list(message("names(attribute)==NULL Create attribute LABELS (e.g. using colnames)"))
    } 
    else {
        
        #create start.auc based on the length of attribute
        start.auc <- vector(length = ncol(attribute))
        # compute AUC for every attribute
        for (i in 1:(ncol(attribute))) {
            # compute AUC of every attribute
            start.auc[i] <- roc(D, attribute[, i], plotROC = FALSE)$auc
        }
        
        # row bind every column in attribute to start.auc
        attribute1 <- rbind(start.auc, attribute[, 1:(ncol(attribute))])
        # sort attributes according to AUCs in decreasing order
        s1 <- attribute1[, order(attribute1[1, ], decreasing = TRUE)]
        s <- s1[2:nrow(s1), ]
        
        # compute running total AUC for attributes 1 to i
        ss <- c()
        # set s1[1,1] to ss[1]
        ss[1] <- s1[1, 1]
        # iterate for all attributes of s
        for (i in 2:ncol(s)) {
            # compute 
            ss[i] <- roc(D, rowSums(s[, 1:i]), plotROC = FALSE)$auc
        }
        
        # create summary table
        tab <- data.frame(t(rbind(s1[1, ], ss)))
        # set header for tab
        colnames(tab) <- c("AUC one variable", "AUC running total")
        
        # plot the curve with running total AUC for sorted attributes
        plot.total.auc <- function(total.auc, label) {
            plot(total.auc, xaxt = "n", type = "l", ylab = "Total AUC", xlab = "Attribute number or identification", 
                lwd = 2)
            points(which.max(total.auc), max(total.auc), col = "red", lwd = 2)
            abline(v = which.max(total.auc), col = "blue", lty = 2, lwd = 2)
            labele = c(names(label))
            mtext(labele, at = 1:ncol(label), side = 1)
        }
        
        # if the parameter plotT is true, return the plot of total auc.
        if (plotT == TRUE) {
            outlist <- list(ordered.attribute = s, total.auc = ss, item = names(s1), summary = tab, 
                plot.total.auc(ss, s))
        # if the parameter plotT is true, return the non-plot of total auc.
        } 
        else outlist <- list(ordered.attribute = s, total.auc = ss, item = names(s1), summary = tab)
        
    }
    class(outlist) = "totalAuc"
    return(outlist)
}

