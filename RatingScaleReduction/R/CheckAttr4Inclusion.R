###############################################################################################
# Function description
#   Check the next attribute for possible inclusion into AUC.
#   The next attribute is added to the running total. The running total is used with the class (decision
#   attribute) to compute AUC. The next attribute is added to the sequence of attributes having the MAX
#   total AUC.
#
# Parameters
#   attribute: a matrix or data.frame containing attributes
#   D: the decision vector
#   plotCheck: If TRUE the plot with two ROC curves is created
#   method: the method to useas in the function roc.test{pROC}
#   boot.n: boostrap replication number
#   alternative: the alternative hypothesis
#
# Return
#   test: the result of the roc.test as in the function roc.test from the package pROC
###############################################################################################

CheckAttr4Inclusion <- function(attribute, D, plotCheck = FALSE, method = c("delong", "bootstrap", 
    "venkatraman", "sensitivity", "specificity"), boot.n, alternative = c("two.sided", "less", "greater")) {
    
    if (length(names(attribute)) == 0) 
        {
            outlist <- list(message("names(attribute)==NULL Create attribute LABELS (e.g. using colnames)"))
        }  
    # compare two ROC curves - for reduced rating scale and rating scale with added next attribute (global MAX criteria)
    else {
        tot.auc <- totalAuc(attribute, D)$total.auc
        item <- totalAuc(attribute, D)$item
        o.attribute <- totalAuc(attribute, D)$ordered.attribute
        j <- which.max(tot.auc)
        
        item.next <- roc(D, rowSums(o.attribute[, 1:(j + 1)]), plotROC = FALSE)
        rsr.auc <- roc(D, rowSums(o.attribute[, 1:j]), plotROC = FALSE)
        test <- roc.test(rsr.auc, item.next, method, alternative, boot.n)  #H1: difference
        outlist <- list(test)
    }
    
    # plot two ROC curves: Total AUC and AUC of the reduced rating scale
    if (plotCheck == TRUE) {
        # compute specificities for Total AUC
        x <- 1 - test$roc1$specificities
        # set sensitivities for Total AUC
        y <- test$roc1$sensitivities
        # compute specificities for reduced rating scale AUC
        x1 <- 1 - test$roc2$specificities
        # set sensitivities for reduced rating scale AUC
        y1 <- test$roc2$sensitivities
        #plot the Total AUC of rsr 
        plot(x, y, type = "l", lwd = 2)
        lines(x1, y1, type = "l", lwd = 2, col = "red")
        legend("bottomright", lwd = c(1, 2), col = c("black", "red"), c(paste("Total AUC of rsr", 
            round(test$roc1$auc, 3)), paste("Total AUC of rsr plus next attribute", round(test$roc2$auc, 
            3))))
        abline(0, 1, lwd = 2, col = "gray")
    }
    
    return(test)
}

