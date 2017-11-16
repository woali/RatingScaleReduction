###############################################################################################

# Function Description
#   Rating scale reduction
#   This package implements a rather sophisticated method published in (Koczkodaj et al., 2017) In
#   essence, it is a stepwise method fro maximizing the area under the area (AUC) of receiver operating
#   characteristic (ROC). In this description, data mining terminology will be used:
#       1.examples (observations in statistics),
#       2.variables in statistics,
#       3.class or decision attribute (decision variable may be used statistics).
#   The implemented algorithm (when reduced to its minimum) comes to using a loop for all attributes
#   (with the class excluded) to compute AUC. Subsequently, attributes are sorted in the descending
#   order by AUC. The attribute with the largest AUC is added to a subset of all attributes (evidently, it
#   cannot be empty since it is supposed to be the minimum subset S of all attributes with the maximum
#   AUC). We keep adding the next in line (according to AUC) attribute to the subset S checking AUC.
#   If it decreases, we stop the procedure. The above procedure can be described by the following
#   algorithm.
#   Algorithm:
#       1. compute AUC of all attributes excluding class
#       2. sort attributes by their AUC in the ascending order
#       3. select the attribute with the largest AUC to subset S
#       4. select the next attribute A with the largest AUC to subset S
#       5. if the AUC of the subset S is larger that AUC of the former AUC then go to 3
#   There are a lot of checking (e.g., if the dataset is not empty or full of replications) involved.

# Parameters
#   attribute: a matrix or data.frame containing attributes
#       D: the decision vector
#       plotRSR: If TRUE the ROC curve is ploted
#   method: the Stop reduction criteria: First Max of AUC or Global Max of AUC

# Return result
#   rsr.auc: total AUC of atrtibutes
#   rsr.label: attribute labels
#   summary: a summary table
###############################################################################################

rsr <- function(attribute, D, plotRSR = FALSE, method = c("Stop1Max", "StopGlobalMax")) {
    method <- match.arg(method)
	if (length(names(attribute)) == 0) {
        outlist <- list(message("names(attribute)==NULL Create attribute LABELS (e.g. using colnames)"))
    }
    
    if (nrow(attribute) != length(D)) {
        stop("Attributes and decision must have the same number of rows")
    } 
    else {
        # calculate AUC for every attribute
        
        start.auc <- vector(length = ncol(attribute))
        for (i in 1:(ncol(attribute))) {
            start.auc[i] <- roc(D, attribute[, i], plotROC = FALSE)$auc
        }
        
        mydata1 <- rbind(start.auc, attribute[, 1:(ncol(attribute))])
        s1 <- mydata1[, order(mydata1[1, ], decreasing = TRUE)]
        s <- s1[2:nrow(s1), ]
        
        # calculate running total AUC for attributes 1 to i
        ss <- c()
        ss[1] <- s1[1, 1]
        for (i in 2:ncol(s)) {
            ss[i] <- roc(D, rowSums(s[, 1:i]), plotROC = FALSE)$auc
        }
        
        # create reduced rating scale taking the criteria 'Stop first Max'
        auc1 <- c()

        if (method == "Stop1Max") 
            {
                i <- 1
                while (ss[i + 1] > ss[i]) {
                  auc1[i] <- ss[i]
                  i <- i + 1
                }
                auc.reduct <- c(auc1, ss[i])
            }  # create reduced rating scale taking the criteria 'Global Max'
        
        else {
            i = which.max(ss)
            auc.reduct <- ss[1:i]
        }
        
### create summary table
        if (length(auc.reduct) > 1) {
            tab <- t(rbind(s1[1, 1:length(auc.reduct)], auc.reduct))
            colnames(tab) <- c("AUC one variable", "AUC running total")
        }
        
        else {
            tab <- data.frame(names(s1)[1], auc.reduct)
            colnames(tab) <- c("Attribut", "AUC running total")
        }
        
### plot the ROC curve for reduced rating scale (sum of attributes in rsr)
        if (plotRSR == TRUE) {
            if (length(auc.reduct) == 1) {  # reduced to one attribute ??
                y <- s[, 1]
                p <- plot.roc(D, y, plotROC = TRUE)
                outlist <- list(rsr.auc = auc.reduct, rsr.label = names(s1)[1], summary = tab, p)
            } 
            else {
                p <- roc(D, rowSums(s[, 1:i]))
            ### prepared data for plotting
                x <- 1 - p$specificities
                y <- p$sensitivities
                dfr <- data.frame(x, y)  
### plot ROC curve of total AUC of reduced rating scale
                pp <- ggplot(dfr, aes(x, y)) + geom_line(col = "red", size = 1) + geom_abline(intercept = 0, 
                  slope = 1, size = 0.5) + geom_polygon(fill = "gray") + theme_bw() + theme(panel.border = element_blank(), 
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
                  ggtitle("ROC curve of total AUC of reduced rating scale") + labs(x = "1-specificities", 
                  y = "sensitivities") + theme(plot.title = element_text(hjust = 0.5))
                
                outlist <- list(rsr.auc = auc.reduct, rsr.label = names(s1[1, 1:length(auc.reduct)]), 
                  summary = tab, pp)
            }

            } 
            else { if (length(auc.reduct) == 1) {  
						outlist <- list(rsr.auc = auc.reduct, rsr.label = names(s1)[1], summary = tab)}
							else {
                outlist <- list(rsr.auc = auc.reduct, rsr.label = names(s1[1, 1:length(auc.reduct)]), 
                  summary = tab)}}
    }
    
    class(outlist) = "RatingScaleReduction"
### the list includes ROC curve of total AUC of reduced rating scale, reduced rating scale label and result summary
    return(outlist)  
}
