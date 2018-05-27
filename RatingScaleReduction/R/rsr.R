rsr <-
function(attribute, D, plotRSR = FALSE, method = c("Stop1Max", "StopGlobalMax")) {
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
