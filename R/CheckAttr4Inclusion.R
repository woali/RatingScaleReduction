CheckAttr4Inclusion <-
function(attribute,D, method=c("delong", "bootstrap",
"venkatraman", "sensitivity", "specificity"), alternative = c("two.sided", "less", "greater")){
if (length(names(attribute))==0){
outlist=list(message('****names(attribute)==NULL!!!! Create attribute LABELS (e.g. using colnames)'))}

else {

tot.auc <-totalAuc(attribute, D)$total.auc
item <-totalAuc(attribute, D)$item
o.attribute <-totalAuc(attribute, D)$ordered.attribute
j <-which.max(tot.auc)
 
item.next <-roc(D, rowSums(o.attribute[,1:(j+1)]), plotROC=FALSE)
rsr.auc <-roc(D, rowSums(o.attribute[,1:j]), plotROC=FALSE)
test <-roc.test(rsr.auc, item.next, method, alternative) #H1: difference
outlist=list(test)
}
return(test)
}
