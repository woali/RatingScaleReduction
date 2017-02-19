startAuc <-
function(attribute, D){
if (length(names(attribute))==0){
outlist=list(message('****names(attribute)==NULL!!!! Create attribute LABELS (e.g. using colnames)'))}

else {
start.auc <-vector(length=ncol(attribute))
for (i in 1:(ncol(attribute))) {
start.auc[i] <-roc(D, attribute[,i], plotROC=FALSE)$auc
}

tab=data.frame(names(attribute)[1:(ncol(attribute))],start.auc)
colnames(tab)=c("item", "auc") 

outlist=list(auc=start.auc, item=names(attribute)[1:(ncol(attribute))], summary=tab)}
class(outlist) = "startAuc"
return(outlist)
}
