totalAuc <-
function(attribute,D, plotT=FALSE){
if (length(names(attribute))==0){
outlist=list(message('****names(attribute)==NULL!!!! Create attribute LABELS (e.g. using colnames)'))}

else {

start.auc <-vector(length=ncol(attribute))
for (i in 1:(ncol(attribute))) {
start.auc[i] <-roc(D, attribute[,i], plotROC=FALSE)$auc}

attribute1 <-rbind(start.auc,attribute[,1:(ncol(attribute))])
s1 <-attribute1[,order(attribute1[1,],  decreasing = TRUE)]
s <-s1[2:nrow(s1),]

ss <-c()
ss[1]<-start.auc[1]
for (i in 2:ncol(s)){
ss[i] <-roc(D, rowSums(s[,1:i]), plotROC=FALSE)$auc
 }

tab <-data.frame(t(rbind(s1[1,],ss)))
colnames(tab) <-c('AUC one variable', 'AUC running total')

plot.total.auc <-function(total.auc, label){
plot(total.auc, xaxt="n", type='l', ylab="Total AUC", xlab="Attribute number or identification", lwd=2)
points(which.max(total.auc),max(total.auc), col='red', lwd=2)
abline(v=which.max(total.auc), col='blue', lty=2, lwd=2)
labele = c(names(label))
mtext(labele,at=1:ncol(label),side=1)
}

if (plotT == TRUE){
outlist=list(ordered.attribute=s, total.auc=ss, item=names(s1), summary=tab, plot.total.auc(ss,s))}
else
outlist=list(ordered.attribute=s, total.auc=ss, item=names(s1), summary=tab)}
class(outlist) = "totalAuc"
return(outlist)
}
