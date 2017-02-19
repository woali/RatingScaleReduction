rsr <-function(attribute,D, plotRSR=FALSE){
if (length(names(attribute))==0){
outlist=list(message('****names(attribute)==NULL!!!! Create attribute LABELS (e.g. using colnames)'))}

else {
start.auc <-vector(length=ncol(attribute))
for (i in 1:(ncol(attribute))) {
start.auc[i] <-roc(D, attribute[,i], plotROC=FALSE)$auc}

mydata1 <-rbind(start.auc,attribute[,1:(ncol(attribute))])
s1 <-mydata1[,order(mydata1[1,],  decreasing = TRUE)]
s <-s1[2:nrow(s1),]

ss <-c()
ss[1]<-start.auc[1]	
for (i in 2:ncol(s)){
ss[i] <-roc(D, rowSums(s[,1:i]), plotROC=FALSE)$auc
 }

auc1 <-c()
i=1
while (ss[i+1]>ss[i]){
auc1[i] <-ss[i]
i=i+1
}

auc.reduct <-c(auc1, ss[i])

if (length(auc.reduct)>1){
tab <-t(rbind(s1[1,1:length(auc.reduct)],auc.reduct))
colnames(tab) <-c('AUC one variable', 'AUC running total')}
else {
tab <-data.frame(names(s1)[1],auc.reduct)
colnames(tab) <-c('Attribut', 'AUC running total')}

if (plotRSR==TRUE){
	if (length(auc.reduct)==1){
	y=s[,1]
	p <-plot.roc(D, y, plotROC=TRUE)
	outlist=list(rsr.auc=auc.reduct, rsr.label=names(s1)[1], summary=tab, p)}
		else {
		y=rowSums(s[,1:i])
		p <-plot.roc(D, y, print.auc=TRUE, main=" ROC curve of AUC of the reduced rating scale")
		outlist=list(rsr.auc=auc.reduct, rsr.label=names(s1[1,1:length(auc.reduct)]), summary=tab, p)}}

else
outlist=list(rsr.auc=auc.reduct, rsr.label=names(s1)[1], summary=tab)}

class(outlist) = "RatingScaleReduction"

message("The criteria: Stop first MAX AUC")
return(outlist)	
}
