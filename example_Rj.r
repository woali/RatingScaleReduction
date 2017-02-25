#####code by woali@ue.katowice.pl
#### 19/02/2017

library(RatingScaleReduction)

###########################################
#Example Depresion BDI

mydata=read.csv(file="BDI.csv")
attach(mydata)

attribute=mydata[,1:21]
D=mydata[,22]
rauc.bdi <-totalAuc(attribute, D, plotT=TRUE)

totalAuc(attribute, D)
diffExamples(attribute)
startAuc(attribute, D)

rsr.bdi <-rsr(attribute, D, plotRSR=TRUE)

##########################################
#Example data312
mydata=read.csv2(file="http://web.ue.katowice.pl/woali/data312.csv")
attach(mydata)

for (i in 1:ncol(mydata)){
mydata[,i] <-as.numeric(mydata[,i])
}

att <-mydata[,2:ncol(mydata)]
d3 <-match("hepato",names(att))
names(att[,-d3])

diffExamples(att[,-d3], hepato)

a3 <-totalAuc(att[,-d3], hepato, plotT=TRUE)
a3$summary

r3 <-rsr(att[,-d3], hepato, plotRSR=TRUE)
r3$summary

#Rfig3
rr <-rowSums(a3$ordered.attribute[,1:2])
par(mfrow=c(1,2))
plot(a3$total.auc, xaxt="n", type='l', ylab="Total AUC", xlab="Attribute label", lwd=2, main="Total AUC")
points(which.max(a3$total.auc),max(a3$total.auc), col='red', lwd=2)
abline(v=which.max(a3$total.auc), col='blue', lty=2, lwd=2)
mtext(c(a3$item[1:2],3:18),at=1:length(a3$item),side=1)
plot.roc(hepato, rr, plotROC=TRUE, print.auc=TRUE, main=" ROC curve of AUC of the reduced rating scale")

#Test deLong, bootsrap
ch1 <-CheckAttr4Inclusion(att[,-d3], hepato, method=c("delong"), alternative=c("two.side"))
ch2 <-CheckAttr4Inclusion(att[,-d3], hepato, method=c("bootstrap"), alternative=c("two.side"))
ch1
ch2

#Rfig4
x=1-ch1$roc1$specificities
y=ch1$roc1$sensitivities

x1=1-ch1$roc2$specificities
y1=ch1$roc2$sensitivities

plot(x,y, type='l', lwd=2)
lines(x1,y1, type='l', lwd=2, col='red')
legend("bottomright", lwd=c(1,2), col=c('black','red'), c(paste('AUC rating scale reduced (stage, bili)=',round(ch1$roc1$auc,3)), 
paste("AUC rating scale (stage, bili, albumin)",round(a3$total.auc[3],3))))
abline(0,1,lwd=2, col='gray')

#Test summary
summ <-rbind(cbind(ch1$statistic, ch1$p.value), cbind(ch2$statistic, ch2$p.value))
colnames(summ) <-c("Z statistics", "p-value")
rownames(summ) <-c("DeLong", "bootstrap")
summ

##list of gray examples
sex <-as.numeric(sex)

gray.ex <-c()
df1 <-unique(data.frame(hepato, status, sex, spiders, stage))

for (i in 1:nrow(df1)){
ex <-grayExamplesN(df1[,2:ncol(df1)], df1[,1], i)$examp
if (nrow(ex)>1){
gray.ex <-rbind(gray.ex, ex[1,])}
}
colnames(gray.ex) <-names(df1)
gray.ex

#all pairs of gray examples
grayExamples(att, D)

detach(data.ins)

