#####code by Alicja Wolny-Dominiak
#####woali@ue.katowice.pl

library(RatingScaleReduction)

###########################################
###########################################
#First Example 

mydata=read.csv(file="http://web.ue.katowice.pl/woali/BDI.csv")
attach(mydata)

attribute=mydata[,1:21]
D=mydata[,22]
rauc.bdi <-totalAuc(attribute, D, plotT=TRUE)

totalAuc(attribute, D)
diffExamples(attribute)
startAuc(attribute, D)

rsr.bdi <-rsr(attribute, D, plotRSR=TRUE)

###Graded Response Model
library(ltm)
grm1 <-grm(attribute)
grm1.rsr <-grm(data.frame(BDI_1,BDI_7,BDI_9,BDI_10,BDI_14,BDI_15))

###CFA
library(lavaan)
library(semPlot)

model1<-'D=~BDI_1+BDI_2+BDI_3+BDI_4+BDI_5+BDI_6+BDI_7+BDI_8+BDI_9+BDI_10+BDI_11+BDI_12+BDI_13+BDI_14+BDI_15+BDI_16+BDI_17+BDI_18+BDI_19+BDI_20+BDI_21'
cfa1<-cfa(model1, data=mydata, ordered=c("BDI_1","BDI_2", "BDI_3","BDI_4","BDI_5","BDI_6","BDI_7","BDI_8",
"BDI_9","BDI_10","BDI_11","BDI_12","BDI_13","BDI_14","BDI_15","BDI_16","BDI_17","BDI_18",
"BDI_19","BDI_20","BDI_21"))
summary(cfa1, fit.measure=TRUE,standardize=TRUE)

model1.rsr<-'D=~BDI_1+BDI_7+BDI_9+BDI_10+BDI_14+BDI_15'
cfa1.rsr<-cfa(model1.rsr, data=mydata, ordered=c("BDI_1","BDI_7", "BDI_9","BDI_10","BDI_14","BDI_15"))
summary(cfa1.rsr, fit.measure=TRUE,standardize=TRUE)

#layout(t(1:2))
semPaths(cfa1, "est", title = FALSE, intercepts = FALSE,  sizeMan=6, edge.label.cex=1.3, edge.color=4,  label.cex=1.5)
title("BDI depresion full scale", line = 1)

semPaths(cfa1.rsr, "est", title = FALSE, intercepts = FALSE,  sizeMan=9, edge.label.cex=1.7, edge.color=4,  label.cex=1.7)
title("BDI depresion reduced scale", line = 1)

##########################################
##########################################
#Second Example 
mydata=read.csv2(file="http://web.ue.katowice.pl/woali/data312.csv")
attach(mydata)

for (i in 1:ncol(mydata)){
mydata[,i] <-as.numeric(mydata[,i])
}

att <-mydata[,2:ncol(mydata)]
d3 <-match("hepato",names(att))

a3 <-totalAuc(att[,-d3], hepato, plotT=TRUE)
a3$summary

r3 <-rsr(att[,-d3], hepato, plotRSR=TRUE)
r3$summary

#Rfig3
rr <-rowSums(a3$ordered.attribute[,1:2])

par(mfrow=c(1,2))
plot(a3$total.auc, xaxt="n", type='l', ylab="Total AUC", xlab="Attribute label", lwd=2, main="Total AUC", 
cex.main=1.3, cex.lab=1.3, cex.axis=1.3)
points(which.max(a3$total.auc),max(a3$total.auc), col='red', lwd=2,cex=2, cex.main=1.3)
abline(v=which.max(a3$total.auc), col='blue', lty=2, lwd=2)
mtext(c(a3$item[1:2],3:18),at=1:length(a3$item),side=1, cex=1.2)
plot.roc(hepato, rr, plotROC=TRUE, print.auc=TRUE, main=" ROC curve of AUC of the reduced rating scale",
cex.main=1.3, cex.lab=1.3, cex.axis=1.3)

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

###Graded Response Model
grm2 <-grm(att[,-d3])
grm22 <-grm(att[,-d3], constrained=TRUE)

