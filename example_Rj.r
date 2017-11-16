#####code by Alicja Wolny-Dominiak, Feng Li
#####woali@ue.katowice.pl

library(RatingScaleReduction)

###########################################
###########################################
#First Example 

mydata = read.csv(file="http://web.ue.katowice.pl/woali/BDI.csv")
attach(mydata)

attribute = mydata[,1:21]
D = mydata[,22]
tauc.bdi <- totalAuc(attribute, D, plotT = TRUE)

tauc.bdi$summary
tauc.bdi$item

rsr.bdi <- rsr(attribute, D, plotRSR = TRUE)
rsr.bdi

##Graded Response Model (GRM)
library(ltm)
grm1 <- grm(attribute)
grm1.rsr <- grm(data.frame(BDI_1,BDI_7,BDI_9,BDI_10,BDI_14,BDI_15))


###CFA
library(lavaan)
library(semPlot)

model1 <- 'D=~BDI_1+BDI_2+BDI_3+BDI_4+BDI_5+BDI_6+BDI_7+BDI_8+BDI_9+BDI_10+BDI_11+BDI_12+BDI_13+BDI_14+BDI_15+BDI_16+BDI_17+BDI_18+BDI_19+BDI_20+BDI_21'
cfa1 <- cfa(model1, data=mydata, ordered=c("BDI_1","BDI_2", "BDI_3","BDI_4","BDI_5","BDI_6","BDI_7","BDI_8",
"BDI_9","BDI_10","BDI_11","BDI_12","BDI_13","BDI_14","BDI_15","BDI_16","BDI_17","BDI_18",
"BDI_19","BDI_20","BDI_21"))
summary(cfa1, fit.measure=TRUE,standardize=TRUE)

model1.rsr <- 'D=~BDI_1+BDI_7+BDI_9+BDI_10+BDI_14+BDI_15'
cfa1.rsr <- cfa(model1.rsr, data=mydata, ordered=c("BDI_1","BDI_7", "BDI_9","BDI_10","BDI_14","BDI_15"))
summary(cfa1.rsr, fit.measure=TRUE,standardize=TRUE)

#layout(t(1:2))
semPaths(cfa1, "est", title = FALSE, intercepts = FALSE,  sizeMan=6, edge.label.cex=1.3, edge.color=4,  label.cex=1.5)
title("BDI depresion full scale", line = 1)

semPaths(cfa1.rsr, "est", title = FALSE, intercepts = FALSE,  sizeMan=9, edge.label.cex=1.7, edge.color=4,  label.cex=1.7)
title("BDI depresion reduced scale", line = 1)

##########################################
##########################################
#Second Example 

##Data manipulation
mydata_red = read.csv2(file = "http://web.ue.katowice.pl/woali/winequality-red.csv")
mydata_white = read.csv2(file = "http://web.ue.katowice.pl/woali/winequality-white.csv")

mydata = rbind(mydata_red,mydata_white)
mydata$decision = 0

for (i in 1:nrow(mydata))
{
  if (mydata$quality[i] > 5)
    mydata$decision[i] = 1
}

mydata = mydata[,-12]

attach(mydata)

for (i in 1:ncol(mydata)){
  mydata[,i] <- as.numeric(mydata[,i])
}

##RSR
attribute <- mydata[,1:11]
D <- mydata[,12]
colnames(attribute) <- c("FA","VA","CA","RS","Ch","FSD","TSD","De","pH","Su","Al")

rauc.wine <- totalAuc(attribute, D, plotT=TRUE)

rauc.wine$summary
rauc.wine$item

rsr(attribute, D,plotRSR=TRUE)
diffExamples(attribute)
startAuc(attribute, D)

rsr.wine <- rsr(attribute, D, plotRSR=TRUE)

rsr.wine$rsr.auc
rsr.wine$rsr.label
rsr.wine$summary



