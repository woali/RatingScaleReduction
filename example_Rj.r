#####code by Alicja Wolny-Dominiak, Feng Li
#####woali@ue.katowice.pl
####Date: march 2018

library(RatingScaleReduction)
library(pROC)

###########################################
###########################################
#First Example 

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

attribute <- mydata[,1:11]
D <- mydata[,12]

rauc.wine <- totalAuc(attribute, D, plotT=TRUE)
rauc.wine$summary
rauc.wine$item

colnames(attribute) <- c("FA","VA","CA","RS","Ch","FSD","TSD","De","pH","Su","Al")

##RSR
diffExamples(attribute)

rsr.wine <- rsr(attribute, D, plotRSR=TRUE)

rsr.wine$rsr.auc
rsr.wine$rsr.label
rsr.wine$summary


######################################
######################################
#Second Example 
library(RatingScaleReduction)
library(pROC)
library(DEoptim)
set.seed(1234)

mydata = read.csv2(file="http://web.ue.katowice.pl/woali/SHS_D8.csv")

attribute = mydata[ ,1:6]
D = mydata[ ,7]

##Testing rating scale 

##classifier - sum of attributes
D.predict <- rowSums(attribute)
(aucResult <- roc(D, D.predict, plotROC = FALSE)$auc)

tot <-totalAuc(attribute, D, plotT = TRUE)
rsrSum <- rsr(attribute, D, plotRSR = TRUE)


##classifier - DEvol

## nsi function has a global minimum inconsistency index 
## Note that the vector of parameters to be optimized must be the first 
## argument of the objective function passed to DEoptim.
nsi1<-function(x){
  
  D.predict <- rowSums(x*mydata)
  -1*roc(D, D.predict, plotROC = FALSE)$auc
}


## DEoptim searches for minima of the objective function between
## lower and upper bounds on each parameter to be optimized. Therefore
## in the call to DEoptim we specify vectors that comprise the
## lower and upper bounds; these vectors are the same length as the
## parameter vector.
lower_nsi <- c(0.1,0.1, 0.1, 0.1, 0.1,0.1)
upper_nsi <- c(3,3,3,3,3,3)

output.all <- DEoptim(nsi1, lower_nsi, upper_nsi, DEoptim.control(itermax=10))

#ouput the optimize result
output.all$optim

#ouput the optimize result of vector
(weight.item.all <- output.all$optim$bestmem)

#all items
(aucResult.all <- -1*output.all$optim$bestval)


## RSR
start.auc <-vector(length=ncol(attribute))
for (i in 1:(ncol(attribute))) {
  start.auc[i] <- roc(D, attribute[,i], plotROC=FALSE)$auc
  }

mydata1 <- rbind(start.auc,attribute[,1:(ncol(attribute))])
s1 <- mydata1[,order(mydata1[1,],  decreasing = TRUE)]
attMs <- s1[2:nrow(s1),]

#DE
results <- matrix(nrow = ncol(attMs), ncol = 2)
results[1, ] <- cbind(1, sort(start.auc)[1])
weight1 <- NULL

for (i in 2:ncol(attMs)) {
  mydata <- attMs[ ,1:i]
  
  lower_nsi <- rep(0.1,i)
  upper_nsi <- rep(3,i)
  
  output <- DEoptim(nsi1, lower_nsi, upper_nsi, DEoptim.control(itermax=10))
  results[i, ] <- cbind(i, -1*output$optim$bestval)
  weight <- output$optim$bestmem
  weight1 <- list(weight1, w = weight)
  
}
colnames(results) <- c("number of items", 'AUCtotalDE')
results
names(attMs)

#Fig.
plot(results[ , 2], xaxt="n", type='l', ylab="AUC of item subset", xlab="Attribute number or identification", lwd=2)
points(which.max(results[ , 2]),max(results[-1, 2]), 
       col='orange', lwd=2)
abline(v=which.max(results[ , 2]), col='green', lty=2, lwd=2)
labele = c(names(attMs))
mtext(labele,at=1:ncol(attMs),side=1)


#Fig. 
w.opt <- weight1[[1]]$w
mydata <- attMs[ ,1:5]
nsi1(w.opt)
D.predictDE <- rowSums(w.opt*attMs[ ,1:5])
D.predictSum <-  rowSums(attMs[ ,1:5])

(p <-roc(D, D.predictDE, plotROC = FALSE))
x=1-p$specificities
y=p$sensitivities

p1 <-roc(D, D.predictSum)
x1=1-p1$specificities
y1=p1$sensitivities
  
plot(x1, y1, type = 'l', xlab="1-specificities", 
     ylab = 'sensitivities')
lines(x, y, col = 'red')

#polygon(x, y, col = 'green')
#polygon(x1, y1, col = 'red')

abline(0, 1) 
legend("bottomright", legend = list("Classifier - SUm", "Classifier - DE"), col = 1:2, lty = 1)

#save.image('examples.RData')
