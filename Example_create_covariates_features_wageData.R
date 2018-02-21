#Creating covariates/features from the data

#Want to take that raw data and turn it into features or covariates which are variables that 
#describe the data as much as possible while giving some compression and making it easier to fit 
#standard machine-learning algorithms.

#Building features can only happen in the training set!!


#Load example Wage data

library(ISLR); library(caret); data(Wage);
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]


#Basic idea - convert factor variables to indicator variables
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass,data=training)
head(predict(dummies,newdata=training))

#Removing zero covariates
nsv <- nearZeroVar(training,saveMetrics=TRUE)
nsv

#Spline basis
library(splines)
bsBasis <- bs(training$age,df=3) 
bsBasis
#See also: ns(),poly()

#Fitting curves with splines
lm1 <- lm(wage ~ bsBasis,data=training)
plot(training$age,training$wage,pch=19,cex=0.5)
points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=0.5)

#Splines on the test set
predict(bsBasis,age=testing$age)
