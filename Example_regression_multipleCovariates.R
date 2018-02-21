#Predicting using regression and MULTIPLE covariates
#again uses the Wage data, comes with ISLR package..

library(ISLR); library(ggplot2); library(caret);
data(Wage); Wage <- subset(Wage,select=-c(logwage))
summary(Wage)

#exploratory analysis - first sub set data into training & test data
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
dim(training); dim(testing)

# use a feature plot to get a quick idea of different trends

featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")

#Plot age versus wage
qplot(age,wage,data=training)

#appears to be a curved trend, but also a set of outliers.. might be able to predict the outliers
#but need to figure out what is causing them. 
#one approach is to plot age vs. wage again but then colour by a third variable (e.g. jobclass or education)

qplot(age,wage,colour=jobclass,data=training)
qplot(age,wage,colour=education,data=training)

#fit a linear model
modFit<- train(wage ~ age + jobclass + education,
               method = "lm",data=training)
finMod <- modFit$finalModel
print(modFit)

#diagnostics
plot(finMod,1,pch=19,cex=0.5,col="#00000010")

#Color by variables not used in the model
qplot(finMod$fitted,finMod$residuals,colour=race,data=training)

#Plot by index
plot(finMod$residuals,pch=19)

#Predicted versus truth in test set
pred <- predict(modFit, testing)
qplot(wage,pred,colour=year,data=testing)

#If you want to use all covariates
modFitAll<- train(wage ~ .,data=training,method="lm")
pred <- predict(modFitAll, testing)
qplot(wage,pred,data=testing)


