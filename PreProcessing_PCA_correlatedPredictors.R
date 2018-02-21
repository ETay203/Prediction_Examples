#Correlated predictors

#preprocessing covariants with principal components analysis. 
#The idea is that often you have multiple quantitative variables and sometimes 
#they'll be highly correlated with each other

library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

#The first thing - leave out just the 58th column of this training set. 
#Which in this case is the outcome. So we look at all the other predictor variables. 
#calculate the correlation between all those columns. 
#The correlation between all predictor variables. And take its absolute value. 
M <- abs(cor(training[,-58]))

#remove variables correlated with themselves (i.e. the diagonal of the matrix) - set these to zero
diag(M) <- 0

#Look for all the predictor variables that that have a very high correlation or are very similar 
#to each other. here we look at values in the matrix greater than 0.8 (i.e. highly correlated)
which(M > 0.8,arr.ind=T)


#Need to figure out a combination of these variables that explain any variability. 
#How?

#We could rotate the plot

#e.g take 0.71 times the 415 variable plus 0.71 times 857 variable. 
#And create a new variable called x. Which is basically the sum of those two variables. 
X <- 0.71*training$num415 + 0.71*training$num857
#Then take the difference of those two variables. 
#0.71 times 415 minus 0.71 times 857. 
Y <- 0.71*training$num415 - 0.71*training$num857
#i.e. x is adding the two variables together, y is subtracting the two variables.

plot(X,Y)

#Find a new set of multivariate variables that are uncorrelated and
#explain as much variance as possible.

#If you put all the variables together in one matrix, find the best matrix created with
#fewer variables (lower rank) that explains the original data.
#The first goal is statistical and the second goal is data compression.

#can acheive the same result as above using caret
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])
#can quantify the rotation using (in fact = how we chose the coefficients used in manual calc above)
prComp$rotation

#prcomp - useful for reducing multiple components to a single variable - not possible manually
#can add colour for spam and non spam
typeColor <- ((spam$type=="spam")*1 + 1)

#calculate principal components on the entire dataset - performed a log transform and added 1
#log transform (log10) makes the data more gaussian looking.. some variables are very skewed
prComp <- prcomp(log10(spam[,-58]+1))

#now again plot principal component one, versus principal component two. 
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")
#Principle component one is no longer a very easy addition of two variables. 
#It might be some quite complicated combination of all the variables in the data set. 
#But it's the combination that explains the most variation in the data. 
#Principle component two is the combination that explains the second most variation. 
#And principal component three explains the third most and so forth. 
#So if I plot principal component one. That's just a variable that I've calculated. 
#Versus variable principal component two that's another variable that I've calculated. 
#Then I color them by the spam indicator. So, each of these points 
#corresponds to a single observation. The red ones correspond to spam observations 
#and the black ones just ham observations. 


#Can do the same using preProcess function in the Caret package  
#use method = "pca"  and the number of principal components to compute, pcaComp = 2
preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
spamPC <- predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)


#We can dopreprocessing with the method PCA, using the preProcess function as before
#Then You can create training predictions by using the predict function. 


preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC <- predict(preProc,log10(training[,-58]+1))

#And then, fitting a model that relates the training variable to the principal component. 
#So here I haven't used the full training set as the data for fitting my model. 
#I've just used the principal components for the model fitting.


modelFit <- train(training$type ~ .,method="glm",data=trainPC)

#In the test data set you have to use the same principal component that you calculated in the
#trained data set for the test variables. 
#So the idea here is we again pass at the pre-process object that we calculated in the training set.
#But now we pass it the new testing data. 
#So this predict function is going to take the principle components we calculated from training.
#And get the new values for the test data set on those same principle components. 

testPC <- predict(preProc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))

# Alternative (sets # of PCs)
#the other thing that you can do is you can actually decide for this analysis to 
#not use the predict function separately. 
#You can build it right into your training exercise. 
#So if you take the train function from the caret package, and you pass it a training set.
#But you tell it to pre process with principal component analysis.
#It will do that pre-processing as part of the training process. 
#And then when you do the prediction on the new data set you just pass it a testing data set 
#and it will actually calculate the PC's for you. 


             modelFit <- train(training$type ~ .,method="glm",preProcess="pca",data=training)
             confusionMatrix(testing$type,predict(modelFit,testing))
             
             # Final thoughts on PCs
             #  - Most useful for linear-type models
             #  - Can make it harder to interpret predictors
             #  - Watch out for outliers!
             #         Transform first (with logs/Box Cox)
             #         Plot predictors to identify problems            