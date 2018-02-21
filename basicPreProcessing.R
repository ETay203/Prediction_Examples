#Why preprocess
#as with any data analyis might want to transform data in order to make them more useful for
#prediction algorithms - particularly true when using model based algorithms


library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve,main="",xlab="ave. capital run length")


#in this example data is very skewed - preprocessing is helpful. 
#here we see there is large sd about the mean.
mean(training$capitalAve)

sd(training$capitalAve)


#standardising
trainCapAve <- training$capitalAve
#subtract mean and divide by the sd
trainCapAveS <- (trainCapAve  - mean(trainCapAve))/sd(trainCapAve) 
mean(trainCapAveS)
sd(trainCapAveS)

#standardising - test set
#have to only use parameters that we estimated in the training set. 
#In other words, when we apply this same standardization to the test set,
#we have to use the mean from the training set, and the standard deviation from the training set,

testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve  - mean(trainCapAve))/sd(trainCapAve) 

#What does this mean? - the mean will not be exactly zero in the test set. 
# And the standard deviation will not be exactly one, 
#because we've standardized by parameters estimated in the training set, 
#but hopefully they'll be close to those values even though we're using not the exact values
# built in the test set. 
mean(testCapAveS)
sd(testCapAveS)


#Standardizing - preProcess function
#You can also use the preProcess function to do a lot of standardization for you. 
#peprocess function is a function that is built into the caret package. 
preObj <- preProcess(training[,-58],method=c("center","scale"))
# And here I'm passing it all of the training variables except for the 58th in the data set, 
# which is the actual outcome that we care about. 
# And I'm telling it to center every variable and scale every variable.


# You can also pass the preprocessed commands directly to the train function in caret, as an argument.
# So, for example here we can send to the preprocessed argument of the train function, the command, 
# the parameters center and scale, and that will center and scale all of the predictors, 
# before using those predictors in the prediction model
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)
#preObj now stores the standardisation by parameters estimated in the TRAINING set
#use preObj to standardise the test set.
testCapAveS <- predict(preObj,testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

#preProcess Arguments

set.seed(32343)
modelFit <- train(type ~.,data=training,
                  preProcess=c("center","scale"),method="glm")
modelFit

#Alternative transformation: Box-Cox transforms
preObj <- preProcess(training[,-58],method=c("BoxCox"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)

#Imputing data

#how can we handle missing values?
#do k-nearest neighbors imputation. 


set.seed(13343)

# Make some values NA - for this example
# You can generate randomly a bunch of values using the rbinom function to set equal to NA 
# and  set those values to be missing. 
# So, now this variable capAve is exactly like the capitalAve valuable only 
# it has a subset of values that are missing. 

training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA] <- NA

# Impute and standardize
#K-nearest neighbors computation find the k (So if k equal to ten, then the 10 nearest) data vectors
#that look most like data vector with the missing value, and average the values of the variable
#that's missing and compute them at that position. 
preObj <- preProcess(training[,-58],method="knnImpute")

#then we can predict on our training set, all of the new values, including the ones that have been 
#imputed with the k-nearest imputation algorithm. 
capAve <- predict(preObj,training[,-58])$capAve

# Standardize true values
#We can then standardize those values, using the same standardization procedure that we did before, 
#by subtracting the mean and divided by the standard dev, deviation. 
capAvTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)


#Imputing data
quantile(capAve - capAveTruth)
quantile((capAve - capAveTruth)[selectNA])
quantile((capAve - capAveTruth)[!selectNA])