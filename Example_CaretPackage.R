#SPAM Example: 

#  1  Data splitting

library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

#  1A - K-fold - can use this to sub-set your training data
set.seed(32323)
folds <- createFolds(y=spam$type,k=10,
                     list=TRUE,returnTrain=TRUE)
sapply(folds,length)
folds[[1]][1:10]


#  1B - Resampling (with replacement!)

set.seed(32323)
folds <- createResample(y=spam$type,times=10,
                        list=TRUE)
sapply(folds,length)
folds[[1]][1:10]

#  1C - Data slicing with Time series (want to use continuous chuncks of time)
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y=tme,initialWindow=20,
                          horizon=10)
names(folds)
folds$train[[1]]
folds$test[[1]]

#

#  2  Model fit

set.seed(32343)
modelFit <- train(type ~.,data=training, method="glm")
modelFit

#  2A Training Options

#you can use a large number of options for training
args(train.default)

#here are some of them:

#preProcess
#A parameter to set a bunch of preprocessing options. 

#weights
# e.g. you can upweight or downweight certain observations. 
#These are particularly useful if you have very unbalanced training set where you have a 
#lot more examples of one type than another. 

#metric
# so by default for factor variable, in other words for categorical variables 
#the default metric is accuracy ("Accuracy") that it's trying to maximize. 
#For continuous variables it's the root mean squared error ("RMSE").
# other options include "Rsquared" for continuous variables and 
#"Kappa" for categorical variables (Kappa = a measure of concordance - used in Kaggle comps).

#trControl
#You can also set a large number of other control parameters using this trControl parameter
#you have to pass it a call to this particular function, trainControl.



function (x, y, method = "rf", preProcess = NULL, ..., weights = NULL, 
          metric = ifelse(is.factor(y), "Accuracy", "RMSE"), maximize = ifelse(metric == 
                 "RMSE", FALSE, TRUE), trControl = trainControl(), tuneGrid = NULL, 
          tuneLength = 3) 
        NULL

# 2 B Train Control

# trainControl argument allows you to be much more precise about the way that you train models. 
# You can tell it which method to use for resampling the data whether it's bootstrapping or cross validation 
# You can tell it the number of times to do bootstrapping or cross validation. 
# You could also tell it how many times to repeat that whole process if you want to be careful 
# about repeated cross-validation. 
# You can tell it the size of the training set with this p parameter, 
# and then you can tell it a bunch of other parameters that depend on the specific problems 
# you're working on. 

# So for example, for time course data: 
# initial window 
# tells you the size of the training dataset, 
# the size of the number of time points that will be in the training data. 

# horizon 
# is the number of time points that you'll be predicting.

# savePredictions
# returns the actual predictions themselves from each of the iterations when it's building the model. 

#summaryFunction
#You can also have it return a different summary function than the default summary 

#preProcOptions 
#set preprocessing options 

#predictionBounds
# sets prediction bounds

#seeds
# set the seeds for all the different resampling layers. 
# This is particularly useful if you're going to be parallelizing your computations across 
# multiple cores. 

# allowParallel
#If you're training models on large numbers of samples with a high number 
#of predictors, using parallel processing can be highly useful for proving the computational efficiency of your analysis. 

# For resampling, there are a bunch of methods that are offered, 
#so this is again passed to the trainControl function. 

# boot = bootstrapping
# uses standard bootstrapping,

# boot632 = bootstrapping with adjustment
# uses bootstrapping that adjusts for the fact that multiple samples are repeatedly resampled
# when you're doing that subsampling. 
# This will reduce some of the bias due to bootstrapping. 

# cv = cross validation
# use cross validation . 


# repeatedcv = repeated cross validation
# You could also use repeated cross validation if you want to do sub cross validation 
# with different random draws.

# LOOCV = leave one out cross validation
# You could use leave one out cross validation and remember there's a bias during 
# its trade off between using large number of folds and smaller number of folds when doing cross validation. 


# You can also tell it the number of bootstrap samples or the number of subsamples to take:
# For boot/cross validation
# Number of subsamples to take

# and 

# the number of times to repeat that subsampling if you're doing something like repeated 
#cross validation. 
# If big this can slow things down - if you have large numbers of samples or 
#you have a model that requires fine tuning across a large number of parameters, 
#you may want to increase for example the number of cross-validation or bootstrap samples that you take. 


args(trainControl)
function (method = "boot", number = ifelse(method %in% c("cv", 
                                 "repeatedcv"), 10, 25), repeats = ifelse(method %in% c("cv", 
                                       "repeatedcv"), 1, number), p = 0.75, initialWindow = NULL, 
          horizon = 1, fixedWindow = TRUE, verboseIter = FALSE, returnData = TRUE, 
          returnResamp = "final", savePredictions = FALSE, classProbs = FALSE, 
          summaryFunction = defaultSummary, selectionFunction = "best", 
          custom = NULL, preProcOptions = list(thresh = 0.95, ICAcomp = 3, 
                                               k = 5), index = NULL, indexOut = NULL, timingSamps = 0, 
          predictionBounds = rep(FALSE, 2), seeds = NA, allowParallel = TRUE) 
        NULL


# 2 C Setting the seed

# It's very useful often to set the overall seed. 
# This is a seed for the entire procedure so you get repeatable results. 
# If you're doing parallel computation you can also set the seed for each resample. 
# You can do that with a seed argument to the train control function. 
# Seeding each resamples is particularly useful for parallel fits but it's often not
# necessary when you're doing all your processing that isn't parallel.

#example  - set the seed using the set.seed function in R, 
# give it a number, an integer number, it will set a seed that's consistent with performance analysis. 
# i.e. it will generate a set of random numbers that is consistent. 
# Then when it generates bootstrap samples. It will generate those bootstrap samples according 
# to the random numbers that come from this seed. 


set.seed(1235)
modelFit2 <- train(type ~.,data=training, method="glm")
modelFit2

set.seed(1235)
modelFit3 <- train(type ~.,data=training, method="glm")
modelFit3

#  3 Final Model

modelFit <- train(type ~.,data=training, method="glm")
modelFit$finalModel

#  4 Prediction

predictions <- predict(modelFit,newdata=testing)
predictions

#  5 Confusion Matrix

confusionMatrix(predictions,testing$type)