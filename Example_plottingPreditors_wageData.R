library(ISLR); library(ggplot2); library(caret);
data(Wage)
summary(Wage)

# split data
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)

#data exploration - REMEMBER ONLY USE TRAINING SET!

# feature plot (caret package)
featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")

#more typical qplots (ggplot2 package)
qplot(age,wage,data=training)
qplot(age,wage,colour=jobclass,data=training)

#add regression smoothers
qq <- qplot(age,wage,colour=education,data=training)
qq +  geom_smooth(method='lm',formula=y~x)

#cut2, making factors (Hmisc package)
cutWage <- cut2(training$wage,g=3)
table(cutWage)
cutWage

#plots with cut2
p1 <- qplot(cutWage,age, data=training,fill=cutWage,
            geom=c("boxplot"))
p1

#boxplots with cut2 - points overlaid

p2 <- qplot(cutWage,age, data=training,fill=cutWage,
            geom=c("boxplot","jitter"))
grid.arrange(p1,p2,ncol=2)

#tables
t1 <- table(cutWage,training$jobclass)
t1

prop.table(t1,1)

#Density plots
qplot(wage,colour=education,data=training,geom="density")