#SPAM prediction example
library(kernlab)
data(spam)
plot(density(spam$your[spam$type=="nonspam"]),
     col="blue",main="",xlab="Frequency of 'your'")
lines(density(spam$your[spam$type=="spam"]),col="red")
plot(density(spam$your[spam$type=="nonspam"]),
     col="blue",main="",xlab="Frequency of 'your'")
lines(density(spam$your[spam$type=="spam"]),col="red")
abline(v=0.5,col="black")
prediction <- ifelse(spam$your > 0.5,"spam","nonspam")
table(prediction,spam$type)/length(spam$type)

#In sample vs. out of sample error examples
library(kernlab); data(spam); set.seed(333)
smallSpam <- spam[sample(dim(spam)[1],size=10),]
spamLabel <- (smallSpam$type=="spam")*1 + 1
plot(smallSpam$capitalAve,col=spamLabel)
#Prediction Rule 1
rule1 <- function(x){
        prediction <- rep(NA,length(x))
        prediction[x > 2.7] <- "spam"
        prediction[x < 2.40] <- "nonspam"
        prediction[(x >= 2.40 & x <= 2.45)] <- "spam"
        prediction[(x > 2.45 & x <= 2.70)] <- "nonspam"
        return(prediction)
}
table(rule1(smallSpam$capitalAve),smallSpam$type)
#Predition Rule 2
rule2 <- function(x){
        prediction <- rep(NA,length(x))
        prediction[x > 2.8] <- "spam"
        prediction[x <= 2.8] <- "nonspam"
        return(prediction)
}
table(rule2(smallSpam$capitalAve),smallSpam$type)
#Apply to all spam data
table(rule1(spam$capitalAve),spam$type)
table(rule2(spam$capitalAve),spam$type)
mean(rule1(spam$capitalAve)==spam$type)
sum(rule1(spam$capitalAve)==spam$type)
sum(rule2(spam$capitalAve)==spam$type)
#Rule 2 was correct slightly more often (rule one looks like) it would be
#better, but rule 1 OVERFIT the data. (Tuned it too closely to the small 
#training set)

#Quiz 1 in Machine Learning Course
#Question 1: Which of the following are components in building a machine learning algorithm?
#Answer: Asking the right question
#Question 2: Suppose we build a prediction algorithm on a data set and it is
#100% accurate on that data set. Why might the algorithm not work well if we collect a new data set?
#Answer: Our algorithm may be overfitting the training data, predicting both the signal and the noise.
#Question 3: What are typical sizes for the training and test sets?
#Answer:60% in the training set, 40% in the testing set.
#Question 4: What are some common error rates for predicting binary variables
#(i.e. variables with two possible values like yes/no, disease/normal, clicked/didn't click)? Check the correct answer(s).
#ANswer:Sensitivity
#Question 5:Suppose that we have created a machine learning algorithm that
#predicts whether a link will be clicked with 99% sensitivity and 99%
#specificity. The rate the link is clicked is 1/1000 of visits to a website. 
#If we predict the link will be clicked on a specific visit, what is the probability it will actually be clicked?
###### Calculating probability of true positive 
###### Sensitivity is: true positive / (true positive plus false negative)
#Answer: 9% (See notebook)



#Spam Example: Data Splitting
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)
#Fit model
set.seed(32343)
#install.packages('e1071', dependencies=TRUE)
modelFit <- train(type ~.,data=training, method="glm")
modelFit
#Final Model
modelFit <- train(type ~.,data=training, method="glm")
modelFit$finalModel
#Prediction
predictions <- predict(modelFit,newdata=testing)
predictions
#Confusion Matrix
confusionMatrix(predictions,testing$type)


#SPAM Example: K-fold
set.seed(32323)
folds <- createFolds(y=spam$type,k=10,
                     list=TRUE,returnTrain=TRUE)
sapply(folds,length)
folds[[1]][1:10]

#SPAM Example: Return test
set.seed(32323)
folds <- createFolds(y=spam$type,k=10,
                     list=TRUE,returnTrain=FALSE)
sapply(folds,length)
folds[[1]][1:10]

#SPAM Example: Resampling
set.seed(32323)
folds <- createResample(y=spam$type,times=10,
                        list=TRUE)
sapply(folds,length)
folds[[1]][1:10] #Note you are sampling with replacement so you might get the same data point more than once

#SPAM Example: Time Slices
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y=tme,initialWindow=20,
                          horizon=10)
names(folds)
folds$train[[1]]
folds$test[[1]]

#SPAM Example
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
modelFit <- train(type ~.,data=training, method="glm")

#SPAM Example: training options
args(train.default)
function (x, y, method = "rf", preProcess = NULL, ..., weights = NULL, 
          metric = ifelse(is.factor(y), "Accuracy", "RMSE"), 
          maximize = ifelse(metric == "RMSE", FALSE, TRUE), 
          trControl = trainControl(), tuneGrid = NULL, 
          tuneLength = 3) 
        NULL
#SPAM Example: trainControl
args(trainControl)
function (method = "boot", number = ifelse(method %in% c("cv", "repeatedcv"), 10, 25), 
          repeats = ifelse(method %in% c("cv", "repeatedcv"), 1, number), 
          p = 0.75, initialWindow = NULL, horizon = 1, fixedWindow = TRUE, 
          verboseIter = FALSE, returnData = TRUE, 
          returnResamp = "final", savePredictions = FALSE, classProbs = FALSE, 
          summaryFunction = defaultSummary, selectionFunction = "best", 
          custom = NULL, 
          preProcOptions = list(thresh = 0.95, ICAcomp = 3, k = 5), 
          index = NULL, indexOut = NULL, timingSamps = 0, 
          predictionBounds = rep(FALSE, 2), seeds = NA, allowParallel = TRUE) 
        NULL


#seed example
set.seed(1235)
modelFit2 <- train(type ~.,data=training, method="glm")
modelFit2



#Example: Wage Data
library(ISLR); library(ggplot2); library(caret);
data(Wage)
summary(Wage)
#Get trainging set
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)
#Feature plot
featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")
#Further data exploration
qplot(age,wage,data=training)
qplot(age,wage,colour=jobclass,data=training)
#Add regression smoothers
qq <- qplot(age,wage,colour=education,data=training)
qq +  geom_smooth(method='lm',formula=y~x)
#Making  factors with cut2 (from Hmisc package)
library(Hmisc)
cutWage <- cut2(training$wage,g=3)
table(cutWage)
#boxplot
p1 <- qplot(cutWage,age, data=training,fill=cutWage,
            geom=c("boxplot"))
p1
#Boxplots with points overlayed
library(gridExtra)
p2 <- qplot(cutWage,age, data=training,fill=cutWage,
            geom=c("boxplot","jitter"))
#Tables
t1 <- table(cutWage,training$jobclass)
t1
prop.table(t1,1) #proportions in each row of the table (using 2 instead of 1 would do proportions in columns)
#Density Plots
qplot(wage,colour=education,data=training,geom="density")
grid.arrange(p1,p2,ncol=2)


#Why preprocess?
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve,main="",xlab="ave. capital run length") #Note that almost
#all of the run lengths are small- data is very skewed so its good to pre-process
#Good idea to standardize the values
mean(training$capitalAve)
sd(training$capitalAve) #Small mean and large standard deviation

trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve  - mean(trainCapAve))/sd(trainCapAve) 
mean(trainCapAveS)
sd(trainCapAveS) #mean and standard deviation are much better

#Can also use the preProcess function
preObj <- preProcess(training[,-58],method=c("center","scale"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

#Can use the followeding
testCapAveS <- predict(preObj,testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

#Can also include the preprocess step in the training step
set.seed(32343)
modelFit <- train(type ~.,data=training,
                  preProcess=c("center","scale"),method="glm")
modelFit

#Other types of transformations: Box-Cox transforms make the data normal
preObj <- preProcess(training[,-58],method=c("BoxCox"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)

#If you have missing data, you can impute it
library(RANN)
set.seed(13343)
# Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA] <- NA
# Impute and standardize
preObj <- preProcess(training[,-58],method="knnImpute")
capAve <- predict(preObj,training[,-58])$capAve
# Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)
#How close are the imputed values? We are looking to see if the values are close
#to zero and there are a couple of ways to look at this
quantile(capAve - capAveTruth)
quantile((capAve - capAveTruth)[selectNA]) 
quantile((capAve - capAveTruth)[!selectNA])


#Covariate creation

library(kernlab);data(spam)
spam$capitalAveSq <- spam$capitalAve^2

library(ISLR); library(caret); data(Wage);
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
#Covariates to add dummy variables
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass,data=training)
head(predict(dummies,newdata=training))
#Removing zero covariates (things that have very little variability and won't be good predictors)
nsv <- nearZeroVar(training,saveMetrics=TRUE)
nsv
#Sex is a near zero variable (there are only males in this dataset) so it wouldn't be good to use sex as a predictor variable

#Splines for polynomial functions
library(splines)
bsBasis <- bs(training$age,df=3) 
bsBasis

lm1 <- lm(wage ~ bsBasis,data=training)
plot(training$age,training$wage,pch=19,cex=0.5)
points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=0.5)

#Splines on the test set
predict(bsBasis,age=testing$age)



#Preprocessings with Principal Component Analysis (PCA)
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

M <- abs(cor(training[,-58]))
diag(M) <- 0 #Everything is correlated to itself so set this sets the diagnols to zero
which(M > 0.8,arr.ind=T)

names(spam)[c(34,32)]
plot(spam[,34],spam[,32])
#We could rotate the plot
X <- 0.71*training$num415 + 0.71*training$num857
Y <- 0.71*training$num415 - 0.71*training$num857
plot(X,Y)
#Principal components in R - prcomp
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])
prComp$rotation

#PCA on SPAM data
typeColor <- ((spam$type=="spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")

#PCA with caret
preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
spamPC <- predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

#Preprocessing with PCA
preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC <- predict(preProc,log10(training[,-58]+1))
modelFit <- train(training$type ~ .,method="glm",data=trainPC)
#Alternative
testPC <- predict(preProc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))


#Predicting with Regressions
#Old Faithful data
library(caret);data(faithful); set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting,
                               p=0.5, list=FALSE)
trainFaith <- faithful[inTrain,]; testFaith <- faithful[-inTrain,]
head(trainFaith)

#Eruption duration versus waiting time
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")

#Fit a linear model
lm1 <- lm(eruptions ~ waiting,data=trainFaith)
summary(lm1)

#Model fit
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,lm1$fitted,lwd=3)
#Predict a new value
coef(lm1)[1] + coef(lm1)[2]*80
newdata <- data.frame(waiting=80)
predict(lm1,newdata)

#Plot predictions - training and test
par(mfrow=c(1,2))
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,predict(lm1),lwd=3)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)

#Get training set/test set errors
# Calculate RMSE on training
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))
# Calculate RMSE on test
sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2))

#Prediction intervals
pred1 <- predict(lm1,newdata=testFaith,interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue")
matlines(testFaith$waiting[ord],pred1[ord,],type="l",,col=c(1,2,2),lty = c(1,1,1), lwd=3)

#Same process with caret
modFit <- train(eruptions ~ waiting,data=trainFaith,method="lm")
summary(modFit$finalModel)


#Predicting with Regression Multiple Covariates
library(ISLR); library(ggplot2); library(caret);
data(Wage); Wage <- subset(Wage,select=-c(logwage))
summary(Wage)

#Get training/test sets
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
dim(training); dim(testing)

#Get Feature Plot
featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,plot="pairs")

#Plot age versus wage
qplot(age,wage,data=training)

#Plot age versus wage colour by jobclass
qplot(age,wage,colour=jobclass,data=training)

#Plot age versus wage colour by education
qplot(age,wage,colour=education,data=training)

#Fit a linear model
modFit<- train(wage ~ age + jobclass + education,
               method = "lm",data=training)
finMod <- modFit$finalModel
print(modFit)

#Diagnostics
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



#Prediction Trees Examples
data(iris); library(ggplot2); library(caret)
table(iris$Species)
inTrain <- createDataPartition(y=iris$Species,p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)
qplot(Petal.Width,Sepal.Width,colour=Species,data=training)
modFit <- train(Species ~ .,method="rpart",data=training)
print(modFit$finalModel)
plot(modFit$finalModel, uniform=TRUE, 
     main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)
#prettier plot
library(rattle)
fancyRpartPlot(modFit$finalModel)
#predicting new values
predict(modFit,newdata=testing)



#BAGGING = Bootstrap-Aggregating
#Produces similar bias but reduces the variance


#Bagged loess example
library(ElemStatLearn); data(ozone,package="ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)
ll <- matrix(NA,nrow=10,ncol=155)
for(i in 1:10){
        ss <- sample(1:dim(ozone)[1],replace=T)
        ozone0 <- ozone[ss,]; ozone0 <- ozone0[order(ozone0$ozone),]
        loess0 <- loess(temperature ~ ozone,data=ozone0,span=0.2)
        ll[i,] <- predict(loess0,newdata=data.frame(ozone=1:155))
}
plot(ozone$ozone,ozone$temperature,pch=19,cex=0.5)
for(i in 1:10){lines(1:155,ll[i,],col="grey",lwd=2)}
lines(1:155,apply(ll,2,mean),col="red",lwd=2)

#More bagging: this fails for some reason
predictors = data.frame(ozone=ozone$ozone)
temperature = ozone$temperature
treebag <- bag(predictors, temperature, B = 10,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))
plot(ozone$ozone,temperature,col='lightgrey',pch=19)
points(ozone$ozone,predict(treebag$fits[[1]]$fit,predictors),pch=19,col="red")
points(ozone$ozone,predict(treebag,predictors),pch=19,col="blue")


#Custom Bagging
ctreeBag$fit

#Parts of Bagging
ctreeBag$pred

#Parts of Bagging
ctreeBag$aggregate



#Random Trees
data(iris); library(ggplot2)
inTrain <- createDataPartition(y=iris$Species,p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
library(caret)
modFit <- train(Species~ .,data=training,method="rf",prox=TRUE)
modFit
#getting a single tree
getTree(modFit$finalModel,k=2)
#Class Centers
irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP); irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col=Species,data=training)
p + geom_point(aes(x=Petal.Width,y=Petal.Length,col=Species),size=5,shape=4,data=irisP)
#Predicting new values
pred <- predict(modFit,testing); testing$predRight <- pred==testing$Species
table(pred,testing$Species)
#Which were the two that we missed?
qplot(Petal.Width,Petal.Length,colour=predRight,data=testing,main="newdata Predictions")



#Boosting
#Wage example
library(ISLR); data(Wage); library(ggplot2); library(caret);
Wage <- subset(Wage,select=-c(logwage))
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
modFit <- train(wage ~ ., method="gbm",data=training,verbose=FALSE)
print(modFit)
qplot(predict(modFit,testing),wage,data=testing)


#Model Based Prediction
#Example:
data(iris); library(ggplot2)
names(iris)
table(iris$Species)
inTrain <- createDataPartition(y=iris$Species,
                               p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)
#Build predictions
modlda = train(Species ~ .,data=training,method="lda")
modnb = train(Species ~ ., data=training,method="nb")
plda = predict(modlda,testing); pnb = predict(modnb,testing)
table(plda,pnb)
#Comparison of results
equalPredictions = (plda==pnb)
qplot(Petal.Width,Sepal.Width,colour=equalPredictions,data=testing)



#Reguarlized Regression
library(ElemStatLearn); data(prostate)
str(prostate)
#code link: http://www.cbcb.umd.edu/~hcorrada/PracticalML/src/selection.R



#Combining Predictors
library(ISLR); data(Wage); library(ggplot2); library(caret);
Wage <- subset(Wage,select=-c(logwage))
# Create a building data set and validation set
inBuild <- createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
validation <- Wage[-inBuild,]; buildData <- Wage[inBuild,]
inTrain <- createDataPartition(y=buildData$wage,p=0.7, list=FALSE)
training <- buildData[inTrain,]; testing <- buildData[-inTrain,]
#Build two different models
mod1 <- train(wage ~.,method="glm",data=training)
mod2 <- train(wage ~.,method="rf",data=training, 
              trControl = trainControl(method="cv"),number=3)
#Predict on the testing set
pred1 <- predict(mod1,testing); pred2 <- predict(mod2,testing)
qplot(pred1,pred2,colour=wage,data=testing)
#Fit a model that combines predictors
predDF <- data.frame(pred1,pred2,wage=testing$wage)
combModFit <- train(wage ~.,method="gam",data=predDF)
combPred <- predict(combModFit,predDF)
#Testing errors- the one with combined predictions is the best
sqrt(sum((pred1-testing$wage)^2))
sqrt(sum((pred2-testing$wage)^2))
sqrt(sum((combPred-testing$wage)^2))
#Predict on validation set
pred1V <- predict(mod1,validation); pred2V <- predict(mod2,validation)
predVDF <- data.frame(pred1=pred1V,pred2=pred2V)
combPredV <- predict(combModFit,predVDF)
#Evaluate on validation
sqrt(sum((pred1V-validation$wage)^2))
sqrt(sum((pred2V-validation$wage)^2))
sqrt(sum((combPredV-validation$wage)^2))



#Forecasting with time-series
#Google data
library(quantmod)
library(forecast)
from.dat <- as.Date("01/01/08", format="%m/%d/%y")
to.dat <- as.Date("12/31/13", format="%m/%d/%y")
getSymbols("GOOG", src="google", from = from.dat, to = to.dat)
#Summarize monthly and store as time series
mGoog <- apply.monthly(GOOG,mean)
googOpen <- Op(mGoog)
ts1 <- ts(googOpen,frequency=12)
plot(ts1,xlab="Years+1", ylab="GOOG")
#Decompose a time series into parts
plot(decompose(ts1),xlab="Years+1")
#Training and test sets
ts1Train <- window(ts1,start=1,end=5)
ts1Test <- window(ts1,start=5,end=(7-0.01))
ts1Train
#Simple moving average
plot(ts1Train)
lines(ma(ts1Train,order=3),col="red")
#Exponential Smoothing
ets1 <- ets(ts1Train,model="MMM")
fcast <- forecast(ets1)
plot(fcast); lines(ts1Test,col="red")
#Get the acuracy
accuracy(fcast,ts1Test)
#Other weblinks:
#https://en.wikipedia.org/wiki/Forecasting
#https://www.otexts.org/fpp/



#Unsupervised Prediction
#Iris example ignoring species labels
data(iris); library(ggplot2)
inTrain <- createDataPartition(y=iris$Species,p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)
#Cluster with k-means
kMeans1 <- kmeans(subset(training,select=-c(Species)),centers=3)
training$clusters <- as.factor(kMeans1$cluster)
qplot(Petal.Width,Petal.Length,colour=clusters,data=training)
#Compare to real labels
table(kMeans1$cluster,training$Species)
#Build predictor
modFit <- train(clusters ~.,data=subset(training,select=-c(Species)),method="rpart")
table(predict(modFit,training),training$Species)
#Apply on test
testClusterPred <- predict(modFit,testing) 
table(testClusterPred ,testing$Species)

