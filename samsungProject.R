#383W Project
#Samsung Data Feature Selection

#load the data and store it
load("C:/Users/scott/Desktop/DSC383W/samsungData.rda")
dim(samsungData)

dimdata = dim(samsungData)
dim(samsungData)

#make activity a factor
samsungData$activity = as.factor(samsungData$activity)

#fix duplicate column names
original = data.frame(table(names(samsungData)))
new = make.names(names=names(samsungData),unique=TRUE, allow_=TRUE)
names(samsungData) = new

# ---------------------Exploratory Analysis-------------------------------------

#get column names
colnames(samsungData)

#print out summary of the activities
summary(factor(samsungData[, dimdata[2]]))
barplot(summary(factor(samsungData[,dimdata[2]])),
        cex.names = .6,
        main = "Activity Factors",
        xlab = "Factor",
        ylab = "Number of Observations",
        col = c("red", "blue", "green", "yellow", "purple", "orange"))

#summary of subject numbers ... # of data points for the subjects
summary(factor(samsungData[, dimdata[2]-1]))
barplot(summary(factor(samsungData[, dimdata[2]-1])),
        cex.names =0.6,
        main = "Number of data points for each individual",
        xlab = "Individual",
        ylab = "Number of data points")

#summary of class labels per subject
my_df = table(samsungData$subject, samsungData$activity)
par(cex.main = 1)
plot(my_df, main="")
title(main="distribution of class labels per subject", outer = FALSE)
barplot(t(as.matrix(my_df)),
        beside = TRUE, 
        main = "Number of Datapoints for Each Activity per Subject",
        xlab = "Subject Number",
        ylab = "Number of observations",
        col = c("red","blue","green","orange","purple","yellow"))
legend("topright",
       colnames(my_df), 
       col = c("red","blue","green","orange","purple","yellow"), 
       lty = 1,
       lwd = 3, 
       cex = .55)

#draw the boxplot for each of the features
last_feature_index = dimdata[2]-2
for (i in seq(1,last_feature_index, 50)) {
  if (i+50 < last_feature_index) {
    end = i + 50
  } else {
    end =last_feature_index
  }
  
  labels = vector(mode="logical", length=0)
  
  for(j in seq(i,end,1)){
    labels = c(labels, colnames(samsungData)[j])
  }
  
  boxplot(samsungData[,i:end], main=paste("Box plots [",i, ",",(i+50), "]"),pos = 1, xpd = TRUE, par(cex.axis=0.6,las=2,mar=c(12,5,5,5)))
}

?par
# analyze the skewness of the features
if(!("moments" %in% rownames(install.packages()))) {install.packages("moments", dependencies = TRUE)}
library(moments)

#calculate how many features for each category
norm = 0
count = 0
high = 0

#if skewed, but not highly skewed - most likely to be a good feature to split data
#therefore, draw density plot to analyze feature
#idea, plots that look more bimodal will most likely be skewed, but not likely highly
#        skewed because there is a roughly equal number of activity features
#if highly skewed, almost all the points must be in one location and the skewness is
#        due to noise in the data
for(i in 1:last_feature_index){
  if(abs(skewness(samsungData[,i])) <= 1){
    norm = norm + 1
  }
  else if(abs(skewness(samsungData[,i])) > 1 & abs(skewness(samsungData[,i])) < 2){
    plot(density(samsungData[,i]), 
         main = colnames(samsungData)[i],
         ylab = "density")
    count = count + 1
  }
  else if(abs(skewness(samsungData[,i])) >= 2){
    high = high + 1
  }
}

#print out the totals
cat("Normal Distribution: " , norm)
cat("Skewed: " , count)
cat("Highly Skewed: " , high)


# ---------------------------------Methods--------------------------------------

#install caret package
if(!("caret" %in% rownames(install.packages()))) {install.packages("caret",dependencies = TRUE)}
library(caret)

#set the seed
set.seed(123)

#partition the data into training (80%) and test  (20%)
inTrain = createDataPartition(y=samsungData$activity,
                              p=.8,
                              list=FALSE)

#create the training and test sets
train = samsungData[inTrain,]
test = samsungData[-inTrain,]

#remove the subject attribute
train = subset(train, select = -subject)
test = subset(test, select = - subject)



# -----------------------------Feature Selection--------------------------------


if(!("parallel" %in% rownames(install.packages()))) {install.packages("parallel",dependencies = TRUE)}
if(!("doParallel" %in% rownames(install.packages()))) {install.packages("doParallel",dependencies = TRUE)}

library(parallel)
library(doParallel)

set.seed(12345)

#do parallel processing
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

#run the random forest model to select the top features
#using 5-fold cross-validation and parallel processing, time = 20 minutes
rf_model<-train(activity~.,data=train,method="rf",
                trControl=trainControl(method="cv",number=5),
                prox=TRUE,allowParallel=TRUE)

stopCluster(cluster)

#print model and final model results
print(rf_model)
print(rf_model$finalModel)

#get the most important features from the random forest results
importance = varImp(rf_model,scale=FALSE)
print(importance)

#retrive the most important variables
model1 = as.name(rownames(importance$importance)[1])
model2 = rownames(importance$importance)[c(1:2)]
model3 = rownames(importance$importance)[c(1:3)]
model4 = rownames(importance$importance)[c(1:4)]
model5 = rownames(importance$importance)[c(1:5)]

# -------------------------------Accuracy---------------------------------------

if(!( "randomForest" %in% rownames(install.packages()))) {install.packages("randomForest",dependencies=TRUE)}

#run random forest algoirthm with the features to determine accuracies of the attributes
set.seed(111)

fit1 = randomForest(activity ~ tBodyAcc.mean...X,
                    data = train,
                    importance = TRUE)

set.seed(222)

fit2 = randomForest(activity ~ tBodyAcc.mean...X + tBodyAcc.energy...X,
                    data = train,
                    importance = TRUE)

set.seed(333)

fit3 = randomForest(activity ~ tBodyAcc.mean...X + tBodyAcc.energy...X + tGravityAcc.mean...Y,
                    data = train,
                    importance = TRUE)

set.seed(444)

fit4 = randomForest(activity ~ tBodyAcc.mean...X + tBodyAcc.energy...X + tGravityAcc.mean...Y + angle.X.gravityMean.,
                    data = train,
                    importance = TRUE)


set.seed(555)

fit5 = randomForest(activity ~ tBodyAcc.mean...X + tBodyAcc.energy...X + tGravityAcc.mean...Y + angle.X.gravityMean. + tGravityAcc.min...X,
                    data = train,
                    importance = TRUE)

# calculate the accuracies for the rf for the features
fit1Acc = 1 - mean(predict(fit1) != train$activity)
fit2Acc = 1 - mean(predict(fit2) != train$activity)
fit3Acc = 1 - mean(predict(fit3) != train$activity)
fit4Acc = 1 - mean(predict(fit4) != train$activity)
fit5Acc = 1 - mean(predict(fit5) != train$activity)

#create array of accuracies
theAccuracies = c(fit1Acc, fit2Acc, fit3Acc, fit4Acc, fit5Acc)

#create plot of accuracies
plot(theAccuracies,
     type = "b",
     main = "Accuracy for Features Selected",
     xlab = "Number of features",
     ylab = "Accuracy")

# --------------------------------Test------------------------------------------

set.seed(11)

test1 = randomForest(activity ~ tBodyAcc.mean...X,
                    data = test,
                    importance = TRUE)

set.seed(22)

test2 = randomForest(activity ~ tBodyAcc.mean...X + tBodyAcc.energy...X,
                    data = test,
                    importance = TRUE)

set.seed(33)

test3 = randomForest(activity ~ tBodyAcc.mean...X + tBodyAcc.energy...X + tGravityAcc.mean...Y,
                    data = test,
                    importance = TRUE)

set.seed(44)

test4 = randomForest(activity ~ tBodyAcc.mean...X + tBodyAcc.energy...X + tGravityAcc.mean...Y + angle.X.gravityMean.,
                    data = test,
                    importance = TRUE)


set.seed(55)

test5 = randomForest(activity ~ tBodyAcc.mean...X + tBodyAcc.energy...X + tGravityAcc.mean...Y + angle.X.gravityMean. + tGravityAcc.min...X,
                    data = test,
                    importance = TRUE)

# calculate the accuracies for the rf for the features
test1Acc = 1 - mean(predict(test1) != test$activity)
test2Acc = 1 - mean(predict(test2) != test$activity)
test3Acc = 1 - mean(predict(test3) != test$activity)
test4Acc = 1 - mean(predict(test4) != test$activity)
test5Acc = 1 - mean(predict(test5) != test$activity)

#create array of accuracies
testAcc = c(test1Acc, test2Acc, test3Acc, test4Acc, test5Acc)

#create plot of accuracies
plot(testAcc,
     type = "b",
     main = "Accuracy for Features Selected",
     xlab = "Number of features",
     ylab = "Accuracy")

#plot comparison of 
plot(theAccuracies,
     col = "blue",
     type = "b",
     main = "Accuracy for Features Selected",
     xlab = "Number of features",
     ylab = "Accuracy")
points(testAcc,
       col = "red",
       type = "b")
legend("bottomright", legend = c("Train", "Test"), col = c("blue", "red"), lty=1:1, cex = 1)

#initialize variables
eighty = 0
ninty = 0

# solve for the # of features required
for(i in 1:5){
  if(eighty == 0 & testAcc[i] > .8){
    eighty = i
  }
  if(ninty == 0 & testAcc[i] > .9){
    ninty = i
  }
}

#print out number of features required
cat("80% accuracy requires " , eighty , " features")
cat("90% accuracy requires ", ninty , " features")

#compare to benchmark
bench = randomForest(activity ~ .,
                     data = test,
                     importance = TRUE)

benchAcc = 1 - mean(predict(bench) != test$activity)

cat("benchmark accuracy: " , benchAcc)

bench2 = randomForest(activity ~ .,
                      data = train,
                      importance = TRUE)

benchAcc2 = 1 - mean(predict(bench2) != train$activity)

cat("benchmark2 accuracy: " , benchAcc2)

#plot the desity functions of the features to analyze them
plot(density(samsungData[,41]),
     main = "Density plots for the top 5 features",
     ylab = "Density",
     col = "blue")
lines(density(samsungData[,57]),
     col = "red")
lines(density(samsungData[,42]),
     col = "green")
lines(density(samsungData[,559]),
     col = "orange")
lines(density(samsungData[,53]),
     col = "purple")
legend("topleft",
       legend = c("tGravityAcc.mean...X", "tGravityAcc.energy...X", "tGravityAcc.mean...Y", "angle.X.gravityMean.", "tGravityAcc.min...X"), 
       col = c("blue","red","green","orange","purple"), 
       lty = 1,
       lwd = 3, 
       cex = .8)

#calculate skewness for the top five features
skewness(samsungData[,41])
skewness(samsungData[,57])
skewness(samsungData[,42])
skewness(samsungData[,559])
skewness(samsungData[,53])
