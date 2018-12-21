################################################################################
# Classifications
# =========================================
################################################################################
#Interactive reading of the external file for mydata data
mydata <- read.table(file.choose(), header=TRUE, sep=";", na.strings="NA", dec=".", strip.white=TRUE)
# Or internal data set
data("iris")
mydata<-iris
#If necessary, the follwong step suppress all rows with missing dat (NA)
mydata<-na.omit(mydata)
# Number of lines
N<- dim(mydata)[1]
# Number of columns
nCol<- dim(mydata)[2]
# Column number for the class
colClass<-5
# Rename the colClass column "Class" for a standart use of formulae
oldnames<-colnames(mydata)
# Transform mydata into a dataframe to be able to transform 
# the "Class" variable into a factor(type needed by 
# several classification methods)
mydata<-data.frame(mydata[,-colClass], as.factor(mydata[,colClass]))
# Now variable Class is the last one, and we give it the 
# name "Class"
colnames(mydata)[nCol]<-"Class"
colClass<-nCol
#load library for classError
library(mclust)
# load class library for knn
library(class)

# Building a training set and a test set
# index is the index of a random sample (1/3 ,2/3) for indexidation set and
# training set.
index <- sample(1:N, size = round(N/3), replace = FALSE,prob = rep(1/N, N))
# Build the two sets using index
training.set <- mydata[-index,-colClass]
training.class<- as.factor(mydata[-index,colClass])
test.set <- mydata[index,-colClass]
test.class <- as.factor(mydata[index,colClass])


# k-nearest neighbours
mydata.knn <- knn(training.set, test.set, training.class, k = 6)
# Print a summary of results
summary(mydata.knn)
# Compute  and print the confusion matrix
cm<-table(as.factor(test.class), mydata.knn)
cm
(sum(cm) - sum(diag(cm)))/sum(cm)
# Mehtod classError from packahe Mclust do the same thing
classError(mydata.knn,as.factor(test.class))$errorRate

# Build a trining set with classes
training.set.class<- data.frame(training.set,training.class)
colnames(training.set.class)<-colnames(mydata)


# SVM
#install.packages("e1071")
library(e1071)
#Training  du modèle
mydata.svm.model<- svm(Class ~ ., data = training.set.class)
mydata.svm.pred<-predict(mydata.svm.model,test.set )
cm<-table(test.class, mydata.svm.pred)
cm
classError(mydata.svm.pred,test.class)$errorRate
# Exercise:
# Perform knn, naive abyes, Mclust and SVM classification 
# on "Credit Cards Default" data ( https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients )


# Decision Trees
#install.packages("tree")
library(tree)
#Tree on all data
mydata.tree<- tree(Class ~ ., data = mydata)
mydata.tree
summary(mydata.tree)
plot(mydata.tree, main=paste("Decision Tree for mydata Data",sep=""));
text(mydata.tree, all=T, cex=0.85)

#Using tree for prediction
mydata.tree<- tree(Class ~ ., data = training.set.class)
mydata.tree.pred<-predict(mydata.tree, test.set, type="class")
cm<-table(test.class, mydata.tree.pred)
cm
classError(mydata.tree.pred,test.class)$errorRate

## visualization
## use party package
#install.packages("party")
library(party)
mydata.tree<-ctree(Class ~ ., data = mydata,controls = ctree_control(maxdepth = 6))
plot(mydata.tree)

#Exercise : use classifcation trees to understanding the Titanic 
#           Data (cf Moddle ) using variables age, sex and 
#           pclass




install.packages("caret")
library(caret)

# Building a training set and a test set
inTrain <- createDataPartition(y = mydata$Class,
                               ## the outcome data are needed
                               p = .75,
                               ## The percentage of data in the
                               ## training set
                               list = FALSE)

## The format of the results
## The output is a set of integers for the rows of data
## that belong in the training set.
str(inTrain)
training <- mydata[ inTrain,]
testing  <- mydata[-inTrain,]
nrow(training)
nrow(testing)

# Using a specific method for a training/test
# Example SVM
svmFit <- train(Class ~ .,
                data = training,
                method = "svmLinear")
svmFit
svmClasses <- predict(svmFit, newdata = testing)
str(svmClasses)
confusionMatrix(data = svmClasses, testing$Class)

#Example knn
knnFit <- train(Class ~ .,
                data = training,
                method = "knn")
knnFit
knnClasses <- predict(knnFit, newdata = testing)
confusionMatrix(data = knnClasses, testing$Class)
#A complete list of available models 
?train_model_list

# Exercise : Find and try C4.5 trees and Naive bayes models

# Comparing models
# kfold cross-validation with k=10 
ctrl <- trainControl(method = "cv",
                     number=10, verboseIter = TRUE)

# train the svm model
svmModel<-train(y=mydata$Class, x=mydata[,-colClass],method = "svmLinear", trControl=ctrl)
knnModel<-train(y=mydata$Class, x=mydata[,-colClass],method = "kknn", trControl=ctrl)

# collect resamples
results <- resamples(list(SVM=svmModel, KNN=knnModel))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)

#Exercise :  add C4.5 trees and Naive bayes models in evaluation for iris data

#Exercise : find the parameter  for Leave one out cross validation

#Exercise :  find the best models between knn, naive bayes, svm and
#            C4.5 trees for the credit defaults dataset.



