#################################################
#Author:NERO
#Dated: 23/12/2017
#Purpose: Analysing bank marketing data 
#Input:Bank_Marketing.txt (Tab separated text)
################################################

#################Loading data into the environment#################

bank <- read.table("bank_additional_full.csv", header=TRUE,sep=";")

#Lets look at dataset and generate initial understanding about the column types
str(bank)

#################Missing Value Check#################
# A quick check:
# If newbank has same number of observation that implies no NA value present
newbank <- na.omit(bank)
nrow(newbank)==nrow(bank)


# Missing check is done for job below. We donot have to repeat this process because we have already
# seen that there is no mssing value above ((no.omit/bank) formula)#

if(length(which(is.na(bank$job)==TRUE)>0)){
  print("There are missing values")
} else{
  print("No missing values")
}


#################Outlier detection and treatment#################
# Let's find the range of individual variables#
summary(bank)
# We look at difference between mean and median in summary if its more there might be outlier #
boxplot(bank$age, main="Age Box plot",
        yaxt="n", xlab="Age", horizontal=TRUE,
        col=terrain.colors(2))

# By plotting histogram we can ensure if there are outliers or not
hist(bank$age,col=terrain.colors(10))

# We choose to remove outlier values for age with using following code #c

banksub <- subset(bank,age<60)


#################Correlation Analysis#################
#It emphsize on what we say using box plot, It can tell if predictor is a good predictor or not a good predictor
#This analysis can help us decide if we can drop some columns/predictors depending upon its correlation with the outcome variable
install.packages("psych")
library(psych)

pairs.panels(banksub[, c(1:8,17)])
pairs.panels(banksub[, c(9:17)])

#################Subset Selection#################
banksub2 <-banksub[, c(1:4,7:9,12,14,15,17)]
str(banksub2)
pairs.panels(banksub2)


#################Binning and data Trsformation#################
banksub2$age <- cut(banksub2$age, c(1,20,40,60,100))
banksub2$is_divorced <- ifelse( banksub2$marital == "divorced", 1, 0)
banksub2$is_single <- ifelse( banksub2$marital == "single", 1, 0)
banksub2$is_married <- ifelse( banksub2$marital == "married", 1, 0)
banksub2$marital <- NULL
str(banksub2)

#################Plotting##################
#################Plotting:Finding Overlap:Major predictor (if any)#############################
#For numerical  values in the new structure we can do box plot and check the overlap
boxplot(duration~y,data=banksub, main="Finding Overlap between predictor and outcome",
        yaxt="n", xlab="Duration", horizontal=TRUE,
        col=terrain.colors(3))
boxplot(duration~y,data=bank_marketing_data_sub,col="red")
boxplot(pdays~y,data=bank_marketing_data_sub,col="red")
boxplot(previous~y,data=bank_marketing_data_sub,col="red")
boxplot(is_divorced~y,data=bank_marketing_data_sub,col="red")
boxplot(is_single~y,data=bank_marketing_data_sub,col="red")
boxplot(is_married~y,data=bank_marketing_data_sub,col="red")
# So there is no clear boudry and there is overlap so by looking at any of those variable we cannot tell the output
# We need to look for relations among variable and use more feature

#################Plotting:Finding Overlap between Predictors###############
##Similarly boundry can be researched between two predictors also
library(ggplot2)
qplot(bank_marketing_data_sub$pdays,bank_marketing_data_sub$duration,data=bank_marketing_data_sub,colour=y,size=3)
#See RelationFinder.r for all combinations


#################Plotting:General Visualization#################
par(mfrow=c(2,2),las=2)
boxplot( duration ~ y, data=bank_marketing_data_sub,col="blue")
boxplot( pdays ~ y, data=bank_marketing_data_sub,col="red")
plot( bank_marketing_data_sub$housing, bank_marketing_data_sub$y,
      xlab="Housing", ylab="Become Customer?", col=c("red","green"))
plot( bank_marketing_data_sub$contact, bank_marketing_data_sub$y,
      xlab="Contact Type", ylab="Become Customer?", col=c("red","green"))


#################Training and testing split#################
#CreateDataPartition present in caret packagesplit in such a way that
#training and testing data will have same ratio for target variable
library(caret)
#Rows selection for training data set
inTrain <- createDataPartition(y=bank_marketing_data_sub$y ,p=0.7,list=FALSE)
training <- bank_marketing_data_sub[inTrain,]
testing <- bank_marketing_data_sub[-inTrain,]
# As we said we have imbalanced data so how can we do  sampling
#Caret will take care of that, So createDataPartition does the magic 
dim(training);dim(testing)
# We can see imbalancing has been taken care of or not
table(training$y); table(testing$y)
#################Decision Tree#################
library(rpart)
library(rpart.plot)
library(rattle)
library(caret)
library(c50)
dt_model<- rpart(y ~ ., data = training)
fancyRpartPlot(dt_model)
summary(dt_model)

#################Testing Decision Tree#################
predictions <- predict(dt_model, testing, type = "class")
# Lets try for individual Values
predict(dt_model, testing[1,-10])
#What is predicted
table(predictions)
# Lets look at the confusion matrix
confusion.matrix <- prop.table(table(predictions, testing$y))
confusion.matrix
confusionMatrix(predictions,testing$y)

############ Random Forest##############
library(randomForest)
model <- randomForest(y ~ ., data=training)
model
#importance of each predictor
importance(model)

############ Testing Random forest ############
library(caret)
predicted <- predict(model, testing)
table(predicted)
confusionMatrix(predicted, testing$y)

#Effect of increasing tree count 
accuracy=c()
for (i in seq(1,50, by=1)) {
  modFit <- randomForest(y ~ ., data=training, ntree=i)
  accuracy <- c(accuracy, confusionMatrix(predict(modFit, testing, type="class"), testing$y)$overall[1])
}
par(mfrow=c(1,1))
plot(x=seq(1,50, by=1), y=accuracy, type="l", col="green",
     main="Accuracy VS Tree-Size", xlab="Tree Size", ylab="Accuracy")