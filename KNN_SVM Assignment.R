#Load the bus data file ('bus1_47.xlsx') and inspect it.

getwd()
setwd("/Users/tosinbademosi/Downloads")

Which attribute describes the "class" or the predicted outcome label?.
routeTag.

Remove any attributes that will not contribute to this classification exercise.
As in the previous lab, remove any rows not corresponding to routes 1 or 47.

bus1 <- read.csv("bus1_47.csv")
#Remove unwanted attribute

busclean <- subset(bus1,select = -c(4))
str(busclean)
#Checking to know the observation that are not wanted.
summary(busclean)
#Split data set.
train_index <- sample(1:nrow(busclean),0.8*nrow(busclean))
test_index <- setdiff(1:nrow(busclean),train_index)

#ceating the normalize function.
NormFun <- function(x) {(x -min(x)/(max(x)-min(x)))}
#apply function
BusNorm <- as.data.frame(lapply(busclean[,c(1,2)], NormFun))
summary(BusNorm)

#Build train and test sets.
busdata_XTrian <- BusNorm[train_index, ]
busdata_XTest <-  BusNorm[-train_index, ]

busdata_YTrian <- busclean[train_index, 3]
busdata_YTest <- busclean[-train_index, 3]


#Creating the knn model
pr <- knn(busdata_XTrian,busdata_XTest,cl=busdata_YTrian,k=3)


#Creating confusion matrix.

tab <- table(pr,busdata_YTest)
View(tab)

# Accuracy determination
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
91.92308

#plotting a scatter graph for misclassified data predicted.
index <- TestSet[,3]!=Y_predict
Mistakes<-TestSet[index,]
#Ploting the graph
plot(busdata_YTest$lon , busdata_YTest$lat , main = "MISCLASSIFIED", xlab = "LON", ylab = "LAT")

plot(Mistakes$lon,Mistakes$lat, main= "MISCLASSIFIED", xlab="LON", ylab="LAT")







#create new data set for SVM.
bussvm <- bus1[1:3]

#Encoding the target feature as factor.

bussvm$routeTag <- factor(bussvm$routeTag, levels = c("Bus1","Bus47"))

#splitting the data set into Train set and Test set.
install.packages("caTools")

library(caTools)
split= sample.split(bussvm$routeTag, SplitRatio = 0.80) 
TrainSet <- subset(bussvm, split ==T)
TestSet <- subset(bussvm, split==F)
#Scaling features
TrainSet[-3] <- scale(TrainSet[-3])
TestSet[-3] <- scale(TestSet[-3])
summary(TrainSet[-3])

# Fitt Train set for SVM.
install.packages("e1071")
classifier = svm(formula = routeTag ~ .,
          data = TrainSet,
          type = 'C-classification',
          kernel = 'linear')

classifier = svm(formula = routeTag ~ ., 
                 data = TrainSet, 
                 type = 'C-classification', 
                 kernel = 'linear') 

#Predict the test set.
Y_predict <- predict(classifier, newdata = TestSet[-3])

#Making a confusion Matrix
cm <- table(TestSet[,3], Y_predict)
View(cm)
accuracy(cm)
 69.48718




index<-A!=B


Why is SVM so much worse than k-NN in this case? Think about the attributes we retained for use in this case, 
and how the data points are distributed in this subspace of the full high-dimensional data space 
(If you need a reminder, you might want to refer to your answers from the previous lab). 
Relate this to how the respective algorithms (k-NN and SVM) algorithms work.
#Reason for the advantages of KNN over SVM
The kNN is easy to compute and the accuracy is more for knn than to SVM base on confusion Matrix for both methods.
KNN accuracy is 92% while SVM Is 69%.

#Testing the results of K when k is 50 and 100 respectively.
pr1 <- knn(busdata_XTrian,busdata_XTest,cl=busdata_YTrian,k=50)
pr2 <- knn(busdata_XTrian,busdata_XTest,cl=busdata_YTrian,k=100)

#create confusion matrix for the testing

tab1 <- table(pr1,busdata_YTest)
View(tab1)
accuracy(tab1)
90%
tab2 <- table(pr2,busdata_YTest)
View(tab2)
accuracy(tab2)
89%
#comment and observation 
i observed that the best k for this operation is K being 3. When k was 3, the accuracy is 92%.But with k being 50, the accuracy
to 90% and when k is finally 100, the accuracy dropped to 89%
  
  
  
  
  
     
     