##Final Project
##Evan Schulze, Eric Snyder, Krishna Suhas

##Question 1
##read in data
read_digits<-function(input)
{
  samp<-read.table(input)
  samp[,1]<-as.integer(samp[,1])
  samp
}
##store data
test<-read_digits("test.txt")
table(sapply(test,typeof))
train<-read_digits("train.txt")
table(sapply(train,typeof))

##Question2
##plot point of interest
view_digit<-function(rownum,indata)
{
  image(t(apply(matrix(as.integer( indata[rownum,2:ncol(indata)]),nrow = 16,ncol = 16),1,rev)),col=c("white","black","gray"))
}
view_digit(3,train)

##Question3
##plot averages of all pixels by class
avgpixels<-function(indata)
{
  library(dplyr)
  library(tidyr)
  samp<-indata %>% group_by(indata[,1]) %>% summarise_all(funs(median))
  samp[,-1]
}

par(mfrow=c(2,5))
sapply(1:10, function(x)view_digit(x,avgpixels(train)))

##Question 4
#KNN Distance Function
predict_knn<-function(testd,traind,met,k)
{
  output<-numeric(nrow(testd))
  sapply(1:nrow(testd), function(i){
    output[i]<<-predict_knn2(testd[i,],traind,met,k)})
  as.integer( output)
}
predict_knn2<-function(testd,traind,met,k)
{
  if(met == "euclidean")
  {
    maintrain<-traind
    traind<-traind[,-1]
    distance<-numeric(nrow(traind))
    distance<-as.vector(as.matrix(pdist(traind,testd)))
    mini<-head(sort(distance),k)
    pos<-numeric(length(mini))
    sapply(1:length(mini), function(i){pos[i]<<-which(distance == mini[i])})
    temp<-as.data.frame(table(maintrain[pos,1]))
    temp<-temp[order(temp$Freq,decreasing = TRUE),]
    return(levels(droplevels(temp[1,1])))
  }
  maintrain<-traind
  traind<-traind[,-1]
  distance<-numeric(nrow(testd)*nrow(traind))
  testd<-t(testd)
  traind<-t(traind)
  distance<-sapply(1:ncol(testd), function(X)
  {sapply(1:ncol(traind), function(Y){distance[(X-1)*ncol(traind)+Y]<-dist(rbind(testd[,X],traind[,Y]),method = met)})})
  temp<-matrix(distance,nrow = ncol(testd))
  mini<-head(sort(temp),k)
  pos<-numeric(length(mini))
  sapply(1:length(mini), function(i){pos[i]<<-which(temp == mini[i])})
  temp<-as.data.frame(table(maintrain[pos,1]))
  temp<-temp[order(temp$Freq,decreasing = TRUE),]
  return(levels(droplevels(temp[1,1])))
}

##Question 5
cv_error_knn <- function(train,met,k){
  ##randomly shuffle the data
  yourData<-train[sample(nrow(train)),]
  ##create 10 equally size folds
  folds <- cut(seq(1,nrow(yourData)),breaks=10,labels=FALSE)
  testsetCopy<-c()
  predicted<-c()
  ##perform 10 fold cross validation
  sapply(1:10, function(i){
    ##segement data by fold 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- yourData[testIndexes, ]
    trainData <- yourData[-testIndexes, ]
    ##use the test and train data partitions to calculate predicted model
    predicted<<-c(predicted,predict_knn(testData[,-1],trainData,met,k))
    testsetCopy <<- c(testsetCopy,testData[,1])
  })
  ##calculate MSE
  mean(testsetCopy-predicted)^2
}

##Question 6
par(mfrow=c(2,1)) ##plot train data CV errors by K value
temp1<-sapply(1:15,function(i){ cv_error_knn(train[1:1000,],"euclidean",k)})
plot(temp1,xlab = "K nearest neighbours", ylab = "error rates", main="euclidean")
temp2<-sapply(1:15,function(i){ cv_error_knn(train[1:1000,],"manhattan",k)})
plot(temp2,xlab = "K nearest neighbours", ylab = "error rates", main="manhattan")

##Question 7
library(caret)
confusion_knn <- function(train,met,k){
  ##randomly shuffle the data
  yourData<-train[sample(nrow(train)),]
  ##create 10 equally size folds
  folds <- cut(seq(1,nrow(yourData)),breaks=10,labels=FALSE)
  actual<-c()
  predicted<-c()
  ##perform 10 fold cross validation
  sapply(1:10, function(i){
    #segement data by fold 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- yourData[testIndexes, ]
    trainData <- yourData[-testIndexes, ]
    ##use the test and train data partitions to calculate predicted model
    predicted<<-c(predicted,predict_knn(testData[,-1],trainData,met,k))
    actual <<- c(actual,as.numeric(testData[,1]))
  })
  ##calculate confusion matrix
  cv.data <- data.frame(predicted,actual)
  df <- table(lapply(cv.data, factor, levels = seq(0, 9, 1)))
}
##calculate confusion matrices for best combinations of k and distance metric
predict_1 <- confusion_knn(train,"euclidean",1)
predict_3 <- confusion_knn(train,"euclidean",3)
predict_15 <- confusion_knn(train,"euclidean",15)

##Question 8
##euclidean, k = 1 is most accurate
predict_1 <- confusion_knn(train,"euclidean",1) ##analyze best combination

##Question 9 
par(mfrow=c(2,1)) ##plot test data CV errors by k value
temp1<-sapply(1:15,function(i){ cv_error_knn(test,"euclidean",k)})
plot(temp1,xlab = "K nearest neighbours", ylab = "error rates", main="euclidean")
temp2<-sapply(1:15,function(i){ cv_error_knn(test,"manhattan",k)})
plot(temp2,xlab = "K nearest neighbours", ylab = "error rates", main="manhattan")
