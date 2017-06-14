library(caret)
library(dplyr)
x <- read.csv("125colourstrain.csv")

x$type <- as.factor(x$type)
set.seed(12498)
inTrain <- createDataPartition(y = x$type, p = .7, list = F)
training <- x[inTrain,]
testing <- x[-inTrain,]

redorpink <- c(3,4,5,28,29,30,34,35,60,85,65,90,95,120)

redorpinktrain <- training[,2+redorpink]
redorpinktest <- testing[,2+redorpink]

trainorder <- order(apply(redorpinktrain, 2, mean))

rptrainplus1 <- 1 + redorpinktrain[,trainorder]
rptestplus1 <- 1 + redorpinktest[,trainorder]

trainratios <- NULL
testratios <- NULL
l <- length(trainorder)
for (i in 1:(l-1)){
  for (j in (i+1):l){
    trainratij <- rptrainplus1[,j]/rptrainplus1[,i]
    trainratios <- cbind(trainratios, trainratij)
    testratij <- rptestplus1[,j]/rptestplus1[,i]
    testratios <- cbind(testratios, testratij)
  }
}

colnames(trainratios) <- paste("ratio",1:91)
colnames(testratios) <- paste("ratio",1:91)

trainingdf <- cbind(training[,c("ID","type")], trainratios)
testingdf <- cbind(testing[,c("ID","type")], testratios)


#control=trainControl(method="repeatedcv", 
#                     number=5, 
#                     repeats=10, 
##                     classProbs=TRUE, 
#                     savePredictions=TRUE#,
                     #summaryFunction = LogLossSummary
#                     )

control <- trainControl(method="cv", number=5, classProbs=TRUE, summaryFunction=mnLogLoss)
set.seed(2124)
model2 <- train(type ~ ., 
                data = trainingdf, 
                method = "cforest", 
                metric = "logLoss",
                preProc = c("center", "scale")
                )
probs2 <- predict(model2, 
                  newdata = testingdf, 
                  type = "prob"
                  )
rownames(probs2) <- paste(rownames(probs2),".jpg",sep="")

logloss <- -sum(
  log(probs2$"1"*(testing$type==1) 
       + probs2$"2"*(testing$type==2) 
       + probs2$"3"*(testing$type==3)
       )
  )/nrow(probs2)


  unknown <- arrange(read.csv("125colours0.csv"),ID)
  redorpinkunknown <- unknown[,2+redorpink]
  rpunknownplus1 <- 1 + redorpinkunknown[,trainorder]
  unknownratios <- NULL
  l <- length(trainorder)
  for (i in 1:(l-1)){
    for (j in (i+1):l){
      unknownratij <- rpunknownplus1[,j]/rpunknownplus1[,i]
      unknownratios <- cbind(unknownratios, unknownratij)
    }
  }
  colnames(unknownratios) <- paste("ratio",1:91)
  unknowndf <- cbind(unknown[,c("ID","type")], unknownratios)
  
  
  probsunknown <- predict(model2, 
                          newdata = unknowndf, 
                          type = "prob"
  )
  unknown$ID <- paste(unknown$ID,".jpg",sep="")
  
  df <- cbind(unknown$ID, probsunknown)
  names(df) <- c("image_name","Type_1","Type_2","Type_3")
  write.csv(df, 
            file = "submission2.csv", 
            row.names = FALSE, 
            quote = FALSE
  )