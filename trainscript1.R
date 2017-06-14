library(caret)
x <- read.csv("125colourstrain.csv")
x$type <- as.factor(x$type)
inTrain <- createDataPartition(y = x$type, p = .7, list = F)
training <- x[inTrain,]
testing <- x[-inTrain,]

#control=trainControl(method="repeatedcv", 
#                     number=5, 
#                     repeats=10, 
##                     classProbs=TRUE, 
#                     savePredictions=TRUE#,
                     #summaryFunction = LogLossSummary
#                     )

model1 <- train(type ~ ., 
                data = training, 
                method = "cforest", 
               # metric = "LogLoss",
                preProc = c("center", "scale")
                )
probs1 <- predict(model1, 
                  newdata = testing, 
                  type = "prob"
                  )
rownames(probs1) <- paste(rownames(probs1),".jpg",sep="")

logloss <- -sum(
  log(probs1$"1"*(testing$type==1) 
       + probs1$"2"*(testing$type==2) 
       + probs1$"3"*(testing$type==3)
       )
  )/nrow(probs1)