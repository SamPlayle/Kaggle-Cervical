library(caret); library(jpeg); library(imager)

datadirectory <- "D:/CervicalCancerData/"

dir <- function(n) {
  ifelse(n > 0, 
         paste(datadirectory, "train/Type_", n, sep = ""), 
         paste(datadirectory, "test/", sep = "")
  )
}


colorfreqs <- function(filepath) {
  im <- readJPEG(filepath)
  imc <- as.cimg(im)
  imr <- resize(imc, size_x = 150, size_y = 150)
  imd <- floor(4.9999*imr) # We want values of 1.00 to go in the .8 box
  #plot(imd)
  x <- 1 + imd[,,1] + 5*imd[,,2] + 25*imd[,,3]
  tabulate(x, nbins = 125)
}

filelist <- function(n) {
  as.integer(
    lapply(
      list.files(dir(n)) ,  
      function(x) strsplit(x, "\\.")[[1]][1]
    )
  )
}

coldf <- function(n) {
  files <- filelist(n)
  x <- lapply(files, 
              function(x) colorfreqs(
                paste(
                  dir(n), "/", x, ".jpg", sep = "")
              )
  )
  y <- cbind(filelist(n), n, do.call(rbind,x))
  dimnames(y)[[2]][1] <- "ID"
  dimnames(y)[[2]][2] <- "type"
  y
}

writecsvs <- function() {
  for (i in 0:3) {
    x <- coldf(i)
    write.csv(x, file = paste("125colours",i,".csv", sep = ""), row.names = FALSE)
  }
  xtrain <- rbind(x1,x2,x3)
  write.csv(xtrain, file = "125coltrain.csv", row.names = FALSE)
}

# needs colordistr2.R

x <- read.csv("125colourstrain.csv")
x$type <- as.factor(x$type)
inTrain <- createDataPartition(y = x$type, p = .7, list = F)
training <- x[inTrain,]
testing <- x[-inTrain,]

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

docsv <- function(){
  unknown <- read.csv("125colours0.csv")
  probsunknown <- predict(model1, 
                          newdata = unknown, 
                          type = "prob"
                          )
  df <- cbind(unknown$ID, probsunknown)
  names(df) <- c("image_name","Type_1","Type_2","Type_3")
  df <- arrange(df, df$image_name)
  write.csv(df, 
            file = "submission1.csv", 
            row.names = FALSE, 
            quote = FALSE
            )
}