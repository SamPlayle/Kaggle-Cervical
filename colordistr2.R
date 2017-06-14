library(jpeg); library(imager)

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