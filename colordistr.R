library(jpeg); library(imager)

datadirectory <- "D:/CervicalCancerData/"

dir <- function(n) {
  ifelse(n > 0, 
         paste(datadirectory, "train/Type_", n, sep = ""), 
         paste(datadirectory, "test/", sep = "")
  )
}


## Discreteplot is a function which outputs a vector showing the 
## distribution of the RGB values (discretized to 0.1) from a scaled
## down 150 * 150 image.

discreteplot <- function(filepath) {
  im <- readJPEG(filepath)
  imc <- as.cimg(im)
  imr <- resize(imc, size_x = 150, size_y = 150)
  imd <- floor(9.9999*imr)/10 # We want values of 1.00 to go in the .9 box
  #plot(imd)
  r <- table(imd[,,1])
  g <- table(imd[,,2])
  b <- table(imd[,,3])
  names(r) <- paste("r", names(r), sep="")
  names(g) <- paste("g", names(g), sep="")
  names(b) <- paste("b", names(b), sep="")
  c(r,g,b)
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
              function(x) discreteplot(
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
    write.csv(x, file = paste("col",i,".csv", sep = ""), row.names = FALSE)
  }
  xtrain <- rbind(x1,x2,x3)
  write.csv(xtrain, file = "coltrain.csv", row.names = FALSE)
}