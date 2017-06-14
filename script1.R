library(jpeg); library(imager); library(dplyr)

datadirectory <- "D:/CervicalCancerData/"
traindir <- function(n) {
  paste(datadirectory, "train/Type_", n, sep = "")
}
testdir <- paste(datadirectory, "test/", sep = "")

#filepath <- paste(traindir(1),"/7.jpg",sep = "")
#im <- readJPEG(filepath)
#imc <- as.cimg(im)
#imr <- resize(imc, size_x = 150, size_y = 150)
#plot(imr*as.numeric(imr[,,1]/(imr[,,2]+imr[,,3]) > 1))
#plot(imr*as.numeric(imr[,,1] > .7 | imr[,,1]/(imr[,,2]+imr[,,3]) > 1))

maskedplot <- function(n) {
  filepath <- paste(traindir(1),"/",n,".jpg",sep = "")
  im <- readJPEG(filepath)
  imc <- as.cimg(im)
  imr <- resize(imc, size_x = 150, size_y = 150)
  r <- imr[,,1]
  g <- imr[,,2]
  b <- imr[,,3]
  plot(imr*as.numeric(
    (r > .7 ) | 
      r/(1.6 * g+.6 * b) > 1 | 
      r > .8 * b &
      r > 1.5 * g
    )
    )
}

maskedplot2 <- function(n) {
  filepath <- paste(traindir(1),"/",n,".jpg",sep = "")
  im <- readJPEG(filepath)
  imc <- as.cimg(im)
  imr <- resize(imc, size_x = 150, size_y = 150)
  hsl <- RGBtoHSL(imr)
  hue <- hsl[,,1]
  l <- hsl[,,3]
  plot(imr*as.numeric(
    (hue < 30 | hue > 240) & l > .5 
  )
  )
}

discreteplot <- function(n) {
  filepath <- paste(traindir(1),"/",n,".jpg",sep = "")
  im <- readJPEG(filepath)
  imc <- as.cimg(im)
  imr <- resize(imc, size_x = 150, size_y = 150)
  imd <- floor(10*imr)/10
  plot(imd)
  r <- table(imd[,,1])
  g <- table(imd[,,2])
  b <- table(imd[,,3])
  names(r) <- paste("r", names(r), sep="")
  names(g) <- paste("g", names(g), sep="")
  names(b) <- paste("b", names(b), sep="")
  c(r,g,b)
}

## Function which reads a image and outputs an image with the pixels clustered 
## with a metric accounting for both the distance in the picture and 
## in RGB space
##
## Every pixel should be mapped onto a point in a 5d space

diagramplot <- function(n,r=1,g=1,b=1) {
  filepath <- paste(traindir(1),"/",n,".jpg",sep = "")
  im <- readJPEG(filepath)
  imc <- as.cimg(im)
  imr <- resize(imc, size_x = 150, size_y = 150)
  XY <- cbind(rep(1:150, each = 150), rep(1:150, times = 150))
  R <- as.vector(imr[,,1])
  G <- as.vector(imr[,,2])
  B <- as.vector(imr[,,3])
  XYRGB <- cbind(XY,R,G,B)
#  names(XY) <- c("X","Y")
#  XYRGB <- NULL
#  for(i in 1:150){
#    for(j in 1:150){
#      XYRGB <- rbind(XYRGB, c(j,i,imr[j,i,]))
#    }
#  }
  XYRGB <- data.frame(XYRGB)
#  names(XYRGB) <- c("X","Y","R","G","B")
  XYRGB$R <- r*XYRGB$R 
  XYRGB$G <- g*XYRGB$G 
  XYRGB$B <- b*XYRGB$B 
  #
  #
  kmobj <- kmeans(XYRGB, centers = 20)
  clustered <- kmobj$centers[kmobj$cluster, ]
  imcl <- imr
  imcl[,,1] <- clustered[,3]/r
  imcl[,,2] <- clustered[,4]/g
  imcl[,,3] <- clustered[,5]/b
  plot(imcl)
}