#setwd("../Dropbox/Data Science/Kaggle/CervicalCancer/R")

seq1 <- seq(from = 0, to = 4.8, by = 0.2)
seq2 <- seq(from = 0, to = 0.8, by = 0.2)
y <- expand.grid(seq1,seq2)
red <- y[,2]
green <- floor(y[,1])/5
blue <- y[,1] - floor(y[,1])
num <- as.integer(round(1 + 5*red + 25*green + 125*blue),0)
pdf(file = "Colour palette.pdf", width = 960/72, height = 480/72)
plot(y[,1], 
     y[,2], 
     pch = 19,
     cex = 4, 
     col = rgb(red, green, blue), 
     ylim = c(-.1,.9),
     xlab = "b + 5 * g",
     ylab = "r",
     main = "Colour chart"
     )
text(y[,1], y[,2] - 0.065, labels = as.character(num))
dev.off()