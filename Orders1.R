#### hindlimb ####

setwd("~/Desktop/olivia hip height") # set working directory

# load data
mydata<-read.csv("mass_eff.csv", row.names="name")
mydata<- subset(mydata, order == "Carnivora" | order == "Artiodactyla"| order == "Rodentia")
mydata <- droplevels(mydata)


palette(c("#db362f", "#feb629", "#2fa3cc")) 
### hmmmm
summary(fit<-lm(eff~log10(mass)*order, mydata))
with(mydata,plot(eff~log10(mass),pch = 19, xlim = c(-2.3, 3.9), ylim = c(0.45,.96), col = mydata$order, font.lab = 2, las = 1, ylab = NA, xlab = NA))
title(ylab=expression(bold(paste("Effective hindlimb length"))),
      xlab=expression(bold(paste("Log10(body mass)"))),
      line=2.5)

palette(c("#c34214", "#fe9929", "#2c7fb8")) 
# kooking around
A1 <- subset(mydata, order == "Artiodactyla")
A1 <- droplevels(A1)
A2 <- subset(mydata, order == "Carnivora")
A2 <- droplevels(A2)
A3 <- subset(mydata, order == "Rodentia")
A3 <- droplevels(A3)


### A1
# have to do this for each one...fuuuu
summary(fit<-lm(eff~log10(mass), A1))
# Lets create a plot with separate lines/CI's for rodentia, carnivora and artiodactyla
preds <- expand.grid(mass = seq(min(A1$mass), max(A1$mass)))
#predict the fits
fits <- as.data.frame(predict(fit, newdata = preds, se.fit = TRUE)) # give me fits and their standard errors
# must turn it in to a data.frame 
# add confidence limits
fits$upr <- fits$fit + (2*fits$se.fit) # approximate upper confidence limit
fits$lwr <- fits$fit - (2*fits$se.fit) # approximate lower confidence limit
fits$se.fit <- fits$residual.scale <- NULL # delete the two variables I dont want
pdat <- cbind(preds, fits) # Combine together the predictors and predictions

lines(log10(pdat$mass), pdat$fit, lty = "solid", col = 1, lwd = 2.5)
lines(log10(pdat$mass), pdat$lwr, lty = "dotted", col = 1, lwd = 2.5)
lines(log10(pdat$mass), pdat$upr, lty = "dotted", col = 1, lwd = 2.5)


### A2
# have to do this for each one...fuuuu
summary(fit<-lm(eff~log10(mass), A2))
# Lets create a plot with separate lines/CI's for rodentia, carnivora and artiodactyla
preds <- expand.grid(mass = seq(min(A2$mass), max(A2$mass)))
#predict the fits
fits <- as.data.frame(predict(fit, newdata = preds, se.fit = TRUE)) # give me fits and their standard errors
# must turn it in to a data.frame 
# add confidence limits
fits$upr <- fits$fit + (2*fits$se.fit) # approximate upper confidence limit
fits$lwr <- fits$fit - (2*fits$se.fit) # approximate lower confidence limit
fits$se.fit <- fits$residual.scale <- NULL # delete the two variables I dont want
pdat <- cbind(preds, fits) # Combine together the predictors and predictions

lines(log10(pdat$mass), pdat$fit, lty = "solid", col = 2, lwd = 2.5)
lines(log10(pdat$mass), pdat$lwr, lty = "dotted", col = 2, lwd = 2.5)
lines(log10(pdat$mass), pdat$upr, lty = "dotted", col = 2, lwd = 2.5)

### A3
# have to do this for each one...fuuuu
summary(fit<-lm(eff~log10(mass), A3))
# Lets create a plot with separate lines/CI's for rodentia, carnivora and artiodactyla
preds <- expand.grid(mass = seq(min(A3$mass), max(A3$mass)))
#predict the fits
fits <- as.data.frame(predict(fit, newdata = preds, se.fit = TRUE)) # give me fits and their standard errors
# must turn it in to a data.frame 
# add confidence limits
fits$upr <- fits$fit + (2*fits$se.fit) # approximate upper confidence limit
fits$lwr <- fits$fit - (2*fits$se.fit) # approximate lower confidence limit
fits$se.fit <- fits$residual.scale <- NULL # delete the two variables I dont want
pdat <- cbind(preds, fits) # Combine together the predictors and predictions

lines(log10(pdat$mass), pdat$fit, lty = "solid", col = 3, lwd = 2.5)
lines(log10(pdat$mass), pdat$lwr, lty = "dotted", col = 3, lwd = 2.5)
lines(log10(pdat$mass), pdat$upr, lty = "dotted", col = 3, lwd = 2.5)

palette(c("#db362f", "#feb629", "#2fa3cc")) 
legend(-2.4,0.99, legend = levels(mydata$order), col = 1:3, pch = 19, bty = "n", cex = 0.9) 
palette(c("#c34214", "#fe9929", "#2c7fb8")) 
legend(-2.495,0.99, legend = levels(mydata$order), col = 1:3, lty = "solid", bty = "n", text.col = blank, lwd = 2.5, seg.len=1, cex = 0.9) 

text(3.1, 0.725, expression(bold(paste("Y = 0.0451x + 0.783"))), cex = 0.9, col = "#c34214")
text(3.1, 0.7, expression(bold(paste("R", ''^{2}, "= 0.429", sep = ""))), cex = 0.9, col = "#c34214")

text(2.4, 0.625, expression(bold(paste("Y = 0.0627x + 0.729"))), cex = 0.9, col = "#fe9929")
text(2.4, 0.6, expression(bold(paste("R", ''^{2}, "= 0.395", sep = ""))), cex = 0.9, col = "#fe9929")

text(1.7, 0.525, expression(bold(paste("Y = 0.0448x + 0.647"))), cex = 0.9, col = "#2c7fb8")
text(1.7, 0.5, expression(bold(paste("R", ''^{2}, "= 0.25", sep = ""))), cex = 0.9, col = "#2c7fb8")

library(png)
library(RCurl)
Arturl<-"http://phylopic.org/assets/images/submissions/e38baba4-adc9-4e58-bbf8-fb4794982e1b.512.png"
Carurl<-"http://phylopic.org/assets/images/submissions/b9d5547b-773c-46c8-841a-0846ff7f09ab.512.png"
Rodurl<-"http://phylopic.org/assets/images/submissions/81930c02-5f26-43f7-9c19-e9831e780e53.512.png"
Art_logo <- readPNG(getURLContent(Arturl))
Car_logo <- readPNG(getURLContent(Carurl))
Rod_logo <- readPNG(getURLContent(Rodurl))
logoing_func<-function(logo, x, y, size){
  dims<-dim(logo)[1:2] #number of x-y pixels for the logo (aspect ratio)
  AR<-dims[1]/dims[2]
  par(usr=c(0, 1, 0, 1))
  rasterImage(logo, x-(size/2), y-(AR*size/2), x+(size/2), y+(AR*size/2), interpolate=TRUE)
}
logoing_func(Art_logo, x=0.86, y=0.625, size=0.1)
logoing_func(Car_logo, x=0.75, y=0.415, size=0.1)
logoing_func(Rod_logo, x=0.64, y=0.23, size=0.1)

#### forelimb ####

setwd("~/Desktop/olivia hip height") # set working directory

# load data
mydata<-read.csv("mass_eff_F.csv", row.names="name")
mydata<- subset(mydata, order == "Carnivora" | order == "Artiodactyla"| order == "Rodentia")
mydata <- droplevels(mydata)


palette(c("#db362f", "#feb629", "#2fa3cc")) 
### hmmmm
summary(fit<-lm(eff~log10(mass)*order, mydata))
with(mydata,plot(eff~log10(mass),pch = 19, col = mydata$order, xlim = c(-2.3, 3.9), ylim = c(0.45,.96), font.lab = 2, las = 1, ylab = NA, xlab = NA))
title(ylab=expression(bold(paste("Effective forelimb length"))),
      xlab=expression(bold(paste("Log10(body mass)"))),
      line=2.5)

palette(c("#c34214", "#fe9929", "#2c7fb8")) 
# kooking around
A1 <- subset(mydata, order == "Artiodactyla")
A1 <- droplevels(A1)
A2 <- subset(mydata, order == "Carnivora")
A2 <- droplevels(A2)
A3 <- subset(mydata, order == "Rodentia")
A3 <- droplevels(A3)


### A1
# have to do this for each one...fuuuu
summary(fit<-lm(eff~log10(mass), A1))
# Lets create a plot with separate lines/CI's for rodentia, carnivora and artiodactyla
preds <- expand.grid(mass = seq(min(A1$mass), max(A1$mass)))
#predict the fits
fits <- as.data.frame(predict(fit, newdata = preds, se.fit = TRUE)) # give me fits and their standard errors
# must turn it in to a data.frame 
# add confidence limits
fits$upr <- fits$fit + (2*fits$se.fit) # approximate upper confidence limit
fits$lwr <- fits$fit - (2*fits$se.fit) # approximate lower confidence limit
fits$se.fit <- fits$residual.scale <- NULL # delete the two variables I dont want
pdat <- cbind(preds, fits) # Combine together the predictors and predictions

lines(log10(pdat$mass), pdat$fit, lty = "solid", col = 1, lwd = 2.5)
lines(log10(pdat$mass), pdat$lwr, lty = "dotted", col = 1, lwd = 2.5)
lines(log10(pdat$mass), pdat$upr, lty = "dotted", col = 1, lwd = 2.5)


### A2
# have to do this for each one...fuuuu
summary(fit<-lm(eff~log10(mass), A2))
# Lets create a plot with separate lines/CI's for rodentia, carnivora and artiodactyla
preds <- expand.grid(mass = seq(min(A2$mass), max(A2$mass)))
#predict the fits
fits <- as.data.frame(predict(fit, newdata = preds, se.fit = TRUE)) # give me fits and their standard errors
# must turn it in to a data.frame 
# add confidence limits
fits$upr <- fits$fit + (2*fits$se.fit) # approximate upper confidence limit
fits$lwr <- fits$fit - (2*fits$se.fit) # approximate lower confidence limit
fits$se.fit <- fits$residual.scale <- NULL # delete the two variables I dont want
pdat <- cbind(preds, fits) # Combine together the predictors and predictions

lines(log10(pdat$mass), pdat$fit, lty = "solid", col = 2, lwd = 2.5)
lines(log10(pdat$mass), pdat$lwr, lty = "dotted", col = 2, lwd = 2.5)
lines(log10(pdat$mass), pdat$upr, lty = "dotted", col = 2, lwd = 2.5)

### A3
# have to do this for each one...fuuuu
summary(fit<-lm(eff~log10(mass), A3))
# Lets create a plot with separate lines/CI's for rodentia, carnivora and artiodactyla
preds <- expand.grid(mass = seq(min(A3$mass), max(A3$mass)))
#predict the fits
fits <- as.data.frame(predict(fit, newdata = preds, se.fit = TRUE)) # give me fits and their standard errors
# must turn it in to a data.frame 
# add confidence limits
fits$upr <- fits$fit + (2*fits$se.fit) # approximate upper confidence limit
fits$lwr <- fits$fit - (2*fits$se.fit) # approximate lower confidence limit
fits$se.fit <- fits$residual.scale <- NULL # delete the two variables I dont want
pdat <- cbind(preds, fits) # Combine together the predictors and predictions

lines(log10(pdat$mass), pdat$fit, lty = "solid", col = 3, lwd = 2.5)
lines(log10(pdat$mass), pdat$lwr, lty = "dotted", col = 3, lwd = 2.5)
lines(log10(pdat$mass), pdat$upr, lty = "dotted", col = 3, lwd = 2.5)

palette(c("#db362f", "#feb629", "#2fa3cc")) 
legend(-2.4,0.99, legend = levels(mydata$order), col = 1:3, pch = 19, bty = "n", cex = 0.9) 
palette(c("#c34214", "#fe9929", "#2c7fb8")) 
legend(-2.495,0.99, legend = levels(mydata$order), col = 1:3, lty = "solid", bty = "n", text.col = blank, lwd = 2.5, seg.len=1, cex = 0.9) 

text(3.1, 0.725, expression(bold(paste("Y = 0.0148x + 0.837"))), cex = 0.9, col = "#c34214")
text(3.1, 0.7, expression(bold(paste("R", ''^{2}, "= 0.0691", sep = ""))), cex = 0.9, col = "#c34214")

text(2.4, 0.625, expression(bold(paste("Y = 0.0435x + 0.758"))), cex = 0.9, col = "#fe9929")
text(2.4, 0.6, expression(bold(paste("R", ''^{2}, "= 0.198", sep = ""))), cex = 0.9, col = "#fe9929")

text(1.7, 0.525, expression(bold(paste("Y = 0.0258x + 0.75"))), cex = 0.9, col = "#2c7fb8")
text(1.7, 0.5, expression(bold(paste("R", ''^{2}, "= 0.145", sep = ""))), cex = 0.9, col = "#2c7fb8")

library(png)
library(RCurl)
Arturl<-"http://phylopic.org/assets/images/submissions/e38baba4-adc9-4e58-bbf8-fb4794982e1b.512.png"
Carurl<-"http://phylopic.org/assets/images/submissions/b9d5547b-773c-46c8-841a-0846ff7f09ab.512.png"
Rodurl<-"http://phylopic.org/assets/images/submissions/81930c02-5f26-43f7-9c19-e9831e780e53.512.png"
Art_logo <- readPNG(getURLContent(Arturl))
Car_logo <- readPNG(getURLContent(Carurl))
Rod_logo <- readPNG(getURLContent(Rodurl))
logoing_func<-function(logo, x, y, size){
  dims<-dim(logo)[1:2] #number of x-y pixels for the logo (aspect ratio)
  AR<-dims[1]/dims[2]
  par(usr=c(0, 1, 0, 1))
  rasterImage(logo, x-(size/2), y-(AR*size/2), x+(size/2), y+(AR*size/2), interpolate=TRUE)
}

logoing_func(Art_logo, x=0.86, y=0.625, size=0.1)
logoing_func(Car_logo, x=0.75, y=0.415, size=0.1)
logoing_func(Rod_logo, x=0.64, y=0.23, size=0.1)


