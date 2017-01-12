install.packages("smatr")
library(smatr)
# download the packages
library(ape)
library(geiger)
#library(nlme)
library(phytools)
#library(picante)
#library(MASS)#### hindlimb ####

setwd("~/Desktop/olivia hip height") # set working directory

# load data
mydata<-read.csv("mass_eff.csv", row.names="name")

# accounting for phylogeny
# load tree
myo<-read.nexus("nature05634-s2.txt")
myo1<-myo[[1]]

# prune tree by the species in mydata
matches<-name.check(myo1,mydata)
pruned<-drop.tip(myo1,matches$tree_not_data)

# Plot the pruned tree
plot(pruned)

# order the data in mydata to be in the same sequence as in the tree
trait_order<-data.frame(mydata[pruned$tip.label,])
rownames(trait_order)<-pruned$tip.label
head(trait_order) # check that this worked
head(pruned$tip.label) # by comparing it to this

palette(c("#2c7fb8", "#238b45"))
summary(fit<-lm(eff~log10(mass), mydata))
with(mydata,plot(eff~log10(mass),pch = 19, col = "black", ylim = c(0.35,1), font.lab = 2, las = 1, ylab = NA, xlab = NA))
title(ylab=expression(bold(paste("Effective hindlimb length"))),
      xlab=expression(bold(paste("Log10(body mass)"))),
      line=2.5)

summary(fit<-sma(eff~log10(mass), mydata))

abline(fit, col="blue", lwd=2.5)

legend("topleft", legend = c("Linear regression", "RMA regression"), col = c("red", "#2fa3cc"), lty = "solid", bty = "n", lwd = 2.5, seg.len=1, cex = 0.9) 


text(2.6, 0.63, expression(bold(paste("Y = 0.132x + 0.716"))), cex = 0.9, col = "#2fa3cc")
text(2.6, 0.61, expression(bold(paste("R", ''^{2}, "= 0.444", sep = ""))), cex = 0.9, col = "#2fa3cc")

text(2.6, 0.53, expression(bold(paste("Y = 0.0758x + 0.69"))), cex = 0.9, col = "red")
text(2.6, 0.51, expression(bold(paste("R", ''^{2}, "= 0.615", sep = ""))), cex = 0.9, col = "red")


#### Question 2: ####
# is there an effect of phylogeny on mass? 
mass<-log10(as.matrix(trait_order[,2])) # head(mass), log10 the mass values in 'trait_order'
rownames(mass)<-rownames(trait_order) # Add the species names to the log10(mass) as row names
massV <- mass[,1] # need this step.... turn 'mass' into a vector instead of dataframe
colnames(mass)<-"mass" # make the column name for the mass data 'mass'
head(mass) # make sure it worked by checking the original mass dataframe
head(massV) # against the massV dataframe

# phylosig randomises the massV data throughout the phylogenetic tree 10000 times to work out how likely
# it is that the mass is in this order
phylosig(pruned, massV, method="lambda", test=TRUE, nsim=10000)
# very unlikely that mass is in this order by chance- the p-value is 8.591604e-55

# is there an effect of phylogeny on effective limb length? 
eff<-as.matrix(trait_order[,3]) # head(eff), Make a matrix of the 3rd column of trait_order (the eff column)
rownames(eff)<-rownames(trait_order) # Add the species names to the eff matrix as row names
effV <- eff[,1] # need this step.... turn 'eff' into a vector instead of dataframe
colnames(eff)<-"eff" # make the column name for the eff data 'eff'
head(eff) # make sure it worked by checking the original eff dataframe
head(effV) # against the massV dataframe

# phylosig randomises the massV data throughout the phylogenetic tree 10000 times to work out how likely
# it is that the effective limb length is in this order
phylosig(pruned, effV, method="lambda", test=TRUE, nsim=10000)
# very unlikely that effective limb length is in this order by chance- the p-value is 2.212986e-51

# Performs phylogenetic RMA regression.
# Phyl.RMA accounts for the effect of phylogeny on effective hip height as a function of mass. 
output<-phyl.RMA(massV, effV, pruned) 
# The intercept is now 0.6894501 (lower than before accounting for phylogeny)
# and the slope is 0.1012439 (steeper than before accounting for phylogeny) meaning that phylogeny
# lessens the effect of mass on effective hip height. p-value is significant: 2.611441e-52
# add the new line to the original plot
abline(output$RMA.beta[1],output$RMA.beta[2],col="#2fa3cc", lwd =2.5)

#### forelimb ####

setwd("~/Desktop/olivia hip height") # set working directory

# load data
mydata<-read.csv("mass_eff_F.csv", row.names="name")

# accounting for phylogeny
# load tree
myo<-read.nexus("nature05634-s2.txt")
myo1<-myo[[1]]

# prune tree by the species in mydata
matches<-name.check(myo1,mydata)
pruned<-drop.tip(myo1,matches$tree_not_data)

# Plot the pruned tree
plot(pruned)

# order the data in mydata to be in the same sequence as in the tree
trait_order<-data.frame(mydata[pruned$tip.label,])
rownames(trait_order)<-pruned$tip.label
head(trait_order) # check that this worked
head(pruned$tip.label) # by comparing it to this

palette(c("#2fa3cc", "#feb24c", "#238b45"))
summary(fit<-lm(eff~log10(mass), mydata))
with(mydata,plot(eff~log10(mass),pch = 19, col = "black", ylim = c(0.35,1), font.lab = 2, las = 1, ylab = NA, xlab = NA))
title(ylab=expression(bold(paste("Effective forelimb length"))),
      xlab=expression(bold(paste("Log10(body mass)"))),
      line=2.5)

summary(fit<-lm(eff~log10(mass), mydata))
abline(fit, col="red", lwd=2.5)

legend("topleft", legend = c("Linear regression", "RMA regression"), col = c("red", "#2fa3cc"), lty = "solid", bty = "n", lwd = 2.5, seg.len=1, cex = 0.9) 

text(2.6, 0.63, expression(bold(paste("Y = 0.0962x + 0.743"))), cex = 0.9, col = "#2fa3cc")
text(2.6, 0.61, expression(bold(paste("R", ''^{2}, "= 0.431", sep = ""))), cex = 0.9, col = "#2fa3cc")

text(2.6, 0.53, expression(bold(paste("Y = 0.0427x + 0.766"))), cex = 0.9, col = "red")
text(2.6, 0.51, expression(bold(paste("R", ''^{2}, "= 0.454", sep = ""))), cex = 0.9, col = "red")

#### Question 2: ####
# is there an effect of phylogeny on mass? 
mass<-log10(as.matrix(trait_order[,2])) # head(mass), log10 the mass values in 'trait_order'
rownames(mass)<-rownames(trait_order) # Add the species names to the log10(mass) as row names
massV <- mass[,1] # need this step.... turn 'mass' into a vector instead of dataframe
colnames(mass)<-"mass" # make the column name for the mass data 'mass'
head(mass) # make sure it worked by checking the original mass dataframe
head(massV) # against the massV dataframe

# phylosig randomises the massV data throughout the phylogenetic tree 10000 times to work out how likely
# it is that the mass is in this order
phylosig(pruned, massV, method="lambda", test=TRUE, nsim=10000)
# very unlikely that mass is in this order by chance- the p-value is 8.591604e-55

# is there an effect of phylogeny on effective limb length? 
eff<-as.matrix(trait_order[,3]) # head(eff), Make a matrix of the 3rd column of trait_order (the eff column)
rownames(eff)<-rownames(trait_order) # Add the species names to the eff matrix as row names
effV <- eff[,1] # need this step.... turn 'eff' into a vector instead of dataframe
colnames(eff)<-"eff" # make the column name for the eff data 'eff'
head(eff) # make sure it worked by checking the original eff dataframe
head(effV) # against the massV dataframe

# phylosig randomises the massV data throughout the phylogenetic tree 10000 times to work out how likely
# it is that the effective limb length is in this order
phylosig(pruned, effV, method="lambda", test=TRUE, nsim=10000)
# very unlikely that effective limb length is in this order by chance- the p-value is 2.212986e-51

# Performs phylogenetic RMA regression.
# Phyl.RMA accounts for the effect of phylogeny on effective hip height as a function of mass. 
output<-phyl.RMA(massV, effV, pruned) 
# The intercept is now 0.6894501 (lower than before accounting for phylogeny)
# and the slope is 0.1012439 (steeper than before accounting for phylogeny) meaning that phylogeny
# lessens the effect of mass on effective hip height. p-value is significant: 2.611441e-52
# add the new line to the original plot
abline(output$RMA.beta[1],output$RMA.beta[2],col="#2fa3cc", lwd =2.5)
