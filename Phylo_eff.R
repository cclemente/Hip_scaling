# download the packages
library(ape)
library(geiger)
#library(nlme)
library(phytools)
#library(picante)
#library(MASS)

#### hindlimb ####

setwd("~/Desktop/olivia hip height") # set working directory

# load data
mydata<-read.csv("mass_eff.csv", row.names="name")

# load tree
myo<-read.nexus("nature05634-s2.txt")
myo1<-myo[[1]]

# prune tree by the species in mydata
matches<-name.check(myo1,mydata)
pruned<-drop.tip(myo1,matches$tree_not_data)


# Plot the pruned tree
plot(pruned, cex = 0.5)

# order the data in mydata to be in the same sequence as in the tree
trait_order<-data.frame(mydata[pruned$tip.label,])
rownames(trait_order)<-pruned$tip.label
head(trait_order) # check that this worked
head(pruned$tip.label) # by comparing it to this

# make a colour palette
palette(c("#2c7fb8", "#238b45", "#feb24c", "#de2d26", "#88419d")) 

#### Question 1: IS there a relationship betweenn mass and eff?? ####
# Scatterplot- same as the one we looked at previously, but coloured by diet
summary(fit<-lm(eff~log10(mass),mydata)) # create a linear model, save the intercept and slope to 'fit'
plot(eff~log10(mass),mydata, pch=19, col=mydata$diet, font.lab = 2, las = 1, ylab = expression(bold(paste("Effective hindlimb length"))), xlab = expression(bold(paste("Log10(body mass)"))))
mydata$diet1 = factor(mydata$diet, levels = c('1','2', '3'),labels = c("Carnivore", "Herbivore", "Omnivore"))
legend("topleft", legend=levels(mydata$diet1), pch = 19, bty = "n", col = c(1:3))
abline(fit, col="red", lwd=2)
LMresid <- fit$residuals

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

# is there an effect of phylogeny on diet? 
diet<-as.matrix(trait_order[,4])# head(eff), Make a matrix of the 4th column of trait_order (the diet column)
rownames(diet)<-rownames(trait_order) # Add the species names to the diet matrix as row names
dietV <- diet[,1] # need this step.... turn 'diet' into a vector instead of dataframe
colnames(diet)<-"diet" # make the column name for the diet data 'diet'
head(diet) # make sure it worked by checking the original diet dataframe
head(dietV) # against the massV dataframe
# phylosig randomises the massV data throughout the phylogenetic tree 10000 times to work out how likely
# it is that the diet is in this order

PHY <- phylosig(pruned, dietV, method="lambda", test=TRUE, nsim=10000, start=c(1,1))
# very unlikely that effective limb length is in this order by chance- the p-value is 6.614822e-43
x<-anc.Bayes(pruned, massV, ngen = 10000)
estimates<-colMeans(x[21:nrow(x),])

x<-anc.Bayes(pruned, effV, ngen = 10000)
estimates<-colMeans(x[21:nrow(x),])


# Performs phylogenetic RMA regression.
# Phyl.RMA accounts for the effect of phylogeny on effective hip height as a function of mass. 
output<-phyl.RMA(massV, effV, pruned) 
# The intercept is now 0.6894501 (lower than before accounting for phylogeny)
# and the slope is 0.1012439 (steeper than before accounting for phylogeny) meaning that phylogeny
# lessens the effect of mass on effective hip height. p-value is significant: 2.611441e-52
# add the new line to the original plot
abline(output$RMA.beta[1],output$RMA.beta[2],col="purple", lwd =2)

# no idea what this does.... 
phyl.cca(pruned, mass, eff, lambda=1.0, fixed=TRUE)
nrow(massV)

#### Question 3: is there an effect of diet?? ####
# get residuals from 'output' list
eff_resid<-output$resid[,1]
head(eff_resid) # check it

# Phylogenetic ANOVA and post-hoc tests
phylANOVA(pruned, dietV, eff_resid, nsim=1000, posthoc=TRUE, p.adj="holm")

# The effect of diet on the residual distribution is not significant: F = 4.878, p = 0.349
boxplot(LMresV~dietV, col=c(1,2,3), xaxt = 'n', pch = 20, font.lab = 2, whisklty = "solid", staplelwd = 2, whisklwd = 2, boxlwd = 2,
        las = 1, ylab = NA, xlab = NA) 
Axis(side=1, at = c(1,2,3), labels=c("Carnivore", "Herbivore", "Omnivore"))
title(ylab=expression(bold(paste("Hindlimb residual distribution"))),
      xlab=expression(bold(paste("Diet"))),
      line=2.5)
# plot it - easy to see that theres no significant effect of diet on the residual distribution, eg. carnivores
# ('1') seem to be slightly more upright (compared to the linear model trend line), but not significantly
# different from herbivores or omnivores.
### 1####
# and now checking residual distribution for habitat type
# is there an effect of phylogeny on habitat preference? 
habitat<-as.matrix(trait_order[,7])
rownames(habitat)<-rownames(trait_order) 
habitatV <- habitat[,1]
colnames(habitat)<-"habitat" 
head(habitat) 
head(habitatV)
phylosig(pruned, habitatV, method="lambda", test=TRUE, nsim=10000)

phylANOVA(pruned, habitatV, eff_resid, nsim=1000, posthoc=TRUE, p.adj="holm")
# The effect of diet on the residual distribution is not significant: F = 4.878, p = 0.349
boxplot(eff_resid~habitat, col=c(1,2,3,4,5), xaxt = 'n', pch = 20, font.lab = 2, whisklty = "solid", staplelwd = 2, whisklwd = 2, boxlwd = 2,
        las = 1, ylab = NA, xlab = NA) 

Axis(side=1, at = c(1,2,3,4,5), labels=c("Forest", "Canopy", "Desert", "Plains", "Alpine"))
title(ylab=expression(bold(paste("Hindlimb residual distribution"))),
      xlab=expression(bold(paste("Habitat"))),
      line=2.5)

# is there an effect of phylogeny on fac? 
fac<-as.matrix(trait_order[,8])
rownames(fac)<-rownames(trait_order) 
facV <- fac[,1]
colnames(fac)<-"fac" 
head(fac)
head(facV)
phylosig(pruned, facV, method="lambda", test=TRUE, nsim=10000)

phylANOVA(pruned, facV, eff_resid, nsim=1000, posthoc=TRUE, p.adj="holm")
boxplot(eff_resid~fac, col=c(1,2,3), xaxt = 'n', pch = 20, font.lab = 2, whisklty = "solid", staplelwd = 2, whisklwd = 2, boxlwd = 2,
        las = 1, ylab = NA, xlab = NA) 
Axis(side=1, at = c(1,2,3), labels=c("Fossorial", "Arboreal", "Cursorial"))
title(ylab=expression(bold(paste("Hindlimb residual distribution"))),
      xlab= expression(bold(paste("Predominant locomotion"))),
      line=2.5)

#### forelimb ####

setwd("~/Desktop/olivia hip height") # set working directory

# load data
mydata<-read.csv("mass_eff_F.csv", row.names="name")

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

# make a colour palette
cols=c("red","blue","green")

#### Question 1: IS there a relationship betweenn mass and eff?? ####
# Scatterplot- same as the one we looked at previously, but coloured by diet
summary(fit<-lm(eff~log10(mass),mydata)) # create a linear model, save the intercept and slope to 'fit'
plot(eff~log10(mass),mydata, pch=19, col=mydata$diet, font.lab = 2, las = 1, ylab = expression(bold(paste("Effective hind-limb length"))), xlab = expression(bold(paste("Log10(body mass)"))))
mydata$diet1 = factor(mydata$diet, levels = c('1','2', '3'),labels = c("Carnivore", "Herbivore", "Omnivore"))
legend("topleft", legend=levels(mydata$diet1), pch = 19, bty = "n", col = c(1:3))
abline(fit, col="red", lwd=2) # add a red abline to the data with 'fit's information
# There's an upward trend- a slope of 0.08, and the summary(lm) indicates that this is significant (p < 2.2e-16)

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

# is there an effect of phylogeny on diet? 
diet<-as.matrix(trait_order[,4])# head(eff), Make a matrix of the 4th column of trait_order (the diet column)
rownames(diet)<-rownames(trait_order) # Add the species names to the diet matrix as row names
dietV <- diet[,1] # need this step.... turn 'diet' into a vector instead of dataframe
colnames(diet)<-"diet" # make the column name for the diet data 'diet'
head(diet) # make sure it worked by checking the original diet dataframe
head(dietV) # against the massV dataframe
# phylosig randomises the massV data throughout the phylogenetic tree 10000 times to work out how likely
# it is that the diet is in this order
phylosig(pruned, dietV, method="lambda", test=TRUE, nsim=10000)
# very unlikely that effective limb length is in this order by chance- the p-value is 6.614822e-43

x<-anc.Bayes(pruned, effV, ngen = 10000)
estimates<-colMeans(x[21:nrow(x),])

# Performs phylogenetic RMA regression.
# Phyl.RMA accounts for the effect of phylogeny on effective hip height as a function of mass. 
output<-phyl.RMA(massV, effV, pruned) 
# The intercept is now 0.6894501 (lower than before accounting for phylogeny)
# and the slope is 0.1012439 (steeper than before accounting for phylogeny) meaning that phylogeny
# lessens the effect of mass on effective hip height. p-value is significant: 2.611441e-52
# add the new line to the original plot
abline(output$RMA.beta[1],output$RMA.beta[2],col="purple", lwd =2)

# no idea what this does.... 
phyl.cca(pruned, mass, eff, lambda=1.0, fixed=TRUE)
nrow(massV)

#### Question 3: is there an effect of diet?? ####
# get residuals from 'output' list
eff_resid<-output$resid[,1]
head(eff_resid) # check it

# Phylogenetic ANOVA and post-hoc tests
phylANOVA(pruned, dietV, eff_resid, nsim=1000, posthoc=TRUE, p.adj="holm")
# The effect of diet on the residual distribution is not significant: F = 4.878, p = 0.349
boxplot(eff_resid~diet, col=c(1,2,3), xaxt = 'n', pch = 20, font.lab = 2, whisklty = "solid", staplelwd = 2, whisklwd = 2, boxlwd = 2,
        las = 1, ylab = NA, xlab = NA) 
Axis(side=1, at = c(1,2,3), labels=c("Carnivore", "Herbivore", "Omnivore"))
title(ylab=expression(bold(paste("Forelimb residual distribution"))),
      xlab=expression(bold(paste("Diet"))),
      line=2.5)
# plot it - easy to see that theres no significant effect of diet on the residual distribution, eg. carnivores
# ('1') seem to be slightly more upright (compared to the linear model trend line), but not significantly
# different from herbivores or omnivores.

# is there an effect of phylogeny on habitat preference? 
habitat<-as.matrix(trait_order[,7])
rownames(habitat)<-rownames(trait_order) 
habitatV <- habitat[,1]
colnames(habitat)<-"diet" 
head(habitat) 
head(habitatV)
phylosig(pruned, habitatV, method="lambda", test=TRUE, nsim=10000)

phylANOVA(pruned, habitatV, eff_resid, nsim=1000, posthoc=TRUE, p.adj="holm")

boxplot(eff_resid~habitat, col=c(1,2,3,4,5), xaxt = 'n', pch = 20, font.lab = 2, whisklty = "solid", staplelwd = 2, whisklwd = 2, boxlwd = 2,
        las = 1, ylab = NA, xlab = NA) 
Axis(side=1, at = c(1,2,3,4,5), labels=c("Forest", "Canopy", "Desert", "Plains", "Alpine"))
title(ylab=expression(bold(paste("Forelimb residual distribution"))),
      xlab=expression(bold(paste("Habitat"))),
      line=2.5)
par()


#effect on fac?
fac<-as.matrix(trait_order[,8])
rownames(fac)<-rownames(trait_order) 
facV <- fac[,1]
colnames(fac)<-"fac" 
head(fac)
head(facV)
phylosig(pruned, facV, method="lambda", test=TRUE, nsim=10000)

phylANOVA(pruned, facV, eff_resid, nsim=1000, posthoc=TRUE, p.adj="holm")
boxplot(eff_resid~fac, col=c(1,2,3), xaxt = 'n', pch = 20, font.lab = 2, whisklty = "solid", staplelwd = 2, whisklwd = 2, boxlwd = 2,
        las = 1, ylab = NA, xlab = NA) 
Axis(side=1, at = c(1,2,3), labels=c("Fossorial", "Arboreal", "Cursorial"))
title(ylab=expression(bold(paste("Forelimb residual distribution"))),
      xlab= expression(bold(paste("Predominant locomotion"))),
      line=2.5)


