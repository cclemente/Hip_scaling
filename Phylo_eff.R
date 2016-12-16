
.libPaths("D:/R code")

install.packages("phytools")
library(ape)
library(geiger)
#library(nlme)
library(phytools)
#library(picante)
#library(MASS)



setwd("H:/olivia hip height")

#loading data
mydata<-read.csv("mass_eff.csv", row.names="name")


#load tree
myo<-read.nexus("nature05634-s2.txt")

myo1<-myo[[1]]

#prune tree
matches<-name.check(myo1,mydata)

pruned<-drop.tip(myo1,matches$tree_not_data)

plot(pruned)

#order the data
trait_order<-data.frame(mydata[pruned$tip.label,])
rownames(trait_order)<-pruned$tip.label
head(trait_order)
head(pruned$tip.label)

cols=c("red","blue","green")

#Question 1: IS there a relationship betweenn mass and eff?? 
plot(eff~log10(mass),mydata, pch=20, col=cols[mydata$diet])
summary(fit<-lm(eff~log10(mass),mydata))
abline(fit, col="red", lwd=2)

#question 2 
#is there an effect of phylogeny? 

mass<-log10(as.matrix(trait_order[,2]))#head(mass)
rownames(mass)<-rownames(trait_order)
massV <- mass[,1] #need this step....
head(mass)
colnames(mass)<-"mass"
head(massV)
phylosig(pruned, massV, method="lambda", test=TRUE, nsim=10000)


eff<-as.matrix(trait_order[,3])#head(eff)
rownames(eff)<-rownames(trait_order)
effV <- eff[,1] #need this step....
head(eff)
colnames(eff)<-"eff"
head(effV)
phylosig(pruned, effV, method="lambda", test=TRUE, nsim=10000)

diet<-as.matrix(trait_order[,4])#head(eff)
rownames(diet)<-rownames(trait_order)
dietV <- diet[,1] #need this step....
head(diet)
colnames(diet)<-"diet"
head(dietV)
phylosig(pruned, dietV, method="lambda", test=TRUE, nsim=10000)

#performs phylogenetic RMA regression.
output<-phyl.RMA(massV, effV, pruned)
abline(output$RMA.beta[1],output$RMA.beta[2],col="grey")


#no idea what this does. 
phyl.cca(pruned, mass, eff, lambda=1.0, fixed=TRUE)
nrow(massV)




#is there effect of diet?? 
#first get residuals from mass. 
#not run
#output$resid
#could also use phylo.resid

eff_resid<-output$resid[,1]
head(eff_resid)

#Phylogenetic ANOVA and post-hoc tests
phylANOVA(pruned, dietV, eff_resid, nsim=1000, posthoc=TRUE, p.adj="holm")

boxplot(eff_resid~dietV)



