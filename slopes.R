#### hindlimb get values ####

setwd("~/Desktop/olivia hip height") # set working directory

# load data
mydata<-read.csv("mass_eff.csv")
summary(ma(eff~log10(mass), mydata))


Orders<-data.frame(order=NA,slope=NA)
tt <- table(mydata$order)
mydata1 <- subset(mydata, order %in% names(tt[tt > 2]))
mydata1 <- droplevels(mydata1)
levels(mydata1$order)
for(b in 1:14) 
{summary(lmResult <- ma(eff~log10(mass), subset(mydata1, order == levels(mydata1$order)[b])))
      slope <- coef(lmResult)["slope"]
      temp<-c(mydata1$order[b],slope)
      Orders<-rbind(Orders,temp)}
Orders<-Orders[-1,]
Orders$type <- "order"
Orders$level <- levels(mydata1$order)
Orders<-na.omit(Orders)
Orders<-Orders[,-1]

Families<-data.frame(family=NA,slope=NA)
tt <- table(mydata$family)
mydata1 <- subset(mydata, family %in% names(tt[tt > 2]))
mydata1 <- droplevels(mydata1)
levels(mydata1$family)
for(b in 1:46) 
{summary(lmResult <- ma(eff~log10(mass), subset(mydata1, family == levels(mydata1$family)[b])))
  slope <- coef(lmResult)["slope"]
  temp<-c(mydata1$family[b],slope)
  Families<-rbind(Families,temp)}
Families<-Families[-1,]
Families$type <- "family"
Families$level <- levels(mydata1$family)
Families<-na.omit(Families)
Families<-Families[,-1]
                 
Genera<-data.frame(genus=NA,slope=NA)
tt <- table(mydata$genus)
mydata1 <- subset(mydata, genus %in% names(tt[tt > 2]))
mydata1 <- droplevels(mydata1)
levels(mydata1$genus)
for(b in 1:22) 
{summary(lmResult <- ma(eff~log10(mass), subset(mydata1, genus == levels(mydata1$genus)[b])))
  slope <- coef(lmResult)["slope"]
  temp<-c(mydata1$genus[b],slope)
  Genera<-rbind(Genera,temp)}
Genera<-Genera[-1,]
Genera$type <- "Genus"
Genera$level <- levels(mydata1$genus)
Genera<-subset(Genera, slope < 5)
Genera<-na.omit(Genera)
Genera<-Genera[,-1]

write.csv(Genera,"F1.csv")
write.csv(Families,"F2.csv")
write.csv(Orders,"F3.csv")

#### hindlimb plot ####
mydata<-read.csv("F10.csv")
mydata$type <- factor(mydata$type, levels(mydata$type)[c(1, 4, 2, 3)]) 
boxplot(slope~type, mydata)
boxplot(mydata$slope~mydata$type, col="grey60", xaxt = 'n', pch = 20, font.lab = 2, whisklty = "solid", staplelwd = 2, whisklwd = 2, boxlwd = 2,
        las = 1, ylab = NA, xlab = NA) 
Axis(side=1, at = c(1,2,3,4), labels=c("Class", "Order", "Family", "Genus"), font = 2)
title(ylab=expression(bold(paste("Hindlimb posture scaling (RMA regression model)"))),
      xlab=NA,
      line=2.5)

#### forelimb get values ####
             
setwd("~/Desktop/olivia hip height") # set working directory

# load data
mydata<-read.csv("mass_eff_F.csv")
summary(ma(eff~log10(mass), mydata))


Orders<-data.frame(order=NA,slope=NA)
tt <- table(mydata$order)
mydata1 <- subset(mydata, order %in% names(tt[tt > 2]))
mydata1 <- droplevels(mydata1)
levels(mydata1$order)
for(b in 1:14) 
{summary(lmResult <- ma(eff~log10(mass), subset(mydata1, order == levels(mydata1$order)[b])))
  slope <- coef(lmResult)["slope"]
  temp<-c(mydata1$order[b],slope)
  Orders<-rbind(Orders,temp)}
Orders<-Orders[-1,]
Orders$type <- "order"
Orders$level <- levels(mydata1$order)
Orders<-na.omit(Orders)
Orders<-Orders[,-1]

Families<-data.frame(family=NA,slope=NA)
tt <- table(mydata$family)
mydata1 <- subset(mydata, family %in% names(tt[tt > 2]))
mydata1 <- droplevels(mydata1)
levels(mydata1$family)
for(b in 1:46) 
{summary(lmResult <- ma(eff~log10(mass), subset(mydata1, family == levels(mydata1$family)[b])))
  slope <- coef(lmResult)["slope"]
  temp<-c(mydata1$family[b],slope)
  Families<-rbind(Families,temp)}
Families<-Families[-1,]
Families$type <- "family"
Families$level <- levels(mydata1$family)
Families<-na.omit(Families)
Families<-Families[,-1]

Genera<-data.frame(genus=NA,slope=NA)
tt <- table(mydata$genus)
mydata1 <- subset(mydata, genus %in% names(tt[tt > 2]))
mydata1 <- droplevels(mydata1)
levels(mydata1$genus)
for(b in 1:22) 
{summary(lmResult <- ma(eff~log10(mass), subset(mydata1, genus == levels(mydata1$genus)[b])))
  slope <- coef(lmResult)["slope"]
  temp<-c(mydata1$genus[b],slope)
  Genera<-rbind(Genera,temp)}
Genera<-Genera[-1,]
Genera$type <- "Genus"
Genera$level <- levels(mydata1$genus)
Genera<-subset(Genera, slope < 5)
Genera<-na.omit(Genera)
Genera<-Genera[,-1]

write.csv(Genera,"F4.csv")
write.csv(Families,"F5.csv")
write.csv(Orders,"F6.csv")

#### forelimb plot ####
mydata<-read.csv("F20.csv")
mydata$type <- factor(mydata$type, levels(mydata$type)[c(1, 4, 2, 3)]) 
boxplot(slope~type, mydata)
boxplot(mydata$slope~mydata$type, col="grey60", xaxt = 'n', pch = 20, font.lab = 2, whisklty = "solid", staplelwd = 2, whisklwd = 2, boxlwd = 2,
        las = 1, ylab = NA, xlab = NA) 
Axis(side=1, at = c(1,2,3,4), labels=c("Class", "Order", "Family", "Genus"), font = 2)
title(ylab=expression(bold(paste("Forelimb posture scaling (RMA regression model)"))),
      xlab=NA,
      line=2.5)
