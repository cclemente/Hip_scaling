
#look for a relationship between size and EFF

setwd("H:/olivia hip height")

mass_data<-read.csv("mass_data5.csv")
eff_data<-read.csv("cvaluesmean.csv")

test<-paste0(as.character(mass_data$genus),"_",as.character(mass_data$species))

mass_data2<-cbind(mass_data,test)

tail(mass_data2)

qq=1

mass_eff<-data.frame(name=NA,mass=NA,eff=NA, diet=NA)
non_match<-data.frame(row_num=NA)

for (qq in 1:nrow(eff_data)){
ind<-which(as.character(mass_data2$test)==as.character(eff_data$name[qq]))
if (length(ind)==1){
temp<-data.frame(name=as.character(eff_data$name[qq]), mass=mass_data2$mass[ind],eff=eff_data$eff[qq],diet=eff_data$diet[qq])
mass_eff<-rbind(mass_eff,temp)
}
else
  non_match<-rbind(non_match,qq)  
}

mass_eff<-mass_eff[-1,]

write.csv(mass_eff,"mass_eff.csv")

plot(eff~log10(mass),mass_eff)
