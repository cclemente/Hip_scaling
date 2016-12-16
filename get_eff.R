setwd("H:/olivia hip height")

name<-read.csv("species_names.csv")
#name$fname[1]

jj = 1

for (jj in 1:length(name)){
  setwd(paste0("H:/olivia hip height","/",name$fname[jj]))
  data1<-read.csv("outdata.csv")
  
  #rerun after each species
  cvalues_mean= matrix(nrow = 0, ncol = 8)
  colnames(cvalues_mean)<-c('name', 'image','thigh','shank','meta','toe', 'HH','eff')
  
  #rerun after each species
  cvalues_raw= matrix(nrow = 0, ncol = 8)
  colnames(cvalues_raw)<-c('name', 'image','thigh','shank','meta','toe', 'HH','eff')
  
  #rerun after each species
  cvalues= matrix(nrow = 0, ncol = 8)
  colnames(cvalues)<-c('name', 'image','thigh','shank','meta','toe', 'HH','eff')
  
  
  for (ii in 1:nrow(data1)){
  thigh=sqrt((data1$hx1[ii]-data1$hx2[ii])^2+(data1$hy1[ii]-data1$hy2[ii])^2)
  shank=sqrt((data1$hx2[ii]-data1$hx3[ii])^2+(data1$hy2[ii]-data1$hy3[ii])^2)
  meta=sqrt((data1$hx3[ii]-data1$hx4[ii])^2+(data1$hy3[ii]-data1$hy4[ii])^2)
  toe=sqrt((data1$hx4[ii]-data1$hx5[ii])^2+(data1$hy4[ii]-data1$hy5[ii])^2)
  HH=sqrt((data1$hx1[ii]-data1$hx5[ii])^2+(data1$hy1[ii]-data1$hy5[ii])^2)
  eff=HH/(thigh+shank+meta+toe)
  
  temp<-c(name$fname[jj],data1$image[ii],thigh,shank,meta,toe,HH,eff)
  cvalues<-rbind(cvalues,temp)
  rownames(cvalues) <- NULL
  }
  
  cvalues<-as.data.frame(cvalues)
  Spout<-colMeans(cvalues)
  Spout$name<-as.character(gsub(" ", "_", name$fname[jj]))
  Spout<-as.data.frame(Spout)
  
  cvalues_mean<-rbind(cvalues_mean,Spout)
  cvalues_raw<-rbind(cvalues_raw,cvalues)
  
}