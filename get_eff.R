#### hindlimb ####
# Set working directory
setwd("H:/olivia hip height")

name<-read.csv("species_names.csv")
#name$fname[1]

# jj = 1

# Rerun after each species- sets the column names for the mean data
cvalues_mean= matrix(nrow = 0, ncol = 8)
colnames(cvalues_mean)<-c('name', 'image','thigh','shank','meta','toe', 'HH','eff')

# Rerun after each species- sets the column names for the raw data
cvalues_raw= matrix(nrow = 0, ncol = 8)
colnames(cvalues_raw)<-c('name', 'image','thigh','shank','meta','toe', 'HH','eff')

# For-loop setting the working directory to the folder for each species
for (jj in 1:length(name)){
  setwd(paste0("H:/olivia hip height","/",name$fname[jj]))
  data1<-read.csv("outdata.csv")
  
  # Rerun after each species- sets the column names for the data
  cvalues= matrix(nrow = 0, ncol = 8)
  colnames(cvalues)<-c('name', 'image','thigh','shank','meta','toe', 'HH','eff')
  
  # For-loop working out the length between each point for each photo
  for (ii in 1:nrow(data1)){
  thigh=sqrt((data1$hx1[ii]-data1$hx2[ii])^2+(data1$hy1[ii]-data1$hy2[ii])^2)
  shank=sqrt((data1$hx2[ii]-data1$hx3[ii])^2+(data1$hy2[ii]-data1$hy3[ii])^2)
  meta=sqrt((data1$hx3[ii]-data1$hx4[ii])^2+(data1$hy3[ii]-data1$hy4[ii])^2)
  toe=sqrt((data1$hx4[ii]-data1$hx5[ii])^2+(data1$hy4[ii]-data1$hy5[ii])^2)
  HH=sqrt((data1$hx1[ii]-data1$hx5[ii])^2+(data1$hy1[ii]-data1$hy5[ii])^2) # including hip height
  eff=HH/(thigh+shank+meta+toe) # and effective limb length
    
  # save these to a temporary file, 'temp'
  temp<-c(name$fname[jj],data1$image[ii],thigh,shank,meta,toe,HH,eff) 
  cvalues<-rbind(cvalues,temp) # Add the temp data to the cvalues matrix
  rownames(cvalues) <- NULL
  }
  
  cvalues<-as.data.frame(cvalues) # make cvalues a dataframe
  Spout<-colMeans(cvalues) # find the means for each column (for each set of photos), save as Spout
  Spout$name<-as.character(gsub(" ", "_", name$fname[jj])) # Change the 'name' column in Spout to the species name (separated by an underscore)
  Spout<-as.data.frame(Spout) # Change Spout from a numeric class to dataframe
  
  cvalues_mean<-rbind(cvalues_mean,Spout) # Create cvalues_mean folder with the Spout data
  cvalues_raw<-rbind(cvalues_raw,cvalues) # Create cvalues_raw folder with the data from each photo
  
}

# This gives us a table with effective limb length for each species
# change the working directory back to the 'olivia hip height' folder and save each table
setwd("~/Desktop/olivia hip height")
write.csv(cvalues_mean, "cvaluesmean.csv")
write.csv(cvalues_raw, "cvaluesraw.csv")

#### forelimb ####
setwd("~/Desktop/olivia hip height")

name<-read.csv("File_name.csv")
#name$fname[1]

#jj = 1

# Rerun after each species- sets the column names for the mean data
cvalues_mean= matrix(nrow = 0, ncol = 8)
colnames(cvalues_mean)<-c('name', 'image','forarm','shank','meta','toe', 'HH','eff')

# Rerun after each species- sets the column names for the raw data
cvalues_raw= matrix(nrow = 0, ncol = 8)
colnames(cvalues_raw)<-c('name', 'image','thigh','shank','meta','toe', 'HH','eff')

# For-loop setting the working directory to the folder for each species
for (jj in 1:nrow(name)){
  setwd(paste0("~/Desktop/olivia hip height","/",name$Fname[jj]))
  data1<-read.csv("outdata.csv")
  
  # Rerun after each species- sets the column names for the data
  cvalues= matrix(nrow = 0, ncol = 8)
  colnames(cvalues)<-c('name', 'image','thigh','shank','meta','toe', 'HH','eff')
  
  # For-loop working out the length between each point for each photo
  for (ii in 1:nrow(data1)){
    thigh=sqrt((data1$fx1[ii]-data1$fx2[ii])^2+(data1$fy1[ii]-data1$fy2[ii])^2)
    shank=sqrt((data1$fx2[ii]-data1$fx3[ii])^2+(data1$fy2[ii]-data1$fy3[ii])^2)
    meta=sqrt((data1$fx3[ii]-data1$fx4[ii])^2+(data1$fy3[ii]-data1$fy4[ii])^2)
    toe=sqrt((data1$fx4[ii]-data1$fx5[ii])^2+(data1$fy4[ii]-data1$fy5[ii])^2)
    HH=sqrt((data1$fx1[ii]-data1$fx5[ii])^2+(data1$fy1[ii]-data1$fy5[ii])^2) # including hip height
    eff=HH/(thigh+shank+meta+toe) # and effective hip height
    # save these to a temporary file
    temp<-c(name$Fname[jj],data1$image[ii],thigh,shank,meta,toe,HH,eff)
    cvalues<-rbind(cvalues,temp) # Add the temp data to the cvalues matrix
    rownames(cvalues) <- NULL
  }
  
  cvalues<-as.data.frame(cvalues) # make cvalues a dataframe
  Spout<-colMeans(cvalues) # find the means for each column (for each set of photos), save as 'Spout'
  Spout$name<-as.character(gsub(" ", "_", name$Fname[jj])) # Change the name column in Spout to the species name (separated by an underscore)
  Spout<-as.data.frame(Spout) # Change Spout from a numeric class to dataframe
  
  cvalues_mean<-rbind(cvalues_mean,Spout) # Create cvalues_mean folder with the Spout data
  cvalues_raw<-rbind(cvalues_raw,cvalues) # Create cvalues_raw folder will the data from each photo
  
}

# This gives us a table with effective limb length for each species
# change the working directory back to the 'olivia hip height' folder and save each table
setwd("~/Desktop/olivia hip height")
write.csv(cvalues_mean, "cvaluesmeanF.csv")
write.csv(cvalues_raw, "cvaluesrawF.csv")
