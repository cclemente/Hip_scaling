
#look for a relationship between size and EFF

setwd("H:/olivia hip height") # Set the working directory

# read in the mass and effective limb length data
mass_data<-read.csv("mass_data5.csv") # I'm going to have to make a new csv with the diet on it to be able to run this script
eff_data<-read.csv("cvaluesmean.csv")

# Combine the genus and species columns in the mass_data, and separate by underscore
# to be identical (hopefully) to the eff names. save this as an object called 'test'
test<-paste0(as.character(mass_data$genus),"_",as.character(mass_data$species))

# add these names back to mass_data
mass_data2<-cbind(mass_data,test)
# Check that it workied right
tail(mass_data2)

# qq=1

# Create blank dataframe called 'mass_eff' with the column names we need
mass_eff<-data.frame(name=NA,mass=NA,eff=NA, diet=NA)
# also create a dataframe to store the row numbers of any species that do not match between mass_data and eff_data
non_match<-data.frame(row_num=NA)

for (qq in 1:nrow(eff_data)){ # for-loop for the number of rows in eff_data
ind<-which(as.character(mass_data2$test)==as.character(eff_data$name[qq])) # check if the species names match in both eff and mass
if (length(ind)==1){ # if they match, then create a dataframe with the species name, mass, effective limb length, and diet
temp<-data.frame(name=as.character(eff_data$name[qq]), mass=mass_data2$mass[ind],eff=eff_data$eff[qq],diet=eff_data$diet[qq])
mass_eff<-rbind(mass_eff,temp) # and add this to the blank dataframe we created before
}
else # otherwise, if the names don't match, add the row number to the blank non_match dataframe so we can easily check the names
  non_match<-rbind(non_match,qq)  
}

# remove the first blank column from the mass_eff dataframe
mass_eff<-mass_eff[-1,]

# export as csv
write.csv(mass_eff,"mass_eff.csv")

# little plot of effective limb length as a function of the log10 of mass- from the dataframe we just created
plot(eff~log10(mass),mass_eff)
  
# from this we can see if hip height varies with size (mass), and yeah it does! Check it out more in phylo_eff.R
