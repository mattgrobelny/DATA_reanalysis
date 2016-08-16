Master_species_gradient_logger_data <- read.csv("./Master_species_gradient_logger_data.csv")

setwd(./)

#Reorgnize data
#Ngib 
Ngib_data<-c(Master_species_gradient_logger_data$Ngib1,Master_species_gradient_logger_data$Ngib2,Master_species_gradient_logger_data$Ngib3)

#Than
Than_data<-vector()
for (i in 4:9){
  Than_data<-c(Than_data,Master_species_gradient_logger_data[,i])
}
#Than_data<-data.frame(Than_data)

#Cwil
Cwil_data<-vector()
for (i in 10:15){
  Cwil_data<-c(Cwil_data,Master_species_gradient_logger_data[,i])
}
#Cwil_data<-data.frame(Cwil_data)

#Lsq
Lsq_data<-vector()
for (i in 16:21){
  Lsq_data<-c(Lsq_data,Master_species_gradient_logger_data[,i])
}
#Lsq_data<-data.frame(Lsq_data)

#Ncor
Ncor_data<-vector()
for (i in 22:27){
  Ncor_data<-c(Ncor_data,Master_species_gradient_logger_data[,i])
}
#Ncor_data<-data.frame(Ncor_data)

#combining all data
all_species<-vector()
all_species<- data.frame(Ngib_data,Than_data,Cwil_data,Lsq_data,Ncor_data)
stack_all_species<-stack(all_species)
#REMOVE NAs
stack_all_species<-na.omit(stack_all_species)

#add new column
stack_all_species["AFGP_content"]<-NA


#import packages
library(foreach)
library(doParallel)

#setup parallel backend to use 2 processors
cl<-makeCluster(4)
registerDoParallel(cl)

#start time
strt<-Sys.time()

#loop
ls<-foreach(i=1:length(stack_all_species[,2])) %dopar% {
  
  if (stack_all_species[i,2]=="Lsq_data"){
    stack_all_species[i,3]<-"Neg"
  }
  else{
    stack_all_species[i,3]<-"Pos"
  }
}

print(Sys.time()-strt)
stopCluster(cl)

write.table(stack_all_species, "./stack_all_species.csv", sep=",")

