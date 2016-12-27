# Data reanalysis for the thermal gradient project
# By: Mateusz Grobelny

########################################################################################################################################
# Libraries
library(ggplot2)
library(RColorBrewer)
library(fields)
library(MASS)
library(FSA)
library(dunn.test)
library(lattice)


# Import data set 
Master_species_gradient_logger_data <- read.csv("~/Documents/OneDrive/Antarctica Files/Data/Gradient Project/Hobodata/Large Tank/DATA_reanalysis/Master_species_gradient_logger_data.csv")

#set wd
setwd("~/Documents/OneDrive/Antarctica Files/Data/Gradient Project/Hobodata/Large Tank/DATA_reanalysis")

########################################################################################################################################
## Reorgnize data

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
all_species_no_Ngib<- data.frame(Ngib_data,Than_data,Cwil_data,Lsq_data,Ncor_data)

stack_all_species<-stack(all_species)

########################################################################################################################################
## BETTER Distribution of desnity of temperatures per species 

# x = the smoothness of the plot ( 0= raw ;; 2= smooth) 
x=2
pos = "identity"
# a = to opasity of graphs how see thru they are
a=0.2
col="Set1"

# base2 <- ggplot(stack_all_species, aes(x=stack_all_species[,1],fill = stack_all_species[,2])) +xlim(-2, 2)+
#   geom_density(adjust = x,na.rm=TRUE,position = pos,alpha = a)+
#   scale_fill_brewer(palette=col)+theme_bw()
# base2

base2 <- ggplot(stack_all_species, aes(x=stack_all_species[,1],fill = stack_all_species[,2]))
# finish_graph<-xlim(-2, 2)+scale_fill_brewer(palette=col)+theme_bw()+geom_vline(xintercept = 1.490, color="red")+
#   geom_vline(xintercept = -1.130, color= "blue")
denisty_plot= base2+geom_density(stat="density",adjust = x,na.rm=TRUE,position = pos,alpha = a)+xlim(-2, 2)+scale_fill_brewer(palette=col)+theme_bw()+
  geom_vline(xintercept = 1.490, color="red",linetype = "dashed",alpha = 0.5)+
  geom_vline(xintercept = -1.130, color= "blue",linetype = "dashed",alpha = 0.5)+
  scale_y_continuous(expand = c(0,0)) 
ggsave(denisty_plot, file="denisty_plot_all_species.png", dpi = 500)

# HARD TO UNDERSTAND GRAPH NOT HELPFUL
# #Histogram 
stack_histogram = base2+geom_histogram(aes(y=..density..),na.rm=TRUE, binwidth = 0.2)+xlim(-2, 2)+scale_fill_brewer(palette=col)+theme_bw()+geom_vline(xintercept = 1.490, color="red")+
  geom_vline(xintercept = -1.130, color= "blue")+scale_y_continuous(expand = c(0,0)) 
ggsave(stack_histogram, file="stack_histogram_all_species.png", dpi = 500)
