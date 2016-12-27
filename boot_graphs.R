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
#Master_species_gradient_logger_data <- read.csv("~/Documents/OneDrive/Antarctica Files/Data/Gradient Project/Hobodata/Large Tank/DATA_reanalysis/Master_species_gradient_logger_data.csv")

#set wd
setwd("~/Documents/OneDrive/Antarctica Files/Data/Gradient Project/Hobodata/Large Tank/DATA_reanalysis")



### /////   RUN BOOTSCRIPT before //// ######

col="Set1"

## Graph all boot data mean vs variance 
boot_data_all_data
boot_data_var_all_data

all_boot_data_xy =data.frame(as.factor(boot_data_all_data[,1]),boot_data_all_data[,2], boot_data_var_all_data[,2])
colnames(all_boot_data_xy) <- c('Species', 'Temp_Mean', 'Variance')

mean_limits <- aes(xmax = boot_data_all_data[,4] , xmin=boot_data_all_data[,3])

var_limits <- aes(ymax =boot_data_var_all_data[,4] , ymin=boot_data_var_all_data[,3])


boot_graph_base<-ggplot(data= all_boot_data_xy, aes(x=Temp_Mean , y=Variance,color=Species),color=brewer.pal(5,"Set1")) 
boot_graph_base +theme_bw()+
  geom_point( )+
  guides(fill = guide_legend(keywidth = 3, keyheight = 1))+
  xlim(-2, 2)+
  geom_vline(xintercept = 1.490, color="red",linetype = "dashed",alpha = 0.5)+
  geom_vline(xintercept = -1.130, color= "blue",linetype = "dashed",alpha = 0.5)+
  xlab(parse(text=paste("Temperature (C","^o",")")))+
  ggtitle(paste("Boot Replicates:",boot_data_all_data[1,5]))

## Graph 1000 boot data mean vs variance 

boot_data_last_1000
boot_data_var_last_1000

all_boot_data_xy =data.frame(as.factor(boot_data_last_1000[,1]),boot_data_last_1000[,2], boot_data_var_last_1000[,2])
colnames(all_boot_data_xy) <- c('Species', 'Temp_Mean', 'Variance')

mean_limits <- aes(xmax = boot_data_all_data[,4] , xmin=boot_data_all_data[,3])

var_limits <- aes(ymax =boot_data_var_all_data[,4] , ymin=boot_data_var_all_data[,3])


boot_graph_base<-ggplot(data= all_boot_data_xy, aes(x=Temp_Mean , y=Variance,color=Species),color=brewer.pal(5,"Set1")) 
boot_graph_base +theme_bw()+
  geom_point( )+
  guides(fill = guide_legend(keywidth = 3, keyheight = 1))+
  xlim(-2, 2)+
  geom_vline(xintercept = 1.490, color="red",linetype = "dashed",alpha = 0.5)+
  geom_vline(xintercept = -1.130, color= "blue",linetype = "dashed",alpha = 0.5)+
  xlab(parse(text=paste("Temperature (C","^o",")")))+
  ggtitle(paste("Boot Replicates:",boot_data_var_last_1000[1,5]))
