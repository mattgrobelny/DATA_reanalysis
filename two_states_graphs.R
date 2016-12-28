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
library(coin)

# Import data set 
#Master_species_gradient_logger_data <- read.csv("~/Documents/OneDrive/Antarctica Files/Data/Gradient Project/Hobodata/Large Tank/DATA_reanalysis/Master_species_gradient_logger_data.csv")

#set wd
setwd("~/Documents/OneDrive/Antarctica Files/Data/Gradient Project/Hobodata/Large Tank/DATA_reanalysis")

levels(time_0_time_end_stack$Species) <- c("C.wilsoni","L.squamifrons", "N.coriiceps", "T.hansoni")
two_stage_all = ggplot(data= time_0_time_end_stack, aes(x=TimeStage, y= Temp))
two_stage_facet = two_stage_all + 
  geom_boxplot()+ 
  theme_bw() +
  facet_grid(.~Species)+
  ylim(-2,2)+
  ylab(parse(text=paste("Temperature (C","^o",")")))+
  geom_hline(yintercept = 1.490, color="red",linetype = "dashed",alpha = 0.5)+
  geom_hline(yintercept = -1.130, color= "blue",linetype = "dashed",alpha = 0.5)+
  stat_summary(fun.y=median, geom="line", aes(group=1), color='red')  + 
  stat_summary(fun.y=median, geom="point", color='red')
two_stage_facet


ggsave(two_stage_facet, file="two_stage_facet.png", dpi = 500)



