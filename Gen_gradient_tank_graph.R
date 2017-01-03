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


#gradient_tank_temps <- read.csv("C:/Users/Matt/SkyDrive/Antarctica Files/Data/Gradient Project/Hobodata/Large Tank/ColdRoom_Temp_log/Master Excel Sheet/gradient_tank_temps.csv", header=T)
gradient_tank_temps <- read.csv("~/Documents/OneDrive/Antarctica Files/Data/Gradient Project/Hobodata/Large Tank/ColdRoom_Temp_log/Master Excel Sheet/gradient_tank_temps.csv", header=TRUE)
x=2000
z=4000

#set up data frames
M1<-gradient_tank_temps$Temp1[x:z]
M2<-gradient_tank_temps$Temp2[x:z]
M3<-gradient_tank_temps$Temp3[x:z]
M4<-gradient_tank_temps$Temp4[x:z]
time<-c(1:2001)
MM_M<-data.frame(time,M1,M2,M3,M4)

V1<-gradient_tank_temps$Numb[x:z]
V2<-gradient_tank_temps$Numb[x:z]
V3<-gradient_tank_temps$Numb[x:z]
V4<-gradient_tank_temps$Numb[x:z]
VV_V<-data.frame(V1,V2,V3,V4)
# boxplot(MM_M)
# #change to ggplot plot?
# matplot(y=MM_M,x=VV_V,type="l",ylim=c(-1.5,1.5),xlab="Duration (hours) ", ylab="Temperature (C)", col=c("red","red4","blue4","blue"),lwd=2.5,xaxt="n") +theme_bw()
# box()
# Axis(x =2000:4000, at = c(2000,2500,3000,3500,4000), side=1, labels = c("0","6","12","18","24"))

mean_for_each_temp_sensor<-data.frame(mean(M1),mean(M2),mean(M3),mean(M4))
MM_M[,5] <- c(1:2001)

ggplot_gradient_temps = ggplot(data=MM_M, aes(time)) + 
  geom_line(aes(y=MM_M[,2],group=1, color="A"))+
  geom_line(aes(y=MM_M[,3],group=1, color="B"))+
  geom_line(aes(y=MM_M[,4],group=1, color="C"))+
  geom_line(aes(y=MM_M[,5],group=1, color="D"))+
  theme_bw()+
  ylim(-2,2)+
  geom_hline(yintercept = 1.49, color = "red",
             linetype = "dashed", alpha = 0.5) + 
  geom_hline(yintercept = -1.13, color = "blue",
             linetype = "dashed", alpha = 0.5) + 
  ylab(expression("Temperature ("*~degree*"C)"))+
  xlab("Duration (hours)")+
  scale_x_discrete("Duration (hours)",breaks= c(0,500,1000,1500,2000),
                   labels = c("0","6","12","18","24"), limits=c(0:2001))+
  scale_colour_manual(name="Temperature Sensor", values=c("A" = "red", "B" = "red4", "C" = "blue4", "D" = "blue"),
  guide = guide_legend(fill = NULL,colour = NULL))+ theme(legend.position = "bottom")
ggplot_gradient_temps


ggsave(ggplot_gradient_temps, file = "ggplot_gradient_temps.png", dpi = 500)

  




