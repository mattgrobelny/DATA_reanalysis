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

XBT_R <- read.csv("~/Documents/OneDrive/Antarctica Files/Data/Gradient Project/XBT/XBT_R.csv")

head(XBT_R)
#Set Depth limit
N<-250

#set up data frames
S1<-XBT_R$Site1[3:N]
S2<-XBT_R$Site2[3:N]
S3<-XBT_R$Site3[3:N]
SS_S<-data.frame(S1,S2,S3)
D1<-XBT_R$Depth[3:N]
D2<-XBT_R$Depth[3:N]
D3<-XBT_R$Depth[3:N]
DD_D<-data.frame(D1,D2,D3)

#plot
matplot(y=DD_D,x=SS_S,type="l",lty=c(1,1,1),ylim = rev(range(-10:max(D1))),xlim=c(-2,2),xlab="Seawater Temperature (°C) ", ylab="Depth (m)", col=c("gray8","gray44","gray64"),lwd=2.5,axes = F)
box()
axis(side = 1, tck = .01, labels = NA)
axis(side = 2, tck = .01, labels = NA)
axis(side = 4, tck = .01, labels = NA)
axis(side = 3, tck = .01, labels = NA)
axis(side = 1, lwd = 0, line = -.7)
axis(side = 2, lwd = 0, line = -.7, las = 1)

#add gradient lines which show the range of the experimental tank
# abline(v=-1.3,col = "blue",lty=2)
# abline(v=1.3,col = "red",lty=2)
abline(h=0,col = "black",lty=3)

###mtext(side = 2, "Outcome Variable",font=3 line = 2)


### Redo in ggplot2

N<-1400

ggplot_XBT_2014 = ggplot(data=na.omit(XBT_R[3:N,1:4]), aes(y=Depth)) + 
  geom_path(aes(x=as.numeric(Site3),group=1, color="Gerlache Strait"), size =1.1)+
  geom_path(aes(x=Site1,group=1, color="Bismark Strait"), size =1.1)+
  geom_path(aes(x=Site2,group=1, color="Dallman Bay"), size =1.1)+
  theme_bw()+
  xlim(-2,2) +
  scale_y_reverse()+
  geom_hline(yintercept = 0, color = "black",
             linetype = "dashed", alpha = 0.7) + 
  xlab(parse(text = paste("Temperature (C", "^o",")")))+
  ylab("Depth (m)")+
  scale_colour_manual(name="Location", values=c("Bismark Strait" = "red", "Dallman Bay" = "red4", "Gerlache Strait" = "blue4"),
                      guide = guide_legend(fill = NULL,colour = NULL))
ggplot_XBT_2014

ggsave(ggplot_XBT_2014, file = "ggplot_XBT_2014.png", dpi = 500)

