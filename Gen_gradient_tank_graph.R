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
library(graphics)
library(kernlab)
library(cluster)

#

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
sem_calc<-function(x){
  sem<-sd(x)/sqrt(length(x))
  return(sem)
}

mean_for_each_temp_sensor<-data.frame(c(mean(M1),mean(M2),mean(M3),mean(M4)),
                                      c(0,107,214,320),
                                      c(sem_calc(M1),sem_calc(M2),sem_calc(M3),sem_calc(M4)),
                                      c(mean(M2)-mean(M1),mean(M3)-mean(M2),mean(M4)-mean(M3),mean(M4)-mean(M3)),
                                      c(max(M1),max(M2),max(M3),max(M4)),
                                      c(min(M1),min(M2),min(M3),min(M4)),
                                      c(max(M2)-max(M1),max(M3)-max(M2),max(M4)-max(M3),max(M4)-max(M3)),
                                      c(min(M2)-min(M1),min(M3)-min(M2),min(M4)-min(M3),min(M4)-min(M3)),
                                      c("A","B","C","D"))

colnames(mean_for_each_temp_sensor) <- c("Mean","Distance","SEM","DeltaTemp","Max","Min","Delta_Max","Delta_Min","Label" )
 # Time Series Temperature of thermal gradiaent 
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
  ylab(expression("Temperature ("*degree*"C)"))+
  xlab("Duration (hours)")+
  scale_x_discrete("Duration (hours)",breaks= c(0,500,1000,1500,2000),
                   labels = c("0","6","12","18","24"), limits=c(0:2001))+
  scale_colour_manual(name="Temperature Sensor", values=c("A" = "red", "B" = "red4", "C" = "blue4", "D" = "blue"),
  guide = guide_legend(fill = NULL,colour = NULL))+ theme(legend.position = "bottom")
ggplot_gradient_temps


ggsave(ggplot_gradient_temps, file = "ggplot_gradient_temps.png", dpi = 500)

# Average Temperature at each thermal couple, against a 1:1 line and a linear fit
color_group<- c("red", "red4", "blue4", "blue")

lm_fit <- lm(mean_for_each_temp_sensor[,1]~mean_for_each_temp_sensor[,2],data=mean_for_each_temp_sensor)
lm_fit_Rsq<-summary(lm_fit)

ggplot_gradient_temp_average = ggplot(data=mean_for_each_temp_sensor, aes(x=mean_for_each_temp_sensor[,2],y=mean_for_each_temp_sensor[,1],label=mean_for_each_temp_sensor[,9])) + 
  geom_hline(yintercept = 1.49, color = "red",
             linetype = "dashed", alpha = 0.5) + 
  geom_hline(yintercept = -1.13, color = "blue",
             linetype = "dashed", alpha = 0.5) + 
  
  geom_line(aes(x=mean_for_each_temp_sensor[,2],y=mean_for_each_temp_sensor[,1],color=abs(mean_for_each_temp_sensor[,4])),size =1) +
  geom_line(aes(x=mean_for_each_temp_sensor[,2],y=mean_for_each_temp_sensor[,6],color=abs(mean_for_each_temp_sensor[,8])),size =1) +
  geom_line(aes(x=mean_for_each_temp_sensor[,2],y=mean_for_each_temp_sensor[,5],color=abs(mean_for_each_temp_sensor[,7])),size =1) +
  geom_smooth(method='lm',formula=y~x, se = FALSE, colour = 'grey40', size =1, linetype = 1,alpha = 0.5)+
  
  # 
  geom_abline(slope=(mean(M4)-mean(M1))/320, intercept=1.3016141929,color='grey45',linetype = 6)+
  geom_point()+
  theme_bw()+
  ylim(-2,2)+
  geom_errorbar(aes(ymin=mean_for_each_temp_sensor[,6], ymax=mean_for_each_temp_sensor[,5]), width=.5) +
   
  ylab(expression("Temperature ("*degree*"C)"))+
  xlab("Distance from Warm End (cm)")+
  # Fucking retarded labling scheme ...
  labs(colour = expression(paste(Delta,"Temp("*degree*"C)")))+
  scale_colour_gradient(high = '#9850d6', low = '#FFD700',guide = "colourbar")+
  geom_text(vjust =-0.2 , hjust=-.4,size = 5, fontface = "bold")+
  theme(
    legend.position = c(.999, .999),
    legend.justification = c("right", "top"),
    legend.box.just = "right")
 
ggplot_gradient_temp_average

ggsave(ggplot_gradient_temp_average, file = "ggplot_gradient_temp_average.png", dpi = 500)



# Marine Temps Plot

# Kmeans Clustering
Marine_temps <- read.csv("~/Documents/OneDrive/Antarctica Files/Data/Gradient Project/Hobodata/Large Tank/DATA_reanalysis/Marine_temps.csv",na.strings='#DIV/0!',stringsAsFactors = F)
marine_temp_no_zeros <-Marine_temps[which(!is.na(Marine_temps$Max.temp) & !is.na(Marine_temps$Min.temp) & !is.na(Marine_temps$Max.pref) & !is.na(Marine_temps$Min.pref)),]

median_v_percent <- data.frame(as.numeric(marine_temp_no_zeros$MedianPref),as.numeric(marine_temp_no_zeros$Delta_temp))

delta_temps<-data.frame(as.numeric(marine_temp_no_zeros$Max.temp-marine_temp_no_zeros$Min.temp),as.numeric(marine_temp_no_zeros$Max.pref-marine_temp_no_zeros$Min.pref))
Kmean_cluster_temps <-kmeans(delta_temps,3)
clusters<-Kmean_cluster_temps$cluster
median_v_percent_matrix<-matrix(data=median_v_percent)
marine_temp_no_zeros<-cbind(marine_temp_no_zeros,clusters)

# # Specral Clustering 
# #Spectral clustering needs a similarity or affinity s(x,y) measure determining how close points x and y are from each other.
# s <- function(x1, x2, alpha=1) {
#   exp(- alpha * norm(as.matrix(x1-x2), type="F"))
# }
# 
# make.similarity <- function(my.data, similarity) {
#   N <- nrow(my.data)
#   S <- matrix(rep(NA,N^2), ncol=N)
#   for(i in 1:N) {
#     for(j in 1:N) {
#       S[i,j] <- similarity(my.data[i,], my.data[j,])
#     }
#   }
#   S
# }
# marine_temp_no_zeros <-Marine_temps[which(Marine_temps$Delta_temp> 0),]
# sim<-make.similarity(delta_temps, s)
# 
# specc_cluster_temps <-specc(x=sim,centers= 3)
# clusters<-specc_cluster_temps@.Data
# marine_temp_no_zeros$clusters<-clusters
# 
#  
# span_of_pref <- ggplot(data= marine_temp_no_zeros, 
#                        aes(y=as.numeric(marine_temp_no_zeros$Max.temp-marine_temp_no_zeros$Min.temp),
#                                                                     x=as.numeric(marine_temp_no_zeros$Max.pref-marine_temp_no_zeros$Min.pref), 
#                                                                     #color =as.factor(marine_temp_no_zeros$clusters), 
#                                                                     label=marine_temp_no_zeros$Common.Name))+
#   #geom_errorbarh(aes(xmin=as.numeric(marine_temp_no_zeros$Min.pref), xmax=as.numeric(marine_temp_no_zeros$Max.pref)))+
#   geom_point(aes(shape=marine_temp_no_zeros$Zone))+
#   ylab("Percent of Prefered range out of min and max range")+
#   xlab("Median Prefered Range C")+
#   labs(colour ="Clustering")+
#   geom_text(vjust =1 , hjust=.3,size =3, fontface = "bold", color='black',check_overlap = TRUE)+
#   theme_bw()
# 
# span_of_pref

span_of_pref <- ggplot(data= marine_temp_no_zeros, 
                       aes(y=as.numeric(marine_temp_no_zeros$Max.temp-marine_temp_no_zeros$Min.temp),
                           x=as.numeric(marine_temp_no_zeros$Max.pref-marine_temp_no_zeros$Min.pref), 
                           color =as.factor(marine_temp_no_zeros$clusters), 
                           label=marine_temp_no_zeros$Common.Name))+
  #geom_errorbarh(aes(xmin=as.numeric(marine_temp_no_zeros$Min.pref), xmax=as.numeric(marine_temp_no_zeros$Max.pref)))+
  geom_point(aes(shape=marine_temp_no_zeros$Zone))+
  geom_smooth()+
  xlab("Delta min and max range")+
  ylab("Delta Prefered Range C")+
  labs(colour ="Clustering")+
  geom_text(vjust =1 , hjust=.3,size =3, fontface = "bold", color='black',check_overlap = TRUE)+
  theme_bw()

span_of_pref


# Calculate silhouette width for many k using PAM

sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam(sim,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)
sil_data<-data.frame(c(1:10),sil_width)
ggplot(data=sil_data, aes(x=sil_data[,1],y =sil_data$sil_width))+
  geom_line()+
  xlim(1,10)+
  xlab("Number of clusters")+
  ylab("Silhouette Width")+
  scale_x_continuous(breaks=c(1:10))+
  theme_bw()



############################################################################################################
Marine_temps <- read.csv("~/Documents/OneDrive/Antarctica Files/Data/Gradient Project/Hobodata/Large Tank/DATA_reanalysis/Marine_temps2.csv",na.strings='#DIV/0!',stringsAsFactors = F)
Fish_data<-Marine_temps[which(Marine_temps$Cat == "Fish"),]
uniq_com_names <- unique(Fish_data$Common.Name)

Fish_data_span_of_pref <- ggplot(data= Fish_data, 
                       aes(y=as.numeric(abs(Fish_data$Max.temp-Fish_data$Min.temp)),
                           x=as.numeric(abs(Fish_data$Max.pref-Fish_data$Min.pref)), 
                           label=Fish_data$Common.Name))+
  #geom_errorbarh(aes(xmin=as.numeric(Fish_data$Min.pref), xmax=as.numeric(Fish_data$Max.pref)))+
  geom_smooth(se = FALSE, color="grey")+
  geom_point(aes(shape=as.factor(Fish_data$Antarctic),color=as.numeric(Fish_data$MedianPref)),size=3)+
  ylab("Delta min and max range")+
  xlab("Delta Prefered Range C")+
  labs(colour ="Median Pref Temp (C)")+
  scale_colour_gradient(high = 'red', low = '#FFD700',guide = "colourbar")+
  geom_text(vjust =1 , hjust=.3,size =3, fontface = "bold", color='black',check_overlap = TRUE)+
  theme_bw()

Fish_data_span_of_pref

Fish_data_span_of_pref <- ggplot(data= Fish_data, 
                                 aes(x=as.numeric(Fish_data$Max.temp),
                                     y=as.numeric(Fish_data$Max.pref), 
                                     label=Fish_data$Common.Name))+
  #geom_errorbarh(aes(xmin=as.numeric(Fish_data$Min.pref), xmax=as.numeric(Fish_data$Max.pref)))+
  geom_smooth(method='lm',formula=y~x,se = FALSE, color="grey")+
  geom_point(aes(shape=as.factor(Fish_data$Antarctic),color=as.numeric(Fish_data$MedianPref)),size=3)+
  xlab("Max.temp")+
  ylab("Max.prefC")+
  labs(colour ="Median Pref Temp (C)")+
  scale_colour_gradient(high = 'red', low = 'blue',guide = "colourbar")+
  geom_text(vjust =1 , hjust=.3,size =3, fontface = "bold", color='black',check_overlap = TRUE)+
  theme_bw()

Fish_data_span_of_pref
Fish_data_span_of_pref <- ggplot(data= Fish_data, 
                                 aes(x=as.numeric(Fish_data$Max.temp),
                                     y=as.numeric(Fish_data$Max.pref)-as.numeric(Fish_data$Min.temp), 
                                     label=Fish_data$Common.Name))+
  #geom_errorbarh(aes(xmin=as.numeric(Fish_data$Min.pref), xmax=as.numeric(Fish_data$Max.pref)))+
  geom_smooth(method='lm',formula=y~x,se = FALSE, color="grey")+
  geom_point(aes(shape=as.factor(Fish_data$Antarctic),color=as.numeric(Fish_data$MedianPref)),size=3)+
  xlab("Min.temp")+
  ylab("Min.pref C")+
  labs(colour ="Median Pref Temp (C)")+
  scale_colour_gradient(high = 'red', low = 'blue',guide = "colourbar")+
  geom_text(vjust =1 , hjust=.3,size =3, fontface = "bold", color='black',check_overlap = TRUE)+
  theme_bw()

Fish_data_span_of_pref

empty <- c(1:length(Fish_data$Common.Name))
empty[1:length(Fish_data$Common.Name)] <-""

Fish_data_uniq <- Fish_data[which]
both_temp_pref <- data.frame(c(Fish_data$Common.Name,empty),
                             c(Fish_data$Common.Name,Fish_data$Common.Name),
                             c(Fish_data$Min.temp,Fish_data$Max.temp),
                             c(as.numeric(Fish_data$Max.pref)-as.numeric(Fish_data$Min.temp),as.numeric(Fish_data$Max.pref)-as.numeric(Fish_data$Min.temp)),
                             c(Fish_data$MedianPref,Fish_data$MedianPref),
                             c(Fish_data$Antarctic,Fish_data$Antarctic))
both_temp_pref <- na.omit(both_temp_pref)

Fish_data_span_of_pref <- ggplot(data= both_temp_pref, 
                                 aes(x=as.numeric(both_temp_pref$c.Fish_data.Min.temp..Fish_data.Max.temp.),
                                     y=as.numeric(both_temp_pref$c.as.numeric.Fish_data.Max.pref....as.numeric.Fish_data.Min.temp...), 
                                     label=both_temp_pref$c.Fish_data.Common.Name..empty.,
                                     group=both_temp_pref$c.Fish_data.Common.Name..Fish_data.Common.Name.))+
  geom_point(aes(shape=as.factor(both_temp_pref$c.Fish_data.Antarctic..Fish_data.Antarctic.),
                 color=as.numeric(both_temp_pref$c.Fish_data.MedianPref..Fish_data.MedianPref.)),size=3)+
  geom_line(aes(color=as.numeric(both_temp_pref$c.Fish_data.MedianPref..Fish_data.MedianPref.)))+
  xlab("Survivable Temeprature Range C")+
  ylab("Prefered Temperature Range C")+
  labs(colour ="Median Pref Temp (C)")+
  scale_colour_gradient(high = 'red', low = 'blue',guide = "colourbar")+
  geom_text(vjust =1 , hjust=.3,size =3, fontface = "bold", color='black',check_overlap = TRUE)+
  theme_bw()
Fish_data_span_of_pref

Fish_data_span_of_pref <- ggplot(data= both_temp_pref, 
                                 aes(x=as.numeric(both_temp_pref$c.Fish_data.Min.temp..Fish_data.Max.temp.),
                                     y=as.numeric(both_temp_pref$c.Fish_data.Min.pref..Fish_data.Max.pref.), 
                                     label=both_temp_pref$c.Fish_data.Common.Name..empty.,
                                     color=both_temp_pref$c.Fish_data.Antarctic..Fish_data.Antarctic.))+
  geom_point(aes(shape=as.factor(both_temp_pref$c.Fish_data.Antarctic..Fish_data.Antarctic.)),size=3)+
  geom_line(aes(group=as.factor(both_temp_pref$c.Fish_data.Common.Name..Fish_data.Common.Name.)))+
  xlab("Survivable Temeprature Range C")+
  ylab("Prefered Temperature Range C")+
  labs(colour ="Median Pref Temp (C)")+
  #scale_colour_gradient(high = 'red', low = 'blue',guide = "colourbar")+
  geom_text(vjust =1 , hjust=.3,size =3, fontface = "bold", color='black',check_overlap = TRUE)+
  theme_bw()
Fish_data_span_of_pref

