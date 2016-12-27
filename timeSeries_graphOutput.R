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
## Functions

#Function1- Input column number for sp from Master dataset. Outputs graph of tempeature vs time.
make_line_graph_of<-function(x,size){
xvals=1:length(na.omit(Master_species_gradient_logger_data[,x]))
yvals=na.omit(Master_species_gradient_logger_data[,x])
x_n_y_vals=data.frame(xvals,yvals)
ggplot(data=x_n_y_vals,
       aes(x=xvals,
           y=yvals)
       )+
  geom_smooth(color="black")+
  ylim(-2, 2)+
  labs(title =paste("Thermal Gradient Data:", as.character(names(Master_species_gradient_logger_data)[x])),
       x= parse(text=paste("Seconds (s","^-1",")")),
       y= parse(text=paste("Temperature (C","^o",")")))+

  #add avg min/max temperatures from gradient
  geom_hline(yintercept = 1.490, color="red")+
  geom_hline(yintercept = -1.130, color= "blue")+

  #Add avaerage line for data from the last 13 to 3 minutes
  geom_hline(yintercept = mean(yvals[(length(yvals)-(15*60)):(length(yvals)-(5*60))]), color= "black",linetype="dashed")+

  #Change theme
  theme_bw()

  #save plot
  filename_x<-paste(names(Master_species_gradient_logger_data)[x],".png")
  filename_x<-gsub("\\s", "",filename_x)
  ggsave(filename=filename_x, width = size, height = size)
}
# Test for Funtion 1
make_line_graph_of(1,5)

########################################################################################################################################
# F2 not working?
# 
# #Function2- Input column number for sp from Master dataset. Outputs graph of tempeature vs time.
# make_line_graph_of_15_to_5<-function(a,x,size){
#   p=0
#   b<-a
#   vals<- data.frame(matrix(nrow=601, ncol= (x-b+2)))
#   yvals=na.omit(Master_species_gradient_logger_data[,x])
#   vals[,(x-b+2)]<-c(1:601)
#   while (b<=x-b+2){
#     for (z in 1:(x-b+1)){
#       vals[,z]<-na.omit(Master_species_gradient_logger_data[,b])[(length(na.omit(Master_species_gradient_logger_data[,b]))-(15*60)):(length(na.omit(Master_species_gradient_logger_data[,b]))-(5*60))]
#       b=b+1
#     }}
#   x_n_y_vals=vals
#   colnames(x_n_y_vals[,2:(x-b+2)])<-names(Master_species_gradient_logger_data)[b:x]
#   #print(x_n_y_vals)
# 
#   #make plot
#   plot<-ggplot(data=x_n_y_vals,
#          aes(x=x_n_y_vals[,(x-a+2)]))+
#     ylim(-2, 2)+
#     labs(title =paste("Thermal Gradient Data Between Last 15 to 5 minutes:", as.character(names(Master_species_gradient_logger_data)[x])),
#          x= parse(text=paste("Seconds (s","^-1",")")),
#          y= parse(text=paste("Temperature (C","^o",")")))+
# 
#     #add avg min/max temperatures from gradient
#     geom_hline(yintercept = 1.490, color="red")+
#     geom_hline(yintercept = -1.130, color= "blue")+
# 
#     #Add avaerage line for data from the last 13 to 3 minutes
#     #geom_hline(yintercept = mean(yvals[(length(yvals)-(15*60)):(length(yvals)-(5*60))]), color= "black",linetype="dashed")+
# 
#     #Change theme
#     theme_bw()
#       b<-a
#       print(x-b+2)
#       while (b<=(x-b+2)){
#         loop_input = paste("geom_smooth(aes(y=",x_n_y_vals[,b],",color='black'))")
#         print(eval(parse(text=loop_input)))
#         plot <- plot + eval(parse(text=loop_input))
#         b<-1+b
#   }
#   #save plot
#   filename_x<-paste(names(Master_species_gradient_logger_data)[x],".png")
#   filename_x<-gsub("\\s", "",filename_x)
#   ggsave(filename=filename_x, width = size, height = size)
# }
# 
# # Test for Funtion 2
# make_line_graph_of_15_to_5(a=1,x=3,size=5)

#Function3- Histogram distribution.
make_line_graph_of_freq<-function(z,x,size){
  yvals=data.frame(na.omit(Master_species_gradient_logger_data[,x]))
  ggplot(data=yvals,aes(x=yvals[,1]))+
    geom_density2d()+
    xlim(-2, 2)+stat_function(fun = dnorm, colour = "red")+
    labs(title =paste("Thermal Gradient Data:", as.character(names(Master_species_gradient_logger_data)[x])),
         y= parse(text=paste("# of Seconds Spent at Temperature (s","^-1",")")),
         x= parse(text=paste("Temperature (C","^o",")")))+

    #add avg min/max temperatures from gradient
    #geom_hline(yintercept = 1.490, color="red")+
    #geom_hline(yintercept = -1.130, color= "blue")+

    #Add avaerage line for data from the last 13 to 3 minutes
    #geom_hline(yintercept = mean(yvals[(length(yvals)-(15*60)):(length(yvals)-(5*60))]), color= "black",linetype="dashed")+

    #Change theme
    theme_bw()
  #save plot
  filename_x<-paste(names(Master_species_gradient_logger_data)[x],".png")
  filename_x<-gsub("\\s", "",filename_x)
  ggsave(filename=filename_x, width = size, height = size)
}
# Test for Funtion 3
make_line_graph_of_freq(0.1,24,5)


########################################################################################################################################
## Data analysis

#Print a graph for each species per individual for all data
#size for graphs values 5-10
size=5
for(i in 1:length(names(Master_species_gradient_logger_data))){
  make_line_graph_of(i,size)
}

#Print a graph for each species per individual for last 13 to 3 minutes

#Print freq distubtion of time spent at each temperature range
binwidth=0.5
for(i in 1:length(names(Master_species_gradient_logger_data))){
  make_line_graph_of_15_to_5(binwidth, i,size)
}
