#gradient_tank_temps <- read.csv("C:/Users/Matt/SkyDrive/Antarctica Files/Data/Gradient Project/Hobodata/Large Tank/ColdRoom_Temp_log/Master Excel Sheet/gradient_tank_temps.csv", header=T)
gradient_tank_temps <- read.csv("~/Documents/OneDrive/Antarctica Files/Data/Gradient Project/Hobodata/Large Tank/ColdRoom_Temp_log/Master Excel Sheet/gradient_tank_temps.csv", header=TRUE)
x=2000
z=4000

#set up data frames
M1<-gradient_tank_temps$Temp1[x:z]
M2<-gradient_tank_temps$Temp2[x:z]
M3<-gradient_tank_temps$Temp3[x:z]
M4<-gradient_tank_temps$Temp4[x:z]

MM_M<-data.frame(M1,M2,M3,M4)
V1<-gradient_tank_temps$Numb[x:z]
V2<-gradient_tank_temps$Numb[x:z]
V3<-gradient_tank_temps$Numb[x:z]
V4<-gradient_tank_temps$Numb[x:z]
VV_V<-data.frame(V1,V2,V3,V4)
boxplot(MM_M)
#change to ggplot plot?
matplot(y=MM_M,x=VV_V,type="l",ylim=c(-1.5,1.5),xlab="Duration (hours) ", ylab="Temperature (C)", col=c("red","red4","blue4","blue"),lwd=2.5,xaxt="n")
box()
Axis(x =2000:4000, at = c(2000,2500,3000,3500,4000), side=1, labels = c("0","6","12","18","24"))

mean_for_each_temp_sensor<-data.frame(mean(M1),mean(M2),mean(M3),mean(M4))
