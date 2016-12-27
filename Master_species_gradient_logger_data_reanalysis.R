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
# 
# #Function1- Input column number for sp from Master dataset. Outputs graph of tempeature vs time.
# make_line_graph_of<-function(x,size){
# xvals=1:length(na.omit(Master_species_gradient_logger_data[,x]))
# yvals=na.omit(Master_species_gradient_logger_data[,x])
# x_n_y_vals=data.frame(xvals,yvals)
# ggplot(data=x_n_y_vals, 
#        aes(x=xvals, 
#            y=yvals)
#        )+
#   geom_smooth(color="black")+
#   ylim(-2, 2)+
#   labs(title =paste("Thermal Gradient Data:", as.character(names(Master_species_gradient_logger_data)[x])), 
#        x= parse(text=paste("Seconds (s","^-1",")")), 
#        y= parse(text=paste("Temperature (C","^o",")")))+
#   
#   #add avg min/max temperatures from gradient 
#   geom_hline(yintercept = 1.490, color="red")+
#   geom_hline(yintercept = -1.130, color= "blue")+
#   
#   #Add avaerage line for data from the last 13 to 3 minutes  
#   geom_hline(yintercept = mean(yvals[(length(yvals)-(15*60)):(length(yvals)-(5*60))]), color= "black",linetype="dashed")+
#   
#   #Change theme
#   theme_bw()
#   
#   #save plot
#   filename_x<-paste(names(Master_species_gradient_logger_data)[x],".png")
#   filename_x<-gsub("\\s", "",filename_x)
#   ggsave(filename=filename_x, width = size, height = size)
# }
# # Test for Funtion 1
# make_line_graph_of(1,5)
# 
# #Function2- Input column number for sp from Master dataset. Outputs graph of tempeature vs time.
# make_line_graph_of_15_to_5<-function(a,x,size){
#   p=0
#   b<-a
#   vals<- data.frame(matrix(nrow=601, ncol= (x-b+2)))
#   vals[,(x-b+2)]<-c(1:601)
#   while (b<=x-b+2){
#     for (z in 1:(x-b+1)){
#       vals[,z]<-na.omit(Master_species_gradient_logger_data[,b])[(length(na.omit(Master_species_gradient_logger_data[,b]))-(15*60)):(length(na.omit(Master_species_gradient_logger_data[,b]))-(5*60))]
#       b=b+1
#     }}
#   x_n_y_vals=vals
#   #colnames(x_n_y_vals[,2:(x-b+2)])<-names(Master_species_gradient_logger_data)[b:x]
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
#   b<-a
#   print(x-b+2)
#   while (b<=(x-b+2)){
#     loop_input = paste("geom_smooth(aes(y=",x_n_y_vals[,b],",color='black')")
#     print(loop_input)
#     plot <- plot + eval(parse(text=loop_input))
#     b<-1+b
#   }
#   #save plot
#   # filename_x<-paste(names(Master_species_gradient_logger_data)[x],".png")
#   # filename_x<-gsub("\\s", "",filename_x)
#   # ggsave(filename=filename_x, width = size, height = size)
# }
# 
# # Test for Funtion 2
# make_line_graph_of_15_to_5(a=1,x=3,size=5)
# 
# #Function3- Histogram distribution.
# make_line_graph_of_freq<-function(z,x,size){
#   yvals=data.frame(na.omit(Master_species_gradient_logger_data[,x]))
#   ggplot(data=yvals,aes(x=yvals[,1]))+
#     geom_density2d()+
#     xlim(-2, 2)+stat_function(fun = dnorm, colour = "red")+
#     labs(title =paste("Thermal Gradient Data:", as.character(names(Master_species_gradient_logger_data)[x])), 
#          y= parse(text=paste("# of Seconds Spent at Temperature (s","^-1",")")), 
#          x= parse(text=paste("Temperature (C","^o",")")))+
#     
#     #add avg min/max temperatures from gradient 
#     #geom_hline(yintercept = 1.490, color="red")+
#     #geom_hline(yintercept = -1.130, color= "blue")+
#     
#     #Add avaerage line for data from the last 13 to 3 minutes  
#     #geom_hline(yintercept = mean(yvals[(length(yvals)-(15*60)):(length(yvals)-(5*60))]), color= "black",linetype="dashed")+
#     
#     #Change theme
#     theme_bw()
#   #save plot
#   filename_x<-paste(names(Master_species_gradient_logger_data)[x],".png")
#   filename_x<-gsub("\\s", "",filename_x)
#   ggsave(filename=filename_x, width = size, height = size)
# }
# # Test for Funtion 3
# make_line_graph_of_freq(0.1,24,5)


########################################################################################################################################
## Data analysis

#Print a graph for each species per individual for all data
#size for graphs values 5-10
size=5
for(i in 1:length(names(Master_species_gradient_logger_data))){
  make_line_graph_of(i,size)
}

#Print a graph for each species per individual for last 13 to 3 minutes

# Print freq distubtion of time spent at each temperature range 
# binwidth=0.5
# for(i in 1:length(names(Master_species_gradient_logger_data))){
#   make_line_graph_of_freq(binwidth, i,size)
# }

########################################################################################################################################
#Print state summary for each indiviual 
b=c(1:27)
stat_summary<-data.frame(summary(na.omit(Master_species_gradient_logger_data[,b])[(length(na.omit(Master_species_gradient_logger_data[,b]))-(15*60)):(length(na.omit(Master_species_gradient_logger_data[,b]))-(5*60))]))

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
## Reorganizing for data last 1000 datapoints  
num_last = 500
#Ngib 
#Ngib_data<-c(Master_species_gradient_logger_data$Ngib1,Master_species_gradient_logger_data$Ngib2,Master_species_gradient_logger_data$Ngib3)
Ngib_data_1000<-vector()
for (i in 1:3){
  Ngib_data_1000<-c(Ngib_data_1000,tail(na.omit(Master_species_gradient_logger_data[,i]),n=num_last))
}

Ngib_data_1000<-data.frame(Ngib_data_1000)

#Than
Than_data_1000<-vector()
for (i in 4:9){
  Than_data_1000<-c(Than_data_1000,tail(na.omit(Master_species_gradient_logger_data[,i]),n=num_last))
}
Than_data_1000<-data.frame(Than_data_1000)

#Cwil
Cwil_data_1000<-vector()
for (i in 10:15){
  Cwil_data_1000<-c(Cwil_data_1000,tail(na.omit(Master_species_gradient_logger_data[,i]),n=num_last))
}
Cwil_data_1000<-data.frame(Cwil_data_1000)

#Lsq
Lsq_data_1000<-vector()
for (i in 16:21){
  Lsq_data_1000<-c(Lsq_data_1000,tail(na.omit(Master_species_gradient_logger_data[,i]),n=num_last))
}
Lsq_data_1000<-data.frame(Lsq_data_1000)

#Ncor
Ncor_data_1000<-vector()
for (i in 22:27){
  Ncor_data_1000<-c(Ncor_data_1000,tail(na.omit(Master_species_gradient_logger_data[,i]),n=num_last))
}
Ncor_data_1000<-data.frame(Ncor_data_1000)

#combining all data last 1000 datapoints 

all_species_1000<-vector()
all_species_1000<- data.frame(Ngib_data_1000,
                        Than_data_1000,
                         Cwil_data_1000,
                         Lsq_data_1000,
                         Ncor_data_1000)

stack_all_species_1000<-stack(na.omit(all_species_1000))
################################################################################################################################################################
#AFGP bearing species vs non afgp bearing

# REMOVE NAs
stack_all_species_1000<-na.omit(stack_all_species_1000)
#
# add new column
stack_all_species_1000["AFGP_content"]<-NA
total= length(stack_all_species_1000[1])
pb <- txtProgressBar(min=1, max=total,style=3)
for (i in 1:total){
  if (as.character(stack_all_species_1000[i,2])=="Lsq_data_1000"){
    stack_all_species_1000[i,3] = as.character("Neg")
    setTxtProgressBar(pb, i)
  }
  else {
    stack_all_species_1000[i,3]<-as.character("Pos")
    setTxtProgressBar(pb, i)
  }
  
}
stack_all_species_1000[,3]<-as.factor(stack_all_species_1000[,3])
names(stack_all_species_1000)<-c()
write.table(stack_all_species_1000, "./stack_all_species_1000", sep=",")

# CDF transformation on individual species
colnames(stack_all_species_1000)<-c("Skip","Temperature","Species","AFGP_content")


##########################################################################################################################################
# # Distribution of desnity of temperatures per species 
# x=5
# pos = "identity"
# a=0.1
# base <- ggplot(all_species, aes(x=all_species[,1])) +xlim(-2, 2)+ geom_density(na.rm=TRUE, colour="blue",adjust = x,position = pos,alpha = a)+
# #base + stat_function(fun = dnorm, colour = "red",args = list(mean = 2, sd = .5))
# geom_density(data=all_species, aes(x=all_species[,2]),kernel="gaussian",na.rm=TRUE, colour="red",adjust = x,position = pos,alpha = a)+
# geom_density(data=all_species, aes(x=all_species[,3]),kernel="gaussian",na.rm=TRUE, colour="black",adjust = x,position = pos,alpha = a)+
# geom_density(data=all_species, aes(x=all_species[,4]),kernel="gaussian",na.rm=TRUE, colour="orange",adjust = x,position = pos,alpha = a)+
# geom_density(data=all_species, aes(x=all_species[,5]),kernel="gaussian",na.rm=TRUE, colour="green",adjust = x,position = pos,alpha = a)
# 
# base

##########################################################################################################################################
## BETTER Distribution of desnity of temperatures per species 

# x = the smoothness of the plot ( 0= raw ;; 2= smooth) 
x=2
pos = "identity"
# a = to opasity of graphs how see thru they are
a=0.5
col="Set1"

# base2 <- ggplot(stack_all_species, aes(x=stack_all_species[,1],fill = stack_all_species[,2])) +xlim(-2, 2)+
#   geom_density(adjust = x,na.rm=TRUE,position = pos,alpha = a)+
#   scale_fill_brewer(palette=col)+theme_bw()
# base2

base2 <- ggplot(stack_all_species, aes(x=stack_all_species[,1],fill = stack_all_species[,2]))
# finish_graph<-xlim(-2, 2)+scale_fill_brewer(palette=col)+theme_bw()+geom_vline(xintercept = 1.490, color="red")+
#   geom_vline(xintercept = -1.130, color= "blue")
base2+geom_density(stat="density",adjust = x,na.rm=TRUE,position = pos,alpha = a)+xlim(-2, 2)+scale_fill_brewer(palette=col)+theme_bw()+
  geom_vline(xintercept = 1.490, color="red",linetype = "dashed",alpha = 0.5)+
  geom_vline(xintercept = -1.130, color= "blue",linetype = "dashed",alpha = 0.5)+
  scale_y_continuous(expand = c(0,0)) 

# HARD TO UNDERSTAND GRAPH NOT HELPFUL
# #Histogram 
 base2+geom_histogram(aes(y=(..count../sum(..count..))*100),na.rm=TRUE, binwidth = 0.2)+xlim(-2, 2)+scale_fill_brewer(palette=col)+theme_bw()+geom_vline(xintercept = 1.490, color="red")+
   geom_vline(xintercept = -1.130, color= "blue")+scale_y_continuous(expand = c(0,0)) 

 ################################################################################################################################################################
# Percentage of time spent at a specific temperature 
base2+geom_histogram(aes(y=(..count../sum(..count..)),fill=AFGP_content),na.rm=TRUE, binwidth = 0.2)+
  xlim(-2, 2)+
  scale_fill_brewer(palette=col)+
  theme_bw()+
  geom_vline(xintercept = 1.490, color="red",linetype = "dashed",alpha = 0.5)+
  geom_vline(xintercept = -1.130, color= "blue",linetype = "dashed",alpha = 0.5)+
  scale_y_continuous(expand = c(0,0),labels = scales::percent) 

################################################################################################################################################################
# Niche temperature vs AFGP enabled temperature 
#For Niche
# Compare species temp distribution to posson distibution

#break up data into temperature bins
my.breaks<- seq(-2,2.5,0.1)
Ngib_bin<-hist(log(na.omit(all_species$Ngib_data)),breaks = my.breaks)
Ngib_bin$counts

Than_bin<-hist(na.omit(all_species$Than_data),breaks = my.breaks)
Than_bin$counts

Cwil_bin<-hist(na.omit(all_species$Cwil_data),breaks = my.breaks)
Cwil_bin$counts

Lsq_bin<-hist(na.omit(all_species$Lsq_data),breaks = my.breaks)
Lsq_bin$counts

Ncor_bin<-hist(na.omit(all_species$Ncor_data),breaks = my.breaks)
Ncor_bin$counts
x<-Ncor_bin$counts

####################################################################################################
# Testing for Normaity using Shapiro-Wilk test
# For SW Test if p>0.05 cannot  

shapiro.test(N) # reject null (not normal distribution)
qqnorm(Ngib_bin$counts) # skewed data

shapiro.test(Than_bin$counts) # reject null (not normal distribution)
qqnorm(Than_bin$counts) # large values at extreems (not normal distribution)

shapiro.test(Cwil_bin$counts) # reject null (not normal distribution)
qqnorm(Cwil_bin$counts) # skewed data

shapiro.test(Lsq_bin$counts) # reject null (not normal distribution)
qqnorm(Lsq_bin$counts) # skewed data

shapiro.test(Ncor_bin$counts) # reject null (not normal distribution)
qqnorm(Ncor_bin$counts)  # skewed data


####################################################################################################
#### DOES NOT WORK RIGHT 

# Fuction to output expected poisson distribtion 
poisson_expected<-function(x){
set.seed(123)
output<-c()
df_f<-data.frame()
#find lambda
new.my.break<-my.breaks[0:length(x)]

for(k in 1:(length(my.breaks)-1)){
  r=mean(x)
  p<-0
  p= r*k / (factorial(k)* exp(1)*r)
  df1<-data.frame(p,p*sum(x))
  df_f<-rbind(df_f,df1)
}
colnames(df_f)<-c("Expected_Probablity","Expected_counts")


#t-test

output.tt<-t.test(x,df_f$Expected_counts,paired=TRUE)
print(output.tt)
#orgnized output data
output.data<-data.frame(new.my.break,x,df_f$Expected_counts)
colnames(output.data)<-c("Temp","Observed","Expected")

output$output.tt<-output.tt
output$output.data<-output.data
return(output)
}

# Using poisson_expected function
#Ngib_poisson_expected<-poisson_expected(Ngib_bin$counts)
Than_poisson_expected<-poisson_expected(Than_bin$counts)
Cwil_poisson_expected<-poisson_expected(Cwil_bin$counts)
Lsq_poisson_expected<-poisson_expected(Lsq_bin$counts)
Ncor_poisson_expected<-poisson_expected(Ncor_bin$counts)
Ncor_poisson_expected


#Print observed vs expected graphs for each species 
obs_vs_expct_graphs<-function(x){
ggplot(data=x$output.data, 
       aes(x=as.numeric(x$output.data[,1]),
           y=as.numeric(x$output.data[,2])))+
  geom_bar(stat="identity")+
  geom_smooth(aes(y=as.numeric(x$output.data[,3])),colour="red",se = FALSE)+
    xlab("Temperature C")+
    ylab("Seconds Spent at Temp")+
    xlim(-2, 2)+
    theme_bw()+
    geom_vline(xintercept = 1.490, color="red",linetype = "dashed",alpha = 0.5)+
    geom_vline(xintercept = -1.130, color= "blue",linetype = "dashed",alpha = 0.5)+
    scale_y_continuous(expand = c(0,0)) 
}

#obs_vs_expct_graphs(Ngib_poisson_expected)
obs_vs_expct_graphs(Than_poisson_expected)
obs_vs_expct_graphs(Cwil_poisson_expected)
obs_vs_expct_graphs(Lsq_poisson_expected)
obs_vs_expct_graphs(Ncor_poisson_expected)

####################################################################################################
#Trying non-peremetric Kruskal_wallis test
# Test if the means of all four groups are statistiaclly similar without assuming normal distribtuon.
Than_temp<-Than_poisson_expected$output.data[,2]
Cwil_temp<-Cwil_poisson_expected$output.data[,2]
Lsq_temp<-Lsq_poisson_expected$output.data[,2]
Ncor_temp<-Ncor_poisson_expected$output.data[,2]

all_species_no_Ngib_temp_data<-list(Than_temp=Than_temp,Cwil_temp=Cwil_temp,Lsq_temp=Lsq_temp,Ncor_temp=Ncor_temp)
kruskal.test(all_species_no_Ngib_temp_data) #fail to reject null, group means are statictially equal
# still not what I want to test?
# I want to test if each species has a random distribution or not!


####################################################################################################
#For AFGP enabled temp disctibution
# AFGP fotifed temps vs non AFGP fortified non parametric test Kruskal_wallis test
all_species_no_Lsq_df<-data.frame(c(na.omit(all_species[,1]),na.omit(all_species[,2]),na.omit(all_species[,3]),na.omit(all_species[,5])))
colnames(all_species_no_Lsq_df)<-c("Temp")
all_species_no_Lsq<-hist(na.omit(all_species_no_Lsq_df$Temp),breaks = my.breaks)
all_species_no_Lsq$counts
all_species_no_Lsq_AFGP_pos_neg_test<-list(AFGP_Pos=all_species_no_Lsq$counts,AFGP_Neg=Lsq_bin$counts)

kruskal.test(all_species_no_Lsq_AFGP_pos_neg_test)

# Non bin test non-peremetric Kruskal_wallis test
all_species_no_Lsq_df_non_bin<-list(AFGP_Pos=all_species_no_Lsq_df,AFGP_Neg=all_species$Lsq_data)
kruskal.test(all_species_no_Lsq_df_non_bin)

####################################################################################################
# One-way Anova test- data needs to be log transformed. 

# Log transforming data for One-Way Anova (needs normally distributed data)
log_Ngib<-log(all_species$Ngib_data)
log_Than<-log(all_species$Than_data)
log_Cwil<-log(all_species$Cwil_data)
log_Lsq<-log(all_species$Lsq_data)
log_Ncor<-log(all_species$Ncor_data)


oneway.test(Temperature ~ Species, 
            data = stack_all_species)

dunnTest(Temperature ~ Species, 
         data = stack_all_species,
         method="bonferroni")

oneway.test(Temperature ~ AFGP_content, 
            data = stack_all_species)

dunnTest(Temperature ~ AFGP_content, 
         data = stack_all_species,
         method="bonferroni")


# Add 3rd axis showing how temp distribution corresponds with AFGP activity
# need means for each interval === Lambda

####################################################################################################
# Kolmogorov-Smirnov Tests
# distributions considered under the null hypothesis are continuous distributions but are otherwise unrestricted.
# non-parametric and distribution free


# organize temp data into AFGP_pos and AFGP_neg
AFGP_Pos_data<-data.frame(AFGP_Pos=all_species_no_Lsq_df)
colnames(AFGP_Pos_data)<-c("AFGP_Pos")
AFGP_Neg_data<-data.frame(AFGP_Neg=all_species$Lsq_data)

## Test for normality
#shapiro.test(AFGP_Pos_data$AFGP_Pos) # reject null (not normal distribution)
qqnorm(AFGP_Pos_data$AFGP_Pos) # does not look normal (values do not fall on a straight line)

#shapiro.test(AFGP_Neg_data$AFGP_Neg) # reject null (not normal distribution)
qqnorm(AFGP_Neg_data$AFGP_Neg) # does not look normal (values do not fall on a straight line)

## Plot CDFs 
# An empirical cumulative distribution function (CDF) is a non-parametric estimator of the underlying 
# CDF of a random variable.  It assigns a probability of 1/n to each datum, 
# orders the data from smallest to largest in value, and calculates the sum of the assigned probabilities 
# up to and including each datum.  The result is a step function that increases by 1/n at each datum.

AFGP_Pos.ecdf<-ecdf(AFGP_Pos_data$AFGP_Pos)
AFGP_Neg.ecdf<-ecdf(AFGP_Neg_data$AFGP_Neg)

## Plot individual CDF for AFGP Pos vs Neg
plot(AFGP_Pos.ecdf,
     xlab = parse(text=paste("Temperature (C","^o",")")), 
     ylab = '', 
     main = 'CDF of AFGP Positive Species Temperature Distribution'
    )
mtext(text = expression(hat(F)[n](x)), side = 2, line = 2.5)

plot(AFGP_Neg.ecdf,
     xlab = parse(text=paste("Temperature (C","^o",")")), 
     ylab = '', 
     main = 'CDF of AFGP Negative Species Temperature Distribution'
     )
mtext(text = expression(hat(F)[n](x)), side = 2, line = 2.5)

############################################################################################################
## Kolmogorov-Smirnov Tests
# The KS-test uses the maximum vertical deviation between the two curves as the statistic D.

KS_Test_AFGP_neg_vs_pos<-ks.test(AFGP_Pos_data$AFGP_Pos,AFGP_Neg_data$AFGP_Neg)
KS_Test_AFGP_neg_vs_pos
######## From KS test ---> Significant difference between the two distributions 

####

##############
# All vs all KS- test

# Ngib vs Than
KS_Than_ngib<-ks.test(all_species[,1],all_species[,2])
KS_Than_ngib # sig

KS_cwil_ngib<-ks.test(all_species[,1],all_species[,3])
KS_cwil_ngib # sig 

KS_lsq_ngib<-ks.test(all_species[,1],all_species[,4])
KS_lsq_ngib # sig

KS_Ncor_ngib<-ks.test(all_species[,1],all_species[,5])
KS_Ncor_ngib # sig

KS_Cwil_Than<-ks.test(all_species[,2],all_species[,3])
KS_Cwil_Than # sig

KS_Lsq_Than<-ks.test(all_species[,2],all_species[,4])
KS_Lsq_Than # sig

KS_Lsq_Cwil<-ks.test(all_species[,2],all_species[,5])
KS_Lsq_Cwil # sig

KS_Ncor_Cwil<-ks.test(all_species[,3],all_species[,5])
KS_Ncor_Cwil # sig

KS_Ncor_Lsq<-ks.test(all_species[,4],all_species[,5])
KS_Ncor_Lsq # sig

####### Ncor Lsq Cwil Ngib Than  *= sig 
# Ncor
# Lsq
# Cwil
# Ngib
# Than                *

############################################################################################################

base3 <- ggplot(stack_all_species, aes(x=stack_all_species[,1],colour = AFGP_content))+

  stat_ecdf(na.rm=TRUE)+
  scale_color_manual(values=c(rgb(224,29,27,maxColorValue=255),rgb(48,115,175,maxColorValue=255)))+
  xlim(-2, 2)+
  ylab("Cumulative Probablility")+
  xlab(parse(text=paste("Temperature (C","^o",")")))+
  geom_vline(xintercept = 1.490, color="red",linetype = "dashed",alpha = 0.5)+
  geom_vline(xintercept = -1.130, color= "blue",linetype = "dashed",alpha = 0.5)+
  scale_y_continuous(expand = c(0,0)) +
  theme_bw()

base3

#fix fonts and I think this analysis is good.

######################################################################

# CDF transformation on individual species
colnames(stack_all_species)<-c("Temperature","Species","AFGP_content")

base4 <- ggplot(stack_all_species, aes(x=stack_all_species[,1],colour = Species))+
  stat_ecdf(na.rm=TRUE)+
  #scale_color_manual(values=c(rgb(224,29,27,maxColorValue=255),rgb(48,115,175,maxColorValue=255)))+
  xlim(-2, 2)+
  ylab("Cumulative Probablility")+
  xlab(parse(text=paste("Temperature (C","^o",")")))+
  geom_vline(xintercept = 1.490, color="red",linetype = "dashed",alpha = 0.5)+
  geom_vline(xintercept = -1.130, color= "blue",linetype = "dashed",alpha = 0.5)+
  scale_y_continuous(expand = c(0,0))+
  theme_bw()

base4


############################################################################################################################################
# Kruskal-Wallis Rank Sum Test AFGP Pos to Neg
#data_kw = stack_all_species
data_kw = stack_all_species_1000
kruskal.test(Temperature ~ AFGP_content, 
             data = data_kw)

dunnTest(Temperature ~ AFGP_content, 
          data = data_kw,
         method="bonferroni") 

############################################################################################################################################
# Kruskal-Wallis Rank Sum Test species

kruskal.test(Temperature ~ Species, 
             data = data_kw)

#Post-hoc test 

dunnTest(Temperature ~ Species, 
         data = data_kw,
         method="bonferroni") 
############################################################################################################################################
############################################################################################################################################
# Bootstraping
# bootobject <- boot(data= , statistic= , R=, ...) where
 

library(boot)
# function to obtain mean from the boot run 
samplemean <- function(x, d) {
  return(mean(x[d]))
}
# function to obtain variance from the boot run 
samplevar <- function(x, d) {
  return(var(x[d]))
}
statfuntion = samplemean
r_val = 5000
results_lsq <- boot(data=na.omit(all_species$Lsq_data), statistic=statfuntion, R=r_val)
results_ncor <- boot(data=na.omit(all_species$Ncor_data), statistic=statfuntion, R=r_val)
results_than <- boot(data=na.omit(all_species$Than_data), statistic=statfuntion, R=r_val)
results_cwil <- boot(data=na.omit(all_species$Cwil_data), statistic=statfuntion, R=r_val)
results_Ngib <- boot(data=na.omit(all_species$Ngib_data), statistic=statfuntion, R=r_val)


# view results
results_lsq
results_ncor
results_than
results_cwil
results_Ngib

plot(results_lsq)
plot(results_ncor)
plot(results_than)
plot(results_cwil)
plot(results_Ngib)

# get 95% confidence interval 
results_lsq_ci <- boot.ci(results_lsq, type="basic")
results_ncor_ci <- boot.ci(results_ncor, type="basic")
results_than_ci <- boot.ci(results_than, type="basic")
results_cwil_ci <-boot.ci(results_cwil, type="basic")
results_Ngib_ci <-boot.ci(results_Ngib, type="basic")

results_lsq_ci
results_ncor_ci
results_than_ci
results_cwil_ci
results_Ngib_ci

boot_strap_stats = c(results_lsq_ci$t0,
  results_ncor_ci$t0,
  results_than_ci$t0,
  results_cwil_ci$t0,
  results_Ngib_ci$t0)
boot_strap_ci_Low = c(results_lsq_ci$basic[4],
                  results_ncor_ci$basic[4],
                  results_than_ci$basic[4],
                  results_cwil_ci$basic[4],
                  results_Ngib_ci$basic[4])

boot_strap_ci_high = c(results_lsq_ci$basic[5],
                      results_ncor_ci$basic[5],
                      results_than_ci$basic[5],
                      results_cwil_ci$basic[5],
                      results_Ngib_ci$basic[5])

boot_strap_rep = c(results_lsq_ci$R,
                       results_ncor_ci$R,
                       results_than_ci$R,
                       results_cwil_ci$R,
                       results_Ngib_ci$R)

boot_data_var = data.frame(c("Lsq","Ncor","Than","Cwil","Ngib"),boot_strap_means,boot_strap_ci_Low, boot_strap_ci_high,boot_strap_rep)

colnames(boot_data_var)<-c("Species","Boot_mean","Boot_CI_low","Boot_CI_high","Boot_reps")
boot_data_var

boot_data
# This is not how you would do a simulation test (not a bootstrap test here). 
# What you want to do mix all the data together and then randomly redivide into two new groups find 
# the mean of each group take the difference and plot it. Repeat lots of times, 10,000 for instance. 
# Then you can find a p-value but counting all the results as or more extreme than your observed result 
# (the original difference in means) and divide by 10000. This is a non-parametric version of a t-test called
# a permutation test. However, you could use a t-test for difference in means but there are more assumptions 
# about the data than this test.

############################################################################################################################################
