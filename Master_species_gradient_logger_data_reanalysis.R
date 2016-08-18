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
# all_species<-vector()
# all_species<- data.frame(Ngib_data,Than_data,Cwil_data,Lsq_data,Ncor_data)
# all_species_no_Ngib<- data.frame(Ngib_data,Than_data,Cwil_data,Lsq_data,Ncor_data)
# 
# stack_all_species<-stack(all_species)

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
x=1
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
# base2+geom_histogram(aes(y=(..count../sum(..count..))*100),na.rm=TRUE, binwidth = 0.2)+xlim(-2, 2)+scale_fill_brewer(palette=col)+theme_bw()+geom_vline(xintercept = 1.490, color="red")+
#   geom_vline(xintercept = -1.130, color= "blue")+scale_y_continuous(expand = c(0,0)) 


################################################################################################################################################################
#AFGP bearing species vs non afgp bearing

# # REMOVE NAs
#  stack_all_species<-na.omit(stack_all_species)
# #
# # add new column
#  stack_all_species["AFGP_content"]<-NA
# 
#  pb <- txtProgressBar(min=1, max=total,style=3)
#  for (i in 1:total){
#    if (as.character(stack_all_species[i,2])=="Lsq_data"){
#      stack_all_species[i,3]<-as.character("Neg")
#      setTxtProgressBar(pb, i)
#    }
#    else {
#      stack_all_species[i,3]<-as.character("Pos")
#      setTxtProgressBar(pb, i)
#    }
# 
#  }
#  stack_all_species[,3]<-as.factor(stack_all_species[,3])
#  names(stack_all_species)<-c()
#  write.table(stack_all_species, "./stack_all_species.csv", sep=",")

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

oneway.test()
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

######## From KS test ---> Significant difference between the two distributions 

############################################################################################################

base3 <- ggplot(stack_all_species, aes(x=stack_all_species[,1],colour = AFGP_content))+

  stat_ecdf(na.rm=TRUE)+
  scale_color_manual(values=c(rgb(224,29,27,maxColorValue=255),rgb(48,115,175,maxColorValue=255)))+
  xlim(-2, 2)+
  ylab(expression(hat(F)[n](x)))+
  xlab(parse(text=paste("Temperature (C","^o",")")))+
  geom_vline(xintercept = 1.490, color="red",linetype = "dashed",alpha = 0.5)+
  geom_vline(xintercept = -1.130, color= "blue",linetype = "dashed",alpha = 0.5)+
  scale_y_continuous(expand = c(0,0)) +
  theme_bw()

base3

#fix fonts and I think this analysis is good.

######################################################################

# CDF transformation on individual species

base4 <- ggplot(stack_all_species, aes(x=stack_all_species[,1],colour = ind))+
  
  stat_ecdf(na.rm=TRUE)+
  #scale_color_manual(values=c(rgb(224,29,27,maxColorValue=255),rgb(48,115,175,maxColorValue=255)))+
  xlim(-2, 2)+
  ylab(expression(hat(F)[n](x)))+
  xlab(parse(text=paste("Temperature (C","^o",")")))+
  geom_vline(xintercept = 1.490, color="red",linetype = "dashed",alpha = 0.5)+
  geom_vline(xintercept = -1.130, color= "blue",linetype = "dashed",alpha = 0.5)+
  scale_y_continuous(expand = c(0,0))+
  theme_bw()

base4

############################################################################################################################################
# Kruskal-Wallis Rank Sum Test AFGP Pos to Neg

kruskal.test(values ~ AFGP_content, 
             data = stack_all_species)

dunnTest(values ~ AFGP_content, 
          data = stack_all_species,
         method="none") 

############################################################################################################################################
# Kruskal-Wallis Rank Sum Test species

kruskal.test(values ~ ind, 
             data = stack_all_species)

#Post-hoc test 

dunnTest(values ~ ind, 
         data = stack_all_species,
         method="bonferroni") 
