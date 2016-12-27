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

base4 <- ggplot(stack_all_species, aes(x=stack_all_species[,1],colour = Species),color=brewer.pal(5,"Set1"))+
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



