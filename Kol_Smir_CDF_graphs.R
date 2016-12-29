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

setwd("~/Documents/OneDrive/Antarctica Files/Data/Gradient Project/Hobodata/Large Tank/DATA_reanalysis")


####################################################################################################
# Kolmogorov-Smirnov Tests
# distributions considered under the null hypothesis are continuous distributions but are otherwise unrestricted.
# non-parametric and distribution free

# No lsq and no ncor
all_species_no_Lsq_Ncor<-data.frame(c(na.omit(all_species[,1]),na.omit(all_species[,2]),na.omit(all_species[,3])))
colnames(all_species_no_Lsq_Ncor)<-c("Temp")

# organize temp data into AFGP_pos and AFGP_neg
AFGP_Pos_data<-data.frame(AFGP_Pos=all_species_no_Lsq_df)
colnames(AFGP_Pos_data)<-c("AFGP_Pos")
AFGP_Neg_data<-data.frame(AFGP_Neg=na.omit(all_species$Lsq_data))

#no Ncor
stacked_no_ncor <- stack_all_species[which(stack_all_species$Species !='Ncor_data'),1:3]

AFGP_Pos_data_no_Ncor<-data.frame(AFGP_Pos=stacked_no_ncor)

#no Ngib
stacked_no_Ngib<- stack_all_species[which(stack_all_species$Species !='Ngib_data' & stack_all_species$Species !='Lsq_data'),1]

AFGP_Pos_stacked_no_Ngib<-data.frame(AFGP_Pos=stacked_no_Ngib)

## Test for normality
#shapiro.test(AFGP_Pos_data$AFGP_Pos) # reject null (not normal distribution)
#qqnorm(AFGP_Pos_data$AFGP_Pos) # does not look normal (values do not fall on a straight line)

#shapiro.test(AFGP_Neg_data$AFGP_Neg) # reject null (not normal distribution)
#qqnorm(AFGP_Neg_data$AFGP_Neg) # does not look normal (values do not fall on a straight line)

## Plot CDFs 
# An empirical cumulative distribution function (CDF) is a non-parametric estimator of the underlying 
# CDF of a random variable.  It assigns a probability of 1/n to each datum, 
# orders the data from smallest to largest in value, and calculates the sum of the assigned probabilities 
# up to and including each datum.  The result is a step function that increases by 1/n at each datum.

AFGP_Pos.ecdf<-ecdf(AFGP_Pos_data$AFGP_Pos)
AFGP_Neg.ecdf<-ecdf(AFGP_Neg_data$AFGP_Neg)

# ## Plot individual CDF for AFGP Pos vs Neg
# plot(AFGP_Pos.ecdf,
#      xlab = parse(text=paste("Temperature (C","^o",")")), 
#      ylab = '', 
#      main = 'CDF of AFGP Positive Species Temperature Distribution'
# )
# mtext(text = expression(hat(F)[n](x)), side = 2, line = 2.5)
# 
# plot(AFGP_Neg.ecdf,
#      xlab = parse(text=paste("Temperature (C","^o",")")), 
#      ylab = '', 
#      main = 'CDF of AFGP Negative Species Temperature Distribution'
# )
# mtext(text = expression(hat(F)[n](x)), side = 2, line = 2.5)
# 
# ############################################################################################################
## Kolmogorov-Smirnov Tests
# The KS-test uses the maximum vertical deviation between the two curves as the statistic D.

KS_Test_AFGP_neg_vs_pos<-ks.test(AFGP_Pos_data$AFGP_Pos,AFGP_Neg_data$AFGP_Neg,alternative = "two.sided")
KS_Test_AFGP_neg_vs_pos


# redo test excluding Ncor as it may be a cold outlier
KS_Test_AFGP_neg_vs_pos_NO_NCOR<-ks.test(AFGP_Pos_data_no_Ncor$AFGP_Pos,AFGP_Neg_data$AFGP_Neg )
KS_Test_AFGP_neg_vs_pos_NO_NCOR
######## From KS test ---> Significant difference between the two distributions 
stack_all_species[which(stack_all_species$Species!="Ngib_data"),1:3]

# redo test excluding Ngib as sample size is too small to be used 
KS_Test_AFGP_neg_vs_pos_NO_Ngib<-ks.test(as.numeric(AFGP_Pos_stacked_no_Ngib$AFGP_Pos),as.numeric(AFGP_Neg_data$AFGP_Neg)) 
KS_Test_AFGP_neg_vs_pos_NO_Ngib


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
# select all data no Ncor 
stacked_no_ncor <- stack_all_species[which(stack_all_species$Species !='Ncor_data'),1:3]

no_Nor_AFGP_CDF <- ggplot(stacked_no_ncor, aes(x=stacked_no_ncor[,1],colour = AFGP_content))+
  
  stat_ecdf(na.rm=TRUE)+
  scale_color_manual(values=c(rgb(224,29,27,maxColorValue=255),rgb(48,115,175,maxColorValue=255)))+
  xlim(-2, 2)+
  ylab("Cumulative Probablility")+
  xlab(parse(text=paste("Temperature (C","^o",")")))+
  geom_vline(xintercept = 1.490, color="red",linetype = "dashed",alpha = 0.5)+
  geom_vline(xintercept = -1.130, color= "blue",linetype = "dashed",alpha = 0.5)+
  scale_y_continuous(expand = c(0,0)) +
  theme_bw()

no_Nor_AFGP_CDF
ggsave(no_Nor_AFGP_CDF, file="CDF_AFGP_pos_v_neg_no_Ncor.png", dpi = 500)

with_Nor_AFGP_CDF <- ggplot(stack_all_species, aes(x=stack_all_species[,1],colour = AFGP_content))+
  
  stat_ecdf(na.rm=TRUE)+
  scale_color_manual(values=c(rgb(224,29,27,maxColorValue=255),rgb(48,115,175,maxColorValue=255)))+
  xlim(-2, 2)+
  ylab("Cumulative Probablility")+
  xlab(parse(text=paste("Temperature (C","^o",")")))+
  geom_vline(xintercept = 1.490, color="red",linetype = "dashed",alpha = 0.5)+
  geom_vline(xintercept = -1.130, color= "blue",linetype = "dashed",alpha = 0.5)+
  scale_y_continuous(expand = c(0,0)) +
  theme_bw()

with_Nor_AFGP_CDF
ggsave(with_Nor_AFGP_CDF, file="CDF_AFGP_pos_v_neg_w_Ncor.png", dpi = 500)

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

ggsave(base4, file="CDF_all_species.png", dpi = 500)
############################################################################################################################################

colnames(stack_all_species)<-c("Temperature","Species","AFGP_content")

base4_noNgib <- ggplot(stack_all_species[which(stack_all_species$Species!="Ngib_data"),1:3], 
                aes(x=stack_all_species[which(stack_all_species$Species!="Ngib_data"),1],colour = Species),color=brewer.pal(4,"Set1"))+
  stat_ecdf(na.rm=TRUE)+
  #scale_color_manual(values=c(rgb(224,29,27,maxColorValue=255),rgb(48,115,175,maxColorValue=255)))+
  xlim(-2, 2)+
  ylab("Cumulative Probablility")+
  xlab(parse(text=paste("Temperature (C","^o",")")))+
  geom_vline(xintercept = 1.490, color="red",linetype = "dashed",alpha = 0.5)+
  geom_vline(xintercept = -1.130, color= "blue",linetype = "dashed",alpha = 0.5)+
  scale_y_continuous(expand = c(0,0))+
  theme_bw()

base4_noNgib

ggsave(base4_noNgib, file="CDF_all_species_noNgib.png", dpi = 500)

############################################################################################################################################

with_Nor_NO_Ngib_AFGP_CDF<- ggplot(stack_all_species[which(stack_all_species$Species!="Ngib_data"),1:3], 
                       aes(x=stack_all_species[which(stack_all_species$Species!="Ngib_data"),1],colour = AFGP_content))+
  stat_ecdf(na.rm=TRUE)+
  scale_color_manual(values=c(rgb(224,29,27,maxColorValue=255),rgb(48,115,175,maxColorValue=255)))+
  xlim(-2, 2)+
  ylab("Cumulative Probablility")+
  xlab(parse(text=paste("Temperature (C","^o",")")))+
  geom_vline(xintercept = 1.490, color="red",linetype = "dashed",alpha = 0.5)+
  geom_vline(xintercept = -1.130, color= "blue",linetype = "dashed",alpha = 0.5)+
  scale_y_continuous(expand = c(0,0)) +
  theme_bw()

with_Nor_NO_Ngib_AFGP_CDF
ggsave(with_Nor_NO_Ngib_AFGP_CDF, file="with_Nor_NO_Ngib_AFGP_CDF.png", dpi = 500)
