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

########################################################################################################################################
# data reorgnization

# Data for time start
number_of_start_end_dtp = 500
time_0 = data.frame(Master_species_gradient_logger_data[1:number_of_start_end_dtp,4:27])
colnames(time_0) <- c(colnames(Master_species_gradient_logger_data)[4:27])
# Data for time end
time_end<-data.frame(c(1:number_of_start_end_dtp))
for (i in 4:27){
  time_end[,i-3]<-tail(na.omit(Master_species_gradient_logger_data[,i]),n=number_of_start_end_dtp)
}
colnames(time_end) <- c(colnames(Master_species_gradient_logger_data)[4:27])

# restack all data
stack_time_0 <-stack(time_0)
stack_time_0[,3] <- "T0"
colnames(stack_time_0) <- c("Temp","Species","TimeStage")

stack_time_end <-stack(time_end)
stack_time_end[,3] <- "TEnd"
colnames(stack_time_end) <- c("Temp","Species","TimeStage")

# put together into one long list
time_0_time_end_stack <-rbind(stack_time_0,stack_time_end)
summary(time_0_time_end_stack)

# Re name to one species name
time_0_time_end_stack$Species <- sub("Ncor[0-9]", "Ncor", time_0_time_end_stack$Species, perl=TRUE)
time_0_time_end_stack$Species <- sub("Cwil[0-9]", "Cwil", time_0_time_end_stack$Species, perl=TRUE)
time_0_time_end_stack$Species <- sub("Lsquam[0-9]", "Lsquam", time_0_time_end_stack$Species, perl=TRUE)
time_0_time_end_stack$Species <- sub("Than[0-9]", "Than", time_0_time_end_stack$Species, perl=TRUE)

# set to correct type
time_0_time_end_stack$Temp<-as.numeric(time_0_time_end_stack$Temp)
time_0_time_end_stack$TimeStage<-as.factor(time_0_time_end_stack$TimeStage)
time_0_time_end_stack$Species<-as.factor(time_0_time_end_stack$Species)

summary(time_0_time_end_stack)
levels(time_0_time_end_stack$Species) <- c("C.wilsoni","L.squamifrons", "N.coriiceps", "T.hansoni")

# NA are from empty rows from Ngib
#Are main effects or interaction effects present in the independent variables?

# Lsq t0 vs t_end
wilcox.test(Temp ~ TimeStage, 
            data= time_0_time_end_stack[which(time_0_time_end_stack$Species=="L.squamifrons"),1:3],
            alternative = "two.sided",
            conf.int =TRUE)

# Ncor t0 vs t_end
wilcox.test(Temp ~ TimeStage, 
            data= time_0_time_end_stack[which(time_0_time_end_stack$Species=="N.coriiceps"),1:3],
            alternative = "two.sided",
            conf.int =TRUE)

# Than t0 vs t_end
wilcox.test(Temp ~ TimeStage, 
            data= time_0_time_end_stack[which(time_0_time_end_stack$Species=="T.hansoni"),1:3],
            alternative = "two.sided",
            conf.int =TRUE)


# Cwil t0 vs t_end
wilcox.test(Temp ~ TimeStage, 
            data= time_0_time_end_stack[which(time_0_time_end_stack$Species=="C.wilsoni"),1:3], 
            alternative = "two.sided",
            conf.int =TRUE)

######################################################################
# No afgp stratfication 
#use kruskal_test for signficance using a sim permutation data set 
# Are the distriubtions of median temps which the fish were found at, at the end of the expereiment different?
ks_test = kruskal_test(Temp ~ Species, 
             data= time_0_time_end_stack[which(time_0_time_end_stack$TimeStage=="TEnd"),1:3],conf.int =TRUE,
             distribution = approximate(B = 10000))
ks_test
# P-value ≤ α: The differences between some of the medians are statistically significant

# How are the median temperatures distributions different between fish species, at the end of the expereiment?
dunnTest(Temp ~ Species, 
         data= time_0_time_end_stack[which(time_0_time_end_stack$TimeStage=="TEnd"),1:3],
         two.sided= TRUE, method="bonferroni")
dunnTest(Temp ~ Species, 
         data= time_0_time_end_stack[which(time_0_time_end_stack$TimeStage=="TEnd"),1:3],
         two.sided= FALSE, method="bonferroni")

######################################################################
# yes afgp stratification 

time_0_time_end_stack_w_afgp
colnames(time_0_time_end_stack_w_afgp) <- c("Temperature", "Species","TimeStage" ,"AFGP_content")

ks_test2 = kruskal_test(Temperature ~ Species, 
                       data= time_0_time_end_stack_w_afgp[which(time_0_time_end_stack_w_afgp$TimeStage=="TEnd"),1:4],conf.int =TRUE,
                       distribution = approximate(B = 10000))
ks_test2

# How are the median temperatures distributions different between fish species, at the end of the expereiment?
dunnTest(Temperature ~ Species, 
         data= time_0_time_end_stack_w_afgp[which(time_0_time_end_stack_w_afgp$TimeStage=="TEnd"),1:4],
         two.sided= TRUE, method="bonferroni")

dunnTest(Temperature ~ AFGP_content, 
         data= time_0_time_end_stack_w_afgp[which(time_0_time_end_stack_w_afgp$TimeStage=="TEnd"),1:4],
         two.sided= TRUE, method="bonferroni")

######################################################################
# pairwise test of end values 
# 
stack_time_end2 = stack_time_0
stack_time_end2$Species <- sub("Ncor[0-9]", "Ncor", stack_time_end2$Species, perl=TRUE)
stack_time_end2$Species <- sub("Cwil[0-9]", "Cwil", stack_time_end2$Species, perl=TRUE)
stack_time_end2$Species <- sub("Lsquam[0-9]", "Lsquam", stack_time_end2$Species, perl=TRUE)
stack_time_end2$Species <- sub("Than[0-9]", "Than", stack_time_end2$Species, perl=TRUE)
head(stack_time_end2)
pairwise.wilcox.test(stack_time_end2$Temp, stack_time_end2$Species, p.adjust.method="bonf")





