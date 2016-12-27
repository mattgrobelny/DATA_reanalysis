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
#Master_species_gradient_logger_data <- read.csv("~/Documents/OneDrive/Antarctica Files/Data/Gradient Project/Hobodata/Large Tank/DATA_reanalysis/Master_species_gradient_logger_data.csv")

#set wd
setwd("~/Documents/OneDrive/Antarctica Files/Data/Gradient Project/Hobodata/Large Tank/DATA_reanalysis")

############################################################################################################################################
# Bootstraping
# bootobject <- boot(data= , statistic= , R=, ...) where



# run bootstrap sampling r_val number of times for each species (cumulative data from all individuals from each species)
# statfuction determines the collection of stat from each sampling run
# statfuntion = samplemean // collect mean values from each bootrun
# statfuntion = variance // collect variance values from each bootrun
# function to obtain mean from the boot run 
samplemean <- function(x, d) {
  return(mean(x[d]))
}
# function to obtain variance from the boot run 
samplevar <- function(x, d) {
  return(var(x[d]))
}

run_boot_strap <- function(data,statfuntion,r_val){

  
results_lsq <- boot(data=na.omit(data$Lsq_data), statistic=statfuntion, R=r_val)
results_ncor <- boot(data=na.omit(data$Ncor_data), statistic=statfuntion, R=r_val)
results_than <- boot(data=na.omit(data$Than_data), statistic=statfuntion, R=r_val)
results_cwil <- boot(data=na.omit(data$Cwil_data), statistic=statfuntion, R=r_val)
results_Ngib <- boot(data=na.omit(data$Ngib_data), statistic=statfuntion, R=r_val)


# # view results
# results_lsq
# results_ncor
# results_than
# results_cwil
# results_Ngib

plot(results_lsq)
title<-paste(names(data)[4],"of",as.character(substitute(statfuntion)))
title(sub=title)

plot(results_ncor)
title<-paste(names(data)[5],"of",as.character(substitute(statfuntion)))
title(sub=title)

plot(results_than)
title<-paste(names(data)[2],"of",as.character(substitute(statfuntion)))
title(sub=title)

plot(results_cwil)
title<-paste(names(data)[3],"of:",as.character(substitute(statfuntion)))
title(sub=title)

plot(results_Ngib)
title<-paste(names(data)[1],"of:",as.character(substitute(statfuntion)))
title(sub=title)

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

boot_data = data.frame(c("Lsq","Ncor","Than","Cwil","Ngib"),boot_strap_stats,boot_strap_ci_Low, boot_strap_ci_high,boot_strap_rep)

colnames(boot_data)<-c("Species","Boot_mean","Boot_CI_low","Boot_CI_high","Boot_reps")
return(boot_data)
}
############################################################################################################################################

# Run bootstrap functions on all data  
r_val = 1000
boot_data_all_data = run_boot_strap(all_species, samplemean, r_val)
boot_data_var_all_data = run_boot_strap(all_species,samplevar, r_val)

boot_data_all_data
boot_data_var_all_data

# Run bootstrap fucntion on last 500 data points 
r_val = 1000
boot_data_all_data = run_boot_strap(all_species, samplemean, r_val)
boot_data_var_all_data = run_boot_strap(all_species,samplevar, r_val)

boot_data_all_data
boot_data_var_all_data
# This is not how you would do a simulation test (not a bootstrap test here). 
# What you want to do mix all the data together and then randomly redivide into two new groups find 
# the mean of each group take the difference and plot it. Repeat lots of times, 10,000 for instance. 
# Then you can find a p-value but counting all the results as or more extreme than your observed result 
# (the original difference in means) and divide by 10000. This is a non-parametric version of a t-test called
# a permutation test. However, you could use a t-test for difference in means but there are more assumptions 
# about the data than this test.

############################################################################################################################################