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
# Perumation (sim) test, unlike bootstrap which resamples itself, permuation mixes all groups together.
data_kw = stack_all_species_1000
#data_kw = stack_all_species

independence_test(Temperature ~ AFGP_content, 
                  data = data_kw,distribution = approximate(B = 100000))


independence_test(Temperature ~ Species, 
             data = data_kw, distribution = approximate(B = 20000))

