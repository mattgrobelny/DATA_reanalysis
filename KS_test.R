# Data reanalysis for the thermal gradient project By: Mateusz Grobelny

######################################################################################################################################## Libraries
library(ggplot2)
library(RColorBrewer)
library(fields)
library(MASS)
library(FSA)
library(dunn.test)
library(lattice)


# Import data set
Master_species_gradient_logger_data <- read.csv("~/Documents/OneDrive/Antarctica Files/Data/Gradient Project/Hobodata/Large Tank/DATA_reanalysis/Master_species_gradient_logger_data.csv")

# set wd
setwd("~/Documents/OneDrive/Antarctica Files/Data/Gradient Project/Hobodata/Large Tank/DATA_reanalysis")

############################################################################################################################################ Kruskal-Wallis Rank Sum Test AFGP Pos to Neg data_kw = stack_all_species
data_kw = stack_all_species_1000
kruskal.test(Temperature ~ AFGP_content, data = data_kw)

dunnTest(Temperature ~ AFGP_content, data = data_kw, method = "bonferroni")

############################################################################################################################################ Kruskal-Wallis Rank Sum Test species

kruskal.test(Temperature ~ Species, data = data_kw)

# Post-hoc test
dunnTest(Temperature ~ Species, data = data_kw, method = "bonferroni")
