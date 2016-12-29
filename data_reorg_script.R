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

######################################################################################################################################## Print state summary for each indiviual
b = c(1:27)
stat_summary <- data.frame(summary(na.omit(Master_species_gradient_logger_data[,
    b])[(length(na.omit(Master_species_gradient_logger_data[, b])) - (15 * 60)):(length(na.omit(Master_species_gradient_logger_data[,
    b])) - (5 * 60))]))

######################################################################################################################################## Reorgnize data

# Ngib
Ngib_data <- c(Master_species_gradient_logger_data$Ngib1, Master_species_gradient_logger_data$Ngib2,
    Master_species_gradient_logger_data$Ngib3)

# Than
Than_data <- vector()
for (i in 4:9) {
    Than_data <- c(Than_data, Master_species_gradient_logger_data[, i])
}
# Than_data<-data.frame(Than_data)

# Cwil
Cwil_data <- vector()
for (i in 10:15) {
    Cwil_data <- c(Cwil_data, Master_species_gradient_logger_data[, i])
}
# Cwil_data<-data.frame(Cwil_data)

# Lsq
Lsq_data <- vector()
for (i in 16:21) {
    Lsq_data <- c(Lsq_data, Master_species_gradient_logger_data[, i])
}
# Lsq_data<-data.frame(Lsq_data)

# Ncor
Ncor_data <- vector()
for (i in 22:27) {
    Ncor_data <- c(Ncor_data, Master_species_gradient_logger_data[, i])
}
# Ncor_data<-data.frame(Ncor_data)

# combining all data
all_species <- vector()
all_species <- data.frame(Ngib_data, Than_data, Cwil_data, Lsq_data, Ncor_data)
all_species_no_Ngib <- data.frame(Ngib_data, Than_data, Cwil_data, Lsq_data, Ncor_data)

stack_all_species <- stack(all_species)


######################################################################################################################################## Reorganizing for data last num_last# datapoints
num_last = 500
# Ngib
# Ngib_data<-c(Master_species_gradient_logger_data$Ngib1,Master_species_gradient_logger_data$Ngib2,Master_species_gradient_logger_data$Ngib3)
Ngib_data_1000 <- vector()
for (i in 1:3) {
    Ngib_data_1000 <- c(Ngib_data_1000, tail(na.omit(Master_species_gradient_logger_data[,
        i]), n = num_last))
}

Ngib_data_1000 <- data.frame(Ngib_data_1000)

# Than
Than_data_1000 <- vector()
for (i in 4:9) {
    Than_data_1000 <- c(Than_data_1000, tail(na.omit(Master_species_gradient_logger_data[,
        i]), n = num_last))
}
Than_data_1000 <- data.frame(Than_data_1000)

# Cwil
Cwil_data_1000 <- vector()
for (i in 10:15) {
    Cwil_data_1000 <- c(Cwil_data_1000, tail(na.omit(Master_species_gradient_logger_data[,
        i]), n = num_last))
}
Cwil_data_1000 <- data.frame(Cwil_data_1000)

# Lsq
Lsq_data_1000 <- vector()
for (i in 16:21) {
    Lsq_data_1000 <- c(Lsq_data_1000, tail(na.omit(Master_species_gradient_logger_data[,
        i]), n = num_last))
}
Lsq_data_1000 <- data.frame(Lsq_data_1000)

# Ncor
Ncor_data_1000 <- vector()
for (i in 22:27) {
    Ncor_data_1000 <- c(Ncor_data_1000, tail(na.omit(Master_species_gradient_logger_data[,
        i]), n = num_last))
}
Ncor_data_1000 <- data.frame(Ncor_data_1000)

# combining all data last 1000 datapoints

all_species_1000 <- vector()
all_species_1000 <- data.frame(Ngib_data_1000, Than_data_1000, Cwil_data_1000, Lsq_data_1000,
    Ncor_data_1000)

stack_all_species_1000 <- stack(na.omit(all_species_1000))
################################################################################################################################################################ Add afgp pos or neg col
add_afgp_cat <- function(data, Lsq_data_name) {
    # REMOVE NAs
    data <- na.omit(data)
    # add new column
    data["AFGP_content"] <- NA
    total = length(data[, 1])
    pb <- txtProgressBar(min = 1, max = total, style = 3)
    for (i in 1:total) {
        if (data[i, 2] == as.character(Lsq_data_name)) {
            data[i, 4] = as.character("Neg")
            setTxtProgressBar(pb, i)
        } else {
            data[i, 3] <- as.character("Pos")
            setTxtProgressBar(pb, i)
        }

    }
    data[, 3] <- as.factor(data[, 3])
    names(data) <- c()
    filename <- paste(as.character(substitute(data)))

    write.table(data, filename, sep = ",")

    # change col names
    colnames(data) <- c("Skip", "Temperature", "Species", "AFGP_content")
    return(data)
}
################################################################################################################################################################ run me to add afgp pos neg column to dataset: stack_all_species_1000 =
################################################################################################################################################################ add_afgp_cat(stack_all_species_1000,Lsq_data_1000)

# run me to add afgp pos neg column to dataset: /// Takes a long time ///
# stack_all_species = add_afgp_cat(stack_all_species,Lsq_data)

# run me to add afgp pos neg column to dataset: /// Takes a long time ///
time_0_time_end_stack_w_afgp = add_afgp_cat(time_0_time_end_stack, "L.squamifrons")
summary(time_0_time_end_stack_w_afgp)
