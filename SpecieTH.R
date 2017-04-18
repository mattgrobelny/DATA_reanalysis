# Libraries
library(ggplot2)
library(RColorBrewer)
library(fields)
library(MASS)
library(FSA)
library(dunn.test)
library(lattice)
library(coin)
library(graphics)
library(kernlab)
library(cluster)

#
speciesTH <- read.csv("~/Documents/OneDrive/Antarctica Files/Data/Gradient Project/Hobodata/Large Tank/DATA_reanalysis/speciesTH.csv")
speciesTH <- na.omit(speciesTH)
speciesTH
positions <- c("C. wilsoni", "T. hansoni ", "N. coriiceps","L. squamifrons")
speciesTH_plot <- ggplot(data= speciesTH, 
                       aes(x=speciesTH$Species,
                           y=speciesTH[,2]))+
  geom_bar(aes(fill=factor(speciesTH[,3], levels=c("Thermal Hysteresis","Blood Serum" ))),
           stat="identity",position = "stack")+
  scale_fill_manual(values=c('#e41a1c','#377eb8'))+
  geom_errorbar(aes(ymin=speciesTH[,5]-speciesTH[,4],
                    ymax=speciesTH[,5]+speciesTH[,4],width = 0.2))+
  scale_x_discrete(limits = positions,
                   labels=c(expression(italic("C. wilsoni"),
                                       italic("T. hansoni "),
                                                  italic("N. coriiceps"),
                                                             italic("L. squamifrons"))))+
  xlab("Species")+
  ylab("Freezing Point Depression (FPD) (°C)")+
  guides(fill=guide_legend(title="FPD Source (°C)"))+
  scale_y_continuous(expand = c(0,0), limits = c(0, 3.5))+
  theme_bw()+
  theme(
    legend.position = c(.999, .999),
    legend.justification = c("right", "top"),
    legend.box.just = "right")
  

speciesTH_plot
#############

ggsave(speciesTH_plot, file = "speciesTH_plot.png", dpi = 500)






