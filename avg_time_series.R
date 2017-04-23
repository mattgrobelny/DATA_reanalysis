setwd("~/Documents/OneDrive/Antarctica Files/Data/Gradient Project/Hobodata/Large Tank/DATA_reanalysis")


Than_data_Avg <-c(1:100)
Cwil_data_Avg<-c(1:100)
Lsq_data_Avg<-c(1:100)
Ncor_data_Avg<-c(1:100)
avg_time <- data.frame(Cwil_data_Avg,Than_data_Avg,Lsq_data_Avg,Ncor_data_Avg)

hundred_data <- vector("list", 27) 
for (i in 1:27) {
  hundred_data[[i]] <- matrix(nrow = 100, ncol = 1) 
}

for (temp_series in c(1:27)){
  list_r = na.omit(Master_species_gradient_logger_data[,temp_series])
  length_list=length(na.omit(Master_species_gradient_logger_data[,temp_series]))
  
  hundred_data[[temp_series]] <-split(list_r,rep(c(1:100),1,each=ceiling(length_list/100)))

} 
hundred_data[[1]][1]
Master_species_gradient_logger_data[1:11,1]
## Gets stats from 100 groups per species

avg_hundred_data<- data.frame(matrix(nrow = 100, ncol = 4) )
stat_hundred_data<- data.frame(matrix(nrow = 100, ncol = 4))
avg_hundred_data[,1]<-c(1:100)
stat_hundred_data[,1]<-c(1:100)
colnames(avg_hundred_data)<-c(colnames(all_species)[2:5])
colnames(stat_hundred_data)<-c(colnames(all_species)[2:5])

than_range=c(4:9)
cwill_range=c(11:15)
lsq_range = c(16:21)
ncor_range = c(22:27)
list_of_ranges =list(than_range,cwill_range,lsq_range,ncor_range)

count=0
for( range in list_of_ranges ){
  count=count+1
  print(count)
  for( i in c(1:100)){
   vals_to_work_with=c()
   for(z in range){
      #print("working on")
      #print(z)
      unlist_vals=unlist(hundred_data[[z]][i])
      #print(unlist_vals)
      vals_to_work_with = c( vals_to_work_with, unlist_vals)
      #print(vals_to_work_with)
    }
    avg_hundred_data[i,count] = median(vals_to_work_with)
    stat_hundred_data[i,count] = 2*(sd(vals_to_work_with)/sqrt(length(vals_to_work_with)))
  }
}
avg_hundred_data_STACK<-stack(avg_hundred_data)
stat_hundred_data_STACK<-stack(stat_hundred_data)
avg_hundred_data_STACK<-cbind(avg_hundred_data_STACK,rep(1:100,4))
stat_hundred_data_STACK<-cbind(stat_hundred_data_STACK,rep(1:100,4))

ggplot(data=avg_hundred_data_STACK, aes(x=avg_hundred_data_STACK$`rep(1:100, 4)`,y=avg_hundred_data_STACK$values,
                                        color=avg_hundred_data_STACK$ind))+
  geom_errorbar(data =stat_hundred_data_STACK, aes(ymin=avg_hundred_data_STACK$values -stat_hundred_data_STACK$values,
                                                   ymax=avg_hundred_data_STACK$values +stat_hundred_data_STACK$values, 
                                                   color= stat_hundred_data_STACK$ind))+
  geom_line()+
  theme_bw()

## Gets stats from 100 groups per indvidual by species 

avg_hundred_data<- data.frame(matrix(nrow = 100, ncol = 27) )
stat_hundred_data<- data.frame(matrix(nrow = 100, ncol = 27))
avg_hundred_data[,1]<-c(1:100)
stat_hundred_data[,1]<-c(1:100)
colnames(avg_hundred_data)<-colnames(Master_species_gradient_logger_data)
colnames(stat_hundred_data)<-colnames(Master_species_gradient_logger_data)

than_range=c(4:9)
cwill_range=c(11:15)
lsq_range = c(16:21)
ncor_range = c(22:27)
list_of_ranges =list(than_range,cwill_range,lsq_range,ncor_range)


for( i in c(1:100)){
    for(z in c(1:27)){
      vals_to_work_with=c()
      #print("working on")
      #print(z)
      unlist_vals=unlist(hundred_data[[z]][i])
      #print(unlist_vals)
      vals_to_work_with = c( vals_to_work_with, unlist_vals)
      #print(vals_to_work_with)
      avg_hundred_data[i,z] = median(vals_to_work_with)
      stat_hundred_data[i,z] = 2*(sd(vals_to_work_with)/sqrt(length(vals_to_work_with)))
    }
    
  }
avg_hundred_data_STACK<-stack(avg_hundred_data)
stat_hundred_data_STACK<-stack(stat_hundred_data)

cat_names <- c(rep("Ngib",100*3),rep("T. hansoni",100*6),rep("C. wilsoni",100*6),rep("L. squamifrons",100*6),rep("N. coriiceps",100*6))
avg_hundred_data_STACK<-cbind(rep(1:100,27),avg_hundred_data_STACK,stat_hundred_data_STACK$values,cat_names)

list_of_species<-as.list(unique(cat_names))
subset<-avg_hundred_data_STACK[which(avg_hundred_data_STACK$cat_names !="Ngib"),]

####################################################################################################

color_pallete=c("L. squamifrons"='red',"C. wilsoni"='blue', "T. hansoni"='dodgerblue4', "N. coriiceps"='navyblue')
  data_for_plot= subset
  positions <- c("L. squamifrons","C. wilsoni", "T. hansoni", "N. coriiceps")
  data_for_plot$cat_names<-factor(data_for_plot$cat_names, levels =positions)
  
  plot_out<- ggplot(data=data_for_plot, 
                    aes(x=data_for_plot$`rep(1:100, 27)`,
                        color=data_for_plot$cat_names))+
    # geom_errorbar(data =stat_hundred_data_STACK, aes(ymin=data_for_plot$values -stat_hundred_data_STACK$values,
    #                                                  ymax=data_for_plot$values +stat_hundred_data_STACK$values, 
    #                                                  color= stat_hundred_data_STACK$ind))+
    #geom_line(aes(y=data_for_plot$values,color=data_for_plot$ind),alpha=1)+
    geom_smooth(aes(y=data_for_plot$values,group=data_for_plot$cat_names),alpha=0.5, se=FALSE)+
    labs(colour ="Species")+
    xlab("Percent of Data")+
    ylab("Temperature (°C)")+
    guides(fill=guide_legend(title="Species"))+
    theme_bw()+
    geom_hline(yintercept = 1.49, color = "red",
               linetype = "dashed", alpha = 0.5) + 
    geom_hline(yintercept = -1.13, color = "blue",
               linetype = "dashed", alpha = 0.5) + 
    ylim(-1.13,1.49)+
    xlim(0,100)+
    scale_colour_manual(values=color_pallete, 
                        labels=c("L. squamifrons","C. wilsoni", "T. hansoni", "N. coriiceps"))+
    theme(legend.position = 'bottom', 
          legend.direction = "horizontal", 
          legend.box = "horizontal") + 
    guides(color = guide_legend(title.position = "top", 
                                # hjust = 0.5 centres the title horizontally
                                title.hjust = 0.5,
                                label.position = "bottom")) 
  plot_out
  ggsave(plot_out, file = paste(species,"minneg_posmax_allspecies_indvidual_percent_data.png", sep=''), dpi = 500)
  
  
  
  plot_out<- ggplot(data=data_for_plot, 
                    aes(x=data_for_plot$`rep(1:100, 27)`,
                        color=data_for_plot$cat_names))+
    # geom_errorbar(data =stat_hundred_data_STACK, aes(ymin=data_for_plot$values -stat_hundred_data_STACK$values,
    #                                                  ymax=data_for_plot$values +stat_hundred_data_STACK$values, 
    #                                                  color= stat_hundred_data_STACK$ind))+
    #geom_line(aes(y=data_for_plot$values,color=data_for_plot$ind),alpha=1)+
    geom_smooth(aes(y=data_for_plot$values,group=data_for_plot$cat_names),alpha=0.5)+
    labs(colour ="Species")+
    xlab("Percent of Data")+
    ylab("Temperature (°C)")+
    guides(fill=guide_legend(title="Species"))+
    theme_bw()+
    geom_hline(yintercept = 1.49, color = "red",
               linetype = "dashed", alpha = 0.5) + 
    geom_hline(yintercept = -1.13, color = "blue",
               linetype = "dashed", alpha = 0.5) + 
    ylim(-2,2)+
    xlim(0,100)+
    theme(legend.position = 'bottom', 
          legend.direction = "horizontal", 
          legend.box = "horizontal") + 
    scale_colour_manual(values=color_pallete, 
                        labels=c("L. squamifrons","C. wilsoni", "T. hansoni", "N. coriiceps"))+
    guides(color = guide_legend(title.position = "top", 
                                # hjust = 0.5 centres the title horizontally
                                title.hjust = 0.5,
                                label.position = "bottom")) 
  plot_out
  ggsave(plot_out, file = paste(species,"neg2_pos2_allspecies_indvidual_percent_data.png", sep=''), dpi = 500)
  

####################################################################################################
for(species in list_of_species){
  subset<-avg_hundred_data_STACK[which(avg_hundred_data_STACK$cat_names ==species),]
  #subset= avg_hundred_data_STACK
  
  data_for_plot= subset
  
  plot_out<- ggplot(data=data_for_plot, 
                    aes(x=data_for_plot$`rep(1:100, 27)`,
                        color=data_for_plot$cat_names))+
    # geom_errorbar(data =stat_hundred_data_STACK, aes(ymin=data_for_plot$values -stat_hundred_data_STACK$values,
    #                                                  ymax=data_for_plot$values +stat_hundred_data_STACK$values, 
    #                                                  color= stat_hundred_data_STACK$ind))+
    geom_line(aes(y=data_for_plot$values,color=data_for_plot$ind),alpha=0.9)+
    #geom_smooth(aes(y=data_for_plot$values,group=data_for_plot$cat_names),alpha=0.5)+
    labs(colour ="Species")+
    xlab("Percent of Data")+
    ylab("Temperature (°C)")+
    guides(fill=guide_legend(title="Species"))+
    theme_bw()+
    geom_hline(yintercept = 1.49, color = "red",
               linetype = "dashed", alpha = 0.5) + 
    geom_hline(yintercept = -1.13, color = "blue",
               linetype = "dashed", alpha = 0.5) + 
    ylim(-2,2)+
    xlim(0,100)+
    theme(legend.position = 'bottom', 
          legend.direction = "horizontal", 
          legend.box = "horizontal") + 
    guides(color = guide_legend(title.position = "top", 
                                # hjust = 0.5 centres the title horizontally
                                title.hjust = 0.5,
                                label.position = "bottom"))
  plot_out
  ggsave(plot_out, file = paste(species,"_indvidual_percent_data.png", sep=''), dpi = 500)
  
}
  ####################################################################################################
for(species in list_of_species){
subset<-avg_hundred_data_STACK[which(avg_hundred_data_STACK$cat_names ==species),]
#subset= avg_hundred_data_STACK

data_for_plot= subset

plot_out<- ggplot(data=data_for_plot, 
       aes(x=data_for_plot$`rep(1:100, 27)`,
           color=data_for_plot$cat_names))+
  # geom_errorbar(data =stat_hundred_data_STACK, aes(ymin=data_for_plot$values -stat_hundred_data_STACK$values,
  #                                                  ymax=data_for_plot$values +stat_hundred_data_STACK$values, 
  #                                                  color= stat_hundred_data_STACK$ind))+
  geom_line(aes(y=data_for_plot$values,color=data_for_plot$ind),alpha=0.3)+
  geom_smooth(aes(y=data_for_plot$values,group=data_for_plot$cat_names),alpha=0.5)+
  labs(colour ="Species")+
  xlab("Percent of Data")+
  ylab("Temperature (°C)")+
  guides(fill=guide_legend(title="Species"))+
  theme_bw()+
  geom_hline(yintercept = 1.49, color = "red",
             linetype = "dashed", alpha = 0.5) + 
  geom_hline(yintercept = -1.13, color = "blue",
             linetype = "dashed", alpha = 0.5) + 
  ylim(-2,2)+
  xlim(0,100)+
  theme(legend.position = 'bottom', 
        legend.direction = "horizontal", 
        legend.box = "horizontal") + 
  guides(color = guide_legend(title.position = "top", 
                              # hjust = 0.5 centres the title horizontally
                              title.hjust = 0.5,
                              label.position = "bottom"))
plot_out
ggsave(plot_out, file = paste(species,"All_percent_data.png", sep=''), dpi = 500)

}




