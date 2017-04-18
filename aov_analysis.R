
# The downloaded binary packages are in
# 	/var/folders/9h/3q0w1d8136g43p7rmltb_gg00000gn/T//RtmpYbSJSR/downloaded_packages

time_0_time_end_stack_aov <- rbind(stack_time_0, stack_time_end)
time_0_time_end_stack_aov[, 4] = time_0_time_end_stack_aov[, 2]

# Re name to one species name
time_0_time_end_stack_aov$Species <- sub("Ncor[0-9]", "Ncor", time_0_time_end_stack_aov$Species,
    perl = TRUE)
time_0_time_end_stack_aov$Species <- sub("Cwil[0-9]", "Cwil", time_0_time_end_stack_aov$Species,
    perl = TRUE)
time_0_time_end_stack_aov$Species <- sub("Lsquam[0-9]", "Lsquam", time_0_time_end_stack_aov$Species,
    perl = TRUE)
time_0_time_end_stack_aov$Species <- sub("Than[0-9]", "Than", time_0_time_end_stack_aov$Species,
    perl = TRUE)


# set to correct type
colnames(time_0_time_end_stack_aov) <- c("Temp", "Species", "TimeStage", "Subject")
time_0_time_end_stack_aov$Temp <- as.numeric(time_0_time_end_stack_aov$Temp)
time_0_time_end_stack_aov$TimeStage <- as.factor(time_0_time_end_stack_aov$TimeStage)
time_0_time_end_stack_aov$Species <- as.factor(time_0_time_end_stack_aov$Species)
time_0_time_end_stack_aov$Subject <- as.factor(time_0_time_end_stack_aov$Subject)

levels(time_0_time_end_stack_aov$Species) <- c("C.wilsoni", "L.squamifrons", "N.coriiceps",
    "T.hansoni")

summary(time_0_time_end_stack_aov)

aov = aov(Temperature ~ Species *AFGP_content, data= time_0_time_end_stack_w_afgp[which(time_0_time_end_stack_w_afgp$TimeStage=="TEnd"),1:4])
summary(aov)

tuk<- TukeyHSD(aov)
plot(tuk)
# stack_time_end2 = stack_time_end stack_time_end2$Species <- sub('Ncor[0-9]',
# 'Ncor', stack_time_end2$Species, perl=TRUE) stack_time_end2$Species <-
# sub('Cwil[0-9]', 'Cwil', stack_time_end2$Species, perl=TRUE)
# stack_time_end2$Species <- sub('Lsquam[0-9]', 'Lsquam',
# stack_time_end2$Species, perl=TRUE) stack_time_end2$Species <- sub('Than[0-9]',
# 'Than', stack_time_end2$Species, perl=TRUE) head(stack_time_end2)
# pairwise.wilcox.test(stack_time_end2$Temp, stack_time_end2$Species,
# p.adjust.method='holm')
