
library(here)
library(patchwork)
library(tidyverse)
library(lubridate) #date changes
library(MetBrewer) #color palette
library(ggbreak) #break y scale


#check to see that here() leads to root directory or top-level folder
#should be Seattle_Aquarium_CleanSeas_PIP
here()

#if incorrect, enter `set_here(path = "...")`
#restart RStudio
#check here() again to make sure you're in the right top-level folder

#i label the non pip plastics data as controls  

#import data
elisa <- read_csv(here("data", "PIP_ELISA.csv"))
env_data <- read_csv(here("data", "KingCounty_EnvData.csv"))
mass <- read_csv(here("data", "Mass.csv"))
temp.0 <- read_csv(here("data", "hobo_temp", "Shallow.csv"))
temp.10 <- read_csv(here("data", "hobo_temp", "Deep.csv"))
env_data <- read_csv(here("data", "KingCounty_EnvData.csv"))
ElectroC <- read_csv(here("data", "Electro_Controls.csv"))
TensileC <- read_csv(here("data", "Tensile_Controls.csv"))
TensileCS <- read_csv(here("data", "Tensile_Controls_Scatter.csv")) #for scatterplot

##############################################################################
#Tensile Strength Analysis controls Scatterplot
##############################################################################

names(TensileCS) [1] <- "Time"
names(TensileCS) [2] <- "Depth"
names(TensileCS) [3] <- "Sample.ID"
names(TensileCS) [4] <- "Thickness"
names(TensileCS) [5] <- "Modulus"
names(TensileCS) [6] <- "Yield.Stress"
names(TensileCS) [7] <- "Yield.Elongation"
names(TensileCS) [8] <- "Break.Elongation"
names(TensileCS) [9] <- "Energy.Break"
names(TensileCS) [10] <- "Post.Modulus"
names(TensileCS) [11] <- "Post.Yield.Stress"
names(TensileCS) [12] <- "Post.Yield.Elongation"
names(TensileCS) [13] <- "Post.Break.Elongation"
names(TensileCS) [14] <- "Post.Energy.Break"

TensileCS$Yield.Elongation <- as.numeric(TensileCS$Yield.Elongation)
TensileCS$Yield.Stress <- as.numeric(TensileCS$Yield.Stress)
TensileCS$Modulus <- as.numeric(TensileCS$Modulus)
TensileCS$Break.Elongation <- as.numeric(TensileCS$Break.Elongation)
TensileCS$Energy.Break <- as.numeric(TensileCS$Energy.Break)
TensileCS$Post.Yield.Elongation <- as.numeric(TensileCS$Post.Yield.Elongation)
TensileCS$Post.Yield.Stress <- as.numeric(TensileCS$Post.Yield.Stress)
TensileCS$Post.Modulus <- as.numeric(TensileCS$Post.Modulus)
TensileCS$Post.Break.Elongation <- as.numeric(TensileCS$Post.Break.Elongation)
TensileCS$Post.Energy.Break <- as.numeric(TensileCS$Post.Energy.Break)
TensileCS$Time <- as.factor(TensileCS$Time)
TensileCS$Sample.ID <- as.factor(TensileCS$Sample.ID)
#TensileCS$Depth <- as.factor(TensileCS$Depth)


modulus.scatter.plot <- ggplot(TensileCS, aes(x=Modulus, y=Post.Modulus, shape=Time, color=Sample.ID, size=Depth)) +
  geom_point() +
  ggtitle("Young's Modulus (MPa)") +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(values = met.brewer("Archambault", 6), name="Sample") +
  scale_size_manual(values=c(2, 2, 4), breaks = c("None", "Zero", "Ten"), labels=c("None", "0 m", "10 m")) +
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  scale_y_continuous(expand = c(0,0), limits=c(0, 2100), breaks = c(500, 1000, 1500, 2000)) +
  scale_x_continuous(expand = c(0,0), limits=c(0, 2100), breaks = c(500, 1000, 1500, 2000)) +
  xlab(expression(paste("Pre-Whale Gut Simulation"))) +
  ylab(expression(paste("Post-Whale Gut Simulation")))

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(modulus.scatter.plot)

#ggsave(here("output", "modulus scatter plot.png"))

Yield.Stress.plot <- ggplot(TensileCS, aes(x=Yield.Stress, y=Post.Yield.Stress, shape=Time, color=Sample.ID, size=Depth)) +
  geom_point() +
  ggtitle("Yield Stress (MPa)") + 
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(values = met.brewer("Archambault", 6)) +
  scale_size_manual(values=c(2, 2, 4)) +
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  theme(legend.position = "none") +
  scale_y_continuous(expand = c(0,0), limits=c(0, 21), breaks = c(5, 10, 15, 20)) +
  scale_x_continuous(expand = c(0,0), limits=c(0, 21), breaks = c(5, 10, 15, 20)) +
  xlab(expression(paste("Pre-Whale Gut Simulation"))) +
  ylab(expression(paste("Post-Whale Gut Simulation")))

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(Yield.Stress.plot)

#ggsave(here("output", "Yield Stress plot.png"))

Yield.Elongation.plot <- ggplot(TensileCS, aes(x=Yield.Elongation, y=Post.Yield.Elongation, shape=Time, color=Sample.ID, size=Depth)) +
  geom_point() +
  ggtitle("Yield Elongation (%)") + 
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(values = met.brewer("Archambault", 6), name="Sample") +
  scale_size_manual(values=c(2, 2, 4), breaks = c("None", "Zero", "Ten"), labels=c("None", "0 m", "10 m")) +
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  theme(legend.position = "none") +
  scale_y_continuous(expand = c(0,0), limits=c(0, 15), breaks = c(5, 10, 15)) +
  scale_x_continuous(expand = c(0,0), limits=c(0, 15), breaks = c(5, 10, 15)) +
  xlab(expression(paste("Pre-Whale Gut Simulation"))) +
  ylab(expression(paste("Post-Whale Gut Simulation")))

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,8, record=T)

print(Yield.Elongation.plot)

#ggsave(here("output", "Yield Elongation plot.png"))

Break.Elongation.plot <- ggplot(TensileCS, aes(x=Break.Elongation, y=Post.Break.Elongation, shape=Time, color=Sample.ID, size=Depth)) +
  geom_point() +
  ggtitle("Break Elongation (%)") + 
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(values = met.brewer("Archambault", 6)) +
  scale_size_manual(values=c(2, 2, 4)) +
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  scale_y_continuous(expand = c(0,0), limits=c(0, 600), breaks = c(200, 400)) +
  scale_x_continuous(expand = c(0,0), limits=c(0, 600), breaks = c(200, 400)) +
  theme(legend.position = "none") +
  xlab(expression(paste("Pre-Whale Gut Simulation"))) +
  ylab(expression(paste("Post-Whale Gut Simulation")))

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(Break.Elongation.plot)

#ggsave(here("output", "Break Elongation plot.png"))


Energy.Break.plot <- ggplot(TensileCS, aes(x=Energy.Break, y=Post.Energy.Break, shape=Time, color=Sample.ID, size=Depth)) +
  geom_point() +
  ggtitle("Energy at Break (J)") + 
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(values = met.brewer("Archambault", 6), name="Sample") +
  scale_size_manual(values=c(2, 2, 4), breaks = c("None", "Zero", "Ten"), labels=c("None", "0 m", "10 m")) +
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  theme(legend.position = "none") +
  scale_y_continuous(expand = c(0,0), limits=c(0, 7), breaks=c(2, 4, 6)) +
  scale_x_continuous(expand = c(0,0), limits=c(0, 7), breaks=c(2, 4, 6)) +
  xlab(expression(paste("Pre-Whale Gut Simulation"))) +
  ylab(expression(paste("Post-Whale Gut Simulation")))

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(20,15, record=T)

print(Energy.Break.plot)

#ggsave(here("output", "Energy at Break plot.png"))


################Final Tensile Figure#######################3

scatter.tensile.plot <- (modulus.scatter.plot) + (Yield.Stress.plot) + (Yield.Elongation.plot) + (Break.Elongation.plot) + (Energy.Break.plot) +
  plot_annotation(title = "Tensile Strength") +
  plot_layout(guides="collect")
  
  
graphics.off()

#open new window, change size if needed
windows(20,15, record=T)

print(scatter.tensile.plot)

#ggsave(here("output", "Scatter Tensile plot.png"))

###############################################################################################################

#evans plots: elastic modulus, yield strain, yield stress, break strain, tensile strength and breaking energy

#change column names
names(TensileC) [1] <- "Time"
names(TensileC) [2] <- "Pre.Post"
names(TensileC) [3] <- "Depth"
names(TensileC) [4] <- "Sample.ID"
names(TensileC) [10] <- "Maximum.Load"
names(TensileC) [11] <- "Modulus"
names(TensileC) [12] <- "Yield.Stress"
names(TensileC) [13] <- "Yield.Elongation"
names(TensileC) [14] <- "Tensile.Stress.Break"
names(TensileC) [15] <- "Break.Elongation"
names(TensileC) [16] <- "Tensile.Stress.Maxforce"
names(TensileC) [17] <- "Tensile.Strain.Maxforce"
names(TensileC) [18] <- "Energy.Yield"
names(TensileC) [19] <- "Energy.Break"
names(TensileC) [20] <- "Break.Location"
names(TensileC) [21] <- "Collector"

#change data to numeric
TensileC$Maximum.Load <- as.numeric(TensileC$Maximum.Load)
TensileC$Yield.Elongation <- as.numeric(TensileC$Yield.Elongation)
TensileC$Yield.Stress <- as.numeric(TensileC$Yield.Stress)
TensileC$Modulus <- as.numeric(TensileC$Modulus)
TensileC$Break.Elongation <- as.numeric(TensileC$Break.Elongation)
TensileC$Tensile.Stress.Break <- as.numeric(TensileC$Tensile.Stress.Break)
TensileC$Tensile.Stress.Maxforce <- as.numeric(TensileC$Tensile.Stress.Maxforce)
TensileC$Energy.Break <- as.numeric(TensileC$Energy.Break)

#######################################################################33333
#time 0
tensile_0 <- TensileC %>%
  filter(Time == 0)

#order so that pre is first, post is second
tensile_0$Pre.Post <- factor(tensile_0$Pre.Post, levels = c("Pre", "Post"))

#pulling out only mid break data (didn't use data where material broke at grip)
tensile_0 <- tensile_0 %>%
 filter(Break.Location %in% c("mid","grip"))

#plots
load.plot.0 <- ggplot(data = tensile_0, aes(x = Sample.ID, y = Maximum.Load, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Maximum Load") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Maximum Load (N)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10))  


modulus.plot.0 <- ggplot(data = tensile_0, aes(x = Sample.ID, y = Modulus, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Modulus of Elasticity") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Modulus of Elasticity (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10))  

yield.stress.plot.0 <- ggplot(data = tensile_0, aes(x = Sample.ID, y = Yield.Stress, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Yield Stress") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Yield Stress (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

yield.elong.plot.0 <- ggplot(data = tensile_0, aes(x = Sample.ID, y = Yield.Elongation, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Yield Elongation") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Yield Elongation (%)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

break.elong.plot.0 <- ggplot(data = tensile_0, aes(x = Sample.ID, y = Break.Elongation, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Break Elongation") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Break Elongation (%)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

stress.MF.plot.0 <- ggplot(data = tensile_0, aes(x = Sample.ID, y = Tensile.Stress.Maxforce, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Tensile Stress at Max Force") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Tensile Stress at Yield \n(Slope Threshold 0%; MPa"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

stress.break.plot.0 <- ggplot(data = tensile_0, aes(x = Sample.ID, y = Tensile.Stress.Break, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Tensile Stress at Break") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Tensile Stress at Break \n(Standard; MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

energy.break.plot.0 <- ggplot(data = tensile_0, aes(x = Sample.ID, y = Energy.Break, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Energy at Break") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Energy at Break (J)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

#put all plots together
#tensile.plot.0 <- (load.plot.0 + modulus.plot.0) /(yield.stress.plot.0 + yield.elong.plot.0) /(break.elong.plot.0 + stress.MF.plot.0) /
  #(stress.break.plot.0 + energy.break.plot.0) + 
  #plot_annotation(title = "Tensile Testing - Month 0")

tensile.plot.0 <- (energy.break.plot.0 + modulus.plot.0 + break.elong.plot.0) +
  plot_annotation(title = "Tensile Testing - Month 0")

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(tensile.plot.0)

#ggsave(here("output", "Tensile Time 0 Controls.png"))
#ggsave(here("output", "Final Figures", "Tensile Time 0.png"))


#time 4, 0 m
tensile_4 <- TensileC %>%
  filter(Time == 4) %>%
  filter(Depth == 0)

tensile_4$Pre.Post <- factor(tensile_4$Pre.Post, levels = c("Pre", "Post"))

#pulling out only mid break data
tensile_4 <- tensile_4 %>%
  filter(Break.Location %in% c("mid","grip"))

#plots
load.plot.4 <- ggplot(data = tensile_4, aes(x = Sample.ID, y = Maximum.Load, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Maximum Load") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Maximum Load (N)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10))  


modulus.plot.4 <- ggplot(data = tensile_4, aes(x = Sample.ID, y = Modulus, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Modulus of Elasticity") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Modulus of Elasticity (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10))  

yield.stress.plot.4 <- ggplot(data = tensile_4, aes(x = Sample.ID, y = Yield.Stress, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Yield Stress") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Yield Stress (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

yield.elong.plot.4 <- ggplot(data = tensile_4, aes(x = Sample.ID, y = Yield.Elongation, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Yield Elongation") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Yield Elongation (%)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

break.elong.plot.4 <- ggplot(data = tensile_4, aes(x = Sample.ID, y = Break.Elongation, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Break Elongation") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Break Elongation (%)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

stress.MF.plot.4 <- ggplot(data = tensile_4, aes(x = Sample.ID, y = Tensile.Stress.Maxforce, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Tensile Stress at Max Force") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Tensile Stress at Yield \n(Slope Threshold 0%; MPa"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

stress.break.plot.4 <- ggplot(data = tensile_4, aes(x = Sample.ID, y = Tensile.Stress.Break, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Tensile Stress at Break") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Tensile Stress at Break \n(Standard; MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

energy.break.plot.4 <- ggplot(data = tensile_4, aes(x = Sample.ID, y = Energy.Break, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Energy at Break") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Energy at Break (J)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

#put plots together
#tensile.plot.4 <- (load.plot.4 + modulus.plot.4) /(yield.stress.plot.4 + yield.elong.plot.4) /(break.elong.plot.4 + stress.yield.plot.4) /
 # (stress.break.plot.4 + energy.break.plot.4)+ plot_annotation (title = "Tensile Testing - Month 4, 0 m")

tensile.plot.4 <- (energy.break.plot.4 + modulus.plot.4 + break.elong.plot.4) +
plot_annotation(title = "Tensile Testing - Month 4 Depth 0")


#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(tensile.plot.4)

#ggsave(here("output", "Tensile Time 4 Controls, 0m.png"))
#ggsave(here("output", "Final Figures", "Tensile Time 4, 0m.png"))

#print(energy.break.plot.4)
#ggsave(here("output", "Final Figures", "Energy at break Time 4, 0m.png"))


#time 4, 10 m
tensile_4.10 <- TensileC %>%
  filter(Time == 4) %>%
  filter(Depth == 10)

tensile_4.10$Pre.Post <- factor(tensile_4.10$Pre.Post, levels = c("Pre", "Post"))

#pulling out only mid break data
tensile_4.10 <- tensile_4.10 %>%
  filter(Break.Location %in% c("mid","grip"))

#plots
load.plot.4.10 <- ggplot(data = tensile_4.10, aes(x = Sample.ID, y = Maximum.Load, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Maximum Load") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Maximum Load (N)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10) )  


modulus.plot.4.10 <- ggplot(data = tensile_4.10, aes(x = Sample.ID, y = Modulus, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Modulus of Elastcity") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Modulus of Elasticity (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10) )  

yield.stress.plot.4.10 <- ggplot(data = tensile_4.10, aes(x = Sample.ID, y = Yield.Stress, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Yield Stress") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Yield Stress (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10) ) 

yield.elong.plot.4.10 <- ggplot(data = tensile_4.10, aes(x = Sample.ID, y = Yield.Elongation, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Yield Elongation") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Yield Elongation (%)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10) ) 

break.elong.plot.4.10 <- ggplot(data = tensile_4.10, aes(x = Sample.ID, y = Break.Elongation, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Break Elongation") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Break Elongation (%)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10) ) 

stress.yield.plot.4.10 <- ggplot(data = tensile_4.10, aes(x = Sample.ID, y = Tensile.Stress.Maxforce, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Tensile Stress at Yield") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Tensile Stress at Yield \n(Slope Threshold 0%; MPa"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10) ) 

stress.break.plot.4.10 <- ggplot(data = tensile_4.10, aes(x = Sample.ID, y = Tensile.Stress.Break, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Tensile Stress at Break") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Tensile Stress at Break \n(Standard; MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10) ) 

energy.break.plot.4.10 <- ggplot(data = tensile_4.10, aes(x = Sample.ID, y = Energy.Break, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Energy at Break") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Energy at Break (J)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10) ) 

#put all plots together
#tensile.plot.4.10 <- (load.plot.4.10 + modulus.plot.4.10) /(yield.stress.plot.4.10 + yield.elong.plot.4.10) /
 # (break.elong.plot.4.10 + stress.yield.plot.4.10) /
  #(stress.break.plot.4.10 + energy.break.plot.4.10)+ plot_annotation (title = "Tensile Testing - Month 4, 10 m")

tensile.plot.4.10 <- (energy.break.plot.4.10 + modulus.plot.4.10 + break.elong.plot.4.10) +
plot_annotation(title = "Tensile Testing - Month 4 Depth 10")

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(tensile.plot.4.10)

#ggsave(here("output", "Tensile Time 4 Controls, 10m.png"))
#ggsave(here("output", "Final Figures", "Tensile Time 4, 10m.png"))

#print(energy.break.plot.4.10)
#ggsave(here("output", "Final Figures", "Energy at break Time 4, 10m.png"))

#time 8, depth 0
tensile_8 <- TensileC %>%
  filter(Time == 8) %>%
  filter(Depth == 0)

tensile_8$Pre.Post <- factor(tensile_8$Pre.Post, levels = c("Pre", "Post"))

#pulling out only mid break data
tensile_8 <- tensile_8 %>%
  filter(Break.Location %in% c("mid","grip"))

#plots
load.plot.8 <- ggplot(data = tensile_8, aes(x = Sample.ID, y = Maximum.Load, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Maximum Load") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Maximum Load (N)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10) )  


modulus.plot.8 <- ggplot(data = tensile_8, aes(x = Sample.ID, y = Modulus, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Modulus of Elastcity") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Modulus of Elasticity (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10))  

yield.stress.plot.8 <- ggplot(data = tensile_8, aes(x = Sample.ID, y = Yield.Stress, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Yield Stress") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Yield Stress (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

yield.elong.plot.8 <- ggplot(data = tensile_8, aes(x = Sample.ID, y = Yield.Elongation, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Yield Elongation") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Yield Elongation (%)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

break.elong.plot.8 <- ggplot(data = tensile_8, aes(x = Sample.ID, y = Break.Elongation, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Break Elongation") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Break Elongation (%)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

stress.yield.plot.8 <- ggplot(data = tensile_8, aes(x = Sample.ID, y = Tensile.Stress.Maxforce, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Tensile Stress at Yield") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Tensile Stress at Yield \n(Slope Threshold 0%; MPa"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

stress.break.plot.8 <- ggplot(data = tensile_8, aes(x = Sample.ID, y = Tensile.Stress.Break, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Tensile Stress at Break") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Tensile Stress at Break \n(Standard; MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

energy.break.plot.8 <- ggplot(data = tensile_8, aes(x = Sample.ID, y = Energy.Break, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Energy at Break") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Energy at Break (J)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

#put all plots together
#tensile.plot.8 <- (load.plot.8 + modulus.plot.8) /(yield.stress.plot.8 + yield.elong.plot.8) /(break.elong.plot.8 + stress.yield.plot.8) /
 # (stress.break.plot.8 + energy.break.plot.8) + plot_annotation(title = "Tensile Testing - Month 8, 0 m")

tensile.plot.8 <- (energy.break.plot.8 + modulus.plot.8 + break.elong.plot.8) +
plot_annotation(title = "Tensile Testing - Month 8 Depth 0")


#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(tensile.plot.8)

#ggsave(here("output", "Tensile Time 8 Controls, 0m.png"))
#ggsave(here("output", "Final Figures", "Tensile Time 8, 0m.png"))

#print(energy.break.plot.8)
#ggsave(here("output", "Final Figures", "Energy at break, Time 8, 0m.png"))

#time 8, depth 10
tensile_8.10 <- TensileC %>%
  filter(Time == 8) %>%
  filter(Depth == 10)

tensile_8.10$Pre.Post <- factor(tensile_8.10$Pre.Post, levels = c("Pre", "Post"))

#pulling out only mid break data
tensile_8.10 <- tensile_8.10 %>%
  filter(Break.Location %in% c("mid","grip"))

#plots
load.plot.8.10 <- ggplot(data = tensile_8.10, aes(x = Sample.ID, y = Maximum.Load, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Maximum Load") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Maximum Load (N)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10))  


modulus.plot.8.10 <- ggplot(data = tensile_8.10, aes(x = Sample.ID, y = Modulus, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Modulus of Elasticity") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Modulus of Elasticity (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10))  

yield.stress.plot.8.10 <- ggplot(data = tensile_8.10, aes(x = Sample.ID, y = Yield.Stress, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Yield Stress") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Yield Stress (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

yield.elong.plot.8.10 <- ggplot(data = tensile_8.10, aes(x = Sample.ID, y = Yield.Elongation, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Yield Elongation") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Yield Elongation (%)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

break.elong.plot.8.10 <- ggplot(data = tensile_8.10, aes(x = Sample.ID, y = Break.Elongation, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Break Elongation") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Break Elongation (%)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

stress.yield.plot.8.10 <- ggplot(data = tensile_8.10, aes(x = Sample.ID, y = Tensile.Stress.Maxforce, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Tensile Stress at Yield") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Tensile Stress at Yield \n(Slope Threshold 0%; MPa"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

stress.break.plot.8.10 <- ggplot(data = tensile_8.10, aes(x = Sample.ID, y = Tensile.Stress.Break, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Tensile Stress at Break") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Tensile Stress at Break \n(Standard; MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

energy.break.plot.8.10 <- ggplot(data = tensile_8.10, aes(x = Sample.ID, y = Energy.Break, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Energy at Break") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Energy at Break (J)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 


#tensile.plot.8.10 <- (load.plot.8.10 + modulus.plot.8.10) /(yield.stress.plot.8.10 + yield.elong.plot.8.10) /
 # (break.elong.plot.8.10 + stress.yield.plot.8.10) /
  #(stress.break.plot.8.10 + energy.break.plot.8.10) + plot_annotation(title = "Tensile Testing - Month 8, 10 m")

tensile.plot.8.10 <- (energy.break.plot.8.10 + modulus.plot.8.10 + break.elong.plot.8.10) +
plot_annotation(title = "Tensile Testing - Month 8 Depth 10")

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(tensile.plot.8.10)

#ggsave(here("output", "Tensile Time 8 Controls, 10m.png"))
#ggsave(here("output", "Final Figures", "Tensile Time 8, 10m.png"))

#print(energy.break.plot.8.10)
#ggsave(here("output", "Final Figures", "Energy at break Time 8, 10m.png"))

##########################################################################################
#combine Elements depths and times

#######Break Elongation###################################################################3
break.elong.plot.0.1 <- ggplot(data = tensile_0, aes(x = Sample.ID, y = Break.Elongation, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Time 0") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Break Elongation (%)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

break.elong.plot.4.2 <- ggplot(data = tensile_4, aes(x = Sample.ID, y = Break.Elongation, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Time 4 Depth 0") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Break Elongation (%)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

break.elong.plot.4.10.3 <- ggplot(data = tensile_4.10, aes(x = Sample.ID, y = Break.Elongation, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Time 4 Depth 10") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Break Elongation (%)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10) ) 

break.elong.plot.8.4 <- ggplot(data = tensile_8, aes(x = Sample.ID, y = Break.Elongation, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Time 8 Depth 0") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Break Elongation (%)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

break.elong.plot.8.10.5 <- ggplot(data = tensile_8.10, aes(x = Sample.ID, y = Break.Elongation, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Time 8 Depth 10") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Break Elongation (%)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

tensile.plot.breakelongation <- (break.elong.plot.0.1 + break.elong.plot.4.2 + break.elong.plot.4.10.3 + break.elong.plot.8.4 + break.elong.plot.8.10.5) +
  plot_annotation(title = "Break Elongation")

graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(tensile.plot.breakelongation)

#ggsave(here("output", "Tensile Controls Break Elongation.png"))

####################Young's modulus###############################################

modulus.plot.0.1 <- ggplot(data = tensile_0, aes(x = Sample.ID, y = Modulus, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Time 0") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Modulus of Elasticity (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

modulus.plot.4.2 <- ggplot(data = tensile_4, aes(x = Sample.ID, y = Modulus, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Time 4 Depth 0") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Modulus of Elasticity (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

modulus.plot.4.10.3 <- ggplot(data = tensile_4.10, aes(x = Sample.ID, y = Modulus, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Time 4 Depth 10") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Modulus of Elasticity (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

modulus.plot.8.4 <- ggplot(data = tensile_8, aes(x = Sample.ID, y = Modulus, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Time 8 Depth 0") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Modulus of Elasticity (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

modulus.plot.8.10.5 <- ggplot(data = tensile_8.10, aes(x = Sample.ID, y = Modulus, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Time 8 Depth 10") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Modulus of Elasticity (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

tensile.plot.modulus <- (modulus.plot.0.1 + modulus.plot.4.2 + modulus.plot.4.10.3 + modulus.plot.8.4 + modulus.plot.8.10.5) +
  plot_annotation(title = "Young's Modulus of Elasticity")

graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(tensile.plot.modulus)

#ggsave(here("output", "Tensile Controls modulus.png"))

####################energy at break###############################################

energy.break.plot.0.1 <- ggplot(data = tensile_0, aes(x = Sample.ID, y = Energy.Break, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Time 0") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Energy at Break (J)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

energy.break.plot.4.2 <- ggplot(data = tensile_4, aes(x = Sample.ID, y = Energy.Break, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Time 4 Depth 0") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Energy at Break (J)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

energy.break.plot.4.10.3 <- ggplot(data = tensile_4.10, aes(x = Sample.ID, y = Energy.Break, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Time 4 Depth 10") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Energy at Break (J)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

energy.break.plot.8.4 <- ggplot(data = tensile_8, aes(x = Sample.ID, y = Energy.Break, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Time 8 Depth 0") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Energy at Break (J)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

energy.break.plot.8.10.5 <- ggplot(data = tensile_8.10, aes(x = Sample.ID, y = Energy.Break, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Time 8 Depth 10") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Energy at Break (J)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

tensile.plot.energy.break <- (energy.break.plot.0.1 + energy.break.plot.4.2 + energy.break.plot.4.10.3 + energy.break.plot.8.4 + energy.break.plot.8.10.5) +
  plot_annotation(title = "Energy at Break")

graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(tensile.plot.energy.break)

#ggsave(here("output", "Tensile Controls Energy at Break.png"))

#####################Yield stress#############################################

yield.stress.plot.0.1 <- ggplot(data = tensile_0, aes(x = Sample.ID, y = Yield.Stress, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Time 0") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Yield Stress (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

yield.stress.plot.4.2 <- ggplot(data = tensile_4, aes(x = Sample.ID, y = Yield.Stress, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Time 4 Depth 0") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Yield Stress (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

yield.stress.plot.4.10.3 <- ggplot(data = tensile_4.10, aes(x = Sample.ID, y = Yield.Stress, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Time 4 Depth 10") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Yield Stress (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

yield.stress.plot.8.4 <- ggplot(data = tensile_8, aes(x = Sample.ID, y = Yield.Stress, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Time 8 Depth 0") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Yield Stress (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

yield.stress.plot.8.10.5 <- ggplot(data = tensile_8.10, aes(x = Sample.ID, y = Yield.Stress, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Time 8 Depth 10") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Yield Stress (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

tensile.plot.yield.stress <- (yield.stress.plot.0.1 + yield.stress.plot.4.2 + yield.stress.plot.4.10.3 + yield.stress.plot.8.4 + yield.stress.plot.8.10.5) +
  plot_annotation(title = "Yield stress")

graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(tensile.plot.yield.stress)

#ggsave(here("output", "Tensile Controls Yield Stress.png"))

####################yield elongation#################################

yield.elong.plot.0.1 <- ggplot(data = tensile_0, aes(x = Sample.ID, y = Yield.Elongation, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Time 0") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Yield Elongation (%)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

yield.elong.plot.4.2 <- ggplot(data = tensile_4, aes(x = Sample.ID, y = Yield.Elongation, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Time 4 Depth 0") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Yield Elongation (%)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 


yield.stress.plot.4.10.3 <- ggplot(data = tensile_4.10, aes(x = Sample.ID, y = Yield.Stress, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Time 4 Depth 10") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Yield Stress (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

yield.elong.plot.8.4 <- ggplot(data = tensile_8, aes(x = Sample.ID, y = Yield.Elongation, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Time 8 Depth 0") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Yield Elongation (%)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

yield.elong.plot.8.10.5 <- ggplot(data = tensile_8.10, aes(x = Sample.ID, y = Yield.Elongation, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Time 8 Depth 10") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Yield Elongation (%)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 


tensile.plot.yield.elong <- (yield.elong.plot.0.1 + yield.elong.plot.4.2 + yield.elong.plot.4.10.3 + yield.elong.plot.8.4 + yield.elong.plot.8.10.5) +
  plot_annotation(title = "Yield Elongation")

graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(tensile.plot.yield.elong)

#ggsave(here("output", "Tensile Controls Yield elongation.png"))

##############################################################################
#ELISA Analysis CONTROLS done
##############################################################################

#change column names
names(elisa) [2] <- "Sample.ID"
names(elisa) [3] <- "Time"

#remove other controls
elisa.dat <- elisa %>%
  filter(Sample.ID %in% c("A", "B", "C", "D", "P", "N"))

#change NA for time = 0 so that can plot that
#elisa.dat[c(1:10),1] <- 0.1

#just plot of time 0
elisa0 <- elisa.dat %>%
  filter(Time == 0)

#plot BPA values
elisa.plot0 <- ggplot(elisa0, aes(x = Sample.ID, y = as.numeric(BPA), fill = Sample.ID)) +
  geom_bar(stat = "identity") +
  labs(title = "Month 0",
       y = "BPA (pg/ml)",
        color = "Sample ID") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  # scale_x_continuous(expand = c(0,0), breaks=c(0, 10, 20, 30)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,150)) +
  scale_fill_manual(values = met.brewer("Archambault", 6)[c(1, 2, 4, 5, 6)])

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(elisa.plot0)

#ggsave(here("output", "ELISA plot Controls - Time 0.png"))


#select data from time 4 
elisa4 <- elisa.dat %>%
  filter(Time == 4)

#plot with values below x axis
elisa.plot4 <- ggplot(elisa4, aes(x = Sample.ID, y = as.numeric(BPA), fill = Sample.ID)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Depth, labeller = labeller(Depth = c("0" = "0 m", "10" = "10 m"))) +
  labs(title = "Month 4",
       x = "Sample ID", color = "Sample ID") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  # scale_x_continuous(expand = c(0,0), breaks=c(0, 10, 20, 30)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,150)) +
  scale_fill_manual(values = met.brewer("Archambault", 6))  

print(elisa.plot4)

#ggsave(here("output", "ELISA plot Control - Time 4.png"))


#select data from time 8
elisa8 <- elisa.dat %>%
  filter(Time == 8)

#plot with values below x axis
elisa.plot8 <- ggplot(elisa8, aes(x = Sample.ID, y = as.numeric(BPA), fill = Sample.ID)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Depth, labeller = labeller(Depth = c("0" = "0 m", "10" = "10 m"))) +
  labs(title = "Month 8",
       color = "Sample ID") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(),
        plot.title = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  # scale_x_continuous(expand = c(0,0), breaks=c(0, 10, 20, 30)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,150)) +
  scale_fill_manual(values = met.brewer("Archambault", 6))  

print(elisa.plot8)

#ggsave(here("output", "ELISA plot Control - Time 8.png"))

######################################################################

Elisa.plot.Combo <- (elisa.plot0 + elisa.plot4 + elisa.plot8) +
plot_annotation(title = "Estrogenicity (BPA)")

windows(8,6, record=T)

print(Elisa.plot.Combo)

#ggsave(here("output", "Elisa Plot Controls Combo.png"))


#just water/solution plot

elisa3 <- elisa %>%
  filter(Sample.ID %in% c("DIW", "DIW and Pepsin", "Fresh DIW", "Tap",
                          "PS-in", "PS-out", "Solution"))

elisa.plot.solutions <- ggplot(elisa3, aes(x = Sample.ID, y = as.numeric(BPA), fill = Sample.ID)) +
  geom_bar(stat = "identity") +
  labs(title = "Estrogenicity - Solutions",
       y = "BPA (pg/ml))",
       x = "Sample ID", color = "Sample ID") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  # scale_x_continuous(expand = c(0,0), breaks=c(0, 10, 20, 30)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 140)) 

print(elisa.plot.solutions)

#ggsave(here("output", "ELISA plot - Solutions.png"))


##############################################################################
#Mass Change CONTROLS done
##############################################################################

#time 0, depth 0

#change column names
names(mass) [1] <- "Depth"
names(mass) [2] <- "Sample.ID"
names(mass) [3] <- "Time"
names(mass) [4] <- "Pre.Weight"
names(mass) [5] <- "Post.Weight"
names(mass) [8] <- "For.Plot"
names(mass)  <- "Perc.Loss"

#remove PIP Samples
avg.mass <- mass %>%
  drop_na(Perc.Loss) %>%
  filter(Sample.ID %in% c("A","B", "C", "D", "P", "N"))

#remove rows with bags gone
avg.mass <- avg.mass[-c(123,126),]

#change average values to negative
avg.mass$negative = avg.mass$Perc.Loss*(-1)

#change NA for time = 0 so that can plot that
#avg.mass[c(1:10),1] <- 0.1

#select data from time 0 
avg.mass0 <- avg.mass %>%
  filter(Time == 0)

#plot with values below x axis
mass.plot0 <- ggplot(avg.mass0, aes(x = Sample.ID, y = negative, fill = Sample.ID)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0) +
  labs(title = "Mass Loss - Month 0",
       y = "Percent Change (%)",
       x = "Sample ID", color = "Sample ID") +
  theme_bw() +
  theme(legend.position = "none",
        # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  # scale_x_continuous(expand = c(0,0), breaks=c(0, 10, 20, 30)) +
  scale_y_continuous(expand = c(0,0), limits = c(-110, 110)) +
  scale_fill_manual(values = met.brewer("Archambault", 10)())  

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(mass.plot0)

#ggsave(here("output", "Mass plot Controls - Month 0.png"))

#select data from time 4
avg.mass4 <- avg.mass %>%
  filter(Time == 4)

#plot with values below x axis
mass.plot4 <- ggplot(avg.mass4, aes(x = Sample.ID, y = negative, fill = Sample.ID)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Depth, labeller = labeller(Depth = c("0" = "0 m", "10" = "10 m"))) +
  labs(title = "Mass Loss - Month 4",
       y = "Percent Change (%)",
       x = "Sample ID", color = "Sample ID") +
  theme_bw() +
  theme(legend.position = "none",
        # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  # scale_x_continuous(expand = c(0,0), breaks=c(0, 10, 20, 30)) +
  scale_y_continuous(expand = c(0,0), limits = c(-125, 125)) +
  scale_fill_manual(values = met.brewer("Archambault", 10))  

print(mass.plot4)

#ggsave(here("output", "Mass plot Controls - Month 4.png"))


#select data from time 8
avg.mass8 <- avg.mass %>%
  filter(Time == 8)

#plot with values below x axis
mass.plot8 <- ggplot(avg.mass8, aes(x = Sample.ID, y = negative, fill = Sample.ID)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Depth, labeller = labeller(Depth = c("0" = "0 m", "10" = "10 m"))) +
  labs(title = "Mass Loss - Month 8",
       y = "Percent Change (%)",
       x = "Sample ID", color = "Sample ID") +
  theme_bw() +
  theme(legend.position = "none",
        # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  # scale_x_continuous(expand = c(0,0), breaks=c(0, 10, 20, 30)) +
  scale_y_continuous(expand = c(0,0), limits = c(-125, 125)) +
  scale_fill_manual(values = met.brewer("Archambault", 10)) 

print(mass.plot8)

#ggsave(here("output", "Mass plot Controls - Month 8.png"))

##############################################################################
#Electrical resistance
##############################################################################

#change column names
names(ElectroC) [1] <- "Depth"
names(ElectroC) [2] <- "Sample.ID"
names(ElectroC) [3] <- "Time.Retrieved"
names(ElectroC) [4] <- "Frequency"
names(ElectroC) [5] <- "Pre"
names(ElectroC) [6] <- "Post"
names(ElectroC) [7] <- "Change"
names(ElectroC) [8] <- "Perc.Change"

#select data
ElectroC <- ElectroC %>%
  mutate(Sample.ID = as.factor(Sample.ID)) %>%
  mutate(Frequency = as.factor(Frequency)) %>%
  mutate(Perc.Change = as.numeric(Perc.Change)) %>%
  drop_na(Perc.Change)

###take out 100 and 120 because all 0 this month
ElectroC <- ElectroC %>%
  filter(Frequency == "1k" | Frequency == "10k" | Frequency == "100k")

ElectroC$Frequency <- factor(ElectroC$Frequency,levels = c("1k", "10k", "100k"))

ElectroC.plot0 <- ggplot(data= ElectroC[ElectroC$Time.Retrieved==0,], aes(x = Sample.ID, y = Perc.Change, fill = Sample.ID)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept=0) +
  facet_wrap(~ Frequency) +
  labs(title = "Percent Change in Electrical Resistance (Ohms)",
       subtitle = "Month 0",
       y = "Percent Change (%)",
       x = "Sample ID", color = "Sample ID") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  scale_y_continuous(expand = c(0,0), limits=c(-120, 120), breaks = c(-100, -50, 0, 50, 100)) +
  scale_fill_manual(values = met.brewer("Archambault", 10))  

#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(ElectroC.plot0)

#ggsave(here("output", "Controls Resistance Change time 0.png"))

############## Month 4 


###filter for depth 0
ElectroC0 <- ElectroC %>%
  filter(Depth == 0.1 | Depth == 0)

#depth 0 plot
ElectroC.plot40 <- ggplot(data= ElectroC0[ElectroC0$Time.Retrieved==4,], aes(x = Sample.ID, y = Perc.Change, fill = Sample.ID)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept=0) +
  facet_wrap(~ Frequency) +
  labs(title = "Percent Change in Electrical Resistance (Ohms)",
       subtitle = "Month 4 Depth 0m",
       y = "Percent Change (%)",
       x = "Sample ID", color = "Sample ID") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  scale_y_continuous(expand = c(0,0), limits=c(-120, 120), breaks = c(-100, -50, 0, 50, 100)) +
  scale_fill_manual(values = met.brewer("Archambault", 10))  


#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(ElectroC.plot40)

#ggsave(here("output", "Control Resistance Change t4 d0.png"))

###################
#filter fot depth 10
ElectroC10 <- ElectroC %>%
  filter(Depth == 0.1 | Depth == 10)


#depth 10 plot
ElectroC.plot410 <- ggplot(data= ElectroC10[ElectroC10$Time.Retrieved==4,], aes(x = Sample.ID, y = Perc.Change, fill = Sample.ID)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept=0) +
  facet_wrap(~ Frequency) +
  labs(title = "Percent Change in Electrical Resistance (Ohms)",
       subtitle = "Month 4 Depth 10m",
       y = "Percent Change (%)",
       x = "Sample ID", color = "Sample ID") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  scale_y_continuous(expand = c(0,0), limits=c(-120, 120), breaks = c(-100, -50, 0, 50, 100)) +
  scale_fill_manual(values = met.brewer("Archambault", 10))  


#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(ElectroC.plot410)

#ggsave(here("output", "Control Resistance Change t4 d10.png"))

################# Month 8


#depth 0
ElectroC.plot80 <- ggplot(data= ElectroC0[ElectroC0$Time.Retrieved==8,], aes(x = Sample.ID, y = Perc.Change, fill = Sample.ID)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept=0) +
  facet_wrap(~ Frequency) +
  labs(title = "Percent Change in Electrical Resistance (Ohms)",
       subtitle = "Month 8 Depth 0m",
       y = "Percent Change (%)",
       x = "Sample ID", color = "Sample ID") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  scale_y_continuous(expand = c(0,0), limits=c(-120, 120), breaks = c(-100, -50, 0, 50, 100)) +
  scale_fill_manual(values = met.brewer("Archambault", 10))  


#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(ElectroC.plot80)

#ggsave(here("output", "Control Resistance Change t8 d00.png"))

###################################

#depth 10
ElectroC.plot810 <- ggplot(data= ElectroC10[ElectroC10$Time.Retrieved==8,], aes(x = Sample.ID, y = Perc.Change, fill = Sample.ID)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept=0) +
  facet_wrap(~ Frequency) +
  labs(title = "Percent Change in Electrical Resistance (Ohms)",
       subtitle = "Month 8 Depth 10m",
       y = "Percent Change (%)",
       x = "Sample ID", color = "Sample ID") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  scale_y_continuous(expand = c(0,0), limits=c(-120, 120), breaks = c(-100, -50, 0, 50, 100)) +
  scale_fill_manual(values = met.brewer("Archambault", 10))  

#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(ElectroC.plot810)

#ggsave(here("output", "Control Resistance Change t8 d10.png"))



###############################################################################
#only 1k
############################################################################333



ElectroC1k <- ElectroC %>%
  filter(Frequency == "1k")

ElectroC1k0 <- ElectroC1k %>%
  filter(Depth == 0.1 | Depth == 0)

ElectroC1k10 <- ElectroC1k %>%
  filter(Depth == 0.1 | Depth == 10)


ElectroC.plot1k0 <- ggplot(data= ElectroC1k[ElectroC1k$Time.Retrieved==0,], aes(x = Sample.ID, y = Perc.Change, fill = Sample.ID)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept=0) +
  labs(title = "Percent Change in Electrical Resistance (Ohms)",
       subtitle = "Month 0, Frequency = 1,000 Hz",
       y = "Percent Change (%)",
       x = "Sample ID", color = "Sample ID") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  scale_y_continuous(expand = c(0,0), limits=c(-100, 100), breaks = c(-100, -50, 0, 50, 100)) +
  scale_fill_manual(values = met.brewer("Archambault", 10))  

#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(ElectroC.plot1k0)

#ggsave(here("output", "Control 1k only Resistance Change t0.png"))
#ggsave(here("output", "Final Figures", "1k only Resistance Change t0 Controls.png"))

##time 4

ElectroC.plot1k40 <- ggplot(data= ElectroC1k0[ElectroC1k0$Time.Retrieved==4,], aes(x = Sample.ID, y = Perc.Change, fill = Sample.ID)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept=0) +
  labs(title = "Percent Change in Electrical Resistance (Ohms)",
       subtitle = "Month 4 Depth 0, Frequency = 1,000 Hz",
       y = "Percent Change (%)",
       x = "Sample ID", color = "Sample ID") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  scale_y_continuous(expand = c(0,0), limits=c(-100, 100), breaks = c(-100, -50, 0, 50, 100)) +
  scale_fill_manual(values = met.brewer("Archambault", 10))  

#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(ElectroC.plot1k40)

#ggsave(here("output", "Controls 1k only Resistance Change t4 d0.png"))
#ggsave(here("output", "Final Figures", "1k only Resistance Change t4 d0 Controls.png"))

#### 

ElectroC.plot1k410 <- ggplot(data= ElectroC1k10[ElectroC1k10$Time.Retrieved==4,], aes(x = Sample.ID, y = Perc.Change, fill = Sample.ID)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept=0) +
  labs(title = "Percent Change in Electrical Resistance (Ohms)",
       subtitle = "Month 4 Depth 10, Frequency = 1,000 Hz",
       y = "Percent Change (%)",
       x = "Sample ID", color = "Sample ID") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  scale_y_continuous(expand = c(0,0), limits=c(-100, 100), breaks = c(-100, -50, 0, 50, 100)) +
  scale_fill_manual(values = met.brewer("Archambault", 10))  

#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(ElectroC.plot1k410)

#ggsave(here("output", "Controls 1k only Resistance Change t4 d10.png"))
#ggsave(here("output", "Final Figures", "1k only Resistance Change t4 d10.png"))

####time 8

ElectroC.plot1k80 <- ggplot(data= ElectroC1k0[ElectroC1k0$Time.Retrieved==8,], aes(x = Sample.ID, y = Perc.Change, fill = Sample.ID)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept=0) +
  labs(title = "Percent Change in Electrical Resistance (Ohms)",
       subtitle = "Month 8 Depth 0, Frequency = 1,000 Hz",
       y = "Percent Change (%)",
       x = "Sample ID", color = "Sample ID") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  scale_y_continuous(expand = c(0,0), limits=c(-100, 100), breaks = c(-100, -50, 0, 50, 100)) +
  scale_fill_manual(values = met.brewer("Archambault", 10))  

#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(ElectroC.plot1k80)

#ggsave(here("output", "Controls 1k only Resistance Change t8 d0.png"))
#ggsave(here("output", "Final Figures", "1k only Resistance Change t8 d0.png"))

#####

ElectroC.plot1k810 <- ggplot(data= ElectroC1k10[ElectroC1k10$Time.Retrieved==8,], aes(x = Sample.ID, y = Perc.Change, fill = Sample.ID)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept=0) +
  labs(title = "Percent Change in Electrical Resistance (Ohms)",
       subtitle = "Month 8 Depth 10, Frequency = 1,000 Hz",
       y = "Percent Change (%)",
       x = "Sample ID", color = "Sample ID") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  scale_y_continuous(expand = c(0,0), limits=c(-100, 100), breaks = c(-100, -50, 0, 50, 100)) +
  scale_fill_manual(values = met.brewer("Archambault", 10))  

#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(ElectroC.plot1k810)

#ggsave(here("output", "Controls 1k only Resistance Change t8 d10.png"))
#ggsave(here("output", "Final Figures", "1k only Resistance Change t8 d10.png"))

#####################stats##################################################

##############tensile########################
modulus.test <- aov(Modulus ~ Pre.Post, data = TensileC)
summary(modulus.test)

modulus0.test <- aov(Modulus ~ Pre.Post, data = tensile_0)
summary(modulus0.test)

modulus4.test <- aov(Modulus ~ Pre.Post, data = tensile_4)
summary(modulus4.test)

modulus410.test <- aov(Modulus ~ Pre.Post, data = tensile_4.10)
summary(modulus410.test)

modulus8.test <- aov(Modulus ~ Pre.Post, data = tensile_8)
summary(modulus8.test)

modulus810.test <- aov(Modulus ~ Pre.Post, data = tensile_8.10)
summary(modulus810.test)

##

Yield.Elongation.test <- aov(Yield.Elongation ~ Pre.Post, data = TensileC)
summary(Yield.Elongation.test)

Yield.Elongation0.test <- aov(Yield.Elongation ~ Pre.Post, data = tensile_0)
summary(Yield.Elongation0.test)

Yield.Elongation4.test <- aov(Yield.Elongation ~ Pre.Post, data = tensile_4)
summary(Yield.Elongation4.test)

Yield.Elongation410.test <- aov(Yield.Elongation ~ Pre.Post, data = tensile_4.10)
summary(Yield.Elongation410.test)

Yield.Elongation8.test <- aov(Yield.Elongation ~ Pre.Post, data = tensile_8)
summary(Yield.Elongation8.test)

Yield.Elongation810.test <- aov(Yield.Elongation ~ Pre.Post, data = tensile_8.10)
summary(Yield.Elongation810.test)

##


Break.Elongation.test <- aov(Break.Elongation ~ Pre.Post, data = TensileC)
summary(Break.Elongation.test)

Break.Elongation0.test <- aov(Break.Elongation ~ Pre.Post, data = tensile_0)
summary(Break.Elongation0.test)

Break.Elongation4.test <- aov(Break.Elongation ~ Pre.Post, data = tensile_4)
summary(Break.Elongation4.test)

Break.Elongation410.test <- aov(Break.Elongation ~ Pre.Post, data = tensile_4.10)
summary(Break.Elongation410.test)

Break.Elongation8.test <- aov(Break.Elongation ~ Pre.Post, data = tensile_8)
summary(Break.Elongation8.test)

Break.Elongation810.test <- aov(Break.Elongation ~ Pre.Post, data = tensile_8.10)
summary(Break.Elongation810.test)

####

Yield.Stress.test <- aov(Yield.Stress ~ Pre.Post, data = TensileC)
summary(Yield.Stress.test)

Yield.Stress0.test <- aov(Yield.Stress ~ Pre.Post, data = tensile_0)
summary(Yield.Stress0.test)

Yield.Stress4.test <- aov(Yield.Stress ~ Pre.Post, data = tensile_4)
summary(Yield.Stress4.test)

Yield.Stress410.test <- aov(Yield.Stress ~ Pre.Post, data = tensile_4.10)
summary(Yield.Stress410.test)

Yield.Stress8.test <- aov(Yield.Stress ~ Pre.Post, data = tensile_8)
summary(Yield.Stress8.test)

Yield.Stress810.test <- aov(Yield.Stress ~ Pre.Post, data = tensile_8.10)
summary(Yield.Stress810.test)

####

Energy.Break.test <- aov(Energy.Break ~ Pre.Post, data = TensileC)
summary(Energy.Break.test)

Energy.Break0.test <- aov(Energy.Break ~ Pre.Post, data = tensile_0)
summary(Energy.Break0.test)

Energy.Break4.test <- aov(Energy.Break ~ Pre.Post, data = tensile_4)
summary(Energy.Break4.test)

Energy.Break410.test <- aov(Energy.Break ~ Pre.Post, data = tensile_4.10)
summary(Energy.Break410.test)

Energy.Break8.test <- aov(Energy.Break ~ Pre.Post, data = tensile_8)
summary(Energy.Break8.test)

Energy.Break810.test <- aov(Energy.Break ~ Pre.Post, data = tensile_8.10)
summary(Energy.Break810.test)

#######################################mass#####################

mass.stats <- read_csv(here("data", "mass_stats.csv"))

names(mass.stats) [1] <- "Depth"
names(mass.stats) [2] <- "Sample.ID"
names(mass.stats) [3] <- "Time"
names(mass.stats) [4] <- "Weight"
names(mass.stats) [5] <- "Pre.Post"

mass_stats <- aov(Weight ~ Pre.Post, data = mass.stats)
summary(mass_stats)

mass.stats0 <- mass.stats %>%
  filter(Time == 0)

mass.stats4 <- mass.stats %>%
  filter(Time == 4)

mass.stats8 <- mass.stats %>%
  filter(Time == 8)

mass.stats40 <- mass.stats4 %>%
  filter(Depth == 0)

mass.stats410 <- mass.stats4 %>%
  filter(Depth == 10)

mass.stats80 <- mass.stats8 %>%
  filter(Depth == 0)

mass.stats810 <- mass.stats8 %>%
  filter(Depth == 10)


mass_stats0 <- aov(Weight ~ Pre.Post, data = mass.stats0)
summary(mass_stats0)

mass_stats40 <- aov(Weight ~ Pre.Post, data = mass.stats40)
summary(mass_stats40)

mass_stats410 <- aov(Weight ~ Pre.Post, data = mass.stats410)
summary(mass_stats410)

mass_stats80 <- aov(Weight ~ Pre.Post, data = mass.stats80)
summary(mass_stats80)

mass_stats810 <- aov(Weight ~ Pre.Post, data = mass.stats810)
summary(mass_stats810)

##################electrical resistance##########################


electro.stats <- read_csv(here("data", "Electro_Stats.csv"))

names(electro.stats) [1] <- "Depth"
names(electro.stats) [2] <- "Sample.ID"
names(electro.stats) [3] <- "Time"
names(electro.stats) [4] <- "Frequency"
names(electro.stats) [5] <- "Resistance"
names(electro.stats) [6] <- "Pre.Post"

electro.stats <- electro.stats %>%
  mutate(Sample.ID = as.factor(Sample.ID)) %>%
  mutate(Frequency = as.factor(Frequency)) %>%

electro.stats1k <- electro.stats %>%
  filter(Frequency == "1k")

electro.stats01k <- electro.stats1k %>%
  filter(Time == 0)

electro.stats41k <- electro.stats1k %>%
  filter(Time == 4)

electro.stats81k <- electro.stats1k %>%
  filter(Time == 8)

electro.stats401k <- electro.stats41k %>%
   filter(Depth == 0)

electro.stats4101k <- electro.stats41k %>%
  filter(Depth == 10)

electro.stats801k <- electro.stats81k %>%
  filter(Depth == 0)

electro.stats8101k <- electro.stats81k %>%
  filter(Depth == 10)

electro_stats1k <- aov(Resistance ~ Pre.Post, data=electro.stats1k)
summary(electro_stats1k)


electro_stats01k <- aov(Resistance ~ Pre.Post, data=electro.stats01k)
summary(electro_stats01k)

electro_stats401k <- aov(Resistance ~ Pre.Post, data=electro.stats401k)
summary(electro_stats401k)

electro_stats4101k <- aov(Resistance ~ Pre.Post, data=electro.stats4101k)
summary(electro_stats4101k)

electro_stats801k <- aov(Resistance ~ Pre.Post, data=electro.stats801k)
summary(electro_stats801k)

electro_stats8101k <- aov(Resistance ~ Pre.Post, data=electro.stats8101k)
summary(electro_stats8101k)

#####################################

##messing around with stats

tensile_C <- TensileC %>%
  filter(Time %in% c("4", "8"))

tensile_8.10$Pre.Post <- factor(tensile_8.10$Pre.Post, levels = c("Pre", "Post"))

tensile_8A.10 <- tensile_8.10 %>%
  filter(Sample.ID == "A")



test1 <- aov(Yield.Elongation ~ Pre.Post, data = tensile_C)
summary(test1)


tensile_D <- tensile_C %>%
  filter(Sample.ID == "D")

test <- aov(Energy.Break ~ Pre.Post, data = tensile_N)
summary(test)




