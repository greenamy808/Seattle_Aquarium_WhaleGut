## This script performs statistics on the TOM FORD Plastic Innovation Prize (2022)


## set up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#load libraries
library(here)
library(patchwork)
library(tidyverse)
library(lubridate) #date changes
library(MetBrewer) #color palette
library(ggbreak) #break y scale
library(corrr) #PCA
library(factoextra) #PCA
library(FactoMineR) #PCA
library(vegan) #NMDS
library(pwr) #power analysis


#check to see that here() leads to root directory or top-level folder
#should be Seattle_Aquarium_CleanSeas_PIP
here()

#if incorrect, enter `set_here(path = "...")`
#restart RStudio
#check here() again to make sure you're in the right top-level folder

#import data
tensile <- read_csv(here("data", "Tensile.csv"))
elisa <- read_csv(here("data", "PIP_ELISA.csv"))
env_data <- read_csv(here("data", "KingCounty_EnvData.csv"))
mass <- read_csv(here("data", "Mass.csv"))
temp.0 <- read_csv(here("data", "hobo_temp", "Shallow.csv"))
temp.10 <- read_csv(here("data", "hobo_temp", "Deep.csv"))
degrad_photo <- read.csv(here("data", "5x5_deg.csv"))
degrad_photo9 <- read.csv(here("data", "9x9_deg.csv"))
Electro <- read_csv(here("data", "Elec_Res.csv"))

#all data in one file
data <- read_csv(here("data", "All PIP Data.csv"))


##############################################################################
#Tensile Strength Analysis
##############################################################################

#evans plots: elastic modulus, yield strain, yield stress, break strain, tensile strength and breaking energy

#change column names
names(tensile) [2] <- "Pre.Post"
names(tensile) [4] <- "Sample.ID"
names(tensile) [11] <- "Maximum.Load"
names(tensile) [12] <- "Modulus"
names(tensile) [13] <- "Yield.Stress"
names(tensile) [14] <- "Yield.Elongation"
names(tensile) [15] <- "Tensile.Stress.Break"
names(tensile) [16] <- "Break.Elongation"
names(tensile) [17] <- "Tensile.Stress.Yield"
names(tensile) [20] <- "Energy.Break"

#change data to numeric
tensile$Maximum.Load <- as.numeric(tensile$Maximum.Load)
tensile$Yield.Elongation <- as.numeric(tensile$Yield.Elongation)
tensile$Yield.Stress <- as.numeric(tensile$Yield.Stress)
tensile$Modulus <- as.numeric(tensile$Modulus)
tensile$Break.Elongation <- as.numeric(tensile$Break.Elongation)
tensile$Tensile.Stress.Break <- as.numeric(tensile$Tensile.Stress.Break)
tensile$Tensile.Stress.Yield <- as.numeric(tensile$Tensile.Stress.Yield)
tensile$Energy.Break <- as.numeric(tensile$Energy.Break)

#time 0
tensile.0 <- tensile %>%
  filter(Time == 0)

#order so that pre is first, post is second
tensile.0$Pre.Post <- factor(tensile.0$Pre.Post, levels = c("Pre", "Post"))

#pulling out only mid break data (didn't use data where material broke at grip)
tensile_0 <- tensile.0 %>%
  filter(break.location.plot == "mid")

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
  ggtitle("Modulus") + 
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

stress.yield.plot.0 <- ggplot(data = tensile_0, aes(x = Sample.ID, y = Tensile.Stress.Yield, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
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
tensile.plot.0 <- (load.plot.0 + modulus.plot.0) /(yield.stress.plot.0 + yield.elong.plot.0) /(break.elong.plot.0 + stress.yield.plot.0) /
  (stress.break.plot.0 + energy.break.plot.0) + 
  plot_annotation(title = "Tensile Testing - Month 0")

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(tensile.plot.0)

ggsave(here("output", "Tensile Time 0.png"))
ggsave(here("output", "Final Figures", "Tensile Time 0.png"))

####
#break energy for time 0 only
###

#plot
energy.break.plot <- ggplot(data = tensile_0, aes(x = Sample.ID, y = Energy.Break, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  scale_y_break(c(1,5), scales = 0.8) +
  theme_bw() +
  ggtitle("Energy at Break") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(1.1)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Energy at Break (J)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10))

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(energy.break.plot)

ggsave(here("output", "Energy break plot.png"))
ggsave(here("output", "Final Figures", "Energy break plot.png"))


#time 4, 0 m
tensile.4 <- tensile %>%
  filter(Time == 4) %>%
  filter(Depth == 0)

tensile.4$Pre.Post <- factor(tensile.4$Pre.Post, levels = c("Pre", "Post"))

#pulling out only mid break data
tensile_4 <- tensile.4 %>%
  filter(break.location.plot == "mid")

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
  scale_fill_manual(values = met.brewer("Archambault", 10) [c(2,9)])  


modulus.plot.4 <- ggplot(data = tensile_4, aes(x = Sample.ID, y = Modulus, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Modulus") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Modulus of Elasticity (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)[c(2,9)])  

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
  scale_fill_manual(values = met.brewer("Archambault", 10)[c(2,9)]) 

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
  scale_fill_manual(values = met.brewer("Archambault", 10) [c(2,9)]) 

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
  scale_fill_manual(values = met.brewer("Archambault", 10) [c(2,9)]) 

stress.yield.plot.4 <- ggplot(data = tensile_4, aes(x = Sample.ID, y = Tensile.Stress.Yield, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post, scales = "free_x") +
  theme_bw() +
  ggtitle("Tensile Stress at Yield") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Tensile Stress at Yield \n(Slope Threshold 0%; MPa"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10) [c(2,9)]) 

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
  scale_fill_manual(values = met.brewer("Archambault", 10) [c(2,9)]) 

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
  scale_fill_manual(values = met.brewer("Archambault", 10) [c(2,9)]) 

#put plots together
tensile.plot.4 <- (load.plot.4 + modulus.plot.4) /(yield.stress.plot.4 + yield.elong.plot.4) /(break.elong.plot.4 + stress.yield.plot.4) /
  (stress.break.plot.4 + energy.break.plot.4)+ plot_annotation (title = "Tensile Testing - Month 4, 0 m")

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(tensile.plot.4)

ggsave(here("output", "Tensile Time 4, 0m.png"))
ggsave(here("output", "Final Figures", "Tensile Time 4, 0m.png"))

print(energy.break.plot.4)
ggsave(here("output", "Final Figures", "Energy at break Time 4, 0m.png"))


#time 4, 10 m
tensile.4.10 <- tensile %>%
  filter(Time == 4) %>%
  filter(Depth == 10)

tensile.4.10$Pre.Post <- factor(tensile.4.10$Pre.Post, levels = c("Pre", "Post"))

#pulling out only mid break data
tensile_4.10 <- tensile.4.10 %>%
  filter(break.location.plot == "mid")

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
  scale_fill_manual(values = met.brewer("Archambault", 10) [c(2,9)])  


modulus.plot.4.10 <- ggplot(data = tensile_4.10, aes(x = Sample.ID, y = Modulus, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Modulus") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Modulus of Elasticity (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10) [c(2,9)])  

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
  scale_fill_manual(values = met.brewer("Archambault", 10) [c(2,9)]) 

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
  scale_fill_manual(values = met.brewer("Archambault", 10) [c(2,9)]) 

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
  scale_fill_manual(values = met.brewer("Archambault", 10) [c(2,9)]) 

stress.yield.plot.4.10 <- ggplot(data = tensile_4.10, aes(x = Sample.ID, y = Tensile.Stress.Yield, group = Sample.ID)) + 
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
  scale_fill_manual(values = met.brewer("Archambault", 10) [c(2,9)]) 

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
  scale_fill_manual(values = met.brewer("Archambault", 10) [c(2,9)]) 

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
  scale_fill_manual(values = met.brewer("Archambault", 10) [c(2,9)]) 

#put all plots together
tensile.plot.4.10 <- (load.plot.4.10 + modulus.plot.4.10) /(yield.stress.plot.4.10 + yield.elong.plot.4.10) /
  (break.elong.plot.4.10 + stress.yield.plot.4.10) /
  (stress.break.plot.4.10 + energy.break.plot.4.10)+ plot_annotation (title = "Tensile Testing - Month 4, 10 m")

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(tensile.plot.4.10)

ggsave(here("output", "Tensile Time 4, 10m.png"))
ggsave(here("output", "Final Figures", "Tensile Time 4, 10m.png"))

print(energy.break.plot.4.10)
ggsave(here("output", "Final Figures", "Energy at break Time 4, 10m.png"))

#time 8, depth 0
tensile.8 <- tensile %>%
  filter(Time == 8) %>%
  filter(Depth == 0)

tensile.8$Pre.Post <- factor(tensile.8$Pre.Post, levels = c("Pre", "Post"))

#pulling out only mid break data
tensile_8 <- tensile.8 %>%
  filter(break.location.plot == "mid")

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
  scale_fill_manual(values = met.brewer("Archambault", 10) [9])  


modulus.plot.8 <- ggplot(data = tensile_8, aes(x = Sample.ID, y = Modulus, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Modulus") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Modulus of Elasticity (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)[9])  

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
  scale_fill_manual(values = met.brewer("Archambault", 10)[9]) 

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
  scale_fill_manual(values = met.brewer("Archambault", 10)[9]) 

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
  scale_fill_manual(values = met.brewer("Archambault", 10)[9]) 

stress.yield.plot.8 <- ggplot(data = tensile_8, aes(x = Sample.ID, y = Tensile.Stress.Yield, group = Sample.ID)) + 
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
  scale_fill_manual(values = met.brewer("Archambault", 10)[9]) 

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
  scale_fill_manual(values = met.brewer("Archambault", 10)[9]) 

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
  scale_fill_manual(values = met.brewer("Archambault", 10)[9]) 

#put all plots together
tensile.plot.8 <- (load.plot.8 + modulus.plot.8) /(yield.stress.plot.8 + yield.elong.plot.8) /(break.elong.plot.8 + stress.yield.plot.8) /
  (stress.break.plot.8 + energy.break.plot.8) + plot_annotation(title = "Tensile Testing - Month 8, 0 m")

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(tensile.plot.8)

ggsave(here("output", "Tensile Time 8, 0m.png"))
ggsave(here("output", "Final Figures", "Tensile Time 8, 0m.png"))

print(energy.break.plot.8)
ggsave(here("output", "Final Figures", "Energy at break, Time 8, 0m.png"))

#time 8, depth 10
tensile.8.10 <- tensile %>%
  filter(Time == 8) %>%
  filter(Depth == 10)

tensile.8.10$Pre.Post <- factor(tensile.8.10$Pre.Post, levels = c("Pre", "Post"))

#pulling out only mid break data
tensile_8.10 <- tensile.8.10 %>%
  filter(break.location.plot == "mid")

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
  scale_fill_manual(values = met.brewer("Archambault", 10)[9])  


modulus.plot.8.10 <- ggplot(data = tensile_8.10, aes(x = Sample.ID, y = Modulus, group = Sample.ID)) + 
  geom_boxplot(aes(fill = Sample.ID)) +
  facet_wrap(~Pre.Post) +
  theme_bw() +
  ggtitle("Modulus") + 
  theme(plot.title = element_text(hjust = 0.5, size = rel(1.1)),
        axis.title = element_text(size = rel(0.6)),
        axis.text.x = element_text(size = rel(1.1)),
        axis.text.y = element_text(size = rel(1.1))) +
  xlab("") +
  theme(legend.position = "none") +
  ylab(expression(paste("Modulus of Elasticity (MPa)"))) +
  scale_fill_manual(values = met.brewer("Archambault", 10)[9])  

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
  scale_fill_manual(values = met.brewer("Archambault", 10)[9]) 

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
  scale_fill_manual(values = met.brewer("Archambault", 10)[9]) 

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
  scale_fill_manual(values = met.brewer("Archambault", 10)[9]) 

stress.yield.plot.8.10 <- ggplot(data = tensile_8.10, aes(x = Sample.ID, y = Tensile.Stress.Yield, group = Sample.ID)) + 
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
  scale_fill_manual(values = met.brewer("Archambault", 10)[9]) 

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
  scale_fill_manual(values = met.brewer("Archambault", 10)[9]) 

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
  scale_fill_manual(values = met.brewer("Archambault", 10)[9]) 


tensile.plot.8.10 <- (load.plot.8.10 + modulus.plot.8.10) /(yield.stress.plot.8.10 + yield.elong.plot.8.10) /
  (break.elong.plot.8.10 + stress.yield.plot.8.10) /
  (stress.break.plot.8.10 + energy.break.plot.8.10) + plot_annotation(title = "Tensile Testing - Month 8, 10 m")

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(tensile.plot.8.10)

ggsave(here("output", "Tensile Time 8, 10m.png"))
ggsave(here("output", "Final Figures", "Tensile Time 8, 10m.png"))

print(energy.break.plot.8.10)
ggsave(here("output", "Final Figures", "Energy at break Time 8, 10m.png"))

##############################################################################
#ELISA Analysis
##############################################################################

#change column names
names(elisa) [2] <- "Sample.ID"
names(elisa) [3] <- "Time"

#remove other controls
elisa.dat <- elisa %>%
  filter(Sample.ID %in% c(1:8, "P", "N"))

#change NA for time = 0 so that can plot that
elisa.dat[c(1:10),1] <- 0.1

#just plot of time 0
elisa0 <- elisa.dat %>%
  filter(Depth == 0.1)

#plot BPA values
elisa.plot0 <- ggplot(elisa0, aes(x = Sample.ID, y = as.numeric(BPA), fill = Sample.ID)) +
  geom_bar(stat = "identity") +
  labs(title = "Estrogenicity (BPA) - Month 0",
       y = "BPA (pg/ml)",
       x = "Sample ID", color = "Sample ID") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  # scale_x_continuous(expand = c(0,0), breaks=c(0, 10, 20, 30)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
  scale_fill_manual(values = met.brewer("Archambault", 10)[c(1:4, 6:7, 9:10)])

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(elisa.plot0)

ggsave(here("output", "ELISA plot - Time 0.png"))
ggsave(here("output", "Final Figures", "ELISA plot - Time 0.png"))

#select data from time 4 
elisa4 <- elisa.dat %>%
  filter(Time == 4)

#plot with values below x axis
elisa.plot4 <- ggplot(elisa4, aes(x = Sample.ID, y = as.numeric(BPA), fill = Sample.ID)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Depth, labeller = labeller(Depth = c("0" = "0 m", "10" = "10 m"))) +
  labs(title = "Estrogenicity - Month 4",
       y = "BPA (pg/ml)",
       x = "Sample ID", color = "Sample ID") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  # scale_x_continuous(expand = c(0,0), breaks=c(0, 10, 20, 30)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
  scale_fill_manual(values = met.brewer("Archambault", 10) [c(2,7,9)])  

print(elisa.plot4)

ggsave(here("output", "ELISA plot - Time 4.png"))
ggsave(here("output", "Final Figures", "ELISA plot - Time 4.png"))

#select data from time 8
elisa8 <- elisa.dat %>%
  filter(Time == 8)

#plot with values below x axis
elisa.plot8 <- ggplot(elisa8, aes(x = Sample.ID, y = as.numeric(BPA), fill = Sample.ID)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Depth, labeller = labeller(Depth = c("0" = "0 m", "10" = "10 m"))) +
  labs(title = "Estrogenicity - Month 8",
       y = "BPA (pg/ml)",
       x = "Sample ID", color = "Sample ID") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  # scale_x_continuous(expand = c(0,0), breaks=c(0, 10, 20, 30)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,200)) +
  scale_fill_manual(values = met.brewer("Archambault", 10)[c(2,9)])  

print(elisa.plot8)

ggsave(here("output", "ELISA plot - Time 8.png"))
ggsave(here("output", "Final Figures", "ELISA plot - Time 8.png"))

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

ggsave(here("output", "ELISA plot - Solutions.png"))


##############################################################################
#Degradation Photography
##############################################################################

#rename columns
names(degrad_photo) [1] <- "Bag.ID"
names(degrad_photo) [4] <- "Sample.ID"
names(degrad_photo) [6] <- "Time.Retrieved"
names(degrad_photo) [7] <- "Date.Retrieved"
names(degrad_photo) [8] <- "Perc.Remaining"
names(degrad_photo) [9] <- "Week"

#select data
degrad_photo <- degrad_photo %>%
  mutate(Sample.ID = as.factor(Sample.ID)) %>%
  mutate(Time.Retrieved = as.numeric(Time.Retrieved)) %>%
  drop_na(Sample.ID)



######### Depth 0
# 5x5 slides

Photo50_Plot <- ggplot(data= degrad_photo[degrad_photo$Depth==0,], aes(x=Week, y=Perc.Remaining, group = Sample.ID, color = Sample.ID)) + 
  geom_point(size = 1) +
  geom_line(linewidth = 1) +
  labs(title = "5 x 5 cm Photo Degradation (0 m)",
       y = "Percent Remaining (%)",
       x = "Time Retrieved (Weeks)", color = "Sample ID") +
  facet_wrap(~ Sample.ID, labeller = labeller(Sample.ID = c("1" = "PIP 1", "2" = "PIP 2", "3" = "PIP 3", "4" = "PIP 4", "5" = "PIP 5", "6" = "PIP 6", "7" = "PIP 7", "8" = "PIP 8", "N" = "Control N", "P" = "Control P"))) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  scale_x_continuous(expand = c(0,0), breaks=c(0, 10, 20, 30)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,110), breaks = c(0,50,100)) +
  scale_color_manual(values=met.brewer("Archambault", 10))

#Turn off graphics panel in R studio
#graphics.off()

#open new window, change size if needed
#windows(8,6, record=T)

print(Photo50_Plot )

ggsave(here("output", "Final Figures", "5x5 - 0m.png"))

###### depth 10


Photo51_Plot <- ggplot(data= degrad_photo[degrad_photo$Depth==10,], aes(x=Week, y=Perc.Remaining, group = Sample.ID, color = Sample.ID)) + 
  geom_point(size = 1) +
  geom_line(linewidth = 1) +
  labs(title = "5 x 5 cm Photo Degradation (10 m)",
       y = "Percent Remaining (%)",
       x = "Time Retrieved (Weeks)", color = "Sample ID") +
  facet_wrap(~ Sample.ID, labeller = labeller(Sample.ID = c("1" = "PIP 1", "2" = "PIP 2", "3" = "PIP 3", "4" = "PIP 4", "5" = "PIP 5", "6" = "PIP 6", "7" = "PIP 7", "8" = "PIP 8", "N" = "Control N", "P" = "Control P"))) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  scale_x_continuous(expand = c(0,0), breaks=c(0, 10, 20, 30)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,110), breaks = c(0,50,100)) +
  scale_color_manual(values=met.brewer("Archambault", 10))

#Turn off graphics panel in R studio
#graphics.off()

#open new window, change size if needed
#windows(8,6, record=T)

print(Photo51_Plot )

ggsave(here("output", "Final Figures", "5x5 - 10m.png")) 


#################################################
#9x9 sheets

#rename columns
names(degrad_photo9) [1] <- "Depth"
names(degrad_photo9) [2] <- "Sample.ID"
names(degrad_photo9) [3] <- "Time"
names(degrad_photo9) [4] <- "Perc.Remaining"

#select data
degrad_photo9 <- degrad_photo9 %>%
  mutate(Sample.ID = as.factor(Sample.ID)) %>%
  mutate(Time = as.numeric(Time)) %>%
  drop_na(Sample.ID)


#######Depth 0

Photo90_Plot <- ggplot(data= degrad_photo9[degrad_photo9$Depth==0,], aes(x=Time, y=Perc.Remaining, group = Sample.ID, color = Sample.ID)) + 
  geom_point(size = 1) +
  geom_line(linewidth = 1) +
  labs(title = "9 x 9 in Photo Degradation (0 m)",
       y = "Percent Remaining (%)",
       x = "Months", color = "Sample ID") +
  facet_wrap(~ Sample.ID, labeller = labeller(Sample.ID = c("1" = "PIP 1", "2" = "PIP 2", "3" = "PIP 3", "4" = "PIP 4", "5" = "PIP 5", "6" = "PIP 6", "7" = "PIP 7", "8" = "PIP 8", "N" = "Control N", "P" = "Control P"))) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  scale_x_continuous(expand = c(0,0), breaks=c(0, 2, 4, 8)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,110), breaks = c(0,50,100)) +
  scale_color_manual(values=met.brewer("Archambault", 10))

#Turn off graphics panel in R studio
#graphics.off()

#open new window, change size if needed
#windows(8,6, record=T)

print(Photo90_Plot)

ggsave(here("output", "Final Figures", "9x9 - 0m.png")) 


##### Depth 10

Photo91_Plot <- ggplot(data= degrad_photo9[degrad_photo9$Depth==10,], aes(x=Time, y=Perc.Remaining, group = Sample.ID, color = Sample.ID)) + 
  geom_point(size = 1) +
  geom_line(linewidth = 1) +
  labs(title = "9 x 9 in Photo Degradation (10 m)",
       y = "Percent Remaining (%)",
       x = "Months", color = "Sample ID") +
  facet_wrap(~ Sample.ID, labeller = labeller(Sample.ID = c("1" = "PIP 1", "2" = "PIP 2", "3" = "PIP 3", "4" = "PIP 4", "5" = "PIP 5", "6" = "PIP 6", "7" = "PIP 7", "8" = "PIP 8", "N" = "Control N", "P" = "Control P"))) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        plot.title = element_text(size = 12, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold")) +
  scale_x_continuous(expand = c(0,0), breaks=c(0, 2, 4, 8)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,110), breaks = c(0,50,100)) +
  scale_color_manual(values=met.brewer("Archambault", 10))

#Turn off graphics panel in R studio
#graphics.off()

#open new window, change size if needed
#windows(8,6, record=T)

print(Photo91_Plot )

ggsave(here("output", "9x9 - 10m.png"))

##############################################################################
#Mass Change
##############################################################################

#time 0, depth 0

#change column names
names(mass) [2] <- "Sample.ID"
names(mass) [3] <- "Time"
names(mass) [4] <- "Pre.Weight"
names(mass) [5] <- "Post.Weight"
names(mass) [8] <- "For.Plot"
names(mass) [9] <- "Perc.Loss"

#remove other controls
avg.mass <- mass %>%
  drop_na(Perc.Loss) %>%
  filter(Sample.ID %in% c(1:8, "P", "N"))

#remove rows with bags gone
avg.mass <- avg.mass[-c(123,126),]

#change average values to negative
avg.mass$negative = avg.mass$Perc.Loss*(-1)

#change NA for time = 0 so that can plot that
avg.mass[c(1:10),1] <- 0.1

#select data from time 0 
avg.mass0 <- avg.mass %>%
  filter(Depth == 0.1)

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
  scale_fill_manual(values = met.brewer("Archambault", 10))  

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(mass.plot0)

ggsave(here("output", "Mass plot - Month 0.png"))
ggsave(here("output", "Final Figures", "Mass plot - Month 0.png"))

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
  scale_fill_manual(values = met.brewer("Archambault", 10)[c(2, 7, 9)])  

print(mass.plot4)

ggsave(here("output", "Mass plot - Month 4.png"))
ggsave(here("output", "Final Figures", "Mass plot - Month 4.png"))


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
  scale_fill_manual(values = met.brewer("Archambault", 10)[c(2,9)])  

print(mass.plot8)

ggsave(here("output", "Mass plot - Month 8.png"))
ggsave(here("output", "Final Figures", "Mass plot - Month 8.png"))

##############################################################################
#Electrical resistance
##############################################################################

#change column names
names(Electro) [1] <- "Depth"
names(Electro) [2] <- "Sample.ID"
names(Electro) [3] <- "Time.Retrieved"
names(Electro) [4] <- "Frequency"
names(Electro) [5] <- "Pre"
names(Electro) [6] <- "Post"
names(Electro) [7] <- "Change"
names(Electro) [8] <- "NegPerc.Loss"
names(Electro) [9] <- "Perc.Change"

#select data
Electro <- Electro %>%
  mutate(Sample.ID = as.factor(Sample.ID)) %>%
  mutate(Frequency = as.factor(Frequency)) %>%
  mutate(Perc.Change = as.numeric(Perc.Change)) %>%
  drop_na(Perc.Change)

###take out 100 and 120 because all 0 this month
Electro <- Electro %>%
  filter(Frequency == "1k" | Frequency == "10k" | Frequency == "100k")

Electro$Frequency <- factor(Electro$Frequency,levels = c("1k", "10k", "100k"))

Electro.plot0 <- ggplot(data= Electro[Electro$Time.Retrieved==0,], aes(x = Sample.ID, y = Perc.Change, fill = Sample.ID)) +
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
  scale_fill_manual(values = met.brewer("Archambault", 10)[c(2, 3, 5, 7, 9)])  

#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(Electro.plot0)

ggsave(here("output", "Resistance Change time 0.png"))

############## Month 4 


###filter for depth 0
Electro0 <- Electro %>%
  filter(Depth == 0.1 | Depth == 0)

#depth 0 plot
Electro.plot40 <- ggplot(data= Electro0[Electro0$Time.Retrieved==4,], aes(x = Sample.ID, y = Perc.Change, fill = Sample.ID)) +
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
  scale_fill_manual(values = met.brewer("Archambault", 10)[c(2, 7, 9)])  


#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(Electro.plot40)

ggsave(here("output", "Resistance Change t4 d0.png"))

###################
#filter fot depth 10
Electro10 <- Electro %>%
  filter(Depth == 0.1 | Depth == 10)


#depth 10 plot
Electro.plot410 <- ggplot(data= Electro10[Electro10$Time.Retrieved==4,], aes(x = Sample.ID, y = Perc.Change, fill = Sample.ID)) +
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
  scale_fill_manual(values = met.brewer("Archambault", 10)[c(2, 9)])  


#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(Electro.plot410)

ggsave(here("output", "Resistance Change t4 d10.png"))

################# Month 8


#depth 0
Electro.plot80 <- ggplot(data= Electro0[Electro0$Time.Retrieved==8,], aes(x = Sample.ID, y = Perc.Change, fill = Sample.ID)) +
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
  scale_fill_manual(values = met.brewer("Archambault", 10)[c(2, 9)])  


#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(Electro.plot80)

ggsave(here("output", "Resistance Change t8 d00.png"))

###################################

#depth 10
Electro.plot810 <- ggplot(data= Electro10[Electro10$Time.Retrieved==8,], aes(x = Sample.ID, y = Perc.Change, fill = Sample.ID)) +
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
  scale_fill_manual(values = met.brewer("Archambault", 10)[c(2, 9)])  

#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(Electro.plot810)

ggsave(here("output", "Resistance Change t8 d10.png"))



###############################################################################
#only 1k
############################################################################333



Electro1k <- Electro %>%
  filter(Frequency == "1k")

Electro1k0 <- Electro1k %>%
  filter(Depth == 0.1 | Depth == 0)

Electro1k10 <- Electro1k %>%
  filter(Depth == 0.1 | Depth == 10)


Electro.plot1k0 <- ggplot(data= Electro1k[Electro1k$Time.Retrieved==0,], aes(x = Sample.ID, y = Perc.Change, fill = Sample.ID)) +
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
  scale_y_continuous(expand = c(0,0), limits=c(-60, 60), breaks = c(-50, -25, 0, 25, 50)) +
  scale_fill_manual(values = met.brewer("Archambault", 10)[c(2, 3, 5, 7, 9)])  

#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(Electro.plot1k0)

ggsave(here("output", "1k only Resistance Change t0.png"))
ggsave(here("output", "Final Figures", "1k only Resistance Change t0.png"))

##time 4

Electro.plot1k40 <- ggplot(data= Electro1k0[Electro1k0$Time.Retrieved==4,], aes(x = Sample.ID, y = Perc.Change, fill = Sample.ID)) +
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
  scale_y_continuous(expand = c(0,0), limits=c(-60, 60), breaks = c(-50, -25, 0, 25, 50)) +
  scale_fill_manual(values = met.brewer("Archambault", 10)[c(2, 7, 9)])

#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(Electro.plot1k40)

ggsave(here("output", "1k only Resistance Change t4 d0.png"))
ggsave(here("output", "Final Figures", "1k only Resistance Change t4 d0.png"))

#### 

Electro.plot1k410 <- ggplot(data= Electro1k10[Electro1k10$Time.Retrieved==4,], aes(x = Sample.ID, y = Perc.Change, fill = Sample.ID)) +
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
  scale_y_continuous(expand = c(0,0), limits=c(-60, 60), breaks = c(-50, -25, 0, 25, 50)) +
  scale_fill_manual(values = met.brewer("Archambault", 10)[c(2,9)])  

#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(Electro.plot1k410)

ggsave(here("output", "1k only Resistance Change t4 d10.png"))
ggsave(here("output", "Final Figures", "1k only Resistance Change t4 d10.png"))

####time 8

Electro.plot1k80 <- ggplot(data= Electro1k0[Electro1k0$Time.Retrieved==8,], aes(x = Sample.ID, y = Perc.Change, fill = Sample.ID)) +
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
  scale_y_continuous(expand = c(0,0), limits=c(-60, 60), breaks = c(-50, -25, 0, 25, 50)) +
  scale_fill_manual(values = met.brewer("Archambault", 10)[c(2,9)])  

#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(Electro.plot1k80)

ggsave(here("output", "1k only Resistance Change t8 d0.png"))
ggsave(here("output", "Final Figures", "1k only Resistance Change t8 d0.png"))

#####

Electro.plot1k810 <- ggplot(data= Electro1k10[Electro1k10$Time.Retrieved==8,], aes(x = Sample.ID, y = Perc.Change, fill = Sample.ID)) +
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
  scale_y_continuous(expand = c(0,0), limits=c(-60, 60), breaks = c(-50, -25, 0, 25, 50)) +
  scale_fill_manual(values = met.brewer("Archambault", 10)[c(2,9)])  

#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(Electro.plot1k810)

ggsave(here("output", "1k only Resistance Change t8 d10.png"))
ggsave(here("output", "Final Figures", "1k only Resistance Change t8 d10.png"))

##############################################################################
#King County Environmental Data
##############################################################################


####################################
# temperature
####################################

#pull out only temp data
temp <- env_data %>%
  select(c(Date, `1_Water_Temperature_degC`, `2_Water_Temperature_degC`)) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) 

#rename column headers
colnames(temp)[2] <- "Shallow"
colnames(temp)[3] <- "Deep"

#pivot data
temp.long <- temp %>%
  pivot_longer(-Date, names_to = "depth", values_to = "temp")

temp.long$depth <- factor(temp.long$depth, levels = c("Shallow", "Deep"))

date.labs <- c("Apr '22", "Jul '22", "Oct '22", "Jan '23")

#plot all data with rolling average of temperature

#only shallow
ggplot(temp, aes(x = Date, y = Shallow)) +
  geom_point(alpha = 0.100, size = 0.5, colour = "grey")+
  geom_smooth(se = FALSE, size = 1)

#only deep
ggplot(temp, aes(x = Date, y = Deep)) +
  geom_point(alpha = 0.100, size = 0.5, colour = "grey")+
  geom_smooth(se = FALSE, size = 1)

#plot together
temp.plot <- ggplot(temp.long, aes(x = Date, y = temp, group = depth, color = as.factor(depth))) +
  geom_point(alpha = 0.100, size = 0.5, colour = "grey") +
  geom_smooth(se = FALSE, aes(group = as.factor(depth)), size = 1.3) +
  labs(title = "Water Temperature",
       y = expression('Temperature'~(degree*C)),
       x = "Month",
       color = "Depth") + 
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 9),
        plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_blank(),
        legend.background = element_blank()) +
  scale_x_continuous(labels = date.labs) +
  scale_y_continuous(expand = c(0,0), limits = c(8, 18)) 

print(temp.plot)

####################################
# salinity
####################################

#pull out only salinity data
sal <- env_data %>%
  select(c(Date, `1_Salinity_PSU`, `2_Salinity_PSU`)) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) 

#rename column headers
colnames(sal)[2] <- "Shallow"
colnames(sal)[3] <- "Deep"

#pivot data
sal.long <- sal %>%
  pivot_longer(-Date, names_to = "depth", values_to = "sal")

sal.long$depth <- factor(sal.long$depth, levels = c("Shallow", "Deep"))


#plot all data with rolling average of salinity

sal.plot <- ggplot(sal.long, aes(x = Date, y = sal, group = depth, color = as.factor(depth))) +
  geom_point(alpha = 0.100, size = 0.5, colour = "grey") +
  geom_smooth(se = FALSE, aes(group = as.factor(depth)), size = 1.3) +
  labs(title = "Salinity",
       y = "Salinity (ppt)",
       x = "Month",
       color = "Depth") +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(size = 9),
        plot.title = element_text(size = 9, face = "bold"),
        axis.title.x = element_blank(),
        legend.background = element_blank()) +
  scale_x_continuous(labels = date.labs) +
  scale_y_continuous(expand = c(0,0), limits = c(13, 33))


print(sal.plot)

####################################
# dissolved oxygen
####################################

#pull out only DO data
do <- env_data %>%
  select(c(Date, `1_Dissolved_Oxygen_mg/L`, `2_Dissolved_Oxygen_mg/L`)) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) 

#rename column headers
colnames(do)[2] <- "Shallow"
colnames(do)[3] <- "Deep"

#pivot data
do.long <- do %>%
  pivot_longer(-Date, names_to = "depth", values_to = "do")

do.long$depth <- factor(do.long$depth, levels = c("Shallow", "Deep"))


#plot all data with rolling average of dissolved oxygen

do.plot <- ggplot(do.long, aes(x = Date, y = do, group = depth, color = as.factor(depth))) +
  geom_point(alpha = 0.100, size = 0.5, colour = "grey") +
  geom_smooth(se = FALSE, aes(group = as.factor(depth)), size = 1.3) +
  labs(title = "Dissolved Oxygen",
       y = "Dissolved O2 (mg/L)",
       x = "Month",
       color = "Depth") +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 9),
        plot.title = element_text(size = 9, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold"),
        legend.background = element_blank()) +
  scale_x_continuous(labels = date.labs) 
# scale_y_continuous(expand = c(0,0), limits = c(13, 33)) +


print(do.plot)

####################################
# pH
####################################

#pull out only ph data
ph <- env_data %>%
  select(c(Date, `1_Sonde_pH`, `Qual_1_Sonde_pH`,
           `2_Sonde_pH`, `Qual_2_Sonde_pH`)) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) 

#rename column headers
colnames(ph)[2] <- "Shallow"
colnames(ph)[3] <- "Qual_1"
colnames(ph)[4] <- "Deep"
colnames(ph)[5] <- "Qual_2"

#remove data with bad qualifier
ph$Deep[ph$Qual_2 == 440] <- "NA"


#pivot data
ph.long <- ph %>%
  select(-Qual_1, -Qual_2) %>%
  drop_na %>%
  mutate(Deep = as.numeric(Deep)) %>%
  pivot_longer(-Date, names_to = "depth", values_to = "ph")

ph.long$depth <- factor(ph.long$depth, levels = c("Shallow", "Deep"))


#plot all data with rolling average of dissolved oxygen

ph.plot <- ggplot(ph.long, aes(x = Date, y = ph, group = depth, color = as.factor(depth))) +
  geom_point(alpha = 0.100, size = 0.5, colour = "grey") +
  geom_smooth(se = FALSE, aes(group = as.factor(depth)), size = 1.3) +
  labs(title = "pH",
       y = "pH",
       x = "Month",
       color = "Depth") +
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 9),
        plot.title = element_text(size = 9, face = "bold"),
        strip.text.x = element_text(size = 9, face = "bold"),
        legend.background = element_blank()) +
  scale_x_continuous(labels = date.labs) +
  scale_y_continuous(expand = c(0,0), limits = c(7.5, 8.7))


print(ph.plot)


###############
# put all environmental plots together
###############

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

env.plot <- (temp.plot + sal.plot) / (do.plot + ph.plot)

print(env.plot)

ggsave(here("output", "EnvFigure.png"))
ggsave(here("output", "Final Figures", "EnvFigure.png"))

##############################################################################
#HOBO Temp Logger
##############################################################################

#change date format for shallow data
temp.hobo.0 <- temp.0 %>%
  mutate(Date = as.Date(Date.Time, format = "%m/%d/%Y")) %>%
  select(-Date.Time) 

#rename column
names(temp.hobo.0) [1] <- "Shallow"

#change date format for deep data
temp.hobo.10 <- temp.10 %>%
  mutate(Date = as.Date(Date.Time, format = "%m/%d/%Y"))%>%
  select(-Date.Time) 

#rename column
names(temp.hobo.10) [1] <- "Deep"

#combine shallow and deep
temp.hobo <- cbind(temp.hobo.0, temp.hobo.10)

#remove extra Date column
temp.hobo <- temp.hobo %>%
  select(-4)

#pivot data
temp.hobo.long <- temp.hobo %>%
  pivot_longer(-Date, names_to = "depth", values_to = "temp")

temp.hobo.long$depth <- factor(temp.hobo.long$depth, levels = c("Shallow", "Deep"))

#select dates of interest (april 1 - Dec 30)
temp.hobo.long <- temp.hobo.long %>%
  filter(between(Date, as.Date('2022-04-01'), as.Date('2022-12-30')))

date.labs <- c("Apr '22", "Jul '22", "Oct '22", "Jan '23")

#plot all data with rolling average of temperature

#plot together
temp.hobo.plot <- ggplot(temp.hobo.long, aes(x = Date, y = temp, group = depth, color = as.factor(depth))) +
  geom_point(alpha = 0.100, size = 0.5, colour = "grey") +
  geom_smooth(se = FALSE, aes(group = as.factor(depth)), size = 1.3) +
  labs(title = "Water Temperature",
       y = expression('Temperature'~(degree*C)),
       x = "Month",
       color = "Depth") + 
  theme_bw() +
  theme(panel.border = element_rect(color = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        axis.text.x = element_text(size = 9),
        plot.title = element_text(size = 9, face = "bold")) +
  scale_x_continuous(labels = date.labs) +
  scale_y_continuous(expand = c(0,0), limits = c(8, 18)) 


#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(temp.hobo.plot)

ggsave(here("output", "Temp - HOBO.png"))
ggsave(here("output", "Final Figures", "Temp - HOBO.png"))



############################################
############################################
# PCA
############################################
############################################

#from this website/tutorial: https://www.datacamp.com/tutorial/pca-analysis-r

#select only numerical data
numerical_data <- data[, c(9:19, 22, 24)]

#remove NAs
numerical_data <- numerical_data %>% drop_na()

#normalize data
data_normalized <- scale(numerical_data)
head(data_normalized)

#correlation matrix
corr_matrix <- cor(data_normalized)

#apply pca
data.pca <- princomp(corr_matrix)
summary(data.pca)

#look at components that explain 87% of the data
data.pca$loadings[, 1:2]

#visualizations

#scree plot, used to determine importance of each principal component and which to retain
fviz_eig(data.pca, addlabels = TRUE)

#graph of variables
fviz_pca_var(data.pca, col.var = "black")

#contribution of each variable
fviz_cos2(data.pca, choice = "var", axes = 1:2)

#biplot combined with cos2
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

###
##only look at yield stress from the tensile measurements
##

#select only numerical data
numerical_data <- data[, c(9:19, 22, 24)]

#remove NAs
numerical_data <- numerical_data %>% drop_na()

#normalize data
data_normalized <- scale(numerical_data)
head(data_normalized)

#correlation matrix
corr_matrix <- cor(data_normalized)

#apply pca
data.pca <- princomp(corr_matrix)
summary(data.pca)

#look at components that explain 87% of the data
data.pca$loadings[, 1:2]

#visualizations

#scree plot, used to determine importance of each principal component and which to retain
fviz_eig(data.pca, addlabels = TRUE)

#graph of variables
fviz_pca_var(data.pca, col.var = "black")

#contribution of each variable
fviz_cos2(data.pca, choice = "var", axes = 1:2)

#biplot combined with cos2
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)


############################################
############################################
# NMDS
############################################
############################################

#for reproducibility 
set.seed(123)

#run nmds
data.nmds <- metaMDS(corr_matrix, distance = "bray",
                     autotransform = F,
                     wascores = TRUE)
#stress
data.nmds
# 0.2805383, the ordination is arbitrary



############################################
############################################
# Power Analysis
############################################
############################################

cohen.ES(test = "t", size = "small") #0.2
cohen.ES(test = "t", size = "medium") #0.5
cohen.ES(test = "t", size = "large") #0.8

#when trying to calculate power with known sample size
t.power <- pwr.t.test(n = 76,
                      d = 0.3,
                      sig.level = .05,
                      power = NULL,
                      type = "paired",
                      alternative = "two.sided")

t.power

#when trying to calculate sample size with power of 0.8
t.size <- pwr.t.test(n = NULL,
                      d = 0.3,
                      sig.level = .05,
                      power = 0.8,
                      type = "paired",
                      alternative = "two.sided")

t.size

names(data) [2] <- "Pre.Post"

data %>% count(Pre.Post)


