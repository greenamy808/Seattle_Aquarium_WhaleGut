## This script performs statistics on the TOM FORD Plastic Innovation Prize (2022)


## set up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#load libraries
library(here)
# library(data.table) #to expand dates CSO
library(vegan)
# library(reshape2)
# library(forcats)
# library(zoo) #allows for rolling averages
# library(car) #looking at qq plot
# library(nlme)
# library(lmerTest) #mixed effects model
# library(emmeans) #contrasts
# library(segmented)
# library(MuMIn) #AIC scores 
# library(faraway) #model residuals
# library(RColorBrewer) #make graphs pretty
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

#import data
# Tensile_controls <- read_csv(here("data", "Controls_PIP_20220524.csv"))
# Tensile_finalists <- read_csv(here("data", "Finalists_PIP_20220525.csv"))
tensile <- read_csv(here("data", "Tensile.csv"))
elisa <- read_csv(here("data", "PIP_ELISA.csv"))
# whale <- read_csv(here("data", "whale_gut_stats.csv"))
# deg_photo <- read_csv(here("data", "deg_photo_stats.csv"))
env_data <- read_csv(here("data", "KingCounty_EnvData.csv"))
mass <- read_csv(here("data", "Mass.csv"))
temp.0 <- read_csv(here("data", "hobo_temp", "Shallow.csv"))
temp.10 <- read_csv(here("data", "hobo_temp", "Deep.csv"))


#from Lyda import
# Tensile_controls <- read.csv("Controls_PIP_20220525.csv", header = TRUE, na.strings=c("","NA"), fileEncoding="UTF-8-BOM")
# Tensile_finalists <- read.csv("Finalists_PIP_20220525.csv", header = TRUE, na.strings=c("","NA"), fileEncoding="UTF-8-BOM")
# elisa <- read.csv("PIP_ELISA.csv", header = TRUE, na.strings=c("","NA"), fileEncoding="UTF-8-BOM")
# whale <-read.csv("whale_gut_stats.csv", header = TRUE, na.strings=c("","NA"), fileEncoding="UTF-8-BOM")
# deg_photo <-read.csv("deg_photo_stats.csv", header = TRUE, na.strings=c("","NA"), fileEncoding="UTF-8-BOM")

##############################################################################
#Tensile Strength Analysis
##############################################################################

#evans plots: elastic modulus, yield strain, yield stress, break strain, tensile strength and breaking energy

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

tensile$Maximum.Load <- as.numeric(tensile$Maximum.Load)
tensile$Yield.Elongation <- as.numeric(tensile$Yield.Elongation)
tensile$Yield.Stress <- as.numeric(tensile$Yield.Stress)
tensile$Modulus <- as.numeric(tensile$Modulus)
tensile$Break.Elongation <- as.numeric(tensile$Break.Elongation)
tensile$Tensile.Stress.Break <- as.numeric(tensile$Tensile.Stress.Break)
tensile$Tensile.Stress.Yield <- as.numeric(tensile$Tensile.Stress.Yield)
tensile$Energy.Break <- as.numeric(tensile$Energy.Break)


# #just select for same data as Lyda, pre-whale time 0
# tensile.pre.0 <- tensile %>%
#   filter(Time == 0) %>%
#   filter(Pre.Post == "Pre") 
# 
# #add a column for type - sample or control
# tensile.pre.0$Type <- c(rep("Sample", 26), rep("Control", 4))
# 
# 
# Load <- ggplot(data = tensile.pre.0, aes(x = Sample.ID, y = Maximum.Load, group = Sample.ID)) + 
#   geom_boxplot(aes(fill = Type)) +
#   scale_fill_manual(values = c("cyan4", "grey"), breaks=c('Finalist', 'Control')) +
#   theme_classic() +
#   ggtitle("Maximum Load") + theme(plot.title = element_text(hjust = 0.5, size = rel(1.6))) +
#   xlab("") +
#   theme(legend.position = "none") +
#   ylab(expression(paste("Maximum Load (N) "))) +
#   theme(axis.title = element_text(size = rel(1.1))) +
#   theme(axis.text.x = element_text(size = rel(1.1))) + 
#   theme(axis.text.y = element_text(size = rel(1.1)))
# 
# 
# Modulus <- ggplot(data = Tensile, aes(x = Sample, y = Modulus, group = Sample)) + 
#   geom_boxplot(aes(fill = Type)) +
#   scale_fill_manual(values = c("cyan4", "grey"), breaks=c('Finalist', 'Control')) +
#   theme_classic() +
#   ggtitle("Modulus") + theme(plot.title = element_text(hjust = 0.5, size = rel(1.6))) +
#   xlab("") +
#   theme(legend.position = "none") +
#   ylab(expression(paste("Modulus of Elasticity (MPa)"))) +
#   theme(axis.title = element_text(size = rel(1.1))) +
#   theme(axis.text.x = element_text(size = rel(1.1))) + 
#   theme(axis.text.y = element_text(size = rel(1.1)))
# 
# Yield <- ggplot(data = Tensile, aes(x = Sample, y = Yield.Elongation, group = Sample)) + 
#   geom_boxplot(aes(fill = Type)) +
#   scale_fill_manual(values = c("cyan4", "grey"), breaks=c('Finalist', 'Control')) +
#   theme_classic() +
#   ggtitle("Yield Elongation") + theme(plot.title = element_text(hjust = 0.5, size = rel(1.6))) +
#   xlab("") +
#   ylab(expression(paste("Yield Elongation (%) "))) +
#   theme(axis.title = element_text(size = rel(1.1))) +
#   theme(axis.text.x = element_text(size = rel(1.1))) + 
#   theme(axis.text.y = element_text(size = rel(1.1)))
# 
# head(Tensile)
# Break <- ggplot(data = Tensile, aes(x = Sample, y = Break.Elongation, group = Sample)) + 
#   geom_boxplot(aes(fill = Type)) +
#   scale_fill_manual(values = c("cyan4", "grey"), breaks=c('Finalist', 'Control')) +
#   theme_classic() +
#   ggtitle("Break Elongation") + theme(plot.title = element_text(hjust = 0.5, size = rel(1.6))) +
#   xlab("") +
#   ylab(expression(paste("Break Elongation (%) "))) +
#   theme(legend.position = "none") +
#   theme(axis.title = element_text(size = rel(1.1))) +
#   theme(axis.text.x = element_text(size = rel(1.1))) + 
#   theme(axis.text.y = element_text(size = rel(1.1)))
# 
# Stress_yield <- ggplot(data = Tensile, aes(x = Sample, y = Tensile.Stress.Yield, group = Sample)) + 
#   geom_boxplot(aes(fill = Type)) +
#   scale_fill_manual(values = c("cyan4", "grey"), breaks=c('Finalist', 'Control')) +
#   theme_classic() +
#   ggtitle("Tensile Stress at Yield") + theme(plot.title = element_text(hjust = 0.5, size = rel(1.6))) +
#   xlab("Sample") +
#   ylab(expression(paste("Tensile Stress at Yield \n(Slope Threshold 0%; MPa"))) +
#   scale_y_continuous(breaks = c(0, 10, 20, 30)) +
#   theme(legend.position = "none") +
#   theme(axis.title = element_text(size = rel(1.1))) +
#   theme(axis.text.x = element_text(size = rel(1.1))) + 
#   theme(axis.text.y = element_text(size = rel(1.1)))
# 
# Stress_break <- ggplot(data = Tensile, aes(x = Sample, y = Tensile.Stress.Break, group = Sample)) + 
#   geom_boxplot(aes(fill = Type)) +
#   scale_fill_manual(values = c("cyan4", "grey"), breaks=c('Finalist', 'Control')) +
#   theme_classic() +
#   ggtitle("Tensile Stress at Break") + theme(plot.title = element_text(hjust = 0.5, size = rel(1.6))) +
#   xlab("Sample") +
#   ylab(expression(paste("Tensile Stress at Break \n(Standard; MPa)"))) +
#   theme(legend.position = "none") +
#   theme(axis.title = element_text(size = rel(1.1))) +
#   theme(axis.text.x = element_text(size = rel(1.1))) + 
#   theme(axis.text.y = element_text(size = rel(1.1)))
# 
# tensile.plot <- (Load + Modulus) /(Break + Yield) /(Stress_break + Stress_yield)
# tensile.plot <- tensile.plot + plot_annotation(title = "Time 0, Pre-whale", tag_levels = 'A')
# 
# #turn off graphics panel in R studio
# graphics.off()
# 
# #open new window, change size if needed
# windows(8,6, record=T)
# 
# print(tensile.plot)
# 
# ggsave(here("output", "Tensile Time 0 Pre-whale.png"))

# #using Lyda's initial T=0 measurements
# Tensile_controls$Type <- "Control"
# Tensile_finalists$Type <- "Finalist"
# 
# #rename columns of interest
# names(Tensile_controls) [6] <- "Maximum.Load"
# names(Tensile_controls) [17] <- "Yield.Elongation"
# names(Tensile_controls) [14] <- "Modulus"
# names(Tensile_controls) [11] <- "Break.Elongation"
# names(Tensile_controls) [12] <- "Tensile.Stress.Break"
# names(Tensile_controls) [20] <- "Tensile.Stress.Yield"
# 
# names(Tensile_finalists) [6] <- "Maximum.Load"
# names(Tensile_finalists) [17] <- "Yield.Elongation"
# names(Tensile_finalists) [14] <- "Modulus"
# names(Tensile_finalists) [11] <- "Break.Elongation"
# names(Tensile_finalists) [12] <- "Tensile.Stress.Break"
# names(Tensile_finalists) [20] <- "Tensile.Stress.Yield"
# 
# Tensile <- rbind(Tensile_controls, Tensile_finalists)
# Tensile$Sample <- as.factor(Tensile$Sample)
# Tensile$Yield.Elongation <- as.numeric(Tensile$Yield.Elongation)  
# Tensile$Tensile.Stress.Yield <- as.numeric(Tensile$Tensile.Stress.Yield)  
# 
# Load <- ggplot(data = Tensile, aes(x = Sample, y = Maximum.Load, group = Sample)) + 
#   geom_boxplot(aes(fill = Type)) +
#   scale_fill_manual(values = c("cyan4", "grey"), breaks=c('Finalist', 'Control')) +
#   theme_classic() +
#   ggtitle("Maximum Load") + theme(plot.title = element_text(hjust = 0.5, size = rel(1.6))) +
#   xlab("") +
#   theme(legend.position = "none") +
#   ylab(expression(paste("Maximum Load (N) "))) +
#   theme(axis.title = element_text(size = rel(1.1))) +
#   theme(axis.text.x = element_text(size = rel(1.1))) + 
#   theme(axis.text.y = element_text(size = rel(1.1)))
# 
# 
# Modulus <- ggplot(data = Tensile, aes(x = Sample, y = Modulus, group = Sample)) + 
#   geom_boxplot(aes(fill = Type)) +
#   scale_fill_manual(values = c("cyan4", "grey"), breaks=c('Finalist', 'Control')) +
#   theme_classic() +
#   ggtitle("Modulus") + theme(plot.title = element_text(hjust = 0.5, size = rel(1.6))) +
#   xlab("") +
#   theme(legend.position = "none") +
#   ylab(expression(paste("Modulus of Elasticity (MPa) "))) +
#   theme(axis.title = element_text(size = rel(1.1))) +
#   theme(axis.text.x = element_text(size = rel(1.1))) + 
#   theme(axis.text.y = element_text(size = rel(1.1)))
# 
# Yield <- ggplot(data = Tensile, aes(x = Sample, y = Yield.Elongation, group = Sample)) + 
#   geom_boxplot(aes(fill = Type)) +
#   scale_fill_manual(values = c("cyan4", "grey"), breaks=c('Finalist', 'Control')) +
#   theme_classic() +
#   ggtitle("Yield Elongation") + theme(plot.title = element_text(hjust = 0.5, size = rel(1.6))) +
#   xlab("") +
#   ylab(expression(paste("Yield Elongation (%) "))) +
#   theme(axis.title = element_text(size = rel(1.1))) +
#   theme(axis.text.x = element_text(size = rel(1.1))) + 
#   theme(axis.text.y = element_text(size = rel(1.1)))
# 
# head(Tensile)
# Break <- ggplot(data = Tensile, aes(x = Sample, y = Break.Elongation, group = Sample)) + 
#   geom_boxplot(aes(fill = Type)) +
#   scale_fill_manual(values = c("cyan4", "grey"), breaks=c('Finalist', 'Control')) +
#   theme_classic() +
#   ggtitle("Break Elongation") + theme(plot.title = element_text(hjust = 0.5, size = rel(1.6))) +
#   xlab("") +
#   ylab(expression(paste("Break Elongation (%) "))) +
#   theme(legend.position = "none") +
#   theme(axis.title = element_text(size = rel(1.1))) +
#   theme(axis.text.x = element_text(size = rel(1.1))) + 
#   theme(axis.text.y = element_text(size = rel(1.1)))
# 
# Stress_yield <- ggplot(data = Tensile, aes(x = Sample, y = Tensile.Stress.Yield, group = Sample)) + 
#   geom_boxplot(aes(fill = Type)) +
#   scale_fill_manual(values = c("cyan4", "grey"), breaks=c('Finalist', 'Control')) +
#   theme_classic() +
#   ggtitle("Tensile Stress at Yield") + theme(plot.title = element_text(hjust = 0.5, size = rel(1.6))) +
#   xlab("Sample") +
#   ylab(expression(paste("Tensile Stress at Yield \n(Slope Threshold 0%; MPa"))) +
#   scale_y_continuous(breaks = c(0, 10, 20, 30)) +
#   theme(legend.position = "none") +
#   theme(axis.title = element_text(size = rel(1.1))) +
#   theme(axis.text.x = element_text(size = rel(1.1))) + 
#   theme(axis.text.y = element_text(size = rel(1.1)))
# 
# Stress_break <- ggplot(data = Tensile, aes(x = Sample, y = Tensile.Stress.Break, group = Sample)) + 
#   geom_boxplot(aes(fill = Type)) +
#   scale_fill_manual(values = c("cyan4", "grey"), breaks=c('Finalist', 'Control')) +
#   theme_classic() +
#   ggtitle("Tensile Stress at Break") + theme(plot.title = element_text(hjust = 0.5, size = rel(1.6))) +
#   xlab("Sample") +
#   ylab(expression(paste("Tensile Stress at Break \n(Standard; MPa)"))) +
#   theme(legend.position = "none") +
#   theme(axis.title = element_text(size = rel(1.1))) +
#   theme(axis.text.x = element_text(size = rel(1.1))) + 
#   theme(axis.text.y = element_text(size = rel(1.1)))
# 
# tensile.plot <- (Load + Modulus) /(Break + Yield) /(Stress_break + Stress_yield)
# tensile.plot <- tensile.plot + plot_annotation(title = "Time 0, Pre-whale", tag_levels = 'A')
# 
# #turn off graphics panel in R studio
# graphics.off()
# 
# #open new window, change size if needed
# windows(8,6, record=T)
# 
# print(tensile.plot)
# 
# ggsave(here("output", "Tensile Time 0 Pre-whale.png"))


#time 0
tensile.0 <- tensile %>%
  filter(Time == 0)

tensile.0$Pre.Post <- factor(tensile.0$Pre.Post, levels = c("Pre", "Post"))

#pulling out only mid break data
tensile_0 <- tensile.0 %>%
  filter(break.location.plot == "mid")

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

# tensile.plot <- (Load + Modulus) /(Break + Yield) /(Stress_break + Stress_yield)
# tensile.plot <- tensile.plot + plot_annotation(title = "Time 0, Pre-whale", tag_levels = 'A')

tensile.plot.0 <- (load.plot.0 + modulus.plot.0) /(yield.stress.plot.0 + yield.elong.plot.0) /(break.elong.plot.0 + stress.yield.plot.0) /
  (stress.break.plot.0 + energy.break.plot.0) + 
  plot_annotation(title = "Tensile Testing - Month 0")

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(tensile.plot.0)

ggsave(here("output", "Tensile Time 0.png"))

####
#break energy for time 0 only
###

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


#time 4, 0 m
tensile.4 <- tensile %>%
  filter(Time == 4) %>%
  filter(Depth == 0)

tensile.4$Pre.Post <- factor(tensile.4$Pre.Post, levels = c("Pre", "Post"))

#pulling out only mid break data
tensile_4 <- tensile.4 %>%
  filter(break.location.plot == "mid")

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

# tensile.plot <- (Load + Modulus) /(Break + Yield) /(Stress_break + Stress_yield)
# tensile.plot <- tensile.plot + plot_annotation(title = "Time 0, Pre-whale", tag_levels = 'A')

tensile.plot.4 <- (load.plot.4 + modulus.plot.4) /(yield.stress.plot.4 + yield.elong.plot.4) /(break.elong.plot.4 + stress.yield.plot.4) /
  (stress.break.plot.4 + energy.break.plot.4)+ plot_annotation (title = "Tensile Testing - Month 4, 0 m")

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(tensile.plot.4)

ggsave(here("output", "Tensile Time 4, 0m.png"))


#time 4, 10 m
tensile.4.10 <- tensile %>%
  filter(Time == 4) %>%
  filter(Depth == 10)

tensile.4.10$Pre.Post <- factor(tensile.4.10$Pre.Post, levels = c("Pre", "Post"))

#pulling out only mid break data
tensile_4.10 <- tensile.4.10 %>%
  filter(break.location.plot == "mid")

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

# tensile.plot <- (Load + Modulus) /(Break + Yield) /(Stress_break + Stress_yield)
# tensile.plot <- tensile.plot + plot_annotation(title = "Time 0, Pre-whale", tag_levels = 'A')

tensile.plot.4.10 <- (load.plot.4.10 + modulus.plot.4.10) /(yield.stress.plot.4.10 + yield.elong.plot.4.10) /
  (break.elong.plot.4.10 + stress.yield.plot.4.10) /
  (stress.break.plot.4.10 + energy.break.plot.4.10)+ plot_annotation (title = "Tensile Testing - Month 4, 10 m")

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(tensile.plot.4.10)

ggsave(here("output", "Tensile Time 4, 10m.png"))


#time 8, depth 0
tensile.8 <- tensile %>%
  filter(Time == 8) %>%
  filter(Depth == 0)

tensile.8$Pre.Post <- factor(tensile.8$Pre.Post, levels = c("Pre", "Post"))

#pulling out only mid break data
tensile_8 <- tensile.8 %>%
  filter(break.location.plot == "mid")

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

# tensile.plot <- (Load + Modulus) /(Break + Yield) /(Stress_break + Stress_yield)
# tensile.plot <- tensile.plot + plot_annotation(title = "Time 0, Pre-whale", tag_levels = 'A')

tensile.plot.8 <- (load.plot.8 + modulus.plot.8) /(yield.stress.plot.8 + yield.elong.plot.8) /(break.elong.plot.8 + stress.yield.plot.8) /
  (stress.break.plot.8 + energy.break.plot.8) + plot_annotation(title = "Tensile Testing - Month 8, 0 m")

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(tensile.plot.8)

ggsave(here("output", "Tensile Time 8, 0m.png"))


#time 8, depth 10
tensile.8.10 <- tensile %>%
  filter(Time == 8) %>%
  filter(Depth == 10)

tensile.8.10$Pre.Post <- factor(tensile.8.10$Pre.Post, levels = c("Pre", "Post"))

#pulling out only mid break data
tensile_8.10 <- tensile.8.10 %>%
  filter(break.location.plot == "mid")

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

# tensile.plot <- (Load + Modulus) /(Break + Yield) /(Stress_break + Stress_yield)
# tensile.plot <- tensile.plot + plot_annotation(title = "Time 0, Pre-whale", tag_levels = 'A')

tensile.plot.8.10 <- (load.plot.8.10 + modulus.plot.8.10) /(yield.stress.plot.8.10 + yield.elong.plot.8.10) /
  (break.elong.plot.8.10 + stress.yield.plot.8.10) /
  (stress.break.plot.8.10 + energy.break.plot.8.10) + plot_annotation(title = "Tensile Testing - Month 8, 10 m")

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(tensile.plot.8.10)

ggsave(here("output", "Tensile Time 8, 10m.png"))

##############################################################################
#ELISA Analysis
##############################################################################


str(elisa)
#need to figure out how to add manual error bars to ggplot

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

# 
# elisa <- elisa %>%
#   arrange(Sample) %>%
#   mutate(Sample = as.factor(Sample)) %>%
#   mutate(Sample.ID = as.factor(Sample.ID)) %>%
#   mutate(Sample = ordered(Sample, levels = c("Finalist", "Control", "Water")))
# 
# elisa.plot <- ggplot(elisa, aes(x = fct_inorder(Sample.ID), y = BPA)) +
#   geom_point(aes(colour = Sample, size = 3)) +
#   #geom_errorbar(aes(ymin = BPA..pg.ml.-CV, ymax = BPA..pg.ml.+CV),width = 0.2) +
#   #geom_linerange(aes(ymin = BPA..pg.ml.-CV, ymax = BPA..pg.ml.+CV)) +
#   scale_colour_manual(values = c("cyan4", "grey", "grey24"), breaks=c('Finalist', 'Control', 'Water')) +
#   ggtitle("ELISA - 2 month") + theme(plot.title = element_text(hjust = 0.5, size = rel(1.6))) +
#   xlab("Sample") +
#   ylab(expression(paste("BPA ", (pg~ml^-1)))) + 
#   scale_y_continuous(trans = 'log10', expand = c(0,0.1)) +
#   #scale_y_continuous(expand = c(0,0)) +
#   theme_classic() +
#   theme(axis.title = element_text(size = rel(1.6))) +
#   theme(axis.text.x = element_text(size = rel(1.5), angle = 45, hjust = 1)) + 
#   theme(axis.text.y = element_text(size = rel(1.5))) +
#   guides(size = "none", colour = guide_legend(override.aes = list(size=3)))


##############################################################################
#Degradation Photography
##############################################################################

# SEE JACKSON R SCRIPT FOR THIS SECTION



# #Degradation Photography
# summary(deg_photo)
# #create a "Sample" column with finalist and control factors
# 
# names(deg_photo) [1] <- "Bag.ID"
# names(deg_photo) [4] <- "Sample.ID"
# names(deg_photo) [6] <- "Time.Retrieved"
# names(deg_photo) [7] <- "Date.Retrieved"
# names(deg_photo) [8] <- "Perc.Remaining"
# 
# deg_photo <- deg_photo %>%
#   mutate(Sample.ID = as.factor(Sample.ID)) %>%
#   mutate(Time.Retrieved = as.numeric(Time.Retrieved)) %>%
#   mutate(Sample = case_when(Sample.ID == "P" ~ "Control",
#                             Sample.ID == "N" ~ "Control",
#                             TRUE ~ "Finalist")) %>%
#   mutate(Sample = as.factor(Sample))
#   
# 
# deg_0 <- ggplot(deg_photo[deg_photo$Depth == 0,], aes(x = Time.Retrieved, y = Perc.Remaining, group = Sample.ID)) +
#   geom_point(size = 5, aes(colour = Sample)) +
#   geom_line(size = 1.5, position=position_jitter(w=0.1, h=0), aes(colour = Sample)) +
#   scale_colour_manual(values = c("cyan4", "grey"), breaks=c('Finalist', 'Control')) +
#   ggtitle("Degradation Photography, 0m") + theme(plot.title = element_text(hjust = 0.5, size = rel(1.6))) +
#   xlab("Week retrieved") +
#   ylab(expression(paste("% Remaining"))) + 
#   scale_y_continuous(expand = c(0,3)) +
#   theme_classic() +
#   theme(legend.position = "none") +
#   theme(axis.title = element_text(size = rel(1.6))) +
#   theme(axis.text.x = element_text(size = rel(1.5))) + 
#   theme(axis.text.y = element_text(size = rel(1.5)))
# 
# deg_10 <- ggplot(deg_photo[deg_photo$Depth == 10,], aes(x = Time.Retrieved, y = X..Remaining, group = Sample.ID)) +
#   geom_point(size = 5, aes(colour = Sample), position=position_jitter(w=0.1, h=0)) +
#   geom_line(size = 1.5, position=position_jitter(w=0.1, h=0), aes(colour = Sample)) +
#   scale_colour_manual(values = c("cyan4", "grey"), breaks=c('Finalist', 'Control')) +
#   ggtitle("Degradation Photography, 10m") + theme(plot.title = element_text(hjust = 0.5, size = rel(1.6))) +
#   xlab("Week retrieved") +
#   ylab("") + 
#   scale_y_continuous(expand = c(0,3)) +
#   theme_classic() +
#   theme(axis.title = element_text(size = rel(1.6))) +
#   theme(axis.text.x = element_text(size = rel(1.5))) + 
#   theme(axis.text.y = element_text(size = rel(1.5)))
# 
# deg_0 + deg_10



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


# #create column with mass/area (g/cm2) - dont want to do volume bc did measure it after
# #or maybe it should just be a % change in mass right now
# 
# str(whale)
# 
# names(whale) [4] <- "Sample.ID"
# names(whale) [15] <- "Post.Weight"
# names(whale) [9] <- "Pre.Weight"
# 
# whale <- whale %>% 
#   mutate(Sample.ID = as.factor(Sample.ID)) %>%
#   drop_na(Sample.ID) %>%
#   mutate(Sample = case_when(Sample.ID == "P" ~ "Control",
#                             Sample.ID == "N" ~ "Control",
#                             Sample.ID == "A" ~ "Control",
#                             Sample.ID == "B" ~ "Control",
#                             Sample.ID == "C" ~ "Control",
#                             Sample.ID == "D" ~ "Control",
#                             TRUE ~ "Finalist")) %>%
#   mutate(Sample = as.factor(Sample))
#   
# whale$mass_change <- ((whale$Post.Weight - whale$Pre.Weight) / whale$Pre.Weight) * 100
# 
# mass.plot <- ggplot(whale, aes(x = Sample.ID, y = mass_change)) +
#   geom_point(size = 5, aes(colour = Sample), position=position_jitter(w=0.1, h=0)) +
#   geom_line(size = 1.5, position=position_jitter(w=0.1, h=0), aes(colour = Sample)) +
#   scale_colour_manual(values = c("cyan4", "grey"), breaks=c('Finalist', 'Control')) +
#   ggtitle("Mass change after whale gut simulation") + theme(plot.title = element_text(hjust = 0.5, size = rel(1.6))) +
#   xlab("Sample") +
#   ylab("% Change") + 
#   scale_y_continuous(expand = c(0,3)) +
#   theme_classic() +
#   theme(axis.title = element_text(size = rel(1.6))) +
#   theme(axis.text.x = element_text(size = rel(1.5))) + 
#   theme(axis.text.y = element_text(size = rel(1.5)))


##############################################################################
#Electrical resistance
##############################################################################

# SEE JACKSON R SCRIPT FOR THIS SECTION

# #need to pivot long to get pre and post in the same column
# #for now, only care about the 110kHz
# head(whale)
# 
# names(whale) [10] <- "Pre.Resistance.100kHz"
# names(whale) [20] <- "Post.Resistance.100kHz"
# 
# elect <- whale %>%
#   select(c(Sample.ID, Replicate, Pre.Resistance.100kHz, Post.Resistance.100kHz)) %>%
#   pivot_longer("Pre.Resistance.100kHz":"Post.Resistance.100kHz", names_to = "Time", values_to = "Resistance") %>%
#   mutate(Time = case_when(Time == "Pre.Resistance.100kHz" ~ "Pre",
#                           Time == "Post.Resistance.100kHz" ~ "Post")) %>%
#   mutate(Sample = case_when(Sample.ID == "P" ~ "Control",
#                             Sample.ID == "N" ~ "Control",
#                             Sample.ID == "A" ~ "Control",
#                             Sample.ID == "B" ~ "Control",
#                             Sample.ID == "C" ~ "Control",
#                             Sample.ID == "D" ~ "Control",
#                             TRUE ~ "Finalist")) %>%
#   mutate(Sample = as.factor(Sample)) %>%
#   mutate(Time = as.factor(Time))
#   
# elect <- as.data.frame(elect)
# str(elect)  
#   
# elec.plot <- ggplot(elect, aes(x = Sample.ID, y = Resistance, group = Sample.ID)) +
#   geom_point(aes(colour = Sample, shape = Time), size = 5, position=position_jitter(w=0.1, h=0)) +
#   geom_line(size = 1.5, position=position_jitter(w=0.1, h=0), aes(colour = Sample)) +
#   scale_colour_manual(values = c("cyan4", "grey"), breaks=c('Finalist', 'Control')) +
#   scale_shape_manual(values=c(1, 19))+
#   ggtitle("Electrical Resistance, 100kHz") + theme(plot.title = element_text(hjust = 0.5, size = rel(1.6))) +
#   xlab("Sample") +
#   ylab("Electrical Resistance (M Ohms)") + 
#   ylim(0,2) +
#   theme_classic() +
#   theme(axis.title = element_text(size = rel(1.6))) +
#   theme(axis.text.x = element_text(size = rel(1.5))) + 
#   theme(axis.text.y = element_text(size = rel(1.5)))
# 
# #turn off graphics panel in R studio
# graphics.off()
# 
# #open new window, change size if needed
# windows(8,6, record=T)
# 
# print(elec.plot)
# 
# ggsave(here("output", "Electrical plot - 2 month.png"))


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












