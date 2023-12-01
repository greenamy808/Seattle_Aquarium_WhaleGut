## This script performs statistics on the TOM FORD Plastic Innovation Prize (2022)


## set up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#load libraries
library(here)
# library(data.table) #to expand dates CSO
# library(vegan)
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
whale <- read_csv(here("data", "whale_gut_stats.csv"))
# deg_photo <- read_csv(here("data", "deg_photo_stats.csv"))
degrad_photo <- read.csv(here("data", "5x5_deg.csv"))
degrad_photo9 <- read.csv(here("data", "9x9_deg.csv"))
env_data <- read_csv(here("data", "KingCounty_EnvData.csv"))
Electro <- read_csv(here("data", "Elec_Res.csv"))
PNEC <- read_csv(here("data", "EUPNEC.csv"))


#from Lyda import
# Tensile_controls <- read.csv("Controls_PIP_20220525.csv", header = TRUE, na.strings=c("","NA"), fileEncoding="UTF-8-BOM")
# Tensile_finalists <- read.csv("Finalists_PIP_20220525.csv", header = TRUE, na.strings=c("","NA"), fileEncoding="UTF-8-BOM")
# elisa <- read.csv("PIP_ELISA.csv", header = TRUE, na.strings=c("","NA"), fileEncoding="UTF-8-BOM")
# whale <-read.csv("whale_gut_stats.csv", header = TRUE, na.strings=c("","NA"), fileEncoding="UTF-8-BOM")
# deg_photo <-read.csv("deg_photo_stats.csv", header = TRUE, na.strings=c("","NA"), fileEncoding="UTF-8-BOM")

##############################################################################
#Tensile Strength Analysis
##############################################################################

# 
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
# 
# 
# #using Aarons's T=0 measurements
# colnames(tensile)
# 
# #rename columns of interest
# names(tensile) [11] <- "Maximum.Load"
# names(tensile) [2] <- "Pre.Post"
# names(tensile) [14] <- "Yield.Elongation"
# names(tensile) [12] <- "Modulus"
# names(tensile) [16] <- "Break.Elongation"
# names(tensile) [15] <- "Tensile.Stress.Break"
# names(tensile) [17] <- "Tensile.Stress.Yield"
# 
# #just select for same data as Lyda
# tensile.dat <- tensile %>%
#   filter(Time == 0) %>%
#   filter(Pre.Post == "Pre") 
# 
# #add a column for type - sample or control
# tensile.dat$Type <- c(rep("Sample", 48), rep("Control", 27))
# 
# tensile.dat$Yield.Elongation <- as.numeric(tensile.dat$Yield.Elongation)  
# tensile.dat$Tensile.Stress.Yield <- as.numeric(tensile.dat$Tensile.Stress.Yield)  
# 
# 
# 
# Load <- ggplot(data = tensile.dat, aes(x = Sample, y = Maximum.Load, group = Sample)) + 
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

##############################################################################
#ELISA Analysis
##############################################################################

# 
# str(elisa)
# #need to figure out how to add manual error bars to ggplot
# 
# names(elisa) [1] <- "Sample.ID"
# names(elisa) [2] <- "BPA"
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
# 
# #turn off graphics panel in R studio
# graphics.off()
# 
# #open new window, change size if needed
# windows(8,6, record=T)
# 
# print(elisa.plot)
# 
# ggsave(here("output", "ELISA plot - 2 month.png"))


##############################################################################
#Degradation Photography
##############################################################################

# 
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
#   geom_line(size = 1, position=position_jitter(w=0.1, h=0), aes(colour = Sample)) +
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
# 
# 
# 
# ##############################################################################
# #Mass Change
# ##############################################################################
# 
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
# 
# #turn off graphics panel in R studio
# graphics.off()
# 
# #open new window, change size if needed
# windows(8,6, record=T)
# 
# print(mass.plot)
# 
# ggsave(here("output", "Mass plot - 2 month.png"))
# 
# ##############################################################################
# #Electrical resistance
# ##############################################################################
# 
# 
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
# 
# 
# ##############################################################################
# #King County Environmental Data
# ##############################################################################
# 
# #pull out only temp data
# temp <- env_data %>%
#   select(c(Date, `1_Water_Temperature_degC`, `2_Water_Temperature_degC`)) %>%
#   mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
# 
# #rename column headers
# colnames(temp)[2] <- "Shallow"
# colnames(temp)[3] <- "Deep"
# 
# #pivot data
# temp.long <- temp %>%
#   pivot_longer(-Date, names_to = "depth", values_to = "temp")
# 
# temp.long$depth <- factor(temp.long$depth, levels = c("Shallow", "Deep"))
# 
# #plot all data with rolling average of temperature
# 
# #only shallow
# ggplot(temp, aes(x = Date, y = Shallow)) +
#   geom_point(alpha = 0.100, size = 0.5, colour = "grey")+
#   geom_smooth(se = FALSE, size = 1)
# 
# #only deep
# ggplot(temp, aes(x = Date, y = Deep)) +
#   geom_point(alpha = 0.100, size = 0.5, colour = "grey")+
#   geom_smooth(se = FALSE, size = 1)
# 
# #plot together
# temp.plot <- ggplot(temp.long, aes(x = Date, y = temp, group = depth, color = as.factor(depth))) +
#   geom_point(alpha = 0.100, size = 0.5, colour = "grey") +
#   geom_smooth(se = FALSE, aes(group = as.factor(depth)), size = 1) +
#   labs(title = "Water Temperature",
#        y = expression('Temperature'~(degree*c)),
#        x = "Month",
#        color = "Depth") +
#   theme(panel.border = element_rect(color = "black", fill = NA),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.position = "right",
#         axis.text.x = element_text(size = 9),
#         plot.title = element_text(size = 6, face = "bold"),
#         strip.text.x = element_text(size = 9, face = "bold"),
#         legend.background = element_blank()) +
#   scale_x_date(date_labels = "%b") +
#   scale_y_continuous(expand = c(0,0), limits = c(8, 18)) +
#   theme_bw()
# 
# #turn off graphics panel in R studio
# graphics.off()
# 
# #open new window, change size if needed
# windows(8,6, record=T)
# 
# print(temp.plot)
# 
# ggsave(here("output", "TempFigure.png"))
# 
# 
# ##############################################################################
# #King County Environmental Data - Plotting all data
# ##############################################################################
# 
# #pull out only temp data
# env <- env_data %>%
#   select(c(Date, `1_Water_Temperature_degC`, `2_Water_Temperature_degC`,
#            `1_Salinity_PSU`, `2_Salinity_PSU`,
#            `1_Dissolved_Oxygen_mg/L`, `2_Dissolved_Oxygen_mg/L`,
#            `1_Chlorophyll_Fluorescence_ug/L`, `2_Chlorophyll_Fluorescence_ug/L`,
#            `1_Turbidity_NTU`, `2_Turbidity_NTU`,
#            `1_Sonde_pH`, `Qual_1_Sonde_pH`,
#            `2_Sonde_pH`, `Qual_2_Sonde_pH`)) %>%
#   mutate(Date = as.Date(Date, format = "%m/%d/%Y"))
# 
# 
# #rename column headers
# colnames(env)[2] <- "Shallow Temp"
# colnames(env)[3] <- "Deep Temp"
# colnames(env)[4] <- "Shallow Salinity"
# colnames(env)[5] <- "Deep Salinity"
# colnames(env)[6] <- "Shallow Dissolved O2"
# colnames(env)[7] <- "Deep Dissolved O2"
# colnames(env)[8] <- "Shallow Chlorophyll"
# colnames(env)[9] <- "Deep Chlorophyll"
# colnames(env)[10] <- "Shallow Turbidity"
# colnames(env)[11] <- "Deep Turbidity"
# colnames(env)[12] <- "Shallow pH"
# colnames(env)[14] <- "Deep pH"
# 
# #pivot data
# env.long <- env %>%
#   pivot_longer(-Date, names_to = "parameters", values_to = "values")
# 
# ggplot(env.long, aes(x = Date, y = values)) +
#   facet_wrap(~parameters, scales = "free") +
#   geom_line()



##############################################################################
#Jackson Degradation photo
##############################################################################

## you don't have to install the packages each time you run R. Just once on 
## each computer you use. then when you want to call up the package, you use 
## library ()

# install.packages("MetBrewer") #adding the color palette
# devtools::install_github("BlakeRMills/MetBrewer")

library(MetBrewer)
met.brewer("Archambault")


# install.packages("devtools")


names(degrad_photo) [1] <- "Bag.ID"
names(degrad_photo) [4] <- "Sample.ID"
names(degrad_photo) [6] <- "Time.Retrieved"
names(degrad_photo) [7] <- "Date.Retrieved"
names(degrad_photo) [8] <- "Perc.Remaining"
names(degrad_photo) [9] <- "Week"

degrad_photo <- degrad_photo %>%
  mutate(Sample.ID = as.factor(Sample.ID)) %>%
  mutate(Time.Retrieved = as.numeric(Time.Retrieved)) %>%
  drop_na(Sample.ID)

  
  
  ######### Depth 0
  
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


#print(Photo50_Plot )


#ggsave(here("output", "5x5 - 0m.png")) #or something like that


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


#print(Photo51_Plot )


#ggsave(here("output", "5x5 - 10m.png")) #or something like that


#################################################

names(degrad_photo9) [1] <- "Depth"
names(degrad_photo9) [2] <- "Sample.ID"
names(degrad_photo9) [3] <- "Time"
names(degrad_photo9) [4] <- "Perc.Remaining"

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


#print(Photo90_Plot )


#ggsave(here("output", "9x9 - 0m.png")) #or something like that


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

 
#print(Photo91_Plot )


#ggsave(here("output", "9x9 - 10m.png")) #or something like that


#################################################################
##Electric Chart
#######################################################################


names(Electro) [1] <- "Depth"
names(Electro) [2] <- "Sample.ID"
names(Electro) [3] <- "Time.Retrieved"
names(Electro) [4] <- "Frequency"
names(Electro) [5] <- "Pre"
names(Electro) [6] <- "Post"
names(Electro) [7] <- "Change"
names(Electro) [8] <- "NegPerc.Loss"
names(Electro) [9] <- "Perc.Change"

library(MetBrewer)
met.brewer("Archambault")
Archambault 

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
  scale_fill_manual(values = met.brewer("Archambault", 10))  

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
  scale_fill_manual(values = met.brewer("Archambault", 10))  


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
  scale_fill_manual(values = met.brewer("Archambault", 10))  


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
  scale_fill_manual(values = met.brewer("Archambault", 10))  


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
  scale_fill_manual(values = met.brewer("Archambault", 10))  

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


#######################################
##estrogenicity + PNEC
#######################################

#change column names
names(PNEC) [2] <- "Sample.ID"
names(PNEC) [3] <- "Time"


#remove other controls
PNEC.dat <- PNEC %>%
  filter(Sample.ID %in% c(1:8, "P", "N", "PNEC"))

#change NA for time = 0 so that can plot that
PNEC.dat[c(1:11),1] <- 0.1

#just plot of time 0
PNEC0 <- PNEC.dat %>%
  filter(Depth == 0.1)

#plot BPA values
PNEC.plot0 <- ggplot(PNEC0, aes(x = Sample.ID, y = as.numeric(BPA), fill = Sample.ID)) +
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
  scale_y_continuous(expand = c(0,0), limits = c(0,1600)) +
  scale_fill_manual(values = met.brewer("Archambault", 11)[c(1:4, 6:7, 9:11)])

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(PNEC.plot0)

ggsave(here("output", "ELISA PNEC plot - Time 0.png"))
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

