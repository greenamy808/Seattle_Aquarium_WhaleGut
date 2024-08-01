## This script performs statistics on the TOM FORD Plastic Innovation Prize (2023)
# FOR MANUSCRIPT FIGURES PUBLICATION
#took amy's code and modified it for figs

## set up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#load libraries
library(here)
library(patchwork)
#library(plyr)
library(tidyverse)
library(pwr) #power analysis
library(ggpattern)


## import data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#import elisa data
elisa <- read.csv(here("data", "PIP_ELISA.csv"))

tensile <- read.csv("Tensile_1.csv", head = TRUE) 

#Lab data has all data (including size) but does not have tensile replicates
#Pre-post is labeled in several columns with each variable
lab <- read.csv("lab_data_1.csv", head = TRUE)

## clean data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#remove electrical information from tensile
#only tested 1k for electrical - get rid of other resistances before re-naming
tensile <- tensile[,-c(5, 23:28)]
str(tensile)

names(tensile) [2] <- "Pre.Post"
names(tensile) [4] <- "PIP.Number"
names(tensile) [14] <- "Tensile.Stress.Break"
names(tensile) [16] <- "Tensile.Stress.Maximum.Force"
names(tensile) [17] <- "Tensile.Strain.Maximum.Force"
names(tensile) [18] <- "Energy.Yield"
names(tensile) [19] <- "Energy.Break"
str(tensile)

#only tested 1k for electrical - get rid of other resistances before re-naming
str(lab)
lab <- lab[,-c(11:12, 14:15)]

names(lab) [4] <- "PIP.Number"
names(lab) [6] <- "Time"
names(lab) [7] <- "Present"
names(lab) [8] <- "Thickness.mm"
names(lab) [9] <- "Size.for.weight.cm.2"
names(lab) [10] <- "Weight"
names(lab) [11] <- "Resistance.1k"

#get rid of missing bags in lab data

str(tensile)

merged_df <- merge(lab, tensile, by = c("Time", "Pre.Post", "Depth", "PIP.Number", "Replicate"), all = TRUE)
merged_df <- merged_df[merged_df$Present != "gone",]
merged_df <- merged_df[!is.na(merged_df$Pre.Post), ]
merged_df$weight.std <- merged_df$Weight / merged_df$Size.for.weight.cm.2




############################################
############################################
# ANOVAs
############################################
############################################

#####
# A only
#####

#all data
dat.A <- merged_df %>%
  filter(PIP.Number == "A")

names(dat.A) [1] <- "Time"

#remove time 0
dat.A.2 <- dat.A %>%
  filter(Time != 0)

#calculate time 0 mean
dat.A %>%
  filter(Time == 0) %>%
  filter(Pre.Post == "Pre") %>%
  select(weight.std, Yield.Elongation, Yield.Stress, Resistance.1k) %>%
  summarize_at(c('weight.std', 'Yield.Elongation', 'Yield.Stress', 'Resistance.1k'), list(mean, sd))

#####
#mass
#####

#select data
dat.A.mass <- dat.A.2 %>%
  select(Time, Pre.Post, Depth, weight.std)


dat.A.mass$Time <- as.factor(dat.A.mass$Time)
dat.A.mass$Depth <- as.factor(dat.A.mass$Depth)
dat.A.mass$Pre.Post <- as.factor(dat.A.mass$Pre.Post)

#three way anova
A.mass.aov <- aov(weight.std ~ Depth * Time * Pre.Post, data = dat.A.mass)
summary(A.mass.aov)

TukeyHSD(A.mass.aov, conf.level = 0.95)

#####
#Yield.Elongation
#####
dat.A.Elon <- dat.A.2 %>%
  select(Time, Pre.Post, Depth, Yield.Elongation)

#normalize by dividing all by time 0 mean 1.7
dat.A.Elon <- dat.A.Elon %>%
  mutate(NElon = Yield.Elongation/1.7)

dat.A.Elon$Time <- as.factor(dat.A.Elon$Time)
dat.A.Elon$Depth <- as.factor(dat.A.Elon$Depth)
dat.A.Elon$Pre.Post <- as.factor(dat.A.Elon$Pre.Post)

#three way anova
A.Elon.aov <- aov(NElon ~ Depth * Time * Pre.Post, data = dat.A.Elon)
summary(A.Elon.aov)

TukeyHSD(A.Elon.aov, conf.level = 0.95)

#####
#Yield.Stress
#####
dat.A.Stress <- dat.A.2 %>%
  select(Time, Pre.Post, Depth, Yield.Stress)

#normalize by dividing all by time 0 mean 4.12
dat.A.Stress <- dat.A.Stress %>%
  mutate(NStress = Yield.Stress/4.12)

dat.A.Stress$Time <- as.factor(dat.A.Stress$Time)
dat.A.Stress$Depth <- as.factor(dat.A.Stress$Depth)
dat.A.Stress$Pre.Post <- as.factor(dat.A.Stress$Pre.Post)

#three way anova
A.Stress.aov <- aov(Yield.Stress ~ Depth * Time * Pre.Post, data = dat.A.Stress)
summary(A.Stress.aov)

TukeyHSD(A.Stress.aov)


#####
#electrical, select only one frequency (1000 Hz)
#####
dat.A.elec <- dat.A.2 %>%
  select(Time, Pre.Post, Depth, Resistance.1k)

#normalize by dividing all by time 0 mean 21
dat.A.elec <- dat.A.elec %>%
  mutate(NElec = Resistance.1k/21)

dat.A.elec$Time <- as.factor(dat.A.elec$Time)
dat.A.elec$Depth <- as.factor(dat.A.elec$Depth)
dat.A.elec$Pre.Post <- as.factor(dat.A.elec$Pre.Post)

#three way anova
A.elec.aov <- aov(Resistance.1k ~ Depth * Time * Pre.Post, data = dat.A.elec)
summary(A.elec.aov)

TukeyHSD(A.elec.aov, conf.level = 0.95)


#####
# B only
#####

#all data
dat.B <- merged_df %>%
  filter(PIP.Number == "B")

names(dat.B) [1] <- "Time"

#remove time 0
dat.B.2 <- dat.B %>%
  filter(Time != 0)

#calculate time 0 mean
dat.B %>%
  filter(Time == 0) %>%
  filter(Pre.Post == "Pre") %>%
  select(weight.std, Yield.Elongation, Yield.Stress, Resistance.1k) %>%
  summarize_at(c('weight.std', 'Yield.Elongation', 'Yield.Stress', 'Resistance.1k'), list(mean, sd))

#####
#mass
#####

#select data
dat.B.mass <- dat.B.2 %>%
  select(Time, Pre.Post, Depth, weight.std)


dat.B.mass$Time <- as.factor(dat.B.mass$Time)
dat.B.mass$Depth <- as.factor(dat.B.mass$Depth)
dat.B.mass$Pre.Post <- as.factor(dat.B.mass$Pre.Post)

#three way anova
B.mass.aov <- aov(weight.std ~ Depth * Time * Pre.Post, data = dat.B.mass)
summary(B.mass.aov)

TukeyHSD(B.mass.aov, conf.level = 0.95)

#####
#Yield.Elongation
#####
dat.B.Elon <- dat.B.2 %>%
  select(Time, Pre.Post, Depth, Yield.Elongation)

#normalize by dividing all by time 0 mean 2.03
dat.B.Elon <- dat.B.Elon %>%
  mutate(NElon = Yield.Elongation/2.03)

dat.B.Elon$Time <- as.factor(dat.B.Elon$Time)
dat.B.Elon$Depth <- as.factor(dat.B.Elon$Depth)
dat.B.Elon$Pre.Post <- as.factor(dat.B.Elon$Pre.Post)

#three way anova
B.Elon.aov <- aov(NElon ~ Depth * Time * Pre.Post, data = dat.B.Elon)
summary(B.Elon.aov)

TukeyHSD(B.Elon.aov, conf.level = 0.95)

#####
#Yield.Stress
#####
dat.B.Stress <- dat.B.2 %>%
  select(Time, Pre.Post, Depth, Yield.Stress)

#normalize by dividing all by time 0 mean 5.44
dat.B.Stress <- dat.B.Stress %>%
  mutate(NStress = Yield.Stress/5.44)

dat.B.Stress$Time <- as.factor(dat.B.Stress$Time)
dat.B.Stress$Depth <- as.factor(dat.B.Stress$Depth)
dat.B.Stress$Pre.Post <- as.factor(dat.B.Stress$Pre.Post)

#three way anova
B.Stress.aov <- aov(Yield.Stress ~ Depth * Time * Pre.Post, data = dat.B.Stress)
summary(B.Stress.aov)

TukeyHSD(B.Stress.aov, conf.level = 0.95)

#####
#electrical, select only one frequency (1000 Hz)
#####
dat.B.elec <- dat.B.2 %>%
  select(Time, Pre.Post, Depth, Resistance.1k)

#normalize by dividing all by time 0 mean 10.6
dat.B.elec <- dat.B.elec %>%
  mutate(NElec = Resistance.1k/10.6)

dat.B.elec$Time <- as.factor(dat.B.elec$Time)
dat.B.elec$Depth <- as.factor(dat.B.elec$Depth)
dat.B.elec$Pre.Post <- as.factor(dat.B.elec$Pre.Post)

#three way anova
B.elec.aov <- aov(Resistance.1k ~ Depth * Time * Pre.Post, data = dat.B.elec)
summary(B.elec.aov)

TukeyHSD(B.elec.aov, conf.level = 0.95)



#####
# C only
#####

#all data
dat.C <- merged_df %>%
  filter(PIP.Number == "C")

names(dat.C) [1] <- "Time"

#remove time 0
dat.C.2 <- dat.C %>%
  filter(Time != 0)

#calculate time 0 mean
dat.C %>%
  filter(Time == 0) %>%
  filter(Pre.Post == "Pre") %>%
  select(weight.std, Yield.Elongation, Yield.Stress, Resistance.1k) %>%
  summarize_at(c('weight.std', 'Yield.Elongation', 'Yield.Stress', 'Resistance.1k'), list(mean, sd))

#####
#mass
#####

#select data
dat.C.mass <- dat.C.2 %>%
  select(Time, Pre.Post, Depth, weight.std)

dat.C.mass$Time <- as.factor(dat.C.mass$Time)
dat.C.mass$Depth <- as.factor(dat.C.mass$Depth)
dat.C.mass$Pre.Post <- as.factor(dat.C.mass$Pre.Post)

#three way anova
C.mass.aov <- aov(weight.std ~ Depth * Time * Pre.Post, data = dat.C.mass)
summary(C.mass.aov)

TukeyHSD(C.mass.aov, conf.level = 0.95)

#####
#Yield.Elongation
#####
dat.C.Elon <- dat.C.2 %>%
  select(Time, Pre.Post, Depth, Yield.Elongation)

#normalize by dividing all by time 0 mean 3.24
dat.C.Elon <- dat.C.Elon %>%
  mutate(NElon = Yield.Elongation/3.24)

dat.C.Elon$Time <- as.factor(dat.C.Elon$Time)
dat.C.Elon$Depth <- as.factor(dat.C.Elon$Depth)
dat.C.Elon$Pre.Post <- as.factor(dat.C.Elon$Pre.Post)

#three way anova
C.Elon.aov <- aov(NElon ~ Depth * Time * Pre.Post, data = dat.C.Elon)
summary(C.Elon.aov)

TukeyHSD(C.Elon.aov, conf.level = 0.95)

#####
#Yield.Stress
#####
dat.C.Stress <- dat.C.2 %>%
  select(Time, Pre.Post, Depth, Yield.Stress)

#normalize by dividing all by time 0 mean 18.9
dat.C.Stress <- dat.C.Stress %>%
  mutate(NStress = Yield.Stress/18.9)

dat.C.Stress$Time <- as.factor(dat.C.Stress$Time)
dat.C.Stress$Depth <- as.factor(dat.C.Stress$Depth)
dat.C.Stress$Pre.Post <- as.factor(dat.C.Stress$Pre.Post)

#three way anova
C.Stress.aov <- aov(Yield.Stress ~ Depth * Time * Pre.Post, data = dat.C.Stress)
summary(C.Stress.aov)

TukeyHSD(C.Stress.aov, conf.level = 0.95)

#####
#electrical, select only one frequency (1000 Hz)
#####
dat.C.elec <- dat.C.2 %>%
  select(Time, Pre.Post, Depth, Resistance.1k)

#normalize by dividing all by time 0 mean 20.7
dat.C.elec <- dat.C.elec %>%
  mutate(NElec = Resistance.1k/20.7)

dat.C.elec$Time <- as.factor(dat.C.elec$Time)
dat.C.elec$Depth <- as.factor(dat.C.elec$Depth)
dat.C.elec$Pre.Post <- as.factor(dat.C.elec$Pre.Post)

#three way anova
C.elec.aov <- aov(Resistance.1k ~ Depth * Time * Pre.Post, data = dat.C.elec)
summary(C.elec.aov)

TukeyHSD(C.elec.aov, conf.level = 0.95)



#####
# D only
#####

#all data
dat.D <- merged_df %>%
  filter(PIP.Number == "D")

names(dat.D) [1] <- "Time"

#remove time 0
dat.D.2 <- dat.D %>%
  filter(Time != 0)

#calculate time 0 mean
dat.D %>%
  filter(Time == 0) %>%
  filter(Pre.Post == "Pre") %>%
  select(weight.std, Yield.Elongation, Yield.Stress, Resistance.1k) %>%
  summarize_at(c('weight.std', 'Yield.Elongation', 'Yield.Stress', 'Resistance.1k'), list(mean, sd))

#####
#mass
#####

#select data
dat.D.mass <- dat.D.2 %>%
  select(Time, Pre.Post, Depth, weight.std)


dat.D.mass$Time <- as.factor(dat.D.mass$Time)
dat.D.mass$Depth <- as.factor(dat.D.mass$Depth)
dat.D.mass$Pre.Post <- as.factor(dat.D.mass$Pre.Post)

#three way anova
D.mass.aov <- aov(weight.std ~ Depth * Time * Pre.Post, data = dat.D.mass)
summary(D.mass.aov)

TukeyHSD(D.mass.aov, conf.level = 0.95)

#####
#Yield.Elongation
#####
dat.D.Elon <- dat.D.2 %>%
  select(Time, Pre.Post, Depth, Yield.Elongation)

#normalize by dividing all by time 0 mean 2.47
dat.D.Elon <- dat.D.Elon %>%
  mutate(NElon = Yield.Elongation/2.47)

dat.D.Elon$Time <- as.factor(dat.D.Elon$Time)
dat.D.Elon$Depth <- as.factor(dat.D.Elon$Depth)
dat.D.Elon$Pre.Post <- as.factor(dat.D.Elon$Pre.Post)

#three way anova
D.Elon.aov <- aov(NElon ~ Depth * Time * Pre.Post, data = dat.D.Elon)
summary(D.Elon.aov)

TukeyHSD(D.Elon.aov, conf.level = 0.95)

#####
#Yield.Stress
#####
dat.D.Stress <- dat.D.2 %>%
  select(Time, Pre.Post, Depth, Yield.Stress)

#normalize by dividing all by time 0 mean 4.01
dat.D.Stress <- dat.D.Stress %>%
  mutate(NStress = Yield.Stress/4.01)

dat.D.Stress$Time <- as.factor(dat.D.Stress$Time)
dat.D.Stress$Depth <- as.factor(dat.D.Stress$Depth)
dat.D.Stress$Pre.Post <- as.factor(dat.D.Stress$Pre.Post)

#three way anova
D.Stress.aov <- aov(Yield.Stress ~ Depth * Time * Pre.Post, data = dat.D.Stress)
summary(D.Stress.aov)

TukeyHSD(D.Stress.aov, conf.level = 0.95)

#####
#electrical, select only one frequency (1000 Hz)
#####
dat.D.elec <- dat.D.2 %>%
  select(Time, Pre.Post, Depth, Resistance.1k)

#normalize by dividing all by time 0 mean 20
dat.D.elec <- dat.D.elec %>%
  mutate(NElec = Resistance.1k/20)

dat.D.elec$Time <- as.factor(dat.D.elec$Time)
dat.D.elec$Depth <- as.factor(dat.D.elec$Depth)
dat.D.elec$Pre.Post <- as.factor(dat.D.elec$Pre.Post)

#three way anova
D.elec.aov <- aov(Resistance.1k ~ Depth * Time * Pre.Post, data = dat.D.elec)
summary(D.elec.aov)

TukeyHSD(D.elec.aov, conf.level = 0.95)


#####
# N only
#####

#all data
dat.N <- merged_df %>%
  filter(PIP.Number == "N")

names(dat.N) [1] <- "Time"

#remove time 0
dat.N.2 <- dat.N %>%
  filter(Time != 0)

#remove duplicate sample N-2
dat.N.3 <- dat.N.2[-2,]

#calculate time 0 mean
dat.N %>%
  filter(Time == 0) %>%
  filter(Pre.Post == "Pre") %>%
  select(weight.std, Yield.Elongation, Yield.Stress, Resistance.1k) %>%
  summarize_at(c('weight.std', 'Yield.Elongation', 'Yield.Stress', 'Resistance.1k'), list(mean, sd))

#####
#mass
#####

#select data
dat.N.mass <- dat.N.2 %>%
  select(Time, Pre.Post, Depth, weight.std)


dat.N.mass$Time <- as.factor(dat.N.mass$Time)
dat.N.mass$Depth <- as.factor(dat.N.mass$Depth)
dat.N.mass$Pre.Post <- as.factor(dat.N.mass$Pre.Post)

#three way anova
N.mass.aov <- aov(weight.std ~ Depth * Time * Pre.Post, data = dat.N.mass)
summary(N.mass.aov)

TukeyHSD(N.mass.aov, conf.level = 0.95)


#####
#Yield.Elongation
#####
dat.N.Elon <- dat.N.2 %>%
  select(Time, Pre.Post, Depth, Yield.Elongation)

#normalize by dividing all by time 0 SINGLE SAMPLE 1.38
dat.N.Elon <- dat.N.Elon %>%
  mutate(NElon = Yield.Elongation/1.38)

dat.N.Elon$Time <- as.factor(dat.N.Elon$Time)
dat.N.Elon$Depth <- as.factor(dat.N.Elon$Depth)
dat.N.Elon$Pre.Post <- as.factor(dat.N.Elon$Pre.Post)

#three way anova
N.Elon.aov <- aov(NElon ~ Depth * Time * Pre.Post, data = dat.N.Elon)
summary(N.Elon.aov)

TukeyHSD(N.Elon.aov, conf.level = 0.95)

#run again without duplicate
dat.N.Elon.2 <- dat.N.3 %>%
  select(Time, Pre.Post, Depth, Yield.Elongation)

#normalize by dividing all by time 0 SINGLE SAMPLE 1.38
dat.N.Elon.2 <- dat.N.Elon.2 %>%
  mutate(NElon = Yield.Elongation/1.38)

dat.N.Elon.2$Time <- as.factor(dat.N.Elon.2$Time)
dat.N.Elon.2$Depth <- as.factor(dat.N.Elon.2$Depth)
dat.N.Elon.2$Pre.Post <- as.factor(dat.N.Elon.2$Pre.Post)

#three way anova
N.Elon.aov.2 <- aov(NElon ~ Depth * Time * Pre.Post, data = dat.N.Elon.2)
summary(N.Elon.aov.2)

TukeyHSD(N.Elon.aov.2, conf.level = 0.95)

#####
#Yield.Stress
#####
dat.N.Stress <- dat.N.2 %>%
  select(Time, Pre.Post, Depth, Yield.Stress)

#normalize by dividing all by time 0 SINGLE SAMPLE 3.47
dat.N.Stress <- dat.N.Stress %>%
  mutate(NStress = Yield.Stress/3.47)

dat.N.Stress$Time <- as.factor(dat.N.Stress$Time)
dat.N.Stress$Depth <- as.factor(dat.N.Stress$Depth)
dat.N.Stress$Pre.Post <- as.factor(dat.N.Stress$Pre.Post)

#three way anova
N.Stress.aov <- aov(Yield.Stress ~ Depth * Time * Pre.Post, data = dat.N.Stress)
summary(N.Stress.aov)

TukeyHSD(N.Stress.aov, conf.level = 0.95)

#run again without duplicate
dat.N.Stress.2 <- dat.N.3 %>%
  select(Time, Pre.Post, Depth, Yield.Stress)

#normalize by dividing all by time 0 SINGLE SAMPLE 3.47
dat.N.Stress.2 <- dat.N.Stress.2 %>%
  mutate(NStress = Yield.Stress/3.47)

dat.N.Stress.2$Time <- as.factor(dat.N.Stress.2$Time)
dat.N.Stress.2$Depth <- as.factor(dat.N.Stress.2$Depth)
dat.N.Stress.2$Pre.Post <- as.factor(dat.N.Stress.2$Pre.Post)

#three way anova
N.Stress.aov.2 <- aov(Yield.Stress ~ Depth * Time * Pre.Post, data = dat.N.Stress.2)
summary(N.Stress.aov.2)

TukeyHSD(N.Stress.aov, conf.level = 0.95)

#####
#electrical, select only one frequency (1000 Hz)
#####
dat.N.elec <- dat.N.2 %>%
  select(Time, Pre.Post, Depth, Resistance.1k)

#normalize by dividing all by time 0 mean 23.5
dat.N.elec <- dat.N.elec %>%
  mutate(NElec = Resistance.1k/23.5)

dat.N.elec$Time <- as.factor(dat.N.elec$Time)
dat.N.elec$Depth <- as.factor(dat.N.elec$Depth)
dat.N.elec$Pre.Post <- as.factor(dat.N.elec$Pre.Post)

#three way anova
N.elec.aov <- aov(Resistance.1k ~ Depth * Time * Pre.Post, data = dat.N.elec)
summary(N.elec.aov)

TukeyHSD(N.elec.aov, conf.level = 0.95)

#rerun after removing duplicate
dat.N.elec.2 <- dat.N.3 %>%
  select(Time, Pre.Post, Depth, Resistance.1k)

#normalize by dividing all by time 0 mean 23.5
dat.N.elec.2 <- dat.N.elec.2 %>%
  mutate(NElec = Resistance.1k/23.5)

dat.N.elec.2$Time <- as.factor(dat.N.elec.2$Time)
dat.N.elec.2$Depth <- as.factor(dat.N.elec.2$Depth)
dat.N.elec.2$Pre.Post <- as.factor(dat.N.elec.2$Pre.Post)

#three way anova
N.elec.aov.2 <- aov(Resistance.1k ~ Depth * Time * Pre.Post, data = dat.N.elec.2)
summary(N.elec.aov.2)

TukeyHSD(N.elec.aov, conf.level = 0.95)

##########################################
#add all ratio values into one table
##########################################

#####
#A
#####
A.ratios <- cbind(dat.A.2$Time, dat.A.2$Pre.Post, dat.A.2$Depth, dat.A.mass$weight.std.std, dat.A.Elon$NElon, dat.A.Stress$NStress, dat.A.elec$NElec)
colnames(A.ratios) = c("Time", "Pre.Post", "Depth", "weight.std", "Yield.Elongation", "Yield.Stress", "Resistance.1k")


#turn to numeric data
A.ratios <- transform(A.ratios, weight.std = as.numeric(weight.std),
                      Yield.Elongation = as.numeric(Yield.Elongation),
                      Yield.Stress = as.numeric(Yield.Stress),
                      Resistance.1k = as.numeric(Resistance.1k))


#shorten to 2 decimal places
A.ratios <- A.ratios %>%
  mutate_at(vars(weight.std, Yield.Elongation, Yield.Stress, Resistance.1k), funs(round(., 1)))


#export as csv
write.table(A.ratios, file = "C:/Users/olsena/OneDrive - Seattle Aquarium/Clean Seas/PIP/Seattle_Aquarium_WhaleGut/output\\A.ratios.csv", sep = ",", row.names = FALSE)

#####
# B
####
B.ratios <- cbind(dat.B.2$Time, dat.B.2$Pre.Post, dat.B.2$Depth, dat.B.mass$weight.std.std, dat.B.Elon$NElon, dat.B.Stress$NStress, dat.B.elec$NElec)
colnames(B.ratios) = c("Time", "Pre.Post", "Depth", "weight.std", "Yield.Elongation", "Yield.Stress", "Resistance.1k")


#turn to numeric data
B.ratios <- transform(B.ratios, weight.std = as.numeric(weight.std),
                      Yield.Elongation = as.numeric(Yield.Elongation),
                      Yield.Stress = as.numeric(Yield.Stress),
                      Resistance.1k = as.numeric(Resistance.1k))


#shorten to 2 decimal places
B.ratios <- B.ratios %>%
  mutate_at(vars(weight.std, Yield.Elongation, Yield.Stress, Resistance.1k), funs(round(., 1)))


#export as csv
write.table(B.ratios, file = "C:/Users/olsena/OneDrive - Seattle Aquarium/Clean Seas/PIP/Seattle_Aquarium_WhaleGut/output\\B.ratios.csv", sep = ",", row.names = FALSE)


#####
# C
####
C.ratios <- cbind(dat.C.2$Time, dat.C.2$Pre.Post, dat.C.2$Depth, dat.C.mass$weight.std.std, dat.C.Elon$NElon, dat.C.Stress$NStress, dat.C.elec$NElec)
colnames(C.ratios) = c("Time", "Pre.Post", "Depth", "weight.std", "Yield.Elongation", "Yield.Stress", "Resistance.1k")


#turn to numeric data
C.ratios <- transform(C.ratios, weight.std = as.numeric(weight.std),
                      Yield.Elongation = as.numeric(Yield.Elongation),
                      Yield.Stress = as.numeric(Yield.Stress),
                      Resistance.1k = as.numeric(Resistance.1k))


#shorten to 2 decimal places
C.ratios <- C.ratios %>%
  mutate_at(vars(weight.std, Yield.Elongation, Yield.Stress, Resistance.1k), funs(round(., 1)))


#export as csv
write.table(C.ratios, file = "C:/Users/olsena/OneDrive - Seattle Aquarium/Clean Seas/PIP/Seattle_Aquarium_WhaleGut/output\\C.ratios.csv", sep = ",", row.names = FALSE)



#####
# D
####
D.ratios <- cbind(dat.D.2$Time, dat.D.2$Pre.Post, dat.D.2$Depth, dat.D.mass$weight.std.std, dat.D.Elon$NElon, dat.D.Stress$NStress, dat.D.elec$NElec)
colnames(D.ratios) = c("Time", "Pre.Post", "Depth", "weight.std", "Yield.Elongation", "Yield.Stress", "Resistance.1k")


#turn to numeric data
D.ratios <- transform(D.ratios, weight.std = as.numeric(weight.std),
                      Yield.Elongation = as.numeric(Yield.Elongation),
                      Yield.Stress = as.numeric(Yield.Stress),
                      Resistance.1k = as.numeric(Resistance.1k))


#shorten to 2 decimal places
D.ratios <- D.ratios %>%
  mutate_at(vars(weight.std, Yield.Elongation, Yield.Stress, Resistance.1k), funs(round(., 1)))


#export as csv
write.table(D.ratios, file = "C:/Users/olsena/OneDrive - Seattle Aquarium/Clean Seas/PIP/Seattle_Aquarium_WhaleGut/output\\D.ratios.csv", sep = ",", row.names = FALSE)


#####
# N
####
N.ratios <- cbind(dat.N.2$Time, dat.N.2$Pre.Post, dat.N.2$Depth, dat.N.mass$weight.std.std, dat.N.Elon$NElon, dat.N.Stress$NStress, dat.N.elec$NElec)
colnames(N.ratios) = c("Time", "Pre.Post", "Depth", "weight.std", "Yield.Elongation", "Yield.Stress", "Resistance.1k")


#turn to numeric data
N.ratios <- transform(N.ratios, weight.std = as.numeric(weight.std),
                      Yield.Elongation = as.numeric(Yield.Elongation),
                      Yield.Stress = as.numeric(Yield.Stress),
                      Resistance.1k = as.numeric(Resistance.1k))


#shorten to 2 decimal places
N.ratios <- N.ratios %>%
  mutate_at(vars(weight.std, Yield.Elongation, Yield.Stress, Resistance.1k), funs(round(., 1)))


#export as csv
write.table(N.ratios, file = "C:/Users/olsena/OneDrive - Seattle Aquarium/Clean Seas/PIP/Seattle_Aquarium_WhaleGut/output\\N.ratios.csv", sep = ",", row.names = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Visualizations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#add plastic type column
plot.data <- merged_df %>%
  mutate(Type = case_when(
    PIP.Number == "P" ~ "Paper",
    PIP.Number == "N" ~ "HDPE Thick film",
    PIP.Number == "A" ~ "HDPE film",
    PIP.Number == "B" ~ "LDPE film",
    PIP.Number == "C" ~ "PP Chip Bag",
    PIP.Number == "D" ~ "PE Poly Bag"
  ))

#create color palette
palette <- c("#C77D83", "#D6746A","#88A0DC", "#52468A","#7C4B73","#4E2A67")
palette2 <- c("#C77D83", "#C77D83","#88A0DC", "#88A0DC","#7C4B73","#7C4B73")


plot.data$Yield.Strain <- plot.data$Yield.Elongation / 100

#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summarized
# groupnames : vector of column names to be used as
# grouping variables

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      se = sd(x[[col]], na.rm=TRUE) / sqrt(length(x[[col]])))  # Calculate SE
  }
  data_sum <- ddply(data, groupnames, .fun = summary_func, col = varname)
  colnames(data_sum)[colnames(data_sum) == "mean"] <- varname
  return(data_sum)
}




#~~~~~~~~~~~~~~~~
#N only
#~~~~~~~~~~~~~~~~

#select only N data
N.plot.data <- plot.data %>%
  filter(PIP.Number == "N")

#rename column
names(N.plot.data) [1] <- "Time"

#mass only

str(N.plot.data)
N.mass.dat <- data_summary(N.plot.data, varname = "weight.std", groupnames = c("Pre.Post", "Depth", "Time"))

#calculate mean and se
# N.mass.dat <- aggregate(weight.std ~ Pre.Post + Depth + Time, data = N.plot.data,
#           FUN = function(x) c(mean = mean(x), se = std.error(x)))

#rename columns
# names(N.mass.dat) [4] <- "se"

#create new column with depth and pre.post for plotting
N.mass.dat <- N.mass.dat %>%
  mutate(Depth.WG = paste(Depth, Pre.Post))

#reorder
N.mass.dat$Depth.WG <- factor(N.mass.dat$Depth.WG,
                              levels = c("NA Pre", "NA Post",
                                         "0 Pre", "0 Post",
                                         "10 Pre", "10 Post"))

N.mass.dat$Pre.Post <- factor(N.mass.dat$Pre.Post,
                              level = c("Pre", "Post"))


#add pattern
N.mass <- ggplot(N.mass.dat, aes(x = factor(Time), y = weight.std, fill = factor(Depth.WG), pattern = Pre.Post)) +
  geom_bar_pattern(stat = "identity", color = "black", width = 0.8, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = weight.std - se, ymax = weight.std + se), width = 0.2,
                position = position_dodge(0.9)) +
  labs(x = "Time",
       fill = "Depth",
       pattern = "Whale Gut") +
  ylab(expression(paste("Mass " (g ~cm^-2)))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.015)) +
  scale_fill_manual(values = palette2,
                    breaks = c('NA Pre', 'NA Post', '0 Pre', '0 Post',
                               '10 Pre', '10 Post'),
                    labels =c('NA', 'NA', '0m', '0m',
                              '10m', '10m')) +
  scale_pattern_manual(values = c(Pre = "none", Post = "stripe")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))



#electrical only

N.elec.dat <- data_summary(N.plot.data, varname = "Resistance.1k", groupnames = c("Pre.Post", "Depth", "Time"))

#add column for plotting
N.elec.dat <- N.elec.dat %>%
  mutate(Depth.WG = paste(Depth, Pre.Post))

#reorder
N.elec.dat$Depth.WG <- factor(N.elec.dat$Depth.WG,
                              levels = c("NA Pre", "NA Post",
                                         "0 Pre", "0 Post",
                                         "10 Pre", "10 Post"))

N.elec.dat$Pre.Post <- factor(N.elec.dat$Pre.Post,
                              level = c("Pre", "Post"))



#add pattern
N.elec <- ggplot(N.elec.dat, aes(x = factor(Time), y = Resistance.1k, fill = factor(Depth.WG), pattern = Pre.Post)) +
  geom_bar_pattern(stat = "identity", color = "black", width = 0.8, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = Resistance.1k - se, ymax = Resistance.1k + se), width = 0.2,
                position = position_dodge(0.9)) +
  labs(x = "Time",
       y = "Electrical Resistance (1k)",
       fill = "Depth",
       pattern = "Whale Gut") +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 65)) +
  scale_fill_manual(values = palette2,
                    breaks = c('NA Pre', 'NA Post', '0 Pre', '0 Post',
                               '10 Pre', '10 Post'),
                    labels =c('NA', 'NA', '0m', '0m',
                              '10m', '10m')) +
  scale_pattern_manual(values = c(Pre = "none", Post = "stripe")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))

#Yield Strain only

N.elon.dat <- data_summary(N.plot.data, varname = "Yield.Strain", groupnames = c("Pre.Post", "Depth", "Time"))

#create new column for plotting
N.elon.dat <- N.elon.dat %>%
  mutate(Depth.WG = paste(Depth, Pre.Post))

#reorder
N.elon.dat$Depth.WG <- factor(N.elon.dat$Depth.WG,
                              levels = c("NA Pre", "NA Post",
                                         "0 Pre", "0 Post",
                                         "10 Pre", "10 Post"))

N.elon.dat$Pre.Post <- factor(N.elon.dat$Pre.Post,
                              level = c("Pre", "Post"))

#add pattern
N.elon <- ggplot(N.elon.dat, aes(x = factor(Time), y = Yield.Strain, fill = factor(Depth.WG), pattern = Pre.Post)) +
  geom_bar_pattern(stat = "identity", color = "black", width = 0.8, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = Yield.Strain - se, ymax = Yield.Strain + se), width = 0.2,
                position = position_dodge(0.9)) +
  labs(x = "Time",
       y = "Yield Strain",
       fill = "Depth",
       pattern = "Whale Gut") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, .17)) +
  scale_fill_manual(values = palette2,
                    breaks = c('NA Pre', 'NA Post', '0 Pre', '0 Post',
                               '10 Pre', '10 Post'),
                    labels =c('NA', 'NA', '0m', '0m',
                              '10m', '10m')) +
  scale_pattern_manual(values = c(Pre = "none", Post = "stripe")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))


#Yield Stress only

N.stress.dat <- data_summary(N.plot.data, varname = "Yield.Stress", groupnames = c("Pre.Post", "Depth", "Time"))

#create a new column for plotting
N.stress.dat <- N.stress.dat %>%
  mutate(Depth.WG = paste(Depth, Pre.Post))

#reorder
N.stress.dat$Depth.WG <- factor(N.stress.dat$Depth.WG,
                                levels = c("NA Pre", "NA Post",
                                           "0 Pre", "0 Post",
                                           "10 Pre", "10 Post"))

N.stress.dat$Pre.Post <- factor(N.stress.dat$Pre.Post,
                                level = c("Pre", "Post"))


#add pattern
N.stress <- ggplot(N.stress.dat, aes(x = factor(Time), y = Yield.Stress, fill = factor(Depth.WG), pattern = Pre.Post)) +
  geom_bar_pattern(stat = "identity", color = "black", width = 0.8, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = Yield.Stress - se, ymax = Yield.Stress + se), width = 0.2,
                position = position_dodge(0.9)) +
  labs(x = "Time",
       y = "Yield Stress (MPa)",
       fill = "Depth",
       pattern = "Whale Gut") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 25)) +
  scale_fill_manual(values = palette2,
                    breaks = c('NA Pre', 'NA Post', '0 Pre', '0 Post',
                               '10 Pre', '10 Post'),
                    labels =c('NA', 'NA', '0m', '0m',
                              '10m', '10m')) +
  scale_pattern_manual(values = c(Pre = "none", Post = "stripe")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))


#put all four plots together
all.N <- N.mass + N.elec + N.elon + N.stress
all.N <- all.N + plot_annotation(
  title = "HDPE Thick Film",
  tag_levels = 'A')
all.N

#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)


print(all.N)

ggsave(here("output", "HDPE thick film plot.png"))


#~~~~~~~~~~~~~~~~
#A only
#~~~~~~~~~~~~~~~~

A.plot.data <- plot.data %>%
  filter(PIP.Number == "A")

names(A.plot.data)[1] <- "Time"

#mass only

A.mass.dat <- data_summary(A.plot.data, varname = "weight.std", groupnames = c("Pre.Post", "Depth", "Time"))

#add new column for plotting
A.mass.dat <- A.mass.dat %>%
  mutate(Depth.WG = paste(Depth, Pre.Post))

#reorder
A.mass.dat$Depth.WG <- factor(A.mass.dat$Depth.WG,
                              levels = c("NA Pre", "NA Post",
                                         "0 Pre", "0 Post",
                                         "10 Pre", "10 Post"))

A.mass.dat$Pre.Post <- factor(A.mass.dat$Pre.Post,
                              level = c("Pre", "Post"))


#add pattern
A.mass <- ggplot(A.mass.dat, aes(x = factor(Time), y = weight.std, fill = factor(Depth.WG), pattern = Pre.Post)) +
  geom_bar_pattern(stat = "identity", color = "black", width = 0.8, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = weight.std - se, ymax = weight.std + se), width = 0.2,
                position = position_dodge(0.9)) +
  labs(x = "Time",
       fill = "Depth",
       pattern = "Whale Gut") +
  ylab(expression(paste("Mass " (g ~cm^-2)))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.015)) +
  scale_fill_manual(values = palette2,
                    breaks = c('NA Pre', 'NA Post', '0 Pre', '0 Post',
                               '10 Pre', '10 Post'),
                    labels =c('NA', 'NA', '0m', '0m',
                              '10m', '10m')) +
  scale_pattern_manual(values = c(Pre = "none", Post = "stripe")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))

#electrical only

A.elec.dat <- data_summary(A.plot.data, varname = "Resistance.1k", groupnames = c("Pre.Post", "Depth", "Time"))

#add column for plotting
A.elec.dat <- A.elec.dat %>%
  mutate(Depth.WG = paste(Depth, Pre.Post))

#reorder
A.elec.dat$Depth.WG <- factor(A.elec.dat$Depth.WG,
                              levels = c("NA Pre", "NA Post",
                                         "0 Pre", "0 Post",
                                         "10 Pre", "10 Post"))

A.elec.dat$Pre.Post <- factor(A.elec.dat$Pre.Post,
                              level = c("Pre", "Post"))


#add pattern
A.elec <- ggplot(A.elec.dat, aes(x = factor(Time), y = Resistance.1k, fill = factor(Depth.WG), pattern = Pre.Post)) +
  geom_bar_pattern(stat = "identity", color = "black", width = 0.8, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = Resistance.1k - se, ymax = Resistance.1k + se), width = 0.2,
                position = position_dodge(0.9)) +
  labs(x = "Time",
       y = "Electrical Resistance (1k)",
       fill = "Depth",
       pattern = "Whale Gut") +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 65)) +
  scale_fill_manual(values = palette2,
                    breaks = c('NA Pre', 'NA Post', '0 Pre', '0 Post',
                               '10 Pre', '10 Post'),
                    labels =c('NA', 'NA', '0m', '0m',
                              '10m', '10m')) +
  scale_pattern_manual(values = c(Pre = "none", Post = "stripe")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))

#Yield Elongation only

A.elon.dat <- data_summary(A.plot.data, varname = "Yield.Strain", groupnames = c("Pre.Post", "Depth", "Time"))

#add column for plotting
A.elon.dat <- A.elon.dat %>%
  mutate(Depth.WG = paste(Depth, Pre.Post))

#reorder
A.elon.dat$Depth.WG <- factor(A.elon.dat$Depth.WG,
                              levels = c("NA Pre", "NA Post",
                                         "0 Pre", "0 Post",
                                         "10 Pre", "10 Post"))


A.elon.dat$Pre.Post <- factor(A.elon.dat$Pre.Post,
                              level = c("Pre", "Post"))

#add pattern
A.elon <- ggplot(A.elon.dat, aes(x = factor(Time), y = Yield.Strain, fill = factor(Depth.WG), pattern = Pre.Post)) +
  geom_bar_pattern(stat = "identity", color = "black", width = 0.8, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = Yield.Strain - se, ymax = Yield.Strain + se), width = 0.2,
                position = position_dodge(0.9)) +
  labs(x = "Time",
       y = "Yield Strain",
       fill = "Depth",
       pattern = "Whale Gut") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, .17)) +
  scale_fill_manual(values = palette2,
                    breaks = c('NA Pre', 'NA Post', '0 Pre', '0 Post',
                               '10 Pre', '10 Post'),
                    labels =c('NA', 'NA', '0m', '0m',
                              '10m', '10m')) +
  scale_pattern_manual(values = c(Pre = "none", Post = "stripe")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))

#Yield Stress only

A.stress.dat <- data_summary(A.plot.data, varname = "Yield.Stress", groupnames = c("Pre.Post", "Depth", "Time"))

#add column for plotting
A.stress.dat <- A.stress.dat %>%
  mutate(Depth.WG = paste(Depth, Pre.Post))

#reorder
A.stress.dat$Depth.WG <- factor(A.stress.dat$Depth.WG,
                                levels = c("NA Pre", "NA Post",
                                           "0 Pre", "0 Post",
                                           "10 Pre", "10 Post"))

A.stress.dat$Pre.Post <- factor(A.stress.dat$Pre.Post,
                                level = c("Pre", "Post"))



#add pattern
A.stress <- ggplot(A.stress.dat, aes(x = factor(Time), y = Yield.Stress, fill = factor(Depth.WG), pattern = Pre.Post)) +
  geom_bar_pattern(stat = "identity", color = "black", width = 0.8, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = Yield.Stress - se, ymax = Yield.Stress + se), width = 0.2,
                position = position_dodge(0.9)) +
  labs(x = "Time",
       y = "Yield Stress (MPa)",
       fill = "Depth",
       pattern = "Whale Gut") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 25)) +
  scale_fill_manual(values = palette2,
                    breaks = c('NA Pre', 'NA Post', '0 Pre', '0 Post',
                               '10 Pre', '10 Post'),
                    labels =c('NA', 'NA', '0m', '0m',
                              '10m', '10m')) +
  scale_pattern_manual(values = c(Pre = "none", Post = "stripe")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))

#put all four plots together
all.A <- A.mass + A.elec + A.elon + A.stress
all.A <- all.A + plot_annotation(
  title = "HDPE Thin Film",
  tag_levels = 'A')

#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)


print(all.A)

ggsave(here("output", "HDPE film plot.png"))


#~~~~~~~~~~~~~~~~
#B only
#~~~~~~~~~~~~~~~~

B.plot.data <- plot.data %>%
  filter(PIP.Number == "B")

names(B.plot.data) [1] <- "Time"

#mass only

B.mass.dat <- data_summary(B.plot.data, varname = "weight.std", groupnames = c("Pre.Post", "Depth", "Time"))

#add new column for plotting
B.mass.dat <- B.mass.dat %>%
  mutate(Depth.WG = paste(Depth, Pre.Post))

#reorder
B.mass.dat$Depth.WG <- factor(B.mass.dat$Depth.WG,
                              levels = c("NA Pre", "NA Post",
                                         "0 Pre", "0 Post",
                                         "10 Pre", "10 Post"))

B.mass.dat$Pre.Post <- factor(B.mass.dat$Pre.Post,
                              level = c("Pre", "Post"))


#add pattern
B.mass <- ggplot(B.mass.dat, aes(x = factor(Time), y = weight.std, fill = factor(Depth.WG), pattern = Pre.Post)) +
  geom_bar_pattern(stat = "identity", color = "black", width = 0.8, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = weight.std - se, ymax = weight.std + se), width = 0.2,
                position = position_dodge(0.9)) +
  labs(x = "Time",
       fill = "Depth",
       pattern = "Whale Gut") +
  ylab(expression(paste("Mass " (g ~cm^-2)))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.015)) +
  scale_fill_manual(values = palette2,
                    breaks = c('NA Pre', 'NA Post', '0 Pre', '0 Post',
                               '10 Pre', '10 Post'),
                    labels =c('NA', 'NA', '0m', '0m',
                              '10m', '10m')) +
  scale_pattern_manual(values = c(Pre = "none", Post = "stripe")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))

#electrical only

B.elec.dat <- data_summary(B.plot.data, varname = "Resistance.1k", groupnames = c("Pre.Post", "Depth", "Time"))

#create new column for plotting
B.elec.dat <- B.elec.dat %>%
  mutate(Depth.WG = paste(Depth, Pre.Post))

#reorder
B.elec.dat$Depth.WG <- factor(B.elec.dat$Depth.WG,
                              levels = c("NA Pre", "NA Post",
                                         "0 Pre", "0 Post",
                                         "10 Pre", "10 Post"))

B.elec.dat$Pre.Post <- factor(B.elec.dat$Pre.Post,
                              level = c("Pre", "Post"))


#add pattern
B.elec <- ggplot(B.elec.dat, aes(x = factor(Time), y = Resistance.1k, fill = factor(Depth.WG), pattern = Pre.Post)) +
  geom_bar_pattern(stat = "identity", color = "black", width = 0.8, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = Resistance.1k - se, ymax = Resistance.1k + se), width = 0.2,
                position = position_dodge(0.9)) +
  labs(x = "Time",
       y = "Electrical Resistance (1k)",
       fill = "Depth",
       pattern = "Whale Gut") +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 65)) +
  scale_fill_manual(values = palette2,
                    breaks = c('NA Pre', 'NA Post', '0 Pre', '0 Post',
                               '10 Pre', '10 Post'),
                    labels =c('NA', 'NA', '0m', '0m',
                              '10m', '10m')) +
  scale_pattern_manual(values = c(Pre = "none", Post = "stripe")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))

#Yield Elongation only

B.elon.dat <- data_summary(B.plot.data, varname = "Yield.Strain", groupnames = c("Pre.Post", "Depth", "Time"))

#new column for plotting
B.elon.dat <- B.elon.dat %>%
  mutate(Depth.WG = paste(Depth, Pre.Post))

#reorder
B.elon.dat$Depth.WG <- factor(B.elon.dat$Depth.WG,
                              levels = c("NA Pre", "NA Post",
                                         "0 Pre", "0 Post",
                                         "10 Pre", "10 Post"))

B.elon.dat$Pre.Post <- factor(B.elon.dat$Pre.Post,
                              level = c("Pre", "Post"))


#add pattern
B.elon <- ggplot(B.elon.dat, aes(x = factor(Time), y = Yield.Strain, fill = factor(Depth.WG), pattern = Pre.Post)) +
  geom_bar_pattern(stat = "identity", color = "black", width = 0.8, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = Yield.Strain - se, ymax = Yield.Strain + se), width = 0.2,
                position = position_dodge(0.9)) +
  labs(x = "Time",
       y = "Yield Strain",
       fill = "Depth",
       pattern = "Whale Gut") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, .17)) +
  scale_fill_manual(values = palette2,
                    breaks = c('NA Pre', 'NA Post', '0 Pre', '0 Post',
                               '10 Pre', '10 Post'),
                    labels =c('NA', 'NA', '0m', '0m',
                              '10m', '10m')) +
  scale_pattern_manual(values = c(Pre = "none", Post = "stripe")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))

#Yield Stress only

B.stress.dat <- data_summary(B.plot.data, varname = "Yield.Stress", groupnames = c("Pre.Post", "Depth", "Time"))

#new column for plotting
B.stress.dat <- B.stress.dat %>%
  mutate(Depth.WG = paste(Depth, Pre.Post))

#reorder
B.stress.dat$Depth.WG <- factor(B.stress.dat$Depth.WG,
                                levels = c("NA Pre", "NA Post",
                                           "0 Pre", "0 Post",
                                           "10 Pre", "10 Post"))

B.stress.dat$Pre.Post <- factor(B.stress.dat$Pre.Post,
                                level = c("Pre", "Post"))


#add pattern
B.stress <- ggplot(B.stress.dat, aes(x = factor(Time), y = Yield.Stress, fill = factor(Depth.WG), pattern = Pre.Post)) +
  geom_bar_pattern(stat = "identity", color = "black", width = 0.8, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = Yield.Stress - se, ymax = Yield.Stress + se), width = 0.2,
                position = position_dodge(0.9)) +
  labs(x = "Time",
       y = "Yield Stress (MPa)",
       fill = "Depth",
       pattern = "Whale Gut") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 25)) +
  scale_fill_manual(values = palette2,
                    breaks = c('NA Pre', 'NA Post', '0 Pre', '0 Post',
                               '10 Pre', '10 Post'),
                    labels =c('NA', 'NA', '0m', '0m',
                              '10m', '10m')) +
  scale_pattern_manual(values = c(Pre = "none", Post = "stripe")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))

#put all four plots together
all.B <- B.mass + B.elec + B.elon + B.stress
all.B <- all.B + plot_annotation(
  title = "LDPE Film",
  tag_levels = 'A')

#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)


print(all.B)

ggsave(here("output", "LDPE film plot.png"))


#~~~~~~~~~~~~~~~~
#C only
#~~~~~~~~~~~~~~~~

C.plot.data <- plot.data %>%
  filter(PIP.Number == "C")

names(C.plot.data) [1] <- "Time"

#mass only

C.mass.dat <- data_summary(C.plot.data, varname = "weight.std", groupnames = c("Pre.Post", "Depth", "Time"))

#new column for plotting
C.mass.dat <- C.mass.dat %>%
  mutate(Depth.WG = paste(Depth, Pre.Post))

#reorder
C.mass.dat$Depth.WG <- factor(C.mass.dat$Depth.WG,
                              levels = c("NA Pre", "NA Post",
                                         "0 Pre", "0 Post",
                                         "10 Pre", "10 Post"))

C.mass.dat$Pre.Post <- factor(C.mass.dat$Pre.Post,
                              level = c("Pre", "Post"))


#add pattern
C.mass <- ggplot(C.mass.dat, aes(x = factor(Time), y = weight.std, fill = factor(Depth.WG), pattern = Pre.Post)) +
  geom_bar_pattern(stat = "identity", color = "black", width = 0.8, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = weight.std - se, ymax = weight.std + se), width = 0.2,
                position = position_dodge(0.9)) +
  labs(x = "Time",
       fill = "Depth",
       pattern = "Whale Gut") +
  ylab(expression(paste("Mass " (g ~cm^-2)))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.015)) +
  scale_fill_manual(values = palette2,
                    breaks = c('NA Pre', 'NA Post', '0 Pre', '0 Post',
                               '10 Pre', '10 Post'),
                    labels =c('NA', 'NA', '0m', '0m',
                              '10m', '10m')) +
  scale_pattern_manual(values = c(Pre = "none", Post = "stripe")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))

#electrical only

C.elec.dat <- data_summary(C.plot.data, varname = "Resistance.1k", groupnames = c("Pre.Post", "Depth", "Time"))

#new column for plotting
C.elec.dat <- C.elec.dat %>%
  mutate(Depth.WG = paste(Depth, Pre.Post))

#reorder
C.elec.dat$Depth.WG <- factor(C.elec.dat$Depth.WG,
                              levels = c("NA Pre", "NA Post",
                                         "0 Pre", "0 Post",
                                         "10 Pre", "10 Post"))

C.elec.dat$Pre.Post <- factor(C.elec.dat$Pre.Post,
                              level = c("Pre", "Post"))


#add pattern
C.elec <- ggplot(C.elec.dat, aes(x = factor(Time), y = Resistance.1k, fill = factor(Depth.WG), pattern = Pre.Post)) +
  geom_bar_pattern(stat = "identity", color = "black", width = 0.8, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = Resistance.1k - se, ymax = Resistance.1k + se), width = 0.2,
                position = position_dodge(0.9)) +
  labs(x = "Time",
       y = "Electrical Resistance (1k)",
       fill = "Depth",
       pattern = "Whale Gut") +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 65)) +
  scale_fill_manual(values = palette2,
                    breaks = c('NA Pre', 'NA Post', '0 Pre', '0 Post',
                               '10 Pre', '10 Post'),
                    labels =c('NA', 'NA', '0m', '0m',
                              '10m', '10m')) +
  scale_pattern_manual(values = c(Pre = "none", Post = "stripe")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))

#Yield Elongation only

C.elon.dat <- data_summary(C.plot.data, varname = "Yield.Strain", groupnames = c("Pre.Post", "Depth", "Time"))

#column for plotting
C.elon.dat <- C.elon.dat %>%
  mutate(Depth.WG = paste(Depth, Pre.Post))

#reorder
C.elon.dat$Depth.WG <- factor(C.elon.dat$Depth.WG,
                              levels = c("NA Pre", "NA Post",
                                         "0 Pre", "0 Post",
                                         "10 Pre", "10 Post"))

C.elon.dat$Pre.Post <- factor(C.elon.dat$Pre.Post,
                              level = c("Pre", "Post"))


#add pattern
C.elon <- ggplot(C.elon.dat, aes(x = factor(Time), y = Yield.Strain, fill = factor(Depth.WG), pattern = Pre.Post)) +
  geom_bar_pattern(stat = "identity", color = "black", width = 0.8, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = Yield.Strain - se, ymax = Yield.Strain + se), width = 0.2,
                position = position_dodge(0.9)) +
  labs(x = "Time",
       y = "Yield Strain",
       fill = "Depth",
       pattern = "Whale Gut") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, .17)) +
  scale_fill_manual(values = palette2,
                    breaks = c('NA Pre', 'NA Post', '0 Pre', '0 Post',
                               '10 Pre', '10 Post'),
                    labels =c('NA', 'NA', '0m', '0m',
                              '10m', '10m')) +
  scale_pattern_manual(values = c(Pre = "none", Post = "stripe")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))

#Yield Stress only

C.stress.dat <- data_summary(C.plot.data, varname = "Yield.Stress", groupnames = c("Pre.Post", "Depth", "Time"))

#new column for plotting
C.stress.dat <- C.stress.dat %>%
  mutate(Depth.WG = paste(Depth, Pre.Post))

#reorder
C.stress.dat$Depth.WG <- factor(C.stress.dat$Depth.WG,
                                levels = c("NA Pre", "NA Post",
                                           "0 Pre", "0 Post",
                                           "10 Pre", "10 Post"))

C.stress.dat$Pre.Post <- factor(C.stress.dat$Pre.Post,
                                level = c("Pre", "Post"))


#add pattern
C.stress <- ggplot(C.stress.dat, aes(x = factor(Time), y = Yield.Stress, fill = factor(Depth.WG), pattern = Pre.Post)) +
  geom_bar_pattern(stat = "identity", color = "black", width = 0.8, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = Yield.Stress - se, ymax = Yield.Stress + se), width = 0.2,
                position = position_dodge(0.9)) +
  labs(x = "Time",
       y = "Yield Stress (MPa)",
       fill = "Depth",
       pattern = "Whale Gut") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 25)) +
  scale_fill_manual(values = palette2,
                    breaks = c('NA Pre', 'NA Post', '0 Pre', '0 Post',
                               '10 Pre', '10 Post'),
                    labels =c('NA', 'NA', '0m', '0m',
                              '10m', '10m')) +
  scale_pattern_manual(values = c(Pre = "none", Post = "stripe")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))

#put all four plots together
all.C <- C.mass + C.elec + C.elon + C.stress
all.C <- all.C + plot_annotation(
  title = "PP Chip Bag Film",
  tag_levels = 'A')

#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)


print(all.C)

ggsave(here("output", "PP Chip Bag film plot.png"))


#~~~~~~~~~~~~~~~~
#D only
#~~~~~~~~~~~~~~~~

D.plot.data <- plot.data %>%
  filter(PIP.Number == "D")

names(D.plot.data) [1] <- "Time"

#mass only

D.mass.dat <- data_summary(D.plot.data, varname = "weight.std", groupnames = c("Pre.Post", "Depth", "Time"))

#new column for plotting
D.mass.dat <- D.mass.dat %>%
  mutate(Depth.WG = paste(Depth, Pre.Post))

#reorder
D.mass.dat$Depth.WG <- factor(D.mass.dat$Depth.WG,
                              levels = c("NA Pre", "NA Post",
                                         "0 Pre", "0 Post",
                                         "10 Pre", "10 Post"))

D.mass.dat$Pre.Post <- factor(D.mass.dat$Pre.Post,
                              level = c("Pre", "Post"))


#add pattern
D.mass <- ggplot(D.mass.dat, aes(x = factor(Time), y = weight.std, fill = factor(Depth.WG), pattern = Pre.Post)) +
  geom_bar_pattern(stat = "identity", color = "black", width = 0.8, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = weight.std - se, ymax = weight.std + se), width = 0.2,
                position = position_dodge(0.9)) +
  labs(x = "Time",
       fill = "Depth",
       pattern = "Whale Gut") +
  ylab(expression(paste("Mass " (g ~cm^-2)))) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.015)) +
  scale_fill_manual(values = palette2,
                    breaks = c('NA Pre', 'NA Post', '0 Pre', '0 Post',
                               '10 Pre', '10 Post'),
                    labels =c('NA', 'NA', '0m', '0m',
                              '10m', '10m')) +
  scale_pattern_manual(values = c(Pre = "none", Post = "stripe")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))

#electrical only

D.elec.dat <- data_summary(D.plot.data, varname = "Resistance.1k", groupnames = c("Pre.Post", "Depth", "Time"))

#new column for plotting
D.elec.dat <- D.elec.dat %>%
  mutate(Depth.WG = paste(Depth, Pre.Post))

#reorder
D.elec.dat$Depth.WG <- factor(D.elec.dat$Depth.WG,
                              levels = c("NA Pre", "NA Post",
                                         "0 Pre", "0 Post",
                                         "10 Pre", "10 Post"))

D.elec.dat$Pre.Post <- factor(D.elec.dat$Pre.Post,
                              level = c("Pre", "Post"))


#add pattern
D.elec <- ggplot(D.elec.dat, aes(x = factor(Time), y = Resistance.1k, fill = factor(Depth.WG), pattern = Pre.Post)) +
  geom_bar_pattern(stat = "identity", color = "black", width = 0.8, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = Resistance.1k - se, ymax = Resistance.1k + se), width = 0.2,
                position = position_dodge(0.9)) +
  labs(x = "Time",
       y = "Electrical Resistance (1k)",
       fill = "Depth",
       pattern = "Whale Gut") +
  theme_bw() +
  theme(legend.position = "right",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 65)) +
  scale_fill_manual(values = palette2,
                    breaks = c('NA Pre', 'NA Post', '0 Pre', '0 Post',
                               '10 Pre', '10 Post'),
                    labels =c('NA', 'NA', '0m', '0m',
                              '10m', '10m')) +
  scale_pattern_manual(values = c(Pre = "none", Post = "stripe")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))

#Yield Elongation only

D.elon.dat <- data_summary(D.plot.data, varname = "Yield.Strain", groupnames = c("Pre.Post", "Depth", "Time"))

#new column for plotting
D.elon.dat <- D.elon.dat %>%
  mutate(Depth.WG = paste(Depth, Pre.Post))

#reorder
D.elon.dat$Depth.WG <- factor(D.elon.dat$Depth.WG,
                              levels = c("NA Pre", "NA Post",
                                         "0 Pre", "0 Post",
                                         "10 Pre", "10 Post"))

D.elon.dat$Pre.Post <- factor(D.elon.dat$Pre.Post,
                              level = c("Pre", "Post"))

#add pattern
D.elon <- ggplot(D.elon.dat, aes(x = factor(Time), y = Yield.Strain, fill = factor(Depth.WG), pattern = Pre.Post)) +
  geom_bar_pattern(stat = "identity", color = "black", width = 0.8, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = Yield.Strain - se, ymax = Yield.Strain + se), width = 0.2,
                position = position_dodge(0.9)) +
  labs(x = "Time",
       y = "Yield Strain",
       fill = "Depth",
       pattern = "Whale Gut") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, .17)) +
  scale_fill_manual(values = palette2,
                    breaks = c('NA Pre', 'NA Post', '0 Pre', '0 Post',
                               '10 Pre', '10 Post'),
                    labels =c('NA', 'NA', '0m', '0m',
                              '10m', '10m')) +
  scale_pattern_manual(values = c(Pre = "none", Post = "stripe")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))

#Yield Stress only

D.stress.dat <- data_summary(D.plot.data, varname = "Yield.Stress", groupnames = c("Pre.Post", "Depth", "Time"))

#new column for plotting
D.stress.dat <- D.stress.dat %>%
  mutate(Depth.WG = paste(Depth, Pre.Post))

#reorder
D.stress.dat$Depth.WG <- factor(D.stress.dat$Depth.WG,
                                levels = c("NA Pre", "NA Post",
                                           "0 Pre", "0 Post",
                                           "10 Pre", "10 Post"))

D.stress.dat$Pre.Post <- factor(D.stress.dat$Pre.Post,
                                level = c("Pre", "Post"))

#add pattern
D.stress <- ggplot(D.stress.dat, aes(x = factor(Time), y = Yield.Stress, fill = factor(Depth.WG), pattern = Pre.Post)) +
  geom_bar_pattern(stat = "identity", color = "black", width = 0.8, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = Yield.Stress - se, ymax = Yield.Stress + se), width = 0.2,
                position = position_dodge(0.9)) +
  labs(x = "Time",
       y = "Yield Stress (MPa)",
       fill = "Depth",
       pattern = "Whale Gut") +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 25)) +
  scale_fill_manual(values = palette2,
                    breaks = c('NA Pre', 'NA Post', '0 Pre', '0 Post',
                               '10 Pre', '10 Post'),
                    labels =c('NA', 'NA', '0m', '0m',
                              '10m', '10m')) +
  scale_pattern_manual(values = c(Pre = "none", Post = "stripe")) +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))

#put all four plots together
all.D <- D.mass + D.elec + D.elon + D.stress
all.D <- all.D + plot_annotation(
  title = "PE Poly Bag Film",
  tag_levels = 'A')

#Turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)


print(all.D)

ggsave(here("output", "PE Poly Bag film plot.png"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BPA Plots
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##############################################################################
#ELISA Analysis
##############################################################################

#change column names
names(elisa) [2] <- "Sample.ID"
names(elisa) [3] <- "Time"

#remove other controls
elisa.dat <- elisa %>%
  filter(Sample.ID %in% c(1:8, "P", "N"))

names(elisa.dat) [1] <- "Depth"

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

