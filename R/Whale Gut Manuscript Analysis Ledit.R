## This script performs statistics on the TOM FORD Plastic Innovation Prize (2023)
# FOR MANUSCRIPT PUBLICATION

#load libraries
library(plyr)
library(dplyr)
library(vegan)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(car) #looking at qq plot
library(nlme)
library(pwr)
library(lmerTest) #mixed effects model
library(emmeans) #contrasts
#library(here)
#library(patchwork)
#library(tidyverse)
# library(lubridate) #date changes
# library(MetBrewer) #color palette
# library(ggbreak) #break y scale
# library(corrr) #PCA
# library(factoextra) #PCA
# library(FactoMineR) #PCA
# library(vegan) #NMDS
#library(pwr) #power analysis

## import data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Tensile data has a lot of the data, 
#but specifically we just want the tensile replicates present here 
#(2 replicates for each - not present in lab_data)
#Pre-Post is organized into 1 column (so 6 replicates for each)
tensile <- read.csv("Tensile_1.csv", head = TRUE) 

#Lab data has all data (including size) but does not have tensile replicates
#Pre-post is labeled in several columns with each variable
lab <- read.csv("Lab_data_1.csv", head = TRUE)

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

str(merged_df)
summary(merged_df)

#create columns with standardized weights for all the lab dataset variables
#weight (pre and post) / size (cm2) = g/cm2

head(merged_df)
merged_df$weight.std <- merged_df$Weight / merged_df$Size.for.weight.cm.2

#############################################
############################################
#mean for all sample type weights
#I double checked that this is the same as before the merge. All good!
merged_df %>%
  filter(Time == 0) %>%
  filter(Pre.Post == "Pre") %>%
  group_by(PIP.Number) %>%
  summarize(
    Mean_Weight = mean(Weight, na.rm = TRUE),
    SE_Weight = sd(Weight, na.rm = TRUE) / sqrt(n()),  # Standard error for Weight
    Mean_Yield = mean(Yield.Elongation, na.rm = TRUE),
    SE_Yield = sd(Yield.Elongation, na.rm = TRUE) / sqrt(n()),  # Standard error for Yield Elongation
    Mean_Stress = mean(Yield.Stress, na.rm = TRUE),
    SE_Stress = sd(Yield.Stress, na.rm = TRUE) / sqrt(n()),  # Standard error for Stress
    Mean_Resistance_1k = mean(Resistance.1k, na.rm = TRUE),
    SE_Resistance_1k = sd(Resistance.1k, na.rm = TRUE) / sqrt(n())  # Standard error for Resistance.1k
  )

#I think I need to do an ANOVA without t = 0, then do T-tests comparing variables to control

#Create data frame without t = 0
merged_df_test <- merged_df  %>%
  filter(Time != 0)
str(merged_df_test)
merged_df_test$Depth <- as.factor(merged_df_test$Depth)

#####
# A - HDPE
#tensile df are 2 lower....
A.wut <- merged_df_test[merged_df_test$PIP.Number == "A",]
#A-4, depth 10, 8mo, post doesnt have tensile, and not in original data set for tensile - too broken

#Mass
A.mass.aov <- aov(weight.std ~ Depth * Time * Pre.Post, data = merged_df_test[merged_df_test$PIP.Number == "A",])
summary(A.mass.aov)
TukeyHSD(A.mass.aov, conf.level = 0.95)

#Yield.Elongation
A.elong.aov <- aov(Yield.Elongation ~ Depth * Time * Pre.Post, data = merged_df_test[merged_df_test$PIP.Number == "A",])
summary(A.elong.aov)
TukeyHSD(A.elong.aov, conf.level = 0.95)

#Yield.Stress
A.stress.aov <- aov(Yield.Stress ~ Depth * Time * Pre.Post, data = merged_df_test[merged_df_test$PIP.Number == "A",])
summary(A.stress.aov)
TukeyHSD(A.stress.aov, conf.level = 0.95)


#electrical, only with 1k
A.elec.aov <- aov(Resistance.1k ~ Depth * Time * Pre.Post, data = merged_df_test[merged_df_test$PIP.Number == "A",])
summary(A.elec.aov)
TukeyHSD(A.elec.aov, conf.level = 0.95)


#####
# B - LDPE
B.wut <- merged_df_test[merged_df_test$PIP.Number == "B",]
#B-4, depth 10, 8mo, pre doesnt have tensile, and not in original data set for tensile - too broken
#B-12, depth 0, 4mo, post doesnt have tensile, and not in original data set for tensile - too broken
#missing depth 10, 8mo, pre; depth 10, 8mo, post - both are gone

#Mass
B.mass.aov <- aov(weight.std ~ Depth * Time * Pre.Post, data = merged_df_test[merged_df_test$PIP.Number == "B",])
summary(B.mass.aov)
TukeyHSD(B.mass.aov, conf.level = 0.95)

#Yield.Elongation
B.elong.aov <- aov(Yield.Elongation ~ Depth * Time * Pre.Post, data = merged_df_test[merged_df_test$PIP.Number == "B",])
summary(B.elong.aov)
TukeyHSD(B.elong.aov, conf.level = 0.95)

#Yield.Stress
B.stress.aov <- aov(Yield.Stress ~ Depth * Time * Pre.Post, data = merged_df_test[merged_df_test$PIP.Number == "B",])
summary(B.stress.aov)
TukeyHSD(B.stress.aov, conf.level = 0.95)

#electrical, only with 1k
B.elec.aov <- aov(Resistance.1k ~ Depth * Time * Pre.Post, data = merged_df_test[merged_df_test$PIP.Number == "B",])
summary(B.elec.aov)
TukeyHSD(B.elec.aov, conf.level = 0.95)



#####
# C = PP chip bag
#####
C.wut <- merged_df_test[merged_df_test$PIP.Number == "C",]
#B-2, depth 10, 8mo, post doesnt have tensile, and not in original data set for tensile - too broken

#Mass
C.mass.aov <- aov(weight.std ~ Depth * Time * Pre.Post, data = merged_df_test[merged_df_test$PIP.Number == "C",])
summary(C.mass.aov)
TukeyHSD(C.mass.aov, conf.level = 0.95)

#Yield.Elongation
C.elong.aov <- aov(Yield.Elongation ~ Depth * Time * Pre.Post, data = merged_df_test[merged_df_test$PIP.Number == "C",])
summary(C.elong.aov)
TukeyHSD(C.elong.aov, conf.level = 0.95)

#Yield.Stress
C.stress.aov <- aov(Yield.Stress ~ Depth * Time * Pre.Post, data = merged_df_test[merged_df_test$PIP.Number == "C",])
summary(C.stress.aov)
TukeyHSD(C.stress.aov, conf.level = 0.95)

#electrical, only with 1k
C.elec.aov <- aov(Resistance.1k ~ Depth * Time * Pre.Post, data = merged_df_test[merged_df_test$PIP.Number == "C",])
summary(C.elec.aov)
TukeyHSD(C.elec.aov, conf.level = 0.95)



#####
# D only
#####
D.wut <- merged_df_test[merged_df_test$PIP.Number == "D",]

#Mass
D.mass.aov <- aov(weight.std ~ Depth * Time * Pre.Post, data = merged_df_test[merged_df_test$PIP.Number == "D",])
summary(D.mass.aov)
TukeyHSD(D.mass.aov, conf.level = 0.95)

#Yield.Elongation
D.elong.aov <- aov(Yield.Elongation ~ Depth * Time * Pre.Post, data = merged_df_test[merged_df_test$PIP.Number == "D",])
summary(D.elong.aov)
TukeyHSD(D.elong.aov, conf.level = 0.95)

#Yield.Stress
D.stress.aov <- aov(Yield.Stress ~ Depth * Time * Pre.Post, data = merged_df_test[merged_df_test$PIP.Number == "D",])
summary(D.stress.aov)
TukeyHSD(D.stress.aov, conf.level = 0.95)

#electrical, only with 1k
D.elec.aov <- aov(Resistance.1k ~ Depth * Time * Pre.Post, data = merged_df_test[merged_df_test$PIP.Number == "D",])
summary(D.elec.aov)
TukeyHSD(D.elec.aov, conf.level = 0.95)


#####
# N - HDPE thick
#####
N.wut <- merged_df_test[merged_df_test$PIP.Number == "N",]
#N-17, depth 0, 4mo, post doesnt have tensile, and not in original data set for tensile - too broken
#missing depth 0, 4mo, post; depth 0, 4mo, pre - both are gone


#Mass
N.mass.aov <- aov(weight.std ~ Depth * Time * Pre.Post, data = merged_df_test[merged_df_test$PIP.Number == "N",])
summary(N.mass.aov)
TukeyHSD(N.mass.aov, conf.level = 0.95)

#Yield.Elongation
N.elong.aov <- aov(Yield.Elongation ~ Depth * Time * Pre.Post, data = merged_df_test[merged_df_test$PIP.Number == "N",])
summary(N.elong.aov)
TukeyHSD(N.elong.aov, conf.level = 0.95)

#Yield.Stress
N.stress.aov <- aov(Yield.Stress ~ Depth * Time * Pre.Post, data = merged_df_test[merged_df_test$PIP.Number == "N",])
summary(N.stress.aov)
TukeyHSD(N.stress.aov, conf.level = 0.95)

#electrical, only with 1k
N.elec.aov <- aov(Resistance.1k ~ Depth * Time * Pre.Post, data = merged_df_test[merged_df_test$PIP.Number == "N",])
summary(N.elec.aov)
TukeyHSD(N.elec.aov, conf.level = 0.95)











##########################################
#add all ratio values into one table
##########################################

#####
#A
#####
A.ratios <- cbind(dat.A.2$Time, dat.A.2$Pre.Post, dat.A.2$Depth, dat.A.mass$NWeight, dat.A.Elon$NElon, dat.A.Stress$NStress, dat.A.elec$NElec)
colnames(A.ratios) = c("Time", "Pre.Post", "Depth", "Weight", "Yield.Elongation", "Yield.Stress", "Resistance.1k")


#turn to numeric data
A.ratios <- transform(A.ratios, Weight = as.numeric(Weight),
          Yield.Elongation = as.numeric(Yield.Elongation),
          Yield.Stress = as.numeric(Yield.Stress),
          Resistance.1k = as.numeric(Resistance.1k))


#shorten to 2 decimal places
A.ratios <- A.ratios %>%
  mutate_at(vars(Weight, Yield.Elongation, Yield.Stress, Resistance.1k), funs(round(., 1)))


#export as csv
write.table(A.ratios, file = "C:/Users/olsena/OneDrive - Seattle Aquarium/Clean Seas/PIP/Seattle_Aquarium_WhaleGut/output\\A.ratios.csv", sep = ",", row.names = FALSE)

#####
# B
####
B.ratios <- cbind(dat.B.2$Time, dat.B.2$Pre.Post, dat.B.2$Depth, dat.B.mass$NWeight, dat.B.Elon$NElon, dat.B.Stress$NStress, dat.B.elec$NElec)
colnames(B.ratios) = c("Time", "Pre.Post", "Depth", "Weight", "Yield.Elongation", "Yield.Stress", "Resistance.1k")


#turn to numeric data
B.ratios <- transform(B.ratios, Weight = as.numeric(Weight),
                      Yield.Elongation = as.numeric(Yield.Elongation),
                      Yield.Stress = as.numeric(Yield.Stress),
                      Resistance.1k = as.numeric(Resistance.1k))


#shorten to 2 decimal places
B.ratios <- B.ratios %>%
  mutate_at(vars(Weight, Yield.Elongation, Yield.Stress, Resistance.1k), funs(round(., 1)))


#export as csv
write.table(B.ratios, file = "C:/Users/olsena/OneDrive - Seattle Aquarium/Clean Seas/PIP/Seattle_Aquarium_WhaleGut/output\\B.ratios.csv", sep = ",", row.names = FALSE)


#####
# C
####
C.ratios <- cbind(dat.C.2$Time, dat.C.2$Pre.Post, dat.C.2$Depth, dat.C.mass$NWeight, dat.C.Elon$NElon, dat.C.Stress$NStress, dat.C.elec$NElec)
colnames(C.ratios) = c("Time", "Pre.Post", "Depth", "Weight", "Yield.Elongation", "Yield.Stress", "Resistance.1k")


#turn to numeric data
C.ratios <- transform(C.ratios, Weight = as.numeric(Weight),
                      Yield.Elongation = as.numeric(Yield.Elongation),
                      Yield.Stress = as.numeric(Yield.Stress),
                      Resistance.1k = as.numeric(Resistance.1k))


#shorten to 2 decimal places
C.ratios <- C.ratios %>%
  mutate_at(vars(Weight, Yield.Elongation, Yield.Stress, Resistance.1k), funs(round(., 1)))


#export as csv
write.table(C.ratios, file = "C:/Users/olsena/OneDrive - Seattle Aquarium/Clean Seas/PIP/Seattle_Aquarium_WhaleGut/output\\C.ratios.csv", sep = ",", row.names = FALSE)



#####
# D
####
D.ratios <- cbind(dat.D.2$Time, dat.D.2$Pre.Post, dat.D.2$Depth, dat.D.mass$NWeight, dat.D.Elon$NElon, dat.D.Stress$NStress, dat.D.elec$NElec)
colnames(D.ratios) = c("Time", "Pre.Post", "Depth", "Weight", "Yield.Elongation", "Yield.Stress", "Resistance.1k")


#turn to numeric data
D.ratios <- transform(D.ratios, Weight = as.numeric(Weight),
                      Yield.Elongation = as.numeric(Yield.Elongation),
                      Yield.Stress = as.numeric(Yield.Stress),
                      Resistance.1k = as.numeric(Resistance.1k))


#shorten to 2 decimal places
D.ratios <- D.ratios %>%
  mutate_at(vars(Weight, Yield.Elongation, Yield.Stress, Resistance.1k), funs(round(., 1)))


#export as csv
write.table(D.ratios, file = "C:/Users/olsena/OneDrive - Seattle Aquarium/Clean Seas/PIP/Seattle_Aquarium_WhaleGut/output\\D.ratios.csv", sep = ",", row.names = FALSE)


#####
# N
####
N.ratios <- cbind(dat.N.2$Time, dat.N.2$Pre.Post, dat.N.2$Depth, dat.N.mass$NWeight, dat.N.Elon$NElon, dat.N.Stress$NStress, dat.N.elec$NElec)
colnames(N.ratios) = c("Time", "Pre.Post", "Depth", "Weight", "Yield.Elongation", "Yield.Stress", "Resistance.1k")


#turn to numeric data
N.ratios <- transform(N.ratios, Weight = as.numeric(Weight),
                      Yield.Elongation = as.numeric(Yield.Elongation),
                      Yield.Stress = as.numeric(Yield.Stress),
                      Resistance.1k = as.numeric(Resistance.1k))


#shorten to 2 decimal places
N.ratios <- N.ratios %>%
  mutate_at(vars(Weight, Yield.Elongation, Yield.Stress, Resistance.1k), funs(round(., 1)))


#export as csv
write.table(N.ratios, file = "C:/Users/olsena/OneDrive - Seattle Aquarium/Clean Seas/PIP/Seattle_Aquarium_WhaleGut/output\\N.ratios.csv", sep = ",", row.names = FALSE)
