## This script performs statistics on the TOM FORD Plastic Innovation Prize (2023)
# FOR MANUSCRIPT PUBLICATION


## set up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#load libraries
library(here)
# library(patchwork)
library(tidyverse)
# library(lubridate) #date changes
# library(MetBrewer) #color palette
# library(ggbreak) #break y scale
# library(corrr) #PCA
# library(factoextra) #PCA
# library(FactoMineR) #PCA
# library(vegan) #NMDS
library(pwr) #power analysis


#check to see that here() leads to root directory or top-level folder
#should be Seattle_Aquarium_CleanSeas_PIP
here()

#if incorrect, enter `set_here(path = "...")`
#restart RStudio
#check here() again to make sure you're in the right top-level folder



## import data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#all data in one file (P, N, A, B, C, D)
original.data <- read_csv(here("data", "All PIP Data.csv"))

#remove added last column
original.data <- original.data[-c(28)]

## clean data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data <- original.data
names(data) [2] <- "Pre.Post"
names(data) [4] <- "PIP.Number"
names(data) [14] <- "Tensile.Stress.Break"
names(data) [16] <- "Tensile.Stress.Maximum.Force"
names(data) [17] <- "Tensile.Strain.Maximum.Force"
names(data) [18] <- "Energy.Yield"
names(data) [19] <- "Energy.Break"


str(data)



############################################
############################################
# Power Analysis
############################################
############################################

cohen.ES(test = "t", size = "small") #0.2
cohen.ES(test = "t", size = "medium") #0.5
cohen.ES(test = "t", size = "large") #0.8

#when trying to calculate power with known sample size (n = 76, power = NULL)
t.power <- pwr.t.test(n = 76,
                      d = 0.3,
                      sig.level = .05,
                      power = NULL,
                      type = "paired",
                      alternative = "two.sided")

t.power

#when trying to calculate sample size with power of 0.8 (n = NULL, power = 0.8)
t.size <- pwr.t.test(n = NULL,
                     d = 0.3,
                     sig.level = .05,
                     power = 0.8,
                     type = "paired",
                     alternative = "two.sided")

t.size


#how many are classified Pre versus Post
data %>% count(Pre.Post)

#calculate power with sample size of 3
t.3 <- pwr.t.test(n = 3,
                  d = 0.5,
                  sig.level = .05,
                  power = NULL,
                  type = "paired",
                  alternative = "two.sided")

t.3

#power for d = 0.2 is 0.056
#power for d = 0.3 is 0.062
#power for d = 0.3 is 0.084



############################################
############################################
# ANOVAs
############################################
############################################

#####
# A only
#####

#all data
dat.A <- data %>%
  filter(PIP.Number == "A")

#remove time 0
dat.A.2 <- dat.A %>%
  filter(Time != 0)

#calculate time 0 mean
dat.A %>%
  filter(Time == 0) %>%
  filter(Pre.Post == "Pre") %>%
  select(Weight, Yield.Elongation, Yield.Stress, Resistance.1k) %>%
  summarize_at(c('Weight', 'Yield.Elongation', 'Yield.Stress', 'Resistance.1k'), mean)

#####
#mass
#####

#select data
dat.A.mass <- dat.A.2 %>%
  select(Time, Pre.Post, Depth, Weight)

#normalize by dividing all by time 0 mean 0.723
dat.A.mass <- dat.A.mass %>%
  mutate(NWeight = Weight/0.723)

#three way anova
A.mass.aov <- aov(NWeight ~ Depth * Time * Pre.Post, data = dat.A.mass)
summary(A.mass.aov)


#####
#Yield.Elongation
#####
dat.A.Elon <- dat.A.2 %>%
  select(Time, Pre.Post, Depth, Yield.Elongation)

#normalize by dividing all by time 0 mean 1.7
dat.A.Elon <- dat.A.Elon %>%
  mutate(NElon = Yield.Elongation/1.7)

#three way anova
A.Elon.aov <- aov(NElon ~ Depth * Time * Pre.Post, data = dat.A.Elon)
summary(A.Elon.aov)


#####
#Yield.Stress
#####
dat.A.Stress <- dat.A.2 %>%
  select(Time, Pre.Post, Depth, Yield.Stress)

#normalize by dividing all by time 0 mean 4.12
dat.A.Stress <- dat.A.Stress %>%
  mutate(NStress = Yield.Stress/4.12)

#three way anova
A.Stress.aov <- aov(Yield.Stress ~ Depth * Time * Pre.Post, data = dat.A.Stress)
summary(A.Stress.aov)


#####
#electrical, select only one frequency (1000 Hz)
#####
dat.A.elec <- dat.A.2 %>%
  select(Time, Pre.Post, Depth, Resistance.1k)

#normalize by dividing all by time 0 mean 21
dat.A.elec <- dat.A.elec %>%
  mutate(NElec = Resistance.1k/21)

#three way anova
A.elec.aov <- aov(Resistance.1k ~ Depth * Time * Pre.Post, data = dat.A.elec)
summary(A.elec.aov)







#####
# B only
#####

#all data
dat.B <- data %>%
  filter(PIP.Number == "B")

#remove time 0
dat.B.2 <- dat.B %>%
  filter(Time != 0)

#calculate time 0 mean
dat.B %>%
  filter(Time == 0) %>%
  filter(Pre.Post == "Pre") %>%
  select(Weight, Yield.Elongation, Yield.Stress, Resistance.1k) %>%
  summarize_at(c('Weight', 'Yield.Elongation', 'Yield.Stress', 'Resistance.1k'), mean)

#####
#mass
#####

#select data
dat.B.mass <- dat.B.2 %>%
  select(Time, Pre.Post, Depth, Weight)

#normalize by dividing all by time 0 mean 0.422
dat.B.mass <- dat.B.mass %>%
  mutate(NWeight = Weight/0.422)

#three way anova
B.mass.aov <- aov(NWeight ~ Depth * Time * Pre.Post, data = dat.B.mass)
summary(B.mass.aov)


#####
#Yield.Elongation
#####
dat.B.Elon <- dat.B.2 %>%
  select(Time, Pre.Post, Depth, Yield.Elongation)

#normalize by dividing all by time 0 mean 2.03
dat.B.Elon <- dat.B.Elon %>%
  mutate(NElon = Yield.Elongation/2.03)

#three way anova
B.Elon.aov <- aov(NElon ~ Depth * Time * Pre.Post, data = dat.B.Elon)
summary(B.Elon.aov)


#####
#Yield.Stress
#####
dat.B.Stress <- dat.B.2 %>%
  select(Time, Pre.Post, Depth, Yield.Stress)

#normalize by dividing all by time 0 mean 5.44
dat.B.Stress <- dat.B.Stress %>%
  mutate(NStress = Yield.Stress/5.44)

#three way anova
B.Stress.aov <- aov(Yield.Stress ~ Depth * Time * Pre.Post, data = dat.B.Stress)
summary(B.Stress.aov)


#####
#electrical, select only one frequency (1000 Hz)
#####
dat.B.elec <- dat.B.2 %>%
  select(Time, Pre.Post, Depth, Resistance.1k)

#normalize by dividing all by time 0 mean 10.6
dat.B.elec <- dat.B.elec %>%
  mutate(NElec = Resistance.1k/10.6)

#three way anova
B.elec.aov <- aov(Resistance.1k ~ Depth * Time * Pre.Post, data = dat.B.elec)
summary(B.elec.aov)





#####
# C only
#####

#all data
dat.C <- data %>%
  filter(PIP.Number == "C")

#remove time 0
dat.C.2 <- dat.C %>%
  filter(Time != 0)

#calculate time 0 mean
dat.C %>%
  filter(Time == 0) %>%
  filter(Pre.Post == "Pre") %>%
  select(Weight, Yield.Elongation, Yield.Stress, Resistance.1k) %>%
  summarize_at(c('Weight', 'Yield.Elongation', 'Yield.Stress', 'Resistance.1k'), mean)

#####
#mass
#####

#select data
dat.C.mass <- dat.C.2 %>%
  select(Time, Pre.Post, Depth, Weight)

#normalize by dividing all by time 0 mean 0.116
dat.C.mass <- dat.C.mass %>%
  mutate(NWeight = Weight/0.116)

#three way anova
C.mass.aov <- aov(NWeight ~ Depth * Time * Pre.Post, data = dat.C.mass)
summary(C.mass.aov)


#####
#Yield.Elongation
#####
dat.C.Elon <- dat.C.2 %>%
  select(Time, Pre.Post, Depth, Yield.Elongation)

#normalize by dividing all by time 0 mean 3.24
dat.C.Elon <- dat.C.Elon %>%
  mutate(NElon = Yield.Elongation/3.24)

#three way anova
C.Elon.aov <- aov(NElon ~ Depth * Time * Pre.Post, data = dat.C.Elon)
summary(C.Elon.aov)


#####
#Yield.Stress
#####
dat.C.Stress <- dat.C.2 %>%
  select(Time, Pre.Post, Depth, Yield.Stress)

#normalize by dividing all by time 0 mean 18.9
dat.C.Stress <- dat.C.Stress %>%
  mutate(NStress = Yield.Stress/18.9)

#three way anova
C.Stress.aov <- aov(Yield.Stress ~ Depth * Time * Pre.Post, data = dat.C.Stress)
summary(C.Stress.aov)


#####
#electrical, select only one frequency (1000 Hz)
#####
dat.C.elec <- dat.C.2 %>%
  select(Time, Pre.Post, Depth, Resistance.1k)

#normalize by dividing all by time 0 mean 20.7
dat.C.elec <- dat.C.elec %>%
  mutate(NElec = Resistance.1k/20.7)

#three way anova
C.elec.aov <- aov(Resistance.1k ~ Depth * Time * Pre.Post, data = dat.C.elec)
summary(C.elec.aov)





#####
# D only
#####

#all data
dat.D <- data %>%
  filter(PIP.Number == "D")

#remove time 0
dat.D.2 <- dat.D %>%
  filter(Time != 0)

#calculate time 0 mean
dat.D %>%
  filter(Time == 0) %>%
  filter(Pre.Post == "Pre") %>%
  select(Weight, Yield.Elongation, Yield.Stress, Resistance.1k) %>%
  summarize_at(c('Weight', 'Yield.Elongation', 'Yield.Stress', 'Resistance.1k'), mean)

#####
#mass
#####

#select data
dat.D.mass <- dat.D.2 %>%
  select(Time, Pre.Post, Depth, Weight)

#normalize by dividing all by time 0 mean 0.498
dat.D.mass <- dat.D.mass %>%
  mutate(NWeight = Weight/0.498)

#three way anova
D.mass.aov <- aov(NWeight ~ Depth * Time * Pre.Post, data = dat.D.mass)
summary(D.mass.aov)


#####
#Yield.Elongation
#####
dat.D.Elon <- dat.D.2 %>%
  select(Time, Pre.Post, Depth, Yield.Elongation)

#normalize by dividing all by time 0 mean 2.47
dat.D.Elon <- dat.D.Elon %>%
  mutate(NElon = Yield.Elongation/2.47)

#three way anova
D.Elon.aov <- aov(NElon ~ Depth * Time * Pre.Post, data = dat.D.Elon)
summary(D.Elon.aov)


#####
#Yield.Stress
#####
dat.D.Stress <- dat.D.2 %>%
  select(Time, Pre.Post, Depth, Yield.Stress)

#normalize by dividing all by time 0 mean 4.01
dat.D.Stress <- dat.D.Stress %>%
  mutate(NStress = Yield.Stress/4.01)

#three way anova
D.Stress.aov <- aov(Yield.Stress ~ Depth * Time * Pre.Post, data = dat.D.Stress)
summary(D.Stress.aov)


#####
#electrical, select only one frequency (1000 Hz)
#####
dat.D.elec <- dat.D.2 %>%
  select(Time, Pre.Post, Depth, Resistance.1k)

#normalize by dividing all by time 0 mean 20
dat.D.elec <- dat.D.elec %>%
  mutate(NElec = Resistance.1k/20)

#three way anova
D.elec.aov <- aov(Resistance.1k ~ Depth * Time * Pre.Post, data = dat.D.elec)
summary(D.elec.aov)




#####
# N only
#####

#all data
dat.N <- data %>%
  filter(PIP.Number == "N")

#remove time 0
dat.N.2 <- dat.N %>%
  filter(Time != 0)

#calculate time 0 mean
dat.N %>%
  filter(Time == 0) %>%
  filter(Pre.Post == "Pre") %>%
  select(Weight, Yield.Elongation, Yield.Stress, Resistance.1k) %>%
  summarize_at(c('Weight', 'Yield.Elongation', 'Yield.Stress', 'Resistance.1k'), mean)

#####
#mass
#####

#select data
dat.N.mass <- dat.N.2 %>%
  select(Time, Pre.Post, Depth, Weight)

#normalize by dividing all by time 0 mean 0.754
dat.N.mass <- dat.N.mass %>%
  mutate(NWeight = Weight/0.754)

#three way anova
N.mass.aov <- aov(NWeight ~ Depth * Time * Pre.Post, data = dat.N.mass)
summary(N.mass.aov)


#####
#Yield.Elongation
#####
dat.N.Elon <- dat.N.2 %>%
  select(Time, Pre.Post, Depth, Yield.Elongation)

#normalize by dividing all by time 0 SINGLE SAMPLE 1.38
dat.N.Elon <- dat.N.Elon %>%
  mutate(NElon = Yield.Elongation/1.38)

#three way anova
N.Elon.aov <- aov(NElon ~ Depth * Time * Pre.Post, data = dat.N.Elon)
summary(N.Elon.aov)


#####
#Yield.Stress
#####
dat.N.Stress <- dat.N.2 %>%
  select(Time, Pre.Post, Depth, Yield.Stress)

#normalize by dividing all by time 0 SINGLE SAMPLE 3.47
dat.N.Stress <- dat.N.Stress %>%
  mutate(NStress = Yield.Stress/3.47)

#three way anova
N.Stress.aov <- aov(Yield.Stress ~ Depth * Time * Pre.Post, data = dat.N.Stress)
summary(N.Stress.aov)


#####
#electrical, select only one frequency (1000 Hz)
#####
dat.N.elec <- dat.N.2 %>%
  select(Time, Pre.Post, Depth, Resistance.1k)

#normalize by dividing all by time 0 mean 23.5
dat.N.elec <- dat.N.elec %>%
  mutate(NElec = Resistance.1k/23.5)

#three way anova
N.elec.aov <- aov(Resistance.1k ~ Depth * Time * Pre.Post, data = dat.N.elec)
summary(N.elec.aov)




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
