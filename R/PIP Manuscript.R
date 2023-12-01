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

dat.A <- data %>% 
  filter(PIP.Number == "A") 

#mass
#three way anova
dat.A.mass <- dat.A %>%
  select(Time, Pre.Post, Depth, Weight)

A.mass.aov <- aov(Weight ~ Depth * Time * Pre.Post, data = dat.A.mass)
summary(A.mass.aov)

#tukeys post hoc test
TukeyHSD(A.mass.aov, conf.level = 0.95)
#only shows Pre.Post and not significant

#one way anova
A.depth.aov <- aov(Weight ~ Depth, data = dat.A.mass)
summary(A.depth.aov)

#bonferroni post hoc
pairwise.t.test(dat.A.mass$Weight, dat.A.mass$Depth, p.adj = 'bonferroni')

#energy.break
dat.A.energy <- dat.A %>%
  select(Time, Pre.Post, Depth, Energy.Break)

A.energy.aov <- aov(Energy.Break ~ Depth * Time * Pre.Post, data = dat.A.energy)
summary(A.energy.aov)

#bonferroni post hoc
pairwise.t.test(dat.A.energy$Energy.Break, dat.A.energy$Time, p.adj = 'bonferroni')


#electrical, select only one frequency (1000 Hz)
dat.A.elec <- dat.A %>%
  select(Time, Pre.Post, Depth, Resistance.1k)

A.elec.aov <- aov(Resistance.1k ~ Depth * Time * Pre.Post, data = dat.A.elec)
summary(A.elec.aov)


#bonferroni post hoc
pairwise.t.test(dat.A.elec$Resistance.1k, dat.A.elec$Time, p.adj = 'bonferroni')

pairwise.t.test(dat.A.elec$Resistance.1k, dat.A.elec$Depth, p.adj = 'bonferroni')

#####
# B only
#####

dat.B <- data %>% 
  filter(PIP.Number == "B") 

#mass
dat.B.mass <- dat.B %>%
  select(Time, Pre.Post, Depth, Weight)

B.mass.aov <- aov(Weight ~ Depth * Time * Pre.Post, data = dat.B.mass)
summary(B.mass.aov)


#energy.break
dat.B.energy <- dat.B %>%
  select(Time, Pre.Post, Depth, Energy.Break)

B.energy.aov <- aov(Energy.Break ~ Depth * Time * Pre.Post, data = dat.B.energy)
summary(B.energy.aov)


#electrical, select only one frequency (1000 Hz)
dat.B.elec <- dat.B %>%
  select(Time, Pre.Post, Depth, Resistance.1k)

B.elec.aov <- aov(Resistance.1k ~ Depth * Time * Pre.Post, data = dat.B.elec)
summary(B.elec.aov)


pairwise.t.test(dat.B.elec$Resistance.1k, dat.B.elec$Depth, p.adj = 'bonferroni')


pairwise.t.test(dat.B.elec$Resistance.1k, dat.B.elec$Time, p.adj = 'bonferroni')

#####
# C only
#####

dat.C <- data %>% 
  filter(PIP.Number == "C") 

#mass
dat.C.mass <- dat.C %>%
  select(Time, Pre.Post, Depth, Weight)

C.mass.aov <- aov(Weight ~ Depth * Time * Pre.Post, data = dat.C.mass)
summary(C.mass.aov)


#energy.break
dat.C.energy <- dat.C %>%
  select(Time, Pre.Post, Depth, Energy.Break)

C.energy.aov <- aov(Energy.Break ~ Depth * Time * Pre.Post, data = dat.C.energy)
summary(C.energy.aov)


pairwise.t.test(dat.C.energy$Energy.Break, dat.C.energy$Depth, p.adj = 'bonferroni')


#electrical, select only one frequency (1000 Hz)
dat.C.elec <- dat.C %>%
  select(Time, Pre.Post, Depth, Resistance.1k)

C.elec.aov <- aov(Resistance.1k ~ Depth * Time * Pre.Post, data = dat.C.elec)
summary(C.elec.aov)

#bonferroni post hoc
pairwise.t.test(dat.C.elec$Resistance.1k, dat.C.elec$Depth, p.adj = 'bonferroni')


pairwise.t.test(dat.C.elec$Resistance.1k, dat.C.elec$Time, p.adj = 'bonferroni')

#####
# D only
#####

dat.D <- data %>% 
  filter(PIP.Number == "D") 

#mass
dat.D.mass <- dat.D %>%
  select(Time, Pre.Post, Depth, Weight)

D.mass.aov <- aov(Weight ~ Depth * Time * Pre.Post, data = dat.D.mass)
summary(D.mass.aov)

pairwise.t.test(dat.D.mass$Weight, dat.D.mass$Depth, p.adj = 'bonferroni')



#energy.break
dat.D.energy <- dat.D %>%
  select(Time, Pre.Post, Depth, Energy.Break)

D.energy.aov <- aov(Energy.Break ~ Depth * Time * Pre.Post, data = dat.D.energy)
summary(D.energy.aov)



#electrical, select only one frequency (1000 Hz)
dat.D.elec <- dat.D %>%
  select(Time, Pre.Post, Depth, Resistance.1k)

D.elec.aov <- aov(Resistance.1k ~ Depth * Time * Pre.Post, data = dat.D.elec)
summary(D.elec.aov)


pairwise.t.test(dat.D.elec$Resistance.1k, dat.D.elec$Depth, p.adj = 'bonferroni')

pairwise.t.test(dat.D.elec$Resistance.1k, dat.D.elec$Time, p.adj = 'bonferroni')

pairwise.t.test(dat.D.elec$Resistance.1k, dat.D.elec$Pre.Post, p.adj = 'bonferroni')


#####
# N only
#####

dat.N <- data %>% 
  filter(PIP.Number == "N") 

#mass
#three way anova
dat.N.mass <- dat.N %>%
  select(Time, Pre.Post, Depth, Weight)

N.mass.aov <- aov(Weight ~ Depth * Time * Pre.Post, data = dat.N.mass)
summary(N.mass.aov)



#energy.break
dat.N.energy <- dat.N %>%
  select(Time, Pre.Post, Depth, Energy.Break)

N.energy.aov <- aov(Energy.Break ~ Depth * Time * Pre.Post, data = dat.N.energy)
summary(N.energy.aov)


pairwise.t.test(dat.N.energy$Energy.Break, dat.N.energy$Time, p.adj = 'bonferroni')


#electrical, select only one frequency (1000 Hz)
dat.A.elec <- dat.A %>%
  select(Time, Pre.Post, Depth, Resistance.1k)

A.elec.aov <- aov(Resistance.1k ~ Depth * Time * Pre.Post, data = dat.A.elec)
summary(A.elec.aov)


pairwise.t.test(dat.A.elec$Resistance.1k, dat.A.elec$Time, p.adj = 'bonferroni')






#tests for normality
plot(D.mass.aov, which = 2)
hist(D.mass.aov$residuals)
shapiro.test(D.mass.aov$residuals)








