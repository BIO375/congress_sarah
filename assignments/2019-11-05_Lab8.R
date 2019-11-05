# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Install package ggfortify, *note* only do install.packages ONCE
# ggfortify is a package that works with ggplot2 to make nice plots
install.packages("ggfortify")
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
install.packages("multcomp")
library("multcomp")
# nlme is used for random effects ANOVA
install.packages("nlme")
library("nlme")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

#### Problem 15-22 ####
# Complete parts a, b, c, d

#a: within group variance(MSerror): 0.0001660000
#b: between group variance 0.0002459167
#c: repeatability= 0.5970059
#d: The femur length has a higher repeatability and the head width was more affected by measurement error.

bug_data <-read_csv("datasets/abd/chapter15/chap15q22WalkingStickHeads.csv", col_types = cols(
  specimen = col_factor() ))

View(bug_data)

#check for normality
ggplot(bug_data) +
  geom_histogram(aes(headwidth), binwidth = .011)

ggplot(bug_data)+
  geom_qq(aes(sample = headwidth, color = ""))

ggplot(bug_data) +
  geom_boxplot(aes(x = "", y = headwidth))

summary(bug_data)

#random effects ANOVA
model02 <- lme(fixed = headwidth ~ 1,
               random = ~1|specimen, data = bug_data)

model02_varcomp <- VarCorr(model02)

model02_varcomp
#between group variance 0.0002459167
#within group variance(MSerror): 0.0001660000

#variation due to specimen
varAmong  <- as.numeric( model02_varcomp[1,1] )

#measurement error
varWithin <- as.numeric( model02_varcomp[2,1] )

repeatability <- varAmong / (varAmong + varWithin)
#repeatability= 0.5970059

#### Problem 15-23 ####
# Complete parts a and c only

#a: You need to run a planned multiple comparison test
#c: Skipped

pine_data <-read_csv("datasets/abd/chapter15/chap15q23LodgepolePineCones.csv", col_types = cols(
  habitat = col_factor() ))

View(pine_data)

summary(pine_data)

ggplot(pine_data) +
  geom_histogram(aes(conemass), binwidth = 1)+
  facet_wrap(~habitat)

ggplot(pine_data)+
  geom_qq(aes(sample = conemass, color = habitat))

ggplot(pine_data) +
  geom_boxplot(aes(x = habitat, y = conemass))

#multiple planned comparisons
model01 <- lm(conemass~habitat, data = pine_data)

planned <- glht(model01, linfct = 
                  mcp(habitat = c("island.absent - island.present = 0")))
confint(planned)
summary(planned)

#### Problem 15-26 ####
# Use the data to perform the correct test.  Please show code for all steps in your process.


mosquito_data <-read_csv("datasets/abd/chapter15/chap15q26MalariaFungusVenom.csv", col_types = cols(
  treatmentGroup = col_factor() ))

View(mosquito_data)

#Step 1
#normal one way fixed effects ANOVA
#checks for normality
ggplot(mosquito_data) +
  geom_histogram(aes(logSporozoiteNumbers), binwidth = .5)+
  facet_wrap(~treatmentGroup)

ggplot(mosquito_data)+
  geom_qq(aes(sample = logSporozoiteNumbers, color = treatmentGroup))

ggplot(mosquito_data) +
  geom_boxplot(aes(x = treatmentGroup, y = logSporozoiteNumbers))

summary(mosquito_data)

#Step 2

model_mosquito <- lm(logSporozoiteNumbers~treatmentGroup, data = mosquito_data)

#Step 3
summ_growth.rate <- mosquito_data %>%
  group_by(treatmentGroup) %>% 
  summarise(mean_SporozoiteNum = mean(logSporozoiteNumbers),
            sd_SporozoiteNum = sd(logSporozoiteNumbers),
            n_SporozoiteNum = n())
ratio_daphnia <-(max(summ_growth.rate$sd_growth.rate))/(min(summ_growth.rate$sd_growth.rate))

autoplot(model_mosquito)

#step 4
anova(model_mosquito)

#Tukey

tukey <- glht(model_mosquito, linfct = mcp(treatmentGroup = "Tukey"))
summary(tukey)

#### Problem 15-30 and/or 15-31 (same data in both problems) ####
# Use the data to perform the correct test.  Please show code for all steps in your process.

crab_data <-read_csv("datasets/abd/chapter15/chap15q30FiddlerCrabFans.csv", col_types = cols(
  crabType = col_factor() ))

crab_data <- crab_data[1:84,]

View(crab_data)

#Step 1
#normal one way fixed effects ANOVA
#checks for normality
ggplot(crab_data) +
  geom_histogram(aes(bodyTemperature), binwidth = .5)+
  facet_wrap(~crabType)

ggplot(crab_data)+
  geom_qq(aes(sample = bodyTemperature, color = crabType))

ggplot(crab_data) +
  geom_boxplot(aes(x = crabType, y = bodyTemperature))

summary(crab_data)

#Step 2
model_crab <- lm(bodyTemperature~crabType, data = crab_data)

#Step 3
summ_stats <- crab_data %>%
  group_by(crabType) %>% 
  summarise(mean_rate = mean(bodyTemperature),
            sd_rate = sd(bodyTemperature),
            n_crab = n())
ratio_crab <-(max(summ_stats$sd_rate))/(min(summ_stats$sd_rate))

autoplot(model_crab)


#Step 4
anova(model_crab)

