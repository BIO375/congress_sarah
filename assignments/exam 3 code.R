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

##### q 10#####
aphid<-read_csv("datasets/exams/aphids.csv",
                col_types = cols(gall_number = col_factor() ))

View(aphid)

model02 <- lme(fixed = thorax_length ~ 1,
               random = ~1|gall_number, data = aphid)

model02_varcomp <- VarCorr(model02)

model02_varcomp

varAmong  <- as.numeric( model02_varcomp[1,1] )

varWithin <- as.numeric( model02_varcomp[2,1] )

repeatability <- varAmong / (varAmong + varWithin)
repeatability


######q 11#####

glucose<-read_csv("datasets/exams/glucose.csv")

View(glucose)

glucoseCor <- cor.test(~ blood_glucose + HbA1c, data = glucose,
                     method = "pearson")
glucoseCor


#####q12#####

drivers <-read_csv("datasets/exams/DriverVision.csv")

View(drivers)

ggplot(data = drivers) +
  geom_point(mapping = aes(x = Age, y = Distance ))

model01 <- lm(Age ~ Distance, data = drivers)

autoplot(model01, smooth.colour = NA)

ggplot(data = drivers)+
  geom_point(aes(x = Age, y = resid(model01)))

summary(model01)
