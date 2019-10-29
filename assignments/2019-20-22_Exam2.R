#All the good stuff 
#Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

install.packages("purrr") #done to update tidyverse as told by RStudio

# To perform sign tests, install and load the package DescTools
install.packages("DescTools")
library("DescTools")

#####Question 9 #####

#import original data set
feather_data <-read.csv("datasets/exams/feathers.csv")

View(feather_data)

#import data set with difference calculated
diffFeather <-read.csv("datasets/exams/feathers_diff.csv")

View(diffFeather)

#summary stats

summ_feathers <- diffFeather %>% 
  summarise(mean_diff = mean(diff),
            median_diff= median(diff),
            sd_diff = sd(diff),
            n_birds = n())
View(summ_feathers)
#the mean is close to the median --> normal

#tests for normality

ggplot(diffFeather) +
  geom_histogram(aes(diff), binwidth = .01)

ggplot(diffFeather) +
  geom_boxplot(aes(x = "", y = diff))
#whiskers aren't very long

ggplot(diffFeather)+
  geom_qq(aes(sample = diff))
#pretty straight

# One-sided, HA that odd feather is less than typical feather
t.test(feather_data$odd, feather_data$typical, 
       alternative = "less", paired =  TRUE, conf.level = 0.95)

#####Question 10#####

vaccine_data <-read.csv("datasets/exams/baker.csv")

#calculate difference in antibody concentration
vaccine_data <- mutate(vaccine_data, diff = After - Before)

View(vaccine_data)

#summary stats

summ_data <- vaccine_data %>% 
  summarise(mean_concentration = mean(diff),
            median_concentration= median(diff),
            sd_concentration = sd(diff),
            n_patients = n())

View(summ_data)
#mean and median very different :(


ggplot(vaccine_data) +
  geom_histogram(aes(diff), binwidth = .5)

ggplot(vaccine_data) +
  geom_boxplot(aes(x = "", y = diff))

ggplot(vaccine_data)+
  geom_qq(aes(sample = diff))

#NONE OF THEM ARE NORMALLLL :,,,,(

vaccine_data<-mutate(vaccine_data, squareroot_diff = sqrt(diff))

vaccine_data<-mutate(vaccine_data, log_diff = log(diff))

View(vaccine_data)

#because there are 2 negative numbers in the differences I don't think mutation
#will work in this instance...onto the sign test

# One-sided, HA that after vaccination antibody concentration is greater than before vaccination antibody concentration
SignTest(vaccine_data$diff, alternative = "greater", mu = 0, conf.level = 0.95)


#####Question 11#####

algae_data <-read.csv("datasets/exams/algae.csv")

View(algae_data)

#summary stats
summ_algae <- algae_data %>%
  group_by(treatment) %>% 
  summarise(mean_growth = mean(growthrate),
            sd_growth = sd(growthrate),
            median_growth= median(growthrate),
            n_algaeLines = n())

View(summ_algae)
#medians are close to the means for each group
#need to do ratio test for sd

#ratio test

ratio <-(max(summ_algae$sd_growth))/(min(summ_algae$sd_growth))

View(ratio)
#rato is 1.13 which is less than 3 :)

#graphs to look at normality
ggplot(algae_data) +
  geom_histogram(aes(growthrate), binwidth = .5)+
  facet_wrap(~treatment)

ggplot(algae_data) +
  geom_boxplot(aes(x = treatment, y = growthrate))

ggplot(algae_data)+
  geom_qq(aes(sample = growthrate, color = treatment))

#none of the graphs look increasingly skewed left or right,
#qq plots are straight enough as well

#two sided, two sample T test
t.test(growthrate ~ treatment, data = algae_data, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)


