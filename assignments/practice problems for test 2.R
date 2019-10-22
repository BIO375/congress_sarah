# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

death2_data <-read.csv("datasets/abd/chapter12/chap12q01DeathAndTaxes.csv")

View(death2_data)

mutate_death <- mutate(death2_data, diff = lowerTaxDeaths - HigherTaxDeaths)

View(mutate_death)

t.test(mutate_death$lowerTaxDeaths, mutate_death$HigherTaxDeaths, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)

#####9####
fish_data <-read.csv("datasets/abd/chapter12/chap12q09Cichlids.csv")

View(fish_data)

ggplot(fish_data) +
  geom_histogram(aes(preference), binwidth = .08)+
  facet_wrap(~genotype)

summ_fish <- fish_data %>%
  group_by(genotype) %>% 
  summarise(mean_preference = mean(preference),
            sd_preference = sd(preference),
            var_preferehce = var(preference),
            n_fish = n())

View(summ_fish)

ratio <-(max(summ_fish$sd_preference))/(min(summ_fish$sd_preference))
View(ratio)

t.test(preference ~ genotype, data = fish_data, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)
#two sample because the ratio was not greater than three
#the book used welch's because it said the ratio was greater than three?????

#####10####

candidate_data <-read.csv("datasets/abd/chapter12/chap12q10WillsPresidents.csv")

View(candidate_data)

t.test(candidate_data$willsLoser, candidate_data$willsWinner, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)

#####16#####

mosquito_data <-read.csv("datasets/abd/chapter12/chap12q16BeerandMosquitoes.csv")

View(mosquito_data)

ggplot(mosquito_data)+
  geom_qq(aes(sample = change))

t.test(change ~ drink, data = mosquito_data, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

#####ch 13 q 1#####

health_data <-read.csv("datasets/abd/chapter13/chap13q01CountryHealthExpenditure.csv")

View(health_data)

ggplot(health_data) +
  geom_histogram(aes(perCapitaHealthExpenditure2010), binwidth = 200)

ggplot(health_data) +
  geom_boxplot(aes(x = "", y = perCapitaHealthExpenditure2010))

ggplot(health_data)+
  geom_qq(aes(sample = perCapitaHealthExpenditure2010))
#a: it is right skewed with a long tail extending out to the right
#b: ln is used because of the right skew 

summ_health <- health_data %>%
  summarise(mean_expenditure = mean(perCapitaHealthExpenditure2010),
            sd_expenditure = sd(perCapitaHealthExpenditure2010),
            n_countries = n())
#D: sample size is 20
View(summ_health)

mutate_healthData <- health_data %>% 
  mutate(natLogSal= log(perCapitaHealthExpenditure2010))
#C

View(mutate_healthData)

summ_health2 <- mutate_healthData %>%
  summarise(mean_expenditure = mean(natLogSal),
            sd_expenditure = sd(natLogSal),
            n_countries = n())

SE<- 1.345936/20

View(SE)

View(summ_health2)
#E: 5.910095
#f: 1.345936
#G: 0.0672968
#H: 
#I:

#####3#####

recycling_data <-read.csv("datasets/abd/chapter13/chap13q03Recycling.csv")

View(recycling_data)

ggplot(recycling_data) +
  geom_histogram(aes(paperUsage), binwidth = 15)+
  facet_wrap(~recyclingBinPresent)

#A: the data is not normally distributed
#B: You can transform the data, or do a non parametric test depending on the std dev ratio
#C: H0: med of absent equals the median of present; HA: med of absent does not equal median of present
#D: no
#E: no

wilcox.test(paperUsage ~ recyclingBinPresent, data = recycling_data, alternative = "two.sided", conf.level = 0.95)

#F: W= 126.5, p=0.031

#####9#####

newt_data <-read.csv("datasets/abd/chapter13/chap13q09Newts.csv")

View(newt_data)

summ_newt <- newt_data %>%
  group_by(locality) %>% 
  summarise(mean_resistance = mean(wholeAnimalResistance),
            sd_resistance = sd(wholeAnimalResistance),
            var_resistance = var(wholeAnimalResistance),
            n_newt = n())

View(summ_newt)

ratio <-(max(summ_newt$sd_resistance))/(min(summ_newt$sd_resistance))

View(ratio)
#A:greater than three can't use two sample t test
#B: mutate and then try again, do whitney if the variances are the same, but they aren't, do MWU test

ggplot(newt_data) +
  geom_histogram(aes(wholeAnimalResistance), binwidth = .1)+
  facet_wrap(~locality)

mutate_newt <- newt_data %>% 
  mutate(natLogSal= log(wholeAnimalResistance))

View(mutate_newt)

#C:the data is bimodal/ right skewed

summ_newt <- mutate_newt %>%
  group_by(locality) %>% 
  summarise(mean_resistance = mean(natLogSal),
            sd_resistance = sd(natLogSal),
            var_resistance = var(natLogSal),
            n_newt = n())


View(summ_newt)

t.test(natLogSal ~ locality, data = mutate_newt, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

#D
#E

