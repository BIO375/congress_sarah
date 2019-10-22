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

#####17#####


