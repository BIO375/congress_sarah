# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

# To perform sign tests, install and load the package DescTools
install.packages("DescTools")
library("DescTools")

# Read in data file, generic version
#<name-you-assign><-read_csv("path-to-file", col_names = TRUE)

#Quesiton 1 code

oldTilt_data<-read_csv("datasets/demos/historicalEarthAngle.csv", col_names= TRUE)

View(oldTilt_data)

#Code for one-sample, two-sided t-test

t.test(oldTilt_data$Obliquity, alternative = "two.sided", mu = 23.4722, conf.level = 0.95)

#end

#Question 2 Code

heartAttack_data<-read_csv("datasets/demos/HeartAttack_short.csv",col_names = TRUE,
               col_types = cols(
                 group = col_character() )
)


View(heartAttack_data)

#summary data

summ_data <- heartAttack_data %>%
  group_by(group) %>% 
  summarise(mean_cholest = mean(cholest),
            sd_cholest = sd(cholest),
            n_sample = n())

View(summ_data)

# Calculate the ratio between the standard deviations as a loose test of homoscedasticity
ratio <-(max(summ_data$sd_cholest))/(min(summ_data$sd_cholest))

View(ratio)

#histogram
ggplot(heartAttack_data) +
  geom_histogram(aes(cholest), binwidth = 30)+
  facet_wrap(~group)

#boxplot
ggplot(heartAttack_data) +
  geom_boxplot(aes(x = group, y = cholest))

#qq plot
ggplot(heartAttack_data) +
  geom_qq(aes(sample = cholest, color = group))


#two- sample, two- sided t-test

t.test(cholest ~ group, data = heartAttack_data, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

#end

#Code for question 3

fulmars <- read_csv("datasets/quinn/chpt3/furness.csv")

View(fulmars)

wilcox.test(METRATE ~ SEX, data = fulmars, alternative = "two.sided", conf.level = 0.95)

#end

#Code for question 4

untidySpider_data <- read_csv("datasets/quinn/chpt3/elgar.csv")

View(untidySpider_data)

#mutating the data..for no reason

untidySpider_data <- mutate(untidySpider_data, diff = HORIZDIM - HORIZLIG)

ggplot(untidySpider_data) +
  geom_boxplot(aes(x = group, y = cholest))
#### CODE BREAKS HERE, NO VARIABLE CALLED GROUP, NOR CHOLEST) ####


t.test(untidySpider_data$HORIZDIM, untidySpider_data$HORIZLIG, 
       alternative = "two.sided", paired = TRUE, conf.level = 0.95)

### 9/10 Code breaks once ####



