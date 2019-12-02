# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

#install desk tools
install.packages("DescTools")
library("DescTools")

##### Question 20 ##### 2/2 pts

#a: you can perform either a Welch's t-test because the ratio is very high indicating homoskedasticity or
#   you can transform the data using a mathematical function and then use a normal 2 sample t test
#b: There is a significant difference between the coloration of kokanee salmon and sockeye salmon 
#   (t= 12.133, df= 33, p<0.0001)



#import data for question 20
salmon_data <-read.csv("datasets/abd/chapter13/chap13q20SalmonColor.csv")

View(salmon_data)

#summary statistics for the salmon data- mean and medians are similar
summ_salmon <- salmon_data %>%
  group_by(species) %>% 
  summarise(mean_Color = mean(skinColor),
            median_Color = median(skinColor),
            IQR_Color = IQR(skinColor),
            sd_Color = sd(skinColor),
            var_Color = var(skinColor))
            
View(summ_salmon)

#test for homoskedasticity
ratio <-(max(summ_salmon$sd_Color))/(min(summ_salmon$sd_Color))

View(ratio)

#ln transformation of the data
mutate_salmon <- salmon_data %>% 
  mutate(natLogSal= log(skinColor))

View(mutate_salmon)

#summary of the new mutated data
summ2_salmon <- mutate_salmon %>%
  group_by(species) %>% 
  summarise(mean_Color = mean(natLogSal),
            median_Color = median(natLogSal),
            IQR_Color = IQR(natLogSal),
            sd_Color = sd(natLogSal),
            var_Color = var(natLogSal))

View(summ2_salmon)

#ratio calculations for the new mutated data
ratio <-(max(summ2_salmon$sd_Color))/(min(summ2_salmon$sd_Color))

View(ratio)

#because ration is under 3 we can use normal 2 sample t test

#2 sample, 2 sided t test
t.test(natLogSal ~ species, data = mutate_salmon, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)

#end of question 20

#####Question 25#####

#There is not a significant difference in the biomass change in rainforests close vs 
#far from clearcutting (S= 21, # of differences=36, p>0.05)

#load in data
tree_data <-read.csv("datasets/abd/chapter13/chap13q25Clearcuts.csv")

View(tree_data)

#the histrogram in the testbook shows that the data is left skewed (has a long tail on the left)

SignTest(tree_data$biomassChange, alternative = "two.sided", mu = 0, conf.level = 0.95)


#####Question 26#####

bird_data <-read_csv("datasets/abd/chapter13/chap13q26ZebraFinchBeaks.csv")

View(bird_data)

#the alt hypothesis would be if the # indicating the amoung to standardized time the female 
#spent with the male is> 0
#one sample, one sided t test

#summary stats
summ_bird <- bird_data %>%
  summarise(mean_time = mean(preference),
            median_time = median(preference),
            IQR_time = IQR(preference),
            sd_time = sd(preference),
            var_time = var(preference))

View(summ_bird)

ggplot(bird_data) +
  geom_histogram(aes(preference), binwidth = 10)

ggplot(bird_data) +
  geom_boxplot(aes(x = "", y = preference))

ggplot(bird_data) +
  geom_qq(aes(sample = preference, color = ""))

#chose the one sided one sample t test because the qq plot and histogram looked fairly normal and 
#the mean and median were similar

t.test(bird_data$preference, 
       alternative = "greater", mu = 0, conf.level = 0.95)

#there is a sigificant difference in the preference 


#####Question 16#####

#a: t= 3.3802 p<0.05 95% CI= 25.93287 < pop mean 1 - pop mean 2 < 110.26713
#b: the weight of the test is based off of the p value, which is 0.003142. Because the calculated t value
#   is greater than t(2)(19)= 2.09 the null hypothesis can be rejected

fish_data <-read_csv("datasets/demos/fishData.csv")

View(fish_data)

#summary stats
summ_fish <- fish_data %>%
  group_by(group) %>% 
  summarise(mean_Color = mean(time),
            median_Color = median(time),
            IQR_Color = IQR(time),
            sd_Color = sd(time),
            var_Color = var(time))
View(summ_fish)


#check for normality
ggplot(fish_data) +
  geom_histogram(aes(time), binwidth = 40)

ggplot(fish_data) +
  geom_boxplot(aes(x = group, y = time))

ggplot(fish_data)+
  geom_qq(aes(sample = time, color = group))

#variance difference was less than three using the rule of thumb, normal two sample t test used
t.test(time ~ group, data = fish_data, var.equal = TRUE, alternative = "two.sided", conf.level = 0.95)





