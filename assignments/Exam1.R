# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")

# Check for updates
tidyverse_update()


#start of code for Q. 11
#reading in the file
exam1<-read_csv("datasets/demos/exam1data.csv")


#summary stats
summ_exam1 <- exam1 %>%
  group_by(form) %>% 
  summarise(mean_n = mean(n),
            median_n = median(n),
            IQR_n = IQR(n),
            sd_n = sd(n),
            var_n = var(n))

#creating the boxplot
ggplot(exam1)+
  geom_boxplot(aes(x = form, y = n), notch = FALSE, varwidth = TRUE)

#creating a histogram, to confirm skew
ggplot(exam1) +
  geom_histogram(aes(n), binwidth = 2)+
  facet_wrap(~form)

#end of code for question 11

#code for question 12

data01 <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")

data01 <- data01 %>% slice(-105)

summ_data01 <- data01 %>%
  group_by(Survival) %>% 
  summarise(mean_n = mean(squamosalHornLength),
            median_n = median(squamosalHornLength),
            IQR_n = IQR(squamosalHornLength),
            sd_n = sd(squamosalHornLength),
            var_n = var(squamosalHornLength))

#SAGE BRUSH CRICKETS EXTRA CREDIT

sage_data <- read_csv("datasets/abd/chapter13/chap13e5SagebrushCrickets.csv")

#a. and b.- frequency of time to mating for females who were starved or fed 
ggplot(sage_data) +
  geom_histogram(aes(timeToMating), color= "red", binwidth = 10)+
  facet_wrap(~feedingStatus)

#log-transformed histogram of time to mating for females who were starved or fed
ggplot(sage_data) +
  geom_histogram(aes(log(timeToMating+1)), color = "green", binwidth = 0.5)+
  facet_wrap(~feedingStatus)
