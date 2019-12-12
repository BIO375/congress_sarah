# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load tidyverse
library("tidyverse")


# Check for updates
tidyverse_update()

#SCENARIO 1 CODE

birthRates<-read_csv("datasets/demos/birthRates.csv")

View(birthRates)

ggplot(birthRates)+
  geom_boxplot(aes(x = "", y = Diff), notch = FALSE, varwidth = TRUE) 


ggplot(birthRates) +
  geom_histogram(aes(Diff), color= "red", binwidth = .5)


summ_data <- birthRates %>%
  summarise(mean_n = mean(Diff),
            median_n = median(Diff),
            IQR_n = IQR(Diff),
            sd_n = sd(Diff),
            var_n = var(Diff))
View(summ_data)


#SCENARIO 2 CODE

data01 <- read_csv("datasets/abd/chapter12/chap12e3HornedLizards.csv")

data01 <- data01 %>% slice(-105)

summ_data01 <- data01 %>%
  group_by(Survival) %>% 
  summarise(mean_n = mean(squamosalHornLength),
            median_n = median(squamosalHornLength),
            IQR_n = IQR(squamosalHornLength),
            sd_n = sd(squamosalHornLength),
            var_n = var(squamosalHornLength))
View(summ_data01)

ggplot(data01)+
  geom_boxplot(aes(x = Survival, y = squamosalHornLength), notch = FALSE, varwidth = TRUE) 

ggplot(data01) +
  geom_histogram(aes(squamosalHornLength), color= "red", binwidth = 1)+
  facet_wrap(~Survival)

#END OF SCENARIO 2 CODE


#### 10/10 Code runs without breaking ####