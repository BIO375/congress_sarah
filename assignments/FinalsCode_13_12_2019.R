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



#####Scenario 1####
swim <- read_csv("datasets/final/insulation.csv")
View(swim)

SwimModel01 <- lm(leanness ~ heat_loss, data = swim)

autoplot(SwimModel01, smooth.colour = NA)

#residual by x plot 

ggplot(data = swim)+
  geom_point(aes(x = leanness, y = resid(SwimModel01)))

summary(SwimModel01)

#####Scenario 2#####

caff <-read_csv("datasets/final/caffeine.csv", col_types = cols(
  group = col_factor() ))

View(caff)

summ_caff <- caff %>%
  group_by(group) %>% 
  summarise(mean_half_life = mean(half_life),
            median_half_life = median(half_life),
            IQR_half_life = IQR(half_life),
            sd_half_life = sd(half_life),
            var_half_life = var(half_life))

View(summ_caff)

ggplot(caff) +
  geom_histogram(aes(half_life), binwidth = .4)+
  facet_wrap(~group)

ggplot(caff)+
  geom_boxplot(aes(x = group, y = half_life), notch = FALSE, varwidth = TRUE)

ggplot(caff)+
  geom_qq(aes(sample = half_life, color = group))


caffModel <- lm(half_life~group, data = caff)

summ_half_life <- caff %>%
  group_by(group) %>% 
  summarise(mean_half_life = mean(half_life),
            sd_half_life = sd(half_life),
            n_half_life = n())

ratio_caff <-(max(summ_half_life$sd_half_life))/(min(summ_half_life$sd_half_life))
View(ratio_caff)

autoplot(caffModel)


anova(caffModel)

summary(caffModel)

tukey <- glht(caffModel, linfct = mcp(group = "Tukey"))
summary(tukey)

#####scenario 3#####

demos <- read_csv("datasets/final/davis.csv")
View(demos)

model_03 <-chisq.test(x = demos$observed, p = demos$expected_p)
model_03
model_03$expected


# Line 29 flipped y and x in formula
# Line 86 should be planned comparison
#### 8/10 code breaks 2x ####