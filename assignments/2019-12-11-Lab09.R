# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

# Load ggfortify for plotting
install.packages("ggfortify")
library("ggfortify")

# Load broom to convert statistical objects to tidy tibbles and plotly
# for confidence bands
# If you have not installed broom before, you will need to execute
install.packages("broom")
library("broom")


# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()
#install.packages(c("haven", "rvest"))


fowler <- read_csv("datasets/demos/fowler.csv")
View(fowler)

#check for normality
ggplot(data = fowler) +
  geom_point(mapping = aes(x = FERTILIZER, y = YIELD ))

#point plot shows evidence of a linear relationship 

#check for bivariate normality

ggplot(data = fowler)+
  geom_boxplot(aes("", FERTILIZER))

ggplot(data = fowler)+
  geom_qq(aes(sample = FERTILIZER))

ggplot(data = fowler)+
  geom_boxplot(aes("", YIELD))

ggplot(data = fowler)+
  geom_qq(aes(sample = YIELD))

#linear regression

model01 <- lm(YIELD ~ FERTILIZER, data = fowler)

autoplot(model01, smooth.colour = NA)

ggplot(data = fowler)+
  geom_point(aes(x = FERTILIZER, y = resid(model01)))

summary(model01)

ggplot(data = fowler, aes(x = FERTILIZER, y = YIELD)) +
  geom_point() +
  geom_smooth(method = "lm", level=0.95) +
  theme_bw()+
  labs( x = "fertilizer", y = "yield")

#### 10/10 code runs without breaking ####