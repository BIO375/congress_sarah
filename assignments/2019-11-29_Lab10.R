# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

install.packages("ggmosaic")
library("ggmosaic")

install.packages("epitools")
library("epitools")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

#####Binomial test beetles#####

49+41 # n = total number of trials
model04 <- binom.test(x= 41, n=90, p=0.5, alternative = "greater", conf.level = 0.95 )
model04


#####chi squared goodness of fit#####
# x = observed counts of each level of your categorical variable
# p = expected probabilities for each level of your categorical variable, must be number between 0 and 1
# x = birth_summ$day_n
# p = birth_summ$expected_p

flowers<- read_csv("datasets/demos/flowerbreeding.csv")
View(flowers)

model05 <-chisq.test(x = flowers$flowers_n, p = flowers$expected_p)
model05

#####contingency analysis#####

#bacteriaTable<- read_csv("datasets/demos/bacteria.csv")
#View(bacteriaTable)

bacteria<- matrix(c(30,17,41,49), 2,2, byrow=TRUE)


dimnames(bacteria) <- list("Location" = c("Xhoffraix", "Bergen op Zoom"),
                        "Sex" = c("Female", "Male"))
View(bacteria)

as.matrix(bacteria)
model06 <- chisq.test(tab01, correct = FALSE)
model06



