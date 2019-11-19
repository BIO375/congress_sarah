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



face<-read_csv("datasets/abd/chapter17/chap17q01FacesAndPenalties")

read_csv("datasets/quinn/chpt5/peake.csv")


