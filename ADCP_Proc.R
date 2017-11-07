install.packages("readr")
library("readr")
library("tidyr")
library("dplyr")
# Read csv file of data into R
data <- read.csv("data.csv", header = FALSE)
