library("readr")
library("tidyr")
library("dplyr")
library("reshape2")
library("tibble")

# Read csv file of data into R
data <- read_csv("data.csv", col_names = FALSE)
data1 <- data [2:7,] %>%
  t %>%
  as.vector %>%
  enframe %>%
  spread(name, value)

# repeat the row 6 times
data1[2:7,] <- data1 