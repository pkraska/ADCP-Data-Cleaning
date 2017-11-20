## If you haven't installed previously, uncomment next three lines lines and run
# install.packages("readr")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("reshape2")

library("readr")
library("tidyr")
library("dplyr")
library("reshape2")

# Read csv file of data into R
data <- read_csv("data.csv", col_names = FALSE)

# Get header information from file
header <- data %>%
  filter(!is.na(data[,1])) %>%
  mutate(head = rep((1:(nrow(header)/6)), each = 6))

for (i in 1:(nrow(header)/6)) {
  row.head <- header[header$head == i,] %>%
    toString()
}

mat.head <- as.matrix(header)

# # testing metling and recasting header rows into single row
# test.head <- header[1:6,]
# mat.head <- as.matrix(test.head)
# data.head <- as.vector(mat.head)

# get header spacing info
rle.data <- rle(is.na(data$X1))   #rle, creates list of lengths and values that repeat

# remove the first element of the lists
na.len <- rle.data$lengths[-1]
# remove header rows, gives number of lines/header rows to add to.
na.len <- na.len[na.len > 6]

na.values <- rle.data$values[-1]

# Remove header info from file to create flat file of just measurments
r.data <- data %>%
  filter(is.na(X1)) %>%               #filter out all header information
  slice(-1) %>%                       #remove first row in datafile
  select(-1)                          #remove first column that is empty

