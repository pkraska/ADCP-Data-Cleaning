library(tidyverse)

# read data as rows of characters
data <- readLines("data.txt")

# remove first two empty rows
data <- data[-1:-2]

# create logical vector of whether or not the row is part of the header or not
header.rows <- substr(data, 0, 1) != " "

# read data as text file, creating a character column for data, then removing the first two blank rows
tab <- read_table("data.txt", col_names = FALSE) %>%
  slice(3:n())

# Add header.rows information to data so we can eventually filter by column
tab$header <- header.rows
colnames(tab) <- c("char", "header")

# Filter data into df.header remove header column, and separate char into columns
df.header <- tab %>%
  filter(header == TRUE) %>%
  select(char) %>%
  separate(char, into = paste("x",c(1:13), sep = "."), sep = "\\s+")

# Consolidate header information from 6 rows into 1 using cbind
header <- df.header[seq(from = 1, to = nrow(df.header), by = 6),] %>%
  cbind(df.header[seq(from = 2, to = nrow(df.header), by = 6),]) %>%
  cbind(df.header[seq(from = 3, to = nrow(df.header), by = 6),]) %>%
  cbind(df.header[seq(from = 4, to = nrow(df.header), by = 6),]) %>%
  cbind(df.header[seq(from = 5, to = nrow(df.header), by = 6),]) %>%
  cbind(df.header[seq(from = 6, to = nrow(df.header), by = 6),]) 

# rename column names as they copied the same names each time in previous step
colnames(header) <- paste("v", seq(from = 1, to = ncol(header), by = 1), sep = "")

# Remove blank columns, and rename columns to human readable format
header <- select(header, -22:-26, -32:-39, -45:-52, -58:-65, -72:-78) %>%
  rename(year = v1,
         month = v2,
         day = v3,
         hour = v4,
         minute = v5,
         second = v6,
         hth_second = v7,
         ens_number = v8,
         ens_in_segment = v9,
         pitch = v10,
         roll = v11,
         cor_heading = v12,
         adcp_temp = v13,
         
         latitude = v40,
         longitude = v41)

# Filter data into df.data remove header column, and separate char into columns
df.data <- tab %>%
  filter(header == FALSE) %>%
  select(char) %>%
  slice(2:n()) %>%
  separate(char, into = paste("x",c(1:9), sep = "."), sep = "\\s+")


# write.table(tab, file = "data.csv", sep = "", col.names = FALSE, row.names = FALSE)

# tab.read <-  read_csv("data.csv")
