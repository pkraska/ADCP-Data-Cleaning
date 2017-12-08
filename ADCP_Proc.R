# Check is all packages required are installed, if not, will install
list.of.packages <- c("readr", "tidyr", "dplyr", "svMisc")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("readr")
library("tidyr")
library("dplyr")
library("tibble")

# Show working directory to make sure you're in the right folder
getwd()

# List TXT files in the working directory
file.list <- list.files(pattern = "*.TXT")

pb <- txtProgressBar(min = 0, max = length(list.files), initial = 0, style =3) 
pb1 <- txtProgressBar(min = 0, max = 14, initial = 0, style = 3)

for (i in 1:length(file.list)) {

  # read data as rows of characters
  data <- readLines(file.list[i])
  
setTxtProgressBar(pb1, 1)

  # remove first two empty rows
  data <- data[-1:-2]

setTxtProgressBar(pb1, 2)
  
    # create logical vector of whether or not the row is part of the header or not
  header.rows <- substr(data, 0, 1) != " "

setTxtProgressBar(pb1, 3)
  
  # read data as text file, creating a character column for data, then removing the first two blank rows
  tab <- suppressMessages(read_table(file.list[i], col_names = FALSE)) %>%
    slice(3:n())
  
setTxtProgressBar(pb1, 4)
  
  # Add header.rows information to data so we can eventually filter by column
  tab$header <- header.rows
  colnames(tab) <- c("char", "header")
  
setTxtProgressBar(pb1, 5)
  
  # Filter data into df.header remove header column, and separate char into columns
  suppressWarnings(df.header <- tab %>%
    filter(header == TRUE) %>%
    select(char) %>%
    separate(char, into = paste("x",c(1:13), sep = "."), sep = "\\s+"))
  
setTxtProgressBar(pb1, 6)
  
  # Filter data into df.data remove header column, and separate char into columns
  df.data <- tab %>%
    filter(header == FALSE) %>%
    select(char) %>%
    slice(2:n()) %>%
    separate(char, into = paste("x",c(1:9), sep = "."), sep = "\\s+")

setTxtProgressBar(pb1, 7)
  
  # create row of header data from the 6 rows that it is stored in
  header <- df.header %>%
    mutate(ens = as.factor(rep(1:(sum(tab$header)/6), each = 6))) %>%
    group_by(ens) %>%
    t %>%
    as.vector %>%
    enframe %>%
    spread(name, value)

setTxtProgressBar(pb1, 8)
  
  # turn header information into a dataframe and remove blank rows
  head.df <- matrix(header, ncol = 84, byrow = TRUE) %>%
    as.data.frame %>%
    select(1:70, 75:84) %>%
    lapply(., as.numeric) %>% 
    as.data.frame %>%
    mutate(ens = as.factor(V84)) %>%
    select(1:22, 28:33, 42:47, 56:61, 71, 72, ens) %>%
    mutate(cm = "cm") %>%
    mutate(BT = "BT") %>%
    mutate(counts = "counts")
  
setTxtProgressBar(pb1, 9)
  
  # get header spacing info
  rle.data <- rle(tab$header == TRUE)   #rle, creates list of lengths and values that repeat
  
setTxtProgressBar(pb1, 10)
  
  # remove the first element of the lists ( weird first row that isn't header and not data)
  na.len <- rle.data$lengths[-1]
  # remove header rows, gives number of lines/header rows to add to.
  na.len <- na.len[na.len > 6]
  
  na.values <- rle.data$values[-1]
  
setTxtProgressBar(pb1, 11)
  
  # Repeat the ensemble number based on the length of each ensemble(determined by the rle function)
  df.data$ens <- as.factor(rep(1:length(na.len), times = na.len))
  
setTxtProgressBar(pb1, 12)
  
  # Left join the ensemble information to the measurements of the ADCP
  all.data <- df.data %>%
    left_join(head.df, by = "ens")
  
setTxtProgressBar(pb1, 13)
  
  write_csv(all.data, paste(substr(file.list[i], 1, nchar(file.list[i]) - 4),".csv", sep = "") )
  
  
setTxtProgressBar(pb1, 14)
close(pb1)
setTxtProgressBar(pb, i)
close(pb)
}

