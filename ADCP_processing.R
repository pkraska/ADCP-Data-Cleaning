# Function to process ADCP data into two flat files (header/data)

ADCPproc <- function(x, unit = "RiverRay") {
  # x is the data you wish to process unit is the type of unit you used to collect
  # data. RiverRay is the current default as it was the first data used to create
  # this script.
  
  require(tidyverse)
    
  # read data as rows of characters
  data <- readLines(x)
    
  # remove first two empty rows
  data <- data[-1:-2]
    
  if (unit == "RiverRay") {
    
    # create logical vector of whether or not the row is part of the header or not
    header.rows <- substr(data, 0, 1) != " "
    
    # read data as text file, creating a character column for data, then removing the
    # first two blank rows
    tab <- read_table(x, col_names = FALSE) %>% slice(3:n())
    
    # Add header.rows information to data so we can eventually filter by column
    tab$header <- header.rows
    colnames(tab) <- c("char", "header")
    
    # use rle (Run Length Encoding) function from base r in order to figure out the
    # ID of each ensemble
    tab.rle <- rle(tab$header)
    
    # create dataframe of the length and header/data sections.  TRUE = header, FALSE
    # = data, remove first row, and filter to only lengths of actual data
    df.rle <- data.frame(lengths = unlist(tab.rle$lengths), values = unlist(tab.rle$values)) %>% 
      slice(2:n()) %>% filter(values == FALSE)
    
    # create a reference for which header information UID is needed to be pasted to
    # each data row
    UID.ref <- rep(1:length(df.rle$lengths), df.rle$lengths)
    
    # Filter data into df.header remove header column, and separate char into columns
    df.header <- tab %>% filter(header == TRUE) %>% 
      select(char) %>% 
      separate(char, into = paste("x", c(1:13), sep = "."), sep = "\\s+")
    
    # Consolidate header information from 6 rows into 1 using cbind (column bind)
    header.long <- df.header[seq(from = 1, to = nrow(df.header), by = 6), ] %>% 
      cbind(df.header[seq(from = 2, to = nrow(df.header), by = 6), ]) %>% 
      cbind(df.header[seq(from = 3, to = nrow(df.header), by = 6), ]) %>% 
      cbind(df.header[seq(from = 4, to = nrow(df.header), by = 6), ]) %>% 
      cbind(df.header[seq(from = 5, to = nrow(df.header), by = 6), ]) %>% 
      cbind(df.header[seq(from = 6, to = nrow(df.header), by = 6), ])
    
    # rename column names as they copied the same names each time in previous step
    colnames(header.long) <- paste("v", seq(from = 1, to = ncol(header.long), 
                                            by = 1), sep = "")
    
    # Remove blank columns, and rename columns to human readable format
    header <- header.long %>% 
      select(-22:-26, -32:-39, -45:-52, -58:-65, -72:-78) %>% 
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
             longitude = v41) %>% 
      mutate(UID = paste(year, month, day, hour, second, hth_second, sep = ".")) %>% 
      select(UID, everything()) %>% write_csv("ADCP_HEADER.csv")
    
    # Filter data into df.data remove header column, and separate char into columns
    df.data <- tab %>% filter(header == FALSE) %>% 
      select(char) %>% 
      slice(2:n()) %>% 
      separate(char, into = paste("x", c(1:9), sep = "."), sep = "\\s+") %>% 
      mutate(UID = header$UID[UID.ref]) %>% 
      select(UID, everything()) %>% 
      write_csv("ADCP_DATA.csv")
  } else {
    message("Haven't finished writing script yet for other Teledyne ADCP units.")
  }
  
}