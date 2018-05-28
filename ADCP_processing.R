# Function to process ADCP data into two flat files (header/data)
x <- "vr065_553.adcp" 

ADCPproc <- function(x, unit = "RiverRay") {
  # x is the data you wish to process unit is the type of unit you used to collect
  # data. RiverRay is the current default as it was the first data used to create
  # this script.
  
  require(tidyverse)
  require(data.table)

  # read data as rows of characters
    data <- readLines(x) %>%
    trimws(., which = 'both') %>%
    data.table(.) %>%
    select(read = 1) %>%
    separate(read, into = paste("V", c(1:13), sep = ""), sep = "\\s+")
    
  
  # Header rows are identified by the 'cm' and 'BT' in the character strings.
  # RiverRay header is 6 lines, while the COERS ADCP is 5.
  regex.header <- regexpr("[a-z]+", readLines(x)) > 0
  header <- rep(NA, length(regex.header))
  
  # Loop over the dataframe looking for the head == TRUE and then change the
  # previous 4 header items to TRUE as well.
  if (unit == "COERS") {
    for (i in 1:length(header)) {
      if (regex.header[i] == TRUE) {
        header[(i - 4):i] <- TRUE
      } else {
        header[i] <- FALSE
      }
    }
  }
  
  data$header <- header
  
  if (unit == "RiverRay") {
    for (i in 1:length(data$header)) {
      if (data$header[i] == TRUE) {
        data$header[(i - 5):i] <- TRUE
      }
    }
  }
  
  # Create data frame of RLE (run length encoding) of the TRUE/FALSE header for the
  # length of each TRUE/FALSE section and trim off the first FALSE as it is part of
  # discarded data
  header.rle <- data.frame(lengths = unlist(rle(data$header)$lengths), values = unlist(rle(data$header)$values)) %>% 
    slice(2:n())
  
  # Create a unique ID for each section of data to link to the header section
  UID.ref <- rep(1:length(header.rle$lengths[header.rle$values == FALSE]), header.rle$lengths[header.rle$values == FALSE])
  
  ensemble.data <- data %>%
    slice(4:n()) %>%
    filter(header == FALSE) %>%
    mutate(UID = UID.ref) %>%
    select(UID, everything()) 
  
  ensemble.header.data <- data %>%
    slice(4:n()) %>%
    filter(header == TRUE) %>%
    select(-header)
  
  if (unit == "COERS") {
    ensemble.header <- ensemble.header.data[seq(from = 1, to = nrow(ensemble.header.data), by = 5), ] %>% 
      cbind(ensemble.header.data[seq(from = 2, to = nrow(ensemble.header.data), by = 5), ]) %>% 
      cbind(ensemble.header.data[seq(from = 3, to = nrow(ensemble.header.data), by = 5), ]) %>% 
      cbind(ensemble.header.data[seq(from = 4, to = nrow(ensemble.header.data), by = 5), ]) %>% 
      cbind(ensemble.header.data[seq(from = 5, to = nrow(ensemble.header.data), by = 5), ])   
    
    # rename column names as they copied the same names each time in previous step
    colnames(ensemble.header) <- paste("v", seq(from = 1, to = ncol(ensemble.header), 
                                                by = 1), sep = "")
    # Remove blank columns, and rename columns to human readable format
    ensemble.header.clean <- ensemble.header %>% 
      select(-22:-26, -33:-39, -49:-52, -55:-65) %>% 
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
             adcp_temp = v13) %>% 
      mutate(UID = paste(year, month, day, hour, second, hth_second, sep = ".")) %>% 
      select(UID, everything()) %>% 
      write_csv("ADCP_HEADER.csv")
  }
  
  if (unit == "RiverRay") {
    ensemble.header <- ensemble.header.data[seq(from = 1, to = nrow(ensemble.header.data), by = 6), ] %>% 
      cbind(ensemble.header.data[seq(from = 2, to = nrow(ensemble.header.data), by = 6), ]) %>% 
      cbind(ensemble.header.data[seq(from = 3, to = nrow(ensemble.header.data), by = 6), ]) %>% 
      cbind(ensemble.header.data[seq(from = 4, to = nrow(ensemble.header.data), by = 6), ]) %>% 
      cbind(ensemble.header.data[seq(from = 5, to = nrow(ensemble.header.data), by = 6), ]) %>%
      cbind(ensemble.header.data[seq(from = 6, to = nrow(ensemble.header.data), by = 6), ])
    
    ensemble.header.clean <- ensemble.header %>% 
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
      select(UID, everything()) %>% 
      write_csv("ADCP_HEADER.csv")
  }

}