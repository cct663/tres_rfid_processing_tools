# Purpose ----
# This script should take the all visits file created in the previous script and use it to calculate
# hourly and daily feeding rates at each nest.

# Load data and settings ----
## Set year analyzed
    #Year.Analyzed <- 2020  # only needed if not set already in first script

## Read the file back in (not necessary if run just after script 3) 
    d <- read.delim(here("5_file_output", paste(Year.Analyzed, "AllVisitsMerged.txt", sep = "_")))
    d2 <- d   # just doing this to save the unmodified version also

# Time thresholds Vitousek et al 2018 ----    
## This applies time thresholds to feeding visits based on Vitousek et al. 2018 PRSB.
# The thresholds were determined from video files. See supplement of that paper.
    thresholds <- as.data.frame(matrix(nrow = 24, ncol = 2))
    colnames(thresholds) <- c("Offset", "Limit")
    thresholds$Offset <- seq(from = 1, to = 24, by = 1)
    thresholds$Limit <- c(rep(136.5, 3), rep(55.5, 3), rep(36.5, 3),
                          rep(20, 3), rep(11, 3), rep(25.5, 9))

## Add the thresholds to the main data frame
    d2<-join(d2, thresholds, "Offset")

# Data wrangling ----    
    ## Save only visits by the focal pair. Also remove days before hatching
        d2 <- subset(d2, d2$focal == "Yes" & d2$Offset > 0)
    
    
    ## Add more time columns
        d2$Time2 <- as.character(d2$Time)
        d2$Hour <- floor(d2$Seconds / 60 / 60)
    
    ## Add some columns that combine box info
        d2$daycombo <- paste(d2$unitbox, d2$yday, sep = "-")
        d2$hourcombo <- paste(d2$unitbox, d2$yday, d2$Hour, sep = "-")
        byday <- as.data.frame(unique(d2$daycombo))
        colnames(byday) <- "daycombo"
        byhour <- as.data.frame(unique(d2$hourcombo))
        colnames(byhour) <- "hourcombo"
    
    ## Make separate male and female objects
        d2f <- subset(d2, as.character(d2$f_rfid) == as.character(d2$rfid))
        d2m <- subset(d2, as.character(d2$m_rfid) == as.character(d2$rfid))
        
    ## Make sure dataframes are ordered by unitbox and time
        d2f <- d2f[order(d2f$unitbox, d2f$FullTime), ]
        d2m <- d2m[order(d2m$unitbox, d2m$FullTime), ]
    
    ## Add in the time of the next read to each row 
        d2f$FullTimeTneg1 <- c(0, d2f$FullTime[1:(nrow(d2f) - 1)])
        d2m$FullTimeTneg1 <- c(0, d2m$FullTime[1:(nrow(d2m) - 1)])
    
    ## Calculate time gap between subsequent reads   
        d2f$Gap <- d2f$FullTime - d2f$FullTimeTneg1
        d2m$Gap <- d2m$FullTime - d2m$FullTimeTneg1
    
    ## Remove reads that are too close together based on thresholds  
        d3f <- subset(d2f, d2f$Gap > d2f$Limit)
        d3m <- subset(d2m, d2m$Gap > d2m$Limit)
    
    ## Make empty objects to be populated later for by day and by hour feeding rates
        byday <- join(byday, d2, "daycombo", type = "left", match = "first")
        byhour <- join(byhour, d2, "hourcombo", type = "left", match = "first")
    
    ## discarding some columns that aren't needed
        byday <- byday[, c("daycombo", "unitbox", "yday", "year",
                           "f_rfid", "m_rfid", "Offset", "Limit")]
        byhour <- byhour[, c("hourcombo", "unitbox", "yday", "year",
                             "f_rfid", "m_rfid", "Offset", "Limit", "Hour")]

# Calculate feeding ----    
    ## Go through a loop to calculate the by day feeding trips  
    for(i in 1:nrow(byday)){
      ## subset for males and females that match the day and unitbox for FEEDING trips
          subf <- subset(d3f, d3f$daycombo == byday$daycombo[i] & 
                           d3f$unitbox == byday$unitbox[i])
          subm <- subset(d3m, d3m$daycombo == byday$daycombo[i] &
                           d3m$unitbox == byday$unitbox[i])
      
      ## add in number of FEEDING trips based on rows in the subset  
          byday$FemFeed[i] <- nrow(subf)
          byday$MalFeed[i] <- nrow(subm)
      
      ## subset for males and females that match day and unit box for TOTAL reads  
          sub2f <- subset(d2f, d2f$daycombo == byday$daycombo[i] &
                            d2f$unitbox == byday$unitbox[i])
          sub2m <- subset(d2m, d2m$daycombo == byday$daycombo[i] &
                            d2m$unitbox == byday$unitbox[i])
      
      ## add in number of TOTAL rfid reads    
          byday$FemTotal[i] <- nrow(sub2f)
          byday$MalTotal[i] <- nrow(sub2m)
      
      ## print progress through the loop to console 
          print(paste(i,"of",nrow(byday),sep=" "))
    }
    
    ## Now do the dame thing except by hour
    for(i in 1:nrow(byhour)){
      ## subset for males and females that match the hour and unitbox for FEEDING trips
          subf <- subset(d3f, d3f$hourcombo == byhour$hourcombo[i] & 
                           d3f$unitbox == byhour$unitbox[i])
          subm <- subset(d3m, d3m$hourcombo == byhour$hourcombo[i] &
                           d3m$unitbox == byhour$unitbox[i])
      
      ## Add in the total number of feeding trips   
          byhour$FemFeed[i] <- nrow(subf)
          byhour$MalFeed[i] <- nrow(subm)
      
      ## subset for males and females that match the hour and unit box for TOTAL reads 
          sub2f <- subset(d2f, d2f$hourcombo == byhour$hourcombo[i] &
                            d2f$unitbox == byhour$unitbox[i])
          sub2m <- subset(d2m, d2m$hourcombo == byhour$hourcombo[i] &
                            d2m$unitbox == byhour$unitbox[i])
      
      ## Add in the total number of rfid reads 
          byhour$FemTotal[i] <- nrow(sub2f)
          byhour$MalTotal[i] <- nrow(sub2m)
      
      ## print the progress through the loop 
          print(paste(i,"of",nrow(byhour),sep=" "))
      
    }
    
    ## Replace 0 values with NAs since those are likely no data for various reasons
        byhour[,"FemFeed"][byhour[,"FemFeed"]==0]<-NA
        byhour[,"MalFeed"][byhour[,"MalFeed"]==0]<-NA
        byday[,"FemFeed"][byday[,"FemFeed"]==0]<-NA
        byday[,"MalFeed"][byday[,"MalFeed"]==0]<-NA
        byhour[,"FemTotal"][byhour[,"FemTotal"]==0]<-NA
        byhour[,"MalTotal"][byhour[,"MalTotal"]==0]<-NA
        byday[,"FemTotal"][byday[,"FemTotal"]==0]<-NA
        byday[,"MalTotal"][byday[,"MalTotal"]==0]<-NA

# Write files ----    
## write the output of feeding rate per day and per hour to file
    write.table(byhour, here("5_file_output", paste(Year.Analyzed, "feed_by_hour.txt", sep = "_")), sep = "\t")
    write.table(byday, here("5_file_output", paste(Year.Analyzed, "feed_by_day.txt", sep = "_")), sep = "\t")