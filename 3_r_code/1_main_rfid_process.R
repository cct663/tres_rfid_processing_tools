# Script information ----
## This code is written to process RFID files used on tree swallow nest
## boxes. See readme file and notes in code for full set of instructions.
## Last updated 3/2/2021
## Written by Conor Taff: cct663@gmail.com
##
## INSTRUCTIONS:
##
## see readme text file for more directions on setting up files
##

## The code is meant to be run in order to process a full batch of RFID files

# Load packages ----
# Load packages used in code. These may need to be installed the first time
pacman::p_load(plyr, lubridate, reshape2, igraph, here, beepr)

# Adjustable settings ----

## Set the year that is being analyzed. The point of this is to delete any
## old data that might have been on the board by mistake.

Year.Analyzed <- 2020

# Read reference files ----

## Dates and info file. This has RFID info paired with nest dates (hatching,
## initiation, fledge/failure). The unit-box column must match the unitbox
## name used in the saved file name. Multiple nests by the same birds should
## be OK, but unique box numbers should be used (e.g. 1_20 & 1_20_2).
RFIDRef <- read.delim(here("2_reference_files", "RFIDRef.txt"))

# Subset to the year being analyzed
RFIDRef <- subset(RFIDRef, RFIDRef$year == Year.Analyzed)

## Full RFID reference. This is the full RFID reference library. It should list
## every possible RFID code that has been deployed to look for matches. At 
## some point the oldest ones can probably be removed. The file lists the year
## that bird was initially banded and sex (M/F or N for nestling), but doesn't
## include other info and should only have one entry per unique RFID code. The
## whole point of this file is to link the RFID code to a band number. This sex
## needs to be replaced later on for nestlings that returned as adults.
RFIDFULL <- read.delim(here("2_reference_files", "FULLRFIDReference.txt"))

# Build breeding stage ----
## Construct a breeding stage reference file from the RFIDRef file. This just
## uses the dates already in the RFIDRef file to make the large breeding stage
## matrix that says what stage each nest was at on each day of the season.

br_stg <- as.data.frame(matrix(nrow = 250, ncol = nrow(RFIDRef)))

## Now filling in full matrix of breeding stages
for(i in 1:nrow(RFIDRef)){  
  
  vec <- rep(NA, 250) ## filling for doy 0 to 250
  ## For all nests
  # Fill in post fate known date
  vec[RFIDRef$j_fate[i]:250] <- seq(30, (30 + 250 - RFIDRef$j_fate[i]), 1)
  # Fill in pre-laying
  vec[1:(RFIDRef$j_first_egg[i] - 1)] <- 
    seq(-RFIDRef$j_first_egg[i] - 28, -30, 1)
  # Fill in for laying
  vec[RFIDRef$j_first_egg[i]:(RFIDRef$j_incub[i] - 1)] <-
    seq(RFIDRef$j_first_egg[i] - RFIDRef$j_incub[i] - 19, -20, 1)
  
  ## For nests that succesfully hatched
  if(is.na(RFIDRef$j_hatch[i]) == FALSE){
    # Fill in from hatch to fate known
    vec[RFIDRef$j_hatch[i]:(RFIDRef$j_fate[i] - 1)] <- 
      seq(0, RFIDRef$j_fate[i] - RFIDRef$j_hatch[i] - 1, 1)
    # Fill in incubation
    vec[RFIDRef$j_incub[i]:(RFIDRef$j_hatch[i] - 1)] <-
      seq(RFIDRef$j_incub[i] - RFIDRef$j_hatch[i], -1, 1)
    
  }
  
  ## For nests that failed before hatching
  if(is.na(RFIDRef$j_hatch[i]) == TRUE){
    # Fill in from incubation to failure date
    vec[RFIDRef$j_incub[i]:(RFIDRef$j_fate[i] - 1)] <-
      seq(RFIDRef$j_incub[i] - RFIDRef$j_fate[i], -1, 1)
  }
  
  
  br_stg[, i] <- vec
}

br_stg2 <- as.data.frame(t(br_stg))
colnames(br_stg2) <- seq(1, 250)
br_stg2$unitbox <- RFIDRef$unitbox
RFIDRef <- join(RFIDRef, br_stg2, "unitbox")

# Build other objects ---- 
## This section sets up a few objects that will be used later on.

## This reads in the list of file names (should be "unit.box.mm.dd.yyyy.txt" files).
## Naming convention is important because unit and box info will be stripped from 
## the file name. The date part actually doesn't matter because each read has a timestamp.
DoList <- list.files(here("0_input_data", Year.Analyzed)) ## Folder with only RFID files to process

## This sets up an object that will be used to calculate the total number of raw RFID hits on each
# date in the season for each file included in the DoList. This can be used to check if there
# are missing days in nests and to calculate how many boards were active on any given day.			
days <- seq(from = 80, to = 240, by = 1)
Schedule <- as.data.frame(matrix(nrow = length(days), ncol = length(DoList) + 1))
colnames(Schedule) <- c("Days", DoList)
Schedule[1:length(days), 1] <- days






# Cleaning Loop ----

## Must prepare reference files first
# Below here is where the file processing starts in earnest. Everything above was just setting up
# objects and reference files to be used later on.

## This is the first big for loop that starts processing each file.
# This one does a few different things, including:
# 1. Read raw RFID text and use a regular expression to strip all lines before the hits.
# 2. Add columns to the reads by stripping unit and box from file name and joining to 
# reference tables that have additional columns.
# 3. For unknown RFID numbers, calculate how similar they are to the known male and female
# at this box (# of digits different) and if they are 1 or 2 digigs off swap the RFID
# number out for the focal bird (this corrects for mis-reads that are common).
# Note that it is possible that this correction is not perfect in all cases, but 
# some inspection of a few files suggested that it does a very good job.
# 4. Fills the rows of the 'Schedule' file described above.
# 5. Rearranges some more columns and saves a modified csv file in a new directory to 
# be used in the next big for loop below.

### Note that R sometimes gives some warnings at the end of this loop about invalid factor levels or 
### incomplete final lines, etc. For the most part, these are fine (they might result in one lost line
### of RFID output). You might be able to resolve them through formatting of the raw text files, but 
### if the output is being produced correctly (check the modified files) they are probably safely ignored.

for(j in 1:length(DoList)){
  
  # I'm not really sure what this does, but some files caused an error due to certain
  # characters being present and google suggested this as a solution and it does
  # seem to suppress that error...sometimes
  
  Sys.setlocale('LC_ALL', 'C')
  
  # Set the first file to process
  
  file <- DoList[j]
  
  # Read in all the lines from the first file and then use a regular expression to find
  # the start of the actual reads by matching to '????Data'. Store the start position
  # returned and then read in the file again as an R object with just the relevant lines.
  
  Lines <- readLines(here("0_input_data", Year.Analyzed, file))
  markers <- grepl("????DATA", Lines)
  starter <- rle(markers)$lengths[1] + 2
  c <- read.delim(here("0_input_data", Year.Analyzed, file), sep = " ", skip = starter, header = FALSE)
  
  # Rename columns and join to reference sheets. Use the file name to create a 'UnitBox'
  # column that can be used to join to other information.		
  
  colnames(c) <- c("rfid", "Date", "Time")
  c$rfid <- as.character(c$rfid)
  RFIDFULL$rfid <- as.character(RFIDFULL$rfid)
  c <- join(c, RFIDFULL, "rfid")
  NestDetail <- strsplit(DoList[j], "[.]")
  Box <- as.factor(paste(unlist(NestDetail)[1], unlist(NestDetail)[2], sep = "_"))
  c$unitbox <- rep(Box, nrow(c))
  c$unitbox <- as.character(c$unitbox)
  RFIDRef$unitbox <- as.character(RFIDRef$unitbox)
  c <- join(c, RFIDRef, "unitbox")
  
  # Oh god this is ugly! This pairs the RFID read to first the male and then the female
  # that own the box and compares each character one-by-one assigning a 1 if they 
  # match and a 0 if they don't. The 10 comparisons are summed so a 10 is a perfect
  # match. This value is saved in a new column. This is necessary because the readers
  # sometimes 'slip' and read a single digit wrong (how is that possible!!). So with 
  # a lot of reads for a focal bird you often get smaller peaks that are just one or
  # two characters different from the focal birds.
  
  c$MatchFemale <- ifelse(substr(c$rfid, 1, 1) == substr(c$f_rfid, 1, 1), 1, 0) +
    ifelse(substr(c$rfid, 2, 2) == substr(c$f_rfid, 2, 2), 1, 0) +
    ifelse(substr(c$rfid, 3, 3) == substr(c$f_rfid, 3, 3), 1, 0) +
    ifelse(substr(c$rfid, 4, 4) == substr(c$f_rfid, 4, 4), 1, 0) +
    ifelse(substr(c$rfid, 5, 5) == substr(c$f_rfid, 5, 5), 1, 0) +
    ifelse(substr(c$rfid, 6, 6) == substr(c$f_rfid, 6, 6), 1, 0) +
    ifelse(substr(c$rfid, 7, 7) == substr(c$f_rfid, 7, 7), 1, 0) +
    ifelse(substr(c$rfid, 8, 8) == substr(c$f_rfid, 8, 8), 1, 0) +
    ifelse(substr(c$rfid, 9, 9) == substr(c$f_rfid, 9, 9), 1, 0) +
    ifelse(substr(c$rfid, 10, 10) == substr(c$f_rfid, 10, 10), 1, 0)
  c$MatchMale<-ifelse(substr(c$rfid, 1, 1) == substr(c$m_rfid, 1, 1), 1, 0) +
    ifelse(substr(c$rfid, 2, 2) == substr(c$m_rfid, 2, 2), 1, 0) +
    ifelse(substr(c$rfid, 3, 3) == substr(c$m_rfid, 3, 3), 1, 0) +
    ifelse(substr(c$rfid, 4, 4) == substr(c$m_rfid, 4, 4), 1, 0) +
    ifelse(substr(c$rfid, 5, 5) == substr(c$m_rfid, 5, 5), 1, 0) +
    ifelse(substr(c$rfid, 6, 6) == substr(c$m_rfid, 6, 6), 1, 0) +
    ifelse(substr(c$rfid, 7, 7) == substr(c$m_rfid, 7, 7), 1, 0) +
    ifelse(substr(c$rfid, 8, 8) == substr(c$m_rfid, 8, 8), 1, 0) +
    ifelse(substr(c$rfid, 9, 9) == substr(c$m_rfid, 9, 9), 1, 0) +
    ifelse(substr(c$rfid, 10, 10) == substr(c$m_rfid, 10, 10), 1, 0)
  
  # This for loop goes through and looks at each RFID hit that did not match any known
  # birds in the full reference database. Presumably, these are mostly mis-reads.
  # It then asks whether these numbers are only 1 or 2 characters different from
  # the focal birds at the nest. If the answer is yes then it replaces the read
  # with that of the focal bird that matches.	
  
  for(r in 1:nrow(c)){	
    ifelse(is.na(c$Band[r]) == TRUE,
           ifelse(c$MatchFemale[r] > 7,
                  c$rfid[r] <- c$f_rfid[r],
                  ifelse(c$MatchMale[r] > 7,
                         c$rfid[r] <- c$m_rfid[r],
                         c$rfid[r] <- c$rfid[r])),
           c$rfid[r] <- c$rfid[r])	
  }	
  
  # This just repeats the comparisons made earlier to confirm that it has worked right.
  # The new column should show that columns that were NA but had focal match scores
  # >7 now have focal match scores of 10. These loops are slow and can be turned off most of
  # the time by setting DoOrDoNot to 0 as long as you are confident it is working (it should).
  
  DoOrDoNot <- 0
  
  if(DoOrDoNot == 1){
    c$MatchFemale2 <- ifelse(substr(c$rfid, 1, 1) == substr(c$f_rfid, 1, 1), 1, 0) +
      ifelse(substr(c$rfid, 2, 2) == substr(c$f_rfid, 2, 2), 1, 0) +
      ifelse(substr(c$rfid, 3, 3) == substr(c$f_rfid, 3, 3), 1, 0) +
      ifelse(substr(c$rfid, 4, 4) == substr(c$f_rfid, 4, 4), 1, 0) +
      ifelse(substr(c$rfid, 5, 5) == substr(c$f_rfid, 5, 5), 1, 0) +
      ifelse(substr(c$rfid, 6, 6) == substr(c$f_rfid, 6, 6), 1, 0) +
      ifelse(substr(c$rfid, 7, 7) == substr(c$f_rfid, 7, 7), 1, 0) +
      ifelse(substr(c$rfid, 8, 8) == substr(c$f_rfid, 8, 8), 1, 0) +
      ifelse(substr(c$rfid, 9, 9) == substr(c$f_rfid, 9, 9), 1, 0) +
      ifelse(substr(c$rfid, 10, 10) == substr(c$f_rfid, 10, 10), 1, 0)
    c$MatchMale2<-ifelse(substr(c$rfid, 1, 1) == substr(c$m_rfid, 1, 1), 1, 0) +
      ifelse(substr(c$rfid, 2, 2) == substr(c$m_rfid, 2, 2), 1, 0) +
      ifelse(substr(c$rfid, 3, 3) == substr(c$m_rfid, 3, 3), 1, 0) +
      ifelse(substr(c$rfid, 4, 4) == substr(c$m_rfid, 4, 4), 1, 0) +
      ifelse(substr(c$rfid, 5, 5) == substr(c$m_rfid, 5, 5), 1, 0) +
      ifelse(substr(c$rfid, 6, 6) == substr(c$m_rfid, 6, 6), 1, 0) +
      ifelse(substr(c$rfid, 7, 7) == substr(c$m_rfid, 7, 7), 1, 0) +
      ifelse(substr(c$rfid, 8, 8) == substr(c$m_rfid, 8, 8), 1, 0) +
      ifelse(substr(c$rfid, 9, 9) == substr(c$m_rfid, 9, 9), 1, 0) +
      ifelse(substr(c$rfid, 10, 10) == substr(c$m_rfid, 10, 10), 1, 0)
  }
  
  # This strips out most of the (now extraneous) columns that we have made and then
  # joins anew to the full RFID reference sheet (results will differ now because
  # mis-reads have been subbed out).
  
  c <- c[, 1:3]
  c <- join(c, RFIDFULL, "rfid")
  
  # This gets rid of any more RFID reads that are still not matching. There should not
  # be many of these, but some could be mis-reads of other bands (rare) or reads of
  # the tag that we use for testing when deploying (more likely) or reads of birds
  # that never made it into the full RFID reference spreadsheet somehow (possible).
  
  c <- subset(c, is.na(c$band) == FALSE)	
  
  # Now that is all done, strip down to the essential info and join to the JDates
  # object to convert dates to meaningful values.
  
  c <- c[,1:3]
  c$yday <- yday(mdy(c$Date))
  c$year <- year(mdy(c$Date))
  
  ## This creates a new object that has only observations of the focal female and then
  # sums up the number of hits for that female on each day of the season and populates
  # the 'Schedule' object with that info. As of 9/28, this object isn't really used in
  # any subsequent analysis, but is useful for error checking and verification to make
  # sure that RFID records are complete, unit was functioning, and nest was active.		
  
  RFIDRef$rfid <- RFIDRef$f_rfid
  rr <- join(c, RFIDRef, "rfid")
  rr <- subset(rr, as.character(rr$rfid) == as.character(rr$f_rfid))
  
  for(u in 1:nrow(Schedule)){
    y <- subset(rr, rr$JDate == Schedule[u, 1])
    y <- subset(y, y$Year == Year.Analyzed)
    Schedule[u, j + 1] <- nrow(y)
  }
  
  ### JUST FOR 2019!!! #####
  ## In this year there were some birds that had PIT tags swapped. I'm just going to gsub out 
  ## the different PIT numbers so that we can work with a single PIT number for each bird.
  if(Year.Analyzed == 2019){
    if(nrow(c) > 2){
      c$rfid <- gsub("041959918C", x = c$rfid, replacement = "04198EAC13")
      c$rfid <- gsub("04198E765C", x = c$rfid, replacement = "04198E12B6")
      c$rfid <- gsub("04198E327F", x = c$rfid, replacement = "04198E4CEE")
      c$rfid <- gsub("04198E7F1B", x = c$rfid, replacement = "04195984C1")
      c$rfid <- gsub("04198E4F2D", x = c$rfid, replacement = "04198E4711")
      c$rfid <- gsub("04198E530D", x = c$rfid, replacement = "04198E2053")
      c$rfid <- gsub("04198E9960", x = c$rfid, replacement = "04198DA1A7")
      c$rfid <- gsub("04198EAA7D", x = c$rfid, replacement = "04198E8595")
    }
  }
  # Same thing for 2020 -> Need to fill in list from Jenny
  #if(Year.Analyzed == 2020){
  #  if(nrow(c) > 2){
  #    c$rfid <- gsub()
  #  }
  #}
  
  ## This changes the working directory to the empty folder where modified text files will be
  # saved and then writes a new copy of the original text file as "Modified.OriginalName.txt".
  # This file will be used in the next step of processing.
  
  write.csv(c, here("1_modified_data", Year.Analyzed, paste("Modified", DoList[j], sep = ".")))
  
  ## Print this at the end of each pass through the loop just to be able to see that R
  # is still working...
  
  print(paste("Cycle", j, "out of", length(DoList), sep = " "))
  
}



# This takes the schedule file created in the last loop and modifies it so that a '1' is recorded
# for every day where at least 10 RFID reads were recorded. This is to mark days where an RFID was 'active'
# in case we want to control for days of possible observation later on. It also saves an unaltered
# version of the Schedule object as 'Schedule2'.


Schedule2 <- Schedule ## Save the old one before changing it
Schedule[1:nrow(Schedule), 2:ncol(Schedule)] <- 
  ifelse(Schedule[1:nrow(Schedule), 2:ncol(Schedule)] > 10, 1, 0)

## This will commonly produce these two errors that can be safely ignored:
## 1. 'incomplete final line': just means the last line was rfid text and not a PIT
## 2. 'failed to parse': this happens when there is a line of text with no date;
## those lines will not be saved anyway, so it is no problem.

## In addition to these two ignorable errors, the loop may crash for particular files.
## In those cases, you may need to find the offending files to fix them. These are 
## the most common problems adn fixes:
## 1. Weird characters that don't parse. This happens sometimes when an RFID
## board is not working properly. Greek or Chinese or weird characters
## get inserted into line and cause the code to fail. Usually it is just
## a few at the start/end of a file. You can scroll through the raw
## text file and delete any that you find to try again.
## 2. Missing 'data' tag for start. The code looks for "????DATA" to find
## where to start parsing data lines. If that line was not copied in
## it will fail. You can just add a line above the first line of data
## and write 'xxxxDATA'
## 3. No data in file. If the file has just the notes but no PIT tag reads
## it may cause the code to fail. In this case I just move the file 
## out to a different folder not to be processed since there is not
## any data to use anyway.




# Merging Loop ----
# Beginning of the next chunk of code with another large loop that processes each of the cleaned up
# and modified files derived from the previous loop.

## Set the working directory to the folder where the modified text files are stored and read in a 
# new 'DoList' that has the modified file names.
DoList <- list.files(here("1_modified_data", Year.Analyzed))

## This is the second big for loop that works with the files generated above. This one
# makes a whole bunch of new columns and rearranges/summarizes data in a useful way.
# Note that errors occur when objects end up with 0 rows (i.e., no visitors). I've tried
# to avoid those by wrapping certain parts of the loop with 'if' statements that test for
# positive row number and these files should be OK to include now.
for(k in 1:length(DoList)){     
  
  ## set working directory and set 'file' to first item on DoList.
  file <- DoList[k]
  
  ## Read in the first file to process
  d <- read.delim(here("1_modified_data", Year.Analyzed, file), sep = ",", header = TRUE, 
                  colClasses = c(NA, "character", rep(NA, 4)))	
  
  ## Restrict it to just records in year being analyzed, set up top	
  d2 <- subset(d, d$year == Year.Analyzed)
  
  ## Check if there are actually any readings in this file. If not, skip it.
  if(nrow(d2) > 0){
    
    ##  Convert the time string to continuous seconds
    d2$Seconds <- period_to_seconds(hms(d2$Time))
    
    ## Strips the unit and box out of the file name, combines them into "UnitBox" column
    NestDetail <- strsplit(DoList[k], "[.]")
    Box <- as.factor(paste(unlist(NestDetail)[2], unlist(NestDetail)[3], sep = "_"))
    
    ## Make a "UnitBox" column that is just filled with the value created above from file name	
    d2$unitbox <- rep(Box, nrow(d2))
    
    ## Join the data object to the RFID reference sheet that has by nest info based on "UnitBox"
    d2 <- join(d2, RFIDRef, "unitbox", type = "left", match = "first")
    d2 <- d2[, 1:18]
    
    ## Make a new column that indicates whether a read is from the focal male/female or a different bird.
    d2$focal <- ifelse(as.character(d2$f_rfid) == as.character(d2$rfid), "Yes",
                       ifelse(as.character(d2$m_rfid) == as.character(d2$rfid), "Yes", "No"))
    
    ## Check on whether there actually are any records left after removing focal birds. If not,
    ## stop the loop here and proceed to the next file. If so, proceed with the loop. 
    if(nrow(d2)>0){
      
      ## Calculate the offset between observation and hatch date (e.g., 2 days before hatch
      ## would yield a result of -2)
      d2$Offset <- d2$yday - d2$j_hatch
      
      ## Take a subset of the file that is after the install date in case there was early
      ## season data from another nest included
      if(nrow(d2)>0){
        
        d2 <- subset(d2, d2$yday > d2$j_install - 1)
        
        ## Merge the version of d2 created so far into one giant ojbect that includes 
        # all observations recorded from the entire batch of files.
        if(k == 1){All.Visits.Merged <- d2}				
        if(k > 1){All.Visits.Merged <- rbind(All.Visits.Merged, d2)}
        
        ## End of the if loops from above
        
      }
    }
    
    ## Print 'Cycle x' on the R console so you can tell that the code is still running.
    print(paste("Cycle", k, "out of", length(DoList), sep = " "))
    
  }
  
}	

# Write the merged file ----
# At this point, the big loops have done their work and we can now focus just on the huge All.Visits.Merged
# object that should contain everything we need to get both feeding rate and network behavior. These
# steps are a bit more straightforward and can be modified if different outputs are required.


# Get rid of duplicate observations that are introduced by reading in overlapping RFID files. First makes a unique
# identifier column in All.Visits.Merged and then discards duplicates.
All.Visits.Merged$UniqueRecord <- paste(All.Visits.Merged$rfid, All.Visits.Merged$yday,
                                        All.Visits.Merged$Seconds, sep = "_")
All.Visits.Merged <- subset(All.Visits.Merged, !duplicated(UniqueRecord))

# Make a column that records time in continuous seconds starting with the first of the year. This is kind of silly, 
# but is easier to work with for calculating intervals, etc.
All.Visits.Merged$FullTime <- All.Visits.Merged$yday*24*60*60 + All.Visits.Merged$Seconds
write.table(All.Visits.Merged, here("5_file_output", "AllVisitsMerged.txt"), sep = "\t")



