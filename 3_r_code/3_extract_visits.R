# Purpose ----
  #This script will take the merged file from previous steps and extract information
    # about every instance where a non focal bird was recorded at a box

# Settings and load data ----
  #Year.Analyzed <- 2020  # only needed if not set already in first script
  
  Minutes <- 2 # The threshold of time between reads to count as separate trips
  SecondsTh <- 30 # threshold in seconds to look for focal pair presense
  
## Read in data ----
  d <- read.delim(here::here("5_file_output", paste(Year.Analyzed, "AllVisitsMerged.txt", sep = "_")))

  rfidref <- read.delim(here::here("2_reference_files/FULLRFIDReference.txt"))
  colnames(rfidref)[2] <- "year_rfid_on"
  
  ## Read in ages file. This has the age and sex of all possible birds from 2013 on.
    tres_ages <- read.delim(here("2_reference_files", "tres_ages.txt"))
    ages_long <- as.data.frame(pivot_longer(tres_ages, cols = starts_with("age"), values_to = "age", names_to = "age_lab"))
    age_fix <- data.frame(age_lab = paste0("age", seq(2013, 2020, 1)), year = seq(2013, 2020, 1))
    ages_long <- join(ages_long, age_fix, "age_lab")
    ages_long <- ages_long[, c("rfid", "sex", "known_age", "age", "year")]
    ages_long$rfid_yr <- paste(ages_long$rfid, ages_long$year, sep = "_")

## Split out non-focals ---- 
  
# Make a new object that contains only the observations of non-box owners visiting other boxes. This cuts down the size of the
# object enormously, but some of the following loops are still a bit slow.
    d$rfid_yr <- paste(d$rfid, d$year, sep = "_")
    d2 <- join(d, ages_long[, c("sex", "known_age", "age", "rfid_yr")], "rfid_yr", "left", "first")
    
    
    d2 <- join(d2, rfidref[, c("band", "rfid")], "rfid", type = "left", match = "first")
    d2 <- subset(d2, d2$sex == "male" | d2$sex == "female")
    d2 <- subset(d2, d2$age > 0) # these are nestlings that returned in later years as adults so they have known sex
    
    all_vis <- subset(d2, d2$focal == "No")

# Make column of unique visitor-box combinations to be used for filtering here.

    all_vis$uniq_box <- paste(all_vis$rfid, all_vis$unitbox, sep = "_")
    uniq_list <- unique(all_vis$uniq_box)
    ul <- data.frame(uniq_box = unique(all_vis$uniq_box))
    ul <- join(ul, all_vis, "uniq_box", "left", "first")

    
# Loop visit characteristics ----    
# This loop goes through each unique visitor-box combination and identifies individual visits based on the time interval
# between subsequent RFID timestamps for the same bird. A series of reads with intervals less than the threshold set above
# are recorded as a single 'visit' with a start time, end time, duration, and number of reads. These reads are then collapsed
# to a single row per unique visit.	


    for(i in 1:length(uniq_list)){
      sub <- subset(all_vis, all_vis$uniq_box == uniq_list[i])
      if(nrow(sub) > 0){
        for(k in 1:(nrow(sub) - 1)){
          sub$Gap[1] <- 0
          if(nrow(sub) > 1){
            if(nrow(sub) > 2){
              temp <- c(sub$FullTime[1], sub$FullTime[1:(nrow(sub)-1)])
              sub$Gap <- sub$FullTime - temp
            }
            if(nrow(sub) < 3){
              sub$Gap[k + 1] <- sub$FullTime[k + 1] - sub$FullTime[k]
            }
            
          }
        }
        sub$VisitNumb[1] <- 1
        if(nrow(sub) > 1){
          for(p in 2:nrow(sub)){
            sub$VisitNumb[p] <- ifelse(sub$Gap[p] > Minutes*60, sub$VisitNumb[p-1] + 1, sub$VisitNumb[p - 1])
          }
        }
        for(n in 1:nrow(sub)){
          sub$VisitStart[n] <- min(subset(sub$FullTime, sub$VisitNumb == sub$VisitNumb[n]))
          sub$VisitEnd[n] <- max(subset(sub$FullTime, sub$VisitNumb == sub$VisitNumb[n]))
          sub$VisitReads[n] <- length(subset(sub$FullTime, sub$VisitNumb == sub$VisitNumb[n]))
        }
        sub$Duration <- sub$VisitEnd - sub$VisitStart
        
        sub <- subset(sub, !duplicated(VisitNumb))
        
        if(i == 1){List.Visits <- sub}
        
        if(i > 1){List.Visits <- rbind(List.Visits, sub)}
      }
      print(paste(i, " of ", length(uniq_list), sep = ""))
    }

# Add stage of visitor ----      
## Join in the breeding stage of the visiting bird
    # This uses the RFID ref and breeding stage matrix built in the first script
        breed <- pivot_longer(RFIDRef, cols = 13:262, names_to = "yday", values_to = "visitor_offset")
        breed2 <- data.frame(rfid = c(breed$f_rfid, breed$m_rfid), yday = rep(breed$yday, 2), visitor_offset = rep(breed$visitor_offset, 2))
        breed2$rfid_day <- paste(breed2$rfid, breed2$yday, sep = "_")
        
        List.Visits$rfid_day <- paste(List.Visits$rfid, List.Visits$yday, sep = "_")
        List.Visits <- join(List.Visits, breed2[, c("rfid_day", "visitor_offset")], "rfid_day", "left", "first")

    # Join to nest box of the visiting bird
        v_ub <- data.frame(rfid = c(RFIDRef$f_rfid, RFIDRef$m_rfid), visitor_ubox = rep(RFIDRef$unitbox, 2))
        List.Visits <- join(List.Visits, v_ub, "rfid", "left", "first")
 
# Add presence of focal pair ----
      # This takes a pretty long time to run. Could probably be improved somehow, but it works fine just slow.  
        for(y in 1:nrow(List.Visits)){
          sub1<-subset(d, d$rfid==List.Visits$f_rfid[y] & 
                         as.character(d$unitbox) == as.character(List.Visits$unitbox[y]))
          
          sub1$Dif1 <- List.Visits$VisitStart[y] - sub1$FullTime
          sub1$Dif2 <- List.Visits$VisitEnd[y] - sub1$FullTime
          
          sub2 <- subset(sub1, sub1$Dif1 < SecondsTh & sub1$Dif1 > 0)
          List.Visits$fFemPBef[y] <- ifelse(nrow(sub2) > 0, 1, 0)
          sub2 <- subset(sub1, sub1$Dif1 < 0 & sub1$Dif2 > 0)
          List.Visits$fFemPDur[y] <- ifelse(nrow(sub2) > 0, 1, 0)
          sub2 <- subset(sub1, sub1$Dif2 < SecondsTh & sub1$Dif2 > 0)
          List.Visits$fFemPAft[y] <- ifelse(nrow(sub2) > 0, 1, 0)
          
          sub1 <- subset(d, d$rfid==List.Visits$m_rfid[y] & 
                         as.character(d$unitbox)==as.character(List.Visits$unitbox[y]))
          
          sub1$Dif1 <- List.Visits$VisitStart[y] - sub1$FullTime
          sub1$Dif2 <- List.Visits$VisitEnd[y] - sub1$FullTime
          
          sub2 <- subset(sub1, sub1$Dif1 < SecondsTh & sub1$Dif1 > 0)
          List.Visits$fMalPBef[y] <- ifelse(nrow(sub2) > 0, 1, 0)
          sub2 <- subset(sub1, sub1$Dif1 < 0 & sub1$Dif2 > 0)
          List.Visits$fMalPDur[y] <- ifelse(nrow(sub2) > 0, 1, 0)
          sub2 <- subset(sub1, sub1$Dif2 < SecondsTh & sub1$Dif2 > 0)
          List.Visits$fMalPAft[y] <- ifelse(nrow(sub2) > 0, 1, 0)
          
          print(paste(y,"of",nrow(List.Visits),sep=" "))
        }	     
        
        
# Save output files ----    
      List.Visits <- join(List.Visits, v_ub, "rfid", "left", "first")
      List.Visits$VisitorUnitBoxDate <- paste(List.Visits$visitor_ubox, List.Visits$yday, sep = "_")



      write.table(List.Visits, here("5_file_output", paste(Year.Analyzed, "Individual_Visits.txt", sep = "_")), sep = "\t")



## EXTRA ----

library(viridis)
ggplot(data = subset(List.Visits, List.Visits$age < 6), mapping = aes(x = visitor_offset, fill = as.factor(age))) +
  geom_density(alpha = 0.5) + facet_wrap(~sex, nrow = 2) + scale_fill_viridis(discrete = TRUE) +
  theme(legend.position = "bottom")

List.Visits$rfid_ubd <- paste(List.Visits$rfid_day, List.Visits$unitbox, sep = "_")
lv2 <- List.Visits[!duplicated(List.Visits$rfid_ubd), ]
ggplot(data = lv2, mapping = aes(y = Offset, x = visitor_offset, fill = "coral3")) +
  geom_jitter(alpha = 0.4, shape = 21) + facet_wrap(~sex, nrow = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") + geom_hline(yintercept = 0, linetype = "dashed") +
  guides(fill = FALSE, color = FALSE)

ggplot(data = lv2, mapping = aes(x = Offset, color = as.factor(age), fill = as.factor(age), by = rfid)) +
  geom_density(alpha = 0.4, binwidth = 1) + facet_wrap(~sex, nrow = 3) + theme_classic() +
  scale_fill_viridis(discrete = TRUE) + scale_color_viridis(discrete = TRUE) +
  guides(color = FALSE) + 
  geom_vline(xintercept = c(0, -14, 25), linetype = "dashed") +
  theme(legend.position = "bottom")


d$ubd <- paste(d$unitbox, d$yday, sep = "_")

dx2 <- d %>%
  group_by(ubd) %>% 
  summarise(total_reads = n())

dx2$year <- 2020
dd <- d[, c("unitbox", "yday", "ubd")]
dx2 <- join(dx2, dd, "ubd", "left", "first")
ggplot(dx2, mapping = aes(x = yday, y = total_reads)) + 
  geom_point(mapping = aes(color = unitbox)) + 
  geom_smooth() +
  guides(color = FALSE)


#write.table(dx2, "2020_reads_by_day.txt", sep = "\t")


#d18 <- read.csv(here::here("5_file_output/AllVisitsMerged_18.csv"))