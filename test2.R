## Loading and preprocessing the data

    #Reading and confirming data
    zipfile <- "repdata_data_StormData.csv.bz2"
    filename <- "repdata_data_StormData.csv.bz2"
    fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    
    #Zip already exists?
    if (!file.exists(zipfile)){
        download.file(fileURL, zipfile)
    }
    
    #Store data for reading - direct from bz2
    arquivoOrigin <- read.table(filename, 
                          sep = ",",
                          header = TRUE,
                          dec = ".",
                          na.strings = "")
    
    #Re-storing dates values for better work
    arquivo <- arquivoOrigin      
    
    #Convert date to easy work
    library(lubridate)
    arquivo$BGN_DATE <-  mdy_hms(arquivo[,2])

    #Convert lower case exponent letter codes
    arquivo$PROPDMGEXP <- toupper(as.character(arquivo$PROPDMGEXP))
    arquivo$CROPDMGEXP <- toupper(as.character(arquivo$CROPDMGEXP))
    
    # Convert codes to "NA", to not interfe with analysis 
    arquivo$PROPDMGEXP[(arquivo$PROPDMGEXP == "-")] <- "NA"
    arquivo$PROPDMGEXP[(arquivo$PROPDMGEXP == "+")] <- "NA"
    arquivo$PROPDMGEXP[(arquivo$PROPDMGEXP == "?")] <- "NA"
    arquivo$CROPDMGEXP[(arquivo$CROPDMGEXP == "-")] <- "NA"
    arquivo$CROPDMGEXP[(arquivo$CROPDMGEXP == "+")] <- "NA"
    arquivo$CROPDMGEXP[(arquivo$CROPDMGEXP == "?")] <- "NA"
    
    #Convert missing values "" to 0, to not interfe with analysis  
    arquivo$FATALITIES[(arquivo$FATALITIES == "")] <- 0
    arquivo$INJURIES[(arquivo$INJURIES == "")] <- 0
    arquivo$PROPDMG[(arquivo$PROPDMG == "")] <- 0
    arquivo$CROPDMG[(arquivo$CROPDMG == "")] <- 0
    arquivo$PROPDMGEXP[(arquivo$PROPDMGEXP == "")] <- 0
    arquivo$CROPDMGEXP[(arquivo$CROPDMGEXP == "")] <- 0
    
    #Transform character codes to numeric, to 
    #calculate exponents
    arquivo$PROPDMGEXP[(arquivo$PROPDMGEXP == "H")] <- 2
    arquivo$PROPDMGEXP[(arquivo$PROPDMGEXP == "K")] <- 3
    arquivo$PROPDMGEXP[(arquivo$PROPDMGEXP == "M")] <- 6
    arquivo$PROPDMGEXP[(arquivo$PROPDMGEXP == "B")] <- 9
    arquivo$CROPDMGEXP[(arquivo$CROPDMGEXP == "H")] <- 2
    arquivo$CROPDMGEXP[(arquivo$CROPDMGEXP == "K")] <- 3
    arquivo$CROPDMGEXP[(arquivo$CROPDMGEXP == "M")] <- 6
    arquivo$CROPDMGEXP[(arquivo$CROPDMGEXP == "B")] <- 9
    
    #Transform exponents as integers for easy calculation
    arquivo$PROPDMGEXP <- as.integer(arquivo$PROPDMGEXP)
    arquivo$CROPDMGEXP <- as.integer(arquivo$CROPDMGEXP)
    
    
## Across the United States, which types of events 
## (as indicated in the EVTYPE) are most harmful with 
## respect to population health?
    
    # Check for missing values from EVTYPE
    any(is.na(arquivo$EVTYPE))
    # No NAs.
    
    # Check what is the most harmful event,
    # by Fatalities and Injuries
    
    #Grouping the sum of Fatalities by Event and
    #ordering in descending way to check most harmful
    
    library(dplyr)
    #Fatality
    evfat <- aggregate(FATALITIES ~ EVTYPE,
                       arquivo,
                       sum)
    
    evfat <- evfat %>% arrange(desc(FATALITIES))
    print(paste("Most frequent fatality: ", evfat[1,]))
    
    #Injury
    evinj <- aggregate(INJURIES ~ EVTYPE,
                       arquivo,
                       sum)
    
    evinj<- evinj %>% arrange(desc(INJURIES))
    print(paste("Most frequent injury: ", evinj[1,]))
    
    par(mfrow=c(1,2), mar=c(2,2,2,2), oma=c(4,2,2,2), cex=0.8)
    barplot(evfat$FATALITIES, names.arg=evfat$EVTYPE, las=2,
            cex.names=0.8, xlab="", ylab="Fatalities", col="blue",
            main="Events with highest Fatalities")
    barplot(evinj$INJURIES, names.arg=evinj$EVTYPE, las=2, cex.names=0.8,
            xlab="", ylab="Injuries", col="blue", 
            main="Events with highest Injuries")
    
        
## Across the United States, which types of events have 
## the greatest economic consequences?
    
  
    # Calculate actual property and crop damage value
    arquivo$PROPDMGTOTAL <- arquivo$PROPDMG * 10^arquivo$PROPDMGEXP
    arquivo$CROPDMGTOTAL <- arquivo$CROPDMG * 10^arquivo$CROPDMGEXP
    
    # Calculate total financial value of the damage
    arquivo$TOTALDMG <- arquivo$PROPDMGTOTAL + arquivo$CROPDMGTOTAL
    
    
    #Grouping the sum of Property Damage by Event and
    #ordering in descending way to check who has greatest 
    #economic consequences
    
    #Property grouping
    evpd <- aggregate(PROPDMGTOTAL ~ EVTYPE,
                       arquivo,
                       sum)
    evpd <- evpd %>% arrange(desc(PROPDMGTOTAL))
    par(mfrow=c(1,2), mar=c(2,2,2,2), oma=c(4,2,2,2), cex=0.8)
    barplot(evpd$PROPDMGTOTAL/10^6, names.arg=evpd$EVTYPE, las=2,
            cex.names=0.8, xlab="", ylab="Property Damage", col="red",
            main="Events with highest Property Damages")
   
    #Crop grouping
    evcd <- aggregate(CROPDMGTOTAL ~ EVTYPE,
                      arquivo,
                      sum)
    evcd <- evcd %>% arrange(desc(CROPDMGTOTAL))
    barplot(evcd$CROPDMGTOTAL/10^6, names.arg=evcd$EVTYPE, las=2,
            cex.names=0.8, xlab="", ylab="Crop Damage", col="blue",
            main="Events with highest Crop Damages")
    
     
    
    