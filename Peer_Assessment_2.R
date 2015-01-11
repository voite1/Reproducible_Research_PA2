# I have selected INJURIES and FATALITIES to show casualties
# AND
# I have selected PROPDMG an CROPDMG as economic damage

# Read CSV file
data <- read.csv("repdata_data_StormData.csv", stringsAsFactors = FALSE)

# Get needed subset of the data
data <- data[, c("BGN_DATE", "PROPDMG", "CROPDMG", "EVTYPE", "INJURIES", "FATALITIES")]

# Make BGN_DATE to be a type of Date and setting up factors
data$BGN_DATE <- as.Date(data$BGN_DATE, "%m/%d/%Y 0:00:00")
data$EVTYPE <- as.factor(data$EVTYPE)

# aggregate INJURIES by EVTYPE
injuries <- data[, c("EVTYPE", "INJURIES")]
injuries <- aggregate(injuries$INJURIES, by=list(injuries$EVTYPE), FUN=sum, rm.na=TRUE)
colnames(injuries) <- c("EVTYPE", "INJURIES")
injuries <- injuries[with(injuries, order(-INJURIES)),]

# aggregate FATALITIES by EVTYPE
fatalities <- data[, c("EVTYPE", "FATALITIES")]
fatalities <- aggregate(fatalities$FATALITIES, by=list(fatalities$EVTYPE), FUN=sum, rm.na=TRUE)
colnames(fatalities) <- c("EVTYPE", "FATALITIES")
fatalities <- injuries[with(fatalities, order(-FATALITIES)),]

casualties <- merge(injuries,fatalities,by="EVTYPE")
tmp <- casualties["EVTYPE", sum(casualties$INJURIES", casualties$FATALITIES), ]





