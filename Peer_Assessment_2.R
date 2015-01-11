# I have selected INJURIES and FATALITIES to show affect on population health
# AND
# I have selected PROPDMG an CROPDMG as indicators of economic damage (heaviest consequences)

# Read CSV file
data <- read.csv("repdata_data_StormData.csv", stringsAsFactors = FALSE)

# Get needed subset of the data - total of 5 variables
data <- data[, c("PROPDMG", "CROPDMG", "EVTYPE", "INJURIES", "FATALITIES")]

# Make EVTYPE to be factor variable
data$EVTYPE <- as.factor(data$EVTYPE)

# aggregate INJURIES by EVTYPE
injuries <- data[, c("EVTYPE", "INJURIES")]
injuries <- aggregate(injuries$INJURIES, by=list(injuries$EVTYPE), FUN=sum, rm.na=TRUE)
colnames(injuries) <- c("EVTYPE", "INJURIES")

# aggregate FATALITIES by EVTYPE
fatalities <- data[, c("EVTYPE", "FATALITIES")]
fatalities <- aggregate(fatalities$FATALITIES, by=list(fatalities$EVTYPE), FUN=sum, rm.na=TRUE)
colnames(fatalities) <- c("EVTYPE", "FATALITIES")

# Combining FATALITIES and INJURIES into single variable called TOTAL
casualties <- merge(injuries,fatalities,by="EVTYPE")
casualties$TOTAL <- casualties$INJURIES + casualties$FATALITIES

# Sorting the data frame in scending order
casualties <- casualties[with(casualties, order(-TOTAL)),]

# Selecting only largest 5 rows to plot
casualties <- casualties[1:5,]

# Reorder the data inside the casualties data frame
casualties <- transform(casualties, EVTYPE = reorder(EVTYPE, order(TOTAL, decreasing=TRUE)))

# Graph casualties$EVTYPE vs casualties$TOTAL using ggplot2 nicely
library(ggplot2)
ggplot(casualties, aes(EVTYPE, TOTAL)) + geom_bar(stat="identity") + xlab("Event Type") +
       ylab("Number of casualties") + ggtitle("5 Harmful Types of Climate Events") +
       theme(axis.text.x=element_text(angle=90, vjust=0.5)) 







