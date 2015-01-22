# I have selected INJURIES and FATALITIES to show affect on population health
# AND
# I have selected PROPDMG an CROPDMG as indicators of economic damage (heaviest consequences)

# Loading needed libraries
library(ggplot2)

# Getting data from the web
if (file.exists("repdata_data_StormData.csv.bz2") & file.exists("repdata_peer2_doc_pd01016005curr.pdf")) {
    print ("Using existing data file repdata_data_StormData.csv.bz2")
} else {
    data_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(data_url, "repdata_data_StormData.csv.bz2", mode="wb")
    
    doc_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf"
    download.file(doc_url, "repdata_peer2_doc_pd01016005curr.pdf")
}

# Reading data file
my_file <- bzfile("repdata_data_StormData.csv.bz2", "r")
data <- read.csv(my_file, stringsAsFactors = FALSE)
close(my_file)

# Get needed subset of the data - total of 5 variables
data <- data[, c("PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP", "EVTYPE", "INJURIES", "FATALITIES")]

# Make EVTYPE to be factor variable
data$EVTYPE <- as.factor(data$EVTYPE)

######################### QUESTION 1 ############################################################

# aggregate INJURIES by EVTYPE
injuries <- data[, c("EVTYPE", "INJURIES")]
injuries <- aggregate(injuries$INJURIES, by=list(injuries$EVTYPE), FUN=sum, rm.na=TRUE)
colnames(injuries) <- c("EVTYPE", "INJURIES")

# aggregate FATALITIES by EVTYPE
fatalities <- data[, c("EVTYPE", "FATALITIES")]
fatalities <- aggregate(fatalities$FATALITIES, by=list(fatalities$EVTYPE), FUN=sum, rm.na=TRUE)
colnames(fatalities) <- c("EVTYPE", "FATALITIES")

# Combining FATALITIES and INJURIES into single variable called TOTAL
himpact <- merge(injuries,fatalities,by="EVTYPE")
himpact$TOTAL <- himpact$INJURIES + himpact$FATALITIES

# Sorting the data frame in scending order
himpact <- himpact[with(himpact, order(-TOTAL)),]

# Selecting only largest 5 rows to plot
himpact <- himpact[1:10,]

# Reorder the data inside the himpact data frame
himpact <- transform(himpact, EVTYPE = reorder(EVTYPE, order(TOTAL, decreasing=TRUE)))

# Showing final data set that will be plotted
head(himpact, 10)

# Graph himpact$EVTYPE vs himpact$TOTAL using ggplot2 nicely
ggplot(himpact, aes(EVTYPE, TOTAL)) + geom_bar(stat="identity") + xlab("Event Type") +
    ylab("Number of incidents") + ggtitle("Frequence of 10 Most Harmful Events") +
    theme(axis.text.x=element_text(angle=90, vjust=0.5)) 

######################### QUESTION 2 ############################################################

# Separate "PROPDMG" and "CROPDMG" into separate data frames
propdmg <- data[, c("EVTYPE", "PROPDMG", "PROPDMGEXP")]
propdmg <- na.omit(propdmg)
cropdmg <- data[, c("EVTYPE", "CROPDMG", "CROPDMGEXP")]
cropdmg <- na.omit(cropdmg)

# Converting all the exponents into the upper case
propdmg[, "PROPDMGEXP"] <- toupper(propdmg[, "PROPDMGEXP"])
cropdmg[, "CROPDMGEXP"] <- toupper(cropdmg[, "CROPDMGEXP"])

# Normalize monetary damage to be represented in dollars
# function to update the data based on the exponent
transf_exp <- function(exp) {
    if (exp %in% c("H"))
        return(2)
    else if (exp %in% c("K"))
        return(3)
    else if (exp %in% c("M"))
        return(6)
    else if (exp %in% c("B"))
        return(9)
    else if (suppressWarnings(!is.na(as.numeric(exp))))
        return(suppressWarnings(as.numeric(exp)))
    else {
        return(0)
    }
}

# Converting to actual numbers PROPDMG and CROPDMG, aggregating by EVTYPE,
# and assigning column names
# propdmg
exponent <- sapply(propdmg$PROPDMGEXP, FUN=transf_exp)
propdmg$PROPDMG <- propdmg$PROPDMG * (10 ** exponent)
propdmg$EVTYPE <- as.factor(propdmg$EVTYPE)
propdmg <- aggregate(propdmg$PROPDMG, by=list(propdmg$EVTYPE), FUN=sum, rm.na=TRUE)
colnames(propdmg) <- c("EVTYPE", "PROPDMG")

# cropdmg
exponent <- sapply(cropdmg$CROPDMGEXP, FUN=transf_exp)
cropdmg$CROPDMG <- cropdmg$CROPDMG * (10 ** exponent)
cropdmg$EVTYPE <- as.factor(cropdmg$EVTYPE)
cropdmg <- aggregate(cropdmg$CROPDMG, by=list(cropdmg$EVTYPE), FUN=sum, rm.na=TRUE)
colnames(cropdmg) <- c("EVTYPE", "CROPDMG")

# Creating economic impact variable eimpact and building TOTAL
eimpact <- merge(propdmg,cropdmg,by="EVTYPE")
eimpact$TOTAL <- eimpact$PROPDMG + eimpact$CROPDMG

# Sorting the data frame in scending order
eimpact <- eimpact[with(eimpact, order(-TOTAL)),]

# Selecting only largest 5 rows to plot
eimpact <- eimpact[1:15,]

eimpact$TOTAL <- eimpact$TOTAL / 1000000000

# Reorder the data inside the himpact data frame
eimpact <- transform(eimpact, EVTYPE = reorder(EVTYPE, order(TOTAL, decreasing=TRUE)))

# Showing final data set that will be plotted
head(eimpact, 15)

# Plot eimpact@TOTAL by eimpact@EVTYPE
ggplot(eimpact, aes(EVTYPE, TOTAL)) + geom_bar(stat="identity") + xlab("Event Type") +
    ylab("Health Impact (Billions)") + ggtitle("15 Most Harmful Events Affecting Health") +
    theme(axis.text.x=element_text(angle=90, vjust=0.5)) 
