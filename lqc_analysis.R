
# The encoding of the script is UTF-8
# R version 3.1.1 (2014-07-10) -- "Sock it to Me"
# Platform: i386-w64-mingw32/i386 (32-bit)
# Operating system: Windows 7 (SP1)
# 20140821 qingcheng

# Couresra-[Reproducible Research][repdata-005][Course Project 2]

# This project involves exploring the U.S. National Oceanic and Atmospheric 
# Administration's (NOAA) storm database. This database tracks characteristics 
# of major storms and weather events in the United States, including when and 
# where they occur, as well as estimates of any fatalities, injuries, and 
# property damage.

# change the locale in English environment
Sys.setlocale('LC_ALL', 'English')
sessionInfo()


# Download and read the dataset
DataUrl<-
    "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
filename = "StormData.csv.bz2"

if (file.exists(filename)){
    print("storm data file already exists locally")
}else{
    print("storm data file not found locally, so dowload from website")
    download.file(DataUrl, destfile=filename)
    dateDownloaded <- date()
    dateDownloaded
}

if(exists("stormData")){
    print("stormData already loaded into R")
}else{
    print("load stormData")
    stormData <- read.csv(bzfile(filename))
}

# Size of loaded data
print(object.size(stormData), units = "MB")
str(stormData)


# selecting only columns relevant to the analysis
data <- stormData[, c("BGN_DATE", "STATE", "EVTYPE", "FATALITIES", "INJURIES",
                      "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
#column names to lower case
colnames(data) <- tolower(colnames(data))  

data$date <- as.Date(data$bgn_date, format = "%m/%d/%Y")
mindate <- min(data$date)
maxdate <- max(data$date)

#all event types to upper case and refactored
data$evtype <- factor(toupper(data$evtype))  
# all exponents defining unit of measurement of costs to upper case and refactored
data$propdmgexp <- factor(toupper(data$propdmgexp))
data$cropdmgexp <- factor(toupper(data$cropdmgexp))
levels(data$cropdmgexp)
levels(data$propdmgexp)


# define multipliers for valid exponents
validExpo <- data.frame(c("", "0", "H", "K", "M", "B"), 
                        c(1, 1, 10^2, 10^3, 10^6, 10^9))
colnames(validExpo) <- c("validexp", "multiplier")

# subset data retaining only records with valid exponent
data <- subset(data, (cropdmgexp %in% validExpo$validexp) &
                   (propdmgexp %in%  validExpo$validexp))
nrow(data)

# convert damage values in number
colnames(validExpo) <- c("validexp", "propdmgmultiplier")
data <- merge(data, validExpo, by.x = "propdmgexp", by.y = "validexp")
data$propdmg <- (data$propdmg * data$propdmgmultiplier)

colnames(validExpo) <- c("validexp", "cropdmgmultiplier")
data <- merge(data, validExpo, by.x = "cropdmgexp", by.y = "validexp")
data$cropdmg <- (data$cropdmg * data$cropdmgmultiplier)


# Population Health
library(plyr)
Fatalities <- aggregate(x = data$fatalities, by = list(data$evtype), FUN = sum)
names(Fatalities) <- c("evtype", "fatalities")
Fatalities <- arrange(Fatalities, fatalities, decreasing = TRUE)
Top10Fatalities <- head(Fatalities, n = 10)
Top10Fatalities <- within(Top10Fatalities, 
                          evtype <- factor(x = evtype, levels = Fatalities$evtype))

Injuries <- aggregate(x = data$injuries, by = list(data$evtype), FUN = sum)
names(Injuries) <- c("evtype", "injuries")
Injuries <- arrange(Injuries, injuries, decreasing = TRUE)
Top10Injuries <- head(Injuries, n = 10)
Top10Injuries <- within(Top10Injuries, 
                        evtype <- factor(x = evtype, levels = Top10Injuries$evtype))


# Economic Consequences
PropertyDmg <- aggregate(x = data$propdmg, by = list(data$evtype), FUN = sum)
names(PropertyDmg) <- c("evtype", "propdmg")
PropertyDmg <- arrange(PropertyDmg, propdmg, decreasing = TRUE)
Top10PropertyDmg <- head(PropertyDmg, n = 10)
Top10PropertyDmg <- within(Top10PropertyDmg, evtype <- 
                               factor(x = evtype, levels = Top10PropertyDmg$evtype))


CropDmg <- aggregate(x = data$cropdmg, by = list(data$evtype), FUN = sum)
names(CropDmg) <- c("evtype", "cropdmg")
CropDmg <- arrange(CropDmg, cropdmg, decreasing = TRUE)
Top10CropDmg <- head(CropDmg, n = 10)
Top10CropDmg <- within(Top10CropDmg, 
                       evtype <- factor(x = evtype, levels = Top10CropDmg$evtype))


# Results
library(ggplot2)
library(gridExtra)

plot1 <- qplot(evtype, data = Top10Fatalities, weight = fatalities, geom = "bar", 
               binwidth = 1) + scale_y_continuous("Number of Fatalities") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    xlab("Event Type") + ggtitle("Total Fatalities by Event Type in the U.S.")

plot2 <- qplot(evtype, data = Top10Injuries, weight = injuries, geom = "bar", 
               binwidth = 1) + scale_y_continuous("Number of Injuries") + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    xlab("Event Type") + ggtitle("Total Injuries by Event Type in the U.S.")
grid.arrange(plot1, plot2, ncol = 2)


plot3 <- qplot(evtype, data = Top10PropertyDmg, weight = propdmg, geom = "bar", 
               binwidth = 1) + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    ylab("Property Damage in US dollars") + 
    xlab("Event Type") + ggtitle("Total Property Damage by Event Type in the U.S.")

plot4 <- qplot(evtype, data = Top10CropDmg, weight = cropdmg, geom = "bar", 
               binwidth = 1) + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    ylab("Crop Damage in US dollars") + 
    xlab("Event Type") + ggtitle("Total Crop Damage by Event Type in the U.S.")
grid.arrange(plot3, plot4, ncol = 2)
