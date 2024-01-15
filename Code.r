# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)

# Download the dataset
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, "repdata%2Fdata%2FStormData.csv.bz2")

# Read the dataset
dataset <- read.csv("repdata%2Fdata%2FStormData.csv.bz2", header = TRUE, sep = ",")

# Clean and process the PROPDMGEXP and CROPDMGEXP columns
dataset$PROPDMGEXP <- suppressWarnings(case_when(
  dataset$PROPDMGEXP %in% c('K', 'k') ~ 3,
  dataset$PROPDMGEXP %in% c('M', 'm') ~ 6,
  dataset$PROPDMGEXP %in% c('B', 'b') ~ 9,
  dataset$PROPDMGEXP %in% c('H', 'h') ~ 2,
  !is.na(as.numeric(dataset$PROPDMGEXP)) ~ as.numeric(dataset$PROPDMGEXP),
  dataset$PROPDMGEXP %in% c("", "-", "?", "+") ~ 0,
  TRUE ~ 0
))

dataset$CROPDMGEXP <- suppressWarnings(case_when(
  dataset$CROPDMGEXP %in% c('K', 'k') ~ 3,
  dataset$CROPDMGEXP %in% c('M', 'm') ~ 6,
  dataset$CROPDMGEXP %in% c('B', 'b') ~ 9,
  dataset$CROPDMGEXP %in% c('H', 'h') ~ 2,
  !is.na(as.numeric(dataset$CROPDMGEXP)) ~ as.numeric(dataset$CROPDMGEXP),
  dataset$CROPDMGEXP %in% c("", "-", "?", "+") ~ 0,
  TRUE ~ 0
))

# Convert values in CROPDMG and PROPDMG columns based on their respective exponents
dataset$CROPDMG <- dataset$CROPDMG * 10 ** dataset$CROPDMGEXP
dataset$PROPDMG <- dataset$PROPDMG * 10 ** dataset$PROPDMGEXP

# Create summarized datasets based on health hazards and economic damage
datasetByHealthHazard <- dataset %>% group_by(EVTYPE) %>% summarise(FATALITIES = sum(FATALITIES), INJURIES = sum(INJURIES))
datasetByEcoDamage <- dataset %>% group_by(EVTYPE) %>% summarise(CROPDMG = sum(CROPDMG), PROPDMG = sum(PROPDMG))

# Order datasets based on fatalities, injuries, property damage, and crop damage
datasetByFatalities <- datasetByHealthHazard[order(datasetByHealthHazard$FATALITIES, decreasing = TRUE),]
datasetByInjuries <- datasetByHealthHazard[order(datasetByHealthHazard$INJURIES, decreasing = TRUE),]
datasetByCropDamage <- datasetByEcoDamage[order(datasetByEcoDamage$CROPDMG, decreasing = TRUE),]
datasetByPropDamage <- datasetByEcoDamage[order(datasetByEcoDamage$PROPDMG, decreasing = TRUE),]

# Display the top 5 rows of the datasetByEcoDamage
head(datasetByEcoDamage)

# Display the top 5 rows of each ordered dataset
datasetByFatalities <- head(datasetByFatalities, 5)
print(datasetByFatalities)

datasetByInjuries <- head(datasetByInjuries, 5)
print(datasetByInjuries)

datasetByPropDamage <- head(datasetByPropDamage, 5)
print(datasetByPropDamage)

datasetByCropDamage <- head(datasetByCropDamage, 5)
print(datasetByCropDamage)

# Create plots for fatalities, injuries, property damage, and crop damage
p1 <- ggplot(data = datasetByFatalities,
             aes(x = reorder(EVTYPE, FATALITIES), y = FATALITIES, fill = EVTYPE)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ylab("Total number of FATALITIES") +
    xlab("Event type")

p2 <- ggplot(data = datasetByInjuries,
             aes(x = reorder(EVTYPE, INJURIES), y = INJURIES, fill = EVTYPE)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ylab("Total number of INJURIES") +
    xlab("Event type")

p3 <- ggplot(data = datasetByPropDamage,
             aes(x = reorder(EVTYPE, PROPDMG), y = PROPDMG, fill = EVTYPE)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ylab("Total Value of Property Damage Dollars.") +
    xlab("Event type")

p4 <- ggplot(data = datasetByCropDamage,
             aes(x = reorder(EVTYPE, CROPDMG), y = CROPDMG, fill = EVTYPE)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ylab("Total Value of Crop Damage Dollars.") +
    xlab("Event type")

# Arrange and display the plots in a grid
grid.arrange(p1, p2, nrow = 2)
grid.arrange(p3, p4, nrow = 2, top = "Economic Impact of Weather")
