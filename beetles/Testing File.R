## Beetle data playground


# load dependencies
library(neonUtilities)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(plotly)

# Grab all the data (takes like 10 minutes)
beetData <- loadByProduct("DP1.10022.001")
names(beetData)

#archBeet <- beetData[1]
fieldBeet <- beetData[3]


#write.csv(beetData[5], "sortData.csv")
write.csv(beetData[3], "fieldData.csv")
#beetles <- fread("fieldData.csv")
sort <- fread("sortData.csv")

sites <- c("CPER","STER","RMNP","NIWO")
carabids <- c("carabid")
# Dates
sort$bet_sorting.setDate <- as.Date(sort$bet_sorting.setDate)


sort <- sort[!(is.na(sort$bet_sorting.taxonID) | sort$bet_sorting.taxonID == ""),]


# Initial filter for the test plot
sort2018 <- sort %>%
  filter(bet_sorting.setDate > "2014-01-01" & bet_sorting.setDate < "2018-01-01") %>%
  #filter(bet_sorting.sampleType %in% carabids) %>%
  select(bet_sorting.domainID,bet_sorting.siteID,bet_sorting.collectDate,bet_sorting.plotID,bet_sorting.trapID,
         bet_sorting.setDate,bet_sorting.sampleType,bet_sorting.taxonID,bet_sorting.scientificName,
         bet_sorting.taxonRank,bet_sorting.individualCount,bet_sorting.identificationQualifier,bet_sorting.nativeStatusCode,
         bet_sorting.identifiedBy) %>%
  mutate(year = substring(bet_sorting.setDate,1,nchar(bet_sorting.setDate) -1)) %>%
  mutate(monthDay = substring(bet_sorting.setDate, 6)) %>%
  mutate(genus = word(bet_sorting.scientificName,1))
  Dataframe1$COL2 <- word(Dataframe2$COL1,1)



sort2018$monthDay <- as.Date(sort2018$monthDay, format = "%m-%d")
## Factorize that data frame
sort2018 <- as.data.frame(unclass(sort2018))
sort <- as.data.frame(unclass(sort))
## Sort data by idenified by to make the bar plot pretty
sortFactor <- within(sortFactor, 
                     bet_sorting.identifiedBy <- factor(bet_sorting.identifiedBy, 
                                      levels=names(sort(table(bet_sorting.identifiedBy), 
                                                        decreasing=FALSE))))
## summary for reference
summary(sortFactor)
 
## Test Plots

## Plot of most beetle IDs
plot<- ggplot(sortFactor, aes(x= bet_sorting.identifiedBy,fill = bet_sorting.identifiedBy, color = bet_sorting.identifiedBy))+
  geom_bar() +
  labs(title = "Density Plot of Beetle Counts from the CPER",
       x = "Date",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 270)) 
  #facet_wrap(~year)

start <- Sys.time()
ggplotly(plot)
end <- Sys.time()
end - start


## Plot of most common species


sort <- within(sort, 
                     bet_sorting.scientificName <- factor(bet_sorting.scientificName, 
                                                        levels=names(sort(table(bet_sorting.scientificName), 
                                                                          decreasing=FALSE))))

plot<- ggplot(sort, aes(x= bet_sorting.scientificName,fill = bet_sorting.scientificName, color = bet_sorting.scientificName))+
  geom_bar() +
  labs(title = "Density Plot of Beetle Counts from the CPER",
       x = "Date",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 270)) 
#facet_wrap(~year)

start <- Sys.time()
ggplotly(plot)
end <- Sys.time()
end - start


## Plot of plotID 

sort <- within(sort, 
                     bet_sorting.plotID <- factor(bet_sorting.plotID, 
                                                          levels=names(sort(table(bet_sorting.plotID), 
                                                                            decreasing=FALSE))))

plot<- ggplot(sort, aes(x= bet_sorting.plotID,fill = bet_sorting.plotID, color = bet_sorting.plotID))+
  geom_bar() +
  labs(title = "Density Plot of Beetle Counts from the CPER",
       x = "Date",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 270)) 
#facet_wrap(~year)

start <- Sys.time()
ggplotly(plot)
end <- Sys.time()
end - start


## plot by domain ID
sort2018f <- sort2018 %>%
  filter(bet_sorting.siteID %in% sites)

sort2018f <- within(sort2018f, 
                    genus <- factor(genus, 
                                                  levels=names(sort2018f(table(genus), 
                                                                     decreasing=FALSE))))

plot<- ggplot(sort2018, aes(x= monthDay,fill = genus))+
  geom_density(alpha = .35) +
  labs(title = "Total Beetles Counts by Site ID for D10.13 in 2017",
       x = "Domain",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 270)) +
  facet_wrap(~bet_sorting.siteID)

start <- Sys.time()
ggplotly(plot)
end <- Sys.time()
end - start
  

