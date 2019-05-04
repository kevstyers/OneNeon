# EDA for VST data
# import dataset
getwd()
setwd("X:/1_Databases/bigNeon/TOS")

library(dplyr)
library(ggplot2)
library(plotly)
library(data.table)

# mapping req
require(dplyr); require(RColorBrewer); require(ggplot2)
require(mapdata); require(maptools)

# load joined_gast


db <- data.table::fread("testVSTdb.csv", stringsAsFactors = FALSE)
db<-db[!duplicated(db$uid), ]


db$year <- substring(db$date, 1,4)
db$individualID <- substring(db$individualID, 14)
#db$year <- as.Date(db$year, format = "%y")
db$plotID <- as.factor(db$plotID)
db$domainID.x <- as.factor(db$domainID.x)
db$siteID.x <- as.factor(db$siteID.x)
db$individualID <- as.factor(db$individualID)
db$growthForm <- as.factor(db$growthForm)

# remove NA
sum(is.na(db$stemDiameter))
names(db)
db<-db[!(db$stemDiameter=="NA" | db$stemDiameter=="")]

write.csv(db, "vstDataBase.csv")
# # apparentindividual
# vstData_3 <- data.table::fread("woodyVSTCall_3.csv", stringsAsFactors = FALSE)
# names(vstData_3) <- c('uid','namedLocation',"date","eventID", "domainID","siteID","plotID","subplotID","individualID","tagStatus","growthForm","plantStatus","stemDiameter","measurementHeight","height","baseCrownHeight","breakHeight","breakDiameter","maxCrownDiameter","ninetyCrownDiameter","canopyPosition","shape","basalStemDiameter","basalStemDiameterMsrmntHeight","maxBaseCrownDiameter","ninetyBaseCrownDiameter","remarks","recordedBy","measuredBy","dataQF","tempShrubStemID")
# names(vstData_3)
# 
# ### Here :TODO: remove NA's and dupes to reduce nrows
# #
# #
# ### Here :TODO: remove NA's and dupes to reduce nrows
# 
# # mappingandtagging
# vstData_4 <- data.table::fread("woodyVSTCall_4.csv",stringsAsFactors = FALSE)
# names(vstData_4) <- c("uidMapNTag", "namedLocation",	"dateMapNTag",	"eventIDMapNTag", "domainID", "siteID", "plotID",	"subplotID",	"nestedSubplotID",	"pointID",	"stemDistance",	"stemAzimuth",	"recordType",	"individualID",	"supportingStemIndividualIDMapNTag",	"previouslyTaggedAsMapNTag",	"samplingProtocolVersionMapNTag",	"taxonIDMapNTag",	"scientificNameMapNTag",	"taxonRankMapNTag",	"identificationReferencesMapNTag",	"morphospeciesIDMapNTag",	"morphospeciesIDRemarksMapNTag",	"identificationQualifierMapNTag",	"remarksMapNTag",	"measuredByMapNTag",	"recordedByMapNTag",	"dataQFMapNTag")
# names(vstData_4)
# # perplotperyear
# vstData_5 <- data.table::fread("woodyVSTCall_5.csv",stringsAsFactors = FALSE)
# names(vstData_5) <- c("uid", "namedLocation", "date",	"domainID",	"siteID",	"plotID",	"plotType",	"nlcdClass",	"decimalLatitude",	"decimalLongitude",	"geodeticDatum",	"coordinateUncertainty",	"easting",	"northing",	"utmZone",	"elevation",	"elevationUncertainty",	"eventID",	"samplingProtocolVersion",	"treesPresent",	"treesAbsentList",	"shrubsPresent",	"shrubsAbsentList",	"lianasPresent",	"lianasAbsentList",	"nestedSubplotAreaShrubSapling",	"nestedSubplotAreaLiana",	"totalSampledAreaTrees",	"totalSampledAreaShrubSapling",	"totalSampledAreaLiana",	"remarks",	"measuredBy",	"recordedBy",	"dataQF")
# names(vstData_5)
# # remove plotID for join
# vstData_4 <- vstData_4 %>%
#   select(-plotID)
# 
# 
# # first join
# joined_data <- merge(vstData_3, vstData_4, by.x = "individualID", by.y = "individualID")
# names(joined_data)
# 
# #add year
# joined_data$year <- substring(joined_data$date, 1,4)
# joined_data$year <- as.Date(joined_data$year)
# str(joined_data)
# 
# #remove extra variables
# joined_test<-select(joined_data, -domainID.y,
#                     -subplotID.y, -siteID.y, -namedLocation.y,
#                     -samplingProtocolVersionMapNTag, -identificationReferencesMapNTag,
#                     -measuredByMapNTag, -recordedByMapNTag, -uidMapNTag,
#                     -measuredBy, -recordedBy)
# 
# # reduce number of variables for the apparentIndividuals csv.
# vst5_trans <- select(vstData_5, "plotID", "nlcdClass", "decimalLatitude", "decimalLongitude", "geodeticDatum", "easting", "northing", "elevation","nestedSubplotAreaShrubSapling",	"nestedSubplotAreaLiana",	"totalSampledAreaTrees",	"totalSampledAreaShrubSapling",	"totalSampledAreaLiana")
# names(vst5_trans)
# 
# joined_gast <- merge(joined_test, vst5_trans, by.x = "plotID", by.y = "plotID", allow.cartesian = TRUE)

# write db

# write.csv(joined_gast, "testVSTdb.csv")

##### TESTING AREA #####
#                      #
#                      #
#                      #    
##### TESTING AREA #####

testing <- db %>%
  filter(growthForm == "single bole tree" & taxonRankMapNTag == "species" & height < 60 & stemDiameter <120 )
  


unique(testing$taxonIDMapNTag)

# draw lines to all of the same individual ID
## would be cool if I could quantify the length of these lines, then I could account for noise,
## ie get rid of all lines > "some obtuse length" 
## I could also use it for comparing domainID plotID to see if there's any correlation
FullPlot <-ggplot(testing, aes(x=stemDiameter, y=height, color = year)) +
  geom_point() + 
  stat_summary(aes(group=individualID), fun.y=mean, geom="line", colour="green") +
  facet_wrap(~domainID.x) +
  xlab("Stem Diameter At DBH(130cm)") +
  ylab("Tree Height (m)") +
  #geom_vline(xintercept =10,color="red") +
  labs(title = "NEON's Single Bole Trees",
       subtitle = "Each point represents an IndividualID. A green line is drawn from each individualID for each year it was sampled.")# This can be used to find erroneuous data points, where the green line is very long.") 

BySite <- db %>%
  filter(domainID.x == "D01", growthForm == "single bole tree" & taxonRankMapNTag == "species")

sitePlot <-ggplot(BySite, aes(x=stemDiameter, y=height, color = year, group=individualID)) +
  geom_point() + 
  stat_summary(aes(group=individualID), fun.y=mean, geom="line", colour="green") +
  facet_wrap(~siteID.x) +
  xlab("Stem Diameter At DBH(130cm)") +
  ylab("Tree Height (m)") +
  #geom_vline(xintercept =10,color="red") +
  labs(title = "NEON's Single Bole Trees",
       subtitle = "Each point represents an IndividualID. A green line is drawn from each individualID for each year it was sampled.")# This can be used to find erroneuous data points, where the green line is very long.") 
  

start <- Sys.time()
sitePlot
end <- Sys.time()
end- start


start <- Sys.time()
# can't include species info. Maybe just provide a table they can punch that number into
ggplotly(sitePlot,tooltip=c("x","y","individualID"))
end <- Sys.time()
end- start

data.table(BySite)

spec <- unique(BySite$taxonIDMapNTag)


start <- Sys.time()
testPlotly
end <- Sys.time()
end- start

start <- Sys.time()
ggplotly(testPlotly)
end <- Sys.time()
end- start



target <- "ARTR2"
joined_gast$plotID <- as.factor(joined_gast$plotID)
joined_gast$domainID.x <- as.factor(joined_gast$domainID.x)
joined_gast$siteID.x <- as.factor(joined_gast$siteID.x)
joined_gast$individualID <- as.factor(joined_gast$individualID)
joined_gast$growthForm <- as.factor(joined_gast$growthForm)
joined_gast$year <- as.Date(joined_gast$year, format = "y")


#experimenting with MApping
usa <- map_data("usa")
canada <- map_data("worldHires", "Canada")
mexico <- map_data("worldHires", "Mexico")
NAmap <- ggplot() + geom_polygon(data = usa, 
                                 aes(x=long, y = lat, group = group), 
                                 fill = "white", 
                                 color="black") +
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") + 
  geom_polygon(data = mexico, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") +
  
  coord_fixed(xlim = c(-100, -65),  ylim = c(25, 50), ratio = 1.2) 
testMap <- NAmap + geom_point(data = specialNEONMapping, aes(x=decimalLongitude,y=decimalLatitude, color = domainID.x))
testMap

ggplotly(testMap)

summary(testing1$decimalLatitude)
unique(testing1$domainID.x)
ggplotly(testMap)
testing1 <- joined_gast %>%
  select(domainID.x,siteID.x,plotID, individualID,growthForm,taxonIDMapNTag, year, height, stemDiameter, decimalLatitude, decimalLongitude)

specialNEONMapping <- joined_gast %>%
  select(domainID.x, plotID, decimalLatitude, decimalLongitude)
specialNEONMapping <- unique(specialNEONMapping)

summary(testing1)

testing2 <- joined_gast %>%
  filter(growthForm == "single bole tree" & taxonRankMapNTag == "species" & taxonIDMapNTag %in% target)# %>% # & height < 60 & stemDiameter <120 )

## see
names
testingPlot <- testPlotly<-ggplot(testing1, aes(x=stemDiameter, y=height, color = year)) +
  geom_point() +
  stat_summary(aes(group=individualID), fun.y=mean, geom="line", colour="green") +
  facet_wrap(~siteID.x)

testingPlot
ggplotly(testingPlot)

joined_gast$taxonIDMapNTag <- as.factor(joined_gast$taxonIDMapNTag)

summary(joined_gast$taxonIDMapNTag)

names(vstData_3) == names(vstData_4)
names(vstData_4) == names(vstData_5)
