
setwd("X:/1_Databases/")
library(neonUtilities)

setOfData <- read.csv("OSdataset.csv")
setOfData$apiCode <- substring(setOfData$apiCode, 6)

head(setOfData)



# Eventually make Loop, just a test of functionality at this point.
pheImageCall <- setOfData[1,]
breedingBirdCountCall <- setOfData[2,]
CDWCall <- setOfData[3,]
grdBeetlesPFCall <- setOfData[4,]
litterFallCall <- setOfData[5,]
mosPathogensCall <- setOfData[6,]
mosTrappedCall <- setOfData[7,]
pheObsCall <- setOfData[8,]
plantPerCoverCall <- setOfData[9,]
smamPathogenCall<- setOfData[10,]
smamPathogenCall
rootSamplingCall<- setOfData[11,]
smamTrapCall <- setOfData[12,]
soilMicrobeCompCall <- setOfData[13,]
tickPathogenCall <- setOfData[14,]
tickTrapCall <- setOfData[15,]
woodyVSTCall <- setOfData[16,]
soilMicrobeMassCall <- setOfData[17,]
soilMicrobeAbundanceCall <- setOfData[18,]
landWaterImagesCall <- setOfData[19,]
benthMicrobeCompCall <- setOfData[20,]
eFishingCall <- setOfData[21,]
macroCollectionCall <- setOfData[22,]
surfaceWaterMicrobeCntCall <- setOfData[23,]
surfaceWaterMicrobeCompCall <- setOfData[24,]
ripVegCoverCall <- setOfData[25,]
zooplankCollectionCall <- setOfData[26,]
ripCompStructureCall <- setOfData[27,]
benthMicrobeAbundCall <- setOfData[28,]
surfaceWaterMicrobeAbundCall <- setOfData[29,]

# expecting 3-4 times as much data as advertised
#pheImage <- loadByProduct(dpID = pheImageCall$apiCode) # not an APIable dataset `TODO` Download dataset directly from Phenonetwork
breedingBirdCountCall <- loadByProduct(dpID = breedingBirdCountCall$apiCode, check.size = FALSE) # 7mb = 34mb, 145 files,
CDWCall <- loadByProduct(dpID = CDWCall$apiCode, check.size = FALSE) #1mb
grdBeetlesPFCall <- loadByProduct(dpID = grdBeetlesPFCall$apiCode, check.size = FALSE) #37 mb, 840 files, 102 mb took a long time to gather all that data
litterFallCall <- loadByProduct(dpID = litterFallCall$apiCode, check.size = FALSE) #17 mb, 565 files, csv total = 51.7
mosPathogensCall <- loadByProduct(dpID = mosPathogensCall$apiCode, check.size = FALSE)#6mb, check.size = FALSE)
mosTrappedCall <- loadByProduct(dpID = mosTrappedCall$apiCode, check.size = FALSE) #32mb
pheObsCall <- loadByProduct(dpID = pheObsCall$apiCode, check.size = FALSE) #107
plantPerCoverCall <- loadByProduct(dpID = plantPerCoverCall$apiCode, check.size = FALSE) #45mb
smamPathogenCall <- loadByProduct(dpID = smamPathogenCall$apiCode, check.size = FALSE) #3 mb
rootSamplingCall <- loadByProduct(dpID = rootSamplingCall$apiCode, check.size = FALSE) #1.5
smamTrapCall <- loadByProduct(dpID = smamTrapCall$apiCode, check.size = FALSE) #3.4mb, 228 files,
soilMicrobeCompCall <- loadByProduct(dpID = soilMicrobeCompCall$apiCode) #.27mb
tickPathogenCall <- loadByProduct(dpID = tickPathogenCall$apiCode, check.size = FALSE) #4mb
tickTrapCall <- loadByProduct(dpID = tickTrapCall$apiCode, check.size = FALSE) #15 mb
woodyVSTCall <- loadByProduct(dpID = woodyVSTCall$apiCode, check.size = FALSE) # 65 mb
soilMicrobeMassCall <- loadByProduct(dpID = soilMicrobeMassCall$apiCode, check.size = FALSE) #.5 mb
soilMicrobeAbundanceCall <- loadByProduct(dpID = soilMicrobeAbundanceCall$apiCode, check.size = FALSE) #4.5
#landWaterImagesCall <- loadByProduct(dpID = landWaterImagesCall$apiCode) # not a set
benthMicrobeCompCall <- loadByProduct(dpID = benthMicrobeCompCall$apiCode, check.size = FALSE) # 0.045mb
eFishingCall <- loadByProduct(dpID = eFishingCall$apiCode, check.size = FALSE) #4.2mb
macroCollectionCall <- loadByProduct(dpID = macroCollectionCall$apiCode, check.size = FALSE) #12.3, 294 files
surfaceWaterMicrobeCntCall <- loadByProduct(dpID = surfaceWaterMicrobeCntCall$apiCode, check.size = FALSE) # 17mb
surfaceWaterMicrobeCompCall <- loadByProduct(dpID = surfaceWaterMicrobeCompCall$apiCode, check.size = FALSE) #.05mb
ripVegCoverCall <- loadByProduct(dpID = ripVegCoverCall$apiCode, check.size = FALSE) #.8 mb
zooplankCollectionCall <- loadByProduct(dpID = zooplankCollectionCall$apiCode, check.size = FALSE) #1.8mb
ripCompStructureCall <- loadByProduct(dpID = ripCompStructureCall$apiCode, check.size = FALSE)#1.5mb
benthMicrobeAbundCall <- loadByProduct(dpID = benthMicrobeAbundCall$apiCode, check.size = FALSE) #.16mb
surfaceWaterMicrobeAbundCall <- loadByProduct(dpID = surfaceWaterMicrobeAbundCall$apiCode, check.size = FALSE) #.39mb


# # seeing how big a csv will be totalling
# names(litterFallCall)
# 
# test1 <- data.frame(grdBeetlesPFCall[1])
# test2 <- data.frame(grdBeetlesPFCall[2])
# test3 <- data.frame(grdBeetlesPFCall[3])
# test4 <- data.frame(grdBeetlesPFCall[4])
# test5 <- data.frame(grdBeetlesPFCall[5])
# 
# write.csv(test1, "test1.csv")
# write.csv(test2, "test2.csv")
# write.csv(test3, "test3.csv")
# write.csv(test4, "test4.csv")
# write.csv(test5, "test5.csv")
# 
# names(grdBeetlesPFCall)

APICalls <- c(breedingBirdCountCall, CDWCall,grdBeetlesPFCall,litterFallCall ,mosPathogensCall, mosTrappedCall ,pheObsCall ,plantPerCoverCall ,smamPathogenCall,rootSamplingCall,smamTrapCall,soilMicrobeCompCall ,tickPathogenCall ,tickTrapCall ,woodyVSTCall ,soilMicrobeMassCall ,soilMicrobeAbundanceCall,benthMicrobeCompCall,eFishingCall,macroCollectionCall,surfaceWaterMicrobeCntCall,surfaceWaterMicrobeCompCall,ripVegCoverCall,zooplankCollectionCall,ripCompStructureCall,benthMicrobeAbundCall, surfaceWaterMicrobeAbundCall)
APICalls <- c(breedingBirdCountCall, CDWCall)

setwd("X:/1_Databases/bigNEON/TOS")
getwd()
#3
write.csv(breedingBirdCountCall[1], "breedingBirdCountCall_1.csv", row.names=FALSE)
write.csv(breedingBirdCountCall[2], "breedingBirdCountCall_2.csv", row.names=FALSE)
write.csv(breedingBirdCountCall[3], "breedingBirdCountCall_3.csv", row.names=FALSE)
#2
write.csv(CDWCall[1],"CDWCall_1.csv", row.names=FALSE)
write.csv(CDWCall[2],"CDWCall_2.csv", row.names=FALSE)
#5
write.csv(grdBeetlesPFCall[1],"grdBeetlesPFCall_1.csv", row.names=FALSE)
write.csv(grdBeetlesPFCall[2], "grdBeetlesPFCall_2.csv", row.names=FALSE)
write.csv(grdBeetlesPFCall[3], "grdBeetlesPFCall_3.csv", row.names=FALSE)
write.csv(grdBeetlesPFCall[4], "grdBeetlesPFCall_4.csv", row.names=FALSE)
write.csv(grdBeetlesPFCall[5], "grdBeetlesPFCall_5.csv", row.names=FALSE)
names(litterFallCall) #4
write.csv(litterFallCall[1],"litterFallCall_1.csv", row.names=FALSE)
write.csv(litterFallCall[2], "litterFallCall_2.csv", row.names=FALSE)
write.csv(litterFallCall[3], "litterFallCall_3.csv", row.names=FALSE)
write.csv(litterFallCall[4], "litterFallCall_4.csv", row.names=FALSE)
#2
write.csv(mosPathogensCall[1],"mosPathogensCall_1.csv", row.names=FALSE)
write.csv(mosPathogensCall[2], "mosPathogensCall_2.csv", row.names=FALSE)
#6
write.csv(mosTrappedCall[1], "mosTrappedCall_1.csv", row.names=FALSE)
write.csv(mosTrappedCall[2], "mosTrappedCall_2.csv", row.names=FALSE)
write.csv(mosTrappedCall[3], "mosTrappedCall_3.csv", row.names=FALSE)
write.csv(mosTrappedCall[4], "mosTrappedCall_4.csv", row.names=FALSE)
write.csv(mosTrappedCall[5], "mosTrappedCall_5.csv", row.names=FALSE)
write.csv(mosTrappedCall[6], "mosTrappedCall_6.csv", row.names=FALSE)
#3
write.csv(pheObsCall[1],"pheObsCall_1.csv", row.names=FALSE)
write.csv(pheObsCall[2], "pheObsCall_2.csv", row.names=FALSE)
write.csv(pheObsCall[3], "pheObsCall_3.csv", row.names=FALSE)
#2
write.csv(plantPerCoverCall[1],"plantPerCoverCall_1.csv", row.names=FALSE)
write.csv(plantPerCoverCall[2], "plantPerCoverCall_2.csv", row.names=FALSE)
#1
write.csv(smamPathogenCall[1],"smamPathogenCall_1.csv", row.names=FALSE)
#4
write.csv(rootSamplingCall[1], "rootSamplingCall_1.csv", row.names=FALSE)
write.csv(rootSamplingCall[2], "rootSamplingCall_2.csv", row.names=FALSE)
write.csv(rootSamplingCall[3], "rootSamplingCall_3.csv", row.names=FALSE)
write.csv(rootSamplingCall[4], "rootSamplingCall_4.csv", row.names=FALSE)
#2
write.csv(smamTrapCall[1], "smamTrapCall_1.csv", row.names=FALSE)
write.csv(smamTrapCall[2], "smamTrapCall_2.csv", row.names=FALSE)
#names(soilMicrobeCompCall) 
#names(tickPathogenCall)
#2
write.csv(tickTrapCall[1], "tickTrapCall_1.csv", row.names=FALSE)
write.csv(tickTrapCall[2], "tickTrapCall_2.csv", row.names=FALSE)
#3-6
write.csv(woodyVSTCall[3], "woodyVSTCall_3.csv", row.names=FALSE)
write.csv(woodyVSTCall[4], "woodyVSTCall_4.csv", row.names=FALSE)
write.csv(woodyVSTCall[5], "woodyVSTCall_5.csv", row.names=FALSE)
write.csv(woodyVSTCall[6], "woodyVSTCall_6.csv", row.names=FALSE)
#1
write.csv(soilMicrobeMassCall[1], "soilMicrobeMassCall_1.csv", row.names=FALSE)
#1
write.csv(soilMicrobeAbundanceCall[1], "soilMicrobeAbundanceCall_1.csv", row.names=FALSE)
#names(landWaterImagesCall)
#1
write.csv(benthMicrobeCompCall[1], "benthMicrobeCompCall_1.csv", row.names=FALSE)
#4
write.csv(eFishingCall[1], "eFishingCall_1.csv", row.names=FALSE)
write.csv(eFishingCall[2], "eFishingCall_2.csv", row.names=FALSE)
write.csv(eFishingCall[3], "eFishingCall_3.csv", row.names=FALSE)
write.csv(eFishingCall[4], "eFishingCall_4.csv", row.names=FALSE)
#3
write.csv(macroCollectionCall[1], "macroCollectionCall_1.csv", row.names=FALSE)
write.csv(macroCollectionCall[2], "macroCollectionCall_2.csv", row.names=FALSE)
write.csv(macroCollectionCall[3], "macroCollectionCall_3.csv", row.names=FALSE)
#3
write.csv(surfaceWaterMicrobeCntCall[1], "surfaceWaterMicrobeCntCall_1.csv", row.names=FALSE)
write.csv(surfaceWaterMicrobeCntCall[2], "surfaceWaterMicrobeCntCall_2.csv", row.names=FALSE)
write.csv(surfaceWaterMicrobeCntCall[3], "surfaceWaterMicrobeCntCall_3.csv", row.names=FALSE)
#1
write.csv(surfaceWaterMicrobeCompCall[1], "surfaceWaterMicrobeCompCall", row.names=FALSE)
#1
write.csv(ripVegCoverCall[1], "ripVegCoverCall_1.csv", row.names=FALSE)
#3-4
write.csv(zooplankCollectionCall[3], "zooplankCollectionCall_3.csv", row.names=FALSE)
write.csv(zooplankCollectionCall[4], "zooplankCollectionCall_4.csv", row.names=FALSE)
#1
write.csv(ripCompStructureCall[1], "ripCompStructureCall_1.csv", row.names=FALSE)
#1
write.csv(benthMicrobeAbundCall[1], "benthMicrobeAbundCall_1.csv", row.names=FALSE)
#1
write.csv(surfaceWaterMicrobeAbundCall[1], "surfaceWaterMicrobeAbundCall_1.csv", row.names=FALSE)
names(surfaceWaterMicrobeAbundCall)
