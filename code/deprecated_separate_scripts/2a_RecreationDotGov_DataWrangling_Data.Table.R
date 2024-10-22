# PUBLIC LANDS VISITATION # 2
# Tyler McIntosh, 2021

#Setup ----

#Load libraries
library(tidyverse)
library(lubridate)
library(sm)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(sf)
library(usmap)
library(vroom)
library(data.table)
library(beepr)

#Setup
rm(list = ls())
setwd("D:/Storage/")


#Load data ----
#Load camping data
campsites <- fread("RIDBFullExport_V1_CSV/Campsites_API_v1.csv")


#Load intermediate camping data
camp.2020 <- fread("Camp2020.csv")
camp.2019 <- fread("Camp2019.csv")
camp.2018 <- fread("Camp2018.csv")
camp.2017 <- fread("Camp2017.csv")
camp.2016 <- fread("Camp2016.csv")
camp.2015 <- fread("Camp2015.csv")
camp.2014 <- fread("Camp2014.csv")

#Finish Cleaning Data


#Examine duplicates issue

#Match Historical Reservation ID and Product ID column types for RBinds
camp.2018 <- camp.2018 %>% mutate(HistoricalReservationID = as.character(HistoricalReservationID))
camp.2017 <- camp.2017 %>% mutate(HistoricalReservationID = as.character(HistoricalReservationID))
camp.2016 <- camp.2016 %>% mutate(HistoricalReservationID = as.character(HistoricalReservationID))
camp.2015 <- camp.2015 %>% mutate(HistoricalReservationID = as.character(HistoricalReservationID))
camp.2014 <- camp.2014 %>% mutate(HistoricalReservationID = as.character(HistoricalReservationID))

camp.2020 <- camp.2020 %>% mutate(ProductID = as.character(ProductID))
camp.2019 <- camp.2019 %>% mutate(ProductID = as.character(ProductID))
camp.2018 <- camp.2018 %>% mutate(ProductID = as.character(ProductID))
camp.2017 <- camp.2017 %>% mutate(ProductID = as.character(ProductID))
camp.2016 <- camp.2016 %>% mutate(ProductID = as.character(ProductID))
camp.2015 <- camp.2015 %>% mutate(ProductID = as.character(ProductID))
camp.2014 <- camp.2014 %>% mutate(ProductID = as.character(ProductID))

camp.2020 <- camp.2020[!duplicated(camp.2020$OrderNumber),]
camp.2019 <- camp.2019[!duplicated(camp.2019$OrderNumber),]
camp.2018 <- camp.2018[!duplicated(camp.2018$OrderNumber),]
camp.2017 <- camp.2017[!duplicated(camp.2017$OrderNumber),]
camp.2016 <- camp.2016[!duplicated(camp.2016$OrderNumber),]
camp.2015 <- camp.2015[!duplicated(camp.2015$OrderNumber),]
camp.2014 <- camp.2014[!duplicated(camp.2014$OrderNumber),]

camp.2020.mini <- camp.2020[1:20000]

#Combine datasets, remove duplicates
camp.ALL <- rbind(camp.2020, camp.2019, camp.2018, camp.2017, camp.2016, camp.2015, camp.2014)
num.all.camp.reservations <- length(camp.ALL$OrderNumber)
#test <- camp.ALL[duplicated(camp.ALL$OrderNumber),] #There are 35,641 duplicates across the datasets based on OrderNumber
camp.ALL <- camp.ALL %>% distinct(OrderNumber, .keep_all = TRUE)
num.distinct.camp.reservations <- length(camp.ALL$OrderNumber)

camp.2020 <- camp.ALL %>% filter(DataYear == 2020)
camp.2019 <- camp.ALL %>% filter(DataYear == 2019)
camp.2018 <- camp.ALL %>% filter(DataYear == 2018)
camp.2017 <- camp.ALL %>% filter(DataYear == 2017)
camp.2016 <- camp.ALL %>% filter(DataYear == 2016)
camp.2015 <- camp.ALL %>% filter(DataYear == 2015)
camp.2014 <- camp.ALL %>% filter(DataYear == 2014)
rm(camp.ALL)

#Write Clean Datasets
write.csv(camp.2020, "Camp2020Clean.csv")
write.csv(camp.2019, "Camp2019Clean.csv")
write.csv(camp.2018, "Camp2018Clean.csv")
write.csv(camp.2017, "Camp2017Clean.csv")
write.csv(camp.2016, "Camp2016Clean.csv")
write.csv(camp.2015, "Camp2015Clean.csv")
write.csv(camp.2014, "Camp2014Clean.csv")


#Get basic camping stats across years

#Get basic camping numbers
get.camp.basics <- function(new.dats, camp.sum, yr){
  basic.nums <- data.table(yr, length(new.dats$OrderNumber), as.integer(sum(new.dats$Days)), as.integer(sum(new.dats$Nights)), as.double(mean(new.dats$DaysInAdvance)), as.double(median(new.dats$DaysInAdvance)))
  camp.sum <- rbind(camp.sum, basic.nums, use.names = FALSE)
  
  return(camp.sum)
}

camp.summary <- data.table(DataYear = integer(), CampRes = integer(), CampDays = integer(), CampNights = integer(), AvgDaysInAdvance = double(), MedianDaysInAdvance = double())
camp.summary <- camp.2014 %>% get.camp.basics(camp.summary, 2014)
camp.summary <- camp.2015 %>% get.camp.basics(camp.summary, 2015)
camp.summary <- camp.2016 %>% get.camp.basics(camp.summary, 2016)
camp.summary <- camp.2017 %>% get.camp.basics(camp.summary, 2017)
camp.summary <- camp.2018 %>% get.camp.basics(camp.summary, 2018)
camp.summary <- camp.2019 %>% get.camp.basics(camp.summary, 2019)
camp.summary <- camp.2020 %>% get.camp.basics(camp.summary, 2020)
write.csv(camp.summary, "CampSummaryByDataYear.csv")

ggplot(camp.summary) + geom_point(aes(x=Year, y=CampRes))
ggplot(camp.summary) + geom_point(aes(x=Year, y=CampDays))
ggplot(camp.summary) + geom_point(aes(x=Year, y=CampNights))


#Make, explore campsite datasets ----
#Get unique types of campsites included in database
#Fixdate
campsites <- campsites[,CreatedDateFix:= as.Date(CreatedDate)]

#Remove duplicates
campsites <- distinct(campsites)
#make dataframe of when added
campsites.existing <- campsites %>% group_by(CreatedDateFix) %>% summarize(AddedCampsites = n())
campsites.existing <- campsites.existing %>% mutate(ExistingCampsites = cumsum(AddedCampsites))

#Expand dataset for full range of dates
campsites.existing.full <- campsites.existing
fill <- data.table(CreatedDateFix = character(), AddedCampsites = integer(), ExistingCampsites = integer())

for (i in c(1:280)) {
  between.dats <- tail(head((seq(campsites.existing$CreatedDateFix[i], campsites.existing$CreatedDateFix[i+1], by="days")), -1), -1)
  fill <- data.table(between.dats, 0, campsites.existing$ExistingCampsites[i])
  campsites.existing.full <- rbind(campsites.existing.full, fill, use.names=FALSE)
}

campsites.existing.full <- na.omit(campsites.existing.full[order(CreatedDateFix),])
write.csv(campsites.existing.full, "FullCalendarListCampsitesExisting.csv")

#Make, explore campgrounds

#Number of campsites in each facility
campgrounds <- campsites %>% group_by(FacilityID) %>% summarize(number.campsites = n())
write.csv(campgrounds, "Campgrounds2021.csv")
ggplot(campgrounds, aes(number.campsites)) + geom_histogram(bins = 100)

#View campsite created dates
ggplot(campsites, aes(CreatedDateFix))+geom_histogram(bins=60)
#It appears that the vast majority of campsites in the database were created in the beginning of 2014
#View campsite locations
#ggplot() + geom_point(data = campsites, aes(x=CampsiteLongitude, y=CampsiteLatitude))

