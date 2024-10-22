
# PUBLIC LANDS VISITATION
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

#Setup
rm(list = ls())
setwd("D:/Storage/")

#Load data ----
#Load camping data
campsites <- fread("RIDBFullExport_V1_CSV/Campsites_API_v1.csv")

#Load reservation data (ALL SMALL RIGHT NOW)
rows = 20000 #Enter number of rows to sample here. nrows = rows if needed
res.2020 <- fread("Recreation.govReservationData/reservations2020/FY20 Historical Reservations Full.csv") #5114789 total rows
res.2019 <- fread("Recreation.govReservationData/reservations2019/reservations2019.csv") #3479643
res.2018 <- fread("Recreation.govReservationData/reservations2018/reservations2018.csv") #3299805
res.2017 <- fread("Recreation.govReservationData/reservations2017/2017.csv") #3303553
res.2016 <- fread("Recreation.govReservationData/reservations2016/2016.csv") #2980714
res.2015 <- fread("Recreation.govReservationData/reservations2015/2015.csv") #2949219
res.2014 <- fread("Recreation.govReservationData/reservations2014/2014.csv") #2654437

#Clean data columns
res.2020 <- res.2020 %>% select(-discount, -nights, -equipmentdescription, -equipmentlength)
res.2019 <- res.2019 %>% select(-discount, -equipmentdescription, -equipmentlength)
res.2018 <- res.2018 %>% select(-EntityID,-(Tent:LatLongPoint))
res.2017 <- res.2017 %>% select(-EntityID,-(Tent:Marinaboat))
res.2016 <- res.2016 %>% select(-EntityID,-(Tent:Marinaboat))
res.2015 <- res.2015 %>% select(-EntityID,-(Tent:Marinaboat))
res.2014 <- res.2014 %>% select(-EntityID,-(Tent:Marinaboat))
#Change column titles of 2019/20 to capitalization of earlier datasets, changes name of inventorytype to EntityType
colnames(res.2020) <- colnames(res.2015)
colnames(res.2019) <- colnames(res.2015)
#Put 2020 FacilityID as integer, only dataset that isn't
res.2020 <- res.2020[,FacilityID:=as.integer(FacilityID)]

#get total data
total.data.entries <- length(res.2020$OrderNumber) + length(res.2019$OrderNumber) + length(res.2018$OrderNumber) + length(res.2017$OrderNumber) + length(res.2016$OrderNumber) + length(res.2015$OrderNumber) + length(res.2014$OrderNumber)

#Set date format for dataset
date.format <- "%Y-%m-%d"

#Make, explore campsite datasets ----
#Get unique types of campsites included in database
print(unique(campsites$TypeOfUse))
#Fixdate
campsites <- campsites[,CreatedDateFix:= as.Date(CreatedDate)]

#Remove duplicates
campsites <- distinct(campsites)
#make dataframe of when added
campsites.existing <- campsites %>% group_by(CreatedDateFix) %>% summarize(AddedCampsites = n())
campsites.existing <- campsites.existing %>% mutate(ExistingCampsites = cumsum(AddedCampsites))

#Make, explore campgrounds

# #Number of campsites in each facility
# campgrounds <- campsites %>% group_by(FacilityID) %>% summarize(number.campsites = n())
# ggplot(campgrounds, aes(number.campsites)) + geom_histogram(bins = 100)
# 
# #View campsite created dates
# ggplot(campsites, aes(CreatedDateFix))+geom_histogram(bins=60)
# #It appears that the vast majority of campsites in the database were created in the beginning of 2014
# #View campsite locations
# #ggplot() + geom_point(data = campsites, aes(x=CampsiteLongitude, y=CampsiteLatitude))
# 
# 
# #Function to filter camping reservations from full datasets, add in days in advance, format dates, get # of nights----

get.camping <- function(res.data, yr) {
  #Fix dates as date type
  res.data <- res.data %>% mutate(StartDate = as.Date(StartDate))
  res.data <- res.data %>% mutate(EndDate = as.Date(EndDate))
  res.data <- res.data %>% mutate(OrderDate = as.Date(OrderDate))
  num.res <- length(res.data$OrderNumber)
  
  #Subset only camping reservations for selected dataset
  res.camp <- res.data %>% filter((EntityType == "CAMPING" | EntityType == "Site") & UseType == "Overnight") #Need the or/and to account for datatype change after 2018
  num.res.camp <- length(res.camp$OrderNumber)
  percent.res.camping <- 100*(num.res.camp/num.res)
  print(paste(percent.res.camping, "percent of the", yr, "dataset is camping reservations (", num.res.camp, "of", num.res, "reservations)"))
  
  #Join Campsite & Reservation data
  #Figure out how many have matches. res.w.campsites
  #becomes the subset of reservations for which there is a campsite. It is NOT joined
  res.camp <- res.camp %>% semi_join(campsites, by="FacilityID")
  num.res.campsites <- length(res.camp$OrderNumber)
  percent.res.with.campsites <- 100*(num.res.campsites/num.res.camp)
  print(paste(percent.res.with.campsites, "percent of the", yr, "camping reservations have an associated campsite (", num.res.campsites, "of", num.res.camp, "camping reservations)"))
  
  #Figure out how early people are making reservations
  res.camp <- res.camp %>% mutate(DaysInAdvance = StartDate - OrderDate)
  
  #Get number of days & nights for each reservation
  res.camp <- res.camp %>% mutate(Nights = EndDate - StartDate)
  res.camp <- res.camp %>% mutate(Days = EndDate - StartDate + 1)
  tot.days <- sum(res.camp$Days)
  tot.nights <- sum(res.camp$Nights)
  print(paste("Camping reservations in", yr, "with an associated campsite total", tot.days, "days and", tot.nights, "nights"))
  
  #Enter data year numbers into dataset
  res.camp <- res.camp %>% mutate(DataYear = yr)
  
  #Get days included in reservation 
  res.camp <- res.camp %>% filter((res.camp$EndDate-res.camp$StartDate > 0))
  num.res.campsites.rightdate <- length(res.camp$OrderNumber)
  percent.res.with.campsites.rightdate <- 100*(num.res.campsites.rightdate/num.res.campsites)
  print(paste(percent.res.with.campsites.rightdate, "percent of the", yr, "camping reservations with an associated campsite have correct dates (", num.res.campsites.rightdate, "of", num.res.campsites, "camping reservations with an associated campsite)"))
  
  return(res.camp)
}

#RUN ACTUAL ANALYSIS ----

#Tester
res.2020.mini <- res.2020[1:20000]
system.time(camp.2020.mini <- res.2020.mini %>% get.camping(2020))

#Full
camp.2020 <- res.2020 %>% get.camping(2020)
camp.2019 <- res.2019 %>% get.camping(2019)
camp.2018 <- res.2018 %>% get.camping(2018)
camp.2017 <- res.2017 %>% get.camping(2017)
camp.2016 <- res.2016 %>% get.camping(2016)
camp.2015 <- res.2015 %>% get.camping(2015)
camp.2014 <- res.2014 %>% get.camping(2014)

#write intermediate files (Save progress)
write.csv(camp.2020, "Camp2020.csv")
write.csv(camp.2019, "Camp2019.csv")
write.csv(camp.2018, "Camp2018.csv")
write.csv(camp.2017, "Camp2017.csv")
write.csv(camp.2016, "Camp2016.csv")
write.csv(camp.2015, "Camp2015.csv")
write.csv(camp.2014, "Camp2014.csv")

#Clear out un-needed data
rm(res.2014, res.2015, res.2016, res.2017, res.2018, res.2019, res.2020)


