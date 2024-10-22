
# PUBLIC LANDS VISITATION Group for Tableau
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
library(usdata)

#Setup
rm(list = ls())

#Setup
main.directory <- "D:/Storage/RecreationAnalysis2021/"
viz.filter.directory <- "D:/Storage/RecreationAnalysis2021/Data viz filters/"
setwd(main.directory)



#Load data
full.expanded <- fread("FULL_ExpandedDataset2.csv")
camps.exist.expand <- fread("CampgroundsExistingFullExpand.csv")

mini <- full.expanded %>% sample_n(100)

mini.camps <- camps.exist.expand %>% sample_n(100)


#JOIN 

#Set directory
setwd(viz.filter.directory)

nights.over.time <- full.expanded %>% group_by(NightDates, ResYear, Agency, FacilityState, ParentLocation, FacilityRegion)%>% summarise(NightsReserved = n())
write.csv(nights.over.time, "NightsOverTimeExp.csv")

spatial.nights <- full.expanded %>% group_by(ResYear, ResMonth, FacilityState, ParentLocation, Park, Agency, FacilityLatitude, FacilityLongitude, FacilityRegion)%>% summarise(NightsReserved = n())
write.csv(spatial.nights, "SpatialNightsExp.csv")

rm(full.expanded)

#Setup
setwd(main.directory)

#Load data
full.normal <- fread("FULL_NormalDataset2.csv")


#Set directory
setwd(viz.filter.directory)

#Filter
# normal.grouped <- full.normal %>% group_by(Agency, RegionDescription, ParentLocation, Park, FacilityID, 
#                                              FacilityZIP, FacilityState, FacilityLongitude, FacilityLatitude,
#                                              CustomerZIP, CustomerState, CustomerCountry, DataYear, RStartYr,
#                                              FacilityRegion, CustomerRegion) %>% summarise (Reservations = n(), NightsReserved = sum(Nights),
#                                                                                                            S.TotalBeforeTax = sum(TotalBeforeTax), 
#                                                                                                            S.TotalPaid = sum(TotalPaid))
# write.csv(normal.grouped, "NormalGrouped.csv")
inflow.outflow <- full.normal %>% group_by(FacilityState, FacilityZIP, CustomerState, CustomerZIP, FacilityRegion, CustomerRegion, RStartYr, InOut)%>% summarise(Reservations = n(), NumberOfPeople = sum(NumberOfPeople))
write.csv(inflow.outflow, "InflowOutflowRes.csv")
general.res.stats <- full.normal %>% group_by(RStartYr)%>% summarise(Reservations = n(), AvgDaysInAdvance = mean(DaysInAdvance), TotalBeforeTax = sum(TotalBeforeTax))
write.csv(general.res.stats, "GeneralResStatsRes.csv")

