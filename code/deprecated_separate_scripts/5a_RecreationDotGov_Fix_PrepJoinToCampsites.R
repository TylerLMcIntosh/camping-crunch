
# PUBLIC LANDS VISITATION FIX DATA MISTAKES, JOIN TO CAMPSITE DATA
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

#Fix months



#mini.expanded <- full.expanded %>% sample_n(size = 100, replace = F)

#Filter
# expanded.grouped <- full.expanded %>% group_by(Agency, RegionDescription, ParentLocation, Park, FacilityID, 
#                                            FacilityZIP, FacilityState, FacilityLongitude, FacilityLatitude,
#                                            CustomerZIP, CustomerState, CustomerCountry, ResYear, ResMonth,
#                                            FacilityRegion, CustomerRegion, ResMonthWritten, NightDates, WeekDay) %>% summarise (NightsReserved = n(), 
#                                                                                           S.TotalBeforeTax = sum(TotalBeforeTax), 
#                                                                                           S.TotalPaid = sum(TotalPaid))
# write.csv(expanded.grouped, "ExpandedGrouped.csv")




#Fix Problem cases----
#Found & fixed multi-state issue
# full.expanded.campground.nights <- full.expanded.fixed %>% group_by(FacilityID, NightDates) %>% summarise(NightsReserved = n())
# full.expanded.campground.nights.more.info <- full.expanded.fixed %>% group_by(FacilityID, NightDates, Weekday, 
#                                                                               Agency, FacilityState) %>% summarise(NightsReserved = n())
# anti <- anti_join(full.expanded.campground.nights.more.info, full.expanded.campground.nights)
# problem.facilities <- unique(anti$FacilityID)
# examine.problem <- full.expanded.fixed %>% filter(FacilityID == 
#                                                     problem.facilities[1])
# unique(examine.problem$FacilityID)
# unique(examine.problem$RegionDescription)
# unique(examine.problem$ParentLocation)
# unique(examine.problem$FacilityZIP)
# unique(examine.problem$FacilityState)
# unique(examine.problem$FacilityLongitude)
# unique(examine.problem$FacilityLatitude)
# unique(examine.problem$Park)
# unique(examine.problem$RegionCode)
# unique(examine.problem)
#Case 1
full.expanded.fixed <- full.expanded[(FacilityID == 232210), `:=` (RegionDescription = "Intermountain Reg.",
                                                                   ParentLocation = "Caribou-Targhee National Forest",
                                                                   FacilityZIP = 83128,
                                                                   FacilityState = "Wyoming",
                                                                   FacilityLongitude = -111.0411,
                                                                   FacilityLatitude = 43.19639,
                                                                   Park = "ALPINE NORTH LOOP",
                                                                   RegionCode = "F4")]
#Case 2
full.expanded.fixed[(FacilityID == 232505), `:=` (RegionDescription = "Southeast Region",
                                                  ParentLocation = "Big South Fork National River & Recreation Area",
                                                  FacilityZIP = 42649,
                                                  FacilityState = "Kentucky",
                                                  FacilityLongitude = -84.51889,
                                                  FacilityLatitude = 36.67806,
                                                  Park = "BLUE HERON CAMPGROUND",
                                                  RegionCode = "SER")]
#Case 3
full.expanded.fixed[(FacilityID == 232768), `:=` (RegionDescription = "Pacific Southwest",
                                                  ParentLocation = "Lake Tahoe Basin Management Unit",
                                                  FacilityZIP = 89448,
                                                  FacilityState = "Nevada",
                                                  FacilityLongitude = -119.9486,
                                                  FacilityLatitude = 38.98194,
                                                  Park = "Nevada Beach Campground and Day Use Pavilion",
                                                  RegionCode = "F5")]

#Case 4
full.expanded.fixed[(FacilityID == 233467), `:=` (RegionDescription = "Northwestern Div.",
                                                  ParentLocation = "Lewis and Clark Lake",
                                                  FacilityZIP = 57078,
                                                  FacilityState = "Nebraska",
                                                  FacilityLongitude = -97.4825,
                                                  FacilityLatitude = 42.85861,
                                                  Park = "COTTONWOOD",
                                                  RegionCode = "CG")]
#Case 5
full.expanded.fixed[(FacilityID == 233530), `:=` (RegionDescription = "Southwestern Div.",
                                                  ParentLocation = "Waurika Lake",
                                                  FacilityZIP = 76365,
                                                  FacilityState = "Oklahoma",
                                                  FacilityLongitude = -98.07533,
                                                  FacilityLatitude = 34.252,
                                                  Park = "KIOWA PARK I",
                                                  RegionCode = "CM")]
#Case 6
full.expanded.fixed[(FacilityID == 251468), `:=` (RegionDescription = "Southeast Region",
                                                  ParentLocation = "Big South Fork National River & Recreation Area",
                                                  FacilityZIP = 42649,
                                                  FacilityState = "Kentucky",
                                                  FacilityLongitude = -84.52364,
                                                  FacilityLatitude = 36.64108,
                                                  Park = "Bear Creek Horse Camp",
                                                  RegionCode = "SER")]
#Case 7
full.expanded.fixed[(FacilityID == 251615), `:=` (RegionDescription = "Pacific Southwest",
                                                  ParentLocation = "Eldorado National Forest",
                                                  FacilityZIP = 95726,
                                                  FacilityState = "California",
                                                  FacilityLongitude = -120.4825,
                                                  FacilityLatitude = 38.76639,
                                                  Park = "BRIDAL VEIL GROUP AREA AND PICNIC GROUND",
                                                  RegionCode = "F5")]
#Case 8
full.expanded.fixed[(FacilityID == 272246), `:=` (RegionDescription = "Sierra Front Field Office",
                                                  ParentLocation = "Indian Creek Recreation Area",
                                                  FacilityZIP = 96120,
                                                  FacilityState = "California",
                                                  FacilityLongitude = -119.7849,
                                                  FacilityLatitude = 38.74794,
                                                  Park = "INDIAN CREEK CAMPGROUND (CA)",
                                                  RegionCode = "LLNVC02000")]
#Case 9
full.expanded.fixed[(FacilityID == 273821), `:=` (RegionDescription = "Southeast Region",
                                                  ParentLocation = "Great Smoky Mountains National Park",
                                                  FacilityZIP = 28785,
                                                  FacilityState = "North Carolina",
                                                  FacilityLongitude = -83.10417,
                                                  FacilityLatitude = 35.75972,
                                                  Park = "BIG CREEK CAMPGROUND (GREAT SMOKY MOUNTAINS NATIONAL PARK)",
                                                  RegionCode = "SER")]
#Case 10
full.expanded.fixed[(FacilityID == 274288), `:=` (RegionDescription = "Lake Havasu Field Office",
                                                  ParentLocation = "Parker Strip Recreation Area",
                                                  FacilityZIP = 92267,
                                                  FacilityState = "California",
                                                  FacilityLongitude = -114.2151,
                                                  FacilityLatitude = 34.21088,
                                                  Park = "CROSSROADS CAMPGROUND",
                                                  RegionCode = "")]
#AGAIN, Check if datasets will be the same when grouping
full.expanded.campground.nights <- full.expanded.fixed %>% group_by(FacilityID, NightDates) %>% summarise(NightsReserved = n())
full.expanded.campground.nights.more.info <- full.expanded.fixed %>% group_by(FacilityID, NightDates, Weekday, 
                                                                              Agency, FacilityState, RegionDescription, 
                                                                              ParentLocation, Park, FacilityZIP, FacilityLongitude,
                                                                              FacilityLatitude, ResYear, ResMonthWritten,
                                                                              FacilityRegion) %>% summarise(NightsReserved = n())
#FOUND, FIXED INCONSISTENT DATA PROBLEM
anti <- anti_join(full.expanded.campground.nights.more.info, full.expanded.campground.nights)
problem.facilities <- unique(anti$FacilityID)
#For each facility with this problem, get the most common of each problem variable, then set ALL instances of this facility to be this
for (i in 1:length(problem.facilities)) {
  examine.problem <- full.expanded.fixed %>% filter(FacilityID == 
                                                      problem.facilities[i])
  fZIP <- names(sort(table(examine.problem$FacilityZIP), decreasing = TRUE)[1])
  rDesc <- names(sort(table(examine.problem$RegionDescription), decreasing = TRUE)[1])
  pLocation <- names(sort(table(examine.problem$ParentLocation), decreasing = TRUE)[1])
  park <- names(sort(table(examine.problem$Park), decreasing = TRUE)[1])
  fLong <- as.numeric(names(sort(table(examine.problem$FacilityLongitude), decreasing = TRUE)[1]))
  fLat <- as.numeric(names(sort(table(examine.problem$FacilityLatitude), decreasing = TRUE)[1]))
  full.expanded.fixed[(FacilityID == problem.facilities[i]), `:=` (RegionDescription = rDesc,
                                                                   ParentLocation = pLocation,
                                                                   FacilityZIP = fZIP,
                                                                   FacilityLongitude = fLong,
                                                                   FacilityLatitude = fLat,
                                                                   Park = park)]
}
#AGAIN, Check if datasets will be the same when grouping
full.expanded.campground.nights <- full.expanded.fixed %>% group_by(FacilityID, NightDates) %>% summarise(NightsReserved = n())
full.expanded.campground.nights.more.info <- full.expanded.fixed %>% group_by(FacilityID, NightDates, Weekday, 
                                                                              Agency, FacilityState, RegionDescription, 
                                                                              ParentLocation, Park, FacilityZIP, FacilityLongitude,
                                                                              FacilityLatitude, ResYear, ResMonthWritten,
                                                                              FacilityRegion) %>% summarise(NightsReserved = n())
#Write files to store
fwrite(full.expanded.campground.nights.more.info, "ExpandedFixed_GroupedByFacilityIDAndNightDates.csv")
gc()
fwrite(full.expanded.fixed, "FullExpandedFixed.csv")
gc()


full.expanded.campground.nights.more.info <- fread("ExpandedFixed_GroupedByFacilityIDAndNightDates.csv")
mini.camp <- full.expanded.campground.nights.more.info[1:10,]
campsites <- fread("RIDBFullExport_V1_CSV/Campsites_API_v1.csv")
facilities <- fread("RIDBFullExport_V1_CSV/Facilities_API_v1.csv")
orgs <- fread("RIDBFullExport_V1_CSV/Organizations_API_v1.csv")
camps.exist.expand <- fread("CampgroundsExistingFullExpand.csv")
facilities <- facilities %>% mutate(FacilityID = as.integer(FacilityID))

#All campsites in the dataset have an associated Facility in the dataset
 # testcampfacilities <- left_join(campsites, facilities, by = "FacilityID")
 # test <-unique(testcampfacilities$CampsiteName)
 # nas <- testcampfacilities[is.na(FacilityName), ]


#Add basic facility data, 
camps.exist.expand <- camps.exist.expand %>% left_join(facilities, by = "FacilityID")
mini <- camps.exist.expand[1:100,]
camps.exist.expand <- camps.exist.expand %>% select(-V1, -CreatedDateFix, -number.campsites, -number.campsites.total,
                                                    -percentage, -FirstDate, -LastDate, -LegacyFacilityID, -(ParentOrgID:ParentRecAreaID),
                                                    -(FacilityDescription:FacilityAdaAccess), -(Keywords:LastUpdatedDate))
setnames(camps.exist.expand, old = "AllDates", new = "NightDates")
setnames(camps.exist.expand, old = "OrgFacilityID", new = "OrgID")
orgs <- orgs %>% mutate(OrgID = as.character(OrgID))
camps.exist.expand <- camps.exist.expand %>% left_join(orgs, by = "OrgID") %>% select(-OrgType, -(OrgImageURL:OrgURLAddress),
                                                                                      -(OrgJurisdictionType:LastUpdatedDate))
camps.exist.expand <- camps.exist.expand %>% mutate(Weekday = weekdays(NightDates)) %>% mutate(DayType = case_when(Weekday == "Monday" ~ "Weekday",
                                                                                                                   Weekday == "Tuesday" ~ "Weekday",
                                                                                                                   Weekday == "Wednesday" ~ "Weekday",
                                                                                                                   Weekday == "Thursday" ~ "Weekday",
                                                                                                                   Weekday == "Friday" ~ "Weekday",
                                                                                                                   Weekday == "Saturday" ~ "Weekend",
                                                                                                                   Weekday == "Sunday" ~ "Weekend",
                                                                                                                   TRUE ~ "Missing"))
camps.exist.expand <- camps.exist.expand %>% mutate(ResMonthWritten = months(NightDates)) %>% mutate(ResYear = year(NightDates))

#Lat/Longs are incorrect for some of the more recent facilities. However, it appears that only 2 of these are in the West. Fixed below
camps.exist.expand[FacilityID==10119505, FacilityLatitude := 20.721055]
camps.exist.expand[FacilityID==10119505, FacilityLongitude := -156.147209]
camps.exist.expand[FacilityID==264411, FacilityLongitude := -FacilityLongitude]


camping.facilities.all <- camps.exist.expand %>% distinct(FacilityID, .keep_all = TRUE) %>% select(FacilityID, FacilityLatitude, FacilityLongitude)
fwrite(camping.facilities.all, "ALLSpatialCampingFacilities.csv")

