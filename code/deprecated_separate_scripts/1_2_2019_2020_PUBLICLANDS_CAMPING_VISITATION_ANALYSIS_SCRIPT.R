
# PUBLIC LANDS VISITATION: 2019/2020 FULL DATA
# Tyler McIntosh, 2021

rm(list = ls())

if(!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)

source(here::here("code", "functions.R"))

install_and_load_packages(
  package_list = c(
    "here",
    "tidyverse",
    "lubridate",
    "sm",
    "rnaturalearth",
    "rnaturalearthdata",
    "ggspatial",
    "sf",
    "usmap",
    "vroom",
    "data.table",
    "disk.frame",
    "pryr",
    "usdata"),
  auto_install = "y"
)



#Set directories
main.directory <- "D:/Storage/RecreationAnalysis2021/"
desktop <- "C:/Users/tyler/Desktop/"
viz.filter.directory <- "D:/Storage/RecreationAnalysis2021/Data viz filters/"

#Set working directory
setwd(main.directory)

#Load datasets from RIDB Staff
util.2019 <- fread("Tyler-WesternPriorities/FY19 Campsite Utilization.csv")
util.2020 <- fread("Tyler-WesternPriorities/FY20 Campsite Utilization.csv")
View(util.2020[1:10,])
min(util.2019$availability_date)
max(util.2019$availability_date)
min(util.2020$availability_date)
max(util.2020$availability_date)

#Load campsite/facility/org data
campsites <- fread("RIDBFullExport_V1_CSV/Campsites_API_v1.csv")
facilities <- fread("RIDBFullExport_V1_CSV/Facilities_API_v1.csv")
orgs <- fread("RIDBFullExport_V1_CSV/Organizations_API_v1.csv")

res.2019 <- fread("Tyler-WesternPriorities/FY19 Historical Reservations w Status.csv")
res.2020 <- fread("Tyler-WesternPriorities/FY20 Historical Reservations w Status.csv")
res.2015 <- fread("Recreation.govReservationData/reservations2015/2015.csv")

#Clean data----
res.2015 <- res.2015 %>% select(-EntityID,-(Tent:Marinaboat))
res.2020 <- res.2020 %>% select(-discount, -nights, -equipmentdescription, -equipmentlength)
res.2019 <- res.2019 %>% select(-discount, -nights, -equipmentdescription, -equipmentlength)
colnames(res.2020) <- c(colnames(res.2015), "Status")
colnames(res.2019) <- c(colnames(res.2015), "Status")
rm(res.2015)

#Filter to camping----
get.camping <- function(res.data, yr) {
  #Fix dates as date type
  res.data <- res.data %>% mutate(StartDate = as.Date(StartDate))
  res.data <- res.data %>% mutate(EndDate = as.Date(EndDate))
  res.data <- res.data %>% mutate(OrderDate = as.Date(OrderDate))
  num.res <- length(res.data$OrderNumber)
  
  #Subset only camping reservations for selected dataset
  res.data <- res.data %>% mutate(FacilityID = as.integer(FacilityID))
  
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
} #Function
camp.2020 <- res.2020 %>% get.camping(2020)
camp.2019 <- res.2019 %>% get.camping(2019)
View(camp.2020[is.na(FacilityID),])
rm(res.2019, res.2020)

#Remove duplicates: THERE ARE NONE----
# camp.2020 <- camp.2020[!duplicated(camp.2020$OrderNumber),]
# camp.2019 <- camp.2019[!duplicated(camp.2019$OrderNumber),]
# camp.2020 <- camp.2020 %>% mutate(ProductID = as.character(ProductID))
# camp.2019 <- camp.2019 %>% mutate(ProductID = as.character(ProductID))
# camp.ALL <- rbind(camp.2020, camp.2019)
# num.all.camp.reservations <- length(camp.ALL$OrderNumber)
# #test <- camp.ALL[duplicated(camp.ALL$OrderNumber),] #There are 35,641 duplicates across the datasets based on OrderNumber
# camp.ALL <- camp.ALL %>% distinct(OrderNumber, .keep_all = TRUE)
# num.distinct.camp.reservations <- length(camp.ALL$OrderNumber)
# rm(camp.ALL)

#Add/fix state abbrvs, country regions----
add.country.regions.fast <- function(dat) {
  dat[,FacilityRegion := "Missing"]
  dat[FacilityState == "Maine" | FacilityState == "New Hampshire" | FacilityState == "Vermont" | FacilityState == "Massachusetts" | FacilityState == "Rhode Island" | FacilityState == "Connecticut" | FacilityState == "New York" | FacilityState == "New Jersey" | FacilityState == "Pennsylvania", FacilityRegion := "Northeast"]
  dat[FacilityState == "Ohio" | FacilityState == "Michigan" | FacilityState == "Indiana" | FacilityState == "Wisconsin" | FacilityState == "Illinois" | FacilityState == "Minnesota" | FacilityState == "Iowa" | FacilityState == "Missouri" | FacilityState == "North Dakota" | FacilityState == "South Dakota" | FacilityState == "Nebraska" | FacilityState == "Kansas", FacilityRegion := "Midwest"]
  dat[FacilityState == "Delaware" | FacilityState == "Maryland" | FacilityState == "Virginia" | FacilityState == "West Virginia" | FacilityState == "Kentucky" | FacilityState == "North Carolina" | FacilityState == "South Carolina" | FacilityState == "Tennessee" | FacilityState == "Georgia" | FacilityState == "Florida" | FacilityState == "Alabama" | FacilityState == "Mississippi" | FacilityState == "Arkansas" | FacilityState == "Louisiana" | FacilityState == "Texas" | FacilityState == "Oklahoma" | FacilityState == "District of Columbia", FacilityRegion := "South"]
  dat[FacilityState == "Montana" | FacilityState == "Idaho" | FacilityState == "Wyoming" | FacilityState == "Colorado" | FacilityState == "New Mexico" | FacilityState == "Arizona" | FacilityState == "Utah" | FacilityState == "Nevada" | FacilityState == "California" | FacilityState == "Oregon" | FacilityState == "Washington" | FacilityState == "Alaska" | FacilityState == "Hawaii", FacilityRegion := "West"]
  dat[,CustomerRegion := "Missing"]
  dat[CustomerState == "Maine" | CustomerState == "New Hampshire" | CustomerState == "Vermont" | CustomerState == "Massachusetts" | CustomerState == "Rhode Island" | CustomerState == "Connecticut" | CustomerState == "New York" | CustomerState == "New Jersey" | CustomerState == "Pennsylvania", CustomerRegion := "Northeast"]
  dat[CustomerState == "Ohio" | CustomerState == "Michigan" | CustomerState == "Indiana" | CustomerState == "Wisconsin" | CustomerState == "Illinois" | CustomerState == "Minnesota" | CustomerState == "Iowa" | CustomerState == "Missouri" | CustomerState == "North Dakota" | CustomerState == "South Dakota" | CustomerState == "Nebraska" | CustomerState == "Kansas", CustomerRegion := "Midwest"]
  dat[CustomerState == "Delaware" | CustomerState == "Maryland" | CustomerState == "Virginia" | CustomerState == "West Virginia" | CustomerState == "Kentucky" | CustomerState == "North Carolina" | CustomerState == "South Carolina" | CustomerState == "Tennessee" | CustomerState == "Georgia" | CustomerState == "Florida" | CustomerState == "Alabama" | CustomerState == "Mississippi" | CustomerState == "Arkansas" | CustomerState == "Louisiana" | CustomerState == "Texas" | CustomerState == "Oklahoma" | CustomerState == "District of Columbia", CustomerRegion := "South"]
  dat[CustomerState == "Montana" | CustomerState == "Idaho" | CustomerState == "Wyoming" | CustomerState == "Colorado" | CustomerState == "New Mexico" | CustomerState == "Arizona" | CustomerState == "Utah" | CustomerState == "Nevada" | CustomerState == "California" | CustomerState == "Oregon" | CustomerState == "Washington" | CustomerState == "Alaska" | CustomerState == "Hawaii", CustomerRegion := "West"]
  return(dat)
}
fix.months.fast <- function(dat) {
  dat[,ResMonthWritten := "Missing"]
  dat[ResMonth == 1, ResMonthWritten := "January"]
  dat[ResMonth == 2, ResMonthWritten := "February"]
  dat[ResMonth == 3, ResMonthWritten := "March"]
  dat[ResMonth == 4, ResMonthWritten := "April"]
  dat[ResMonth == 5, ResMonthWritten := "May"]
  dat[ResMonth == 6, ResMonthWritten := "June"]
  dat[ResMonth == 7, ResMonthWritten := "July"]
  dat[ResMonth == 8, ResMonthWritten := "August"]
  dat[ResMonth == 9, ResMonthWritten := "September"]
  dat[ResMonth == 10, ResMonthWritten := "October"]
  dat[ResMonth == 11, ResMonthWritten := "November"]
  dat[ResMonth == 12, ResMonthWritten := "December"]
  return(dat)
}
fix.state.abbreviations.state.char.fast <- function(dat) {
  dat[nchar(FacilityState)==2, FacilityState := abbr2state(FacilityState)]
  dat[nchar(CustomerState)==2, CustomerState := abbr2state(CustomerState)]
  dat[nchar(CustomerState)==3, CustomerState := abbr2state(substr(CustomerState, 0, 2))] #Catch a few like "CA "
  return(dat)
}
camp.2019 <- camp.2019 %>% fix.state.abbreviations.state.char.fast()
camp.2020 <- camp.2020 %>% fix.state.abbreviations.state.char.fast()
camp.2019 <- camp.2019 %>% add.country.regions.fast()
camp.2020 <- camp.2020 %>% add.country.regions.fast()

#Fix multi-state problem cases----
fix.problem.cases.1 <- function(dat){
  #Case 1
  dat[(FacilityID == 232210), `:=` (RegionDescription = "Intermountain Reg.",
                                                     ParentLocation = "Caribou-Targhee National Forest",
                                                     FacilityZIP = 83128,
                                                     FacilityState = "Wyoming",
                                                     FacilityLongitude = -111.0411,
                                                     FacilityLatitude = 43.19639,
                                                     Park = "ALPINE NORTH LOOP",
                                                     RegionCode = "F4")]
  #Case 2
  dat[(FacilityID == 232505), `:=` (RegionDescription = "Southeast Region",
                                    ParentLocation = "Big South Fork National River & Recreation Area",
                                    FacilityZIP = 42649,
                                    FacilityState = "Kentucky",
                                    FacilityLongitude = -84.51889,
                                    FacilityLatitude = 36.67806,
                                    Park = "BLUE HERON CAMPGROUND",
                                    RegionCode = "SER")]
  #Case 3
  dat[(FacilityID == 232768), `:=` (RegionDescription = "Pacific Southwest",
                                    ParentLocation = "Lake Tahoe Basin Management Unit",
                                    FacilityZIP = 89448,
                                    FacilityState = "Nevada",
                                    FacilityLongitude = -119.9486,
                                    FacilityLatitude = 38.98194,
                                    Park = "Nevada Beach Campground and Day Use Pavilion",
                                    RegionCode = "F5")]
  
  #Case 4
  dat[(FacilityID == 233467), `:=` (RegionDescription = "Northwestern Div.",
                                    ParentLocation = "Lewis and Clark Lake",
                                    FacilityZIP = 57078,
                                    FacilityState = "Nebraska",
                                    FacilityLongitude = -97.4825,
                                    FacilityLatitude = 42.85861,
                                    Park = "COTTONWOOD",
                                    RegionCode = "CG")]
  #Case 5
  dat[(FacilityID == 233530), `:=` (RegionDescription = "Southwestern Div.",
                                    ParentLocation = "Waurika Lake",
                                    FacilityZIP = 76365,
                                    FacilityState = "Oklahoma",
                                    FacilityLongitude = -98.07533,
                                    FacilityLatitude = 34.252,
                                    Park = "KIOWA PARK I",
                                    RegionCode = "CM")]
  #Case 6
  dat[(FacilityID == 251468), `:=` (RegionDescription = "Southeast Region",
                                    ParentLocation = "Big South Fork National River & Recreation Area",
                                    FacilityZIP = 42649,
                                    FacilityState = "Kentucky",
                                    FacilityLongitude = -84.52364,
                                    FacilityLatitude = 36.64108,
                                    Park = "Bear Creek Horse Camp",
                                    RegionCode = "SER")]
  #Case 7
  dat[(FacilityID == 251615), `:=` (RegionDescription = "Pacific Southwest",
                                    ParentLocation = "Eldorado National Forest",
                                    FacilityZIP = 95726,
                                    FacilityState = "California",
                                    FacilityLongitude = -120.4825,
                                    FacilityLatitude = 38.76639,
                                    Park = "BRIDAL VEIL GROUP AREA AND PICNIC GROUND",
                                    RegionCode = "F5")]
  #Case 8
  dat[(FacilityID == 272246), `:=` (RegionDescription = "Sierra Front Field Office",
                                    ParentLocation = "Indian Creek Recreation Area",
                                    FacilityZIP = 96120,
                                    FacilityState = "California",
                                    FacilityLongitude = -119.7849,
                                    FacilityLatitude = 38.74794,
                                    Park = "INDIAN CREEK CAMPGROUND (CA)",
                                    RegionCode = "LLNVC02000")]
  #Case 9
  dat[(FacilityID == 273821), `:=` (RegionDescription = "Southeast Region",
                                    ParentLocation = "Great Smoky Mountains National Park",
                                    FacilityZIP = 28785,
                                    FacilityState = "North Carolina",
                                    FacilityLongitude = -83.10417,
                                    FacilityLatitude = 35.75972,
                                    Park = "BIG CREEK CAMPGROUND (GREAT SMOKY MOUNTAINS NATIONAL PARK)",
                                    RegionCode = "SER")]
  #Case 10
  dat[(FacilityID == 274288), `:=` (RegionDescription = "Lake Havasu Field Office",
                                    ParentLocation = "Parker Strip Recreation Area",
                                    FacilityZIP = 92267,
                                    FacilityState = "California",
                                    FacilityLongitude = -114.2151,
                                    FacilityLatitude = 34.21088,
                                    Park = "CROSSROADS CAMPGROUND",
                                    RegionCode = "")]
  return(dat)
}
camp.2019 <- camp.2019 %>% fix.problem.cases.1()
camp.2020 <- camp.2020 %>% fix.problem.cases.1()

#Look at and remove canceled reservations----
canceled.2019 <- camp.2019[Status == "CLOSURE",]
canceled.2020 <- camp.2020[Status == "CLOSURE",]
#Get % cancelled
100*(length(canceled.2019$FacilityID)/length(camp.2019$FacilityID)) 
100*(length(canceled.2020$FacilityID)/length(camp.2020$FacilityID))
rm(canceled.2019, canceled.2020)
#Retain only those reservations that weren't cancelled
camp.2019 <- camp.2019[Status != "CLOSURE",]
camp.2020 <- camp.2020[Status != "CLOSURE",]


#Look at inconsistent data problem -> NOT AN ISSUE HERE! Yay cleaner data----
fix.inconsistent.data <- function(dat) {
  dat.campground <- dat %>% group_by(FacilityID) %>% summarise(Reservations = n())
  dat.campground.more.info <- dat %>% group_by(FacilityID, Agency, FacilityState, RegionDescription, 
                                               ParentLocation, Park, FacilityZIP, FacilityLongitude,
                                               FacilityLatitude, FacilityRegion) %>% summarise(Reservations = n())
  print(length(dat.campground$FacilityID))
  print(length(dat.campground.more.info$FacilityID))
  # anti <- anti_join(dat.campground.more.info, dat.campground)
  # problem.facilities <- unique(anti$FacilityID)
  # #For each facility with this problem, get the most common of each problem variable, then set ALL instances of this facility to be this
  # for (i in 1:length(problem.facilities)) {
  #   examine.problem <- dat %>% filter(FacilityID == problem.facilities[i])
  #   fZIP <- names(sort(table(examine.problem$FacilityZIP), decreasing = TRUE)[1])
  #   rDesc <- names(sort(table(examine.problem$RegionDescription), decreasing = TRUE)[1])
  #   pLocation <- names(sort(table(examine.problem$ParentLocation), decreasing = TRUE)[1])
  #   park <- names(sort(table(examine.problem$Park), decreasing = TRUE)[1])
  #   fLong <- as.numeric(names(sort(table(examine.problem$FacilityLongitude), decreasing = TRUE)[1]))
  #   fLat <- as.numeric(names(sort(table(examine.problem$FacilityLatitude), decreasing = TRUE)[1]))
  #   dat[(FacilityID == problem.facilities[i]), `:=` (RegionDescription = rDesc,
  #                                                    ParentLocation = pLocation,
  #                                                    FacilityZIP = fZIP,
  #                                                    FacilityLongitude = fLong,
  #                                                    FacilityLatitude = fLat,
  #                                                    Park = park)]
  # }
  return(dat)
}
camp.2019 <- camp.2019 %>% fix.inconsistent.data()
camp.2020 <- camp.2020 %>% fix.inconsistent.data()




#CAMPSITES UTIL DATA----

#Prep util data----
campsites <- campsites %>% select(-(CampsiteName:LastUpdatedDate))
setnames(campsites, old = "CampsiteID", new = "campsite_id")

util.2019 <- util.2019 %>% left_join(campsites, by = "campsite_id")
util.2020 <- util.2020 %>% left_join(campsites, by = "campsite_id")
rm(campsites)

#Bad FacilityID matches & Reserved >1 for Eric
# bad.match.2019 <- unique(util.2019[is.na(FacilityID),]$campsite_id)
# bad.match.2019 <- as.data.table(bad.match.2019)
# setnames(bad.match.2019, old = "x", new = "campsite_id")
# fwrite(bad.match.2019, "BadMatches2019.csv")
# reserved.multiple.2019 <- util.2019[reserved >1 ,]
# fwrite(reserved.multiple.2019, "ReservedMultiple2019.csv")

#Separate into calendar years
util.2019 <- util.2019 %>% mutate(RealYear = year(availability_date))
util.2020 <- util.2020 %>% mutate(RealYear = year(availability_date))
util.2019 <- util.2019[RealYear == 2019,]
util.2019 <- rbind(util.2019, util.2020[RealYear == 2019,])
util.2020 <- util.2020[RealYear == 2020,]

#Remove util instances without FacilityID, not included in reservation dataset
util.2019 <- util.2019[!is.na(FacilityID),]
util.2020 <- util.2020[!is.na(FacilityID),]

#Turn >1 into 1s, via conversation w/ Eric
#More information from Eric: >1s can actually be treated as what it says (true double-bookings). Very high numbers primarily Camp4 in Yosemite
# util.2019[reserved > 1, reserved := 1]
# util.2020[reserved > 1, reserved := 1]
# util.2019[web > 1, web := 1]
# util.2020[web > 1, web := 1]
# util.2019[fcfs > 1, fcfs := 1]
# util.2020[fcfs > 1, fcfs := 1]
# util.2019[field > 1, field := 1]
# util.2020[field > 1, field := 1]

#Add months and days to dataset

weekday.weekend <- function(dat){
  dat <- dat %>% mutate(DayType = case_when(Weekday == "Monday" ~ "Weekday",
                                            Weekday == "Tuesday" ~ "Weekday",
                                            Weekday == "Wednesday" ~ "Weekday",
                                            Weekday == "Thursday" ~ "Weekday",
                                            Weekday == "Friday" ~ "Weekday",
                                            Weekday == "Saturday" ~ "Weekend",
                                            Weekday == "Sunday" ~ "Weekend",
                                            TRUE ~ "Missing"))
  return(dat)
}
fix.months.fast <- function(dat) {
  dat[,ResMonthWritten := "Missing"]
  dat[ResMonth == 1, ResMonthWritten := "January"]
  dat[ResMonth == 2, ResMonthWritten := "February"]
  dat[ResMonth == 3, ResMonthWritten := "March"]
  dat[ResMonth == 4, ResMonthWritten := "April"]
  dat[ResMonth == 5, ResMonthWritten := "May"]
  dat[ResMonth == 6, ResMonthWritten := "June"]
  dat[ResMonth == 7, ResMonthWritten := "July"]
  dat[ResMonth == 8, ResMonthWritten := "August"]
  dat[ResMonth == 9, ResMonthWritten := "September"]
  dat[ResMonth == 10, ResMonthWritten := "October"]
  dat[ResMonth == 11, ResMonthWritten := "November"]
  dat[ResMonth == 12, ResMonthWritten := "December"]
  return(dat)
}

util.2019[,ResMonth := month(availability_date)]
util.2019[,Weekday := weekdays(availability_date)]
util.2020[,ResMonth := month(availability_date)]
util.2020[,Weekday := weekdays(availability_date)]
util.2019 <- util.2019 %>% fix.months.fast()
util.2020 <- util.2020 %>% fix.months.fast()
util.2019 <- util.2019 %>% weekday.weekend()
util.2020 <- util.2020 %>% weekday.weekend()

#Bind together and write
all.utils <- rbind(util.2019, util.2020)
rm(util.2019, util.2020)
View(all.utils[1:10,])
fwrite(all.utils, "All_utilization.csv")

fixed.facility.dats <- fread("FULLOccupancyRatesPlusSpatial.csv")
fixed.facility.dats <- fixed.facility.dats %>% distinct(FacilityID, .keep_all = TRUE)
fixed.facility.dats <- fixed.facility.dats %>% select(-NightDates, -number.campsites.existing, -(Weekday:ResYear), 
                                                      -(NightsReserved:ReservableOccupancyPercentFixed), -(NightsReservedFixed:OverallNPGroup))
fwrite(fixed.facility.dats, "FixedFacilityData.csv")

all.utils <- all.utils %>% left_join(fixed.facility.dats, by = "FacilityID")
View(all.utils[1:10,])
fwrite(all.utils, "All_utilization.csv")

#GetOverall#s
all.utils <- fread("All_utilization.csv")

#RemoveAK-HI
unique(all.utils$FacilityState)
all.utils.noAKHI <- all.utils %>% filter(FacilityState != "Alaska")
rm(all.utils)
all.utils.noAKHI <- all.utils.noAKHI %>% filter(FacilityState != "Hawaii")
unique(all.utils.noAKHI$FacilityState)
fwrite(all.utils.noAKHI, "All_utilization_NoAKHI.csv")

#Group and join spatial data
#CHOOSE DATASET HERE
#all.utils <- fread("All_utilization.csv")
all.utils.noAKHI <- fread("All_utilization_NoAKHI.csv")
spatial.analysis <- fread("USE_UniqueFacilityIDs_WithSpatialAnalysis.csv")
fixed.facility.dats <- fread("FixedFacilityData.csv")

#Function to run join and write datasets
join.spatial.data.utils <- function(dat, dat.name, spatial.analysis, fixed.facility.dats) {
  spatial.dat.utils.group <- dat %>% group_by(FacilityID, availability_date, RealYear, ResMonthWritten, DayType) %>%
    summarise(Available = sum(available), FCFS = sum(fcfs), Closed = sum(closed), Reserved = sum(reserved), 
              AvailAndFCFS = sum(available)+sum(fcfs), TotOccupancy = (sum(reserved)/(sum(available))))
  spatial.dat.utils.group <- spatial.dat.utils.group %>% left_join(fixed.facility.dats, by = "FacilityID")
  spatial.dat.utils.group <- spatial.dat.utils.group %>% left_join(spatial.analysis, by = "FacilityID")
  spatial.dat.utils.group <- spatial.dat.utils.group %>% as.data.table()
  
  add.year.section.fast <- function(dat) {
    dat[,YearSection := "Off-Season"]
    dat[ResMonthWritten == "June" | ResMonthWritten == "July" | ResMonthWritten == "August", YearSection := "Summer"]
    return(dat)
  }
  
  spatial.dat.utils.group <- spatial.dat.utils.group %>% add.year.section.fast()
  
  #NEED TO FIX OVER 100%s!
  spatial.dat.utils.group[, OccupancyOver100 := "No"]
  spatial.dat.utils.group[TotOccupancy > 1, OccupancyOver100 := "Yes"]
  spatial.dat.utils.group[, NightsReservedFixed := Reserved]
  spatial.dat.utils.group[TotOccupancy > 1, NightsReservedFixed := Available]
  spatial.dat.utils.group[, TotOccupancyFixed := TotOccupancy]
  spatial.dat.utils.group[TotOccupancy > 1, TotOccupancyFixed := 1]
  
  
  # ADD IN just IN/OUT PADUS / NPs/ etc
  spatial.dat.utils.group[, OverallPADUSGroup := "Outside PADUS 1-2"]
  spatial.dat.utils.group[PADUSGroup == "In PADUS 1-2", OverallPADUSGroup := "In PADUS 1-2"]
  spatial.dat.utils.group[, OverallNPGroup := "Outside NP"]
  spatial.dat.utils.group[NPGroup == "In NP", OverallNPGroup := "In NP"]
  
  fwrite(spatial.dat.utils.group, paste("SPATIAL_", dat.name, "_utilization.csv", sep = ""))
}

#join.spatial.data.utils(all.utils, "All", spatial.analysis, fixed.facility.dats)
join.spatial.data.utils(all.utils.noAKHI, "NoAKHI", spatial.analysis, fixed.facility.dats)


#Examine dataset
SPATIAL_NoAKHI_utilization <- fread("SPATIAL_NoAKHI_utilization.csv")
View(SPATIAL_NoAKHI_utilization[1:10,])
View(SPATIAL_NoAKHI_utilization[FacilityID == 232496,])
View(SPATIAL_NoAKHI_utilization[FacilityID == 232448,])



#Pull overall stats
util.2019.noAKHI <- SPATIAL_NoAKHI_utilization[RealYear == 2019,]
util.2020.noAKHI <- SPATIAL_NoAKHI_utilization[RealYear == 2020,]

#Get overall reservation %s
get.res.avail.plus.fcfs <- function(dat) {
  return(100*(sum(dat$Reserved)/(sum(dat$Available)+sum(dat$FCFS))))
}
get.res.avail <- function(dat) {
  return(100*(sum(dat$Reserved)/sum(dat$Available)))
}
get.avail.over.all <- function(dat) {
  return(100*(sum(dat$Available)/(sum(dat$Available)+sum(dat$FCFS))))
}

#Get summer subsets
get.summer.weekends <- function(dat) {
  dat <- dat[DayType == "Weekend",]
  dat <- dat[ResMonthWritten == "June" | ResMonthWritten == "July" | ResMonthWritten == "August",]
  return(dat)
}
get.summer.weekdays <- function(dat) {
  dat <- dat[DayType == "Weekday",]
  dat <- dat[ResMonthWritten == "June" | ResMonthWritten == "July" | ResMonthWritten == "August",]
  return(dat)
}
get.summer <- function(dat) {
  dat <- dat[ResMonthWritten == "June" | ResMonthWritten == "July" | ResMonthWritten == "August",]
  return(dat)
}

# WRONG CALCULATIONS, based on conversation w/ Eric
# sum.wends.2019 <- util.2019 %>% get.summer.weekends()
# sum.wends.2020 <- util.2020 %>% get.summer.weekends()
# Summer.Weekends.2019  <- 100*(sum(sum.wends.2019$reserved)/sum(sum.wends.2019$available))
# Summer.Weekends.2020 <- 100*(sum(sum.wends.2020$reserved)/sum(sum.wends.2020$available))
# Summer.Weekends.2019.web  <- 100*(sum(sum.wends.2019$web)/sum(sum.wends.2019$available))
# Summer.Weekends.2020.web <- 100*(sum(sum.wends.2020$web)/sum(sum.wends.2020$available))
# Overall.2019 <- 100*(sum(util.2019$reserved)/sum(util.2019$available))
# Overall.2020 <- 100*(sum(util.2020$reserved)/sum(util.2020$available))
# Overall.2019.web <- 100*(sum(util.2019$web)/sum(util.2019$reserved))
# Overall.2020.web <- 100*(sum(util.2020$web)/sum(util.2020$reserved))
# Overall.2019.field <- 100*(sum(util.2019$field)/sum(util.2019$reserved))
# Overall.2020.field <- 100*(sum(util.2020$field)/sum(util.2020$reserved))
Overall.2019.avail.over.tot.NOAK <- util.2019.noAKHI %>% get.avail.over.all()
Overall.2020.avail.over.tot.NOAK <- util.2020.noAKHI %>% get.avail.over.all()
Summer.2019.avail.over.tot.NOAK <- util.2019.noAKHI %>% get.summer() %>% get.avail.over.all()
Summer.2020.avail.over.tot.NOAK <- util.2020.noAKHI %>% get.summer() %>% get.avail.over.all()

#WITH AK-HI
# Overall.2019.plusfcfs <- util.2019 %>% get.res.avail.plus.fcfs()
# Overall.2020.plusfcfs <- util.2020 %>% get.res.avail.plus.fcfs()
# Summer.Weekends.2019.plusfcfs <- util.2019 %>%
#   get.summer.weekends() %>% get.res.avail.plus.fcfs()
# Summer.Weekends.2020.plusfcfs <- util.2020 %>%
#   get.summer.weekends() %>% get.res.avail.plus.fcfs()
# Summer.Weekdays.2019.plusfcfs  <- util.2019 %>%
#   get.summer.weekdays() %>% get.res.avail.plus.fcfs()
# Summer.Weekdays.2020.plusfcfs <- util.2020 %>%
#   get.summer.weekdays() %>% get.res.avail.plus.fcfs()
# Summer.2019.plusfcfs  <- util.2019 %>%
#   get.summer() %>% get.res.avail.plus.fcfs()
# Summer.2020.plusfcfs <- util.2020 %>%
#   get.summer() %>% get.res.avail.plus.fcfs()

#NoAK-HI FCFS
# NoAKHI.Overall.2019.plusfcfs <- util.2019.noAKHI %>% get.res.avail.plus.fcfs()
# NoAKHI.Overall.2020.plusfcfs <- util.2020.noAKHI %>% get.res.avail.plus.fcfs()
# NoAKHI.Summer.Weekends.2019.plusfcfs  <- util.2019.noAKHI %>% 
#   get.summer.weekends() %>% get.res.avail.plus.fcfs()
# NoAKHI.Summer.Weekends.2020.plusfcfs <- util.2020.noAKHI %>% 
#   get.summer.weekends() %>% get.res.avail.plus.fcfs()
# NoAKHI.Summer.Weekdays.2019.plusfcfs  <- util.2019.noAKHI %>% 
#   get.summer.weekdays() %>% get.res.avail.plus.fcfs()
# NoAKHI.Summer.Weekdays.2020.plusfcfs <- util.2020.noAKHI %>% 
#   get.summer.weekdays() %>% get.res.avail.plus.fcfs()
# NoAKHI.Summer.2019.plusfcfs  <- util.2019.noAKHI %>% 
#   get.summer() %>% get.res.avail.plus.fcfs()
# NoAKHI.Summer.2020.plusfcfs <- util.2020.noAKHI %>% 
#   get.summer() %>% get.res.avail.plus.fcfs()

#NoAK-HI NO FCFS
NoAKHI.Overall.2019 <- util.2019.noAKHI %>% get.res.avail()
NoAKHI.Overall.2020 <- util.2020.noAKHI %>% get.res.avail()
NoAKHI.Summer.Weekends.2019  <- util.2019.noAKHI %>% 
  get.summer.weekends() %>% get.res.avail()
NoAKHI.Summer.Weekends.2020 <- util.2020.noAKHI %>% 
  get.summer.weekends() %>% get.res.avail()
NoAKHI.Summer.Weekdays.2019  <- util.2019.noAKHI %>% 
  get.summer.weekdays() %>% get.res.avail()
NoAKHI.Summer.Weekdays.2020 <- util.2020.noAKHI %>% 
  get.summer.weekdays() %>% get.res.avail()
NoAKHI.Summer.2019  <- util.2019.noAKHI %>% 
  get.summer() %>% get.res.avail()
NoAKHI.Summer.2020 <- util.2020.noAKHI %>% 
  get.summer() %>% get.res.avail()


# #Get % of campsite reservations made online (in database!)
# 100*(length(util.2019[web == 1,]$FacilityID)/length(util.2019[reserved == 1,]$FacilityID))
# 100*(length(util.2020[web == 1,]$FacilityID)/length(util.2020[reserved == 1,]$FacilityID))
# 
# #Get % of campsite nights closed
# 100*(length(util.2019[closed == 1,]$FacilityID)/length(util.2019$FacilityID))
# 100*(length(util.2020[closed == 1,]$FacilityID)/length(util.2020$FacilityID))
# 100*(length(sum.wends.2019[closed == 1,]$FacilityID)/length(sum.wends.2019$FacilityID))
# 100*(length(sum.wends.2020[closed == 1,]$FacilityID)/length(sum.wends.2020$FacilityID))
# 
# #Get % of site-nights available on FCFS basis
# 100*(length(all.utils[fcfs == 1 & RealYear == 2019,]$FacilityID)/length(all.utils[RealYear == 2019,]$FacilityID))
# 100*(length(all.utils[fcfs == 1 & RealYear == 2019,]$FacilityID)/length(all.utils[RealYear == 2020,]$FacilityID))
# 100*(length(sum.wends.2019[fcfs == 1,]$FacilityID)/length(sum.wends.2019$FacilityID))
# 100*(length(sum.wends.2020[fcfs == 1,]$FacilityID)/length(sum.wends.2020$FacilityID))




#Clean up for mapping
For_Mapping_SPATIAL_NoAKHI_utilization <- SPATIAL_NoAKHI_utilization %>% select(-availability_date, -(FCFS:Reserved), -TotOccupancy, -OrgAbbrevName, -FacilityZIP, -(PADUS_NEAR_m:OverallNPGroup), -OccupancyOver100, -TotOccupancyFixed)
For_Mapping_SPATIAL_NoAKHI_utilization <- For_Mapping_SPATIAL_NoAKHI_utilization %>% relocate(AvailAndFCFS, .after = last_col())
For_Mapping_SPATIAL_NoAKHI_utilization <- For_Mapping_SPATIAL_NoAKHI_utilization %>% relocate(Available, .after = last_col())
For_Mapping_SPATIAL_NoAKHI_utilization <- For_Mapping_SPATIAL_NoAKHI_utilization %>%
  group_by(across(FacilityID:FacilityRegion)) %>% summarise(NightsReserved = sum(NightsReservedFixed), 
                                                         ReservableSites = mean(Available),
                                                         Available = sum(Available),  
                                                         AllSites = sum(AvailAndFCFS),
                                                         FullFacilitySize = mean(AvailAndFCFS))
View(For_Mapping_SPATIAL_NoAKHI_utilization[1:10,])

fwrite(For_Mapping_SPATIAL_NoAKHI_utilization, "For_Mapping_SPATIAL_NoAKHI_utilization.csv")

For_Mapping_SPATIAL_NoAKHI_utilization <- fread("For_Mapping_SPATIAL_NoAKHI_utilization.csv")
View(For_Mapping_SPATIAL_NoAKHI_utilization[FacilityID==251535,])

CA_Stats <- For_Mapping_SPATIAL_NoAKHI_utilization[FacilityState=='California',]
CA_Stats <- CA_Stats %>% select(-ReservableSites, -AllSites, -FullFacilitySize)
fwrite(CA_Stats, "California_2019_2020_Utilization.csv")

View(all.utils.noAKHI[FacilityID==251535,])
sum(all.utils.noAKHI[FacilityID==251535,]$reserved)/sum(all.utils.noAKHI[FacilityID==251535,]$available)
sum(all.utils.noAKHI[FacilityID==232453,]$reserved)/sum(all.utils.noAKHI[FacilityID==232453,]$available)

unique(all.utils.noAKHI[FacilityID==251535,]$campsite_id)

#Group into one entry for each facility, on summer weekends only, avg across 2019 & 2020, export to csv


#Perform calculations on basic state/region stats
statestats <- fread("StateLandUse_Protection.csv")
statestats[State == "District of Columbia", `Gap 2 Acres` := 0]
statestats <- statestats %>% mutate(`Gap 2 Acres` = as.numeric(`Gap 2 Acres`))
str(statestats)
statestats <- statestats %>% filter(State != "Alaska" & State != "Hawaii")
statestats <- statestats %>% group_by(Region) %>% summarise_at(vars(`Gap 1 Acres`:Full.State.Acreage, `Cropland  5/ Acres`:`Total land area 2/ Acres`), sum)
statestats <- as.data.table(statestats)
statestats <- statestats[,PercentOfLand_Urban := 100* (`Urban areas Acres`/`Total land area 2/ Acres`)]
statestats <- statestats[,PercentOfLand_Gap12 := 100* (`Total GAP 1-2 Acres`/`Total land area 2/ Acres`)]
fwrite(statestats, "RegionLandUse_Protection.csv")
