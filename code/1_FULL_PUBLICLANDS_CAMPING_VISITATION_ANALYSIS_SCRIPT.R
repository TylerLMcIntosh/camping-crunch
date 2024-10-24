
# PUBLIC LANDS VISITATION: FULL ANALYSIS SCRIPT
# Tyler McIntosh, 2021

#SCRIPT OVERVIEW
#This script contains analysis performed on RIDB data by Center for Western Priorities in summer of 2021.
#Throughout processing, datasets were subsetted and combined as needed, in addition to being written and read as needed.
#Some write/reads (and associated removal of coerced columns) and much testing script have been eliminated from this document for clarity.

#Setup & Load data used in analysis----

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
    #"rnaturalearth",
    #"rnaturalearthdata",
    #"ggspatial",
    "sf",
    "usmap",
    #"vroom",
    "data.table",
    #"disk.frame",
    "pryr",
    "usdata"),
  auto_install = "y"
)

# Manage directories
raw_dir <- here::here("data/raw")
derived_dir <- here::here("data/derived")
dir_ensure(derived_dir)


# #Set directories
# main.directory <- "D:/Storage/RecreationAnalysis2021/"
# desktop <- "C:/Users/tyler/Desktop/"
# viz.filter.directory <- "D:/Storage/RecreationAnalysis2021/Data viz filters/"
# 
# #Set working directory
# setwd(main.directory)

#Set date format for dataset
date.format <- "%Y-%m-%d"

# Unzip all raw data files if still zipped
unzip_if_zipped(here::here(raw_dir, "Recreation.govReservationData"))

#Load data used in analysis
#Load camping data
campsites <- data.table::fread(here::here(raw_dir, "RIDBFullExport_V1_CSV/Campsites_API_v1.csv"))
facilities <- data.table::fread(here::here(raw_dir, "RIDBFullExport_V1_CSV/Facilities_API_v1.csv"))
orgs <- data.table::fread(here::here(raw_dir, "RIDBFullExport_V1_CSV/Organizations_API_v1.csv"))

#Load all raw datasets
res.2020 <- data.table::fread(here::here(raw_dir, "Recreation.govReservationData/reservations2020/FY20 Historical Reservations Full.csv"))
res.2019 <- data.table::fread(here::here(raw_dir, "Recreation.govReservationData/reservations2019/reservations2019.csv"))
res.2018 <- data.table::fread(here::here(raw_dir, "Recreation.govReservationData/reservations2018/reservations2018.csv"))
res.2017 <- data.table::fread(here::here(raw_dir, "Recreation.govReservationData/reservations2017/2017.csv"))
res.2016 <- data.table::fread(here::here(raw_dir, "Recreation.govReservationData/reservations2016/2016.csv"))
res.2015 <- data.table::fread(here::here(raw_dir, "Recreation.govReservationData/reservations2015/2015.csv"))
res.2014 <- data.table::fread(here::here(raw_dir, "Recreation.govReservationData/reservations2014/2014.csv"))

# length(res.2020[date(orderdate) == date(startdate)]$orderdate)/length(res.2020$orderdate)
# length(res.2019[date(orderdate) == date(startdate)]$orderdate)/length(res.2019$orderdate)
# length(res.2018[date(OrderDate) == date(StartDate)]$OrderDate)/length(res.2018$OrderDate)
# length(res.2017[date(OrderDate) == date(StartDate)]$OrderDate)/length(res.2017$OrderDate)
# length(res.2016[date(OrderDate) == date(StartDate)]$OrderDate)/length(res.2016$OrderDate)
# length(res.2015[date(OrderDate) == date(StartDate)]$OrderDate)/length(res.2015$OrderDate)
# length(res.2014[date(OrderDate) == date(StartDate)]$OrderDate)/length(res.2014$OrderDate)
# 
# 
# date(res.2020$orderdate[1])

#CAMPING RESERVATIONS ANALYSIS----

#PART 1: Clean reservation data columns to create consistent data----
res.2020 <- res.2020 |>
  select(-discount, -nights, -equipmentdescription, -equipmentlength) |>
  mutate(facilityid = as.integer(facilityid))
res.2019 <- res.2019 %>% select(-discount, -equipmentdescription, -equipmentlength)
res.2018 <- res.2018 %>% select(-EntityID,-(Tent:LatLongPoint))
res.2017 <- res.2017 %>% select(-EntityID,-(Tent:Marinaboat))
res.2016 <- res.2016 %>% select(-EntityID,-(Tent:Marinaboat))
res.2015 <- res.2015 %>% select(-EntityID,-(Tent:Marinaboat))
res.2014 <- res.2014 %>% select(-EntityID,-(Tent:Marinaboat))
#Change column titles of 2019/20 to capitalization of earlier datasets, changes name of inventorytype to EntityType
colnames(res.2020) <- colnames(res.2015)
colnames(res.2019) <- colnames(res.2015)

#Get total data
total.data.entries <- length(res.2020$OrderNumber) + length(res.2019$OrderNumber) + length(res.2018$OrderNumber) + length(res.2017$OrderNumber) + length(res.2016$OrderNumber) + length(res.2015$OrderNumber) + length(res.2014$OrderNumber)


#PART 2: Filter camping reservations from full reservation datasets & create new information from existing columns----

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
  # print(typeof(res.camp$FacilityID))
  # print(typeof(campsites$FacilityID))
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

#Full
camp.2020 <- res.2020 %>% get.camping(2020)
camp.2019 <- res.2019 %>% get.camping(2019)
camp.2018 <- res.2018 %>% get.camping(2018)
camp.2017 <- res.2017 %>% get.camping(2017)
camp.2016 <- res.2016 %>% get.camping(2016)
camp.2015 <- res.2015 %>% get.camping(2015)
camp.2014 <- res.2014 %>% get.camping(2014)

#write intermediate files (Save progress)
# data.table::fwrite(camp.2020, here::here(derived_dir, "Camp2020.gz"))
# data.table::fwrite(camp.2019, here::here(derived_dir, "Camp2019.gz"))
# data.table::fwrite(camp.2018, here::here(derived_dir, "Camp2018.gz"))
# data.table::fwrite(camp.2017, here::here(derived_dir, "Camp2017.gz"))
# data.table::fwrite(camp.2016, here::here(derived_dir, "Camp2016.gz"))
# data.table::fwrite(camp.2015, here::here(derived_dir, "Camp2015.gz"))
# data.table::fwrite(camp.2014, here::here(derived_dir, "Camp2014.gz"))

#Clear out un-needed data
rm(res.2014, res.2015, res.2016, res.2017, res.2018, res.2019, res.2020)
gc()








#PART 3: Continue to clean data & remove all duplicates across years and datasets----
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

#Combine datasets, remove duplicates across years
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
gc()

#Write Clean Datasets
data.table::fwrite(camp.2020, here::here(derived_dir, "Camp2020Clean.gz"))
data.table::fwrite(camp.2019, here::here(derived_dir, "Camp2019Clean.gz"))
data.table::fwrite(camp.2018, here::here(derived_dir, "Camp2018Clean.gz"))
data.table::fwrite(camp.2017, here::here(derived_dir, "Camp2017Clean.gz"))
data.table::fwrite(camp.2016, here::here(derived_dir, "Camp2016Clean.gz"))
data.table::fwrite(camp.2015, here::here(derived_dir, "Camp2015Clean.gz"))
data.table::fwrite(camp.2014, here::here(derived_dir, "Camp2014Clean.gz"))


# To read in if starting partway through after crash
if(!exists("camp.2020")) {camp.2020 <- data.table::fread(here::here(derived_dir, "Camp2020Clean.gz"))}
if(!exists("camp.2019")) { camp.2019 <- data.table::fread(here::here(derived_dir, "Camp2019Clean.gz"))}
if(!exists("camp.2018")) { camp.2018 <- data.table::fread(here::here(derived_dir, "Camp2018Clean.gz"))}
if(!exists("camp.2017")) { camp.2017 <- data.table::fread(here::here(derived_dir, "Camp2017Clean.gz"))}
if(!exists("camp.2016")) { camp.2016 <- data.table::fread(here::here(derived_dir, "Camp2016Clean.gz"))}
if(!exists("camp.2015")) { camp.2015 <- data.table::fread(here::here(derived_dir, "Camp2015Clean.gz"))}
if(!exists("camp.2014")) { camp.2014 <- data.table::fread(here::here(derived_dir, "Camp2014Clean.gz"))}


#PART 4: Get basic camping stats across years----

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
write.csv(camp.summary, here::here(derived_dir, "CampSummaryByDataYear.csv"))

ggplot(camp.summary) + geom_point(aes(x=DataYear, y=CampRes))
ggplot(camp.summary) + geom_point(aes(x=DataYear, y=CampDays))
ggplot(camp.summary) + geom_point(aes(x=DataYear, y=CampNights))

#PART 5: Expand reservation data to be for each night-necessary step for analyzing reservable occupancy rates. Split into calendar year data----

#Expand dataset
expand.data <- function(dats){
#NEED TO ADD MISSING ROW HERE!!!!!!!!!
  dats <- dats %>% mutate(NightDates = mapply(function(x,y) toString(head(seq(as.Date(x), as.Date(y), by="days"), -1)), StartDate, EndDate))
  dats.expand <- dats %>% separate_rows(NightDates, sep = ", ", convert = TRUE) #SEPARATE ROWS
  dats.expand <- dats.expand %>% mutate(NightDates = as.Date(NightDates))
  dats.expand <- dats.expand %>% mutate(Weekday = weekdays(NightDates))
  dats.expand <- dats.expand %>% mutate(ResYear = year(NightDates))
  dats.expand <- dats.expand %>% mutate(ResMonth = month(NightDates))
  return(dats.expand)
}

#Run expand on all datasets
camp.2020.expand <- camp.2020 %>% expand.data()
rm(camp.2020)
camp.2019.expand <- camp.2019 %>% expand.data()
rm(camp.2019)
camp.2018.expand <- camp.2018 %>% expand.data()
rm(camp.2018)
camp.2017.expand <- camp.2017 %>% expand.data()
rm(camp.2017)
camp.2016.expand <- camp.2016 %>% expand.data()
rm(camp.2016)
camp.2015.expand <- camp.2015 %>% expand.data()
rm(camp.2015)
camp.2014.expand <- camp.2014 %>% expand.data()
rm(camp.2014)
#beep()

#Create "true" datasets and export to CSVs

make.true.dataset <- function(yr){
  true.camp <- rbind(camp.2020.expand %>% filter(ResYear == yr), 
                     camp.2019.expand %>% filter(ResYear == yr), 
                     camp.2018.expand %>% filter(ResYear == yr), 
                     camp.2017.expand %>% filter(ResYear == yr), 
                     camp.2016.expand %>% filter(ResYear == yr), 
                     camp.2015.expand %>% filter(ResYear == yr), 
                     camp.2014.expand %>% filter(ResYear == yr))
  print(paste("Filter Done", yr))
  data.table::fwrite(true.camp, here::here(derived_dir, paste("True", yr, "Camping_Expanded.gz", sep="")))
  print(paste("Write True CSV Compressed Done", yr))
  rm(true.camp)
}

make.true.dataset(2021)
make.true.dataset(2020)
make.true.dataset(2019)
make.true.dataset(2018)
make.true.dataset(2017)
make.true.dataset(2016)
make.true.dataset(2015)
make.true.dataset(2014)

rm(camp.2014.expand, camp.2015.expand, camp.2016.expand, camp.2017.expand, camp.2018.expand, camp.2019.expand, camp.2020.expand)
gc()

#PART 6: Remove unnecessary data from expanded datasets, clean both expanded and normal reservation datasets, combine into full datasets----

#CLEANING FUNCTIONS
expand.clean <- function(dat) {
  dat <- dat %>% select(-UseFee, -TranFee, -AttrFee, -Tax, -HistoricalReservationID, -OrgID, -CodeHierarchy, -SiteType, -UseType, -ProductID, -NumberOfPeople, -DaysInAdvance, -EntityType, -StartDate, -EndDate, -OrderDate)
  dat <- dat %>% fix.state.abbreviations.state.char.fast()
  dat <- dat %>% mutate(ResYear = as.character(ResYear))
  return(dat)
}
norm.clean <- function(dat) {
  dat <- dat %>% select(-UseFee, -TranFee, -AttrFee, -Tax, -HistoricalReservationID, -OrgID, -CodeHierarchy, -SiteType, -UseType, -ProductID, -EntityType)
  dat <- dat %>% fix.state.abbreviations.state.char.fast()
  return(dat)
}

fix.state.abbreviations.state.char.fast <- function(dat) {
  dat[nchar(FacilityState)==2, FacilityState := abbr2state(FacilityState)]
  dat[nchar(CustomerState)==2, CustomerState := abbr2state(CustomerState)]
  dat[nchar(CustomerState)==3, CustomerState := abbr2state(substr(CustomerState, 0, 2))] #Catch a few like "CA "
  return(dat)
}

#Summarizing function
add.summary <- function(dat) {
  f.sum <- dat %>% group_by(FacilityID, NightDates, ResYear)%>% summarise(NightsReserved = n())
  #s.State <- dat %>% group_by(FacilityState, ResYear) %>% summarise(NightsReserved = n(), AvgDaysAhead = mean(DaysInAdvance))
  #s.Weekday <- dat %>% group_by(Weekday, ResYear) %>% summarise(NightsReserved = n(), AvgDaysAhead = mean(DaysInAdvance))
  #s.Month <- dat %>% group_by(ResMonth, ResYear) %>% summarise(NightsReserved = n(), AvgDaysAhead = mean(DaysInAdvance))
  #full.s <- dat %>% group_by(ResYear, ResMonth, Weekday, FacilityState, Agency) %>% summarise(NightsReserved = n(), AvgDaysAhead = mean(DaysInAdvance))
  #sumState <<- rbind(sumState, s.State)
  #sumWeekday <<- rbind(sumWeekday, s.Weekday)
  #sumMonth <<- rbind(sumMonth, s.Month)
  #fullSum <<- rbind(fullSum, full.s)
  facilitySum <<- rbind(facilitySum, f.sum)
}

#Do the things with expanded dataset
deal.with.expanded <- function(dat) {
  dat <- dat %>% expand.clean()
  add.summary(dat)
  full.expand <<- rbind(full.expand, dat)
  print(paste("Done with ", dat$ResYear[1], " dataset", sep = ""))
}

#Do the things with normal dataset
deal.with.norm <- function(dat) {
  dat <- dat %>% norm.clean()
  full.norm <<- rbind(full.norm, dat)
  print("Done with dataset")
}

#START  CODE
camp.2020.expand <- data.table::fread(here::here(derived_dir, "True2020Camping_Expanded.gz"))
camp.2020.expand <- camp.2020.expand %>% expand.clean()

#Use 2020 dataset to start
facilitySum <- camp.2020.expand %>% group_by(FacilityID, NightDates, ResYear)%>% summarise(NightsReserved = n())
full.expand <- camp.2020.expand
rm(camp.2020.expand)

#Add in other data
camp.2019.expand <-  data.table::fread(here::here(derived_dir, "True2019Camping_Expanded.gz"))
camp.2019.expand %>% deal.with.expanded()
rm(camp.2019.expand)

camp.2018.expand <-  data.table::fread(here::here(derived_dir, "True2018Camping_Expanded.gz"))
camp.2018.expand %>% deal.with.expanded()
rm(camp.2018.expand)

camp.2017.expand <-  data.table::fread(here::here(derived_dir, "True2017Camping_Expanded.gz"))
camp.2017.expand %>% deal.with.expanded()
rm(camp.2017.expand)

camp.2016.expand <-  data.table::fread(here::here(derived_dir, "True2016Camping_Expanded.gz"))
camp.2016.expand %>% deal.with.expanded()
rm(camp.2016.expand)

camp.2015.expand <-  data.table::fread(here::here(derived_dir, "True2015Camping_Expanded.gz"))
camp.2015.expand %>% deal.with.expanded()
rm(camp.2015.expand)

camp.2014.expand <-  data.table::fread(here::here(derived_dir, "True2014Camping_Expanded.gz"))
camp.2014.expand %>% deal.with.expanded()
rm(camp.2014.expand)

#Write compiled dataset
data.table::fwrite(facilitySum, here::here(derived_dir, "FacilitySum.csv"))
data.table::fwrite(full.expand, here::here(derived_dir, "FULL_ExpandedDataset.gz"))
rm(full.expand, facilitySum)

#Do normal datasets
camp.2020 <- fread(here::here(derived_dir, "Camp2020Clean.gz"))
camp.2020 <- camp.2020 %>% norm.clean()
full.norm <- camp.2020
rm(camp.2020)

camp.2019 <- fread(here::here(derived_dir, "Camp2019Clean.gz"))
camp.2019 %>% deal.with.norm()
rm(camp.2019)

camp.2018 <- fread(here::here(derived_dir, "Camp2018Clean.gz"))
camp.2018 %>% deal.with.norm()
rm(camp.2018)

camp.2017 <- fread(here::here(derived_dir, "Camp2017Clean.gz"))
camp.2017 %>% deal.with.norm()
rm(camp.2017)

camp.2016 <- fread(here::here(derived_dir, "Camp2016Clean.gz"))
camp.2016 %>% deal.with.norm()
rm(camp.2016)

camp.2015 <- fread(here::here(derived_dir, "Camp2015Clean.gz"))
camp.2015 %>% deal.with.norm()
rm(camp.2015)

camp.2014 <- fread(here::here(derived_dir, "Camp2014Clean.gz"))
camp.2014 %>% deal.with.norm()
rm(camp.2014)

#Write compiled dataset
data.table::fwrite(full.norm, here::here(derived_dir, "FULL_NormalDataset.gz"))
rm(full.norm)

gc()

#Finished
print("Done")

#PART 7: Fix state abbreviations, months, add country regions to datasets----
#Create functions to fix some data
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


#ADD FIXES TO FULL DATASETS

if(!exists("full.expanded")) {full.expanded <- data.table::fread(here::here(derived_dir, "FULL_ExpandedDataset.gz"))}

full.expanded <- data.table::fread(here::here(derived_dir, "FULL_ExpandedDataset.gz"))
full.expanded <- full.expanded %>% fix.months.fast()
full.expanded <- full.expanded %>% fix.state.abbreviations.state.char.fast()
full.expanded <- full.expanded %>% add.country.regions.fast()
gc()
fwrite(full.expanded, here::here(derived_dir, "FULL_ExpandedDataset2.gz"))
print("Completed data export")
rm(full.expanded)
gc()

full.normal <- fread(here::here(derived_dir, "FULL_NormalDataset.gz"))
full.normal <- full.normal %>% fix.state.abbreviations.state.char.fast()
full.normal <- full.normal %>% mutate(RStartYr = year(StartDate))
full.normal <- full.normal %>% add.country.regions.fast()
full.normal <- full.normal %>% mutate(InOut = case_when(CustomerState == "" ~ "Can't Determine",
                                                        FacilityState == "" ~ "Can't Determine",
                                                        CustomerState == FacilityState ~ "In-State",
                                                        CustomerState != FacilityState ~ "Out-of-State",
                                                        TRUE ~ "Missing"))
fwrite(full.normal, here::here(derived_dir, "FULL_NormalDataset2.gz"))
rm(full.normal)
gc()



#PART 8: Find and fix inconsistencies in datasets that prevent accurate grouping----

full.expanded <- fread(here::here(derived_dir, "FULL_ExpandedDataset2.gz"))

#Fix Problem cases in expanded dataset
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
#FOUND, FIXED INCONSISTENT DATA PROBLEM in expanded dataset
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
fwrite(full.expanded.campground.nights.more.info, here::here(derived_dir, "ExpandedFixed_GroupedByFacilityIDAndNightDates.gz"))
gc()
fwrite(full.expanded.fixed, here::here(derived_dir, "FullExpandedFixed.gz"))
gc()


#CAMPSITE ANALYSIS----

#PART 1: Analyze campsites----
campsites <- data.table::fread(here::here(raw_dir, "RIDBFullExport_V1_CSV/Campsites_API_v1.csv"))

#Fixdate
campsites <- campsites[,CreatedDateFix:= as.Date(CreatedDate)]

#Remove duplicates
campsites <- distinct(campsites)
#make dataframe of when added
campsites.existing <- campsites %>% group_by(CreatedDateFix) %>% summarize(AddedCampsites = n())
campsites.existing <- campsites.existing %>% mutate(ExistingCampsites = cumsum(AddedCampsites))

#View campsite created dates
ggplot(campsites, aes(CreatedDateFix))+geom_histogram(bins=60)
#It appears that the vast majority of campsites in the database were created in the beginning of 2014

#Expand dataset for full range of dates when campsites were added to the database
campsites.existing.full <- campsites.existing
fill <- data.table(CreatedDateFix = character(), AddedCampsites = integer(), ExistingCampsites = integer())

head(campsites.existing)
head(campsites.existing.full)

for (i in c(1:280)) {
  between.dats <- tail(head((seq(campsites.existing$CreatedDateFix[i], campsites.existing$CreatedDateFix[i+1], by="days")), -1), -1)
  fill <- data.table(CreatedDateFix = as.Date(between.dats), AddedCampsites = 0, ExistingCampsites = campsites.existing$ExistingCampsites[i])
  campsites.existing.full <- dplyr::bind_rows(campsites.existing.full, fill)
}

campsites.existing.full <- campsites.existing.full |>
  dplyr::arrange("CreatedDateFix") %>%
  filter(complete.cases(.))
data.table::fwrite(campsites.existing.full, here::here(derived_dir, "FullCalendarListCampsitesExisting.gz"))

#Make, explore campgrounds

#Number of campsites in each facility
campgrounds <- campsites %>% group_by(FacilityID) %>% summarize(number.campsites.total = n())
write.csv(campgrounds, here::here(derived_dir, "Campgrounds2021.csv"))
ggplot(campgrounds, aes(number.campsites.total)) + geom_histogram(bins = 100)

#Figure out if campgrounds are usually added all at once, or in pieces)
campgrounds.created.date <- campsites %>%
  group_by(FacilityID, CreatedDateFix) %>%
  summarize("number.campsites" = n())
campgrounds.created.date <- left_join(campgrounds.created.date, campgrounds, by = c("FacilityID"))
campgrounds.created.date <- campgrounds.created.date %>% mutate(percentage = 100*(number.campsites/number.campsites.total))
ggplot(campgrounds.created.date, aes(percentage)) + geom_histogram(bins = 100)
print(paste((sum(campgrounds.created.date$percentage > 95, na.rm = TRUE) / sum(campgrounds.created.date$percentage >= 50, na.rm = TRUE)*100), "% of the campgrounds had 95% of their campsites immediately upon inclusion"))

#PART 2: Expand existing campgrounds dataset----

#Get campgrounds with only one entry in created.date, meaning they were only added once. All of these can be mutated to get new data
campgrounds.existing.full <- campgrounds.created.date %>% group_by(FacilityID) %>% filter(n() == 1) %>% mutate(FirstDate = case_when(CreatedDateFix == "2014-05-02" ~ as.Date("2014-01-01"), TRUE ~ CreatedDateFix)) %>% mutate(LastDate = as.Date("2021-08-19")) %>% mutate(number.campsites.existing = number.campsites.total)

#Get campgrounds with multiple entries in created.date, meaning they were added to multiple times
campground.nums.multi.add <- campgrounds.created.date %>% group_by(FacilityID) %>% filter(n() > 1) #get all campgrounds that have been added to multiple times
unique.nums.multi.add <- unique(campground.nums.multi.add$FacilityID) #get unique campground IDs for campgrounds that have been added to multiple times

#For each unique campground number added to multiple times, pull all entries from campgrounds.created.date and create new data
for (i in unique.nums.multi.add) { #Replace test.nums with unique.nums.multi.add when ready
  campsite.additions <- campgrounds.created.date %>% filter(FacilityID == i) %>% arrange(CreatedDateFix) #Get additions and put in  temporal order
  num.additions <- length(campsite.additions$FacilityID) #Get number of additions
  #Make empty vectors for new data
  LastDates <- c()
  number.campsites.existing <- c()
  FirstDates <- c()
  for (d in c(1:num.additions)) { #Run for loop over each addition, create FirstDates, LastDates, number of campsites existing
    if (d == 1) {
      number.campsites.existing <- number.campsites.existing %>% append(campsite.additions$number.campsites[d], after = length(number.campsites.existing))
      if (campsite.additions$CreatedDateFix[d] == "2014-05-02") {
        FirstDates <- FirstDates %>% append(as.Date("2014-01-01"), after = length(FirstDates))
      } else {
        FirstDates <- FirstDates %>% append(campsite.additions$CreatedDateFix[d], after = length(FirstDates))
      }
      LastDates <- LastDates %>% append(campsite.additions$CreatedDateFix[d+1]-1, after = length(LastDates))
    } else if (d == num.additions) {
      LastDates <- LastDates %>% append(as.Date("2021-08-19"), after = length(LastDates))
      FirstDates <- FirstDates %>% append(campsite.additions$CreatedDateFix[d], after = length(FirstDates))
      number.campsites.existing <- number.campsites.existing %>% append(campsite.additions$number.campsites[d] + last(number.campsites.existing), after = length(number.campsites.existing))
    } else {
      FirstDates <- FirstDates %>% append(campsite.additions$CreatedDateFix[d], after = length(FirstDates))
      LastDates <- LastDates %>% append(campsite.additions$CreatedDateFix[d+1]-1, after = length(LastDates))
      number.campsites.existing <- number.campsites.existing %>% append(campsite.additions$number.campsites[d] + last(number.campsites.existing), after = length(number.campsites.existing))
    }
  }
  campsite.additions <- campsite.additions %>% cbind(FirstDate = FirstDates, LastDate = LastDates, number.campsites.existing = number.campsites.existing) #bind new data
  campgrounds.existing.full <- campgrounds.existing.full %>% rbind(campsite.additions) #bind to full dataset
}

#Create new column with all dates for each entry
campgrounds.existing.full <- campgrounds.existing.full %>% mutate(AllDates = mapply(function(x,y) toString(seq(as.Date(x), as.Date(y), by="days")), FirstDate, LastDate))
#Create new, expanded dataset with a day for each and every campground and the number of campsites that exist
campgrounds.existing.full.expand <- campgrounds.existing.full %>% separate_rows(AllDates, sep = ", ", convert = TRUE)
#Write file
fwrite(campgrounds.existing.full.expand, here::here(derived_dir, "CampgroundsExistingFullExpand.gz"))


#PART 3: Prep for join----

camps.exist.expand <- fread(here::here(derived_dir, "CampgroundsExistingFullExpand.gz"))
facilities <- facilities %>% mutate(FacilityID = as.integer(FacilityID))
#All campsites in the dataset have an associated Facility in the dataset
# testcampfacilities <- left_join(campsites, facilities, by = "FacilityID")
# test <-unique(testcampfacilities$CampsiteName)
# nas <- testcampfacilities[is.na(FacilityName), ]

#Prep camping information
#Add basic facility data, 
camps.exist.expand <- camps.exist.expand %>% left_join(facilities, by = "FacilityID")
mini <- camps.exist.expand[1:100,]
camps.exist.expand <- camps.exist.expand %>% select(-CreatedDateFix, -number.campsites, -number.campsites.total,
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

#Write to lat/long csv for GIS analysis
camping.facilities.all <- camps.exist.expand %>% distinct(FacilityID, .keep_all = TRUE) %>% select(FacilityID, FacilityLatitude, FacilityLongitude)
fwrite(camping.facilities.all, here::here(derived_dir, "ALLSpatialCampingFacilities.gz"))


#PERFORM GIS ANALYSIS
#Add back in to full campsite dataset
spatial.analyzed.campgrounds <- fread(here::here(raw_dir, "Facilities_Spatial_Analysis_2.csv"))
spatial.analyzed.campgrounds <- spatial.analyzed.campgrounds %>% select(-OID_, -TARGET_FID)

#Add FacilityRegion, create groups from proximity analysis
add.country.regions.facilities <- function(dat) {
  dat[,FacilityRegion := "Missing"]
  dat[FacilityState == "Maine" | FacilityState == "New Hampshire" | FacilityState == "Vermont" | FacilityState == "Massachusetts" | FacilityState == "Rhode Island" | FacilityState == "Connecticut" | FacilityState == "New York" | FacilityState == "New Jersey" | FacilityState == "Pennsylvania", FacilityRegion := "Northeast"]
  dat[FacilityState == "Ohio" | FacilityState == "Michigan" | FacilityState == "Indiana" | FacilityState == "Wisconsin" | FacilityState == "Illinois" | FacilityState == "Minnesota" | FacilityState == "Iowa" | FacilityState == "Missouri" | FacilityState == "North Dakota" | FacilityState == "South Dakota" | FacilityState == "Nebraska" | FacilityState == "Kansas", FacilityRegion := "Midwest"]
  dat[FacilityState == "Delaware" | FacilityState == "Maryland" | FacilityState == "Virginia" | FacilityState == "West Virginia" | FacilityState == "Kentucky" | FacilityState == "North Carolina" | FacilityState == "South Carolina" | FacilityState == "Tennessee" | FacilityState == "Georgia" | FacilityState == "Florida" | FacilityState == "Alabama" | FacilityState == "Mississippi" | FacilityState == "Arkansas" | FacilityState == "Louisiana" | FacilityState == "Texas" | FacilityState == "Oklahoma" | FacilityState == "District of Columbia", FacilityRegion := "South"]
  dat[FacilityState == "Montana" | FacilityState == "Idaho" | FacilityState == "Wyoming" | FacilityState == "Colorado" | FacilityState == "New Mexico" | FacilityState == "Arizona" | FacilityState == "Utah" | FacilityState == "Nevada" | FacilityState == "California" | FacilityState == "Oregon" | FacilityState == "Washington" | FacilityState == "Alaska" | FacilityState == "Hawaii", FacilityRegion := "West"]
  return(dat)
}
setnames(spatial.analyzed.campgrounds, old = "STATE", new = "FacilityState")
spatial.analyzed.campgrounds <- spatial.analyzed.campgrounds %>% add.country.regions.facilities()

#CREATE GROUPS FROM SPATIAL PROXIMITY ANALYSIS
#NP Grouping
spatial.analyzed.campgrounds[Win10kmNP != -1, NPGroup := "5-10km of NP"]
spatial.analyzed.campgrounds[Win5kmNP != -1, NPGroup := "0-5km of NP"]
spatial.analyzed.campgrounds[WinNP != -1, NPGroup := "In NP"]
spatial.analyzed.campgrounds[is.na(NPGroup), NPGroup := "10+km of NP"]

#NM Grouping
spatial.analyzed.campgrounds[Win10kmNM != -1, NMGroup := "5-10km of NM"]
spatial.analyzed.campgrounds[Win5kmNM != -1, NMGroup := "0-5km of NM"]
spatial.analyzed.campgrounds[WinNM != -1, NMGroup := "In NM"]
spatial.analyzed.campgrounds[is.na(NMGroup), NMGroup := "10+km of NM"]

#NM-NP Grouping Combined
spatial.analyzed.campgrounds[NMGroup == "5-10km of NM" | NPGroup == "5-10km of NP", NM_NPGroup := "5-10km of NM or NP"]
spatial.analyzed.campgrounds[NMGroup == "0-5km of NM" | NPGroup == "0-5km of NP", NM_NPGroup := "0-5km of NM or NP"]
spatial.analyzed.campgrounds[NMGroup == "In NM" | NPGroup == "In NP", NM_NPGroup := "In NM or NP"]
spatial.analyzed.campgrounds[is.na(NM_NPGroup), NM_NPGroup := "10+km of NM or NP"]

spatial.analyzed.campgrounds <- spatial.analyzed.campgrounds %>% select(-(Win5kmNP:Win5kmNM), -WinNP, -WinNM)

#AllUrbanGrouping
spatial.analyzed.campgrounds[Win30kmAllUrban != -1, AllUrbanGroup := "15-30km of Urban"]
spatial.analyzed.campgrounds[Win15kmAllUrban != -1, AllUrbanGroup := "0-15km of Urban"]
spatial.analyzed.campgrounds[is.na(AllUrbanGroup), AllUrbanGroup := "30+km of Urban"]

#BigUrbanGrouping
spatial.analyzed.campgrounds[Win100kmBigUrban != -1, BigUrbanGroup := "50-100km of Big Urban"]
spatial.analyzed.campgrounds[Win50kmBigUrban != -1, BigUrbanGroup := "0-50km of Big Urban"]
spatial.analyzed.campgrounds[is.na(BigUrbanGroup), BigUrbanGroup := "100+km of Big Urban"]

spatial.analyzed.campgrounds <- spatial.analyzed.campgrounds %>% select(-(Win15kmAllUrban:Win100kmBigUrban))

#PADUS 1-2 Grouping
spatial.analyzed.campgrounds[Win10kmPAD12 != -1, PADUSGroup := "5-10km of PADUS 1-2"]
spatial.analyzed.campgrounds[Win5kmPAD12 != -1, PADUSGroup := "0-5km of PADUS 1-2"]
spatial.analyzed.campgrounds[PADUS_NEAR_m == 0, PADUSGroup := "In PADUS 1-2"]
spatial.analyzed.campgrounds[is.na(PADUSGroup), PADUSGroup := "10+km of PADUS 1-2"]

spatial.analyzed.campgrounds <- spatial.analyzed.campgrounds %>% select(-Win10kmPAD12, -Win5kmPAD12)

#ADD SPATIAL INFORMATION INTO FULL EXPANDED LIST
spatial.analyzed.campgrounds <- spatial.analyzed.campgrounds %>% select(-FacilityLatitude, -FacilityLongitude)
camps.exist.expand.spatial.data <- camps.exist.expand %>% left_join(spatial.analyzed.campgrounds, by = "FacilityID")

#View(camps.exist.expand.spatial.data[1:100,])

#Add "missing" to facilities w/ bad lat/long, either 0's or USACE messed up data
camps.exist.expand.spatial.data[is.na(FacilityState), LocationGood := "Bad Lat/Long"]
camps.exist.expand.spatial.data[is.na(LocationGood), LocationGood := "Good Lat/Long"]

#View(camps.exist.expand.spatial.data[LocationGood == "Bad Lat/Long",])
fwrite(camps.exist.expand.spatial.data, here::here(derived_dir, "CampgroundNightsExisting_WithSpatialAnalysis.gz"))
camps.exist.expand.spatial.data <- fread(here::here(derived_dir, "CampgroundNightsExisting_WithSpatialAnalysis.gz"))

# ADD IN just IN/OUT PADUS / NPs / etc
camps.exist.expand.spatial.data[, OverallPADUSGroup := "Outside PADUS 1-2"]
camps.exist.expand.spatial.data[PADUSGroup == "In PADUS 1-2", OverallPADUSGroup := "In PADUS 1-2"]
camps.exist.expand.spatial.data[, OverallNPGroup := "Outside NP"]
camps.exist.expand.spatial.data[NPGroup == "In NP", OverallNPGroup := "In NP"]
#View(camps.exist.expand.spatial.data[1:10,])
fwrite(camps.exist.expand.spatial.data, here::here(derived_dir,"CampgroundNightsExisting_WithSpatialAnalysis.gz"))

#OCCUPANCY ANALYSIS----

#PART 1: Join!----
camps.exist.expand.spatial.data <- fread(here::here(derived_dir,"CampgroundNightsExisting_WithSpatialAnalysis.gz"))


full.expanded.fixed <- fread(here::here(derived_dir, "FullExpandedFixed.gz"))
reservable.locations <- unique(full.expanded.fixed$FacilityID) #All facilities ever reserved over the study time frame. Assume any others are un-reservable
#Remove facilities without a match from camps.exist.spatial.data
camps.exist.expand.spatial.data.reservable <- camps.exist.expand.spatial.data %>% filter(FacilityID %in% reservable.locations)
fwrite(camps.exist.expand.spatial.data.reservable, here::here(derived_dir, "USE_ReservableCampgroundNightsExisting_WithSpatialAnalysis.gz"))
rm(camps.exist.expand.spatial.data)

#Separate dataset for different joins
camps.exist.expand.spatial.data.reservable <-fread(here::here(derived_dir, "USE_ReservableCampgroundNightsExisting_WithSpatialAnalysis.gz"))
#View(camps.exist.expand.spatial.data.reservable[1:10,])
camps.exist.expand.reservable <- camps.exist.expand.spatial.data.reservable %>% select(-(PADUS_NEAR_m:KernDens_40km), -(NPGroup:PADUSGroup), -OverallPADUSGroup, -OverallNPGroup)
camps.exist.unique.spatial.clean <- camps.exist.expand.spatial.data.reservable %>% select(FacilityID, PADUS_NEAR_m:KernDens_40km, NPGroup:PADUSGroup, OverallPADUSGroup, OverallNPGroup)
camps.exist.unique.spatial.clean <- camps.exist.unique.spatial.clean %>% distinct(FacilityID, .keep_all = TRUE)
rm(camps.exist.expand.spatial.data.reservable)
fwrite(camps.exist.expand.reservable, here::here(derived_dir, "USE_ReservableCampgroundNightsExisting.gz"))
fwrite(camps.exist.unique.spatial.clean, here::here(derived_dir, "USE_UniqueFacilityIDs_WithSpatialAnalysis.gz"))

expanded.fixed.grouped <- fread(here::here(derived_dir, "ExpandedFixed_GroupedByFacilityIDAndNightDates.gz"))
camps.exist.expand.reservable <- fread(here::here(derived_dir, "USE_ReservableCampgroundNightsExisting.gz"))

# View(expanded.fixed.grouped[1:10,])
# View(camps.exist.expand.reservable[1:10,])
# View(camps.exist.unique.spatial.clean[1:10,])

expanded.fixed.grouped <- expanded.fixed.grouped %>% select(-Weekday, -ResMonthWritten, -FacilityLongitude, -FacilityLatitude, -Agency)

expanded.fixed.grouped.descriptive <- expanded.fixed.grouped %>% select(FacilityID, FacilityState, RegionDescription, ParentLocation,
                                                                        Park, FacilityZIP, FacilityRegion)
expanded.fixed.grouped.descriptive <- expanded.fixed.grouped.descriptive %>% distinct(FacilityID, .keep_all = TRUE)
expanded.fixed.grouped.stat <- expanded.fixed.grouped %>% select(NightDates, FacilityID, NightsReserved)
rm(expanded.fixed.grouped)

#RUN THE JOIN
occupancy.rates <- camps.exist.expand.reservable %>% left_join(expanded.fixed.grouped.descriptive, by = c("FacilityID"))
occupancy.rates <- occupancy.rates |>
  mutate(NightDates = as.Date(NightDates)) |>
  left_join(expanded.fixed.grouped.stat |>
              mutate(NightDates = as.Date(NightDates)), by = c("FacilityID", "NightDates"))
rm(expanded.fixed.grouped.descriptive, expanded.fixed.grouped.stat, camps.exist.expand.reservable)

#Fix occupancy data
#View(occupancy.rates[1:10,])
occupancy.rates[is.na(NightsReserved), NightsReserved := 0]
occupancy.rates <- occupancy.rates %>% mutate(ReservableOccupancyPercent = (NightsReserved/number.campsites.existing))
occupancy.rates <- occupancy.rates %>% mutate(ROPercentOver100 = case_when(ReservableOccupancyPercent > 1 ~ "Yes",
                                                                                     ReservableOccupancyPercent <= 1 ~ "No",
                                                                                     TRUE ~ "Missing"))
occupancy.rates <- occupancy.rates %>% mutate(ReservableOccupancyPercentFixed = case_when(ReservableOccupancyPercent > 1 ~ 1,
                                                                                                    ReservableOccupancyPercent <= 1 ~ ReservableOccupancyPercent,
                                                                                                    TRUE ~ 0))
#add year section
add.year.section.fast <- function(dat) {
  dat[,YearSection := "Off-Season"]
  dat[ResMonthWritten == "June" | ResMonthWritten == "July" | ResMonthWritten == "August", YearSection := "Summer"]
  return(dat)
}
occupancy.rates <- occupancy.rates %>% add.year.section.fast()

#View(occupancy.rates[1:10,])
fwrite(occupancy.rates, here::here(derived_dir, "OccupancyRatesInitial.gz"))


#Examine joined data, find errors and make small fixes
#View(occupancy.rates[FacilityState.x != FacilityState.y,] %>% distinct(FacilityID, .keep_all = TRUE))
states.not.same <- occupancy.rates[FacilityState.x != FacilityState.y,] %>% distinct(FacilityID, .keep_all = TRUE)
states.not.same <- states.not.same$FacilityID
#View(occupancy.rates[LocationGood == "Bad Lat/Long",] %>% distinct(FacilityID, .keep_all = TRUE))
#View(occupancy.rates[FacilityName != Park,c("FacilityID", "FacilityName", "Park")] %>% distinct(FacilityID, .keep_all = TRUE))

occupancy.rates[,FacilityState := FacilityState.y]
occupancy.rates[,FacilityRegion := FacilityRegion.y]
geographic.states.better <- c(232186,264411,273348,273360,
                              273799,273820,10050273,274314,274410) #By hand checked to see which FacilityIDs are better based on their lat/long or their res data
occupancy.rates[FacilityID %in% geographic.states.better, FacilityState := FacilityState.x]
occupancy.rates[FacilityID %in% geographic.states.better, FacilityRegion := FacilityRegion.x]
occupancy.rates[FacilityID %in% setdiff(states.not.same,geographic.states.better), LocationGood := "Bad Lat/Long"]

occupancy.rates <- occupancy.rates %>% select(-Park, -OrgID, -FacilityState.x, -FacilityState.y,
                                              -FacilityRegion.x, -FacilityRegion.y)
#View(occupancy.rates[1:10,])
fwrite(occupancy.rates, here::here(derived_dir, "OccupancyRatesInitial.gz"))

#Remove 2021 numbers
occupancy.rates <- fread(here::here(derived_dir, "OccupancyRatesInitial.gz"))
View(occupancy.rates[1:10,])
occupancy.rates <- occupancy.rates[ResYear != 2021,]

#Fix nights reserved raw number to account for over-100% capacity
occupancy.rates[,NightsReservedFixed := NightsReserved]
occupancy.rates[ROPercentOver100 == "Yes", NightsReservedFixed := number.campsites.existing]
fwrite(occupancy.rates, here::here(derived_dir, "OccupancyRatesInitial.gz"))


100*(length(occupancy.rates[ROPercentOver100 == "Yes",]$FacilityID) / length(occupancy.rates$FacilityID)) #3.7 % of facility-days had over 100% occupancy


#Re-group occupancy rates data by JUST facilityID to join with spatial data
occupancy.rates.spatial <- occupancy.rates %>% group_by(FacilityID) %>% summarise(AvgOccupancy = mean(ReservableOccupancyPercentFixed))
camps.exist.unique.spatial.clean <- fread(here::here(derived_dir, "USE_UniqueFacilityIDs_WithSpatialAnalysis.gz"))
occupancy.rates.spatial <- occupancy.rates.spatial %>% left_join(camps.exist.unique.spatial.clean, by = "FacilityID")
fwrite(occupancy.rates.spatial, here::here(derived_dir, "OccupancyRatesSpatialInitial.gz"))




#Examine 0% instances
look.for.zeros <- occupancy.rates %>% group_by(FacilityID, ResYear) %>% summarise(AvgOccupancy = mean(ReservableOccupancyPercentFixed), AvgCampsites = mean(number.campsites.existing))
#View(look.for.zeros %>% filter(AvgOccupancy == 0 & AvgCampsites > 100)) #There are 12 instances of campgrounds with a year of 0% occupancy but over 100 campsites.

#Write full dataset
occupancy.rates <- fread(here::here(derived_dir, "OccupancyRatesInitial.gz"))
occupancy.rates.full.plus.spatial <- occupancy.rates %>% left_join(camps.exist.unique.spatial.clean, by = "FacilityID")
#View(occupancy.rates.full.plus.spatial[1:10,])
fwrite(occupancy.rates.full.plus.spatial, here::here(derived_dir, "FULLOccupancyRatesPlusSpatial.gz"))



#DATASETS AT THIS POINT:
#OccupancyRatesInitial.gz is all occupancy rates grouped by FacilityID and Date, with Occupancy
#OccupancyRatesSpatialInitial.gz is occupancy rates grouped by only FacilityID, with Average Occupancy and Spatial analysis data
#FULLOccupancyRatesPlusSpatial.gz is all occupancy rates grouped by FacilityID and Date, with Occupancy and all spatial analysis data

#PART 2: Subset for generalized percentages----
occupancy.rates.full.plus.spatial <- fread(here::here(derived_dir, "FULLOccupancyRatesPlusSpatial.gz"))
#View((occupancy.rates.full.plus.spatial[1:10,]))
NoAKHI.occupancy.rates.full.plus.spatial <- occupancy.rates.full.plus.spatial %>%
  filter(FacilityState != "Alaska") |>
  filter(FacilityState != "Hawaii")


#INTEGRATE THE TWO DATASETS, REPLACE 2019/20 ESTIMATES WITH ACTUAL???
SPATIAL_NoAKHI_utilization <- fread(here::here(raw_dir, "SPATIAL_NoAKHI_utilization.csv"))

# View((NoAKHI.occupancy.rates.full.plus.spatial[1:10,]))
# View((SPATIAL_NoAKHI_utilization[1:10,]))

NoAKHI.occupancy.rates.full.plus.spatial <- NoAKHI.occupancy.rates.full.plus.spatial %>% select(-Weekday, -NightsReserved, -ReservableOccupancyPercent,
                                                                                                 -ROPercentOver100, -ReservableOccupancyPercentFixed,
                                                                                                -(PADUS_NEAR_m:KernDens_40km), -(NMGroup:NM_NPGroup), -PADUSGroup)
SPATIAL_NoAKHI_utilization <- SPATIAL_NoAKHI_utilization %>% select(-OccupancyOver100, -TotOccupancy, -TotOccupancyFixed,
                                                                    -Available, -FCFS, -Closed, -Reserved, -(PADUS_NEAR_m:KernDens_40km), -(NMGroup:NM_NPGroup), -PADUSGroup)
setnames(SPATIAL_NoAKHI_utilization, old = "availability_date", new = "NightDates")
setnames(SPATIAL_NoAKHI_utilization, old = "AvailAndFCFS", new = "number.campsites.existing")
setnames(SPATIAL_NoAKHI_utilization, old = "RealYear", new = "ResYear")

setcolorder(SPATIAL_NoAKHI_utilization, c("FacilityID", "number.campsites.existing", "NightDates", "FacilityName", "FacilityLongitude", "FacilityLatitude",
                                          "OrgName", "OrgAbbrevName", "DayType", "ResMonthWritten", "ResYear", "LocationGood",
                                          "RegionDescription", "ParentLocation", "FacilityZIP", "YearSection",
                                          "FacilityState", "FacilityRegion", "NightsReservedFixed", "NPGroup", "AllUrbanGroup", "BigUrbanGroup", "OverallPADUSGroup", "OverallNPGroup"))
SPATIAL_NoAKHI_utilization <- SPATIAL_NoAKHI_utilization %>% mutate(NightDates = as.Date(NightDates))
NoAKHI.occupancy.rates.full.plus.spatial <- NoAKHI.occupancy.rates.full.plus.spatial %>% mutate(NightDates = as.Date(NightDates))


NoAKHI.occupancy.rates.full.plus.spatial.minus.19.20 <- NoAKHI.occupancy.rates.full.plus.spatial %>% filter(ResYear == 2014 | ResYear == 2015 |
                                                                                                              ResYear == 2016 | ResYear == 2017 |
                                                                                                              ResYear == 2018)
NoAKHI.occupancy.rates.full.plus.spatial.19.20 <- NoAKHI.occupancy.rates.full.plus.spatial %>% filter(ResYear == 2019 | ResYear == 2020)

# length(unique(NoAKHI.occupancy.rates.full.plus.spatial.19.20$FacilityID))
# length(unique(SPATIAL_NoAKHI_utilization$FacilityID))
# length(unique(NoAKHI.occupancy.rates.full.plus.spatial.19.20$NightDates))
# length(unique(SPATIAL_NoAKHI_utilization$NightDates))
# min(unique(NoAKHI.occupancy.rates.full.plus.spatial.19.20$NightDates))
# min(unique(SPATIAL_NoAKHI_utilization$NightDates))
# max(unique(NoAKHI.occupancy.rates.full.plus.spatial.19.20$NightDates))
# max(unique(SPATIAL_NoAKHI_utilization$NightDates))

#Rbind
CombinedDatasets.occupancy.rates <- NoAKHI.occupancy.rates.full.plus.spatial.minus.19.20 %>% rbind(SPATIAL_NoAKHI_utilization)
#View(CombinedDatasets.occupancy.rates[1:10,])

NoNP.2019.2020.dataset.occupancy.rates <- SPATIAL_NoAKHI_utilization %>% filter(NPGroup == "10+km of NP")

dir_toplines <- here::here("data/toplines")
dir_ensure(dir_toplines)

#MakeToplinesFunction
make.toplines <- function(dat, dat.summer, dat.summer.weekend, dat.summer.weekday, dat.off.season, name, ...) {
  full.year <- dat %>% 
    group_by(...) %>% 
    summarise(NightsReservedFixed = sum(NightsReservedFixed), number.campsites.existing = sum(number.campsites.existing)) %>%
    mutate(PercentCampsitesFilled = (NightsReservedFixed/number.campsites.existing)) %>%
    mutate(Subset = "Full Year")
  summer <- dat.summer %>% 
    group_by(...) %>% 
    summarise(NightsReservedFixed = sum(NightsReservedFixed), number.campsites.existing = sum(number.campsites.existing)) %>%
    mutate(PercentCampsitesFilled = (NightsReservedFixed/number.campsites.existing)) %>%
    mutate(Subset = "Summer")
  summer.weekend <- dat.summer.weekend %>% 
    group_by(...) %>% 
    summarise(NightsReservedFixed = sum(NightsReservedFixed), number.campsites.existing = sum(number.campsites.existing)) %>%
    mutate(PercentCampsitesFilled = (NightsReservedFixed/number.campsites.existing)) %>%
    mutate(Subset = "Summer Weekends")
  summer.weekday <- dat.summer.weekday %>% 
    group_by(...) %>% 
    summarise(NightsReservedFixed = sum(NightsReservedFixed), number.campsites.existing = sum(number.campsites.existing)) %>%
    mutate(PercentCampsitesFilled = (NightsReservedFixed/number.campsites.existing)) %>%
    mutate(Subset = "Summer Weekdays")
  off.season <- dat.off.season %>% 
    group_by(...) %>% 
    summarise(NightsReservedFixed = sum(NightsReservedFixed), number.campsites.existing = sum(number.campsites.existing)) %>%
    mutate(PercentCampsitesFilled = (NightsReservedFixed/number.campsites.existing)) %>%
    mutate(Subset = "Off-Season")
  all.subs <- rbind(full.year, summer, summer.weekend, summer.weekday, off.season)
  fwrite(all.subs, here::here(dir_toplines, paste(name, ".csv", sep = "")))
}

get.name <- function(dat) {
  return(as.character(substitute(dat)))
}

#Create toplines subsets and export
run.toplines.subsets <- function(data.set, data.set.name) {
  summer.occupancy.rates <- data.set %>% filter(YearSection == "Summer")
  #fwrite(summer.occupancy.rates, paste(get.name(summer.occupancy.rates), ".csv", sep = ""))
  summer.weekend.occupancy.rates <- summer.occupancy.rates %>% filter(DayType == "Weekend")
  #fwrite(summer.weekend.occupancy.rates, paste(get.name(summer.weekend.occupancy.rates), ".csv", sep = ""))
  summer.weekday.occupancy.rates <- summer.occupancy.rates %>% filter(DayType == "Weekday")
  #fwrite(summer.weekday.occupancy.rates, paste(get.name(summer.weekday.occupancy.rates), ".csv", sep = ""))
  offseason.occupancy.rates <- data.set %>% filter(YearSection == "Off-Season")
  #View(summer.weekend.occupancy.rates[1:10,])
  
  
  
  make.toplines(data.set, summer.occupancy.rates, 
                summer.weekend.occupancy.rates, summer.weekday.occupancy.rates, offseason.occupancy.rates, 
                paste(data.set.name, ".just.year", sep=""), ResYear)
  make.toplines(data.set, summer.occupancy.rates, 
                summer.weekend.occupancy.rates, summer.weekday.occupancy.rates, offseason.occupancy.rates, 
                paste(data.set.name, ".region.year", sep=""), FacilityRegion, ResYear)
  make.toplines(data.set, summer.occupancy.rates, 
                summer.weekend.occupancy.rates, summer.weekday.occupancy.rates, offseason.occupancy.rates, 
                paste(data.set.name, ".agency.year", sep=""), ResYear, OrgName)
  make.toplines(data.set, summer.occupancy.rates, 
                summer.weekend.occupancy.rates, summer.weekday.occupancy.rates, offseason.occupancy.rates, 
                paste(data.set.name, ".state.region.year", sep=""), FacilityState, ResYear, FacilityRegion)
  make.toplines(data.set, summer.occupancy.rates, 
                summer.weekend.occupancy.rates, summer.weekday.occupancy.rates, offseason.occupancy.rates, 
                paste(data.set.name, ".agency.region.year", sep=""), OrgName, ResYear, FacilityRegion)
  make.toplines(data.set, summer.occupancy.rates, 
                summer.weekend.occupancy.rates, summer.weekday.occupancy.rates, offseason.occupancy.rates, 
                paste(data.set.name, ".state.year", sep=""), FacilityState, ResYear)
  make.toplines(data.set, summer.occupancy.rates, 
                summer.weekend.occupancy.rates, summer.weekday.occupancy.rates, offseason.occupancy.rates, 
                paste(data.set.name, ".PADUS.year", sep=""), OverallPADUSGroup, ResYear)
  make.toplines(data.set, summer.occupancy.rates, 
                summer.weekend.occupancy.rates, summer.weekday.occupancy.rates, offseason.occupancy.rates, 
                paste(data.set.name, ".BigUrban.year", sep=""), BigUrbanGroup, ResYear)
  make.toplines(data.set, summer.occupancy.rates, 
                summer.weekend.occupancy.rates, summer.weekday.occupancy.rates, offseason.occupancy.rates, 
                paste(data.set.name, ".AllUrban.year", sep=""), AllUrbanGroup, ResYear)
  make.toplines(data.set, summer.occupancy.rates, 
                summer.weekend.occupancy.rates, summer.weekday.occupancy.rates, offseason.occupancy.rates, 
                paste(data.set.name, ".NP.year", sep=""), OverallNPGroup, ResYear)
  make.toplines(data.set, summer.occupancy.rates, 
                summer.weekend.occupancy.rates, summer.weekday.occupancy.rates, offseason.occupancy.rates, 
                paste(data.set.name, ".NP.allgroups", sep=""), NPGroup)
}

#run.toplines.subsets(occupancy.rates.full.plus.spatial, "FullDataset")
run.toplines.subsets(NoAKHI.occupancy.rates.full.plus.spatial, "NoAKHI_Dataset")
run.toplines.subsets(CombinedDatasets.occupancy.rates, "NoAKHI_COMBINED_Dataset")
run.toplines.subsets(NoNP.2019.2020.dataset.occupancy.rates, "NoAKHI_NoNP_2019_20_Dataset")
run.toplines.subsets(SPATIAL_NoAKHI_utilization, "NoAKHI_2019_20_Dataset")


