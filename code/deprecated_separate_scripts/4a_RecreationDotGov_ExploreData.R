
# PUBLIC LANDS VISITATION Explore data (FINALLY!)
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
setwd("D:/Storage/RecreationAnalysis2021/")



#FINAL CLEAN
expand.clean <- function(dat) {
  dat <- dat %>% select(-V1, -UseFee, -TranFee, -AttrFee, -Tax, -HistoricalReservationID, -OrgID, -CodeHierarchy, -SiteType, -UseType, -ProductID, -NumberOfPeople, -DaysInAdvance, -EntityType, -StartDate, -EndDate, -OrderDate)
  dat <- dat %>% fix.state.abbreviations.state.char()
  dat <- dat %>% mutate(ResYear = as.character(ResYear))
  return(dat)
}

norm.clean <- function(dat) {
  dat <- dat %>% select(-V1, -UseFee, -TranFee, -AttrFee, -Tax, -HistoricalReservationID, -OrgID, -CodeHierarchy, -SiteType, -UseType, -ProductID, -EntityType)
  dat <- dat %>% fix.state.abbreviations.state.char()
  return(dat)
}

#Function
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

#START OF CODE
camp.2020.expand <- fread("True2020Camping_Expanded.csv")
camp.2020.expand <- camp.2020.expand %>% expand.clean()

#Use 2020 dataset to start
facilitySum <- camp.2020.expand %>% group_by(FacilityID, NightDates, ResYear)%>% summarise(NightsReserved = n())
#sumState <- camp.2020.expand %>% group_by(FacilityState, ResYear) %>% summarise(NightsReserved = n(), AvgDaysAhead = mean(DaysInAdvance))
#sumWeekday <- camp.2020.expand %>% group_by(Weekday, ResYear) %>% summarise(NightsReserved = n(), AvgDaysAhead = mean(DaysInAdvance))
#sumMonth <- camp.2020.expand %>% group_by(ResMonth, ResYear) %>% summarise(NightsReserved = n(), AvgDaysAhead = mean(DaysInAdvance))
#fullSum <- camp.2020.expand %>% group_by(ResYear, ResMonth, Weekday, FacilityState, Agency) %>% summarise(NightsReserved = n(), AvgDaysAhead = mean(DaysInAdvance))
full.expand <- camp.2020.expand
rm(camp.2020.expand)

#Testing
# camp.2020.expand.m <- camp.2020.expand %>% sample_n(size=20000, replace = F) #Make mini dataset for testing if needed
# facilitySum <- camp.2020.expand.m %>% group_by(FacilityID, NightDates, ResYear)%>% summarise(NightsReserved = n())
# full.expand <- camp.2020.expand.m
# rm(camp.2020.expand.m)

#Add in other data
camp.2019.expand <- fread("True2019Camping_Expanded.csv")
# camp.2019.expand.m <- camp.2019.expand %>% sample_n(size=20000, replace = F) #Make mini dataset for testing if needed
# camp.2019.expand.m %>% deal.with.expanded()
# rm(camp.2019.expand.m)
camp.2019.expand %>% deal.with.expanded()
rm(camp.2019.expand)

camp.2018.expand <- fread("True2018Camping_Expanded.csv")
camp.2018.expand %>% deal.with.expanded()
rm(camp.2018.expand)

camp.2017.expand <- fread("True2017Camping_Expanded.csv")
camp.2017.expand %>% deal.with.expanded()
rm(camp.2017.expand)

camp.2016.expand <- fread("True2016Camping_Expanded.csv")
camp.2016.expand %>% deal.with.expanded()
rm(camp.2016.expand)

camp.2015.expand <- fread("True2015Camping_Expanded.csv")
camp.2015.expand %>% deal.with.expanded()
rm(camp.2015.expand)

camp.2014.expand <- fread("True2014Camping_Expanded.csv")
camp.2014.expand %>% deal.with.expanded()
rm(camp.2014.expand)

#Write compiled datasets
# write.csv(sumState, "SumState.csv")
# write.csv(sumMonth, "SumMonth.csv")
# write.csv(sumWeekday, "SumWeekday.csv")
# write.csv(fullSum, "FullSum.csv")
write.csv(facilitySum, "FacilitySum.csv")
write.csv(full.expand, "FULL_ExpandedDataset.csv")
rm(full.expand, facilitySum)




#Do normal datasets
camp.2020 <- fread("Camp2020Clean.csv")
camp.2020 <- camp.2020 %>% norm.clean()
# camp.2020.m <- camp.2020 %>% sample_n(size = 20000, replace = F)
# full.norm <- camp.2020.m
full.norm <- camp.2020
rm(camp.2020)

camp.2019 <- fread("Camp2019Clean.csv")
camp.2019 %>% deal.with.norm()
rm(camp.2019)

camp.2018 <- fread("Camp2018Clean.csv")
camp.2018 %>% deal.with.norm()
rm(camp.2018)

camp.2017 <- fread("Camp2017Clean.csv")
camp.2017 %>% deal.with.norm()
rm(camp.2017)

camp.2016 <- fread("Camp2016Clean.csv")
camp.2016 %>% deal.with.norm()
rm(camp.2016)

camp.2015 <- fread("Camp2015Clean.csv")
camp.2015 %>% deal.with.norm()
rm(camp.2015)

camp.2014 <- fread("Camp2014Clean.csv")
camp.2014 %>% deal.with.norm()
rm(camp.2014)

#Write compiled dataset
write.csv(full.norm, "FULL_NormalDataset.csv")
rm(full.norm)

#Finished
print("Done")


#Fix & add regions, months, states

# add.country.regions <- function(dat) {
#   dat <- dat %>% mutate(FacilityRegion = case_when(FacilityState == "Maine" | FacilityState == "New Hampshire" | FacilityState == "Vermont" | FacilityState == "Massachusetts" | FacilityState == "Rhode Island" | FacilityState == "Connecticut" | FacilityState == "New York" | FacilityState == "New Jersey" | FacilityState == "Pennsylvania" ~ "Northeast", 
#                                                    FacilityState == "Ohio" | FacilityState == "Michigan" | FacilityState == "Indiana" | FacilityState == "Wisconsin" | FacilityState == "Illinois" | FacilityState == "Minnesota" | FacilityState == "Iowa" | FacilityState == "Missouri" | FacilityState == "North Dakota" | FacilityState == "South Dakota" | FacilityState == "Nebraska" | FacilityState == "Kansas" ~ "Midwest",
#                                                    FacilityState == "Delaware" | FacilityState == "Maryland" | FacilityState == "Virginia" | FacilityState == "West Virginia" | FacilityState == "Kentucky" | FacilityState == "North Carolina" | FacilityState == "South Carolina" | FacilityState == "Tennessee" | FacilityState == "Georgia" | FacilityState == "Florida" | FacilityState == "Alabama" | FacilityState == "Mississippi" | FacilityState == "Arkansas" | FacilityState == "Louisiana" | FacilityState == "Texas" | FacilityState == "Oklahoma" | FacilityState == "District of Columbia" ~ "South",
#                                                    FacilityState == "Montana" | FacilityState == "Idaho" | FacilityState == "Wyoming" | FacilityState == "Colorado" | FacilityState == "New Mexico" | FacilityState == "Arizona" | FacilityState == "Utah" | FacilityState == "Nevada" | FacilityState == "California" | FacilityState == "Oregon" | FacilityState == "Washington" | FacilityState == "Alaska" | FacilityState == "Hawaii" ~ "West",
#                                                    TRUE ~ "Missing"))
#   dat <- dat %>% mutate(CustomerRegion = case_when(CustomerState == "Maine" | CustomerState == "New Hampshire" | CustomerState == "Vermont" | CustomerState == "Massachusetts" | CustomerState == "Rhode Island" | CustomerState == "Connecticut" | CustomerState == "New York" | CustomerState == "New Jersey" | CustomerState == "Pennsylvania" ~ "Northeast", 
#                                                    CustomerState == "Ohio" | CustomerState == "Michigan" | CustomerState == "Indiana" | CustomerState == "Wisconsin" | CustomerState == "Illinois" | CustomerState == "Minnesota" | CustomerState == "Iowa" | CustomerState == "Missouri" | CustomerState == "North Dakota" | CustomerState == "South Dakota" | CustomerState == "Nebraska" | CustomerState == "Kansas" ~ "Midwest",
#                                                    CustomerState == "Delaware" | CustomerState == "Maryland" | CustomerState == "Virginia" | CustomerState == "West Virginia" | CustomerState == "Kentucky" | CustomerState == "North Carolina" | CustomerState == "South Carolina" | CustomerState == "Tennessee" | CustomerState == "Georgia" | CustomerState == "Florida" | CustomerState == "Alabama" | CustomerState == "Mississippi" | CustomerState == "Arkansas" | CustomerState == "Louisiana" | CustomerState == "Texas" | CustomerState == "Oklahoma" | CustomerState == "District of Columbia" ~ "South",
#                                                    CustomerState == "Montana" | CustomerState == "Idaho" | CustomerState == "Wyoming" | CustomerState == "Colorado" | CustomerState == "New Mexico" | CustomerState == "Arizona" | CustomerState == "Utah" | CustomerState == "Nevada" | CustomerState == "California" | CustomerState == "Oregon" | CustomerState == "Washington" | CustomerState == "Alaska" | CustomerState == "Hawaii" ~ "West",
#                                                    TRUE ~ "Missing"))
#   return(dat)
# }

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

# fix.months <- function(dat) {
#   dat <- dat %>% mutate(ResMonthWritten = case_when(ResMonth == 1 ~ "January",
#                                                     ResMonth == 2 ~ "February",
#                                                     ResMonth == 3 ~ "March",
#                                                     ResMonth == 4 ~ "April",
#                                                     ResMonth == 5 ~ "May",
#                                                     ResMonth == 6 ~ "June",
#                                                     ResMonth == 7 ~ "July",
#                                                     ResMonth == 8 ~ "August",
#                                                     ResMonth == 9 ~ "September",
#                                                     ResMonth == 10 ~ "October",
#                                                     ResMonth == 11 ~ "November",
#                                                     ResMonth == 12 ~ "December",
#                                                     TRUE ~ "Missing"))
#   return(dat)
# }

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


# #Fix all state abbreviations, set year to character
# fix.state.abbreviations.state.char <- function(dat) {
#   #Fix Facility States
#   abbrfixed <- dat %>% filter(nchar(FacilityState)==2)%>% mutate(FacilityState = abbr2state(FacilityState))
#   good <- dat %>% filter(nchar(FacilityState)>2 | nchar(FacilityState)==0)
#   fixed1 <- rbind(abbrfixed, good)
#   
#   #Fix customer states
#   abbrfixed <- fixed1 %>% filter(nchar(CustomerState)==2)%>% mutate(CustomerState = abbr2state(CustomerState))
#   abbr.extra.space <- fixed1 %>% filter(nchar(CustomerState)==3) %>% mutate(CustomerState = abbr2state(substr(CustomerState, 0, 2)))
#   good <- fixed1 %>% filter(nchar(CustomerState)>3 | nchar(CustomerState)==0)
#   fixed2 <- rbind(abbrfixed, abbr.extra.space, good)
#   
#   return(fixed2)
# }

fix.state.abbreviations.state.char.fast <- function(dat) {
  dat[nchar(FacilityState)==2, FacilityState := abbr2state(FacilityState)]
  dat[nchar(CustomerState)==2, CustomerState := abbr2state(CustomerState)]
  dat[nchar(CustomerState)==3, CustomerState := abbr2state(substr(CustomerState, 0, 2))]
  return(dat)
}

#ADD FIXES TO FULL DATASETS

main.directory <- "D:/Storage/RecreationAnalysis2021/"
desktop <- "C:/Users/tyler/Desktop/"
setwd(main.directory)

#Test on smaller dataset
# small <- fread("FULL_ExpandedDataset.csv", nrows = 1000000)
# small2 <- small
# system.time(sm <- small %>% add.country.regions())
# system.time(sm2 <- small2 %>% add.country.regions.fast())
# system.time(sm <- small %>% fix.months())
# system.time(sm2 <- small2 %>% fix.months.fast())
# system.time(sm <- small %>% fix.state.abbreviations.state.char())
# system.time(sm2 <- small2 %>% fix.state.abbreviations.state.char.fast())
# all.equal(sm,sm2)

full.expanded <- fread("FULL_ExpandedDataset.csv")
full.expanded <- full.expanded %>% fix.months.fast()
full.expanded <- full.expanded %>% fix.state.abbreviations.state.char.fast()
full.expanded <- full.expanded %>% add.country.regions.fast()
gc()
fwrite(full.expanded, "FULL_ExpandedDataset2.csv")
print("Completed data export")
rm(full.expanded)
gc()

full.normal <- fread("FULL_NormalDataset.csv")
full.normal <- full.normal %>% fix.state.abbreviations.state.char.fast()
full.normal <- full.normal %>% mutate(RStartYr = year(StartDate))
full.normal <- full.normal %>% add.country.regions.fast()
full.normal <- full.normal %>% mutate(InOut = case_when(CustomerState == "" ~ "Can't Determine",
                                                        FacilityState == "" ~ "Can't Determine",
                                                        CustomerState == FacilityState ~ "In-State",
                                                        CustomerState != FacilityState ~ "Out-of-State",
                                                        TRUE ~ "Missing"))
fwrite(full.normal, "FULL_NormalDataset2.csv")
rm(full.normal)
gc()

