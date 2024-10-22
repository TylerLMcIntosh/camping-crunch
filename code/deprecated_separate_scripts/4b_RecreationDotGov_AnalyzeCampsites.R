
# PUBLIC LANDS VISITATION: Analyze added campsites dataset
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
setwd("D:/Storage/RecreationAnalysis2021/")

campsites <- fread("RIDBFullExport_V1_CSV/Campsites_API_v1.csv")
#Make, explore campsite datasets ----
#Get unique types of campsites included in database

#Fixdate
campsites <- campsites[,CreatedDateFix:= as.Date(CreatedDate)]

#Remove duplicates
campsites <- distinct(campsites)
#make dataframe of when added
campsites.existing <- campsites %>% group_by(CreatedDateFix) %>% summarize(AddedCampsites = n())
campsites.existing <- campsites.existing %>% mutate(ExistingCampsites = cumsum(AddedCampsites))

#Expand dataset for full range of dates----
campsites.existing.full <- campsites.existing
fill <- data.table(CreatedDateFix = character(), AddedCampsites = integer(), ExistingCampsites = integer())

for (i in c(1:280)) {
  between.dats <- tail(head((seq(campsites.existing$CreatedDateFix[i], campsites.existing$CreatedDateFix[i+1], by="days")), -1), -1)
  fill <- data.table(between.dats, 0, campsites.existing$ExistingCampsites[i])
  campsites.existing.full <- rbind(campsites.existing.full, fill, use.names=FALSE)
}

campsites.existing.full <- na.omit(campsites.existing.full[order(CreatedDateFix),])
#write.csv(campsites.existing.full, "FullCalendarListCampsitesExisting.csv")

#Make, explore campgrounds

#Number of campsites in each facility
campgrounds <- campsites %>% group_by(FacilityID) %>% summarize(number.campsites.total = n())




#Figure out if campgrounds are usually added all at once, or in pieces)
campgrounds.created.date <- campsites %>% group_by(FacilityID, CreatedDateFix) %>% summarize(number.campsites = n())
campgrounds.created.date <- left_join(campgrounds.created.date, campgrounds, by = "FacilityID")
campgrounds.created.date <- campgrounds.created.date %>% mutate(percentage = 100*(number.campsites/number.campsites.total))
ggplot(campgrounds.created.date, aes(percentage)) + geom_histogram(bins = 100)
print(paste((sum(campgrounds.created.date$percentage > 95, na.rm = TRUE) / sum(campgrounds.created.date$percentage >= 50, na.rm = TRUE)*100), "% of the campgrounds had 95% of their campsites immediately upon inclusion"))




#Expand existing campgrounds dataset----

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
write.csv(campgrounds.existing.full.expand, "CampgroundsExistingFullExpand.csv")








#Check to see if sites are being reserved PRIOR to being added to the dataset - if they are (beyond the start-of-dataset exception, then may just have to use end-#, which would honestly be easier)
full.reservations <- fread("FULL_NormalDataset.csv")
mini <- full.reservations %>% sample_n(100, FALSE)
test <- full.reservations %>% filter(FacilityID == 251839)

res.pre.campsites.added <- full.reservations %>% filter(OrderDate < ymd("2014-05-02"))
res.pre.campsites.added <- left_join(campgrounds.created.date, res.pre.campsites.added[!duplicated(FacilityID)], by = "FacilityID") %>% select(-V1, -CustomerZIP, -CustomerState, -CustomerCountry, -DataYear)


res.2016 <- fread("Recreation.govReservationData/reservations2016/2016.csv")
mini2016 <- res.2016 %>% sample_n(100, FALSE)

#write.csv(campgrounds, "Campgrounds2021.csv")
#ggplot(campgrounds, aes(number.campsites)) + geom_histogram(bins = 100)

#Join facility data from reservation data to campsites
Full.Res.Grouped <- fread("Data viz filters/8.5.21.6pm/NormalGrouped.csv")

campsites.added.info <- left_join(campsites, Full.Res.Grouped[!duplicated(FacilityID)], by = "FacilityID") %>% select(-V1, -CustomerZIP, -CustomerState, -CustomerCountry, -DataYear, -RStartYr, -CustomerRegion, -Reservations, -NightsReserved, -S.TotalBeforeTax, -S.TotalPaid) %>% filter(TypeOfUse == "Overnight")
#write.csv(campsites.added.info, "CampsitesAddedInfo.csv")
