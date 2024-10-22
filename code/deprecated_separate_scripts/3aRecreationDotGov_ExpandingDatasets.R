# PUBLIC LANDS VISITATION Expand Datasets
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

#Read in cleaned data
camp.2020 <- fread("Camp2020Clean.csv")
camp.2019 <- fread("Camp2019Clean.csv")
camp.2018 <- fread("Camp2018Clean.csv")
camp.2017 <- fread("Camp2017Clean.csv")
camp.2016 <- fread("Camp2016Clean.csv")
camp.2015 <- fread("Camp2015Clean.csv")
camp.2014 <- fread("Camp2014Clean.csv")

#Remove columns created by read-in
camp.2020 <- camp.2020 %>% select(-V1)
camp.2019 <- camp.2019 %>% select(-V1)
camp.2018 <- camp.2018 %>% select(-V1)
camp.2017 <- camp.2017 %>% select(-V1)
camp.2016 <- camp.2016 %>% select(-V1)
camp.2015 <- camp.2015 %>% select(-V1)
camp.2014 <- camp.2014 %>% select(-V1)


#Expand dataset
expand.data <- function(dats){
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
beep()

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
  write.csv(true.camp, paste("True", yr, "Camping_Expanded.csv", sep=""))
  print(paste("Write True CSV Done", yr))
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
