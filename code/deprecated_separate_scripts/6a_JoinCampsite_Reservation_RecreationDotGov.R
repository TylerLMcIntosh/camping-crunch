




#NEED TO ADD IN: FacilityState (WILL NEED GIS???), FacilityRegion


mini <- full.expanded.fixed %>% sample_n(100)
mini.camps <- camps.exist.expand %>% sample_n(100)
mini.working <- reservable.occupancy[1:100,]


#THERE ARE ABOUT 600 FACILITIES ACROSS THE COUNTRY THAT HAVE HAD ZERO RESERVATIONS MADE OVER THE PAST 7 YEARS... what to do about these?????
test1 <- length(camps.exist.expand[!duplicated(FacilityID)]$FacilityID)
test2 <- length(full.expanded.campground.nights.more.info[!duplicated(FacilityID)]$FacilityID)

test <- camps.exist.expand %>% left_join(full.expanded.campground.nights.more.info[!duplicated(FacilityID)], by = "FacilityID") %>% 
  select(-V1, -CreatedDateFix, -number.campsites, -number.campsites.total, -percentage, -FirstDate, -LastDate, -WeekDay, -ResMonthWritten, -NightsReserved)

reservable.occupancy <- camps.exist.expand %>% left_join(full.expanded.campground.nights.more.info, by = c("FacilityID", "NightDates")) %>% 
  select(-V1, -CreatedDateFix, -number.campsites, -number.campsites.total, -percentage, -FirstDate, -LastDate)
reservable.occupancy <- reservable.occupancy %>% mutate(ReservableOccupancyPercent = 100*(NightsReserved/number.campsites.existing))
reservable.occupancy <- reservable.occupancy %>% mutate(ROPercentOver100 = case_when(ReservableOccupancyPercent > 100 ~ "Yes",
                                                                                     ReservableOccupancyPercent <= 100 ~ "No",
                                                                                     TRUE ~ "Missing"))
reservable.occupancy <- reservable.occupancy %>% mutate(ReservableOccupancyPercentFixed = case_when(ReservableOccupancyPercent > 100 ~ 100.00,
                                                                                                    ReservableOccupancyPercent <= 100 ~ ReservableOccupancyPercent,
                                                                                                    TRUE ~ 0))
reservable.occupancy <- reservable.occupancy %>% mutate(DayType = case_when(Weekday == "Monday" ~ "Weekday",
                                                                            Weekday == "Tuesday" ~ "Weekday",
                                                                            Weekday == "Wednesday" ~ "Weekday",
                                                                            Weekday == "Thursday" ~ "Weekday",
                                                                            Weekday == "Friday" ~ "Weekday",
                                                                            Weekday == "Saturday" ~ "Weekend",
                                                                            Weekday == "Sunday" ~ "Weekend",
                                                                            TRUE ~ "Missing"))
gc()
fwrite(reservable.occupancy, "FULL_OccupancyRates.csv")
gc()
