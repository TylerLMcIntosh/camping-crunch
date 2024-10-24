# Camping Crunch: Data Analysis on data from Reservations.gov
_Tyler L. McIntosh_

Topline stats are output in data/toplines.

Full datasets are:
- OccupancyRatesInitial.gz: all occupancy rates grouped by FacilityID and Date, with Occupancy
- OccupancyRatesSpatialInitial.gz: occupancy rates grouped by only FacilityID, with Average Occupancy and Spatial analysis data
- FULLOccupancyRatesPlusSpatial.gz: all occupancy rates grouped by FacilityID and Date, with Occupancy and all spatial analysis data


In addition to the code provided here, the following files / directories are required in data/raw:

- raw/
    - Facilities_Spatial_Analysis_2.csv
    - Recreation.govReservationData/
      - reservations2011.zip
      - reservations2012.zip
      - reservations2013.zip
      - reservations2014.zip
      - reservations2015.zip
      - reservations2016.zip
      - reservations2017.zip
      - reservations2018.zip
      - reservations2019.zip
      - reservations2020.zip
    - RIDBFullExport_V1_CSV/
      - Activities_API_v1.csv
      - CampsiteAttributes_API_v1.csv
      - Campsites_API_v1.csv
      - EntityActivities_API_v1.csv
      - Events_API_v1.csv
      - Facilities_API_v1.csv
      - FacilityAddresses_API_v1.csv
      - Links_API_v1.csv
      - Media_API_v1.csv
      - MemberTours_API_v1.csv
      - Organizations_API_v1.csv
      - OrgEntities_API_v1.csv
      - PermitEntranceAttributes_API_v1.csv
      - PermitEntrances_API_v1.csv
      - PermitEntranceZones_API_v1.csv
      - PermittedEquipment_API_v1.csv
      - RecAreaAddresses_API_v1.csv
      - RecAreaFacilities_API_v1.csv
      - RecAreas_API_v1.csv
      - TourAttributes_API_v1.csv
      - Tours_API_v1.csv
    - SPATIAL_NoAKHI_utilization.csv