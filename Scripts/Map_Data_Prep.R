##Grab Airport Codes for dest, origin, and layover airports
## Load Packages
pacman::p_load(tidyverse,skimr,stargazer,pander,kableExtra, ggplot2, haven, 
               broom, sandwich, lmtest, memisc, xtable, AER, plm)
## Read in the data
OD <- read_csv("Data/Origin_Destination_07-11.csv")

## Count Stops
OD07_11_flight_path <- OD %>% 
  dplyr::select(Year,Quarter,AirportGroup,OpCarrier) %>% 
  group_by(Year,Quarter,OpCarrier) %>% 
  distinct(AirportGroup) %>% 
  ungroup() %>% 
  mutate(numstops = count.fields(textConnection(AirportGroup), sep = ":") - 2) 

## Split Direct Flights 
OD07_11_flight_path1 <- OD07_11_flight_path %>%
  filter(numstops == 0) %>%
  separate(AirportGroup, sep = ":", into = c("Origin","Destination"), remove = FALSE)

## Split Indirect Flights
OD07_11_flight_path2 <- OD07_11_flight_path %>%
  filter(numstops == 1) %>%
  separate(AirportGroup, sep = ":", into = c("Origin","Layover","Destination"), remove = FALSE)

Dest_Org07_11 <- OD07_11_flight_path1 %>%
  full_join(y = OD07_11_flight_path2)

rm(OD07_11_flight_path)
rm(OD07_11_flight_path1)
rm(OD07_11_flight_path2)


USair <- read_csv("Data/us-airports.csv") 

USair <-  USair %>% 
  dplyr::select(latitude_deg,longitude_deg,iata_code)

USair <- USair %>% 
  mutate(Destination = iata_code,
         Origin = iata_code,
         Layover = iata_code)
  
Loc_Dest <- Dest_Org07_11 %>%
  dplyr::select(Destination,Year,Quarter,AirportGroup,OpCarrier) %>% 
  left_join(y = USair, by = "Destination") 
Loc_Dest <- Loc_Dest %>% 
  mutate(Dest_lat = as.numeric(latitude_deg),
         Dest_long = as.numeric(longitude_deg)) 
Loc_Dest <- Loc_Dest %>% 
  dplyr::select(-c(Origin,Layover,latitude_deg,longitude_deg,iata_code))

Loc_Org <-   Dest_Org07_11 %>%
  dplyr::select(Origin,Year,Quarter,AirportGroup,OpCarrier) %>% 
  left_join(y = USair, by = "Origin") 
Loc_Org <- Loc_Org %>% 
  mutate(Org_lat = as.numeric(latitude_deg),
         Org_long = as.numeric(longitude_deg)) 
Loc_Org <- Loc_Org %>% 
  dplyr::select(-c(Destination,Layover,latitude_deg,longitude_deg,iata_code))
  
Loc_Lay <-  Dest_Org07_11 %>%
  filter(numstops == 1) %>% 
  dplyr::select(Layover,Year,Quarter,AirportGroup,OpCarrier) %>% 
  left_join(y = USair, by = "Layover") 
Loc_Lay <- Loc_Lay %>% 
  mutate(Lay_lat = as.numeric(latitude_deg),
         Lay_long = as.numeric(longitude_deg)) 
Loc_Lay <- Loc_Lay %>% 
  dplyr::select(-c(Origin,Destination,latitude_deg,longitude_deg,iata_code,OpCarrier))
  
Loc_OD <- Loc_Org %>% 
  full_join(y = Loc_Dest)

Loc_OLD <- Loc_OD %>% 
  full_join(y = Loc_Lay)

write_csv(Loc_OD, "Data/Airport_DO_Locations.csv")
write_csv(Loc_Lay, "Data/Airport_Lay_Locations.csv")

rm(Loc_Dest)
rm(Loc_Org)
rm(Loc_Lay)
rm(Loc_OD)

