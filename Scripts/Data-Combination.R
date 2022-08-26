## Setup and load packages
pacman::p_load(tidyverse,skimr,stargazer,pander,
               kableExtra, ggplot2, haven, broom, 
               sandwich, lmtest, memisc, xtable, AER, plm)

## PRINT BOOLEAN ##
print <- FALSE

#### 2007 ####
## Q1 ##
## Load data

OD07Q1 <- read_csv("Data/2007-D1B1-Data/Origin_Destination_2007Q1.csv")
## Select necessary Data
OD07Q1 <- OD07Q1 %>% 
  dplyr::select(Year,Quarter,OriginAirportID,OriginCountry,
                OriginStateFips,DestAirportID,DestCountry,DestStateFips,
                AirportGroup,OriginWac,DestWac,WacGroup,
                Passengers,MktFare,OpCarrier) %>% 
  filter(OriginCountry == "US" & DestCountry == "US" & 
           (OpCarrier == "NW" | OpCarrier == "DL"))
## Test if the filters worked
if (print) {
  OD07Q1 %>% distinct(OpCarrier, OriginCountry,Quarter) %>% view()
}
## Q2 ##
## Load data
OD07Q2 <- read_csv("Data/2007-D1B1-Data/Origin_Destination_2007Q2.csv")
## Select necessary Data
OD07Q2 <- OD07Q2 %>% 
  dplyr::select(Year,Quarter,OriginAirportID,OriginCountry,
                OriginStateFips,DestAirportID,DestCountry,DestStateFips,
                AirportGroup,OriginWac,DestWac,WacGroup,
                Passengers,MktFare,OpCarrier) %>% 
  filter(OriginCountry == "US" & DestCountry == "US" & 
           (OpCarrier == "NW" | OpCarrier == "DL"))
## Test if the filters worked
if (print) {
  OD07Q2 %>% distinct(OpCarrier, OriginCountry,Quarter) %>% view()
}
## Q3 ##
## Load data
OD07Q3 <- read_csv("Data/2007-D1B1-Data/Origin_Destination_2007Q3.csv")
## Select necessary Data
OD07Q3 <- OD07Q3 %>% 
  dplyr::select(Year,Quarter,OriginAirportID,OriginCountry,
                OriginStateFips,DestAirportID,DestCountry,DestStateFips,
                AirportGroup,OriginWac,DestWac,WacGroup,
                Passengers,MktFare,OpCarrier) %>% 
  filter(OriginCountry == "US" & DestCountry == "US" & 
           (OpCarrier == "NW" | OpCarrier == "DL"))
## Test if the filters worked
if (print) {
  OD07Q3 %>% distinct(OpCarrier, OriginCountry,Quarter) %>% view()
}
## Q4 ##
## Load data
OD07Q4 <- read_csv("Data/2007-D1B1-Data/Origin_Destination_2007Q4.csv")
## Select necessary Data
OD07Q4 <- OD07Q4 %>% 
  dplyr::select(Year,Quarter,OriginAirportID,OriginCountry,
                OriginStateFips,DestAirportID,DestCountry,DestStateFips,
                AirportGroup,OriginWac,DestWac,WacGroup,
                Passengers,MktFare,OpCarrier) %>% 
  filter(OriginCountry == "US" & DestCountry == "US" & 
           (OpCarrier == "NW" | OpCarrier == "DL"))
## Test if the filters worked
if (print) {
  OD07Q4 %>% distinct(OpCarrier, OriginCountry,Quarter) %>% view()
}

## Combine For 2007
OD07 <- OD07Q1 %>% bind_rows(OD07Q2) %>% bind_rows(OD07Q3) %>% bind_rows(OD07Q4)
## drop other data frames
rm(OD07Q1,OD07Q2,OD07Q3,OD07Q4)

#### 2008 ####
## Q1 ##
## Load data
OD08Q1 <- read_csv("Data/2008-D1B1-Data/Origin_Destination_2008Q1.csv")
## Select necessary Data
OD08Q1 <- OD08Q1 %>% 
  dplyr::select(Year,Quarter,OriginAirportID,OriginCountry,OriginStateFips,DestAirportID,DestCountry,DestStateFips,AirportGroup,OriginWac,DestWac,WacGroup,Passengers,MktFare,OpCarrier) %>% 
  filter(OriginCountry == "US" & DestCountry == "US" & (OpCarrier == "NW" | OpCarrier == "DL"))
## Test if the filters worked
if (print) {
  OD08Q1 %>% distinct(OpCarrier, OriginCountry) %>% view()
}
## Q2 ##
## Load data
OD08Q2 <- read_csv("Data/2008-D1B1-Data/Origin_Destination_2008Q2.csv")
## Select necessary Data
OD08Q2 <- OD08Q2 %>% 
  dplyr::select(Year,Quarter,OriginAirportID,OriginCountry,OriginStateFips,DestAirportID,DestCountry,DestStateFips,AirportGroup,OriginWac,DestWac,WacGroup,Passengers,MktFare,OpCarrier) %>% 
  filter(OriginCountry == "US" & DestCountry == "US" & (OpCarrier == "NW" | OpCarrier == "DL"))
## Test if the filters worked
if (print) {
  OD08Q2 %>% distinct(OpCarrier, OriginCountry) %>% view()
}
## Q3 ##
## Load data
OD08Q3 <- read_csv("Data/2008-D1B1-Data/Origin_Destination_2008Q3.csv")
## Select necessary Data
OD08Q3 <- OD08Q3 %>% 
  dplyr::select(Year,Quarter,OriginAirportID,OriginCountry,OriginStateFips,DestAirportID,DestCountry,DestStateFips,AirportGroup,OriginWac,DestWac,WacGroup,Passengers,MktFare,OpCarrier) %>% 
  filter(OriginCountry == "US" & DestCountry == "US" & (OpCarrier == "NW" | OpCarrier == "DL"))
## Test if the filters worked
if (print) {
  OD08Q3 %>% distinct(OpCarrier, OriginCountry) %>% view()
}
## Q4 ##
## Load data
OD08Q4 <- read_csv("Data/2008-D1B1-Data/Origin_Destination_2008Q4.csv")
## Select necessary Data
OD08Q4 <- OD08Q4 %>% 
  dplyr::select(Year,Quarter,OriginAirportID,OriginCountry,OriginStateFips,DestAirportID,DestCountry,DestStateFips,AirportGroup,OriginWac,DestWac,WacGroup,Passengers,MktFare,OpCarrier) %>% 
  filter(OriginCountry == "US" & DestCountry == "US" & (OpCarrier == "NW" | OpCarrier == "DL"))
## Test if the filters worked
if (print) {
  OD08Q4 %>% distinct(OpCarrier, OriginCountry) %>% view()
}
## Combine For 2008
OD08 <- OD08Q1 %>% bind_rows(OD08Q2) %>% bind_rows(OD08Q3) %>% bind_rows(OD08Q4)
## drop other data frames
rm(OD08Q1,OD08Q2,OD08Q3,OD08Q4)

#### 2009 ####
## Q1 ##
## Load data
OD09Q1 <- read_csv("Data/2009-D1B1-Data/Origin_Destination_2009Q1.csv")
## Select necessary Data
OD09Q1 <- OD09Q1 %>% 
  dplyr::select(Year,Quarter,OriginAirportID,OriginCountry,OriginStateFips,DestAirportID,DestCountry,DestStateFips,AirportGroup,OriginWac,DestWac,WacGroup,Passengers,MktFare,OpCarrier) %>% 
  filter(OriginCountry == "US" & DestCountry == "US" & (OpCarrier == "NW" | OpCarrier == "DL"))
## Test if the filters worked
if (print) {
  OD09Q1 %>% distinct(OpCarrier, OriginCountry) %>% view()
}
## Q2 ##
## Load data
OD09Q2 <- read_csv("2009-D1B1-Data/Origin_Destination_2009Q2.csv")
## Select necessary Data
OD09Q2 <- OD09Q2 %>% 
  dplyr::select(Year,Quarter,OriginAirportID,OriginCountry,OriginStateFips,DestAirportID,DestCountry,DestStateFips,AirportGroup,OriginWac,DestWac,WacGroup,Passengers,MktFare,OpCarrier) %>% 
  filter(OriginCountry == "US" & DestCountry == "US" & (OpCarrier == "NW" | OpCarrier == "DL"))
## Test if the filters worked
if (print) {
  OD09Q2 %>% distinct(OpCarrier, OriginCountry) %>% view()
}
## Q3 ##
## Load data
OD09Q3 <- read_csv("2009-D1B1-Data/Origin_Destination_2009Q3.csv")
## Select necessary Data
OD09Q3 <- OD09Q3 %>% 
  dplyr::select(Year,Quarter,OriginAirportID,OriginCountry,OriginStateFips,DestAirportID,DestCountry,DestStateFips,AirportGroup,OriginWac,DestWac,WacGroup,Passengers,MktFare,OpCarrier) %>% 
  filter(OriginCountry == "US" & DestCountry == "US" & (OpCarrier == "NW" | OpCarrier == "DL"))
## Test if the filters worked
if (print) {
  OD09Q3 %>% distinct(OpCarrier, OriginCountry) %>% view()
}
## Q4 ##
## Load data
OD09Q4 <- read_csv("2009-D1B1-Data/Origin_Destination_2009Q4.csv")
## Select necessary Data
OD09Q4 <- OD09Q4 %>% 
  dplyr::select(Year,Quarter,OriginAirportID,OriginCountry,OriginStateFips,DestAirportID,DestCountry,DestStateFips,AirportGroup,OriginWac,DestWac,WacGroup,Passengers,MktFare,OpCarrier) %>% 
  filter(OriginCountry == "US" & DestCountry == "US" & (OpCarrier == "NW" | OpCarrier == "DL"))
## Test if the filters worked
if (print) {
  OD09Q4 %>% distinct(OpCarrier, OriginCountry) %>% view()
}
## Combine For 2009
OD09 <- OD09Q1 %>% bind_rows(OD09Q2) %>% bind_rows(OD09Q3) %>% bind_rows(OD09Q4)
## drop other data frames
rm(OD09Q1,OD09Q2,OD09Q3,OD09Q4)

#### 2010 ####
## Q1 ##
## Load data
OD10Q1 <- read_csv("Data/2010-D1B1-Data/Origin_Destination_2010Q1.csv")
## Select necessary Data
OD10Q1 <- OD10Q1 %>% 
  dplyr::select(Year,Quarter,OriginAirportID,OriginCountry,OriginStateFips,DestAirportID,DestCountry,DestStateFips,AirportGroup,OriginWac,DestWac,WacGroup,Passengers,MktFare,OpCarrier) %>% 
  filter(OriginCountry == "US" & DestCountry == "US" & (OpCarrier == "NW" | OpCarrier == "DL"))
## Test if the filters worked
if (print) {
  OD10Q1 %>% distinct(OpCarrier, OriginCountry) %>% view()
}
## Q2 ##
## Load data
OD10Q2 <- read_csv("Data/2010-D1B1-Data/Origin_Destination_2010Q2.csv")
## Select necessary Data
OD10Q2 <- OD10Q2 %>% 
  dplyr::select(Year,Quarter,OriginAirportID,OriginCountry,OriginStateFips,DestAirportID,DestCountry,DestStateFips,AirportGroup,OriginWac,DestWac,WacGroup,Passengers,MktFare,OpCarrier) %>% 
  filter(OriginCountry == "US" & DestCountry == "US" & (OpCarrier == "NW" | OpCarrier == "DL"))
## Test if the filters worked
if (print) {
  OD10Q2 %>% distinct(OpCarrier, OriginCountry) %>% view()
}
## Q3 ##
## Load data
OD10Q3 <- read_csv("Data/2010-D1B1-Data/Origin_Destination_2010Q3.csv")
## Select necessary Data
OD10Q3 <- OD10Q3 %>% 
  dplyr::select(Year,Quarter,OriginAirportID,OriginCountry,OriginStateFips,DestAirportID,DestCountry,DestStateFips,AirportGroup,OriginWac,DestWac,WacGroup,Passengers,MktFare,OpCarrier) %>% 
  filter(OriginCountry == "US" & DestCountry == "US" & (OpCarrier == "NW" | OpCarrier == "DL"))
## Test if the filters worked
if (print) {
  OD10Q3 %>% distinct(OpCarrier, OriginCountry) %>% view()
}
## Q4 ##
## Load data
OD10Q4 <- read_csv("Data/2010-D1B1-Data/Origin_Destination_2010Q4.csv")
## Select necessary Data
OD10Q4 <- OD10Q4 %>% 
  dplyr::select(Year,Quarter,OriginAirportID,OriginCountry,OriginStateFips,DestAirportID,DestCountry,DestStateFips,AirportGroup,OriginWac,DestWac,WacGroup,Passengers,MktFare,OpCarrier) %>% 
  filter(OriginCountry == "US" & DestCountry == "US" & (OpCarrier == "NW" | OpCarrier == "DL"))
## Test if the filters worked
if (print) {
  OD10Q4 %>% distinct(OpCarrier, OriginCountry) %>% view()
}
## Combine For 2010
OD10 <- OD10Q1 %>% bind_rows(OD10Q2) %>% bind_rows(OD10Q3) %>% bind_rows(OD10Q4)
## drop other data frames
rm(OD10Q1,OD10Q2,OD10Q3,OD10Q4)

#### 2011 ####
## Q1 ##
## Load data
OD11Q1 <- read_csv("Data/2011-D1B1-Data/Origin_Destination_2011Q1.csv")
## Select necessary Data
OD11Q1 <- OD11Q1 %>% 
  dplyr::select(Year,Quarter,OriginAirportID,OriginCountry,OriginStateFips,DestAirportID,DestCountry,DestStateFips,AirportGroup,OriginWac,DestWac,WacGroup,Passengers,MktFare,OpCarrier) %>% 
  filter(OriginCountry == "US" & DestCountry == "US" & (OpCarrier == "NW" | OpCarrier == "DL"))
## Test if the filters worked
if (print) {
  OD11Q1 %>% distinct(OpCarrier, OriginCountry) %>% view()
}
## Q2 ##
## Load data
OD11Q2 <- read_csv("Data/2011-D1B1-Data/Origin_Destination_2011Q2.csv")
## Select necessary Data
OD11Q2 <- OD11Q2 %>% 
  dplyr::select(Year,Quarter,OriginAirportID,OriginCountry,OriginStateFips,DestAirportID,DestCountry,DestStateFips,AirportGroup,OriginWac,DestWac,WacGroup,Passengers,MktFare,OpCarrier) %>% 
  filter(OriginCountry == "US" & DestCountry == "US" & (OpCarrier == "NW" | OpCarrier == "DL"))
## Test if the filters worked
if (print) {
  OD11Q2 %>% distinct(OpCarrier, OriginCountry) %>% view()
}
## Q3 ##
## Load data
OD11Q3 <- read_csv("Data/2011-D1B1-Data/Origin_Destination_2011Q3.csv")
## Select necessary Data
OD11Q3 <- OD11Q3 %>% 
  dplyr::select(Year,Quarter,OriginAirportID,OriginCountry,OriginStateFips,DestAirportID,DestCountry,DestStateFips,AirportGroup,OriginWac,DestWac,WacGroup,Passengers,MktFare,OpCarrier) %>% 
  filter(OriginCountry == "US" & DestCountry == "US" & (OpCarrier == "NW" | OpCarrier == "DL"))
## Test if the filters worked
if (print) {
  OD11Q3 %>% distinct(OpCarrier, OriginCountry) %>% view()
}
## Q4 ##
## Load data
OD11Q4 <- read_csv("Data/2011-D1B1-Data/Origin_Destination_2011Q4.csv")
## Select necessary Data
OD11Q4 <- OD11Q4 %>% 
  dplyr::select(Year,Quarter,OriginAirportID,OriginCountry,OriginStateFips,DestAirportID,DestCountry,DestStateFips,AirportGroup,OriginWac,DestWac,WacGroup,Passengers,MktFare,OpCarrier) %>% 
  filter(OriginCountry == "US" & DestCountry == "US" & (OpCarrier == "NW" | OpCarrier == "DL"))
## Test if the filters worked
if (print) {
  OD11Q4 %>% distinct(OpCarrier, OriginCountry) %>% view()
}
## Combine For 2011
OD11 <- OD11Q1 %>% bind_rows(OD11Q2) %>% bind_rows(OD11Q3) %>% bind_rows(OD11Q4)
## drop other data frames
rm(OD11Q1,OD11Q2,OD11Q3,OD11Q4)

#### Combine All Years and Export####
## bind rows
OD07_11 <- OD07 %>% bind_rows(OD08) %>% bind_rows(OD09) %>% bind_rows(OD10) %>% bind_rows(OD11)

## export to a csv file
write_csv(OD07_11,"Data/Origin_Destination_07-11.csv")




