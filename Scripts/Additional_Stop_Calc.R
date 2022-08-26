
# Setup -------------------------------------------------------------------

## Load packages
pacman::p_load(tidyverse,skimr,stargazer,pander,
               kableExtra, ggplot2, haven, broom, 
               sandwich, lmtest, memisc, xtable, AER, plm,
               reshape2, igraph)

## Load Data 
OD_NoStop <- read_csv("Data/OD_Eigen_NoStop.csv")
OD_OneStop <- read_csv("Data/OD_Eigen_OneStop.csv")

## Combine
OD <- OD_NoStop %>% full_join(OD_OneStop)
skim(OD)
# Orgin Destinination Pairs -----------------------------------------------

## Create Time and OD group variables ----

OD_per <- OD %>% 
  mutate(period = case_when((Year == 2007 & Quarter == 1) ~ 1, (Year == 2007 & Quarter == 2) ~ 2,
                            (Year == 2007 & Quarter == 3) ~ 3, (Year == 2007 & Quarter == 4) ~ 4,
                            (Year == 2008 & Quarter == 1) ~ 5, (Year == 2008 & Quarter == 2) ~ 6,
                            (Year == 2008 & Quarter == 3) ~ 7, (Year == 2008 & Quarter == 4) ~ 8,
                            (Year == 2009 & Quarter == 1) ~ 9, (Year == 2009 & Quarter == 2) ~ 10,
                            (Year == 2009 & Quarter == 3) ~ 11, (Year == 2009 & Quarter == 4) ~ 12,
                            (Year == 2010 & Quarter == 1) ~ 13, (Year == 2010 & Quarter == 2) ~ 14,
                            (Year == 2010 & Quarter == 3) ~ 15, (Year == 2010 & Quarter == 4) ~ 16,
                            (Year == 2011 & Quarter == 1) ~ 17, (Year == 2011 & Quarter == 2) ~ 18,
                            (Year == 2011 & Quarter == 3) ~ 19, (Year == 2011 & Quarter == 4) ~ 20,)) %>% 
  unite("OD_Group", Airport_Origin:Airport_Destination, remove = FALSE)


## Filter by carrier ----
OD_car <- OD_per %>% 
  filter(OpCarrier == "NW" | OpCarrier == "DL")

## Select Period OpCarrier OD_Group numstops and group ----

OD_sel <- OD_car %>% 
  dplyr::select(period, OpCarrier, AirportGroup, OD_Group, numstops)

## Pivot wide on Carrier ----

NW_better <- OD_sel %>% group_by(period, OpCarrier, OD_Group) %>% 
  filter(period <= 12) %>% 
  distinct(period, OpCarrier, OD_Group, numstops) %>% 
  ungroup() %>% 
  group_by(period, OD_Group) %>% 
  filter((OpCarrier == "NW" & numstops == 0) | (OpCarrier == "DL" & numstops == 1)) %>% 
  mutate(n_1 = dplyr::n()) %>% 
  filter(n_1 > 1L) %>% 
  arrange(period, OD_Group) %>% 
  mutate(competition = "NW_better") %>% 
  ungroup()

DL_better <- OD_sel %>% group_by(period, OpCarrier, OD_Group) %>% 
  filter(period <= 12) %>% 
  distinct(period, OpCarrier, OD_Group, numstops) %>% 
  ungroup() %>% 
  group_by(period, OD_Group) %>% 
  filter((OpCarrier == "NW" & numstops == 1) | (OpCarrier == "DL" & numstops == 0)) %>% 
  mutate(n_1 = dplyr::n()) %>% 
  filter(n_1 > 1L) %>% 
  arrange(period, OD_Group) %>% 
  mutate(competition = "DL_better") %>% 
  ungroup()

Direct <- OD_sel %>% group_by(period, OpCarrier, OD_Group) %>% 
  filter(period <= 12) %>% 
  distinct(period, OpCarrier, OD_Group, numstops) %>% 
  ungroup() %>% 
  group_by(period, OD_Group) %>% 
  filter((OpCarrier == "NW" & numstops == 0) | (OpCarrier == "DL" & numstops == 0)) %>% 
  mutate(n_1 = dplyr::n()) %>% 
  filter(n_1 > 1L) %>% 
  arrange(period, OD_Group) %>% 
  mutate(competition = "Direct") %>% 
  ungroup()

Stop_Direct <- OD_sel %>% group_by(period, OpCarrier, OD_Group) %>% 
  filter(period <= 12) %>% 
  distinct(period, OpCarrier, OD_Group, numstops) %>% 
  ungroup() %>% 
  group_by(period, OD_Group) %>% 
  filter((OpCarrier == "NW" & numstops == 1) | (OpCarrier == "DL" & numstops == 1)) %>% 
  mutate(n_1 = dplyr::n()) %>% 
  filter(n_1 > 1L) %>% 
  arrange(period, OD_Group) %>% 
  mutate(competition = "Stop_Direct") %>% 
  ungroup()

Comp <- Direct %>% full_join(NW_better) %>% full_join(DL_better) %>% full_join(Stop_Direct) %>% 
  dplyr::select(-c(n_1)) 
skim(Comp)

OD_better <- OD_sel %>% left_join(Comp) %>% 
  mutate(compGroup = case_when((!is.na(competition)) ~ competition,
                                (is.na(competition) & OpCarrier == "DL" ~ "Delta No-Comp"),
                               (is.na(competition) & OpCarrier == "NW") ~ "Northwestern No-Comp")) %>% 
  dplyr::select(-competition)

skim(OD_better)
## Combine the rest of the data
FINISHED <- OD_per %>% right_join(OD_better)
skim(FINISHED)

## Export Finished Data 
write_csv(FINISHED, "Data/Regression-Data.csv")
