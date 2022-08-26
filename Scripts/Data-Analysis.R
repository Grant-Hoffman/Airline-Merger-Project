# Regression and Data Analysis 


# Setup ------------------------------------------------------------------------ 

# Load packages
pacman::p_load(tidyverse,skimr,stargazer,pander,
               kableExtra, ggplot2, haven, broom, 
               sandwich, lmtest, memisc, xtable, AER, plm,
               reshape2, igraph, dynlm)
# Load Data
OD <- read_csv("Data/Regression-Data.csv")
skim(OD)
OD_new <- OD %>% 
  mutate(market_share_Origin = market_share_Origin * 10000,
         market_share_Destination = market_share_Destination * 10000,
         market_share_Layover = market_share_Layover * 10000,
         compGroup = case_when("NW_better" == compGroup ~ "Northwestern Better",
                               "DL_better" == compGroup ~ "Delta Better",
                               ("NW_better" != compGroup & "DL_better" != compGroup) ~ compGroup))

# Summary Tables --------------------------------------------------------------
## Variable summary table
OD_summary_table <-  OD_new %>% 
  dplyr::select(MktFare, Passengers, eigenvalues_Origin, eigenvalues_Destination, eigenvalues_Layover,
                market_share_Origin, market_share_Destination, market_share_Layover) %>% 
  skim_without_charts() %>% 
  dplyr::select(-c(skim_variable, numeric.p25,numeric.p50,numeric.p75, n_missing, complete_rate, skim_type)) %>%
  mutate(Variable = c("Market Fare", "Passengers", "Origin","Destination","Layover",
                      "Origin","Destination","Layover")) %>% 
  relocate(Variable) %>% 
  kable(format = 'latex',
        col.names = c("Variable", "Mean", "Standard Deviation", "Min", "Max"),
        digits = 2,
        caption = "Summary Table \\label{sumTab}",
        booktabs = F) %>%   
  kable_styling(latex_options = c("scale_down")) %>% 
  pack_rows("Eigenvector Centrality", 3, 5) %>%
  pack_rows("Market Share", 6, 8) %>% 
  add_footnote("Note: market share is in basis points", notation = "none")

OD_summary_table

## Create summary of route type 
Route_type_sum <- OD_new %>% 
  filter(period <= 12) %>% 
  group_by(compGroup) %>% 
  summarize(numRoutes = dplyr::n()) %>% 
  ungroup() 


Route_type_means <- OD_new %>% 
  dplyr::select(compGroup, OpCarrier, MktFare, Passengers,
                market_share_Origin, market_share_Destination, market_share_Layover,
                eigenvalues_Origin, eigenvalues_Destination, eigenvalues_Layover) %>% 
  group_by(compGroup, OpCarrier) %>% 
  summarize(mean_markFare = mean(MktFare),
            mean_Pass = mean(Passengers),
            mean_OrigShare = mean(market_share_Origin),
            mean_DestShare = mean(market_share_Destination),
            mean_LayShare = mean(market_share_Layover, na.rm = TRUE),
            mean_OrigEig = mean(eigenvalues_Origin),
            mean_DestEig = mean(eigenvalues_Destination),
            mean_LayEig = mean(eigenvalues_Layover, na.rm = TRUE)) %>% 
  pivot_wider(names_from = OpCarrier, 
              values_from = c(mean_markFare, mean_Pass, mean_OrigShare, mean_DestShare, mean_LayShare, 
                              mean_OrigEig, mean_DestEig, mean_LayEig)) %>% 
  left_join(Route_type_sum) %>% 
  relocate(numRoutes) %>% 
  relocate(compGroup) %>% 
  replace(is.na(.), 0)

## Route type table ----
Route_type_means %>% 
  dplyr::select(compGroup, numRoutes, mean_Pass_DL, mean_Pass_NW, mean_markFare_DL, mean_markFare_NW) %>%  
  kable(format = 'latex',
        col.names = c("Route Competition", "# of Routes", "Delta", "Northwestern", "Delta", "Northwestern"),
        caption = "Route Competition \\label{RCtab}",
        booktabs = F,
        digits = 2) %>% 
  kable_styling(latex_options = c("scale_down")) %>% 
  add_header_above(c(" " = 2, "Average # Passengers" = 2, "Average Fare" = 2))

## Market Share Route type table ----
Route_type_means %>% 
  dplyr::select(compGroup, mean_OrigShare_DL, mean_DestShare_DL, mean_LayShare_DL, 
                mean_OrigShare_NW, mean_DestShare_NW, mean_LayShare_NW) %>%  
  kable(format = 'latex',
        col.names = c("Route Competition", "Origin", "Destination", "Layover",
                      "Origin", "Destination", "Layover"),
        caption = "Route Competition Market Share \\label{RCMktTab}",
        booktabs = F,
        digits = 3) %>% 
  kable_styling(latex_options = c("scale_down")) %>% 
  add_header_above(c(" ", "Delta" = 3, "Northwestern" = 3))

## Eigenvalue route type table ----
Route_type_means %>% 
  dplyr::select(compGroup, mean_OrigEig_DL, mean_DestEig_DL, mean_LayEig_DL, 
                mean_OrigEig_NW, mean_DestEig_NW, mean_LayEig_NW) %>%  
  kable(format = 'latex',
        col.names = c("Route Competition", "Origin", "Destination", "Layover",
                      "Origin", "Destination", "Layover"),
        caption = "Route Competition Eigenvalues \\label{RCEigTab}",
        booktabs = F,
        digits = 3) %>% 
  kable_styling(latex_options = c("scale_down")) %>% 
  add_header_above(c(" ", "Delta" = 3, "Northwestern" = 3))


# Regression --------------------------------------------------------
## change post-merger competion values to the competition value with the most passengers throughout the ex-ante years. 
OD_merge <- OD_new %>% 
  filter(period <= 12) %>% 
  group_by(OD_Group) %>% 
  arrange(Passengers) %>% 
  summarize(compGroup_perm = first(compGroup))

OD_ed <- OD_new %>% left_join(OD_merge) 

OD_perm <- OD_ed %>% 
  mutate(compGroup = if_else(period > 12, compGroup_perm, compGroup)) %>% 
  filter(!is.na(compGroup))

skim(OD_perm)
## aggregate over Airport over period
OD_reg <- OD_perm %>% 
  dplyr::select(period, OD_Group, compGroup, MktFare, Passengers, numstops,
                eigenvalues_Origin, eigenvalues_Destination, eigenvalues_Layover,
                market_share_Origin, market_share_Destination, market_share_Layover) %>%
  group_by(period, OD_Group) %>%
  arrange(period, OD_Group, Passengers) %>% 
  summarize(MktFare = mean(MktFare),
            compGroup = first(compGroup),
            Passengers = mean(Passengers),
            numstops = first(numstops),
            eigenvalues_Origin = mean(eigenvalues_Origin),
            eigenvalues_Destination = mean(eigenvalues_Destination),
            eigenvalues_Layover = mean(eigenvalues_Layover, na.rm = TRUE),
            market_share_Origin = mean(market_share_Origin),
            market_share_Destination = mean(market_share_Destination),
            market_share_Layover = mean(market_share_Layover, na.rm = TRUE)) %>% 
  replace(is.na(.), 0) %>% 
  mutate(Origin_EMS = eigenvalues_Origin * market_share_Origin,
         Destination_EMS = eigenvalues_Destination * market_share_Destination,
         Layover_EMS = eigenvalues_Layover * market_share_Layover,
         ln_MktFare = if_else(MktFare > 0, log(MktFare), 0)) %>% 
  rename(period = time)
## first model only route types as independant vars. and MktFare dependent var ----

mod1 <- plm(MktFare ~ Passengers + factor(compGroup),
            data = OD_reg,
            index = c("OD_Group","time"),
            model = "within")

## second model with passengers and market_shares and eigenvalues 
mod2 <- plm(MktFare ~ Passengers + factor(compGroup) + 
              market_share_Origin + market_share_Destination  + 
              market_share_Layover + eigenvalues_Origin + eigenvalues_Destination + eigenvalues_Layover,
            data = OD_reg,
            index = c("OD_Group","time"),
            model = "within")

## Third Model with eigen centrality market share interactions
mod3 <- plm(MktFare ~ Passengers + factor(compGroup) +  
              market_share_Origin + market_share_Destination  + 
                market_share_Layover + eigenvalues_Origin + eigenvalues_Destination + eigenvalues_Layover + 
              Origin_EMS + Destination_EMS + Layover_EMS,
            data = OD_reg,
            index = c("OD_Group","time"),
            model = "within")

## Fourth Model with ln(MktFare) dependent variable
mod4 <- plm(ln_MktFare ~ Passengers + factor(compGroup) + 
              market_share_Origin + market_share_Destination  + 
              market_share_Layover + eigenvalues_Origin + eigenvalues_Destination + eigenvalues_Layover + 
              Origin_EMS + Destination_EMS + Layover_EMS,
            data = OD_reg,
            index = c("OD_Group", "time"),
            model = "within")

## Regression Table
stargazer(mod1, mod2, mod3, mod4, type = "latex",
          title = "Market Fares Regression Analysis",
          dep.var.caption = "Origin-Destination Pair and Time Fixed Effects",
          dep.var.labels = c("Market Fare$^{1}$", "Market Fare Growth$^{2}$"),
          covariate.labels = c("Passengers$^{3}$", "\\textbf{Competition$^{4}$} \\\\ \\hline \\quad Direct", 
                               "\\quad Delta Better", "\\quad Northwest Only", "\\quad Northwest Better", 
                               "\\textbf{Market Share$^{5}$} \\\\ \\hline \\quad Origin", 
                               "\\quad Destination", "\\quad Layover",
                               "\\textbf{Eigenvector Centrality$^{6}$} \\\\ \\hline \\quad Origin", 
                               "\\quad Destination", "\\quad Layover",
                               "\\textbf{Centrality * Market Share$^{7}$} \\\\ \\hline \\quad Origin",
                               "\\quad Destination", "\\quad Layover"),
          single.row = TRUE,
          digits = 2,
          digits.extra = 3,
          font.size = "small",
          df = FALSE,
          label = "RegTab",
          notes = c("1. Average market fare for any flight with each OD pair for each quarter", 
                  "2. The natural log of average market fare.", 
                  "3. The average number of passengers per itinerary for each OD-pair", 
                  "4. Competition type dummies with \\textit{Delta Only} as the default", 
                  "5. Market shares for each airport in the itinerary for the OD-pair", 
                  "6. Eigenvector centrality of each airport in the itinerary for the OD-pair", 
                  "7. The interaction term for market share and eigenvector centrality"),
          notes.align = "l") 


?stargazer()


