# Load Packages ----
pacman::p_load(tidyverse,skimr,stargazer,pander,
               kableExtra, ggplot2, haven, broom, 
               sandwich, lmtest, memisc, xtable, AER, plm,
               reshape2, igraph)

# Set Print Binary ----
print <- FALSE
sample <- FALSE

# Functions ----
## Market Share Calculation ----
Market_Share <-  function (data, sample, print) {
  
  
  ## Select necessary Data
  OD_select <- data %>% 
    dplyr::select(Year,Quarter,AirportGroup,OpCarrier,Passengers,MktFare)
  ## sample data
  if(sample) {
    RNGversion("4.0.0")
    set.seed(3357)
    OD_select <- OD_select[sample(1:nrow(OD_select)), ]
    OD_select <-  OD_select[1:1000,]
  }
  ## Count stops 
  OD_numStop <- OD_select %>% 
    group_by(Year,Quarter,AirportGroup) %>% 
    mutate(numstops = count.fields(textConnection(AirportGroup), sep = ":") - 2) %>% 
    ungroup() %>% 
    mutate(row = row_number())
  
  
  ## Sperate Airport Groups
  ## no stop
  OD_noStop <- OD_numStop %>% 
    filter(numstops == 0) %>%
    separate(AirportGroup, sep = ":", into = c("Origin","Destination"), remove = FALSE)
  ## one stop
  OD_oneStop <- OD_numStop %>% 
    filter(numstops == 1) %>% 
    separate(AirportGroup, sep = ":", into = c("Origin","Layover","Destination"), remove = FALSE)
  
  ## join ##
  OD_allStop <- OD_noStop %>% 
    full_join(OD_oneStop)
  
  ## remove two previous data sets
  rm(OD_noStop,OD_oneStop, OD_numStop,OD_select)
  
  ## print to check data
  if (print){
    skim(OD_allStop)
  }
  
  ## Calculate Market Share per Carrier per Airport##
  ## make data long with new olumn for airport_role and Airport
  OD_allstop_long <- OD_allStop %>% 
    pivot_longer(cols = c(Origin,Layover,Destination), names_to = "airport_role", 
                 values_to = "Airport") %>% 
    na.omit()
  ## Group by Quarter and Airport adn then normalize the data 
  OD_allstop_share <- OD_allstop_long %>% 
    group_by(Quarter,Airport) %>%
    mutate(market_share = Passengers / sum(Passengers)) %>% 
    ungroup()
  
  if(print){
    OD_allstop_share %>% 
      arrange(Airport) %>% 
      view()
    OD_allstop_share %>% group_by(Quarter, Airport) %>% 
      summarize(sum_time = sum(market_share)) %>% 
      ungroup() %>% 
      skim()
  }
  return(OD_allstop_share)
}

## Pair Calculation ----
Pair <-  function(data, print){
  ## Split Data to put layover as destination for OL pairings and Layover as Origin in LD pairings
  OD_OL_Pair <- data %>%
    filter(airport_role != "Destination" & numstops == 1) %>%
    pivot_wider(id_cols = c(Quarter,AirportGroup, Passengers, MktFare, OpCarrier,
                            numstops,row),
                names_from = airport_role, 
                values_from = Airport) %>% 
    rename(Layover = Destination) %>% 
    dplyr::select(-row) %>% 
    na.omit()
  if(print == TRUE){
    view(skim(OD_OL_Pair))
  }
  OD_LD_Pair <- data %>% 
    filter(airport_role != "Origin" & numstops == 1) %>% 
    pivot_wider(id_cols = c(Quarter,AirportGroup, Passengers, MktFare, OpCarrier,
                            numstops,row),
                names_from = airport_role, 
                values_from = Airport) %>% 
    rename(Layover = Origin) %>% 
    dplyr::select(-row) %>% 
    na.omit()
  if(print == TRUE){
    view(skim(OD_LD_Pair))
  }
  OD_OD_Pair <- data %>% 
    filter(airport_role != "Layover" & numstops == 0) %>% 
    pivot_wider(id_cols = c(Quarter,AirportGroup, Passengers, MktFare, OpCarrier,
                            numstops, row),
                names_from = airport_role,
                values_from = Airport) %>% 
    dplyr::select(-row)
  if(print == TRUE){
    view(skim(OD_OD_Pair))
  }
  ## merge all the data together
  
  OD_Pair <- OD_OD_Pair %>% bind_rows(OD_LD_Pair) %>% bind_rows(OD_OL_Pair)
  if(print == TRUE){
    view(skim(OD_Pair))
  }
  OD_Pair %>% group_by(Quarter, Origin, Destination) %>% 
    arrange(Origin) %>% 
    summarize(sum_pass = sum(Passengers)) %>% 
    ungroup()
  if(print == TRUE){
    skim(OD_Pair)
  }
  return(OD_Pair)
}

## Eigenvalue centralty calculation ----
eig_calc <-  function(data_pair,data_share,print,q){
  ## function to calculate Eigenvalues with q = to the quarter to do it for
  ## Seperate Quarters and create graph
  ODQ_graph <- data_pair %>% 
    filter(Quarter == q) %>% 
    dplyr::select(Origin, Destination, Passengers) %>% 
    graph_from_data_frame(directed = T)
  ## add weights
  E(ODQ_graph)$weight = data_pair$Passengers
  
  ## Create Adjacency Matrix
  ODQ_Matrix <- ODQ_graph %>%
    get.adjacency(attr = "Passengers",
                  sparse = F)
  if(print == TRUE){
    print(ODQ_Matrix)
    print(is_weighted(ODQ_graph))
    plot <- plot(ODQ_graph)
    ggsave(filename = "Figures/Example Network.png", plot)
  }
  
  ## Calculate eigen values
  eigen_valQ <-  eigen(ODQ_Matrix, symmetric = FALSE, only.values = TRUE)
  ## Calculate eigen_vector centrality
  eigen_centvalQ <- eigen_centrality(ODQ_graph, directed = TRUE, scale = TRUE)
  ## Create Data Frame with eigenvector centrality values nad Airport IACA
  eigen_DFQ <- data.frame("Airport" = V(ODQ_graph)$name,"eigenvalues" = eigen_centvalQ$vector) 
  ## Join this with allstop_share data
  ODQ_Eigen <- data_share %>% 
    filter(Quarter == q) %>% 
    left_join(y = eigen_DFQ, by = "Airport")
  
  ## pivot back to wide 
  ODQ_Eigen_wide <- ODQ_Eigen %>% 
    pivot_wider(id_cols = c(row,Year,Quarter, Passengers, MktFare, OpCarrier,
                            numstops, AirportGroup),
                names_from = airport_role, 
                values_from = c(Airport, eigenvalues, market_share))
  
  return(ODQ_Eigen_wide)
}


# 2007 ------------------------------------------------------------------------- 
## Load data
OD07 <- read_csv("Data/OD_Eigenvector_07.csv")

## Mark_Share and Pair ----
## create market share data set 
Mark_share07 <- Market_Share(data = OD07,sample = TRUE ,print = FALSE)

## Create Proper Pairs 
Pair_07 <- Pair(data = Mark_share07, print = FALSE)

## Quarter 1 ----
## Calaculate eigenvector centrality for Quarter 1
eigen_07Q1 <- eig_calc(data_pair = Pair_07, data_share = Mark_share07, print = T, q = 1)

## Quarter 2 ----
## Calculate eigenvector centrality for Quarter 2 
eigen_07Q2 <- eig_calc(data_pair = Pair_07, data_share = Mark_share07, print = F, q = 2)

## Quarter 3 ----
## Calculate eigenvector centrality for Quarter 3
eigen_07Q3 <- eig_calc(data_pair = Pair_07, data_share = Mark_share07, print = F, q = 3)

## Quarter 4 ----
## Calculate eigenvector centrality for Quarter 4 
eigen_07Q4 <- eig_calc(data_pair = Pair_07, data_share = Mark_share07, print = F, q = 4)

## Join All Quarters ----
eigen_07 <- eigen_07Q1 %>% bind_rows(eigen_07Q2) %>% bind_rows(eigen_07Q3) %>% bind_rows(eigen_07Q4)

### Keep only eigen_07 ----
rm(Mark_share07,Pair_07,eigen_07Q1,eigen_07Q2,eigen_07Q3,eigen_07Q4,OD07)

# 2008 ------------------------------------------------------------------------- 
## Load data 
OD08 <- read_csv("Data/OD_Eigenvector_08.csv")

## Market_Share and Pair ----
## create market share data set 
Mark_share08 <- Market_Share(data = OD08,sample = FALSE,print = FALSE)

## Create Proper Pairs 
Pair_08 <- Pair(data = Mark_share08, print = FALSE)
## Quarter 1 ----
## Calaculate eigenvector centrality for Quarter 1
eigen_08Q1 <- eig_calc(data_pair = Pair_08, data_share = Mark_share08, print = F, q = 1)

## Quarter 2 ----
## Calculate eigenvector centrality for Quarter 2 
eigen_08Q2 <- eig_calc(data_pair = Pair_08, data_share = Mark_share08, print = F, q = 2)

## Quarter 3 ----
## Calculate eigenvector centrality for Quarter 3
eigen_08Q3 <- eig_calc(data_pair = Pair_08, data_share = Mark_share08, print = F, q = 3)

## Quarter 4 ----
## Calculate eigenvector centrality for Quarter 4 
eigen_08Q4 <- eig_calc(data_pair = Pair_08, data_share = Mark_share08, print = F, q = 4)

## Join All Quarters ----
eigen_08 <- eigen_08Q1 %>% bind_rows(eigen_08Q2) %>% bind_rows(eigen_08Q3) %>% bind_rows(eigen_08Q4)

### Keep only eigen_08 ----
rm(Mark_share08,Pair_08,eigen_08Q1,eigen_08Q2,eigen_08Q3,eigen_08Q4,OD08)

# 2009 ------------------------------------------------------------------------- 
## Load data 
OD09 <- read_csv("Data/OD_Eigenvector_09.csv")

## Market_Share and Pair ----
## create market share data set 
Mark_share09 <- Market_Share(data = OD09,sample = FALSE,print = FALSE)

## Create Proper Pairs 
Pair_09 <- Pair(data = Mark_share09, print = FALSE)
## Quarter 1 ----
## Calaculate eigenvector centrality for Quarter 1
eigen_09Q1 <- eig_calc(data_pair = Pair_09, data_share = Mark_share09, print = F, q = 1)

## Quarter 2 ----
## Calculate eigenvector centrality for Quarter 2 
eigen_09Q2 <- eig_calc(data_pair = Pair_09, data_share = Mark_share09, print = F, q = 2)

## Quarter 3 ----
## Calculate eigenvector centrality for Quarter 3
eigen_09Q3 <- eig_calc(data_pair = Pair_09, data_share = Mark_share09, print = F, q = 3)

## Quarter 4 ----
## Calculate eigenvector centrality for Quarter 4 
eigen_09Q4 <- eig_calc(data_pair = Pair_09, data_share = Mark_share09, print = F, q = 4)

## Join All Quarters ----
eigen_09 <- eigen_09Q1 %>% bind_rows(eigen_09Q2) %>% bind_rows(eigen_09Q3) %>% bind_rows(eigen_09Q4)

### Keep only eigen_09 ----
rm(Mark_share09,Pair_09,eigen_09Q1,eigen_09Q2,eigen_09Q3,eigen_09Q4,OD09)

# 2010 ------------------------------------------------------------------------- 
## Load data 
OD10 <- read_csv("Data/OD_Eigenvector_10.csv")

## Market_Share and Pair ----
## create market share data set 
Mark_share10 <- Market_Share(data = OD10,sample = FALSE,print = FALSE)

## Create Proper Pairs 
Pair_10 <- Pair(data = Mark_share10, print = FALSE)
## Quarter 1 ----
## Calaculate eigenvector centrality for Quarter 1
eigen_10Q1 <- eig_calc(data_pair = Pair_10, data_share = Mark_share10, print = F, q = 1)

## Quarter 2 ----
## Calculate eigenvector centrality for Quarter 2 
eigen_10Q2 <- eig_calc(data_pair = Pair_10, data_share = Mark_share10, print = F, q = 2)

## Quarter 3 ----
## Calculate eigenvector centrality for Quarter 3
eigen_10Q3 <- eig_calc(data_pair = Pair_10, data_share = Mark_share10, print = F, q = 3)

## Quarter 4 ----
## Calculate eigenvector centrality for Quarter 4 
eigen_10Q4 <- eig_calc(data_pair = Pair_10, data_share = Mark_share10, print = F, q = 4)

## Join All Quarters ----
eigen_10 <- eigen_10Q1 %>% bind_rows(eigen_10Q2) %>% bind_rows(eigen_10Q3) %>% bind_rows(eigen_10Q4)

### Keep only eigen_10 ----
rm(Mark_share10,Pair_10,eigen_10Q1,eigen_10Q2,eigen_10Q3,eigen_10Q4,OD10)

# 2011 ------------------------------------------------------------------------- 
## Load data 
OD11 <- read_csv("Data/OD_Eigenvector_11.csv")

## Market_Share and Pair ----
## create market share data set 
Mark_share11 <- Market_Share(data = OD11,sample = FALSE,print = FALSE)

## Create Proper Pairs 
Pair_11 <- Pair(data = Mark_share11, print = FALSE)
## Quarter 1 ----
## Calaculate eigenvector centrality for Quarter 1
eigen_11Q1 <- eig_calc(data_pair = Pair_11, data_share = Mark_share11, print = F, q = 1)

## Quarter 2 ----
## Calculate eigenvector centrality for Quarter 2 
eigen_11Q2 <- eig_calc(data_pair = Pair_11, data_share = Mark_share11, print = F, q = 2)

## Quarter 3 ----
## Calculate eigenvector centrality for Quarter 3
eigen_11Q3 <- eig_calc(data_pair = Pair_11, data_share = Mark_share11, print = F, q = 3)

## Quarter 4 ----
## Calculate eigenvector centrality for Quarter 4 
eigen_11Q4 <- eig_calc(data_pair = Pair_11, data_share = Mark_share11, print = F, q = 4)

## Join All Quarters ----
eigen_11 <- eigen_11Q1 %>% bind_rows(eigen_11Q2) %>% bind_rows(eigen_11Q3) %>% bind_rows(eigen_11Q4)

### Keep only eigen_11 ----
rm(Mark_share11,Pair_11,eigen_11Q1,eigen_11Q2,eigen_11Q3,eigen_11Q4,OD11)

# Combine All ----
eigen <- eigen_07 %>% bind_rows(eigen_08) %>%  bind_rows(eigen_09) %>% bind_rows(eigen_10) %>% bind_rows(eigen_11)
skim(eigen)
## sperate one stop and no stop trips
eigen_one_stop <- eigen %>% 
  filter(numstops == 1)

eigen_no_stop <-  eigen %>% 
  filter(numstops == 0) %>% 
  dplyr::select(-c(eigenvalues_Layover,Airport_Layover,market_share_Layover))
# export 
write_csv(eigen_no_stop, "Data/OD_Eigen_NoStop.csv")
write_csv(eigen_one_stop, "Data/OD_Eigen_OneStop.csv")


# # TEST MATRIX ------------------------------------------------------------------
# g1 <- graph( edges=c(1,2,1,4,1,5,3,4,3,5,3,6,4,5,4,6,5,6), n=3, directed=F)
# plot(g1)
# m1 <- g1 %>% 
#   get.adjacency(sparse = F)
# m1
# eigen(m1, symmetric = TRUE, only.values = TRUE)
# eigen_vectors(m1)
# ec_g1 <- eigen_centrality(g1, directed = FALSE, scale = TRUE)
# ?eigen_centrality()
# new_data <- data.frame("centrality" = ec_g1$vector, "nodes" = c(1,2,3,4,5,6))
# 
# 
# 
# ## A simple example with a couple of actors
# ## The typical case is that these tables are read in from files....
# actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David",
#                             "Esmeralda"),
#                      age=c(48,33,45,34,21),
#                      gender=c("F","M","F","M","F"))
# relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
#                                "David", "Esmeralda"),
#                         to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
#                         same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
#                         friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))
# g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)
# print(g, e=TRUE, v=TRUE)
# plot(g)
