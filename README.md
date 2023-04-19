# Airline-Merger-Project
- Paper and code studying the Northwest-Delta Airline Merger of 2009.
- The data is not included in this repository but was taken from the Department of Transportations annual D1B1 D100 survey.
- Hoffman_EC280 is the full paper

-A guide to readers
  - Start with the poster and the powerpoint presentation to get a good background on the project
  - Then move on to the paper

-A guide to coders/data scientists/interested freaks
  -The scripts folder contains 6 files that are each a step in the data analysis process
  -All data starts in the Data-Combonation file
    -The goal of the file is to combine DOT data that I loaded in per quarter
    -The data starts out split because filtering can only run on the smaller samples because of computing power limitations
    -The data is then combined into a singular file
  -Then the data goes to the Final_Grant-Hoffman file 
    -This file separates the AirportGroup which shows airport in the flight like so: ABC:DEF:GHI or UVW:XYZ
    -numstops is created to help seperate these airports later. 
  -Eigenvector_Data_Combo
    -Does the same thing as the Data-Combination file
  -Eigenvector_Calculator
    -Calculates network stats and market share for each airport in each quarter
    -seperates the data into one_stop and no_stop
  -Additional__Stop_Calc
    -Calculates the competition stats and recombines the data
  -Data-Analysis
    -Does the regessions and produces the main results for the paper
