##THIS FILE RUNS DATA PROCESS
library(readr)
library(ggplot2)
library(stringr)
library(tidyr)
library(dplyr)
library(httr)
library(jsonlite)
library(purrr)
library(lubridate)
library(zoo)

#READS IN DATA FROM WI DHHS API
DATA.REQUEST  <- httr::GET("https://opendata.arcgis.com/datasets/b913e9591eae4912b33dc5b4e88646c5_10.geojson?where=GEO%20%3D%20'County'")

print("Downloaded data from DHS")
WI_COVID_DATA <-  fromJSON(rawToChar(DATA.REQUEST$content))
WI_COVID_DATA <- WI_COVID_DATA[[c(4,2)]]
WI_COVID_DATA <- WI_COVID_DATA[,c("OBJECTID","GEOID","GEO","NAME","NEGATIVE", "POSITIVE","DEATHS", "DATE")]

#LoadDttm changed in the data, so I removed this line.
#WI_COVID_DATA$Date <- .POSIXct(WI_COVID_DATA$DATE/1000)
WI_COVID_DATA$Date <- lubridate::date(WI_COVID_DATA$DATE)
MAX_DATE <- max(WI_COVID_DATA$Date)

#ELIMINATE NON-COUNTY DATA
WI_COVID_DATA <- WI_COVID_DATA[which(WI_COVID_DATA$GEO == "County"),]
#ELIMINATE OLDER DATA
WI_COVID_DATA <- WI_COVID_DATA[which(WI_COVID_DATA$Date > Sys.Date() %m+% months(-3)),]
#WI_COVID_DATA[which(WI_COVID_DATA$NEGATIVE == -999),]$NEGATIVE <- 0
#WI_COVID_DATA[which(WI_COVID_DATA$POSITIVE == -999),]$POSITIVE <- 0
print("Subset to last three months")


#This uses min_negative and min_positive but returns -999 for NA values
WI_COVID_DATA <- WI_COVID_DATA %>%
                group_by(GEO, GEOID) %>%
                summarise(., min_negative = min(NEGATIVE),
                          min_positive = min(POSITIVE),
                          min_deaths = min(DEATHS)) %>%
                inner_join(x = ., y = WI_COVID_DATA, by = c("GEO", "GEOID")) %>%
                mutate(., NEGATIVE = NEGATIVE-min_negative,
                       POSITIVE = POSITIVE - min_positive,
                       DEATHS = DEATHS - min_deaths) %>%
              select(., -c(min_negative, min_positive, min_deaths))

print("Calculated counts from last three months")

#READ IN A FILE WITH COUNTY POPULATION VALUES AND REGION FROM DHS.  NOTE THAT THESE ARE 2018 VALUES.
WI_POP <- read_csv("WI_POP.csv")
REGION_POP <- aggregate(WI_POP$POPULATION, by = list(WI_POP$REGION), FUN = sum)
colnames(REGION_POP) <- c("REGION", "REG_POP")
WI_POP <- merge(x = WI_POP, y = REGION_POP, by = "REGION")
TOP_6_POP_COUNTIES <- WI_POP[order(-WI_POP$POPULATION),c("COUNTY")]
TOP_6_POP_COUNTIES <- TOP_6_POP_COUNTIES[1:6]

#COUNTY_VALUES <- WI_COVID_DATA[which(WI_COVID_DATA$GEO == "County"), c(1:8)]
COUNTY_VALUES <- WI_COVID_DATA[which(WI_COVID_DATA$GEO == "County"),]
COUNTY_VALUES$COUNTY <- str_to_upper(COUNTY_VALUES$NAME)
COUNTY_VALUES <- merge(x = COUNTY_VALUES, y = WI_POP, by = "COUNTY")
COUNTY_VALUES$COUNTY_POSITIVE_PER_THOUS <- COUNTY_VALUES$POSITIVE / (COUNTY_VALUES$POPULATION/1000) 
COUNTY_VALUES$TOTAL_TESTS <- COUNTY_VALUES$NEGATIVE + COUNTY_VALUES$POSITIVE

#CALCULATE DAILY CHANGES BY COUNTY
COUNTY_VALUES <- COUNTY_VALUES[order(COUNTY_VALUES$COUNTY, COUNTY_VALUES$Date ),]

COUNTY_VALUES <- COUNTY_VALUES %>%
                  group_by(COUNTY) %>%
                  mutate(.,
                         CHANGE_POS = POSITIVE - lag(POSITIVE),
                         CHANGE_NEG = NEGATIVE - lag(NEGATIVE),
                         CHANGE_DEATHS = DEATHS - lag(DEATHS)
                         )

COUNTY_VALUES$CHANGE_TOTAL_TESTS <- COUNTY_VALUES$CHANGE_POS + COUNTY_VALUES$CHANGE_NEG
COUNTY_VALUES$PERCENT_POS_CUM <- COUNTY_VALUES$POSITIVE / COUNTY_VALUES$TOTAL_TESTS
COUNTY_VALUES$PERCENT_POS <- COUNTY_VALUES$CHANGE_POS / COUNTY_VALUES$CHANGE_TOTAL_TESTS

COUNTY_VALUES<- COUNTY_VALUES %>%
                group_by(COUNTY) %>%
                mutate(AVG_7_PERCENT_POS = rollmean(PERCENT_POS, k = 7, fill = NA, align = "right"),
                       AVG_7_CHANGE_POS = rollmean(CHANGE_POS, k = 7, fill = NA, align = "right"))



COUNTY_VALUES_TOP_6_COUNTIES <- COUNTY_VALUES[which(COUNTY_VALUES$COUNTY %in% TOP_6_POP_COUNTIES),]

COUNTY_VALUES_DANE <- COUNTY_VALUES[which(COUNTY_VALUES$COUNTY == "DANE"),]

print("Calculated county values")


REGION_VALUES <- COUNTY_VALUES %>%
                  group_by(Date, REGION) %>%
                  summarize(., 
                            NEGATIVE = sum(NEGATIVE),
                            POSITIVE = sum(POSITIVE),
                            DEATHS = sum(DEATHS),
                            CHANGE_POS = sum(CHANGE_POS),
                            CHANGE_NEG = sum(CHANGE_NEG),
                            CHANGE_DEATHS = sum(CHANGE_DEATHS),
                            CHANGE_TOTAL_TESTS = sum(CHANGE_TOTAL_TESTS),
                            REG_POP = sum(POPULATION),
                  ) %>%
                  mutate(.,
                         TOTAL_TESTS = NEGATIVE + POSITIVE,
                         REGION_POSITIVE_PER_THOUS = POSITIVE / (REG_POP/1000)
                        )
REGION_VALUES$PERCENT_POS_CUM <- REGION_VALUES$POSITIVE / REGION_VALUES$TOTAL_TESTS                     
REGION_VALUES$PERCENT_POS <- REGION_VALUES$CHANGE_POS/REGION_VALUES$CHANGE_TOTAL_TESTS    

REGION_VALUES<- REGION_VALUES %>%
  group_by(REGION) %>%
  mutate(AVG_7_PERCENT_POS = rollmean(PERCENT_POS, k = 7, fill = NA, align = "right"),
         AVG_7_CHANGE_POS = rollmean(CHANGE_POS, k = 7, fill = NA, align = "right"))

print("Calculated region values")


STATEWIDE_VALUES  <- COUNTY_VALUES  %>%
                  group_by(Date) %>%
                  summarize(., 
                            NEGATIVE = sum(NEGATIVE),
                            POSITIVE = sum(POSITIVE),
                            DEATHS = sum(DEATHS),
                            CHANGE_POS = sum(CHANGE_POS),
                            CHANGE_NEG = sum(CHANGE_NEG),
                            CHANGE_DEATHS = sum(CHANGE_DEATHS),
                            CHANGE_TOTAL_TESTS = sum(CHANGE_TOTAL_TESTS),
                            STATE_POP = sum(POPULATION)) %>%
                  mutate(.,
                         TOTAL_TESTS = NEGATIVE + POSITIVE,
                         STATE_POSITIVE_PER_THOUS = POSITIVE / (STATE_POP/1000)
                         )

STATEWIDE_VALUES$PERCENT_POS_CUM <- STATEWIDE_VALUES$POSITIVE / STATEWIDE_VALUES$TOTAL_TESTS                     
STATEWIDE_VALUES$PERCENT_POS <- STATEWIDE_VALUES$CHANGE_POS/ STATEWIDE_VALUES$CHANGE_TOTAL_TESTS   


STATEWIDE_VALUES <- STATEWIDE_VALUES %>%
                    mutate(AVG_7_PERCENT_POS = rollmean(PERCENT_POS, k = 7, fill = NA, align = "right"),
                           AVG_7_CHANGE_POS = rollmean(CHANGE_POS, k = 7, fill = NA, align = "right"),
                           AVG_7_CHANGE_DEATHS = rollmean(CHANGE_DEATHS, k = 7, fill = NA, align = "right"))    
