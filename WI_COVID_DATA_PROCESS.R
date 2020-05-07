library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(httr)
library(jsonlite)
library(purrr)
library(lubridate)

#READS IN DATA FROM WI DHHS API
DATA_REQUEST <- httr::GET("https://opendata.arcgis.com/datasets/b913e9591eae4912b33dc5b4e88646c5_10.geojson")
WI_COVID_DATA <-  fromJSON(rawToChar(DATA_REQUEST$content))
WI_COVID_DATA <- WI_COVID_DATA[[c(4,2)]]
WI_COVID_DATA <- WI_COVID_DATA[,1:7]
#LoadDttm changed in the data, so I removed this line.
#WI_COVID_DATA$Date <- .POSIXct(WI_COVID_DATA$LoadDttm/1000)
WI_COVID_DATA$Date <- lubridate::date(WI_COVID_DATA$LoadDttm)

#READ IN A FILE WITH COUNTY POPULATION VALUES AND REGION FROM DHS.  NOTE THAT THESE ARE 2018 VALUES.
WI_POP <- read_csv("WI_POP.csv")
REGION_POP <- aggregate(WI_POP$POPULATION, by = list(WI_POP$REGION), FUN = sum)
colnames(REGION_POP) <- c("REGION", "REG_POP")
WI_POP <- merge(x = WI_POP, y = REGION_POP, by = "REGION")
TOP_6_POP_COUNTIES <- WI_POP[order(-WI_POP$POPULATION),c("COUNTY")]
TOP_6_POP_COUNTIES <- TOP_6_POP_COUNTIES[1:6]

COUNTY_VALUES <- WI_COVID_DATA[which(WI_COVID_DATA$GEO == "County"), c(1:8)]
COUNTY_VALUES$COUNTY <- str_to_upper(COUNTY_VALUES$NAME)
COUNTY_VALUES <- merge(x = COUNTY_VALUES, y = WI_POP, by = "COUNTY")
COUNTY_VALUES$COUNTY_POSITIVE_PER_THOUS <- COUNTY_VALUES$POSITIVE / (COUNTY_VALUES$POPULATION/1000) 
COUNTY_VALUES$TOTAL_TESTS <- COUNTY_VALUES$NEGATIVE + COUNTY_VALUES$POSITIVE
COUNTY_VALUES$PERCENT_POS <- COUNTY_VALUES$POSITIVE / COUNTY_VALUES$TOTAL_TESTS

#CALCULATE DAILY CHANGES BY COUNTY
COUNTY_VALUES <- COUNTY_VALUES[order(COUNTY_VALUES$COUNTY, COUNTY_VALUES$Date ),]

COUNTY_VALUES <- COUNTY_VALUES %>%
                  group_by(COUNTY) %>%
                  mutate(.,
                         CHANGE_POS = POSITIVE - lag(POSITIVE)
                         )


COUNTY_VALUES_TOP_6_COUNTIES <- COUNTY_VALUES[which(COUNTY_VALUES$COUNTY %in% TOP_6_POP_COUNTIES),]








REGION_VALUES <- COUNTY_VALUES %>%
                  group_by(Date, REGION) %>%
                  summarize(., 
                            NEGATIVE = sum(NEGATIVE),
                            POSITIVE = sum(POSITIVE),
                            CHANGE_POS = sum(CHANGE_POS),
                            REG_POP = sum(POPULATION),
                  ) %>%
                  mutate(.,
                         TOTAL_TESTS = NEGATIVE + POSITIVE,
                         REGION_POSITIVE_PER_THOUS = POSITIVE / (REG_POP/1000)
                        )
REGION_VALUES$PERCENT_POS <- REGION_VALUES$POSITIVE / REGION_VALUES$TOTAL_TESTS                     
    





