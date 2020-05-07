#This file is used to try things out
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(httr)
library(jsonlite)
library(purrr)
library(lubridate)

#COVID_19_DATA <- read_csv("~/Downloads/COVID-19_Historical_Data_Table(1).csv", 
#                                                            col_types = cols(LoadDttm = col_number()))


DATA_REQUEST <- httr::GET("https://opendata.arcgis.com/datasets/b913e9591eae4912b33dc5b4e88646c5_10.geojson")
WI_COVID_DATA <-  fromJSON(rawToChar(DATA_REQUEST$content))
WI_COVID_DATA <- WI_COVID_DATA[[c(4,2)]]
WI_COVID_DATA <- WI_COVID_DATA[,1:7]
#LoadDttm changed in the data, so I removed this line.
#WI_COVID_DATA$Date <- .POSIXct(WI_COVID_DATA$LoadDttm/1000)
WI_COVID_DATA$Date <- lubridate::date(WI_COVID_DATA$LoadDttm)


WI_POP <- read_csv("WI_POP.csv")
REGION_POP <- aggregate(WI_POP$POPULATION, by = list(WI_POP$REGION), FUN = sum)
colnames(REGION_POP) <- c("REGION", "REG_POP")
WI_POP <- merge(x = WI_POP, y = REGION_POP, by = "REGION")

COUNTY_VALUES <- WI_COVID_DATA[which(WI_COVID_DATA$GEO == "County"), c(1:7)]
COUNTY_VALUES$COUNTY <- str_to_upper(COUNTY_VALUES$NAME)
COUNTY_VALUES <- merge(x = COUNTY_VALUES, y = WI_POP, by = "COUNTY")
COUNTY_VALUES$COUNTY_POSITIVE_PER_THOUS <- COUNTY_VALUES$POSITIVE / (COUNTY_VALUES$POPULATION/1000)  

COUNTY_VALUES_TOP_6_COUNTIES <- COUNTY_VALUES[which(COUNTY_VALUES$COUNTY %in% 
                                                      c("MILWAUKEE", "DANE", 
                                                        "WAUKESHA", "BROWN", "RACINE", "OUTAGAMIE")),]



TOP_6_COUNTIES_POSITIVE <- ggplot(COUNTY_VALUES_TOP_6_COUNTIES, aes(Date, POSITIVE))
TOP_6_COUNTIES_POSITIVE + geom_line(aes(color = COUNTY, group = COUNTY)) + scale_y_log10()

TOP_6_COUNTIES_POSITIVE + geom_line(aes(color = COUNTY, group = COUNTY))



TOP_6_COUNTIES_POSITIVE_PER_THOUS <- ggplot(COUNTY_VALUES_TOP_6_COUNTIES, aes(Date, COUNTY_POSITIVE_PER_THOUS))
TOP_6_COUNTIES_POSITIVE_PER_THOUS + geom_line(aes(color = COUNTY, group = COUNTY)) + scale_y_log10() + labs(y = "Positive Tests per Thousand Population", 
                                                                                                        title = "Covid-19 Positive Tests (Log Scale)")

TOP_6_COUNTIES_POSITIVE_PER_THOUS + geom_line(aes(color = COUNTY, group = COUNTY)) +  labs(y = "Positive Tests per Thousand People", 
                                                                                                        title = "Covid-19 Positive Tests")







STATE_VALUES <- WI_COVID_DATA[which(WI_COVID_DATA$GEO == "State"), c(1:7)]
STATE_VALUES$TOTAL_TESTS <- STATE_VALUES$NEGATIVE + STATE_VALUES$POSITIVE
STATE_VALUES$Percent_POS <- STATE_VALUES$POSITIVE / STATE_VALUES$TOTAL_TESTS






graph1 <- ggplot(STATE_VALUES, aes(LoadDttm, POSITIVE))
graph1 + geom_line() + scale_y_log10()

graph_PERCENT_POSITIVE <- ggplot(STATE_VALUES, aes(LoadDttm, Percent_POS))
graph_PERCENT_POSITIVE + geom_line() + ylim(0,.25)