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

MIN_DATE_INTERVAL <- -3

#READS IN DATA FROM WI DHHS API
#County Filter Stopped Working, so removed it.
#DATA.REQUEST  <- httr::GET("https://opendata.arcgis.com/datasets/b913e9591eae4912b33dc5b4e88646c5_10.geojson?where=GEO%20%3D%20'County'")
#DATA.REQUEST  <- httr::GET("https://opendata.arcgis.com/datasets/5374188992374b318d3e2305216ee413_12.geojson")
DATA.REQUEST <- httr::GET("https://opendata.arcgis.com/datasets/0b7bac0afc464e7783474cb62272d9b8_12.geojson")

print("Downloaded data from DHS")
WI_COVID_DATA <-  fromJSON(rawToChar(DATA.REQUEST$content))
WI_COVID_DATA <- WI_COVID_DATA[[c(4,2)]]
WI_COVID_DATA <- WI_COVID_DATA[,c("OBJECTID","GEOID","GEO","NAME","NEGATIVE", "POSITIVE","HOSP_YES", "HOSP_NO", "HOSP_UNK", "POS_FEM","POS_MALE", "POS_OTH", "POS_0_9", "POS_10_19","POS_20_29", "POS_30_39", "POS_40_49", "POS_50_59", "POS_60_69", "POS_70_79", "POS_80_89", "POS_90"  ,"DEATHS","DTHS_FEM","DTHS_MALE", "DTHS_OTH", "DTHS_0_9", "DTHS_10_19","DTHS_20_29", "DTHS_30_39", "DTHS_40_49", "DTHS_50_59", "DTHS_60_69", "DTHS_70_79", "DTHS_80_89", "DTHS_90"   , "DATE")]



#LoadDttm changed in the data, so I removed this line.
#WI_COVID_DATA$Date <- .POSIXct(WI_COVID_DATA$DATE/1000)
WI_COVID_DATA$Date <- lubridate::date(WI_COVID_DATA$DATE)
MAX_DATE <- max(WI_COVID_DATA$Date)

#ELIMINATE NON-COUNTY DATA
WI_COVID_DATA <- WI_COVID_DATA[which(WI_COVID_DATA$GEO == "County"),]


#ELIMINATE OLDER DATA
#WI_COVID_DATA <- WI_COVID_DATA[which(WI_COVID_DATA$Date > Sys.Date() %m+% months(-3)),]
if(length(WI_COVID_DATA[which(WI_COVID_DATA$POSITIVE == -999),]$NEGATIVE)>0){WI_COVID_DATA[which(WI_COVID_DATA$NEGATIVE == -999),]$NEGATIVE <- 0}
if(length(WI_COVID_DATA[which(WI_COVID_DATA$POSITIVE == -999),]$POSITIVE)>0){WI_COVID_DATA[which(WI_COVID_DATA$POSITIVE == -999),]$POSITIVE <- 0}
if(length(WI_COVID_DATA[which(WI_COVID_DATA$POSITIVE == -999),]$HOSP_YES)>0){ WI_COVID_DATA[which(WI_COVID_DATA$HOSP_YES == -999),]$HOSP_YES <- 0}
if(length(WI_COVID_DATA[which(WI_COVID_DATA$POSITIVE == -999),]$DEATHS)>0){WI_COVID_DATA[which(WI_COVID_DATA$DEATHS == -999),]$DEATHS <- 0}

#Remove date with no data - state system upgrade being done.
WI_COVID_DATA <- WI_COVID_DATA[which(!(WI_COVID_DATA$Date == "2020-10-17")),]

print("Subset to last three months")

WI_COVID_AGE  <- WI_COVID_DATA[,c("OBJECTID","GEOID","GEO","NAME", "POS_0_9", "POS_10_19","POS_20_29", "POS_30_39", "POS_40_49", "POS_50_59", "POS_60_69", "POS_70_79", "POS_80_89", "POS_90" , "Date")]
#WI_COVID_HOSP  <- WI_COVID_DATA[,c("OBJECTID","GEOID","GEO","NAME", "HOSP_YES", "HOSP_NO", "HOSP_UNK", "Date")]

#This uses min_negative and min_positive but returns -999 for NA values
# WI_COVID_DATA <- WI_COVID_DATA %>%
#                 group_by(GEO, GEOID) %>%
#                 summarise(., min_negative = min(NEGATIVE),
#                           min_positive = min(POSITIVE),
#                           min_hos = min(HOSP_YES),
#                           min_deaths = min(DEATHS)) %>%
#                 inner_join(x = ., y = WI_COVID_DATA, by = c("GEO", "GEOID")) %>%
#                 mutate(., NEGATIVE = NEGATIVE-min_negative,
#                        POSITIVE = POSITIVE - min_positive,
#                        HOSP_YES = HOSP_YES - min_hos,
#                        DEATHS = DEATHS - min_deaths) %>%
#               select(., -c(min_negative, min_positive,min_hos, min_deaths))

#print("Calculated counts from last three months")

#READ IN A FILE WITH COUNTY POPULATION VALUES AND REGION FROM DHS.  NOTE THAT THESE ARE 2018 VALUES.
WI_POP <- read_csv("WI_POP.csv")
REGION_POP <- aggregate(WI_POP$POPULATION, by = list(WI_POP$REGION), FUN = sum)
colnames(REGION_POP) <- c("REGION", "REG_POP")
WI_POP <- merge(x = WI_POP, y = REGION_POP, by = "REGION")
TOP_6_POP_COUNTIES <- WI_POP[order(-WI_POP$POPULATION),c("COUNTY")]
TOP_6_POP_COUNTIES <- TOP_6_POP_COUNTIES[1:6]

TOP_6_POP_COUNTIES_EACH_REGION  <- WI_POP %>%
                                   group_by(REGION) %>%
                                   slice_max(order_by = POPULATION, n = 6)


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
                         CHANGE_HOSP = HOSP_YES - lag(HOSP_YES),
                         CHANGE_DEATHS = DEATHS - lag(DEATHS)
                         )

COUNTY_VALUES$CHANGE_TOTAL_TESTS <- COUNTY_VALUES$CHANGE_POS + COUNTY_VALUES$CHANGE_NEG
COUNTY_VALUES$PERCENT_POS_CUM <- COUNTY_VALUES$POSITIVE / COUNTY_VALUES$TOTAL_TESTS
COUNTY_VALUES$PERCENT_POS <- COUNTY_VALUES$CHANGE_POS / COUNTY_VALUES$CHANGE_TOTAL_TESTS

COUNTY_VALUES <- COUNTY_VALUES %>%
                group_by(COUNTY) %>%
                mutate(AVG_7_PERCENT_POS = rollmean(PERCENT_POS, k = 7, fill = NA, align = "right"),
                       AVG_7_CHANGE_POS = rollmean(CHANGE_POS, k = 7, fill = NA, align = "right"),
                       AVG_7_CHANGE_HOSP = rollmean(CHANGE_HOSP, k = 7, fill = NA, align = "right"))



COUNTY_VALUES_TOP_6_COUNTIES <- COUNTY_VALUES[which(COUNTY_VALUES$COUNTY %in% TOP_6_POP_COUNTIES),]

COUNTY_VALUES_TOP_6_EACH_REGION  <- COUNTY_VALUES[which(COUNTY_VALUES$COUNTY %in% TOP_6_POP_COUNTIES_EACH_REGION$COUNTY),]



#COUNTY_VALUES_DANE <- COUNTY_VALUES[which(COUNTY_VALUES$COUNTY == "DANE"),]

#COUNTY_VALUES_DANE$MAX_POS  <- NA
#COUNTY_VALUES_DANE$MAX_POS  <- as.integer(COUNTY_VALUES_DANE$MAX_POS)
#COUNTY_VALUES_DANE[which(COUNTY_VALUES_DANE$Date ==  MAX_DATE),]$MAX_POS  <- COUNTY_VALUES_DANE[which(COUNTY_VALUES_DANE$Date ==  MAX_DATE),]$CHANGE_POS

COUNTY_VALUES_SELECT <- COUNTY_VALUES[which(COUNTY_VALUES$COUNTY %in% c("DANE", "BROWN", "EAU CLAIRE", "FLORENCE")),]

COUNTY_VALUES_SELECT$MAX_POS  <- NA
COUNTY_VALUES_SELECT$MAX_POS  <- as.integer(COUNTY_VALUES_SELECT$MAX_POS)
COUNTY_VALUES_SELECT[which(COUNTY_VALUES_SELECT$Date ==  MAX_DATE),]$MAX_POS  <- COUNTY_VALUES_SELECT[which(COUNTY_VALUES_SELECT$Date ==  MAX_DATE),]$CHANGE_POS




print("Calculated county values")


REGION_VALUES <- COUNTY_VALUES %>%
                  group_by(Date, REGION) %>%
                  summarize(., 
                            NEGATIVE = sum(NEGATIVE),
                            POSITIVE = sum(POSITIVE),
                            HOSP_YES = sum(HOSP_YES),
                            DEATHS = sum(DEATHS),
                            CHANGE_POS = sum(CHANGE_POS),
                            CHANGE_NEG = sum(CHANGE_NEG),
                            CHANGE_HOSP = sum(CHANGE_HOSP),
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
         AVG_7_CHANGE_POS = rollmean(CHANGE_POS, k = 7, fill = NA, align = "right"),
          AVG_7_CHANGE_HOSP = rollmean(CHANGE_HOSP, k = 7, fill = NA, align = "right"),
         AVG_7_CHANGE_DEATHS = rollmean(CHANGE_DEATHS, k = 7, fill = NA, align = "right"))

print("Calculated region values")

#Calculate statewide values.

STATEWIDE_VALUES  <- COUNTY_VALUES  %>%
                  group_by(Date) %>%
                  summarize(., 
                            NEGATIVE = sum(NEGATIVE),
                            POSITIVE = sum(POSITIVE),
                            HOSP_YES = sum(HOSP_YES),
                            DEATHS = sum(DEATHS),
                            CHANGE_POS = sum(CHANGE_POS),
                            CHANGE_NEG = sum(CHANGE_NEG),
                            CHANGE_HOSP = sum(CHANGE_HOSP),
                            CHANGE_DEATHS = sum(CHANGE_DEATHS),
                            CHANGE_TOTAL_TESTS = sum(CHANGE_TOTAL_TESTS),
                            STATE_POP = sum(POPULATION)) %>%
                  mutate(.,
                         TOTAL_TESTS = NEGATIVE + POSITIVE,
                         STATE_POSITIVE_PER_THOUS = POSITIVE / (STATE_POP/1000)
                         )

STATEWIDE_VALUES$PERCENT_POS_CUM <- STATEWIDE_VALUES$POSITIVE / STATEWIDE_VALUES$TOTAL_TESTS                     
STATEWIDE_VALUES$PERCENT_POS <- STATEWIDE_VALUES$CHANGE_POS/ STATEWIDE_VALUES$CHANGE_TOTAL_TESTS   

#WI_COVID_DATA <- WI_COVID_DATA[which(WI_COVID_DATA$Date > Sys.Date() %m+% months(-3)),]
#MIN_DATE <- MAX_DATE %m+% months(MIN_DATE_INTERVAL)
MIN_DATE_percent_compare <- "2020-08-01"

STATEWIDE_VALUES <- STATEWIDE_VALUES %>%
                    mutate(., AVG_7_PERCENT_POS = rollmean(PERCENT_POS, k = 7, fill = NA, align = "right"),
                           AVG_7_CHANGE_POS = rollmean(CHANGE_POS, k = 7, fill = NA, align = "right"),
                            AVG_7_CHANGE_HOSP = rollmean(CHANGE_HOSP, k = 7, fill = NA, align = "right"),
                           AVG_7_CHANGE_DEATHS = rollmean(CHANGE_DEATHS, k = 7, fill = NA, align = "right"))

                             
STATEWIDE_VALUES$CHANGE_POS_percent <- STATEWIDE_VALUES$CHANGE_POS /  
                             STATEWIDE_VALUES[which(STATEWIDE_VALUES$Date == MIN_DATE_percent_compare),]$AVG_7_CHANGE_POS
                           
STATEWIDE_VALUES$CHANGE_HOSP_percent <- STATEWIDE_VALUES$CHANGE_HOSP / 
                             STATEWIDE_VALUES[which(STATEWIDE_VALUES$Date == MIN_DATE_percent_compare),]$AVG_7_CHANGE_HOSP
                             

STATEWIDE_VALUES$CHANGE_DEATHS_percent <- STATEWIDE_VALUES$CHANGE_DEATHS /  
                             STATEWIDE_VALUES[which(STATEWIDE_VALUES$Date == MIN_DATE_percent_compare),]$AVG_7_CHANGE_DEATHS
                           
STATEWIDE_VALUES <- STATEWIDE_VALUES %>%
                  mutate(., AVG_7_CHANGE_POS_percent =  rollmean(STATEWIDE_VALUES$CHANGE_POS_percent, k = 7, fill = NA, align = "right"),
                         AVG_7_CHANGE_HOSP_percent =  rollmean(STATEWIDE_VALUES$CHANGE_HOSP_percent, k = 7, fill = NA, align = "right"),
                         AVG_7_CHANGE_DEATHS_percent =  rollmean(STATEWIDE_VALUES$CHANGE_DEATHS_percent, k = 7, fill = NA, align = "right"))    

                           
                           
STATEWIDE_VALUES$MAX_POS  <- NA
STATEWIDE_VALUES$MAX_POS  <- as.integer(STATEWIDE_VALUES$MAX_POS)
STATEWIDE_VALUES[which(STATEWIDE_VALUES$Date ==  MAX_DATE),]$MAX_POS  <- STATEWIDE_VALUES[which(STATEWIDE_VALUES$Date ==  MAX_DATE),]$CHANGE_POS


STATEWIDE_VALUES$MAX_HOSP  <- NA
STATEWIDE_VALUES$MAX_HOSP  <- as.integer(STATEWIDE_VALUES$MAX_HOSP)
STATEWIDE_VALUES[which(STATEWIDE_VALUES$Date == MAX_DATE),]$MAX_HOSP  <- STATEWIDE_VALUES[which(STATEWIDE_VALUES$Date == MAX_DATE),]$CHANGE_HOSP

STATEWIDE_VALUES$MAX_DEATHS  <- NA
STATEWIDE_VALUES$MAX_DEATHS  <- as.integer(STATEWIDE_VALUES$MAX_DEATHS)
STATEWIDE_VALUES[which(STATEWIDE_VALUES$Date == MAX_DATE),]$MAX_DEATHS  <- STATEWIDE_VALUES[which(STATEWIDE_VALUES$Date == MAX_DATE),]$CHANGE_DEATHS


### Log curve fit

STATEWIDE_VALUES_SMOOTH <- STATEWIDE_VALUES[which(!(STATEWIDE_VALUES$CHANGE_DEATHS < 0 
                                                    | STATEWIDE_VALUES$CHANGE_HOSP < 0)),]

DAY_GROUPS_A <-2:5
DAY_GROUPS_B <- c(6,7,1)
STATEWIDE_VALUES_SMOOTH <-  mutate(STATEWIDE_VALUES_SMOOTH, day_count = row_number())

MAX_DAY_COUNT <- STATEWIDE_VALUES_SMOOTH[which(STATEWIDE_VALUES_SMOOTH$Date == MAX_DATE),]$day_count

STATEWIDE_VALUES_SMOOTH <- STATEWIDE_VALUES_SMOOTH %>%
                    mutate(., year_count = year(Date),
                           week_count = week(Date),
                           day_of_week_count = if_else(wday(Date) %in% DAY_GROUPS_A, 1, 2)) %>%
                          group_by(year_count, week_count, day_of_week_count) %>%
                          summarize(mean_CHANGE_DEATHS = mean(CHANGE_DEATHS),
                                    mean_CHANGE_HOSP = mean(CHANGE_HOSP),
                                    Date = max(Date)) %>%
                    mutate(., log_CHANGE_DEATHS = if_else(mean_CHANGE_DEATHS == 0, 0, log(mean_CHANGE_DEATHS)), 
                           log_CHANGE_HOSP = if_else(mean_CHANGE_HOSP ==0, 0,log(mean_CHANGE_HOSP)))



 #lm_DATE_MIN <- MAX_DATE - 50
lm_DATE_MIN <- date("2020-09-15") 


Deaths_lm <- lm(log_CHANGE_DEATHS ~ Date, STATEWIDE_VALUES_SMOOTH, subset = (Date >= lm_DATE_MIN))

summary(Deaths_lm)


Death_Doubling <- as.numeric((log(2))/ (Deaths_lm$coefficients[2]))

Hosp_lm <- lm(log_CHANGE_HOSP ~ Date, STATEWIDE_VALUES_SMOOTH, subset = (Date >= lm_DATE_MIN))

summary(Hosp_lm)


Hosp_Doubling <- as.numeric((log(2))/ (Hosp_lm$coefficients[2]))
