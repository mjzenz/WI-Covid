#This file is used to try things out
library(readr)
library(ggplot2)
library(stringr)



COVID_19_DATA <- read_csv("~/Downloads/COVID-19_Historical_Data_Table(1).csv", 
                                                            col_types = cols(LoadDttm = col_number()))
WI_POP <- read_csv("WI_POP.csv")
REGION_POP <- aggregate(WI_POP$POPULATION, by = list(WI_POP$REGION), FUN = sum)
colnames(REGION_POP) c("REGION", "REG_POP")
WI_POP <- merge(x = WI_POP, y = REGION_POP, by = "REGION")

COUNTY_VALUES <- COVID_19_DATA[which(COVID_19_DATA$GEO == "County"), c(1:7)]
COUNTY_VALUES$NAME <- str_to_upper(COUNTY_VALUES$NAME)
COUNTY_VALUES <- merge(x = COUNTY_VALUES, y = WI_POP, by.x = "NAME", by.y = "COUNTY")






STATE_VALUES <- COVID_19_DATA[which(COVID_19_DATA$GEO == "State"), c(1:7)]
STATE_VALUES$TOTAL_TESTS <- STATE_VALUES$NEGATIVE + STATE_VALUES$POSITIVE
STATE_VALUES$Percent_POS <- STATE_VALUES$POSITIVE / STATE_VALUES$TOTAL_TESTS






graph1 <- ggplot(STATE_VALUES, aes(LoadDttm, POSITIVE))
graph1 + geom_line() + scale_y_log10()

graph_PERCENT_POSITIVE <- ggplot(STATE_VALUES, aes(LoadDttm, Percent_POS))
graph_PERCENT_POSITIVE + geom_line() + ylim(0,.25)