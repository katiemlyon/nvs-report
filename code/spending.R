## @knitr spending
#nvs2018 <- read.csv("C:/Users/klyon/hd_visitorsurvey/nvs-reports/data/nvs2018.csv")

#library(plyr)
library(dplyr)
library(tidyr)
library(likert)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(scales)
#library(waffle)
#library(extrafont)

#load functions
source("code/functions/calc_pct.R")
source("code/functions/round_df.R")

#read data
nvs2018 <- read.csv("data/nvs2018.csv")

##### Spending
spending <-
  subset(
    nvs2018,
    select = c(HOTELCOST:SOUVCOST, OTHCOST)
  )
spending <- data.frame(apply(spending, 2, as.numeric))
#spending <- na.omit(spending)
str(spending)

# create total spending variable
spending$spendTotal <- rowSums (spending, na.rm = TRUE)
str(spending$spendTotal)


names(spending) = c(
  "Hotel, bed & breakfast, cabin, etc",
  "Camping fees",
  "Restaurants and bars",
  "Groceries",
  "Gasoline and oil",
  "Local transportation",
  "Guides and tour fees",
  "Equipment rental",
  "Sporting goods",
  "Souvenirs/clothing and other retail",
  "Other",
  "Total expenditures"
)

spendingTitle <- "Spending"


####
## Local/Nonlocal
local <-
  subset(
    nvs2018,
    select = c(LOCALAREA)
  )
str(local)

## Local/Nonlocal
local <- nvs2018$LOCALAREA
str(local)

localTable <- table(local)
localTable

propLocal = round_df((localTable)["Local"]/sum(localTable)*100,0)
propLocal #percent local

propNonlocal = round_df((localTable)["Nonlocal"]/sum(localTable)*100,0)
propNonlocal #percent nonlocal


# Time Spent in Area
timeLocal <- nvs2018$DAYSinCOMMUNITY
timeLocal[timeLocal=="999"] <- NA
str(timeLocal)

# Total spent per day
spend <- subset(nvs2018, select=c(LOCALAREA, TOTALpersonday))
spend <- na.omit(spend)
str(spend)
mean(spend$TOTALpersonday)

# subset local and nonlocal spending
str(nvs2018$LOCALAREA)
spendLocal <- subset(nvs2018, LOCALAREA == "Local",
                     select=c(TOTALpersonday))
spendLocal <- na.omit(spendLocal)
range(spendLocal$TOTALpersonday)
mean(spendLocal$TOTALpersonday)

spendNonloc<- subset(nvs2018, LOCALAREA == "Nonlocal",
                     select=c(TOTALpersonday))
spendNonloc <- na.omit(spendNonloc)
range(spendNonloc$TOTALpersonday)
mean(spendNonloc$TOTALpersonday)

# Table comparing local/nonlocal spending
library(mosaic)
library(kableExtra)
spendTable <- favstats(TOTALpersonday ~ LOCALAREA, data = spend)
names(spendTable)
spendTable <- spendTable[, c("LOCALAREA", "n", "median", "mean", "sd", "min", "max")]
names(spendTable) = c("Visitors", "n", "Median", "Mean", "SD", "Min", "Max")
spendTable
kable(spendTable)
kable(spendTable,
      digits = c(0, 0, 2, 2, 2, 2, 2),
      align = "lcrrrrr") %>%
  kable_styling(full_width = FALSE, position = "left") %>%
  footnote(
    footnote_order = c("number", "general"),
    number = c(
      "n = number of visitors who answered both locality and expenditure questions."
    ),
    general = "For each respondent, reported expenditures were divided by the number of persons in their group that shared expenses in order to determine the spending per person per trip. This number was then divided by the number of days spent in the local area to determine the spending per person per day for each respondent. For respondents who reported spending less than one full day in the local community, trip length was set equal to one day. These visitor spending estimates are appropriate for the sampling periods selected by refuge staff (see table 3 for sampling period dates and figure 1 for the primary visitor activities in which people participated), and may not be representative of the total population of visitors to this refuge."
  )


## Other Costs
otherCost <- nvs2018$OTHERCOSTTXT
otherCost <- otherCost[!is.na(nvs2018$OTHERCOSTTXT)]

