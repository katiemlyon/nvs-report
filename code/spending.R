## @knitr spending
#nvs2018 <- read.csv("C:/Users/klyon/hd_visitorsurvey/nvs-reports/data/nvs2018.csv")

library(dplyr)
library(tidyr)
library(likert)
library(waffle)
library(extrafont)
library(ggplot2)


##### Spending
spending <-
  subset(
    nvs2018,
    select = c(HOTELCOST:SOUVCOST, OTHCOST)
  )
#spending[spending == "9999"] <- NA
spending <- data.frame(apply(spending, 2, as.numeric))
#spending[is.na(spending)] <- ""
range(spending, na.rm = TRUE)

spending$spendTotal <- rowSums (spending, na.rm = TRUE)
str(spending$spendTotal)
spending$spendTotal <- sum(spending)




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
  "Other"
)

spendingTitle <- "Spending"

#spendingTable$high <- round_df(spendingTable$high, 0)
#spendingTable[with(spendingTable, order(-high)), ] %>% select (Item, high)

#### WTP
# wtpTitle <- "If your total trip costs were to increase, what is the maximum extra amount you would pay and still visit this refuge?"


## Other Cost
otherCost <- nvs2018$OTHERCOSTTXT
otherCost <- otherCost[!is.na(nvs2018$OTHERCOSTTXT)]
otherCost

