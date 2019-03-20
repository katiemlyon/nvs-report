nvs2018 <- read.csv("../data/nvs2018.csv")
library(dplyr)
#library(tidyr)
library(likert)
#library(waffle)
#library(extrafont)
#library(ggplot2)

## activities

act12 <-
  subset(
    nvs2018,
    select = c(WILDOB:SPEVACT)
  )
#act12[act12 == "0"] <- NA
act12[act12 == "9"] <- NA
act12 <- data.frame(apply(act12, 2, as.factor))
str(act12)

names(act12) = c(
  "Wildlife observation",
  "Bird watching",
  "Photography",
  "Big game hunting",
  "Upland/Small game hunting",
  "Waterfowl/Migratory bird hunting",
  "Freshwater fishing",
  "Saltwater fishing",
  "Hiking/Walking",
  "Jogging/Running/Exercising",
  "Bicycling",
  "Auto tour route/Driving",
  "Motorized boating",
  "Nonmotorized boating",
  "Foraging",
  "Picnicking",
  "Volunteering",
  "Environmental education",
  "Interpretative program",
  "Refuge special event"
)


dichotlevels <- c('No', 'Yes')

# recode each factor and explicitly set the levels
for(i in seq_along(act12)) {
  act12[,i] <- factor(act12[,i], levels = dichotlevels)
}
act12Prop <- likert(act12)
act12Prop
act12Title <- "Activities"

act12Table <- summary(act12Prop)
str(act12Table)

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

act12Table$high <- round_df(act12Table$high, 0)

act12Table[with(act12Table, order(-high)), ] %>% select (Item, high)