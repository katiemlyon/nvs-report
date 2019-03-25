## @knitr activities

nvs2018 <- read.csv("data/nvs2018.csv")
library(dplyr)
library(likert)
library(reshape2)
library(ggplot2)

#library(waffle)
#library(extrafont)
#library(tidyr)

## activities

act12 <-
  subset(
    nvs2018,
    select = c(WILDOB:SPEVACT)
  )
str(act12)

#act12[act12 == "0"] <- NA
#act12[act12 == "9"] <- NA
#act12 <- data.frame(apply(act12, 2, as.factor))


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

####################

# bar plots
act12 <-
  subset(
    nvs2018,
    select = c(WILDOB:SPEVACT)
  )
act12 <- na.omit(act12)
str(act12)
### convert to 'long form'
df <- melt(act12, measure.vars = names(act12))

df2 <- as.matrix(act12Table)
str(df2)

qplot(variable, data = df2, fill = Yes)

df2 <- arrange(act12Prop, Yes)
df2$Item <- factor(df2$Item, levels = df2$Item)
ggplot(df2, aes(Item, Yes, fill = Item)) + geom_col() + coord_flip() +
  scale_fill_brewer(palette="Spectral")


c <- ggplot(data.frame(test.2.A.percents), aes(x = factor(name), y = percent))
c + geom_bar(position = "identity", stat = "identity")
