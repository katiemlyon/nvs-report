## @knitr futvisit

nvs2018 <- read.csv("data/nvs2018.csv")
require(dplyr)
require(likert)


###################################################

# Recreation Demand
demandTitle <- "Visitors’ beliefs about how changes to the refuge would impact participation in their primary activity at this refuge (n=###)"
#demand <- demand[rowSums(is.na(demand[,1:11]))!=11,] #remove the rows with NA in first 11 columns

demandItems <- subset(nvs2018, select = c(LESSWAT:HABQUAL))
#demandItems <- subset(demand, select = c(LESSWAT:HABQUAL))
demandItems[demandItems=="9"] <- NA
demandItems <- data.frame(apply(demandItems, 2, as.factor))
str(demandItems)


names(demandItems) = c(
  "Less water in lakes, rivers, or streams available for recreation",
  "More acreage open to hunting and fishing",
  "More infrastructure (for example, bathrooms, observation decks)",
  "Recreation equipment available for rent (for example, fishing poles, binoculars, snowshoes)",
  "Less regulations on fishing",
  "Less regulations on hunting",
  "A greater diversity of species",
  "Fewer number of a single, preferred species",
  "More people participating in my primary activity",
  "An improvement in the quality of wetlands",
  "An improvement in the quality of wildlife habitat other than wetlands"
)

demandlevels <- c("Decrease", "Stay the same", "Increase")

# recode each factor and explicitly set the levels
for(i in seq_along(demandItems)) {
  demandItems[,i] <- factor(demandItems[,i], levels = demandlevels)
}
demandProp <- likert(demandItems)
demandProp
plot(demandProp, centered=TRUE)

## With only one varaible by primary activity
futWater <- likert(demandItems[,1, drop=FALSE], grouping=nvs2018$PRIMACTCODED)
plot(futWater)

futAcres <- likert(demandItems[,2, drop=FALSE], grouping=nvs2018$PRIMACTCODED)
plot(futAcres)

futInfastr <- likert(demandItems[,3, drop=FALSE], grouping=nvs2018$PRIMACTCODED)
plot(futInfastr)

futEquip <- likert(demandItems[,4, drop=FALSE], grouping=nvs2018$PRIMACTCODED)
plot(futEquip)
futFishReg <- likert(demandItems[,5, drop=FALSE], grouping=nvs2018$PRIMACTCODED)
plot(futFishReg)
futHuntReg <- likert(demandItems[,6, drop=FALSE], grouping=nvs2018$PRIMACTCODED)
plot(futHuntReg)
futDiversity <- likert(demandItems[,7, drop=FALSE], grouping=nvs2018$PRIMACTCODED)
plot(futDiversity)
futDiversity <- likert(demandItems[,8, drop=FALSE], grouping=nvs2018$PRIMACTCODED)
plot(futDiversity)
futPeople <- likert(demandItems[,9, drop=FALSE], grouping=nvs2018$PRIMACTCODED)
plot(futPeople)
futWetlands <- likert(demandItems[,10, drop=FALSE], grouping=nvs2018$PRIMACTCODED)
plot(futWetlands)
futHabitat <- likert(demandItems[,11, drop=FALSE], grouping=nvs2018$PRIMACTCODED)
plot(futHabitat)

demandTable <- summary(demandProp)
str(demandTable)

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}


#demandTable$high <- format(demandTable$high, digits = 0)
demandTable$high <- round_df(demandTable$high, 0)

# sort
demandTable[with(demandTable, order(-high)), ] %>% dplyr::select (Item, high)

#################

# Look at demand by top two primary activities
demand2Items <- subset(nvsPrimAct, select = c(LESSWAT:HABQUAL))
demand2Items[demand2Items=="9"] <- NA
demand2Items <- data.frame(apply(demand2Items, 2, as.factor))
str(demand2Items)


names(demand2Items) = c(
  "Less water in lakes, rivers, or streams available for recreation",
  "More acreage open to hunting and fishing",
  "More infrastructure (for example, bathrooms, observation decks)",
  "Recreation equipment available for rent (for example, fishing poles, binoculars, snowshoes)",
  "Less regulations on fishing",
  "Less regulations on hunting",
  "A greater diversity of species",
  "Fewer number of a single, preferred species",
  "More people participating in my primary activity",
  "An improvement in the quality of wetlands",
  "An improvement in the quality of wildlife habitat other than wetlands"
)

demandlevels <- c("Decrease", "Stay the same", "Increase")

# recode each factor and explicitly set the levels
for(i in seq_along(demand2Items)) {
  demand2Items[,i] <- factor(demand2Items[,i], levels = demandlevels)
}

##### Grouped by top 2 activities
l29g <- likert(demand2Items, grouping=nvsPrimAct$PRIMACTCODED)
summary(l29g)

# Plots
plot(l29g)

# Density plots
plot(l29g, type='density', legend='Primary Activity')

## With only one varaible
lone <- likert(demand2Items[,2, drop=FALSE])
plot(lone)

loneg <- likert(demand2Items[,2, drop=FALSE], grouping=nvsPrimAct$PRIMACTCODED)
plot(loneg)
