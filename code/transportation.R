## @knitr transEval

# This is our external R script
# We're adding two chunks variablesXY and plotXY
library(likert)

##### Importance of transportation-related items
transImpTitle <- "Importance of transportation-related features when visiting this refuge"

transImpItems <- subset(nvs2018, select = c(ROADCONDIMP:ACCESSIMP))
transImpItems[transImpItems == "9"] <- NA
#transImpItems <- mutate_if(transImpItems, is.numeric, as.factor)
transImpItems <- data.frame(apply(transImpItems, 2, as.factor))
str(transImpItems)

names(transImpItems) = c(
  "Surface condition of refuge roads",
  "Surface conditions of parking areas",
  "Condition of bridges on roadways",
  "Condition of trails and boardwalks",
  "Condition of boat launches",
  "Number of places for parking",
  "Number of places to pull over on refuge roads",
  "Safety of driving conditions on refuge roads",
  "Safety of refuge road entrances/exits",
  "Safety of roads/trails for nonmotorized users",
  "Signs on highways directing you to this refuge",
  "Signs directing you around refuge roads",
  "Signs directing you on trails",
  "Access for people with physical disabilities or who have difficulty walking")


implevels <- c(
  'Not at all important',
  'Slightly important',
  'Moderately important',
  'Very important',
  'Extremely important'
)

tryCatch({
  # This will throw an error because all the transImpItems must have the same number of levels.
  lbad <- likert(transImpItems)
}, error = function(e) {
  print("This is good that an error was thrown!")
  print(e)
})

sapply(transImpItems, class) #Verify that all the columns are indeed factors
sapply(transImpItems, function(x) {
  length(levels(x))
}) # The number of levels in each factor

# Here we will recode each factor and explicitly set the levels
for (i in seq_along(transImpItems)) {
  transImpItems[, i] <- factor(transImpItems[,i], levels = implevels)
}

transImp <- likert(transImpItems)
transImp
summary(transImp)
plot(transImp) + ggtitle(transImpTitle)

##### Satisfaction with transportation items
transSatTitle <-
  "Rate how satisfied you are with the way this refuge is managing each feature"

transSatItems <- subset(nvs2018, select = c(ROADCONDSAT:ACCESSSAT))
transSatItems[transSatItems == "0"] <- NA
transSatItems[transSatItems == "9"] <- NA
#transSatItems <- mutate_if(transSatItems, is.numeric, as.factor)
transSatItems <- data.frame(apply(transSatItems, 2, as.factor))
str(transSatItems)

names(transSatItems) = c(
  "Surface condition of refuge roads",
  "Surface conditions of parking areas",
  "Condition of bridges on roadways",
  "Condition of trails and boardwalks",
  "Condition of boat launches",
  "Number of places for parking",
  "Number of places to pull over on refuge roads",
  "Safety of driving conditions on refuge roads",
  "Safety of refuge road entrances/exits",
  "Safety of roads/trails for nonmotorized users",
  "Signs on highways directing you to this refuge",
  "Signs directing you around refuge roads",
  "Signs directing you on trails",
  "Access for people with physical disabilities or who have difficulty walking")

satlevels <- c(
  'Not at all satisfied',
  'Slightly satisfied',
  'Moderately satisfied',
  'Very satisfied',
  'Extremely satisfied'
)

tryCatch({
  # This will throw an error because all the transSatItems must have the same number of levels.
  lbad <- likert(transSatItems)
}, error = function(e) {
  print("This is good that an error was thrown!")
  print(e)
})

sapply(transSatItems, class) #Verify that all the columns are indeed factors
sapply(transSatItems, function(x) {
  length(levels(x))
}) # The number of levels in each factor

# Here we will recode each factor and explicitly set the levels
for (i in seq_along(transSatItems)) {
  transSatItems[, i] <- factor(transSatItems[,i], levels = satlevels)
}

transSat <- likert(transSatItems)
transSat
summary(transSat)
plot(transSat)

########################

library(ggplot2)
library(ggalt)
library(scales)

# create table from summary statistics
transImpTable <- as.data.frame(summary(transImp))
str(transImpTable)

transSatTable <- summary(transSat)
str(transSatTable)

transMeans <- merge(transImpTable, transSatTable, by = "Item")
str(transMeans)

ggTrans <- ggplot(transMeans, aes(x=mean.x, 
                                  xend=mean.y, 
                                  y=Item, 
                                  group=Item)) + 
  geom_dumbbell(color = "light blue", 
                colour_x = "darkred",
                colour_xend = "darkBlue",
                size_x = 2.5,
                size_xend = 2.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(1,5))

plot(ggTrans)

########################

