## @knitr safe
require(likert)
library(dplyr)
library(gridExtra)
library(reshape2)

#nvs2018 <- read.csv("data/nvs2018.csv")
df <- subset(nvs2018, select = c(FELTWEL:TREATDIF, GENDER, AGECAT, WHITE:OTHERETH))
df <- na.omit(df)


##### Safe and Welcome
safeTitle <- "Safe and Welcome"

safeItems <- subset(df, select = c(FELTWEL:TREATDIF))
str(safeItems)
#safeItems[safeItems=="9"] <- NA
#safeItems <- droplevels(safeItems, exclude = "")
#safeItems <- data.frame(apply(safeItems, 2, as.factor))

agreeLevels <- c('Strongly disagree',
                 'Disagree',
                 'Neither',
                 'Agree',
                 'Strongly agree')

names(safeItems) = c(
  "I felt welcome during my visit to this refuge.", 
  "I felt safe during my visit to this refuge.", 
  "Crime is a problem at this refuge.", 
  "I feel comfortable being in nature.", 
  "I do not like being in nature by myself.", 
  "People closest to me enjoy participating in nature-based recreation.", 
  "Generally, people who look like me are treated differently when they participate in nature-based recreation."
)

tryCatch({
  # This will throw an error because all the items must have the same number of levels.
  lbad <- likert(safeItems)
}, error=function(e) {
  print("This is good that an error was thrown!")
  print(e)
})
sapply(safeItems, class) #Verify that all the columns are indeed factors
sapply(safeItems, function(x) { length(levels(x)) } ) # The number of levels in each factor
for(i in seq_along(safeItems)) {
  safeItems[,i] <- factor(safeItems[,i], levels=agreeLevels)
}

safe <- likert(safeItems)
safe
plot(safe)
plot(safe, centered=TRUE) + ggtitle(safeTitle)
plot(safe, group.order=names(safeItems))


################################

##### Safe and Welcome Subset
#title <- "Safe and Welcome"
safwelItems <- subset(df, select = c(FELTWEL:CRIMEPROB))
#safwelItems <- droplevels(safwelItems, exclude = "")
#safwelItems[safwelItems == "9"] <- NA
safwelItems <- data.frame(apply(safwelItems, 2, as.factor))
levels(safwelItems$FELTWEL)

names(safwelItems) = c(
  "I felt welcome during my visit to this refuge.",
  "I felt safe during my visit to this refuge.",
  "Crime is a problem at this refuge."
)

tryCatch({
  # This will throw an error because all the items must have the same number of levels.
  lbad <- likert(safwelItems)
}, error=function(e) {
  print("This is good that an error was thrown!")
  print(e)
})
sapply(safwelItems, class) #Verify that all the columns are indeed factors
sapply(safwelItems, function(x) { length(levels(x)) } ) # The number of levels in each factor
for(i in seq_along(safwelItems)) {
  safwelItems[,i] <- factor(safwelItems[,i], levels=agreeLevels)
}

safeWelcome <- likert(safwelItems)
safeWelcome
plot(safeWelcome)
plot(safeWelcome, group.order=names(safwelItems))

plot(safeWelcome, centered=TRUE) + ggtitle(safeTitle)

################################

#### Comfort in Nature
comfortTitle <- "Comfort in Nature"

comfortItems <- subset(df, select = c(COMFORT:TREATDIF))
#comfortItems[comfortItems == "9"] <- NA
#comfortItems <- droplevels(comfortItems, exclude = "")
comfortItems <- data.frame(apply(comfortItems, 2, as.factor))

str(comfortItems)

names(comfortItems) = c(
  "I feel comfortable being in nature.",
  "I do not like being in nature by myself.",
  "People closest to me enjoy participating in nature-based recreation.",
  "Generally, people who look like me are treated differently when they participate in nature-based recreation."
)

tryCatch({
  # This will throw an error because all the items must have the same number of levels.
  lbad <- likert(comfortItems)
}, error=function(e) {
  print("This is good that an error was thrown!")
  print(e)
})
sapply(comfortItems, class) #Verify that all the columns are indeed factors
sapply(comfortItems, function(x) { length(levels(x)) } ) # The number of levels in each factor
for(i in seq_along(comfortItems)) {
  comfortItems[,i] <- factor(comfortItems[,i], levels=agreeLevels)
}

comfort <- likert(comfortItems)
comfort
plot(comfort)

################################

# Group by gender
genderSafe <- likert(safeItems, grouping = df$GENDER)
str(genderSafe)
plot(genderSafe)

# Group by age
ageSafe <- likert(safeItems, grouping = df$AGECAT)
plot(ageSafe)

ageComfort <- likert(comfortItems, grouping = df$AGECAT)
plot(ageComfort)

# Group by race and ethnicity
hispanicSafe <- likert(safeItems, grouping = df$HISPANIC)
plot(hispanicSafe)

whiteSafe <- likert(safeItems, grouping = df$WHITE)
plot(whiteSafe)

blackSafe <- likert(safeItems, grouping = df$AFRAMER)
plot(blackSafe)

asianSafe <- likert(safeItems, grouping = df$ASIAN)
plot(asianSafe)

