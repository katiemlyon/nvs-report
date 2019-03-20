## @knitr altTrans

nvs2018 <- read.csv("data/nvs2018.csv")
library(dplyr)
#library(tidyr)
library(likert)
library(grid)


#library(waffle)
#library(extrafont)
#library(ggplot2)

#####Alternative Transportation#
altTransTitle <- "How likely visitors would be to use each transportation option at this refuge if it were available in the future."


altTransItems <- subset(nvs2018, select = c(BUSTRAM:PEDPATH))
altTransItems[altTransItems=="9"] <- NA
#altTransItems <- mutate_if(altTransItems, is.numeric, as.factor)
altTransItems <- data.frame(apply(altTransItems, 2, as.factor))
str(altTransItems)

names(altTransItems) = c(
  "Bus or tram that takes passengers to different points within refuge boundaries",
  "Bus or tram that provides a guided tour",
  "Refuge-sponsored shuttle with a dedicated stop in the local community for picking up people at set times",
  "Public transit systems that stops at or near this refuge",
  "Bike-share program that offers bicycles for rent on or near this refuge",
  "Pedestrian paths for access to this refuge from the local community"
)

likelylevels <- c('Not at all likely',
               'Slightly likely',
               'Moderately likely',
               'Very likely',
               'Extremely likely')

tryCatch({
  # This will throw an error because all the altTransItems must have the same number of levels.
  lbad <- likert(altTransItems)
}, error=function(e) { 
  print("This is good that an error was thrown!")
  print(e) 
})

sapply(altTransItems, class) #Verify that all the columns are indeed factors
sapply(altTransItems, function(x) { length(levels(x)) } ) # The number of levels in each factor

# recode each factor and explicitly set the levels
for(i in seq_along(altTransItems)) {
  altTransItems[,i] <- factor(altTransItems[,i], levels = likelylevels)
}

altTrans <- likert(altTransItems)
altTrans
summary(altTrans)

altTransTable <- summary(altTrans)

str(altTransTable)

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

altTransTable$high <- round_df(altTransTable$high, 0)

altTransTable[with(altTransTable, order(-high)), ] %>% select (Item, high)

plot(altTrans) + ggtitle(altTransTitle)
plot(altTrans, centered=TRUE)
