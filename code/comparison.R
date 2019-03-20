
nvs2018 <- read.csv("data/nvs2018.csv")
library(magrittr)
library(plyr)
library(dplyr)
library(ggplot2)
library(likert)

nvs2018$PRIMACTCODED[nvs2018$PRIMACTCODED == "999"] <- NA
table(nvs2018$PRIMACTCODED)

# create new activity variable with refined categories
#attach(nvs2018)
#nvs2018$ACTIVITY <- NA
#nvs2018$ACTIVITY[PRIMACTCODED=='hiking'] <- "Hiking/Walking"
#nvs2018$ACTIVITY[PRIMACTCODED=='fishing'] <- "Fishing"
#nvs2018$ACTIVITY[PRIMACTCODED=='hunting'] <- "Hunting"
#nvs2018$ACTIVITY[PRIMACTCODED =='wildlife observation'] <- "Wildlife observation"
#nvs2018$ACTIVITY[PRIMACTCODED=='fishing' | PRIMACTCODED=='hunting'] <- "Fishing/Hunting"
#detach(nvs2018)
#table(nvs2018$ACTIVITY)


###################################################

# Recreation Demand
demand <- subset(nvs2018, select = c(LESSWAT:HABQUAL, PRIMACTCODED, AGECAT))
demand[demand == "9" | demand == "999"] <- NA
sum(rowSums(is.na(demand[,1:11]))==11) #number of people skipped
demand <- demand[rowSums(is.na(demand[,1:11]))!=11,] #remove the rows with NA in first 11 columns

demand <- subset(demand, !is.na(PRIMACTCODED)) #remove rows with null activity data

demandItems <- subset(demand, select = c(LESSWAT:HABQUAL))
demandItems[demandItems=="0"] <- NA #OSU data cleaning
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
demandTitle <- "Visitors’ beliefs about how changes to the refuge would impact participation in their primary activity at this refuge (n=###)"
#plot(demandProp, centered=FALSE) + ggtitle(demandTitle)

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

demandTable$high <- round_df(demandTable$high, 0)

demandTable[with(demandTable, order(-high)), ] %>% select (Item, high)

# compare activity types
demandPlot <- ggplot(data = demand, aes(x=demandItems, fill=ACTIVITY)) + 
  geom_bar() +  # Barplot
  facet_grid(demandItems ~ ACTIVITY)   # wrap up everything to showcase by multiple cols


demand[1:11] <- lapply(demand[1:11], factor, levels=demandlevels)
likt <- likert(demand[,c(1:11)])
plot(likt)





demandLikert <- likert(demandItems, grouping=demand$ACTIVITY)
str(demandLikert$results)

#Make a summary from results, grouped by the variable in column 1 of results:
demandGroup <- likert(summary = l29g$results, grouping = l29g$results[,1])
str(demandGroup)

demandPlot <- likert(demandItems, grouping=demand$ACTIVITY)

str(demandPlot$results)

knitr::opts_chunk$set(fig.height = knitr::opts_chunk$get('fig.height')*2)

plot(demandPlot) + ggtitle(demandTitle)



##############################
#Grouped by activity:
title <- "Visitors’ beliefs about how changes to the refuge would impact participation in their primary activity at this refuge (n=###)"

items <- subset(demand, select = c(LESSWAT:HABQUAL))
#items[items=="9"] <- NA
items <- data.frame(apply(items, 2, as.factor))
head(items); ncol(items)

names(items) = c(
  "Less water", 
  "More acreage hunt/fish", 
  "More infrastructure", 
  "Rental equipment)", 
  "Less regulations on fishing", 
  "Less regulations on hunting", 
  "A greater diversity of species", 
  "Fewer number of a single, preferred species", 
  "More people participating in my primary activity", 
  "Improve quality of wetlands",
  "Improve quality of habitat other than wetlands",
  "Activity",
  "Age Category"
)

#levels(items) <- c("Decrease", "Stay the same", "Increase")

itemsLikert <- likert(items)
str(itemsLikert)

itemsLikertSum <- likert(summary = itemsLikert$results)

str(itemsLikertSum)
summary(itemsLikertSum)

## Scale figure height and width, and reduce size of legend.text for plots:

scale_height = knitr::opts_chunk$get('fig.height')*0.5
scale_width = knitr::opts_chunk$get('fig.width')*1.25
knitr::opts_chunk$set(fig.height = scale_height, fig.width = scale_width)

theme_update(legend.text = element_text(size = rel(0.7)))

# Plots
plot(itemsLikertSum) + ggtitle(title)

plot(itemsLikertSum, centered=FALSE) + ggtitle(title)
#plot(itemsLikertSum, include.center=FALSE) + ggtitle(title)

plot(itemsLikertSum, center=2) + ggtitle(title)


# Heat
plot(itemsLikertSum, type = 'heat') + ggtitle(title) + 
  theme(legend.position = 'none')

# scale_y_discrete(labels = function(x) stringer::str_wrap(x, width = 15))
# to wrap labels would sort `Mean (SD)` column out of order.
# given values, legend is removed to make space for axes labels

# Density plots on likert summary results are not supported.
# plot(l29s, type='density') + ggtitle(title)
# plot(l29s, type='density', facet=FALSE, legend='Material') + ggtitle(title)

# Grouped Summary results
l29g <- likert(items, grouping=demand$ACTIVITY)
str(l29g$results)

#Make a summary from results, grouped by the variable in column 1 of results:
l29gs <- likert(summary = l29g$results, grouping = l29g$results[,1])
str(l29gs)

## Rescale figures to full height:
#knitr::opts_chunk$set(fig.height = knitr::opts_chunk$get('fig.height')*2)

# Grouped plots
plot(l29gs) + ggtitle(title)
plot(l29gs, centered=FALSE) + ggtitle(title)

plot(itemsLikertSum) + ggtitle(title)
