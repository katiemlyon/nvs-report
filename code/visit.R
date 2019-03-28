## @knitr visit

nvs2018 <- read.csv("data/nvs2018.csv")

library(plyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(likert)
#library(raster)
library(tidyverse)
#library(waffle)

# function to round percentages to whole number
source("code/functions/round_df.R")

#################

## activities

act12 <-
  subset(nvs2018,
         select = c(WILDOB:SPEVACT))
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
for (i in seq_along(act12)) {
  act12[, i] <- factor(act12[, i], levels = dichotlevels)
}
act12Prop <- likert(act12)
act12Prop
act12Title <- "Activities"

act12Table <- summary(act12Prop)
str(act12Table)

act12Table$high <- round_df(act12Table$high, 0)

act12Table[with(act12Table, order(-high)),] %>%
  dplyr::select (Item, high)

## Primary activity
# subset primary activity text
str(nvs2018$PRIMACTCODED)
nvs2018$PRIMACTCODED[nvs2018$PRIMACTCODED == "999"] <- NA

primact <- na.omit(nvs2018$PRIMACTCODED)
primact <- data.frame(primact)
str(primact)

# Frequency Table
primactFreq <- table(primact)
primactFreq <- as.data.frame(primactFreq)
primactFreq <-
  primactFreq[order(primactFreq$Freq, decreasing = TRUE), ]
primactFreq # print table

primactProp <- prop.table(table(primact)) * 100 # cell percentages
primactProp <- round_df(primactProp) # cell percentages
primactProp <- as.data.frame(primactProp)
primactProp <-
  primactProp[order(primactProp$Freq, decreasing = TRUE), ]
primactProp

sum(primactProp$Freq)

# re-order levels
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x), decreasing = FALSE)))
}

primactGrp <- primact
primactGrp1 <- group_by(primact)
primactGrp2 <- dplyr::count(primact, primact)
primactGrp3 <- mutate(primactGrp2, highlight_flag = ifelse(primact == 'Hiking/Walking', T, F))


#primactGrp$primact <- as.character(primactGrp$primact)

# primary activity bar graph with highlighted bar
primactBar <- primactGrp3 %>%
  #group_by(primact) %>%
  #count(primact) %>%
  #mutate(highlight_flag = ifelse(primact == 'Hiking/Walking', T, F)) %>%
  ggplot(aes(x = fct_reorder(primact, n), y = n)) +
  geom_bar(aes(fill = highlight_flag), stat = 'identity') +
  scale_fill_manual(values = c('#595959', 'orange')) +
  coord_flip() +
  labs(x = 'Primary Activity'
       ,y = 'Number of Participants'
       ,title = str_c("Most visitors to this refuge "
                      , "\nparticipate in Hiking/Walking")
  ) +
  #theme with white background
  theme_bw() +
  theme(text = element_text(color = '#444444')
        ,plot.title = element_text(size = 18, face = 'bold')
        ,legend.position = 'none'
        ,axis.title = element_text(face = 'bold')
        ,axis.title.y = element_text(angle = 90, vjust = .5)
        #eliminates background, gridlines, and chart border
        ,plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
  )
primactBar

################################

## Visitor Center
summary(nvs2018$VISCEN)

#nvs2018["VISCEN"] <- lapply(nvs2018["VISCEN"] , factor)
vcAct <- subset(nvs2018, select = c(ASKINFO:OTHERVIS))
summary(vcAct)

#vcOther <- na.omit(nvs2018$OTHERVISTXT)
#vcOther <- sort(vcOther)
#vcOther

#############


#############
## Visits to Public Lands

# This NWR
nvs2018$REFLASTYR[nvs2018$REFLASTYR == "999"] <- NA
str(nvs2018$REFLASTYR)
nvs2018$REFLASTYR <- as.numeric(nvs2018$REFLASTYR)
refLast12 <- na.omit(nvs2018$REFLASTYR)
mean(refLast12)
summary(refLast12)
refLength = length(refLast12[refLast12])
refOne = round_df(length(refLast12[refLast12 == 1]) / refLength * 100)
refTwo = round_df(length(refLast12[refLast12 == 2]) / refLength * 100)

# Other NWRs
nvs2018$NWRLASTYR[nvs2018$NWRLASTYR == "999"] <- NA
str(nvs2018$NWRLASTYR)
nwrLast12 <- as.numeric(nvs2018$NWRLASTYR)
nwrLast12 <- na.omit(nwrLast12)
mean(nwrLast12)
summary(nwrLast12)
nwrLength = length(nwrLast12[nwrLast12])
nwrZero = round_df(length(nwrLast12[nwrLast12 == 0]) / nwrLength * 100)
nwrOne = round_df(length(nwrLast12[nwrLast12 == 1]) / nwrLength * 100)
nwrTwo = round_df(length(nwrLast12[nwrLast12 == 2]) / nwrLength * 100)

# Other Public Lands
nvs2018$OTHPUBLASTYR[nvs2018$OTHPUBLASTYR == "999"] <- NA
str(nvs2018$OTHPUBLASTYR)
nvs2018$OTHPUBLASTYR <- as.numeric(nvs2018$OTHPUBLASTYR)
othLast12 <- na.omit(nvs2018$OTHPUBLASTYR)
mean(othLast12)
summary(othLast12)
othLength = length(othLast12[othLast12])
othZero = round_df(length(othLast12[othLast12 == 0]) / othLength * 100)
othOne = round_df(length(othLast12[othLast12 == 1]) / othLength * 100)
othTwo = round_df(length(othLast12[othLast12 == 2]) / othLength * 100)

nvs2018 %>%
  dplyr::select(REFLASTYR:OTHPUBLASTYR) %>%
  na.omit -> visits
str(visits)
range(visits)

visits[visits > 10 & visits <= 20] <- "11-20"
visits[visits > 20 & visits <= 49] <- "21-49"
visits[visits > 50 & visits <= 99] <- "50-99"
visits[visits > 100 & visits <= 199] <- "100-199"
visits[visits > 200 & visits <= 299] <- "200-299"
visits[visits > 300 & visits <= 399] <- "300+"

names(visits) = c("This Refuge", "Other NWRs", "Other Public Lands")
str(visits)

visit_levels <- names(visits)[order(visits)]
str(visits)

###################
