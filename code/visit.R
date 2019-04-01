## @knitr visit

library(plyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(grid)
library(gridExtra)
library(likert)
library(tidyverse)

# function to round percentages to whole number
source("code/functions/round_df.R")

nvs2018 <- read.csv("data/nvs2018.csv")

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
# Trip Purpose

tripPurp <- na.omit(nvs2018$TRIPPURP)
tripPurp <- as.data.frame(tripPurp)
levels(tripPurp$tripPurp)
levels(tripPurp$tripPurp) <- c(
  "Incidental Stop",
  "One of Many",
  "Primary Purpose"
)

purpose <- prop.table(table(tripPurp))*100 # cell percentages
purpose <- round_df(purpose) # cell percentages
purpose <- as.data.frame(purpose)
purpose <- purpose[order(purpose$Freq, decreasing = TRUE),]
colnames(purpose) <- c("Purpose", "Proportion")
purpose

purpose1 <- purpose$Purpose[1]
purpose1 <- as.character(purpose1)
purpose1
purpose1Prop <- purpose$Proportion[1]
purpose2 <- purpose$Purpose[2]
purpose2 <- as.character(purpose2)
purpose2
purpose2Prop <- purpose$Proportion[2]
purpose3 <- purpose$Purpose[3]
purpose3 <- as.character(purpose3)
purpose3
purpose3Prop <- purpose$Proportion[3]

#=========================================
# BAR CHART WITH HIGHLIGHTED BAR - PERCENT
#=========================================
tripPurpBar <- purpose %>%
  mutate(highlight_flag = ifelse(Purpose == 'Primary Purpose', T, F)) %>%
  ggplot(aes(x = Purpose, y = Proportion)) +
  geom_bar(stat = "identity", aes(fill = highlight_flag)) +
  scale_fill_manual(values = c('#595959', '#d95f02')) +
  coord_flip() +
  labs(x = 'Primary Purpose'
       ,y = 'Percent of Respondents'
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

grid.arrange(textGrob("Type of Trip to this Refuge",
                                      gp = gpar(fontsize = 2.5*11, fontface = "bold")),
                             tripPurpBar,
                             heights = c(0.1, 1))


##############################################
# Group Size
##############################################

group <- subset(nvs2018, select = c(ADULTNUM:MINORNUM))
group <- na.omit(group)
group <- as.data.frame(group)

# Adults
str(group$ADULTNUM)
range(group$ADULTNUM)
# Minors
str(group$MINORNUM)
range(group$MINORNUM)

# drop responses where Adult = 0
group <- group[ which(group$ADULTNUM!=0), ]
group %>% glimpse()

groupsize <- rowSums (group, na.rm = TRUE, dims = 1)
groupsize
range(groupsize)

groupmean <- round_df(mean(groupsize), 0)
groupmean #average group size

groupsizeProp <- prop.table(table(groupsize)) * 100
groupsizeProp
range(groupsizeProp)

singleProp <- data.frame(groupsizeProp) #convert to data frame
singleProp <- subset(singleProp, groupsize == "1") #groupsize equals 1
singleProp <- round_df(sum(singleProp$Freq), 0) #get proportions
singleProp #percent that were alone

groupProp <- data.frame(groupsizeProp) #convert to data frame
groupProp <- subset(groupProp, groupsize != "1") #groupsize greater than 1
groupProp <-
  round_df(sum(groupProp$Freq), 0) #add proportions for groups greater than 1
groupProp #percent that were in a group

adults <- prop.table(table(nvs2018$ADULTNUM)) * 100
adults

# Group composition - Locals

localgrp <- subset(nvs2018, LOCALAREA == "Local",
                   select = ADULTNUM:MINORNUM)
str(localgrp)
localgrp <- na.omit(localgrp)
range(localgrp$ADULTNUM)
range(localgrp)

localgrp$grpSize <- rowSums (localgrp, na.rm = TRUE)
range(localgrp$grpSize)
localgrpSize <- round_df(mean(localgrp$grpSize))
localgrpSize

# Group composition - Nonlocals

nonlocgrp <- subset(nvs2018, LOCALAREA == "Nonlocal",
                    select = ADULTNUM:MINORNUM)
head(nonlocgrp)
nonlocgrpSize <- rowSums (nonlocgrp, na.rm = TRUE)
range(nonlocgrpSize)
nonlocgrpSize <- subset(nonlocgrpSize, nonlocgrpSize < 999)
range(nonlocgrpSize)
nonlocgrpSize <- round_df(mean(nonlocgrpSize), 0)
nonlocgrpSize


##############################################
# Visits to Public Lands
##############################################

# This NWR
nvs2018$REFLASTYR[nvs2018$REFLASTYR == "999"] <- NA
str(nvs2018$REFLASTYR)
nvs2018$REFLASTYR <- as.numeric(nvs2018$REFLASTYR)
refLast12 <- na.omit(nvs2018$REFLASTYR) #remove missing
refVisitsLast12 <- round_df(mean(refLast12), 0)
refVisitsLast12 #average visits to this refuge last 12 months

summary(refLast12)
refLength <- length(refLast12[refLast12])
refOne <- round_df(length(refLast12[refLast12 == 1]) / refLength * 100) #percent first time visitors
refTwo <- round_df(length(refLast12[refLast12 == 2]) / refLength * 100)
repeatVis <- round_df(length(refLast12[refLast12 >= 2]) / refLength * 100) #percent repeat visitors

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
