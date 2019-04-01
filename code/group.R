## @knitr group
# Group composition

#==============
# LOAD PACKAGES
#==============
library(ggplot2)
library(grid)
library(gridExtra)
library(tidyverse)
source("code/functions/round_df.R")

nvs2018 <- read.csv("data/nvs2018.csv")

group <- subset(nvs2018, select = c(ADULTNUM:MINORNUM))
group <- na.omit(group)
group <- as.data.frame(group)


# Adults
str(group$ADULTNUM)
range(group$ADULTNUM)
# Minors
str(group$MINORNUM)
range(group$MINORNUM)

#--------
# INSPECT
#--------
group %>% glimpse()
group %>% count()

# drop responses where Adult = 0
group <- group[ which(group$ADULTNUM!=0), ]
group %>% glimpse()

#######################

groupsize <- rowSums (group, na.rm = TRUE, dims = 1)
groupsize
range(groupsize)

groupmean <- single <- round_df(mean(groupsize), 2)
groupmean #average group size

groupsizeProp <- prop.table(table(groupsize)) * 100
groupsizeProp
range(groupsizeProp)

single <- round_df(groupsizeProp["1"], 0)
single #percent that were alone

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
nonlocgrpSize <- round_df(mean(nonlocgrpSize))
nonlocgrpSize


#=================
# SIMPLE BAR CHART
#=================
ggplot(data = group, aes(x = ADULTNUM)) +
  geom_bar()

ggplot(data = group, aes(x = MINORNUM)) +
  geom_bar()

#===========================================
# SIMPLE BAR CHART (USING THE PIPE OPERATOR)
#===========================================

# Number of Adults
group %>%
  ggplot(aes(x = ADULTNUM)) +
  geom_bar()


gg <- ggplot(data = group, aes(x=ADULTNUM))

ggAdult <- gg +
  geom_bar(data=group,
           aes( y = ..count../sum(..count..), fill = ADULTNUM)) +
  coord_flip() +
  theme_bw()
ggAdult


# Number of Minors
group %>%
  ggplot(aes(x = MINORNUM)) +
  geom_bar()

ggMinor <- gg +
  geom_bar(data=group,
           aes( y = ..count../sum(..count..), fill = MINORNUM)) +
  coord_flip() +
  theme_bw()
ggMinor

## Plutting it together
ggGroup <- grid.arrange(ggAdult,
                        ggMinor
)

