# Group composition

#==============
# LOAD PACKAGES
#==============
library(ggplot2)
library(grid)
library(gridExtra)
library(tidyverse)
nvs2018 <- read.csv("data/nvs2018.csv")
group <- subset(nvs2018, select = c(ADULTNUM:MINORNUM))
group <- na.omit(group)
group <- as.data.frame(group)

#--------
# INSPECT
#--------
group %>% glimpse()
group %>% count(ADULTNUM)
group %>% count(MINORNUM)

# drop responses where Adult = 0
group <- group[ which(group$ADULTNUM!=0), ]
group %>% count(ADULTNUM)

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

#===============================
# BAR CHART WITH HIGHLIGHTED BAR
#===============================
group %>%
  group_by(ADULTNUM) %>%
  summarise(ADULTNUM_count = n()) %>%
  mutate(highlight_flag = ifelse(ADULTNUM == 1, T, F)) %>%
  ggplot(aes(x = fct_reorder(ADULTNUM, ADULTNUM_count), y = ADULTNUM_count)) +
  geom_bar(aes(fill = highlight_flag), stat = 'identity') +
  scale_fill_manual(values = c('#595959', 'orange')) +
  coord_flip() +
  labs(x = '# of Adults in Group'
       ,y = '# of Adults in Group'
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

tripPurpBar <- grid.arrange(textGrob("Primary Purpose of Trip to this Refuge",
                      gp = gpar(fontsize = 2.5*11, fontface = "bold")),
             tripPurpBar,
             heights = c(0.1, 1))

#######################

groupsize <- rowSums (group, na.rm = TRUE, dims = 1)
groupsize
range(groupsize) #OSU cleanup 43814

groupsizeProp <- prop.table(table(groupsize)) * 100
groupsizeProp
str(groupsizeProp)

single <- round_df(groupsizeProp["1"], 0)
single #percent that were alone

group <- data.frame(groupsizeProp) #convert to data frame
group <- subset(group, groupsize != "1") #groupsize greater than 1
group <-
  round_df(sum(group$Freq), 0) #add proportions for groups greater than 1
group #percent that were in a group

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

