## @knitr primact

nvs2018 <- read.csv("data/nvs2018.csv")

#### Primary Activity
require(likert)
library(plyr)
library(dplyr)

## subset primary activity text
# omit NA
primact <- na.omit(nvs2018$PRIMACTCODED)
primact <- as.data.frame(primact)

# sort by name
#primact <- primact[sort(primact$PRIMACTCODED),]

# Frequencies
primactFreq <- table(primact)
primactFreq <- as.data.frame(primactFreq)
primactFreq <- primactFreq[order(primactFreq$Freq, decreasing = TRUE),]
primactFreq # print table

# Percentages
primactProp <- prop.table(primactFreq)*100 # cell percentages
primactProp <- round_df(primactProp) # cell percentages
primactProp <- as.data.frame(primactProp)
primactProp <- primactProp[order(primactProp$Freq, decreasing = TRUE),]
primactProp

sum(primactProp$Freq)

#############

primact %>%
  group_by(PRIMACTCODED) %>%
  mutate(my_ranks = order(order(PRIMACTCODED, decreasing=TRUE)))

by_act <- primact %>% arrange(PRIMACTCODED) %>%
  #group_by(PRIMACTCODED) %>%
  mutate(rank = rank(PRIMACTCODED, ties.method = "first"))

by_act %>% filter(rank <= 3)

#############

# re-order levels
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x), decreasing = FALSE)))
}

#############

primactBar <- ggplot(data=subset(nvs2018, !is.na(primact)), aes(x = reorder_size(`PRIMACTCODED`))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Primary Activity") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  coord_flip()


## Primary Activity by Age

ggplot(data=subset(nvs2018, !is.na(AGECAT)), aes(x = reorder_size(`PRIMACTCODED`))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("PRIMACTCODED") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  facet_grid(~ AGECAT) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(data=subset(nvs2018, !is.na(AGECAT)), aes(x = reorder_size(`AGECAT`))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Age") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  facet_grid(~ PRIMACTCODED == "fishing") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#############

## Top 2 Primary Activities
#library(ggrepel)
#library(tidyverse)

# top 2 activities
primact2 <- nvs2018 %>%
  count(PRIMACTCODED) %>%
  top_n(2) %>%
  arrange(n, PRIMACTCODED) %>%
  mutate(PRIMACTCODED = factor(PRIMACTCODED, levels = unique(PRIMACTCODED)))

## Subset top two primary activities

# calculate frequencies
tab <- table(nvs2018$PRIMACTCODED)
# sort
tab_s <- sort(tab)
# extract 2 most frequent
top2 <- tail(names(tab_s), 2)
# subset of data frame
nvsPrimAct <- subset(nvs2018, PRIMACTCODED %in% top2)
# order factor levels
nvsPrimAct$PRIMACTCODED <- factor(nvsPrimAct$PRIMACTCODED, levels = rev(top2))

#===============================
# BAR CHART WITH HIGHLIGHTED BAR
#===============================
primactBar <- primact %>%
  group_by(primact) %>%
  summarise(primact_count = n()) %>%
  mutate(highlight_flag = ifelse(primact == 'Hiking/Walking', T, F)) %>%
  ggplot(aes(x = fct_reorder(primact, primact_count), y = primact_count)) +
  geom_bar(aes(fill = highlight_flag), stat = 'identity') +
  scale_fill_manual(values = c('#595959', 'orange')) +
  coord_flip() +
  labs(x = 'Primary Activity'
       ,y = 'Number of Participants'
       ,title = str_c("Most visitors to this refuge "
                      , "\nparticipate in Hiking/Walking")
  ) +
  theme_bw() +
  theme(text = element_text(color = '#444444')
        ,plot.title = element_text(size = 18, face = 'bold')
        ,legend.position = 'none'
        ,axis.title = element_text(face = 'bold')
        ,axis.title.y = element_text(angle = 90, vjust = .5)
        ,plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
  )
primactBar
