#### Primary activity
library(plyr)
library(dplyr)

## subset primary activity text
primact <- subset(nvs2018, select = c(PRIMACTCODED))
primact[primact=="999"] <- NA

# remove rows where NA"
primact <- primact[!(is.na(primact$PRIMACTCODEDCODED)),]

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

## Primary Activity by Age
# re-order levels
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}
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

