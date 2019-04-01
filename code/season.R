# Season
library(likert)
season <-
  subset(nvs2018, select =  c(SPRVIS, SUMVIS, FALLVIS, WINTVIS))
str(season)
season$SPRVIS <- as.factor(season$SPRVIS)
season$SUMVIS <- as.factor(season$SUMVIS)
season$FALLVIS <- as.factor(season$FALLVIS)
season$WINTVIS <- as.factor(season$WINTVIS)
seasonL <- likert(season)
seasonT <- summary(season)
str(seasonT)
seasonT


nvs2018 %>%
  select(SPRVIS:WINTVIS) %>%
  na.omit -> season
str(season)

names(season) = c("Spring", "Summer", "Fall", "Winter")
str(season)

spring

season_levels <- names(season)
season_levels  # notice the changed order of factor levels

season %>%
  gather(key = items, value = answer) %>%
  mutate(answer = factor(answer),
         items = factor(items)) -> season2

season2$items <- factor(season2$items, levels = season_levels)

seasonBar <- ggplot(season2, aes(x = items)) +
  geom_bar(aes(fill = answer), position = "fill", show.legend = FALSE)+
  coord_flip() +
  scale_x_discrete(limits = rev(levels(season2$items))) +
  labs(y = "Percent Visited in Last 12 Months",
       x = "Season") +
  scale_fill_brewer(palette = "Greens") +
  scale_color_manual()

seasonBar

########
nvs2018 %>%
  select(SPRVIS, SUMVIS, FALLVIS, WINTVIS) %>%
  na.omit()

library(ggplot2)
library(grid)
library(gridExtra)
library(tidyverse)
season2 <- subset(nvs2018, select = c(SPRVIS, SUMVIS, FALLVIS, WINTVIS))
season2 <- na.omit(season2)
season2 <- as.data.frame(season2)

#--------
# INSPECT
#--------
season2 %>% glimpse()
spring <- season2 %>% count(SPRVIS)
summer <- season2 %>% count(SUMVIS)
fall <- season2 %>% count(FALLVIS)
winter <- season2 %>% count(WINTVIS)

seasonProp <- prop.table(table(season2))*100 # cell percentages
seasonProp <- round_df(seasonProp) # cell percentages
seasonProp <- as.data.frame(seasonProp)
seasonProp <- seasonProp[order(seasonProp$Freq, decreasing = TRUE),]
colnames(seasonProp) <- c("Spring", "Summer", "Fall", "Winter", "Proportion")
seasonProp

purpose1 <- purpose$Purpose[1]
purpose1Prop <- purpose$Proportion[1]
purpose2 <- purpose$Purpose[2]
purpose2Prop <- purpose$Proportion[2]
purpose3 <- purpose$Purpose[3]
purpose3Prop <- purpose$Proportion[3]

nvs2018 %>%
  select(LOCALAREA, season2) %>%
  na.omit() %>%
  group_by(LOCALAREA, season2) %>%
  summarise(ptg = n()) %>%
  mutate(ptg = prop.table(ptg)*100)
