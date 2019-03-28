# Season

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
