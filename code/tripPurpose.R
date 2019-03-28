#==============
# LOAD PACKAGES
#==============
library(ggplot2)
library(grid)
library(gridExtra)
library(tidyverse)
tripPurp <- na.omit(nvs2018$TRIPPURP)
tripPurp <- as.data.frame(tripPurp)

#--------
# INSPECT
#--------
tripPurp %>% glimpse()
tripPurp %>% count(tripPurp)

#=================
# SIMPLE BAR CHART
#=================
ggplot(data = tripPurp, aes(x = tripPurp)) +
  geom_bar()

#===========================================
# SIMPLE BAR CHART (USING THE PIPE OPERATOR)
#===========================================
tripPurp %>%
  ggplot(aes(x = tripPurp)) +
  geom_bar()

#===============================
# BAR CHART WITH HIGHLIGHTED BAR
#===============================
tripPurp %>%
  mutate(highlight_flag = ifelse(tripPurp == 'the primary purpose or sole destination of trip', T, F)) %>%
  ggplot(aes(x = tripPurp)) +
  geom_bar(aes(fill = highlight_flag))

#===============================
# BAR CHART WITH HIGHLIGHTED BAR
#===============================
tripPurp %>%
  mutate(highlight_flag = ifelse(tripPurp == 'the primary purpose or sole destination of trip', T, F)) %>%
  ggplot(aes(x = tripPurp)) +
  geom_bar(aes(fill = highlight_flag)) +
  scale_fill_manual(values = c('#595959', 'red'))

#===============================
# BAR CHART WITH HIGHLIGHTED BAR
#===============================
tripPurp %>%
  group_by(tripPurp) %>%
  summarise(tripPurp_count = n()) %>%
  mutate(highlight_flag = ifelse(tripPurp == '', T, F)) %>%
  ggplot(aes(x = fct_reorder(tripPurp, tripPurp_count), y = tripPurp_count)) +
  geom_bar(aes(fill = highlight_flag), stat = 'identity') +
  scale_fill_manual(values = c('#595959', 'orange')) +
  coord_flip() +
  labs(x = 'Primary Activity'
       ,y = 'Number of Participants'
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
