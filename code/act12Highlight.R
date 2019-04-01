nvs2018 <- read.csv("data/nvs2018.csv")

#==============
# LOAD PACKAGES
#==============
library(ggplot2)
library(tidyverse)
primact <- na.omit(nvs2018$PRIMACTCODED)
primact <- as.data.frame(primact)

#--------
# INSPECT
#--------
primact %>% glimpse()
primact %>% count(primact)

#=================
# SIMPLE BAR CHART
#=================
ggplot(data = primact, aes(x = primact)) +
  geom_bar()

#===========================================
# SIMPLE BAR CHART (USING THE PIPE OPERATOR)
#===========================================
primact %>%
  ggplot(aes(x = primact)) +
  geom_bar()

#===============================
# BAR CHART WITH HIGHLIGHTED BAR
#===============================
primact %>%
  mutate(highlight_flag = ifelse(primact == 'Hiking/Walking', T, F)) %>%
  ggplot(aes(x = primact)) +
  geom_bar(aes(fill = highlight_flag))

#===============================
# BAR CHART WITH HIGHLIGHTED BAR
#===============================
primact %>%
  mutate(highlight_flag = ifelse(primact == 'Hiking/Walking', T, F)) %>%
  ggplot(aes(x = primact)) +
  geom_bar(aes(fill = highlight_flag)) +
  scale_fill_manual(values = c('#595959', 'orange'))

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

