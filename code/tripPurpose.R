## @knitr trip-purpose

#--------------
# LOAD PACKAGES
#--------------
library(ggplot2)
library(grid)
library(gridExtra)
library(tidyverse)
source("code/functions/round_df.R")

tripPurp <- na.omit(nvs2018$TRIPPURP)
tripPurp <- as.data.frame(tripPurp)

#--------
# INSPECT
#--------
tripPurp %>% glimpse()
tripPurp %>% count()

purpose <- prop.table(table(tripPurp))*100 # cell percentages
purpose <- round_df(purpose) # cell percentages
purpose <- as.data.frame(purpose)
purpose <- purpose[order(purpose$Freq, decreasing = TRUE),]
colnames(purpose) <- c("Purpose", "Proportion")
purpose

purpose1 <- purpose$Purpose[1]
purpose1Prop <- purpose$Proportion[1]
purpose2 <- purpose$Purpose[2]
purpose2Prop <- purpose$Proportion[2]
purpose3 <- purpose$Purpose[3]
purpose3Prop <- purpose$Proportion[3]

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

#========================================
# BAR CHART WITH HIGHLIGHTED BAR - COUNTS
#========================================
tripPurp %>%
  mutate(highlight_flag = ifelse(tripPurp == 'the primary purpose or sole destination of trip', T, F)) %>%
  ggplot(aes(x = tripPurp)) +
  geom_bar(aes(fill = highlight_flag)) +
  scale_fill_manual(values = c('#595959', 'red')) +
  coord_flip() +
  labs(x = 'Primary Purpose'
       ,y = 'Number of Respondents'
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

grid.arrange(textGrob("Primary Purpose of Trip to this Refuge",
                                     gp = gpar(fontsize = 12, fontface = "bold")),
                            tripPurpBar,
                            heights = c(0.1, 1))

#=========================================
# BAR CHART WITH HIGHLIGHTED BAR - LOCAL/NONLOCAL
#=========================================
tripPurpBar <- purpose %>%
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

grid.arrange(textGrob("Primary Purpose of Trip to this Refuge",
                      gp = gpar(fontsize = 12, fontface = "bold")),
             tripPurpBar,
             heights = c(0.1, 1))
