library(purrr)
library(tidyr)
library(ggplot2)

nvs2018 %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +     # In separate panels
  geom_histogram()                         # as histogram
# geom_density()                           # as density

nvs2018 %>%
  keep(is.factor) %>%                     # Keep only factor columns
  ggplot(aes_string()) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +     # In separate panels
  geom_histogram()                         # as histogram
