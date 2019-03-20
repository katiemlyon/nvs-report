library(stringr)
library(tidyverse)

refugeName <- c(nvs2018$RefugeName)

reports <- tibble(
  filename = str_c("VisitorSurveyResults-", RefugeName, ".html"),
  params = map(RefugeName, ~list(RefugeName = .))
)

reports %>%
  select(output_file = filename, params) %>%
  pwalk(rmarkdown::render, input = "index.Rmd", output_dir = "output/")