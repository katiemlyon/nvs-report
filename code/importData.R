# import 2018 refuge data from .sav

library(foreign)

nvs2018 <-
  read.spss(
    "data/NVS_MergedDataset_2019.02.09.sav",
    use.value.labels = TRUE,
    trim_values = TRUE,
    to.data.frame = TRUE,
    stringsAsFactors = FALSE
  )

# save data as csv file
write.csv(nvs2018,
          file = "data/nvs2018.csv",
          row.names = FALSE,
          na = "")

#nvs2018 <- read.csv("C:/Users/klyon/hd_visitorsurvey/nvs-reports/data/nvs2018.csv")
