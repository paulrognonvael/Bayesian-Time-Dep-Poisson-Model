library(lubridate)
library(tidyverse)

data<- read_delim("DEMO_DEATH_OPEN_W17_.txt", 
                  ";", escape_double = FALSE, col_types = cols(DT_DATE = col_date(format = "%d/%m/%Y")), 
                  trim_ws = TRUE)

data <- data %>% mutate(week = week(DT_DATE))
colnames(data)[colnames(data)=="NR_YEAR"] <- "year"

data <- data %>% group_by(year,week) %>% summarise(deaths = sum(MS_NUM_DEATH))
weekly_data_2009_2020 <- data %>% filter(week<18) %>% 
  pivot_wider(names_from = "year", values_from = c("deaths"))
write_csv(weekly_data_2009_2020,
          "weekly_deaths_belgium_week1_17_years2009_2020.csv")
