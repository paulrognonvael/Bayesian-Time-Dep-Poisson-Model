library(tidyverse)
library(lubridate)


# Source 1

data_daily_2018_2020 <-read_delim("2020-05-07_deces_quotidiens_departement_csv.csv",
                  ";", escape_double = FALSE, trim_ws = TRUE)

data_daily_2018_2020 <- data_daily_2018_2020 %>% 
  filter(Zone == "France") %>% mutate(date = lubridate::dmy(Date_evenement),
                                      week = lubridate::week(date)) %>% 
  filter(week < 17) %>% 
  mutate(day_deaths_2018 = c(data_daily_2018_2020$Total_deces_2018[1],diff(Total_deces_2018)),
         day_deaths_2019 = c(data_daily_2018_2020$Total_deces_2019[1],diff(Total_deces_2019)),
         day_deaths_2020 = c(data_daily_2018_2020$Total_deces_2020[1],diff(Total_deces_2020))) %>% 
  select(Date_evenement,week,day_deaths_2018,day_deaths_2019,day_deaths_2020)

write_csv(data_daily_2018_2020,
          "daily_deaths_france_week9_22_years2018_2020.csv")

data_weekly_2018_2020 <- data_daily_2018_2020 %>% 
  group_by(week) %>% 
  summarise(week_deaths_2018 = sum(day_deaths_2018),
            week_deaths_2019 = sum(day_deaths_2019),
            week_deaths_2020 = sum(day_deaths_2020))
write_csv(data_weekly_2018_2020,
          "weekly_deaths_france_week9_22_years2018_2020.csv")

# Source 2

deces_2018 <- read_delim("deces-2018.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
deces_2018 <- deces_2018 %>% select(datedeces) %>% mutate(death=rep(1,n()),
                                                          date = parse_date_time(datedeces,order="ymd"))

deces_2019 <- read_delim("deces-2019.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
deces_2019 <- deces_2019 %>% select(datedeces) %>% mutate(death=rep(1,n()),
                                                          date = parse_date_time(datedeces,order="ymd"))

daily_deces_2018_2019 <- rbind(deces_2018,deces_2019)
daily_deces_2018_2019 <- daily_deces_2018_2019 %>% 
  mutate(year=year(date),
         week = week(date)) 

group_cols <- c("year","week")
weekly_deces_2018_2019 <- daily_deces_2018_2019 %>% 
  group_by(year,week) %>% 
  summarise(deaths=sum(death))
write_csv(weekly_deces_2018_2019, "weekly_deaths_france_week9_22_years2018_2020_2ndsource.csv")
weekly_deces_2018_2019_2 <- weekly_deces_2018_2019 %>% filter(year %in% c("2018","2019"),week %in% seq(9,16,1))
weekly_deces_2018_2019_2 <- pivot_wider(weekly_deces_2018_2019_2, names_from = "year",
                                        values_from = c("deaths"))

weekly_data_merge<-cbind(data_weekly_2018_2020,weekly_deces_2018_2019_2)
weekly_data_merge <- weekly_data_merge[,-1] %>% 
  mutate(ratio_2018 = week_deaths_2018/`2018`)
write_csv(weekly_data_merge, "merged_data_weekly.csv")
