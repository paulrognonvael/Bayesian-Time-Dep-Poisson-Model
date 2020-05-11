library(tidyverse)
library(lubridate)

# Source 2

# 2010
deces_2010 <- read_delim("deces-2010.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
deces_2010 <- deces_2010 %>% select(datedeces) %>%
  mutate(death=rep(1,n()),
         date = parse_date_time(datedeces,order="ymd"),
         year=year(date),
         week = week(date)) %>% 
  filter(year > 2009, week %in% seq(1,16,1))

# 2011
deces_2011 <- read_delim("deces-2011.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
deces_2011 <- deces_2011 %>% select(datedeces) %>%
  mutate(death=rep(1,n()),
         date = parse_date_time(datedeces,order="ymd"),
         year=year(date),
         week = week(date)) %>% 
  filter(year > 2009, week %in% seq(1,16,1))

# 2012
deces_2012 <- read_delim("deces-2012.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
deces_2012 <- deces_2012 %>% select(datedeces) %>%
  mutate(death=rep(1,n()),
         date = parse_date_time(datedeces,order="ymd"),
         year=year(date),
         week = week(date)) %>% 
  filter(year > 2009, week %in% seq(1,16,1))

# 2013
deces_2013 <- read_delim("deces-2013.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
deces_2013 <- deces_2013 %>% select(datedeces) %>%
  mutate(death=rep(1,n()),
         date = parse_date_time(datedeces,order="ymd"),
         year=year(date),
         week = week(date)) %>% 
  filter(year > 2009, week %in% seq(1,16,1))

# 2014
deces_2014 <- read_delim("deces-2014.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
deces_2014 <- deces_2014 %>% select(datedeces) %>%
  mutate(death=rep(1,n()),
         date = parse_date_time(datedeces,order="ymd"),
         year=year(date),
         week = week(date)) %>% 
  filter(year > 2009, week %in% seq(1,16,1))

# 2015
deces_2015 <- read_delim("deces-2015.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
deces_2015 <- deces_2015 %>% select(datedeces) %>%
  mutate(death=rep(1,n()),
         date = parse_date_time(datedeces,order="ymd"),
         year=year(date),
         week = week(date)) %>% 
  filter(year > 2009, week %in% seq(1,16,1))

# 2016
deces_2016 <- read_delim("deces-2016.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
deces_2016 <- deces_2016 %>% select(datedeces) %>%
  mutate(death=rep(1,n()),
         date = parse_date_time(datedeces,order="ymd"),
         year=year(date),
         week = week(date)) %>% 
  filter(year > 2009, week %in% seq(1,16,1))

# 2017
deces_2017 <- read_delim("deces-2017.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
deces_2017 <- deces_2017 %>% select(datedeces) %>%
  mutate(death=rep(1,n()),
         date = parse_date_time(datedeces,order="ymd"),
         year=year(date),
         week = week(date)) %>% 
  filter(year > 2009, week %in% seq(1,16,1))

# 2018
deces_2018 <- read_delim("deces-2018.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
deces_2018 <- deces_2018 %>% select(datedeces) %>%
  mutate(death=rep(1,n()),
         date = parse_date_time(datedeces,order="ymd"),
         year=year(date),
         week = week(date)) %>% 
  filter(year > 2009, week %in% seq(1,16,1))

# 2019
deces_2019 <- read_delim("deces-2019.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
deces_2019 <- deces_2019 %>% select(datedeces) %>%
  mutate(death=rep(1,n()),
         date = parse_date_time(datedeces,order="ymd"),
         year=year(date),
         week = week(date)) %>% 
  filter(year > 2009, week %in% seq(1,16,1))




# 2020

# January
deces_jan2020 <- read_delim("deces-2020-m01.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
deces_jan2020 <- deces_jan2020 %>% select(datedeces) %>%
  mutate(death=rep(1,n()),
         date = parse_date_time(datedeces,order="ymd"),
         year=year(date),
         week = week(date)) %>% 
  filter(year > 2009, week %in% seq(1,16,1))

# February
deces_feb2020 <- read_delim("Deces_2020_M02.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE)
deces_feb2020 <- deces_feb2020 %>% select(datedeces) %>%
  mutate(death=rep(1,n()),
         date = parse_date_time(datedeces,order="ymd"),
         year=year(date),
         week = week(date)) %>% 
  filter(year > 2009, week %in% seq(1,16,1))

# March
deces_mar2020 <- read_delim("Deces_2020_M03.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE)
deces_mar2020 <- deces_mar2020 %>% select(datedeces) %>%
  mutate(death=rep(1,n()),
         date = parse_date_time(datedeces,order="ymd"),
         year=year(date),
         week = week(date)) %>% 
  filter(year > 2009, week %in% seq(1,9,1))

# Merge
# Marge of 2010 to 2020

daily_deces_2010_mar2020 <- rbind(deces_2010,deces_2011,deces_2012,
                               deces_2013,deces_2014,deces_2015,
                               deces_2016,deces_2017,deces_2018,
                               deces_2019,deces_jan2020,
                               deces_feb2020,deces_mar2020)



weekly_deces_2010_mar2020 <- daily_deces_2010_mar2020 %>% 
  group_by(year,week) %>% 
  summarise(deaths=sum(death))

weekly_deces_2010_mar2020_2 <- pivot_wider(weekly_deces_2010_mar2020, names_from = "year",
                                        values_from = c("deaths"))

rm(deces_2010,deces_2011,deces_2012,
   deces_2013,deces_2014,deces_2015,
   deces_2016,deces_2017,deces_2018,
   deces_2019,deces_jan2020,
   deces_feb2020,deces_mar2020,daily_deces_2010_mar2020)

# Source 1

data_daily_2018_2020 <-read_delim("2020-05-07_deces_quotidiens_departement_csv.csv",
                  ";", escape_double = FALSE, trim_ws = TRUE)

data_daily_2018_2020 <- data_daily_2018_2020 %>%
  filter(Zone == "France") %>% mutate(date = lubridate::dmy(Date_evenement),
                                      week = lubridate::week(date)) %>%
  filter(week < 17, week > 9) %>%
  mutate(#day_deaths_2018 = c(data_daily_2018_2020$Total_deces_2018[1],diff(Total_deces_2018)),
         #day_deaths_2019 = c(data_daily_2018_2020$Total_deces_2019[1],diff(Total_deces_2019)),
         `2020` = c(data_daily_2018_2020$Total_deces_2020[1],diff(Total_deces_2020))) %>%
  select(week,
         #day_deaths_2018,
         #day_deaths_2019,
         `2020`)

write_csv(data_daily_2018_2020,
          "daily_deaths_france_week9_22_years2018_2020.csv")

data_weekly_2018_2020 <- data_daily_2018_2020 %>%
  group_by(week) %>%
  summarise(#week_deaths_2018 = sum(day_deaths_2018),
            #week_deaths_2019 = sum(day_deaths_2019),
            `2020` = sum(`2020`))

weekly_deces_2010_mar2020_2$`2020` <- c(weekly_deces_2010_mar2020_2$`2020`[1:9],
                                        data_weekly_2018_2020$`2020`)

write_csv(data_weekly_2018_2020,
           "weekly_deaths_france_week1_16_years2010_2020.csv")


