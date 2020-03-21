library(dplyr)
library(tidyr)

confirmed_link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
deaths_link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
recovered_link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"

confirmed_wide <- read.csv(file = confirmed_link)
deaths_wide <- read.csv(file = deaths_link)
recovered_wide <- read.csv(file = recovered_link)

confirmed_long <- confirmed_wide %>%
  gather(Date, Confirmed, -c("Province.State","Country.Region","Lat","Long"))

confirmed_long$Date <- confirmed_long$Date %>%
  gsub(pattern = "X",replacement = "") %>%
  gsub(pattern = "\\.",replacement = "/") %>%
  as.Date(format = "%m/%d/%y")

deaths_long <- deaths_wide %>%
  gather(Date, Deaths, -c("Province.State","Country.Region","Lat","Long"))

deaths_long$Date <- deaths_long$Date %>%
  gsub(pattern = "X",replacement = "") %>%
  gsub(pattern = "\\.",replacement = "/") %>%
  as.Date(format = "%m/%d/%y")

recovered_long <- recovered_wide %>%
  gather(Date, Recovered, -c("Province.State","Country.Region","Lat","Long"))

recovered_long$Date <- recovered_long$Date %>%
  gsub(pattern = "X",replacement = "") %>%
  gsub(pattern = "\\.",replacement = "/") %>%
  as.Date(format = "%m/%d/%y")

covid19_long <- confirmed_long %>% left_join(deaths_long) %>% left_join(recovered_long)

# aggregate country level USdata to states
split_list <- covid19_long$Province.State %>%
  as.character %>%
  strsplit(split = ",") %>%
  lapply(function(x) trimws(x))
indecies_to_replace <- which(lapply(split_list, function(x) length(x))==2)
state_codes <- split_list[indecies_to_replace] %>%
  lapply(function(x) x[2]) %>%
  lapply(function(x) state.name[grep(x, state.abb)]) %>%
  lapply(function(x) ifelse(length(x)==0,"District of Columbia",x)) %>%
  unlist

covid19_long$Province.State[indecies_to_replace]<-state_codes

#covid19_long$Weekday <- weekdays(as.Date(covid19_long$Date))
covid19_long$DeathRate <- covid19_long$Deaths/(covid19_long$Confirmed)

write.csv(x = covid19_long,file = "./Data/COVID19_TS_long.csv",row.names = FALSE)

rm(list = ls())
