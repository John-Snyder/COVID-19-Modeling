library(dplyr)
library(tidyr)

confirmed_link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
deaths_link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
recovered_link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"

confirmed_wide <- read.csv(file = confirmed_link)
deaths_wide    <- read.csv(file = deaths_link)
recovered_wide <- read.csv(file = recovered_link)

# Make files from wide format(columns for dates) to long format (rows for dates)
confirmed_long <- confirmed_wide %>%
  gather(Date, Confirmed, -c("Province.State","Country.Region","Lat","Long"))

deaths_long    <- deaths_wide %>%
  gather(Date, Deaths, -c("Province.State","Country.Region","Lat","Long"))

recovered_long <- recovered_wide %>%
  gather(Date, Recovered, -c("Province.State","Country.Region","Lat","Long"))

# Join confirmed, deaths, and recovered together, and format date for conversion
# to an R date
covid19_long <- confirmed_long %>%
  left_join(deaths_long) %>%
  left_join(recovered_long)

covid19_long$Date <- covid19_long$Date %>%
    gsub(pattern = "X",replacement = "") %>%
    gsub(pattern = "\\.",replacement = "/") %>%
    as.Date(format = "%m/%d/%y")

#####################################################################################
# Prior to march 9 or 10, US data was at the county level, 
# so this aggregates country level US data to states
# This will probably be useless after changes which will be happening soon
# https://github.com/CSSEGISandData/COVID-19/issues/1250
#####################################################################################

# The only rows in the dataset with commas are the US county/state combinations.
#  There, it is '<county>, <State>'.  
split_list <- covid19_long$Province.State %>%
  as.character %>%
  strsplit(split = ",") %>%
  # Some county state combinations have whitespace on the beginning or end
  lapply(function(x) trimws(x))

# Find the rows of the dataset which have commas (just US states)
indecies_to_replace <- which(lapply(split_list, function(x) length(x))==2)
state_codes <- split_list[indecies_to_replace] %>%
  # This takes the state abbreviation
  lapply(function(x) x[2]) %>%
  # This looks up the full state name for that abbreviation
  lapply(function(x) state.name[grep(x, state.abb)]) %>%
  # This manually fixes the DC entries.
  lapply(function(x) ifelse(length(x)==0,"District of Columbia",x)) %>%
  unlist

covid19_long$Province.State[indecies_to_replace] <- state_codes

# At this point, we have multiple records for each state per day because it was
#  previously county level data, so we aggregate this into one record per day/state
covid19_long <- 
covid19_long %>%
  group_by(Province.State,Country.Region,Date) %>%
  summarise(Lat = mean(Lat),
            Long = mean(Long),
            Confirmed = sum(Confirmed),
            Deaths=sum(Deaths),
            Recovered = sum(Recovered))


#covid19_long$Weekday <- weekdays(as.Date(covid19_long$Date))
covid19_long$DeathRate <- covid19_long$Deaths/(covid19_long$Confirmed)

write.csv(x = covid19_long,file = "./Data/COVID19_TS_long.csv",row.names = FALSE)

rm(list = ls())
