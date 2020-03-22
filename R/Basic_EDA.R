library(dplyr)
library(tidyr)
library(ggplot2)
source("./R/Import_TS_data.R")

covid19_long <- read.csv("./Data/COVID19_TS_long.csv")
covid19_long <- 
  covid19_long %>%
  filter(Confirmed>1) %>%
  group_by(Province.State,Country.Region) %>%
  arrange(Date) %>% 
  mutate(Days_Since_First = 1:n(),
         Country_Province = paste(Country.Region,Province.State,sep="-"),
         deriv=c(0,diff(Confirmed,lag = 1)))

covid19_long_US <- covid19_long %>% 
  filter(Country.Region == "US") %>% 
  filter(!(Province.State %in% c("Diamond Princess","Grand Princess"))) %>%
  mutate(Missouri = ifelse(Province.State=="Missouri","Yes","No"))

covid19_long_Japan <- covid19_long %>% 
  filter(Country.Region == "Italy")

#pdf(file = "~/Desktop/plot.pdf",width = 30,height = 20)
ggplot(covid19_long_US,
       aes(x=Days_Since_First,
           y=log(Confirmed),
           group=Country_Province,
           color=Country_Province)) +
  geom_line() + 
  theme_bw() + guides(color = FALSE,
                      size = FALSE)

ggplot(covid19_long_US,
       aes(x=Days_Since_First,
           y=sqrt(deriv),
           group=Country_Province,
           color=Country_Province)) + geom_line() + theme_bw() + guides(color = FALSE, size = FALSE)
#dev.off()
