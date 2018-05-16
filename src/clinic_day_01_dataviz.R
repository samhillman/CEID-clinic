##### Header #####
## Authors - Sam Hillman ##
## Date - 14-May-2018 ##
## Purpose - Computational Clinic ##
## Data = mers.csv ##

##### Packages #####
library(tidyverse)
library(lubridate)

##### Data #####
mers <- read_csv("data/cases.csv")
head(mers)

##### Initial error correction / new variables #####
mers$hospitalized[890] <- c('2015-02-20')
mers <- mers[-471,]

mers$onset2 <- ymd(mers$onset)
mers$hospitalized2 <- ymd(mers$hospitalized)
class(mers$onset2)

day0 <- min(na.omit(mers$onset2))
mers$epi.day <- as.numeric(mers$onset2 - day0)

##### First plot #####
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day)) +
  labs(x='Epidemic day', y='Case count', 
       title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  labs(x='Epidemic day', y='Case count', 
       title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

##### Exercise 1 - Modify the epidemic curve using the argument position="fill" #####
#this looks wrong!
ggplot(data=mers, aes(x=epi.day, fill=country)) +
  geom_bar(position = "fill") +
  labs(x='Epidemic day', y='Case count', 
       title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

##### Exercise 2 - another way to modify a bar plot is to change the coordinates #####
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  labs(x='Epidemic day', y='Case count', 
       title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") +
  coord_flip() +
  coord_polar()

##### Univariate Plots #####
mers$infectious.period <- mers$hospitalized2-mers$onset2 
class(mers$infectious.period)

mers$infectious.period <- as.numeric(mers$infectious.period, units = "days")

ggplot(data=mers) +
geom_histogram(aes(x=infectious.period)) +
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

mers$infectious.period2 <- ifelse(mers$infectious.period<0,0,mers$infectious.period)

ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period2)) +
  labs(x='Infectious period', 
       y='Frequency',
       title='Distribution of calculated MERS infectious period (positive values only)')

ggplot(data=mers) +
  geom_density(mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency',
       title='Probability density for MERS infectious period (positive values only)')

ggplot(data=mers) +
  geom_area(stat='bin', mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency',
       title='Area plot for MERS infectious period (positive values only)')

##### Exercise 3 - experiment with other univariate plot types like geom_dotplot and geom_bar #####
ggplot(mers, aes(x = infectious.period2)) +
  geom_dotplot()

ggplot(mers, aes(x = infectious.period2)) +
  geom_bar()

##### Exercise 4 - use corrected infectious period variable #####
ggplot(mers, aes(x = epi.day,
                 y = infectious.period2)) +
  geom_point()

ggplot(mers, aes(x = epi.day,
                 y = infectious.period2)) +
  geom_smooth()

##### Exercise 5 - plot geom_smooth for each country #####
ggplot(mers, aes(x = epi.day,
                 y = infectious.period2)) +
  geom_smooth(aes(color = country), method = "loess")

#not very pretty - done quickly to catch up!

##### Faceting #####
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping = aes(color=country)) +
  facet_wrap(~ country) +
  scale_y_continuous(limits = c(0, 50)) 

##### Exercise 6 - study variation in the case fatality rate #####
head(mers)
names(mers)


mers %>% 
  group_by(country, epi.day) %>% 
  summarise(cfr = sum(!is.na(death))/length(death)) %>%
  na.omit() %>%
  ggplot(aes(x = epi.day,
             y = cfr)) +
  geom_point(aes(color = country)) +
  facet_wrap(~country) +
  labs(x='Epidemic day', y='CFR',
       title='CFR over time')

#try without dplyr!
head(mers)
cfr <- data.frame()

mers$dead <- ifelse(is.na(mers$death), TRUE, FALSE)

for (i in seq_along(mers$country)){
  mean[i] <- mean(cfr$dead)
}

jordan <- subset(mers, country == "Jordan")
mean(jordan$dead)


for (i in seq_along(mers_country)) {
  mers_cfr[i] <- ifelse(mers$country == mers$country[i], 
                        sum(!is.na(death))/length(death)), 
                        mers_cfr[i])
}


cfr_function <- function(df) {
  for (i in seq_along(unique(df$country)))
    subset(df, df$country == df[i])
}

cfr_function(mers)

##### Exercise 7 - ggplot extensions #####
install.packages("ggplot2-exts")
libary(ggplot2-exts)
#nb needed to update R, ran out of time

##### Plotly #####
install.packages("plotly")
library(plotly)
epi.curve <- ggplot(data = mers) +
  geom_bar(aes(x = epi.day)) +
  labs(x = 'Epidemic day', 
       y = 'Case count', 
       title = 'Global count of MERS cases by date of symptom onset',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
ggplotly(epi.curve)

##### Exercise 8 - make plot interactive #####
country_infec_period <- ggplot(mers, aes(x = epi.day,
                 y = infectious.period2)) +
  geom_smooth(aes(color = country), method = "loess")

ggplotly(country_infec_period)
