##### Header #####
## Authors - Lea Briard and Sam Hillman ##
## Date - 14-May-2018 ##
## Purpose - Computational Clinic ##
## Data = wnv.csv, https://raw.githubusercontent.com/jdrakephd/ideas-workshop/master/wnv.csv ##

##### Packages #####
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

##### Data #####
wnv <- read_csv("https://raw.githubusercontent.com/jdrakephd/ideas-workshop/master/wnv.csv")
head(wnv)
names(wnv)

##### Histogram #####
## use ggplot to create histogram for the total number of cases in each state in each year
ggplot(wnv,mapping=aes(x = Total, fill = State)) + 
  geom_histogram() +
  labs(xlab = "Year",
       ylab = "Total number of WNV cases")

#Create new column called log_total and then plot
wnv %>%
  mutate(log_total = log10(Total)) %>%
  ggplot(aes(x = State, y = log_total, fill = State)) + 
  geom_bar(stat="identity") +
  labs(xlab = "Year",
       ylab = "Total number of WNV cases") +
  facet_wrap(~Year)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#try to create stacked histogram
wnv %>%
  mutate(log_total = log(Total)) %>%
  ggplot(mapping = aes(x = State, y = log_total)) + 
  geom_bar(stat="identity") +
  labs(xlab = "Year",
       ylab = "Total number of WNV cases")

wnv %>% mutate(state_id = str_s)

#### CFR - use operators to calculate the raw case fatality rate (CFR) #####
head(wnv)
wnv %>% group_by(State, Year) %>% summarise(CFR = (Fatal / Total) * 100) 

wnv$CFR <- (wnv$Fatal / wnv$Total) * 100
head(wnv)

##### Sums #####
wnv$Total == rowSums(wnv[,3:5])
sum(wnv$Total) == sum(wnv[,3:5])

### Exercise.  Use modular arithmetic to provide an annual case count for each state rounded
#(down) to the nearest dozen. Use modular arithmetic to extract the rounding errors associated
#with this calculate, then add the errors to obtain the total error
mround <- function(x, base) {
  base * round(x/base)
}

mround(wnv$Total, 12)

wnv$Total %/% 12

##### Functions #####
wnv <- mutate(wnv, ndr = EncephMen / Total)
head(wnv)

fun_mean <- function(x) {
  l <- length(x)
  s <- sum(x)
  m <- s / l
  return(m)
}
fun_mean(c(2, 3, 4, 5))

fun_se <- function (x){
  se <- sd(x) / sqrt(length(x))
  return(se)
}


names(wnv)
wnv %>%
  filter(State == "California" | State == "Colorado" | State == "New York") %>%
  group_by(State) %>%
  summarise(ndr_mean = fun_mean(ndr), ndr_se=fun_se(ndr)) %>%
  ggplot(aes(x = State, y = ndr_mean)) +
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = ndr_mean - ndr_se, 
                    ymax = ndr_mean + ndr_se))

wnv %>%
  group_by(State) %>%
  summarise(ndr_mean = fun_mean(ndr), ndr_se=fun_se(ndr)) %>%
  ggplot(aes(x = State, y = ndr_mean)) +
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = ndr_mean - ndr_se, 
                    ymax = ndr_mean + ndr_se)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##### Control of Flow #####
head(wnv$Longitude)
mean(wnv$Longitude)


wnv$east_west <- ifelse(wnv$Longitude < mean(wnv$Longitude), "West", "East")
str(wnv$Longitude)
head(wnv)
table(wnv$east_west)

wnv %>%
  group_by(east_west) %>%
  summarise(CFR_mean = fun_mean(CFR), CFR_se=fun_se(CFR)) 

wilcox.test(wnv$CFR~wnv$east_west)

##### Loops #####
states_reporting <- c()

for (i in seq_along(wnv$Year)) {
  states_reporting <- c(states_reporting,   sum(wnv$Total[i] > 0))
}

