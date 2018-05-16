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

##### Defining Functions #####
#function for finding mean
fun_mean <- function(x) {
  l <- length(x)
  s <- sum(x)
  m <- s / l
  return(m)
}

#function for finding standard error
fun_se <- function (x){
  se <- sd(x) / sqrt(length(x))
  return(se)
}
#mround for rounding to any nearest number 
mround <- function(x, base) {
  base * round(x/base)
}

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
  theme(axis.text.x = element_blank(),
        legend.key.width = unit(0.3, "cm"))


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
wnv %>% 
  ungroup() %>%
  group_by(State, Year) %>% 
  summarise(CFR = (Fatal / Total) * 100) 

wnv$CFR <- (wnv$Fatal / wnv$Total) * 100

head(wnv)

##### Sums #####
wnv$Total == rowSums(wnv[,3:5])
sum(wnv$Total) == sum(wnv[,3:5])

### Exercise.  Use modular arithmetic to provide an annual case count for each state rounded
#(down) to the nearest dozen. Use modular arithmetic to extract the rounding errors associated
#with this calculate, then add the errors to obtain the total error


mround(wnv$Total, 12)

wnv$Total %/% 12

##### Functions #####
wnv <- mutate(wnv, ndr = EncephMen / Total)
head(wnv)

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

head(wnv)
table(wnv$east_west)

wnv %>%
  group_by(east_west) %>%
  summarise(CFR_mean = fun_mean(CFR), CFR_se=fun_se(CFR)) 

#non-parametric test needed
hist(wnv$CFR)
wilcox.test(wnv$CFR ~ wnv$east_west)

#latitudinal gradient?
ggplot(wnv, aes(x = Latitude, y = CFR)) +
         geom_point()

ggplot(wnv, aes(x = Latitude, y = CFR)) +
  geom_smooth(method = "gam")

summary(lm(CFR ~ Latitude, data = wnv))

##### Loops #####
head(wnv)

#basic for loop, states reporting over all years
states_reporting <- 0

for (i in seq_along(wnv$Year)) {
  ifelse(wnv$Total[i] > 0, states_reporting <- states_reporting + 1, states_reporting)
  }

print(states_reporting)

#todo this isn't working
total_number <- function(x) {
  as.matrix(counts) <- 0
  for (i in seq_along$x) {
    ifelse(x[i] > 0, counts <- counts + 1, counts)
  }
}

total_number(wnv$Total)





##### Help #####
#prop.test
?prop.test
#calculate confidence intervals for the case fatality rates in all states 
#for which there have been reported cases of WNV
head(wnv)

summarised_state_totals <- wnv %>% group_by(State) %>%
                              summarise(alive = sum(Total - Fatal), 
                                        fatal = sum(Fatal)) 

prop.test <- prop.test(summarised_state_totals$fatal, summarised_state_totals$alive)
prop.test

#need to use binom.test
#need to get this to work when I have time
binom.test(summarised_state_totals$fatal, summarised_state_totals$alive)
