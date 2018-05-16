###### Packages and Data ######
install.packages("modelr")
library(modelr)
library(tidyverse)
library(GGally)
library(magrittr)

ld.prism.pop <- read_csv("data/lyme_climate_dem.csv")
names(ld.prism.pop)

##### Pairs #####
ggpairs(ld.prism.pop,columns=c("prcp","avtemp","size","cases"))

##### Task 3 - new columns #####
ld.prism.pop <- ld.prism.pop %>% mutate(log10.size = log10(size),
                        log10.casesplusone = log10(cases + 1))

ggpairs(ld.prism.pop, columns = c("prcp", "avtemp", "log10.size", "log10.casesplusone"))
#cannot log10 a zero value

##### Task 4 + 5 - simple lm model #####
set.seed(222)

data_subset     <- sample_n(ld.prism.pop, 100)
prcp.vs.avtemp  <- ggplot(data_subset, aes(x = prcp, y = avtemp)) +
                    geom_point()

prcp.vs.avtemp + geom_smooth(method = "lm")

##### Task 6 - lm model #####
lm.1 <- lm(avtemp ~ prcp, data = data_subset)
summary(lm.1)

#or with full dataset
lm.2 <- lm(avtemp ~ prcp, data = ld.prism.pop)
summary(lm.2)

##### Task 7 - slope #####
summary(lm(avtemp ~ prcp, data = data_subset))

#slope
summary(lm(avtemp ~ prcp, data = data_subset))$coefficients[2,1]
#p value
summary(lm(avtemp ~ prcp, data = data_subset))$coefficients[2,4]


##### Task 8 - modelr #####
#not one line for readability
ld.prism.pop %>%
  na.omit() %>% 
  group_by(year) %>% 
  summarise(total_pop = sum(size)) %>%
    ggplot(aes(x = year, y = total_pop)) +
    geom_point() +
    geom_line()

##### Task 9, 10, 11, 12 - by state #####
by_state <- ld.prism.pop %>% group_by(state)
by_state %<>% nest
by_state

by_state$data[[10]]

lm_function <- function(df) {
  lm(size ~ year, data = df)
}

models <- purrr::map(by_state$data, lm_function)

##### Task 13 + 14 - new column #####
by_state <- mutate(by_state, model = map(data, lm_function))
by_state %<>% mutate(resids = map2(data, model, add_residuals))

#inspect residuals for Georgia only
head(by_state$resids[[10]])
str(by_state$resids[[10]])           
str(by_state$resids[[10]]$resid)    

##### Task 15 #####
sum_resids <- function(x) {
  sum(abs(x$resid))
}

by_state %<>% mutate(totalResid = map(resids,sum_resids))

by_state$totalResid[[10]]

##### Task 16 - 18 #####
model_slope <- function(model) {
  model$coefficients[2]
}

by_state <- by_state %>% mutate(slope = purrr::map(model, model_slope))

slopes <- unnest(by_state, slope)
totalResids <- unnest(by_state, totalResid)

head(slopes)

slopes %>% 
  ggplot(aes(state, y = slope)) +
  geom_point(aes(x = state)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

head(totalResids)

totalResids %>% 
  ggplot(aes(state, y = totalResid)) +
  geom_point(aes(x = state)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##### Task 19 #####
#Repeat Tasks 9 and 10 using a different data frame name, by_state2
by_state2 <- ld.prism.pop %>% group_by(state)
by_state2 %<>% nest
by_state2

##### Task 20 #####
#function that accepts an element of by_state2$data and returns spearman correlation between precip and cases

by_state2$data
rain_cases_corr <- function(df) {
  suppressWarnings(cor.test(df$cases, df$prcp, method = "spearman")$estimate)
}

by_state2 <- by_state2 %>% mutate(spCor = purrr::map(data, rain_cases_corr))

spCor <- unnest(by_state2, spCor)
spCor %<>% arrange(desc(spCor))
spCor$state <- factor(spCor$state, levels=unique(spCor$state))

ggplot(spCor,aes(state,spCor))  +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
