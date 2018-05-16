##### Packages #####
install.packages("tldyverse")
install.packages("stringr")
install.packages("GGally")
install.packages("maptools")
install.packages("ggmap")
install.packages("maps")
library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)
library(GGally)
library(maptools)
library(ggmap)
library(maps)

##### Data #####
ld <- read_csv("data/lyme.csv")
pop <- read_csv("data/pop.csv")
prism <- read_csv("data/climate.csv")

head(ld) #lyme.csv
head(pop) #pop.csv
head(prism) #climate.csv

##### Functions #####
fips.builder <- function(st,ct){
  if (str_length(ct)==3){
    fips<-paste(as.character(st),as.character(ct),sep="") %>% as.integer
  }
  else if (str_length(ct)==2){
    fips<-paste(as.character(st),"0",as.character(ct),sep="") %>% as.integer
  }
  else {
    3
    fips<-paste(as.character(st),"00",as.character(ct),sep="") %>% as.integer
  }
  return(fips)
}

##### Task 2 + 3 #####
head(pop)

#select the columns in dataset pop called fips and any column that starts with "pop2", and then assign in back to pop
pop %<>% select(fips,starts_with("pop2")) 

#in dataset pop, gather all the columns that start with "pop2" into one column, with the new ldentifying column called "str_year" with all the indivldual years, 
#and then place all the values into a new column called "size", then remove all na values
pop %<>% gather(starts_with("pop2"),key="str_year",value="size") %>% na.omit

#create a new column called year in dataset pop, with the values within the column being the same as those within column str_year but with any character "str" removed
pop %<>% mutate(year=str_replace_all(str_year,"pop",""))

#replace the year column values with the same values but with character type integer
pop %<>% mutate(year=as.integer(year))

#replace the values in column fips with the same values but with any values starting with 0 (^O) removed
pop %<>% mutate(fips=str_replace_all(fips,"^0",""))

#change values in fips column to an integer value
pop %<>% mutate(fips=as.integer(fips))

#remove state wlde summaries
#need to remove any values where the last three digits are all 0 (ie 13000 is Georgia wlde state summaries)
filter(pop, fips %% 100 != 0) #not assigned on purpose

#remove str_year
select(pop, -str_year) #not assigned to an variable on purpose


##### Task 4 - lyme disease #####
head(ld)

ld %<>% gather(starts_with("Cases"), key = "str_year", value = "cases") 
ld %<>% mutate(year = str_replace_all(str_year, "Cases", "")) 
ld %<>% mutate(year = as.integer(year))
ld %<>% rename(state = STNAME, county = CTYNAME)

head(ld)

ld %<>% rowwise() %>% 
  mutate(fips=fips.builder(STCODE,CTYCODE)) 

ld %<>% select(-c(STCODE,CTYCODE,str_year))

head(ld)

##### Task 5 - combine data sets #####
names(ld)
names(prism)

lyme_climate <- inner_join(ld, prism) 
head(lyme_climate)

##### Task 6 - combine data sets ##### 
#combine demographic data
names(lyme_climate)
names(pop)

lyme_climate_dem <- inner_join(lyme_climate, pop)

head(lyme_climate_dem)

##### Task 7 - summary information #####
#determine how many lyme disease cases reported per year
head(ld)

ld %>% 
  ungroup() %>%
  group_by(year) %>%
  summarise(cases_per_year = sum(cases)) %>%
  arrange(desc(cases_per_year))
    
#average number of cases per state averaged across county and year
ld %>%
  ungroup() %>%
  group_by(state, county, year) %>%
  summarise(cases_per_year = sum(cases)) %>%
  arrange(desc(cases_per_year))

##### Task 8 - save data frame #####
#using datagame lyme_climate_dem combined above in task 7
write_csv(lyme_climate_dem, "data/lyme_climate_dem.csv")
saveRDS(lyme_climate_dem, "data/lyme_climate_dem.Rda")

##### Task 9 - mapping annotations ##### 
#get map data for US counties and states
county_map <- map_data("county")
state_map <- map_data("state")

#in dataset lyme_climate_dem, group by the fips
lyme_climate_dem <- read_csv("data/lyme_climate_dem.csv")
head(lyme_climate_dem)

#rewrote to pipe to avoid intermediate steps
ag.fips <- lyme_climate_dem %>%
              ungroup() %>%
              group_by(fips) %>%
              summarise(all.cases = sum(cases))

#join state, county and fips from lyme_climate_dem to ld.16y and reassign it to ld.16y
#explicit by argument
ld.16y <- left_join(select(lyme_climate_dem, c(state,county,fips)), ag.fips, by = "fips")

#find distinct values ld.16y, then change the names of columns from region to state and county to subregion
ld.16y <- distinct(ld.16y) %>% 
            rename(region = state, subregion = county)

#remove all occurences of " County" 
ld.16y$subregion <- str_replace_all(ld.16y$subregion," County","")

#change all text to lower case in region and subregion
ld.16y$region <- tolower(ld.16y$region)
ld.16y$subregion <- tolower(ld.16y$subregion)

head(ld.16y)

#add new column called log10cases which is the log10 of all cases value (+1 to account for zeroes)
ld.16y %<>% mutate(log10cases=log10(1+all.cases))

#create map.ld.16y by joining county_map to ld.16y
#check names of both to add explicit by argument
names(county_map)
names(ld.16y)

map.ld.16y <- left_join(county_map, ld.16y, by = c("region", "subregion"))

#plot map.ld.16y, with long vs lat and showing the log10cases values
ggplot(map.ld.16y) +
  geom_point(aes(long, lat, color = log10cases), size=0.1) +
  scale_colour_gradientn(colours = rev(rainbow(4))) +
  labs(x = "Longitude",
       y = "Latitude",
       color = "Number of cases \n(log10)")

  