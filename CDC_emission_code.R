library(tidyverse)
glimpse(Natural_Science_Dataset)

# drop repeated columns with description
# drop columns POLLUTANT, MEASURE, SEASONALITY, and Flags
Drop_duplicates <- Natural_Science_Dataset %>% 
  select(Country, FLIGHT, FREQUENCY, SOURCE, TIME, Value)

# filter frequency=annual, flight=passenger, source=domestic
cleaned<-Drop_duplicates %>% 
  filter(FREQUENCY == "A", FLIGHT == "P") %>% 
  filter(SOURCE == "RES_DOM_IN" | SOURCE == "NRES_DOM_IN") %>%
  select(-FREQUENCY, -FLIGHT) %>% 
  drop_na()

# sum yearly values for each country, then mean yearly values for each country
cleaned_year_ave <- cleaned %>% 
  group_by(Country,TIME) %>% 
  summarize(total_value = sum(Value)) %>%  # sum yearly values
  group_by(Country) %>% 
  summarise(year_ave = mean(total_value)) # mean yearly values

# take log for yearly mean values for Tableau visualization
cleaned_year_ave$log_values <- log(cleaned_year_ave$year_ave)
write_csv(cleaned_year_ave,"/Users/zyliazhang/Desktop/CDC/country_Pval_log.csv")
