#load needed libraries
library(tidyverse)
library(lubridate)

#Setting wd and loading data
#put the path of where you are storing the data on your computer
paths = c('C:/Users/eliza/Documents/BANA 7047 - Data Mining II/Group Project/Data', 'C:/Users/ashle/Downloads/8451_The_Complete_Journey_2_Master (1)/The_Complete_Journey_2_Master', 'C:/Users/xiaoj/Desktop/BANA/BANA7047 Data Mining II/Project/8451_The_Complete_Journey_2_Master/The_Complete_Journey_2_Master')
#use the code Sys.info()[7] to find out how your name is stored on your local machine, copy to this list
names(paths) = c('eliza', 'ashle', 'xiaoj')
#this will set the wd
setwd(paths[Sys.info()[7]])

#load data sets
households <- read_csv('5000_households.csv', col_types = cols(.default = 'f', HSHD_NUM = 'c'), na = c("null", "", "NA", "NOT AVAILABLE", "Unknown"))
transactions <- read_csv('5000_transactions.csv', col_types = cols(.default = '?', STORE_R = 'f', YEAR = 'f'), na = c("null", "", "NA"))
products <- read_csv('5000_products.csv', col_types = cols(.default = 'f', PRODUCT_NUM = 'c'), na = c("null", "", "NA"))

#rename column 'X5' to be more descriptive
products <- products %>% rename(ORGANIC = X5)

#rename truncated date and store region column on transactions
transactions <- transactions %>%
  rename(DATE = PURCHASE_) %>%
  rename(REGION = STORE_R)

#make 'DATE' column in transactions dataset into a date format
transactions <- transactions %>%
  mutate(DATE = dmy(DATE))

#reorder factors on households dataset
households <- households %>%
  mutate(AGE_RANGE = fct_relevel(AGE_RANGE,
                                 "19-24",
                                 "25-34",
                                 "35-44",
                                 "45-54",
                                 "55-64",
                                 "65-74",
                                 "75+")) %>%
  mutate(INCOME_RANGE = fct_relevel(INCOME_RANGE,
                                    "UNDER 35K",
                                    "35-49K",
                                    "50-74K",
                                    "75-99K",
                                    "100-150K",
                                    "150K+")) %>%
  mutate(HH_SIZE = fct_relevel(HH_SIZE,
                               "1",
                               "2",
                               "3",
                               "4",
                               "5+")) %>%
  mutate(CHILDREN = fct_relevel(CHILDREN,
                                "1",
                                "2",
                                "3+"))

#By default the households dataset encodes all households without children as having a 'CHILDREN' value of NA. This encodes these households with a CHILDREN value of 0 to avoid missing values.
households <- households %>%
  mutate(CHILDREN = ifelse(HSHD_COMPOSITION %in% c("1 Adult", "2 Adults", "Single Male", "Single Female"), 0, CHILDREN))

#There are some transactions with negative spends and negative units or units of 0, assuming that the negative spends and negative units are returns, unsure of the transactions with 0 units, but we are filtering these from the transactions table
returns <- transactions %>% 
  filter(UNITS < 1) #I am assuming the items with negative spends and positive units are items bought with a coupon that reduced the price below 0

transactions <- anti_join(transactions, returns, by = c("BASKET_NUM", "PRODUCT_NUM"))

#creating a data set with basket level detail
mode <- function(x){
  which.max(tabulate(x))
}

baskets <- transactions %>%
  mutate(YEAR = as.numeric(YEAR)) %>%
  mutate(DATE = as.numeric(DATE)) %>%
  group_by(HSHD_NUM, BASKET_NUM) %>%
  summarise(
    PRODUCTS = n(),
    TOTAL_SPEND = sum(SPEND),
    TOTAL_UNITS = sum(UNITS),
    DATE = mode(DATE), #baskets can have multiple dates but they all appear to only differ by one day, suggesting these are purchases taking place around midnight - the first item scanned may have been at 11:58 pm the previous day and the last item scanned at 12:01 am the current day - this is an assumption as time of scan is not available. This function takes the mode as the actual date
    YEAR = mode(YEAR) #none of the baskets appear to have multiple years, but this would catch a New Year's Eve purchase going forward
  ) %>%
  mutate(YEAR = ifelse(YEAR == 1, "2016", "2017")) %>%
  mutate(DATE = as_date(DATE, origin = lubridate::origin))


#confirming primary keys in each table are unique
households %>% 
  count(HSHD_NUM) %>% 
  filter(n > 1)

products %>%
  count(PRODUCT_NUM) %>% 
  filter(n > 1)

baskets %>%
  count(BASKET_NUM) %>% 
  filter(n > 1)

#create dataset for market basket analysis
market_basket <- products %>%
  mutate(ORGANIC = str_replace(ORGANIC, "N", "NON-ORGANIC")) %>%
  mutate(ORGANIC = str_replace(ORGANIC, "Y", "ORGANIC")) %>% 
  mutate(PRODUCT_NAME = str_c(ORGANIC, COMMODITY, sep = " ")) %>%
  select(PRODUCT_NUM, PRODUCT_NAME) %>%
  mutate(INDICATOR = 1) %>%
  right_join(transactions, by = "PRODUCT_NUM") %>%
  select(BASKET_NUM, PRODUCT_NAME, INDICATOR) %>%
  distinct() %>%
  pivot_wider(names_from = PRODUCT_NAME, 
              values_from = INDICATOR, 
              values_fill = list(INDICATOR = 0))

#create dataset with demographic and spending data for classification of organic households
organic_households <- products %>%
  select(PRODUCT_NUM, ORGANIC) %>%
  mutate(ORGANIC = str_replace(ORGANIC, "N", "NORG")) %>%
  mutate(ORGANIC = str_replace(ORGANIC, "Y", "ORG")) %>% 
  right_join(transactions, by = "PRODUCT_NUM") %>%
  group_by(HSHD_NUM, ORGANIC) %>%
  summarise(
    TOTAL_SPEND = sum(SPEND),
    TOTAL_UNITS = sum(UNITS),
    UNIQUE_ITEMS = n_distinct(PRODUCT_NUM)) %>% 
  ungroup() %>%
  pivot_wider(names_from = ORGANIC, values_from = c(TOTAL_SPEND, TOTAL_UNITS, UNIQUE_ITEMS), values_fill = list(TOTAL_SPEND = 0, TOTAL_UNITS = 0, UNIQUE_ITEMS = 0)) %>%
  left_join(households, by = "HSHD_NUM") %>%
  mutate(ORG_HOUSE = ifelse(TOTAL_UNITS_ORG > 0, 1, 0)) %>%
  filter(complete.cases(.) == TRUE)

#creating a datset to target households who increased their purchasing of organic products between 2016 & 2017
#first we will take the baskets dataset & compute the average spend per basket per year and the average cost per item per year

basket_by_year <- baskets %>%
  mutate(COST_PER_UNIT = TOTAL_SPEND / TOTAL_UNITS) %>%
  group_by(HSHD_NUM, YEAR) %>% 
  summarise(
    AVG_SPEND = mean(TOTAL_SPEND),
    AVG_UNITS = mean(TOTAL_UNITS),
    AVG_COST_PER_UNIT = mean(COST_PER_UNIT)
  ) %>%
  pivot_wider(names_from = c(YEAR), values_from = c(AVG_SPEND, AVG_UNITS, AVG_COST_PER_UNIT), values_fill = list(AVG_SPEND = 0, AVG_UNITS = 0, AVG_COST_PER_UNIT = 0))

household_increase <- products %>%
  select(PRODUCT_NUM, ORGANIC) %>%
  mutate(ORGANIC = str_replace(ORGANIC, "N", "NORG")) %>%
  mutate(ORGANIC = str_replace(ORGANIC, "Y", "ORG")) %>% 
  right_join(transactions, by = "PRODUCT_NUM") %>%
  group_by(HSHD_NUM, ORGANIC, YEAR) %>%
  summarise(
    TRIPS = n(),
    TOTAL_SPEND = sum(SPEND),
    TOTAL_UNITS = sum(UNITS),
    UNIQUE_ITEMS = n_distinct(PRODUCT_NUM)) %>% 
  ungroup() %>%
  pivot_wider(names_from = c(ORGANIC, YEAR), values_from = c(TRIPS, TOTAL_SPEND, TOTAL_UNITS, UNIQUE_ITEMS), values_fill = list(TRIPS = 0, TOTAL_SPEND = 0, TOTAL_UNITS = 0, UNIQUE_ITEMS = 0)) %>%
  left_join(households, by = "HSHD_NUM") %>%
  mutate(ORG_INCREASE = ifelse(TOTAL_SPEND_ORG_2016 < TOTAL_SPEND_ORG_2017, 1, 0)) %>%
  filter(complete.cases(.) == TRUE) %>%
  filter(TOTAL_SPEND_ORG_2016 > 0) %>%
  mutate(PERCENT_ORG_CHANGE = (TOTAL_SPEND_ORG_2017 - TOTAL_SPEND_ORG_2016) / TOTAL_SPEND_ORG_2016) %>%
  left_join(basket_by_year, by = "HSHD_NUM")

#save out created datasets to work with in future

setwd(paths[Sys.info()[7]])
save(market_basket, file = "market_basket.Rdata", compress = FALSE)
save(organic_households, file = "organic_households.Rdata", compress = FALSE)
save(household_increase, file = "household_increase.Rdata", compress = FALSE)

#should be able to use function "load(file = "organic_households.Rdata")" to load dataset when you need it in a different notebook