##load data sets
households <- read.csv("5000_households.csv")
products <- read.csv("5000_products.csv")
transactions <- read.csv("5000_transactions.csv")


#EDA on household set
#household number is unique id
#5000 obs, 9 variables
#800-900 null values for each variable
summary(households)
str(households)
#rename L to something more understandable "Loyalty"
households$Loyalty <- households$L
households <- households[,-2]

#see number of loyal vs nonloyal households in data set
plot(households$Loyalty)
table(households$Loyalty) #there are 729 non loyal customers and 4271 loyal customers in the data set

#plot income ranges w/ loyalty
plot(households$INCOME_RANGE)
table1 <- table(households$Loyalty, households$INCOME_RANGE)
barplot(table1, legend.text = c("Non-Loyal","Loyal"))

#look at loyalty by age range
table2 <- table(households$Loyalty,households$AGE_RANGE)
barplot(table2, legend.text = c("Non-Loyal","Loyal"))

levels(households$MARITAL) #married, null, single, unknown (maybe combine null and unknown?)
levels(households$HOMEOWNER) #homeowner, null, renter, unknown (combine null and unknown again?)
levels(households$HSHD_COMPOSITION) #adult, adult and kids, 2 adults, 2 adults and kids, N/A, null, single female, single male
levels(households$HH_SIZE) #1,2,3,4,5+, null
levels(households$CHILDREN) #1,2,3+, not available, null

#EDA on products set
#product number is unique id
#151141 observations, 5 variables 
levels(products$DEPARTMENT) #food, non-food, pharmacy
levels(products$COMMODITY) #42 different sub categories of products (i.e. meat-chicken meat-pork, produce, personal care, seafood, etc.)
levels(products$BRAND_TY) #national or private prand
str(products)
summary(products)
head(products)
#no missing values

#EDA on transactions set
#10625553 observations, 9 variables
#connected to households by hh # and products by product #
str(transactions)
head(transactions)

#eliza's r----------------------------------------------------------------------------------------------
install.packages("tidyverse")
library(tidyverse)

#Setting wd and lodaing data
#put the path of where you are storing the data on your computer
paths=c('C:/Users/eliza/Documents/BANA 7047 - Data Mining II/Group Project/Data', 'path2', 'path3')
#use the code Sys.info()[7] to find out how yoru name is stored on your local machine, copy to this list
names(paths)=c('eliza', 'name2', 'name3')
#this will set the wd
setwd(paths[Sys.info()[7]])
#load data sets
households <- read_csv('5000_households.csv', col_types=cols(.default='f', HSHD_NUM='c'))
transactions <- read_csv('5000_transactions.csv', col_types=cols(.default='?', STORE_R='f', YEAR='f'))
products <- read_csv('5000_products.csv', col_types=cols(.default='f', PRODUCT_NUM='c'))
#rename column 'X5' to be more descriptive
products <- products %>% rename(ORGANIC = X5)
#rename truncated date and store region column on transactions
transactions <- transactions %>%
  rename(DATE=PURCHASE_) %>%
  rename(REGION = STORE_R)

#households summary
summary(households, maxsum=20)

#products summary
summary(products, maxsum = 30)

#transaction summary
summary(transactions)

#I was curious how man y transactions there are with negative spends - I think these might be returns & we may want to filter?
returns <- transactions %>% 
  filter(SPEND<0) %>%
  group_by(BASKET_NUM) %>%
  summarise(
    ITEMS = n(),
    TOTAL_RETURN = sum(SPEND),
    TOTAL_UNITS = sum(UNITS)
  )
summary(returns)


#creating a data set with basket level detail - filter out negative spends
baskets <- transactions %>%
  filter(SPEND>0) %>%
  group_by(HSHD_NUM, BASKET_NUM) %>%
  summarise(
    PRODUCTS = n(),
    TOTAL_SPEND = sum(SPEND),
    TOTAL_UNITS = sum(UNITS)
  )
summary(baskets)

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


merged_df <- transactions %>%
  filter(SPEND>0) 
merged_df <- left_join(merged_df, products, by='PRODUCT_NUM') 
merged_df <- left_join(merged_df, households, by='HSHD_NUM')
merged_df <- left_join(merged_df, baskets, by='BASKET_NUM')
save(merged_df, file='merged_df.Rdata')

str(merged_df)
summary(merged_df)

merged2 <- na.omit(merged_df)
summary(merged2)

#treating the nulls in merged 2 to get data down to the demographic hh level
merged2 <- droplevels(merged2[!merged2$AGE_RANGE == 'null',])
str(merged2)



#creating lookup table that links transactions to baskets & households
trans_key <- transactions %>% 
  filter(SPEND>0) %>%
  select(BASKET_NUM, HSHD_NUM, DATE) %>%
  distinct()

#confirming each basket only has one HSHD_NUM & DATE
baskets <- left_join(baskets, trans_key, by='BASKET_NUM')  
summary(baskets)

count(baskets['BASKET_NUM'])

#visualizations-----------------------------------------------------------------------------------
plot(merged_df$ORGANIC)
table(merged_df$ORGANIC)
537868 /(537868 +13396674 ) #0.03859962
#only 0.0386 or about 4% of the products are flagged as organic


organic.income <- table(merged_df$ORGANIC,merged_df$AGE_RANGE)
barplot(organic.income, legend.text = c("Non-Organic","Organic"))

#create HH_df--------------------------------------------------------------------------------------
hh_df <- households
hh_df <- left_join(hh_df, transactions, by='HSHD_NUM')
hh_df <- left_join(hh_df, products, by='PRODUCT_NUM')
merged_df <- left_join(merged_df, baskets, by='PRODUCT_NUM')
save(merged_df, file='merged_df.Rdata')

summary(hh_df)
