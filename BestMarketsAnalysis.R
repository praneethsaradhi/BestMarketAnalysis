library(readr)
library(dplyr)
library(ggplot2)

fcc <- read_csv("Datasets/2017-fCC-New-Coders-Survey-Data.csv")
dim(fcc)

fcc %>%
  group_by(JobRoleInterest) %>%
  summarise(freq = n()*100/nrow(fcc)) %>%

splitted_interests <- fcc %>%
  select(JobRoleInterest) %>%
  tidyr::drop_na() %>%
  rowwise %>% 
  mutate(opts = length(stringr::str_split(JobRoleInterest, ",")[[1]]))

n_of_options <- splitted_interests %>%
  ungroup() %>%  
  group_by(opts) %>%
  summarize(freq = n()*100/nrow(splitted_interests))

n_of_options

web_or_mobile <- stringr::str_detect(fcc$JobRoleInterest, "Web Developer|Mobile Developer")
freq_table <- table(web_or_mobile)
freq_table <- freq_table * 100 / sum(freq_table)
freq_table

df <- tibble::tibble(x = c("Other Subject","Web or Mobile Developpement"),
                       y = freq_table)

ggplot(data = df, aes(x = x, y = y, fill = x)) +
  geom_histogram(stat = "identity")



fcc_good <- fcc %>%
  tidyr::drop_na(JobRoleInterest) 

fcc_good %>%
  group_by(CountryLive) %>%
  summarise(`Absolute frequency` = n(),
            `Percentage` = n() * 100 /  nrow(fcc_good) ) %>%
  arrange(desc(Percentage))

fcc_good <- fcc_good %>%
  mutate(MonthsProgramming = replace(MonthsProgramming,  MonthsProgramming == 0, 1) )

fcc_good <- fcc_good %>%
  mutate(money_per_month = MoneyForLearning/MonthsProgramming) 

fcc_good %>%
  summarise(na_count = sum(is.na(money_per_month)) ) %>%

fcc_good  <-  fcc_good %>% tidyr::drop_na(money_per_month)

fcc_good  <-  fcc_good %>% tidyr::drop_na(CountryLive)

fcc_good %>% group_by(CountryLive) %>%
  summarise(freq = n() ) %>%
  arrange(desc(freq)) %>%
  head()

countries_mean  <-  fcc_good %>% 
  filter(CountryLive == 'United States of America' | CountryLive == 'India' | CountryLive == 'United Kingdom'|CountryLive == 'Canada') %>%
  group_by(CountryLive) %>%
  summarize(mean = mean(money_per_month)) %>%
  arrange(desc(mean))

countries_mean

only_4  <-  fcc_good %>% 
  filter(CountryLive == 'United States of America' | CountryLive == 'India' | CountryLive == 'United Kingdom'|CountryLive == 'Canada')

only_4 <- only_4 %>%
  mutate(index = row_number())

ggplot( data = only_4, aes(x = CountryLive, y = money_per_month)) +
  geom_boxplot() +
  ggtitle("Money Spent Per Month Per Country\n(Distributions)") +
  xlab("Country") +
  ylab("Money per month (US dollars)") +
  theme_bw()

fcc_good  <- fcc_good %>% 
  filter(money_per_month < 20000)

countries_mean = fcc_good %>% 
  filter(CountryLive == 'United States of America' | CountryLive == 'India' | CountryLive == 'United Kingdom'|CountryLive == 'Canada') %>%
  group_by(CountryLive) %>%
  summarize(mean = mean(money_per_month)) %>%
  arrange(desc(mean))

countries_mean

only_4  <-  fcc_good %>% 
  filter(CountryLive == 'United States of America' | CountryLive == 'India' | CountryLive == 'United Kingdom'|CountryLive == 'Canada') %>%
  mutate(index = row_number())

ggplot( data = only_4, aes(x = CountryLive, y = money_per_month)) +
  geom_boxplot() +
  ggtitle("Money Spent Per Month Per Country\n(Distributions)") +
  xlab("Country") +
  ylab("Money per month (US dollars)") +
  theme_bw()

india_outliers  <-  only_4 %>%
  filter(CountryLive == 'India' & 
           money_per_month >= 2500)

india_outliers

only_4 <-  only_4 %>% 
  filter(!(index %in% india_outliers$index))

us_outliers = only_4 %>%
  filter(CountryLive == 'United States of America' & 
           money_per_month >= 6000)

us_outliers

only_4  <-  only_4 %>% 
  filter(!(index %in% us_outliers$index))

canada_outliers = only_4 %>%
  filter(CountryLive == 'Canada' & 
           money_per_month >= 4500 &
           MonthsProgramming <= 3)

canada_outliers

only_4  <-  only_4 %>% 
  filter(!(index %in% canada_outliers$index))
                                                                                                                                                                                                                                                                                              
only_4 %>% group_by(CountryLive) %>%
  summarise(freq = n() * 100 / nrow(only_4) ) %>%
  arrange(desc(freq)) %>%
  head()

only_4 %>% group_by(CountryLive) %>%
  summarise(freq = n() ) %>%
  arrange(desc(freq)) %>%
  head()