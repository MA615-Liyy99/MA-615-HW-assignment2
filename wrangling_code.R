library("tidyverse")
library("dplyr")

spending_raw <- readxl::read_xlsx("spending.xlsx")# read data
spending_raw

dim(spending_raw)

# tidy data, make each row an observation
spending <- spending_raw %>% 
  pivot_longer(-country, names_to = "year", values_to = "spending")
spending

# count the included countries: 192
length(unique(spending$country))

# find missing value
c <- unique(spending[!complete.cases(spending), ]$country)# 9 countries with missing value
left <- setdiff(unique(spending$country), c)

spending <- spending %>% filter(country %in% left)
# length(unique(spending$country))


# look at the spending in 1994 and reorder them in increasing value
filter(spending, year == 1994) %>% arrange(spending)

# look at the spending for specific countries from 1994 to 2009
filter(spending, country == c("China", "United States"))

# look at the average spending per year
s_1 <- group_by(spending, year)
summarise(s_1, mean = mean(spending, na.rm = TRUE))

# look at the average spending per country
s_2 <- group_by(spending, country)
summarise(s_2, mean = mean(spending, na.rm = TRUE))



----------------------------------------------------



expectancy_raw <- readxl::read_xlsx("expectancy.xlsx")
expectancy_raw

dim(expectancy_raw)

# tidy data, make each row an observation
expectancy <- expectancy_raw %>% 
  pivot_longer(-country, names_to = "year", values_to = "expectancy")
expectancy

# countries included
length(unique(expectancy$country))# 195

# merge two datasets
df <- merge(spending, expectancy, by = c("country", "year"))
length(unique(df$country))# 181

# colSums(is.na(df))
