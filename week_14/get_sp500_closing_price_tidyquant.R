### use the tidyquant package to load in all closing stock
### prices for the S&P500

### you must download and install tidyquant to run this script

### use some simple cleaning rules to create a data set to then
### use with PCA

library(tidyverse)
library(tidyquant)

tq_index_options()

tq_index("SP500")

### get all symbols associated with the S&P500
sp500_symbols <- (tq_index("SP500"))$symbol

### practice getting the stock price for a single stock in the S&P500
stock_1 <- tq_get(sp500_symbols[1], get = "stock.prices", from = "2018-01-01", to = "2020-04-14")

### loop over all get all s&p500 stocks, only keep the closing price

get_close_price <- function(my_symbol, from_date)
{
  tq_get(my_symbol, get = "stock.prices", from = from_date) %>% 
    dplyr::select(symbol, date, close)
}

### remove the stock with the "."
sp500_symbols_b <- tibble::tibble(
  ssmbol = sp500_symbols
) %>% 
  filter(!stringr::str_detect(ssmbol, "\\.")) %>% 
  pull(ssmbol)

### get all closing prices from Jan 01, 2018
all_close <- purrr::map_dfr(sp500_symbols_b, 
                            get_close_price,
                            from_date = "2018-01-01")

all_close %>% count(symbol) %>% 
  count(n)

### only keep the symbols with all trading days
max_days <- all_close %>% count(symbol) %>% 
  count(n) %>% 
  filter(nn == max(nn)) %>% 
  pull(n)

symbols_keep <- all_close %>% 
  count(symbol) %>% 
  filter(n == max_days) %>% 
  pull(symbol)

keep_close <- all_close %>% 
  filter(symbol %in% symbols_keep)

### plot the closing price for all stock prices
keep_close %>% 
  ggplot(mapping = aes(x = date, y = close)) +
  geom_line(mapping = aes(group = symbol),
            alpha = 0.1) +
  theme_bw()

### find the unique days
unique_days <- keep_close %>% 
  distinct(date) %>% 
  tibble::rowid_to_column("date_id")

### merge in the unique days
keep_close_b <- keep_close %>% 
  left_join(unique_days, by = "date")

keep_close_b %>% tail()

### convert to wide format
keep_close_c <- keep_close_b %>% 
  mutate(day_number = sprintf("day_%03d", date_id)) %>% 
  dplyr::select(symbol, close, date, day_number)

keep_wf <- keep_close_c %>% 
  dplyr::select(symbol, close, day_number) %>% 
  tidyr::spread(day_number, close)

### save the long and wide format data sets
### uncomment the code below to save the data as CSVs
# keep_close_c %>% 
#   readr::write_csv("sp500_stock_close_long_format.csv", col_names = TRUE)
# 
# keep_wf %>% 
#   readr::write_csv("sp500_stock_close_wide_format.csv", col_names = TRUE)

