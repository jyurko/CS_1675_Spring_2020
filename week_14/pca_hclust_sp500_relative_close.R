### read in the long format s&p500 data and work with the 
### relative closing price, relative to the first day in the data set

### this script requires a few extra steps in order to calculate the
### relative closing prce from the first day in the data set
### those operations are provided for you using tidyverse operations

### this script also uses the caret package

library(dplyr)
library(ggplot2)

### load in the long data, first run the tidyquant script
### to get all of the data, uncomment the code below and set the file name
### to the appropriate location in your directory
# stocks_lf <- readr::read_csv("week_14/sp500_stock_close_long_format.csv", col_names = TRUE)

### which stocks have missing values
stocks_lf %>% 
  filter(is.na(close)) %>% 
  count(symbol)

symbols_remove <- stocks_lf %>% 
  filter(is.na(close)) %>% 
  count(symbol) %>% 
  pull(symbol)

### remove those symbols with missing values for simplicity
stocks_lf_b <- stocks_lf %>% 
  filter(!symbol %in% symbols_remove)

### calculate the relative change ih the close price from the
### first trading day, first check it's correct
stocks_lf_b %>% 
  group_by(symbol) %>% 
  mutate(first_close = close[which.min(date)]) %>% 
  ungroup() %>% 
  filter(date == min(date))

stocks_lf_b %>% 
  group_by(symbol) %>% 
  mutate(first_close = close[which.min(date)]) %>% 
  ungroup() %>% 
  filter(date == min(date)) %>% 
  mutate(close - first_close) %>% 
  summary()

### calculate the relative change in the price from the
### first trading day
stocks_lf_c <- stocks_lf_b %>% 
  group_by(symbol) %>% 
  mutate(first_close = close[which.min(date)]) %>% 
  ungroup() %>% 
  mutate(rel_close = (close - first_close) / first_close)

### visualize the relative closing price series for a few stocks
symbols_names <- stocks_lf_c %>% 
  distinct(symbol) %>% 
  pull()

stocks_lf_c %>% 
  filter(symbol %in% symbols_names[1:6]) %>% 
  ggplot(mapping = aes(x = date, y = rel_close)) +
  geom_line(mapping = aes(group = symbol)) +
  facet_wrap(~symbol, scales = "free_y") +
  theme_bw()

### look at 24 different stocks
stocks_lf_c %>% 
  filter(symbol %in% symbols_names[1:24]) %>% 
  ggplot(mapping = aes(x = date, y = rel_close)) +
  geom_line(mapping = aes(group = symbol)) +
  facet_wrap(~symbol, scales = "free_y") +
  theme_bw()

### look at all stocks together
stocks_lf_c %>% 
  ggplot(mapping = aes(x = date, y = rel_close)) +
  geom_line(mapping = aes(group = symbol),
            alpha = 0.2) +
  theme_bw()

### summarize the stocks relative closing price per day
stocks_lf_c %>% 
  ggplot(mapping = aes(x = date, y = rel_close)) +
  geom_boxplot(mapping = aes(group = day_number)) +
  theme_bw()

### resahpe to wide fprmat with the relative closing price
wf <- stocks_lf_c %>% 
  dplyr::select(symbol, rel_close, day_number) %>% 
  tidyr::spread(day_number, rel_close)

### confirm trading day 1 is all zero
nzv_col <- caret::nearZeroVar(wf)

nzv_col

### yes it's zero, remove
wf_ready <- wf %>% select(-nzv_col)

wf_ready %>% select(1:10) %>% head()

### look at the distributions of the relative closing price for a few days
wf_ready %>% 
  select(1:10) %>% 
  tidyr::gather(key = "key", value = "rel_close", -symbol) %>% 
  ggplot(mapping = aes(x = rel_close)) +
  geom_histogram(bins = 25) +
  facet_wrap(~key, scales = "free_y") +
  theme_bw() +
  theme(axis.text.y = element_blank())

### look at all day relative close distributions
wf_ready %>% 
  tidyr::gather(key = "key", value = "rel_close", -symbol) %>% 
  ggplot(mapping = aes(x = rel_close)) +
  geom_freqpoly(mapping = aes(group = key),
                bins = 25,
                alpha = 0.2) +
  theme_bw()

### perform PCA on the wide format relative closing price
close_pca <- prcomp(wf_ready %>% select(-symbol) %>% as.data.frame(),
                    scale. = TRUE)

library(factoextra)

factoextra::get_eigenvalue(close_pca) %>% head(25)

factoextra::fviz_screeplot(close_pca)

### look at the distributions of the PCs
close_pca$x %>% as.data.frame() %>% 
  select(1:25) %>% 
  tibble::rowid_to_column() %>% 
  tidyr::gather(key = "key", value = "pc_score", -rowid) %>% 
  mutate(pc_num = as.numeric(stringr::str_extract(key, "\\d+"))) %>% 
  ggplot(mapping = aes(x = pc_score)) +
  geom_histogram(bins = 25) +
  facet_wrap(~pc_num, scales = "free") +
  theme_bw() +
  theme(axis.text.y = element_blank())

close_pca$x %>% as.data.frame() %>% 
  select(1:25) %>% 
  tibble::rowid_to_column() %>% 
  tidyr::gather(key = "key", value = "pc_score", -rowid) %>% 
  mutate(pc_num = as.numeric(stringr::str_extract(key, "\\d+"))) %>% 
  ggplot(mapping = aes(x = as.factor(pc_num), y = pc_score)) +
  geom_boxplot() +
  theme_bw()

### scatter plot between the first 2 PCs
close_pca$x %>% as.data.frame() %>% 
  mutate(symbol = symbols_names) %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(color = "grey") +
  geom_text(mapping = aes(label = symbol),
            check_overlap = TRUE) +
  theme_bw()

### which trading days contribute to the first 8 PCs
trading_days <- stocks_lf_c %>% 
  distinct(date, day_number)

### extract the % contribution to the first 5 PCs for each trading day
### plot the contributions over time
(factoextra::get_pca(close_pca))$contrib %>% as.data.frame() %>% 
  select(1:8) %>% 
  tibble::rownames_to_column("day_number") %>% 
  tidyr::gather(key = "pc_word", value = "contrib_to_pc", -day_number) %>% 
  mutate(pc_num = stringr::str_extract(pc_word, "\\d+")) %>% 
  mutate_at("pc_num", as.numeric) %>% 
  left_join(trading_days, by = "day_number") %>% 
  ggplot(mapping = aes(x = date, y = contrib_to_pc)) +
  geom_area(mapping = aes(fill = as.factor(pc_num))) +
  ggthemes::scale_fill_colorblind("PC") +
  theme_bw()

### look at which of the original variables (columns) "actively" contribute
### to each PC..."active" is defined as being above the threshold value
### of equal contribution...visualize as a heat map
factoextra::fviz_contrib(close_pca, choice = c("var"), axes = 1)

(factoextra::get_pca(close_pca))$contrib %>% as.data.frame() %>% 
  tibble::rownames_to_column("day_number") %>% 
  tidyr::gather(key = "key", value = "contrib_to_pc", -day_number) %>% 
  mutate(day = as.numeric(stringr::str_extract(day_number, "\\d+")),
         pc = as.numeric(stringr::str_extract(key, "\\d+"))) %>% 
  filter(pc < 50) %>% 
  ggplot(mapping = aes(x = pc, y = day)) +
  geom_tile(mapping = aes(fill = contrib_to_pc > 100 * (1/length(close_pca$center)),
                          group = interaction(pc, day))) +
  scale_fill_manual("Trading day actively contributes to PC?",
                    values = c("TRUE" = "darkred",
                               "FALSE" = "grey70")) +
  theme_bw() +
  theme(legend.position = "top")

### use 14 PCs for clustering
close_pc_scores <- close_pca$x %>% as.data.frame() %>% select(1:14)

close_hclust <- hclust(d = dist(close_pc_scores), method = "ward.D2")

plot(close_hclust)

### look at cluster plot with 3 clusters
factoextra::fviz_cluster(list(data = wf_ready %>% select(-symbol), 
                              cluster = cutree(close_hclust, k = 3))) +
  theme_bw()

factoextra::fviz_cluster(list(data = wf_ready %>% select(-symbol), 
                              cluster = cutree(close_hclust, k = 6))) +
  theme_bw()

### break up the relative closing prices based on the 3 clusters
symbol_clusters <- wf_ready %>% 
  mutate(cluster_id = cutree(close_hclust, k = 3)) %>% 
  select(symbol, cluster_id)

symbol_clusters %>% count(cluster_id)

stocks_lf_c %>% 
  left_join(symbol_clusters, by = "symbol") %>% 
  ggplot(mapping = aes(x = date, y = rel_close)) +
  geom_line(mapping = aes(group = symbol),
            alpha = 0.1) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~cluster_id, labeller = "label_both") +
  theme_bw()

### summarize the relative close price in each cluster
### use ribbons to show the 25th through 75th quantiles
### (so 50% of the stocks in the cluster) and the median
### as black curve
stocks_lf_c %>% 
  left_join(symbol_clusters, by = "symbol") %>% 
  ggplot(mapping = aes(x = date, y = rel_close)) +
  stat_summary(geom = "ribbon",
               fun.ymin = function(x){quantile(x, 0.25)},
               fun.ymax = function(x){quantile(x, 0.75)},
               fill = "grey50") +
  stat_summary(geom = "line",
               fun.y = "median",
               color = "black") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~cluster_id, labeller = "label_both") +
  theme_bw()

### 