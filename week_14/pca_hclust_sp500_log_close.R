### practice using PCA on the wide format stock prices
###
### this script takes the log-transform of the closing price to demonstrate
### removing skew from variables in the data set before using PCA

library(dplyr)
library(ggplot2)

### load in the long and wide format data, first run the tidyquant script
### to get all of the data, uncomment the code below and set the file name
### to the appropriate location in your directory
# stocks_lf <- readr::read_csv("week_14/sp500_stock_close_long_format.csv", col_names = TRUE)

# stocks_wf <- readr::read_csv("week_14/sp500_stock_close_wide_format.csv", col_names = TRUE)

### plot the time series stock prices for a few stock prices

stocks_lf %>% 
  filter(symbol %in% stocks_wf$symbol[1:6]) %>% 
  ggplot(mapping = aes(x = date, y = close)) +
  geom_line(mapping = aes(group = symbol)) +
  facet_wrap(~symbol, scales = "free_y") +
  theme_bw()

### look at all stock prices
stocks_lf %>% 
  filter(symbol %in% stocks_wf$symbol[1:24]) %>% 
  ggplot(mapping = aes(x = date, y = close)) +
  geom_line(mapping = aes(group = symbol)) +
  facet_wrap(~symbol, scales = "free_y") +
  theme_bw()

### look at all prices
stocks_lf %>% 
  ggplot(mapping = aes(x = date, y = close)) +
  geom_line(mapping = aes(group = symbol),
            alpha = 0.1) +
  theme_bw()

### check if any of the columns have no variance
caret::nearZeroVar(stocks_wf %>% select(-symbol))

### check for missing values
stocks_wf %>% 
  na.omit() %>% 
  dim()

### which stocks have missing values
stocks_lf %>% 
  filter(is.na(close)) %>% 
  count(symbol)

symbols_remove <- stocks_lf %>% 
  filter(is.na(close)) %>% 
  count(symbol) %>% 
  pull(symbol)

### remove those symbols with missing values
stocks_wf_b <- stocks_wf %>% 
  filter(!symbol %in% symbols_remove)

### look at the distribution of a few days
stocks_wf_b %>% 
  select(1:10) %>% 
  tidyr::gather(key = "key", value = "value", -symbol) %>% 
  ggplot(mapping = aes(x = value)) +
  geom_histogram(bins = 25) +
  geom_rug() +
  facet_wrap(~key, scales = "free") +
  theme_bw() +
  theme(axis.text.y = element_blank())

### try and reduce the skew with the log transform of the closing price
stocks_wf_b %>% 
  select(1:10) %>% 
  tidyr::gather(key = "key", value = "value", -symbol) %>% 
  ggplot(mapping = aes(x = log(value))) +
  geom_histogram(bins = 25) +
  geom_rug() +
  facet_wrap(~key, scales = "free") +
  theme_bw() +
  theme(axis.text.y = element_blank())

### apply the log-transformation to all stock closing prices
log_stocks_wf <- stocks_wf_b %>% 
  select(-symbol) %>% 
  mutate_all(log) %>% 
  mutate(symbol = stocks_wf_b$symbol) %>% 
  select(names(stocks_wf_b))

### look at the log-stock closing price
stocks_lf %>% 
  ggplot(mapping = aes(x = date, y = log(close))) +
  geom_line(mapping = aes(group = symbol),
            alpha = 0.1) +
  theme_bw()

### perform PCA on the wide-format stock log closing prices
### standardize them
stocks_pca <- prcomp(log_stocks_wf %>% select(-symbol) %>% as.data.frame(),
                     scale. = TRUE)

library(factoextra)

factoextra::fviz_screeplot(stocks_pca)

factoextra::get_eigenvalue(stocks_pca) %>% head()

### look at the distrbutions of the first few PCs
stocks_pca$x %>% as.data.frame() %>% 
  select(1:5) %>% 
  tibble::rowid_to_column() %>% 
  tidyr::gather(key = "pc_name", value = "pc_value", -rowid) %>% 
  ggplot(mapping = aes(x = pc_name, y = pc_value)) +
  geom_boxplot() +
  theme_bw()

stocks_pca$x %>% as.data.frame() %>% 
  select(2:5) %>% 
  tibble::rowid_to_column() %>% 
  tidyr::gather(key = "pc_name", value = "pc_value", -rowid) %>% 
  ggplot(mapping = aes(x = pc_name, y = pc_value)) +
  geom_boxplot() +
  theme_bw()

### plot the observations in the PC space
stocks_pca$x %>% as.data.frame() %>% 
  mutate(symbol = log_stocks_wf$symbol) %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point() +
  geom_text(mapping = aes(label = symbol),
            check_overlap = TRUE) +
  theme_bw()

### look at the stocks with the very negatvie PC1 values
stocks_name_view <- stocks_pca$x %>% as.data.frame() %>% 
  mutate(symbol = log_stocks_wf$symbol) %>% 
  filter(PC1 < -50) %>% 
  pull(symbol)

set_symbol_alpha <- c(rep(1, length(stocks_name_view)), 0.05)
names(set_symbol_alpha) <- c(stocks_name_view, "other")

stocks_lf %>% 
  mutate(viz_symbol = ifelse(symbol %in% stocks_name_view,
                             symbol,
                             "other")) %>% 
  ggplot(mapping = aes(x = date, y = log(close))) +
  geom_line(mapping = aes(group = symbol,
                          color = viz_symbol,
                          alpha = viz_symbol)) +
  scale_alpha_manual(guide = FALSE,
                     values = set_symbol_alpha) +
  scale_color_discrete("") +
  theme_bw() +
  theme(legend.position = "top") +
  guides(color = guide_legend(nrow = 1, override.aes = list(alpha = 1.0)))

### which trading days contribute to which of the first 5 PCs?
trading_days <- stocks_lf %>% 
  distinct(date, day_number)

### extract the % contribution to the first 5 PCs for each trading day
### plot the contributions over time
(factoextra::get_pca(stocks_pca))$contrib %>% as.data.frame() %>% 
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

### perform hierarchical clustering on the first 8 PCs
stocks_pc_scores <- stocks_pca$x %>% as.data.frame() %>% select(1:8)

stocks_hclust <- hclust(d = dist(stocks_pc_scores), method = "ward.D2")

plot(stocks_hclust)

### look at the cluster plot
factoextra::fviz_cluster(list(data = log_stocks_wf %>% select(-symbol), 
                              cluster = cutree(stocks_hclust, k = 3)))

### plot the log stock prices broken up by clusters
symbol_clusters <- log_stocks_wf %>% 
  mutate(cluster_id = cutree(stocks_hclust, k = 3)) %>% 
  select(symbol, cluster_id)

symbol_clusters %>% 
  count(cluster_id)

stocks_lf %>% 
  left_join(symbol_clusters, by = "symbol") %>% 
  ggplot(mapping = aes(x = date, y = log(close))) +
  geom_line(mapping = aes(group = symbol),
            alpha = 0.1) +
  facet_wrap(~cluster_id, labeller = "label_both") +
  theme_bw()

### remove the 3rd cluster
log_stocks_reduce_wf <- log_stocks_wf %>% 
  left_join(symbol_clusters, by = "symbol") %>% 
  filter(cluster_id != 3) %>% 
  select(-cluster_id)

### repeat the PCA and hclust
stocks_reduce_pca <- prcomp(log_stocks_reduce_wf %>% select(-symbol) %>% as.data.frame(),
                            scale. = TRUE)

factoextra::fviz_screeplot(stocks_reduce_pca)

factoextra::get_eigenvalue(stocks_reduce_pca) %>% head(11)

stocks_reduce_pca$x %>% as.data.frame() %>% 
  mutate(symbol = log_stocks_reduce_wf$symbol) %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point() +
  geom_text(mapping = aes(label = symbol),
            check_overlap = TRUE) +
  theme_bw()

### look at the contribution to the first few PCs overtime
(factoextra::get_pca(stocks_reduce_pca))$contrib %>% as.data.frame() %>% 
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

### perform hierarchical clustering on the first 8 PCs
stocks_reduce_pc_scores <- stocks_reduce_pca$x %>% as.data.frame() %>% select(1:8)

stocks_reduce_hclust <- hclust(d = dist(stocks_reduce_pc_scores), method = "ward.D2")

plot(stocks_reduce_hclust)

### look at the cluster plot with 4 clusters
factoextra::fviz_cluster(list(data = log_stocks_reduce_wf %>% select(-symbol), 
                              cluster = cutree(stocks_reduce_hclust, k = 4)))
