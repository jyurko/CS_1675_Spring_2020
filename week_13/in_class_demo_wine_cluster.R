### wine data set example
### you will need the following packages to run this script
###
### tidyverse (you should have)
### factoextra
### gridExtra
### useful
### cluster (you probably have, double check list of packages)
### GGally

library(dplyr)

library(ggplot2)

### set the path for the wine dataset
wine_url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"

wine <- readr::read_csv(wine_url,
                        col_names = c("Cultivar", "Alcohol", "Malic_acid",
                                      "Ash", "Alcalinity_of_ash",
                                      "Magnesium", "Total_phenols",
                                      "Flavanoids", "Nonflavanoid_phenols",
                                      "Proanthocyanin", "Color_intensity",
                                      "Hue", "OD280_OD315_of_diluted_wines",
                                      "Proline"))

### show the glimpse
wine %>% glimpse()

### show the summary statistics -- the variables have
### different scales!
wine %>% summary()

### the Cultivar variable is actually categorical, even though
### it is listed as a numeric, show the levels below
wine %>% count(Cultivar)

### show the histograms
wine %>% 
  tibble::rowid_to_column("obs_id") %>% 
  tidyr::gather(key = "key", value = "value", -obs_id, -Cultivar) %>% 
  ggplot(mapping = aes(x = value)) +
  geom_histogram(bins = 25) +
  facet_wrap(~key, scales = "free") +
  theme_bw()

### standardize the variables, but remove the Cultivar
### categorical variable
### use the `scale()` function

wine_df <- wine %>% 
  select(-Cultivar) %>% 
  as.data.frame() %>% 
  scale(center = TRUE, scale = TRUE) %>% 
  as.data.frame() %>% tbl_df()

### histograms of the standardized variables
wine_df %>% 
  tibble::rowid_to_column("obs_id") %>% 
  tidyr::gather(key = "key", value = "value", -obs_id) %>% 
  ggplot(mapping = aes(x = value)) +
  geom_histogram(bins = 25) +
  facet_wrap(~key, scales = "free") +
  theme_bw()


### perform the kmeans clustering
set.seed(1234)
k2 <- kmeans(x = wine_df,
             centers = 2,
             iter.max = 50,
             nstart = 25)
### look at the structure
str(k2)

### cluster assignments
unique(k2$cluster)

library(factoextra)

fviz_cluster(k2, data = wine_df)

set.seed(71421)
### try out several differet number of numbers
k3 <- kmeans(x = wine_df, centers = 3, iter.max = 50, nstart = 25)
k4 <- kmeans(x = wine_df, centers = 4, iter.max = 50, nstart = 25)
k5 <- kmeans(x = wine_df, centers = 5, iter.max = 50, nstart = 25)

### visualize the cluster results

library(gridExtra)

p2 <- fviz_cluster(k2, geom = "point", data = wine_df) + ggtitle("K = 2")
p3 <- fviz_cluster(k3, geom = "point", data = wine_df) + ggtitle("K = 3")
p4 <- fviz_cluster(k4, geom = "point", data = wine_df) + ggtitle("K = 4")
p5 <- fviz_cluster(k5, geom = "point", data = wine_df) + ggtitle("K = 5")

grid.arrange(p2, p3, p4, p5, nrow = 2)

### optimal number of clusters with the knee bend
set.seed(81231)
fviz_nbclust(x = wine_df,
             FUNcluster = kmeans,
             method = "wss")

### now use hartigens rule

library(useful)

k_hart <- FitKMeans(wine_df,
                    max.clusters = 20,
                    nstart = 25,
                    seed = 81231)

PlotHartigan(k_hart)

### use the gap statistic

library(cluster)

set.seed(32113)
gap_stat <- clusGap(wine_df,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

fviz_gap_stat(gap_stat)

### how do the identified clusters from kmeans
### clustering compare to the Cultvar values?
### use 3 clusters, are any of the Cultivar values
### uniquely associated with the clusters?
wine %>% 
  mutate(cluster_id = k3$cluster) %>% 
  count(Cultivar, cluster_id) %>% 
  ggplot(mapping = aes(x = as.factor(Cultivar),
                       y = as.factor(cluster_id))) +
  geom_tile(mapping = aes(fill = n),
            color = "black") +
  geom_text(mapping = aes(label = n),
            color = "white") +
  theme_bw()

### look at all pairwise scatter plots using GGally
### color based on the 3 identified clusters
### this line of code may take a bit to complete
### there are a lot of plots to make!!!!
wine %>% 
  select(-Cultivar) %>% 
  mutate(cluster_id = k3$cluster) %>% 
  GGally::ggpairs(columns = 1:13,
                  mapping = aes(color = as.factor(cluster_id)))
  

### now use the hierarchical clustering
### hclust()
wine_dist <- dist(wine_df, method = "euclidean")

factoextra::fviz_dist(wine_dist)

# ?factoextra::get_dist

wineH1 <- hclust(d = dist(wine_df), method = "single")

plot(wineH1)

wineH2 <- hclust(d = dist(wine_df), method = "complete")

plot(wineH2)

wineH3 <- hclust(d = dist(wine_df), method = "average")

plot(wineH3)
plot(wineH3, hang = -1)

wineH4 <- hclust(d = dist(wine_df), method = "ward.D2")

plot(wineH4)

plot(wineH4, labels = FALSE)

plot(wineH4)
rect.hclust(wineH4, k = 3, border = 2:4)

plot(wineH4)
rect.hclust(wineH4, h = 30, border = 5:6)

### extract the cluster IDs from the WARD method
cutree(wineH4, k = 3)

### compare the clusters to the cultvar
wine %>% 
  mutate(cluster_id = cutree(wineH4, k = 3)) %>% 
  count(Cultivar, cluster_id) %>% 
  ggplot(mapping = aes(x = as.factor(Cultivar),
                       y = as.factor(cluster_id))) +
  geom_tile(mapping = aes(fill = n),
            color = "black") +
  geom_text(mapping = aes(label = n),
            color = "white")

### cluster plot
factoextra::fviz_cluster(list(data = wine_df, cluster = cutree(wineH4, k = 3)))
