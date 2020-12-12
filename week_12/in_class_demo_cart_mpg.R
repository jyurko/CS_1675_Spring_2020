### CS 1675 Spring 2020 - in class demo
### use the mpg data set and fit with a simple CART
###
### to run this script you need the following packages
### tidyverse
### rpart
### rpart.plot

library(dplyr)
library(ggplot2)

mpg %>% glimpse()

mpg %>% 
  ggplot(mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(color = class,
                           shape = drv),
             size = 3) +
  theme_bw()

mpg %>% 
  count(class)

mpg %>% count(drv)

### fit a CART for the hwy mpg

library(rpart)
library(rpart.plot)

### create a decision stump based on the displ, class, and drv
mod1 <- rpart(hwy ~ displ + class + drv,
              data = mpg,
              method = "anova",
              control = rpart.control(maxdepth = 1))

rpart.plot(mod1)

mean(mpg$hwy)

### define a prediction grid to help visualize model behavior
test_grid <- expand.grid(displ = seq(min(mpg$displ), max(mpg$displ), length.out = 25),
                         class = unique(mpg$class),
                         drv = unique(mpg$drv),
                         KEEP.OUT.ATTRS = FALSE,
                         stringsAsFactors = TRUE) %>% 
  as.data.frame() %>% tbl_df()

### make predictions on the test set with the simple CART
test_grid %>% 
  mutate(pred_tree = predict(mod1, test_grid)) %>% 
  ggplot(mapping = aes(x = displ)) +
  geom_point(data = mpg,
             mapping = aes(x = displ, y = hwy,
                           color = drv),
             size = 2, alpha = 0.5) +
  geom_line(mapping = aes(y = pred_tree,
                          group = interaction(class, drv),
                          color = drv,
                          linetype = drv),
            size = 1.15) +
  facet_grid(. ~ class) +
  ggthemes::scale_color_colorblind() +
  scale_linetype_discrete() +
  theme_bw() +
  theme(legend.position = "top")

### now create a slightly more complex tree with 2 splits or
### 4 terminal nodes
mod2 <- rpart(hwy ~ displ + class + drv, 
              data = mpg,
              method = "anova",
              control = rpart.control(maxdepth = 2))

rpart.plot(mod2)

### make predictions to visualize the model behavior
test_grid %>% 
  mutate(pred_tree = predict(mod2, test_grid)) %>% 
  ggplot(mapping = aes(x = displ)) +
  geom_point(data = mpg,
             mapping = aes(x = displ, y = hwy,
                           color = drv),
             size = 2, alpha = 0.5) +
  geom_line(mapping = aes(y = pred_tree,
                          group = interaction(class, drv),
                          color = drv,
                          linetype = drv),
            size = 1.15) +
  facet_grid(. ~ class) +
  ggthemes::scale_color_colorblind() +
  scale_linetype_discrete() +
  theme_bw() +
  theme(legend.position = "top")

### build a deep tree
mod3 <- rpart(hwy ~ displ + class + drv,
              data = mpg,
              method = "anova",
              control = rpart.control(cp = 0))

rpart.plot(mod3)

plotcp(mod3)

### make predictions with the deep tree
test_grid %>% 
  mutate(pred_tree = predict(mod3, test_grid)) %>% 
  ggplot(mapping = aes(x = displ)) +
  geom_point(data = mpg,
             mapping = aes(x = displ, y = hwy,
                           color = drv),
             size = 2, alpha = 0.5) +
  geom_line(mapping = aes(y = pred_tree,
                          group = interaction(class, drv),
                          color = drv,
                          linetype = drv),
            size = 1.15) +
  facet_grid( . ~ class, labeller = "label_both") +
  ggthemes::scale_color_colorblind() +
  scale_linetype_discrete() +
  theme_bw() +
  theme(legend.position = "top")

### so actually prune the tree based on the cp value within
### one standard error of the lowest cross-validation error
mod_prune <- prune(mod3, 0.02)

rpart.plot(mod_prune)

### make predictions with the optimally pruned tree

test_grid %>% 
  mutate(pred_tree = predict(mod_prune, test_grid)) %>% 
  ggplot(mapping = aes(x = displ)) +
  geom_point(data = mpg,
             mapping = aes(x = displ, y = hwy,
                           color = drv),
             size = 2, alpha = 0.5) +
  geom_line(mapping = aes(y = pred_tree,
                          group = interaction(class, drv),
                          color = drv,
                          linetype = drv),
            size = 1.15) +
  facet_grid( . ~ class, labeller = "label_both") +
  ggthemes::scale_color_colorblind() +
  scale_linetype_discrete() +
  theme_bw() +
  theme(legend.position = "top")
