### practice using neural networks for classification
### to use this script you need the following packages
###
### tidyverse
### mlbench
### caret
### neuralnet
### nnet
### NeuralNetTools
### plotROC

library(dplyr)
library(ggplot2)

### use ionosphere data set for practice

data("Ionosphere", package = "mlbench")

### remove first 2 variables turn the class into a factor
my_ion <- Ionosphere %>% 
  select(-V1, -V2) %>% 
  mutate(Class = factor(Class, levels = c("good", "bad")))

### use the neuralnet package to fit the model

library(neuralnet)

### try a simple single layer with a few hidden units
set.seed(41231)
mod_1layer_small <- neuralnet(Class ~ .,
                              data = my_ion,
                              hidden = 3,
                              err.fct = 'ce',
                              act.fct = 'logistic',
                              linear.output = FALSE,
                              likelihood = TRUE)

plot(mod_1layer_small, rep = "best", show.weights = FALSE)

mod_1layer_small$result.matrix %>% as.data.frame()

### a single hidden layer with more hidden units
set.seed(41231)
mod_1layer <- neuralnet(Class ~ .,
                        data = my_ion,
                        hidden = 15,
                        err.fct = 'ce',
                        act.fct = 'logistic',
                        linear.output = FALSE,
                        likelihood = TRUE)

plot(mod_1layer, rep = "best", show.weights = FALSE)

### use 2 hidden layers with just a small number of hidden units
set.seed(41231)
mod_2layers_small <- neuralnet(Class ~ .,
                               data = my_ion,
                               hidden = c(3, 3),
                               err.fct = 'ce',
                               act.fct = 'logistic',
                               linear.output = FALSE,
                               likelihood = TRUE)

plot(mod_2layers_small, rep = "best", show.weights = TRUE)

### use 2 hidden layers with more hidden units
set.seed(41231)
mod_2layers <- neuralnet(Class ~ .,
                         data = my_ion,
                         hidden = c(15, 7),
                         err.fct = 'ce',
                         act.fct = 'logistic',
                         linear.output = FALSE,
                         likelihood = TRUE)

plot(mod_2layers, rep = "best", show.weights = FALSE, intercept = FALSE)

### use 3 hidden layers with a small number of hidden units
set.seed(41231)
mod_3layers_small <- neuralnet(Class ~ .,
                               data = my_ion,
                               hidden = c(3, 3, 2),
                               err.fct = 'ce',
                               act.fct = 'logistic',
                               linear.output = FALSE,
                               likelihood = TRUE)

plot(mod_3layers_small, rep = "best", show.weights = FALSE, intercept = FALSE)

### use 3 hidden layers with more hidden units
set.seed(41231)
mod_3layers <- neuralnet(Class ~ .,
                         data = my_ion,
                         hidden = c(15, 7, 5),
                         err.fct = 'ce',
                         act.fct = 'logistic',
                         linear.output = FALSE,
                         likelihood = TRUE)

plot(mod_3layers, rep = "best", show.weights = FALSE, intercept = FALSE)

### use AIC / BIC to determine the simplest or best model to use
### which guards against overfitting

mod_1layer_small$result.matrix %>% as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  tbl_df() %>% 
  slice(1:5) %>% 
  tidyr::spread(rowname, V1) %>% 
  mutate(layer1 = 3, layer2 = 0, layer3 = 0) %>% 
  bind_rows(mod_1layer$result.matrix %>% as.data.frame() %>% 
              tibble::rownames_to_column() %>% 
              tbl_df() %>% 
              slice(1:5) %>% 
              tidyr::spread(rowname, V1) %>% 
              mutate(layer1 = 15, layer2 = 0, layer3 = 0)) %>% 
  bind_rows(mod_2layers_small$result.matrix %>% as.data.frame() %>% 
              tibble::rownames_to_column() %>% 
              tbl_df() %>% 
              slice(1:5) %>% 
              tidyr::spread(rowname, V1) %>% 
              mutate(layer1 = 3, layer2 = 3, layer3 = 0)) %>% 
  bind_rows(mod_2layers$result.matrix %>% as.data.frame() %>% 
              tibble::rownames_to_column() %>% 
              tbl_df() %>% 
              slice(1:5) %>% 
              tidyr::spread(rowname, V1) %>% 
              mutate(layer1 = 15, layer2 = 7, layer3 = 0)) %>% 
  bind_rows(mod_3layers_small$result.matrix %>% as.data.frame() %>% 
              tibble::rownames_to_column() %>% 
              tbl_df() %>% 
              slice(1:5) %>% 
              tidyr::spread(rowname, V1) %>% 
              mutate(layer1 = 3, layer2 = 3, layer3 = 2)) %>% 
  bind_rows(mod_3layers$result.matrix %>% as.data.frame() %>% 
              tibble::rownames_to_column() %>% 
              tbl_df() %>% 
              slice(1:5) %>% 
              tidyr::spread(rowname, V1) %>% 
              mutate(layer1 = 15, layer2 = 7, layer3 = 5)) %>% 
  arrange(aic, bic)

### use cross-validation to assess performance on hold-out test sets

library(caret)

### we will use the nnet package to fit the models with caret
### to tune the number of hidden units in layer 1 and the 
### amount of WEIGHT DECAY

### use 5-fold cross-validation
ctrl_cv05 <- trainControl(method = "cv", number = 5)

### try out different combinations of the number of hidden units
### refered as `size` and the amount of regularization via `decay`
nnet_grid <- expand.grid(size = c(3, 4, 10, 15),
                         decay = exp(seq(-6, 3, length.out = 31)))

set.seed(31311)
fit_nnet <- train(Class ~ ., data = my_ion,
                  method = "nnet",
                  metric = "Accuracy",
                  tuneGrid = nnet_grid,
                  preProc = c("center", "scale"),
                  trControl = ctrl_cv05,
                  trace=FALSE)

fit_nnet

### access nnet result directly
fit_nnet$finalModel

plot(fit_nnet, xTrans = log)

### the best tuning parameters - the number of hidden units and the
### the regularization strength are contained in the bestTune field
fit_nnet$bestTune

### we can visualize the nnet model with the NeuralNetTools package
library(NeuralNetTools)

plotnet(fit_nnet$finalModel)

### let's look at the confusion matrix averaged over teh different
### folds
confusionMatrix.train(fit_nnet)

### tune the neural network based upon the ROC curve
### maximize the Area under the ROC curve (AUC)

ctrl_cv05_roc <- trainControl(method = "cv", number = 5,
                              summaryFunction = twoClassSummary,
                              classProbs = TRUE,
                              savePredictions = TRUE)

set.seed(31331)
fit_nnet_roc <- train(Class ~ ., data = my_ion,
                      method = "nnet",
                      metric = "ROC",
                      tuneGrid = nnet_grid,
                      preProc = c("center", "scale"),
                      trControl = ctrl_cv05_roc,
                      trace=FALSE)

fit_nnet_roc

fit_nnet_roc$bestTune

confusionMatrix.train(fit_nnet_roc)

plot(fit_nnet_roc, xTrans = log)

### let's visualize the ROC curve, let's compare 3 specific
### weight decay values
decay_focus <- nnet_grid %>% distinct(decay) %>% 
  filter(decay %in% c(min(nnet_grid$decay), 
                      fit_nnet_roc$bestTune$decay,
                      max(nnet_grid$decay))) %>% 
  pull(decay)

library(plotROC)

fit_nnet_roc$pred %>% tbl_df() %>% 
  filter(decay %in% decay_focus,
         size %in% c(3, 15)) %>% 
  ggplot(mapping = aes(m = good,
                       d = ifelse(obs == "good",
                                  1,
                                  0))) +
  geom_roc(cutoffs.at = 0.5,
           mapping = aes(color = Resample)) +
  geom_roc(cutoffs.at = 0.5) +
  coord_equal() +
  facet_grid(size ~ decay, labeller = "label_both") +
  style_roc()

### compare the weight decay values for the same fold
fit_nnet_roc$pred %>% tbl_df() %>% 
  filter(decay %in% decay_focus,
         size %in% c(3, 15)) %>% 
  ggplot(mapping = aes(m = good,
                       d = ifelse(obs == "good",
                                  1,
                                  0))) +
  geom_roc(cutoffs.at = 0.5,
           mapping = aes(color = as.factor(decay))) +
  coord_equal() +
  facet_grid(size ~ Resample, labeller = "label_both") +
  style_roc() +
  theme(legend.position = "top")

### compare the different numbers of hidden units averaged
### over the 5 folds for the same regularization strength
fit_nnet_roc$pred %>% tbl_df() %>% 
  filter(decay %in% decay_focus,
         size %in% c(3, 15)) %>% 
  ggplot(mapping = aes(m = good,
                       d = ifelse(obs == "good",
                                  1,
                                  0))) +
  geom_roc(cutoffs.at = 0.5,
           mapping = aes(color = as.factor(size))) +
  coord_equal() +
  facet_grid(. ~ decay, labeller = "label_both") +
  style_roc() +
  theme(legend.position = "top")
