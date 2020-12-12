### regression and classification example with random forests and
### boosted trees using caret

### packages needed to run this script
### tidyverse (you should have)
### caret
### plotROC (for visualizing ROC curves)
###
### caret you tell you what other packages you need for fitting models

library(dplyr)
library(ggplot2)

library(caret)

### if you need to download the concrete data set please uncomment the 
### following three lines of code
# uci_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/concrete/compressive/Concrete_Data.xls"
# download.file(uci_url, destfile = "concrete_data.xls", mode = "wb")
# concrete <- readxl::read_excel("concrete_data.xls", col_names = TRUE)

### if you already downloaded it, just specify the location in the file path
### below. as an example my relative path is shown
concrete <- readxl::read_excel("week_04/concrete_data.xls", col_names = TRUE)

### change the variable names
my_concrete <- concrete %>% 
  purrr::set_names(c("cement", "slag", "flyash", "water", "superplast", "coarseagg",
                     "fineagg", "age", "strength"))

my_concrete %>% glimpse()

my_concrete %>% summary()

### fit a random forest model without tuning mtry
ctrl_nocv <- trainControl(method = "none")

### use a value of mtry=2
set.seed(12341)
fit_rf_nocv_2 <- train(strength ~ .,
                       data = my_concrete,
                       method = "rf",
                       metric = "RMSE",
                       trControl = ctrl_nocv,
                       tuneGrid = expand.grid(mtry = 2),
                       importance = TRUE)

fit_rf_nocv_2

fit_rf_nocv_2$finalModel

### plot the OOB error as a function of the number
### of trees
tibble::tibble(
  n_tree = 1:length(fit_rf_nocv_2$finalModel$mse),
  oob_mse = fit_rf_nocv_2$finalModel$mse
) %>% 
  mutate(mtry = 2) %>% 
  ggplot(mapping = aes(x = n_tree, y = oob_mse)) +
  geom_line(size = 1.15) +
  facet_grid(. ~ mtry, labeller = "label_both") +
  labs(x = "number of trees", y = "OOB error") +
  theme_bw()

### use a value of mtry=4
set.seed(12341)
fit_rf_nocv_4 <- train(strength ~ .,
                       data = my_concrete,
                       method = "rf",
                       metric = "RMSE",
                       trControl = ctrl_nocv,
                       tuneGrid = expand.grid(mtry = 4),
                       importance = TRUE)

fit_rf_nocv_4$finalModel

### try again with the max number of inputs: 8
set.seed(12341)
fit_rf_nocv_8 <- train(strength ~ .,
                       data = my_concrete,
                       method = "rf",
                       metric = "RMSE",
                       trControl = ctrl_nocv,
                       tuneGrid = expand.grid(mtry = 8),
                       importance = TRUE)

fit_rf_nocv_8$finalModel

### compare the 3 specific values based on the OOB error
tibble::tibble(
  n_tree = 1:length(fit_rf_nocv_2$finalModel$mse),
  oob_mse = fit_rf_nocv_2$finalModel$mse
) %>% 
  mutate(mtry = 2) %>% 
  bind_rows(tibble::tibble(n_tree = 1:length(fit_rf_nocv_4$finalModel$mse),
                           oob_mse = fit_rf_nocv_4$finalModel$mse) %>% 
              mutate(mtry = 4)) %>% 
  bind_rows(tibble::tibble(n_tree = 1:length(fit_rf_nocv_8$finalModel$mse),
                           oob_mse = fit_rf_nocv_8$finalModel$mse) %>% 
              mutate(mtry = 8)) %>% 
  ggplot(mapping = aes(x = n_tree,
                       y = oob_mse)) +
  geom_line(size = 1.15,
            mapping = aes(color = as.factor(mtry),
                          linetype = as.factor(mtry))) +
  ggthemes::scale_color_colorblind("mtry") +
  scale_linetype_discrete("mtry") +
  labs(x = "number of trees",
       y = "OOB error") +
  theme_bw() +
  theme(legend.position = "top")

### specify cross-validation to tune mtry
ctrl_k05 <- trainControl(method = "cv", number = 5)

### use cross-validation to tune mtry
set.seed(12341)
fit_rf_cv05 <- train(strength ~ .,
                     data = my_concrete,
                     method = "rf",
                     metric = "RMSE",
                     trControl = ctrl_k05,
                     tuneGrid = expand.grid(mtry = seq(2, 8, by = 1)),
                     importance = TRUE)

### print out the results
fit_rf_cv05

### look at the RMSE summaries including the standard error on the averages
fit_rf_cv05$results %>% tbl_df() %>% 
  ggplot(mapping = aes(x = mtry)) +
  geom_linerange(mapping = aes(ymin = RMSE - RMSESD/sqrt(5),
                               ymax = RMSE + RMSESD/sqrt(5),
                               group = mtry)) +
  geom_point(mapping = aes(y = RMSE)) +
  theme_bw()

### now fit a boosted tree with GBM, use the default tuning
### grid for now
set.seed(12341)
fit_gbm_cv05 <- train(strength ~ .,
                      data = my_concrete,
                      method = "gbm",
                      metric = "RMSE",
                      trControl = ctrl_k05)

fit_gbm_cv05

plot(fit_gbm_cv05)

# gbm::gbm.perf(fit_gbm_cv05$finalModel, oobag.curve = TRUE)

### now fit a boosted tree model with XGBOOST
set.seed(12341)
fit_xgb_cv05 <- train(strength ~ .,
                      data = my_concrete,
                      method = "xgbTree",
                      metric = "RMSE",
                      trControl = ctrl_k05,
                      importance = TRUE)

### print out the results - there are lot of tuning parameter combinations!
fit_xgb_cv05

### visualize the RMSE cross-validation performance per tuning parameter
plot(fit_xgb_cv05)

### compare the performance of the tuned random forest model with the
### tuned boosted trees, however let's have some context
### by comparing the performance relative to a simpler linear model
### fit a regularized model with elastic net penalty of a linear
### model with all pair-wise interactions

### notice that unlike the tree-based methods the inputs are preprocessed
### by calling the `preProc` argument

### just use a default tuning grid for elastic net
set.seed(12341)
fit_glmnet_pairs <- train(strength ~ (.)^2, data = my_concrete,
                          method = "glmnet",
                          metric = "RMSE",
                          preProc = c("center", "scale"),
                          trControl = ctrl_k05)

fit_glmnet_pairs

### let's now compare all 3 models
concrete_model_compare <- resamples(list(GLMNET = fit_glmnet_pairs,
                                         RF = fit_rf_cv05,
                                         XGB = fit_xgb_cv05,
                                         GBM = fit_gbm_cv05))

### extract all of the resample fold performance metrics per model
concrete_model_compare_lf <- concrete_model_compare$values %>% tbl_df() %>% 
  tidyr::gather(key = "key", value = "metric_value", -Resample) %>% 
  tidyr::separate(key,
                  c("model_name", "metric_name"),
                  sep = "~")

### visualize the performance metrics sumamries, show the individual
### fold results with markers and the cross-validation average
### and standard error, which model is better?
concrete_model_compare_lf %>% 
  ggplot(mapping = aes(x = model_name, y = metric_value)) +
  geom_point() +
  stat_summary(fun.data = "mean_se",
               color = "red",
               fun.args = list(mult = 1)) +
  coord_flip() +
  facet_grid( . ~ metric_name, scales = "free_x") +
  theme_bw()

### show just the tree based methods
concrete_model_compare_lf %>% 
  filter(model_name != "GLMNET") %>% 
  ggplot(mapping = aes(x = model_name, y = metric_value)) +
  geom_point() +
  stat_summary(fun.data = "mean_se",
               color = "red",
               fun.args = list(mult = 1)) +
  coord_flip() +
  facet_grid( . ~ metric_name, scales = "free_x") +
  theme_bw()

### -- ### -- ###

### binary classification example with IONOSPHERE data set
data("Ionosphere", package = "mlbench")

### remove the first 2 variables, turn the Class into a factor
my_ion <- Ionosphere %>% 
  select(-V1, -V2) %>% 
  mutate(Class = factor(Class, levels = c("good", "bad")))

### set the grid of mtry values to consider
rf_grid <- expand.grid(mtry = seq(2, 30, by = 4))

### identify the optimal value of mtry based on accuracy
set.seed(4321)
fit_rf_ion <- train(Class ~ ., data = my_ion,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = ctrl_k05,
                    tuneGrid = rf_grid,
                    importance = TRUE)

### print the results
fit_rf_ion

### how does the tuned value compare to the default value?
sqrt(32)

### plot the results
plot(fit_rf_ion)

### why is it better to use a small value for mtry even though
### there are over 30 inputs? consider the correlations between
### inputs!
my_ion %>% 
  select(-Class) %>% 
  cor() %>% 
  corrplot::corrplot(type = "upper", method = "square")

### reorder the inputs to make it easier to see highly correlated
### inputs grouped together
my_ion %>% 
  select(-Class) %>% 
  cor() %>% 
  corrplot::corrplot(type = "upper", method = "square",
                     order = "hclust")

### scatter plot between two of teh inputs as a check,
### include the best fit line between as a check
my_ion %>% 
  ggplot(mapping = aes(x = V21, y = V13)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ x) +
  theme_bw()

### accuracy performance metrics
confusionMatrix.train(fit_rf_ion)

### changing from Accuracy to ROC - maximizing the AUC
### need to create a new trainControl function call
### and include specifics for the ROC curve
ctrl_k05_roc <- trainControl(method = "cv", number = 5,
                             summaryFunction = twoClassSummary,
                             classProbs = TRUE,
                             savePredictions = TRUE)

### fit the random forest model
set.seed(4321)
fit_rf_ion_roc <- train(Class ~ ., data = my_ion,
                        method = "rf",
                        metric = "ROC",
                        trControl = ctrl_k05_roc,
                        tuneGrid = rf_grid,
                        importance = TRUE)

### print the results
fit_rf_ion_roc

### plot the results...is the behavior different
### from when we were focused on accuracy?
plot(fit_rf_ion_roc)

### check the accuracy
confusionMatrix.train(fit_rf_ion_roc)

### plot the ROC curve, first look at how the predictions
### on the fold test sets are structured
fit_rf_ion_roc$pred %>% tbl_df()

### plot the ROC curve with the plotROC
library(plotROC)

### need to follow the specific requirements of the plotROC
### package to create the ROC curves
### has a special set of aesthetics and geom
fit_rf_ion_roc$pred %>% tbl_df() %>% 
  filter(mtry %in% c(2, 10, 18, 26)) %>% 
  ggplot(mapping = aes(m = good,
                       d = ifelse(obs == "good",
                                  1, 
                                  0))) +
  geom_roc(cutoffs.at = 0.5,
           mapping = aes(color = as.factor(mtry))) +
  coord_equal() +
  style_roc()

### look at the values per fold
fit_rf_ion_roc$pred %>% tbl_df() %>% 
  filter(mtry %in% c(2, 10, 18, 26)) %>% 
  ggplot(mapping = aes(m = good,
                       d = ifelse(obs == "good",
                                  1, 
                                  0))) +
  geom_roc(cutoffs.at = 0.5,
           mapping = aes(color = Resample)) +
  facet_wrap( ~ mtry, labeller = "label_both") +
  coord_equal() +
  style_roc()

### variable importance rankings, check when variables are the most
### important
plot(varImp(fit_rf_ion_roc))

### use a boosted tree classifer with GBM, continue to maximize the
### AUC and use the default tuning grid
set.seed(4321)
fit_gbm_ion_roc <- train(Class ~ .,
                         data = my_ion,
                         method = "gbm",
                         metric = "ROC",
                         trControl = ctrl_k05_roc)

fit_gbm_ion_roc

plot(fit_gbm_ion_roc)

### use a boosted tree classifier with xgboost, continue to maximize
### the AUC and use the default tuning grid for now
set.seed(4321)
fit_xgboost_ion_roc <- train(Class ~ ., data = my_ion,
                             method = "xgbTree",
                             metric = "ROC",
                             trControl = ctrl_k05_roc,
                             importance = TRUE)

### print the results
fit_xgboost_ion_roc

### plot the results
plot(fit_xgboost_ion_roc)

### check the accuracy
confusionMatrix.train(fit_xgboost_ion_roc)

### before comparing random forst and boosted trees, fit a regularized
### logistic regression model with elastic net penalty for comparison
### it's always important to consider a simpler method to check if the
### extra complexity of the more advanced models is worth it!

### use a custom grid this time for elastic net
enet_grid <- expand.grid(alpha = c(0.1, 0.2, 0.3, 0.4),
                         lambda = exp(seq(-6, 1, length.out = 21)))

set.seed(4321)
fit_glmnet_roc <- train(Class ~ ., data = my_ion,
                        method = "glmnet",
                        metric = "ROC",
                        tuneGrid = enet_grid,
                        preProc = c("center", "scale"),
                        trControl = ctrl_k05_roc)

plot(fit_glmnet_roc, xTrans = log)

### now compare all 3 models based on the ROC curve metrics
ion_model_compare <- resamples(list(GLMNET = fit_glmnet_roc,
                                    RF = fit_rf_ion_roc,
                                    XGB = fit_xgboost_ion_roc,
                                    GBM = fit_gbm_ion_roc))

### can use the default plot method to compare
dotplot(ion_model_compare)

### or extract the results and and create a custom figure
ion_model_compare_lf <- ion_model_compare$values %>% tbl_df() %>% 
  tidyr::gather(key = "key", value = "metric_value", -Resample) %>% 
  tidyr::separate(key,
                  c("model_name", "metric_name"),
                  sep = "~")

### which model do you think is betteR?
ion_model_compare_lf %>% 
  ggplot(mapping = aes(x = model_name, y = metric_value)) +
  geom_point() +
  stat_summary(fun.data = "mean_se",
               color = "red",
               fun.args = list(mult = 1)) +
  coord_flip() +
  facet_grid( . ~ metric_name, scales = "free_x") +
  theme_bw()

### show just the tree based methods
ion_model_compare_lf %>% 
  filter(model_name != "GLMNET") %>% 
  ggplot(mapping = aes(x = model_name, y = metric_value)) +
  geom_point() +
  stat_summary(fun.data = "mean_se",
               color = "red",
               fun.args = list(mult = 1)) +
  coord_flip() +
  facet_grid( . ~ metric_name, scales = "free_x") +
  theme_bw()
