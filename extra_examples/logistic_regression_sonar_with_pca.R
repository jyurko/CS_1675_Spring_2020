### train a logistic regression model using features from PCA

library(tidyverse)

library(rsample)

library(yardstick)

### use the Sonar data set
data("Sonar", package="mlbench")

Sonar %>% dim()

Sonar %>% glimpse()

levels(Sonar$Class)

mean(Sonar$Class == "M")
mean(Sonar$Class == "R")

### the input features are correlated
Sonar %>% select(-Class) %>% 
  cor() %>% 
  corrplot::corrplot(type = "upper", method = "square")

Sonar %>% select(-Class) %>% 
  cor() %>% 
  corrplot::corrplot(type = "upper", method = "square",
                     order = "hclust")

### extract the feature names
feature_names <- Sonar %>% select(-Class) %>% names()

### create the resample folds, 5-fold with 5-repeats, statify by Class

set.seed(91231)

cv_info <- rsample::vfold_cv(Sonar, v = 5, repeats = 5, strata = "Class")

cv_info

### the splits are contained in the splits column
cv_info$splits[[1]]

cv_info$splits[[2]]

### within each fold we need to perform the following actions:

### preprocess the input features

### fit the model using the preprocessed features on the fold's training set

### predict and store the performance metrics on the training set

### preprocess the fold's hold-out test set based on the training set

### predict the hold-out test set and calculate performance metrics

### define a function that operates within a resample fold

train_and_test_pca_glm <- function(a_split, repeat_id, fold_id, nComp,
                                   x_names, y_name, y_levels, threshold)
{
  # extract the training set
  train_df <- as.data.frame(a_split, data = "analysis")
  
  # extract the training features
  x_train <- train_df %>% dplyr::select(x_names)
  
  # preprocess the training set
  pca_train <- prcomp(x_train, center = TRUE, scale. = TRUE)
  
  # extract the PC scores on the training set using the desired
  # number of components to retain
  z_train <- pca_train$x[, 1:nComp, drop=FALSE] %>% 
    as.data.frame() %>% tibble::as_tibble() %>% 
    purrr::set_names(sprintf("PC_%02d", 1:nComp))
  
  # assemble the training set for the fold
  train_ready <- z_train %>% bind_cols(train_df %>% dplyr::select(y_name))
  
  # define the formula as using all linear additive terms for the
  # PC scores
  my_formula <- as.formula( sprintf("%s ~ .", y_name) )
  
  # fit the model on the training set
  my_mod <- glm(my_formula, data = train_ready, family = "binomial")
  
  # predict the training set and measure performance
  train_results <- train_df %>% 
    mutate(pred_prob = as.vector(predict(my_mod, newdata = train_ready, type="response")),
           pred_class = ifelse( pred_prob > threshold, y_levels[2], y_levels[1])) %>% 
    mutate(pred_class = factor(pred_class, levels = y_levels)) %>% 
    select(y_name, pred_prob, pred_class) %>% 
    purrr::set_names(c("truth", "Class1", "predicted")) %>% 
    mutate(Class2 = 1 - Class1)
  
  # calculate the accuracy on the training set
  train_accuracy <- yardstick::metrics(train_results, truth, predicted) %>% 
    dplyr::select(.metric, .estimate) %>% 
    tidyr::spread(.metric, .estimate)
  
  # calculate the ROC AUC on the trainin gset
  train_roc <- train_results %>% 
    yardstick::roc_auc(truth, Class2) %>% 
    dplyr::select(.metric, .estimate) %>% 
    tidyr::spread(.metric, .estimate)
  
  train_metrics <- train_accuracy %>% 
    bind_cols(train_roc) %>% 
    mutate(repeat_id = repeat_id, fold_id = fold_id)
  
  # preprocess the hold-out test set
  
  # extract the test set from the rsample object
  test_df <- as.data.frame(a_split, data = "assessment")
  
  # extract the features
  x_test <- test_df %>% dplyr::select(x_names)
  
  # standardize the test set features based on the training set
  x_test_stan <- scale(x_test, center = pca_train$center, scale = pca_train$scale)
  
  # project the test set standardized features onto the PC space based on the training set
  z_test <- x_test_stan %*% pca_train$rotation[, 1:nComp, drop=FALSE] %>% 
    as.data.frame() %>% tibble::as_tibble() %>% 
    purrr::set_names(sprintf("PC_%02d", 1:nComp))
  
  # predict the hold-out test
  test_results <- test_df %>% 
    mutate(pred_prob = as.vector(predict(my_mod, newdata = z_test, type="response")),
           pred_class = ifelse(pred_prob > threshold, y_levels[2], y_levels[1])) %>% 
    mutate(pred_class = factor(pred_class, levels = y_levels)) %>% 
    select(y_name, pred_prob, pred_class) %>% 
    purrr::set_names(c("truth", "Class1", "predicted")) %>% 
    mutate(Class2 = 1 - Class1)
  
  # measure performance on the hold-out test set
  
  # calculate the accuracy on the hold out test at the assumed threshold value
  test_accuracy <- yardstick::metrics(test_results, truth, predicted) %>% 
    dplyr::select(.metric, .estimate) %>% 
    tidyr::spread(.metric, .estimate)
  
  # calculate the ROC AUC on the test set
  test_roc <- test_results %>% 
    yardstick::roc_auc(truth, Class2) %>% 
    dplyr::select(.metric, .estimate) %>% 
    tidyr::spread(.metric, .estimate)
  
  test_metrics <- test_accuracy %>% 
    bind_cols(test_roc) %>% 
    mutate(repeat_id = repeat_id, fold_id = fold_id)
  
  # combine the training set and test set performance together
  train_metrics %>% mutate(from_set = "train") %>% 
    bind_rows(test_metrics %>% mutate(from_set = "test"))
}

### test out the function behavior
train_and_test_pca_glm(cv_info$splits[[1]], cv_info$id[[1]], cv_info$id2[[1]],
                       nComp = 3, 
                       feature_names, "Class", levels(Sonar$Class), 0.5)

train_and_test_pca_glm(cv_info$splits[[1]], cv_info$id[[1]], cv_info$id2[[1]],
                       nComp = 1, 
                       feature_names, "Class", levels(Sonar$Class), 0.5)

### create a wrapper function which iterates over the splits for a specific 
### number of PC's to retain

evaluate_resample_pca_glm <- function(nComp, resample_info, x_names, y_name, y_levels, threshold)
{
  purrr::pmap_dfr(list(resample_info$splits,
                       resample_info$id,
                       resample_info$id2),
                  train_and_test_pca_glm,
                  nComp = nComp,
                  x_names = x_names,
                  y_name = y_name,
                  y_levels = y_levels,
                  threshold = threshold) %>% 
    mutate(nComp = nComp)
}

### test out the cross-validation for a particular number of components to retain

check_cv_results_3 <- evaluate_resample_pca_glm(3, cv_info,
                                                feature_names, "Class", levels(Sonar$Class), 0.5)

check_cv_results_3

### execute the cross-validation for up to 22 PCs to retain

pca_glm_tune_results <- purrr::map_dfr(1:22,
                                       evaluate_resample_pca_glm,
                                       resample_info = cv_info,
                                       x_names = feature_names,
                                       y_name = "Class",
                                       y_levels = levels(Sonar$Class),
                                       threshold = 0.5)

### check the output format

pca_glm_tune_results

### summarize the results for each nComp value

pca_glm_tune_summary <- pca_glm_tune_results %>% 
  group_by(nComp, from_set) %>% 
  summarise(n_repeats = n(),
            avg_roc_auc = mean(roc_auc),
            sd_roc_auc = sd(roc_auc),
            avg_accuracy = mean(accuracy),
            sd_accuracy = sd(accuracy)) %>% 
  ungroup() %>% 
  mutate(se_roc_auc = sd_roc_auc / sqrt(n_repeats),
         se_accuracy = sd_accuracy / sqrt(n_repeats))

### the best performing model on the test set
best_accuracy_mod <- pca_glm_tune_summary %>% 
  filter(from_set == "test") %>% 
  arrange(desc(avg_accuracy)) %>% 
  slice(1)

best_roc_mod <- pca_glm_tune_summary %>% 
  filter(from_set == "test") %>% 
  arrange(desc(avg_roc_auc)) %>% 
  slice(1)

best_accuracy_mod

best_roc_mod

### study the cross-validation results between the training set and test set
### for accuracy

pca_glm_tune_results %>% 
  ggplot(mapping = aes(x = nComp, y = accuracy)) +
  geom_vline(xintercept = best_accuracy_mod$nComp,
             color = "grey", linetype = "dashed", size = 1.) +
  # geom_point(mapping = aes(color = from_set), alpha = 0.25) +
  stat_summary(fun.data = "mean_se",
               mapping = aes(color = from_set)) +
  geom_hline(yintercept = best_accuracy_mod$avg_accuracy - best_accuracy_mod$se_accuracy,
             color = "grey50", linetype = "dashed", size = 1.) +
  geom_hline(yintercept = mean(Sonar$Class == levels(Sonar$Class)[2]),
             color = "black", linetype = "dotted", size = 1.2) +
  scale_color_brewer("", palette = "Set1") +
  theme_bw()

### look at the ROC AUC on the training and test set
pca_glm_tune_results %>% 
  ggplot(mapping = aes(x = nComp, y = roc_auc)) +
  geom_vline(xintercept = best_roc_mod$nComp,
             color = "grey", linetype = "dashed", size = 1.) +
  # geom_point(mapping = aes(color = from_set), alpha = 0.25) +
  stat_summary(fun.data = "mean_se",
               mapping = aes(color = from_set)) +
  geom_hline(yintercept = best_roc_mod$avg_roc_auc - best_roc_mod$se_roc_auc,
             color = "grey50", linetype = "dashed", size = 1.) +
  scale_color_brewer("", palette = "Set1") +
  theme_bw()
