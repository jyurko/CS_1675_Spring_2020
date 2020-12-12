### practice training binary classifiers with caret

library(tidyverse)

### we will work with the Ionosphere data set that was introduced
### earlier in the semester

data("Ionosphere", package = "mlbench")

### remove the first 2 variables, because the exploration from earlier
### revealed they are not necessary

my_ion <- Ionosphere %>% 
  select(-V1, -V2) %>% 
  mutate(Class = factor(Class, levels = c("good", "bad")))

my_ion %>% glimpse()

### histograms of the variables

my_ion %>% tibble::rowid_to_column() %>% 
  tidyr::gather(key = "key", value = "value", -rowid, -Class) %>% 
  mutate(input_number = as.numeric(stringr::str_extract(key, "\\d+"))) %>% 
  ggplot(mapping = aes(x = value)) +
  geom_histogram(bins = 25) +
  facet_wrap(~input_number, scales = "free_y") +
  theme_bw() +
  theme(axis.text.y = element_blank())

### break up the histograms by the binary outcome, the Class
my_ion %>% tibble::rowid_to_column() %>% 
  tidyr::gather(key = "key", value = "value", -rowid, -Class) %>% 
  mutate(input_number = as.numeric(stringr::str_extract(key, "\\d+"))) %>% 
  ggplot(mapping = aes(x = value)) +
  geom_freqpoly(bins = 25,
                mapping = aes(color = Class,
                              y = stat(density)),
                size = 1.) +
  facet_wrap(~input_number, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  theme(axis.text.y = element_blank())

### fraction of good observations in the data set

my_ion %>% count(Class)

mean(my_ion$Class == "good")

### train multiple models using caret

library(caret)

### tune by maxmimizing the area under the roc curve and specify
### the resampling scheme to be 5-fold cross-validation

ctrl_cv05_roc <- trainControl(method = "cv", number = 5,
                              summaryFunction = twoClassSummary,
                              classProbs = TRUE,
                              savePredictions = TRUE)

### elastic net -- logistic regression with the elastic net penalty

set.seed(4321)

fit_glmnet <- train(Class ~ .,
                    data = my_ion,
                    method = "glmnet",
                    metric = "ROC",
                    preProcess = c("center", "scale"),
                    trControl = ctrl_cv05_roc)

fit_glmnet

### try out a finer search grid to tune the parameters

enet_grid <- expand.grid(alpha = seq(0.1, 0.9, by = 0.1),
                         lambda = exp(seq(-6, 1, length.out = 21)))

set.seed(4321)

fit_glmnet_tune <- train(Class ~ .,
                         data = my_ion,
                         method = "glmnet",
                         metric = "ROC",
                         tuneGrid = enet_grid,
                         preProcess = c("center", "scale"),
                         trControl = ctrl_cv05_roc)

plot(fit_glmnet_tune, xTrans = log)

fit_glmnet_tune$bestTune

confusionMatrix.train(fit_glmnet_tune)

### what features matter? please see the caret documentation for a discussion
### of how this works 
### http://topepo.github.io/caret/variable-importance.html

plot(varImp(fit_glmnet_tune))

### can we try out all pairwise combinations?

model.matrix(Class ~ (.)^2, my_ion) %>% dim()

### we can't try out the model that includes all pair wise interactions
### because there would be more features than observations!!!!

### let's try a neural network model to identify more complex patterns for us

set.seed(4321)

fit_nnet <- train(Class ~ ., 
                  data = my_ion,
                  method = "nnet",
                  metric = "ROC",
                  preProcess = c('center', 'scale'),
                  trControl = ctrl_cv05_roc,
                  trace=FALSE)

fit_nnet

### try tuning the number of hidden units and the weight decay some more

nnet_grid <- expand.grid(size = c(3, 5, 10, 15),
                         decay = exp(seq(-6, 2, length.out = 25)))

nnet_grid %>% dim()

set.seed(4321)

fit_nnet_tune <- train(Class ~ ., 
                       data = my_ion,
                       method = "nnet",
                       metric = "ROC",
                       tuneGrid = nnet_grid,
                       preProcess = c('center', 'scale'),
                       trControl = ctrl_cv05_roc,
                       trace=FALSE)

fit_nnet_tune$bestTune

plot(fit_nnet_tune, xTrans = log)

confusionMatrix.train(fit_nnet_tune)

### look at the network

library(NeuralNetTools)

plotnet(fit_nnet_tune$finalModel)

### what features matter?

plot(varImp(fit_nnet_tune))

### random forest

set.seed(4321)

fit_rf <- train(Class ~ ., 
                data = my_ion,
                method = "rf",
                metric = "ROC",
                trControl = ctrl_cv05_roc,
                importance = TRUE)

fit_rf

confusionMatrix.train(fit_rf)

### what variables matter?

plot(varImp(fit_rf))

### compile the model

model_results <- resamples(list(GLMNET = fit_glmnet_tune,
                                NNET = fit_nnet_tune,
                                RF = fit_rf))

dotplot(model_results)

### study predictions by varying the top 4 features as viewed by
### the random forest

top_4_inputs <- (varImp(fit_rf))$importance %>% 
  tibble::rownames_to_column("var_name") %>% 
  arrange(desc(good)) %>% 
  slice(1:4) %>% 
  pull(var_name)

top_4_inputs

### create a prediction grid

my_ion %>% select(-Class) %>% dim()

### define a function to help create the grid of input values
### specify 25 evenly spaced values between the min and max for
### the top 2 inputs (just my choice for how to do this)
### specify 5 quantiles for the third and fourth ranked inputs
### all other inputs will be fixed at their median values

make_input_grid <- function(var_name, top_input_names, all_data)
{
  xvar <- all_data %>% select(var_name) %>% pull()
  
  if (var_name %in% top_input_names[1:2]){
    # use 25 unique values between the min/max values
    xgrid <- seq(min(xvar), max(xvar), length.out = 25)
  } else if (var_name %in% top_input_names[3:4]){
    # specify quantiles to use
    xgrid <- quantile(xvar, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE)
    xgrid <- as.vector(xgrid)
  } else {
    # set to their median values
    xgrid <- median(xvar, na.rm = TRUE)
  }
  
  return(xgrid)
}

### create a list where 1 element in the list corresponds to an input feature

all_input_names <- my_ion %>% select(-Class) %>% names()

test_input_list <- purrr::map(all_input_names,
                              make_input_grid,
                              top_input_names = top_4_inputs,
                              all_data = my_ion)

length(test_input_list)

test_input_list[[1]]

all_input_names

test_input_list[[2]]

top_4_inputs

### convert our list into a data frame for the grid

test_input_grid <- expand.grid(test_input_list, 
                               KEEP.OUT.ATTRS = FALSE,
                               stringsAsFactors = FALSE) %>% 
  purrr::set_names(all_input_names)

test_input_grid %>% glimpse()

### make predictions on the grid

### predictions of the class with the random forest model

pred_test_class_rf <- predict(fit_rf, test_input_grid)

pred_test_class_rf %>% head()

### visualize the classifications

test_input_grid %>% 
  mutate(pred_class = pred_test_class_rf) %>% 
  ggplot(mapping = aes(x = V5, y = V3)) +
  geom_raster(mapping = aes(fill = pred_class)) +
  facet_grid(V7 ~ V8, labeller = "label_both") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  theme(legend.position = "top")

### let's now study the predicted probability instead of the classification
### set the `type ` argument to "prob" in the `predict()` call

pred_test_prob_rf <- predict(fit_rf, test_input_grid, type = "prob")

pred_test_prob_rf %>% class()

pred_test_prob_rf %>% head()

### visualize the predicted probabilities
test_input_grid %>% 
  bind_cols(pred_test_prob_rf) %>% 
  ggplot(mapping = aes(x = V5, y = V3)) +
  geom_raster(mapping = aes(fill = good)) +
  facet_grid(V7 ~ V8, labeller = "label_both") +
  scale_fill_viridis_b() +
  theme_bw() +
  theme(legend.position = "top")

### look at the predicted probability surface with the nnet
pred_test_prob_nnet <- predict(fit_nnet_tune, test_input_grid, type = "prob")

test_input_grid %>% 
  bind_cols(pred_test_prob_nnet) %>% 
  ggplot(mapping = aes(x = V5, y = V3)) +
  geom_raster(mapping = aes(fill = good)) +
  facet_grid(V7 ~ V8, labeller = "label_both") +
  scale_fill_viridis_b() +
  theme_bw() +
  theme(legend.position = "top")
