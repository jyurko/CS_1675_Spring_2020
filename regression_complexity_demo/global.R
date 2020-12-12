### packages and functions used for the regression complexity
### demo RShiny app

library(dplyr)
library(ggplot2)
library(rsample)

### define a function which creates the true relationship
my_true_func <- function(x, bvec)
{
  Xmat <- model.matrix( ~ poly(x, degree = 8, raw=TRUE), data.frame(x = x))
  
  as.numeric(Xmat %*% as.matrix(bvec))
}

### create a very fine grid of points to help visualize the
### true relationship
x_fine <- seq(-2.95 - 0.02, 2.95 + 0.02, length.out = 101)

### options for the noise level
sigma_choices <- c(0.05, 0.45, 0.95, 2.75, 5.2)
names(sigma_choices) <- c("very low", "low", "moderate", "high", "very high")

### define a function to visualize the FOLDS
separate_fold_splits <- function(a_split, fold_id, rep_id)
{
  as.data.frame(a_split, data = "analysis") %>% 
    mutate(fold_id = fold_id, rep_id = rep_id, type = "Training") %>% 
    bind_rows(as.data.frame(a_split, data = "assessment") %>% 
                mutate(fold_id = fold_id, rep_id = rep_id,
                       type = "Hold-out"))
}

### define a function which trains the model for a given polynomial order

fit_lm_poly <- function(poly_order, train_set)
{
  if (poly_order == 0) {
    return(lm(y ~ 1, data = train_set))
  } else {
    return(lm(y ~ poly(x, poly_order, raw=TRUE), data = train_set))
  }
}

### define a function which reads in the rsample splits, turns it
### into the training set, and fits the model
fit_to_train <- function(a_split, poly_order){
  fit_lm_poly(poly_order, as.data.frame(a_split, data = "analysis"))
}

### use modelr::rmse and modler::rsquare to calculate the holdout test
### performance metrics using the model
assess_on_test <- function(a_model, a_split, perform_func)
{
  perform_func(a_model, as.data.frame(a_split, data = "assessment"))
}

### define a function which reads in the resampling info, the polynomial
### order, then fits on teh train set, assesses on the holdout sets
### and returns the holdout set performance

cv_perform_results <- function(poly_order, resample_info)
{
  resample_info %>% 
    mutate(a_model = purrr::map(splits,
                                fit_to_train,
                                poly_order = poly_order)) %>% 
    mutate(holdout_rmse = purrr::map2_dbl(a_model,
                                          splits,
                                          assess_on_test,
                                          perform_func = modelr::rmse),
           holdout_rsquare = purrr::map2_dbl(a_model,
                                             splits,
                                             assess_on_test,
                                             perform_func = modelr::rsquare)) %>% 
    select(starts_with("id"), starts_with("holdout_")) %>% 
    mutate(poly_order = poly_order)
}