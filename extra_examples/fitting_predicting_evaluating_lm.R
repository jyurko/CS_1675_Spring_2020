### complexity demonstration using information criterions
### use the quadratic demonstration from earlier

### single INPUT but POLYNOMIAL basis

library(tidyverse)

### define the TRUE polynomial function to generate the data

my_quad_func <- function(x, beta_vec)
{
  beta_vec[1] + beta_vec[2] * x + beta_vec[3] * x^2
}

### generate 100 random input values from a standard normal
set.seed(8001)
x_demo <- rnorm(n = 100)

### set the true model trend coefficeints
beta_true <- c(0.33, 1.15, -2.25)

### specify the NOISE or RESIDUAL ERROR
sigma_noisy <- 2.75
# sigma_noisy <- 1

### generate the random responses, we must evaluate the mean trend
### then generate random outputs around the mean
set.seed(8100)
noisy_df <- tibble::tibble(
  x = x_demo
) %>% 
  mutate(mu = my_quad_func(x, beta_true),
         y = rnorm(n = n(), mean = mu, sd = sigma_noisy))

### but the training set will be based on a SUBSET of all the values
### subset the first 30 rows
### change this if you would like to try out more or less data
my_train <- noisy_df %>% 
  tibble::rowid_to_column("obs_id") %>% 
  slice(1:30)

my_train %>% glimpse()

### to help visualize let's create a 'fine' grid of points
x_fine <- seq(min(x_demo) - 0.02, max(x_demo) + 0.02, length.out = 101)

fine_grid <- tibble::tibble(x = x_fine) %>% 
  mutate(f_true = my_quad_func(x, beta_true))

### visualize the noisy training set relative to the TRUTH for this toy problem
noisy_df %>% 
  ggplot(mapping = aes(x = x)) +
  geom_line(data = fine_grid,
            mapping = aes(y = f_true),
            color = "red", size = 1.45) +
  geom_point(mapping = aes(y = y), color = NA) +
  geom_point(data = my_train,
             mapping = aes(y = y),
             color = "black", size = 4.5, alpha = 0.65) +
  labs(y = "y") +
  theme_bw()

### fit a quadratic polynomial to the data

### the formula could be typed as

mod_2formula <- lm( y ~ x + I(x^2), data = my_train)

mod_2formula %>% summary()

### but rather than using the formula...we can use a shortcut
### with the `poly()` to create the polynomial features
mod2 <- lm( y ~ poly(x, 2, raw=TRUE), data = my_train)


mod2 %>% summary()

### visualize the coefficients

library(coefplot)

coefplot::coefplot(mod2) + theme_bw()

### let's predict the "fine" prediction or test set

pred_mean_mod2 <- predict(mod2, fine_grid)

pred_mean_mod2 %>% class()

pred_mean_mod2 %>% length()

### however lets look at the confidence interval - the uncertainty on the mean trend

pred_mean_ci_mod2 <- predict(mod2, fine_grid, interval = "confidence")

pred_mean_ci_mod2 %>% class()

colnames(pred_mean_ci_mod2)

### the prediction interval - the uncetainty of the response around the mean

pred_interval_mod2 <- predict(mod2, fine_grid, interval = "prediction")

pred_interval_mod2 %>% class()

colnames(pred_interval_mod2)

### let's check that the mean trends are the same between the two interval types
all.equal(pred_mean_ci_mod2[, 'fit'], pred_interval_mod2[, 'fit'])

### but the interval lower bounds...
all.equal(pred_mean_ci_mod2[, 'lwr'], pred_interval_mod2[, 'lwr'])

### let's now combine the predictions and the uncertainty intervals together
### for visualization

pred_fine_viz_mod2 <- fine_grid %>% 
  bind_cols(pred_mean_ci_mod2 %>% 
              as.data.frame() %>% tibble::as_tibble() %>% 
              dplyr::select(fit, ci_lwr = lwr, ci_upr = upr)) %>% 
  bind_cols(pred_interval_mod2 %>% 
              as.data.frame() %>% tibble::as_tibble() %>% 
              dplyr::select(pred_lwr = lwr, pred_upr = upr))

pred_fine_viz_mod2 %>% glimpse()

### visualize the predictions over the fine grid and compare to the training points
pred_fine_viz_mod2 %>% 
  ggplot(mapping = aes(x = x)) +
  geom_ribbon(mapping = aes(ymin = pred_lwr, ymax = pred_upr),
              fill = "orange") +
  geom_ribbon(mapping = aes(ymin = ci_lwr, ymax = ci_upr),
              fill = "grey50") +
  geom_line(mapping = aes(y = fit),
            color = "black", size = 1.15) +
  geom_line(mapping = aes(y = f_true),
            color = "red", linetype = "dashed", size = 1.15) +
  geom_point(data = my_train,
             mapping = aes(x = x, y = y),
             color = "black", size = 4.5, alpha = 0.65) +
  labs(y = "y") +
  theme_bw()

### what if I used an 8th order polynomial?

mod8 <- lm( y ~ poly(x, 8, raw=TRUE), data = my_train)

coefplot::coefplot(mod8) + theme_bw()

### the poly function makes it a little tricky to directly compare
### our two models, we would need to modify the variable names

coefplot::multiplot(mod2, mod8) + theme_bw()

### make predictions on the test or prediction set with the 8th order polynomial

pred_mean_ci_mod8 <- predict(mod8, fine_grid, interval = "confidence")

pred_interval_mod8 <- predict(mod8, fine_grid, interval = "prediction")

pred_fine_viz_mod8 <- fine_grid %>% 
  bind_cols(pred_mean_ci_mod8 %>% 
              as.data.frame() %>% tibble::as_tibble() %>% 
              dplyr::select(fit, ci_lwr = lwr, ci_upr = upr)) %>% 
  bind_cols(pred_interval_mod8 %>% 
              as.data.frame() %>% tibble::as_tibble() %>% 
              dplyr::select(pred_lwr = lwr, pred_upr = upr))

pred_fine_viz_mod8 %>% 
  ggplot(mapping = aes(x = x)) +
  geom_ribbon(mapping = aes(ymin = pred_lwr, ymax = pred_upr),
              fill = "orange") +
  geom_ribbon(mapping = aes(ymin = ci_lwr, ymax = ci_upr),
              fill = "grey50") +
  geom_line(mapping = aes(y = fit),
            color = "black", size = 1.15) +
  geom_line(mapping = aes(y = f_true),
            color = "red", linetype = "dashed", size = 1.15) +
  geom_point(data = my_train,
             mapping = aes(x = x, y = y),
             color = "black", size = 4.5, alpha = 0.65) +
  labs(y = "y") +
  theme_bw()

### use coord_cartesian() to zoom in
pred_fine_viz_mod8 %>% 
  ggplot(mapping = aes(x = x)) +
  geom_ribbon(mapping = aes(ymin = pred_lwr, ymax = pred_upr),
              fill = "orange") +
  geom_ribbon(mapping = aes(ymin = ci_lwr, ymax = ci_upr),
              fill = "grey50") +
  geom_line(mapping = aes(y = fit),
            color = "black", size = 1.15) +
  geom_line(mapping = aes(y = f_true),
            color = "red", linetype = "dashed", size = 1.15) +
  geom_point(data = my_train,
             mapping = aes(x = x, y = y),
             color = "black", size = 4.5, alpha = 0.65) +
  coord_cartesian(ylim = c(-25, 10)) +
  labs(y = "y") +
  theme_bw()

### we can put the two model predictions together and compare with facets
pred_fine_viz_mod2 %>% 
  mutate(J = 2) %>% 
  bind_rows(pred_fine_viz_mod8 %>% 
              mutate(J = 8)) %>% 
  ggplot(mapping = aes(x = x)) +
  geom_ribbon(mapping = aes(ymin = pred_lwr, ymax = pred_upr,
                            group = J),
              fill = "orange") +
  geom_ribbon(mapping = aes(ymin = ci_lwr, ymax = ci_upr,
                            group = J),
              fill = "grey50") +
  geom_line(mapping = aes(y = fit, group = J),
            color = "black", size = 1.15) +
  geom_line(mapping = aes(y = f_true, group = J),
            color = "red", linetype = "dashed", size = 1.15) +
  geom_point(data = my_train,
             mapping = aes(x = x, y = y),
             color = "black", size = 4.5, alpha = 0.65) +
  coord_cartesian(ylim = c(-25, 10)) +
  facet_wrap(~J, labeller = "label_both") +
  labs(y = "y") +
  theme_bw()

### how can we know which model is best?

### the `broom` package for accessing TIDY versions of model object results

broom::glance(mod2)

broom::glance(mod8)

### the 8th order polynomial is conssered the best based on R squared on the trainng
### set...what about if we used other models?

### define a function to fit any polynomial degree

fit_lm_poly <- function(J, train_set)
{
  if(J == 0){
    # INTERCEPT-ONLY -- CONSTANT!!!
    return( lm( y ~ 1, train_set) )
  } else {
    # fit the polynomial basis function
    return( lm( y ~ poly(x, J, raw=TRUE), train_set) )
  }
}

### we can loop over all polynomials we wish to fit, lets try a constant
### up to an 8th order polynomial

### return a LIST where each element is a modle object
all_models <- purrr::map(0:8, fit_lm_poly, train_set = my_train)

### intercept only model
all_models[[1]]

all_models[[1]] %>% summary()

### the quadratic model
all_models[[1 + 2]] %>% summary()

### check that its the same
summary(mod2)

### loop over and extract the performance metrics on the training set with broom

extract_metrics <- function(my_mod, mod_name)
{
  broom::glance(my_mod) %>% 
    mutate(model_name = mod_name)
}

all_model_metrics <- purrr::map2_dfr(all_models,
                                     as.character(0:8),
                                     extract_metrics)

all_model_metrics

### plot the r-squared and the sigma with respect to the degree of the polynomial

all_model_metrics %>% 
  dplyr::select(model_name, r.squared, sigma) %>% 
  tidyr::gather(key = "key", value = "value", -model_name) %>% 
  ggplot(mapping = aes(x = model_name, y = value)) +
  geom_line(size = 1.5,
            mapping = aes(group = key)) +
  geom_point(size = 4.5) +
  facet_wrap(~key, scales = "free_y") +
  theme_bw()

### complexity metrics from information criterio--penalize model performance
### based on the number of parameters

### AIC, BIC - for non-bayesian analyses
### in lecture we will discuss the more formal -- log-marginal-likelihood (Evidence)

all_model_metrics %>% 
  dplyr::select(model_name, AIC, BIC) %>% 
  tidyr::gather(key = "key", value = "value", -model_name) %>% 
  ggplot(mapping = aes(x = model_name, y = value)) +
  geom_line(size = 1.5,
            mapping = aes(group = key)) +
  geom_point(size = 4.5) +
  facet_wrap(~key, scales = "free_y") +
  theme_bw()
