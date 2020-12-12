### in class demo 03/04/2020 -- LASSO regression with GLMNET

### this example uses the following packages, if you do not have
### them installed please download and install

library(dplyr)
library(ggplot2)
library(coefplot)
library(glmnet)

### create the synthetic dataset -- NOISY quadratic relationship

my_quad_func <- function(x, beta_vec)
{
  beta_vec[1] + beta_vec[2] * x + beta_vec[3] * (x^2)
}

set.seed(8001)
x_demo <- rnorm(n = 100, mean = 0, sd = 1)

### set true parameter values
beta_true <- c(0.33, 1.15, -2.25)
sigma_noisy <- 2.75

### evaluate linear predictor and generate random observations
set.seed(8100)
noisy_df <- tibble::tibble(
  x = x_demo
) %>% 
  mutate(mu = my_quad_func(x, beta_true),
         y = rnorm(n = n(),
                   mean = mu,
                   sd = sigma_noisy))

### visualize and denote the first 30 points as the training set
### if you would like to use more or fewer training points
### change the variable below
num_train_points <- 30

noisy_df %>% 
  tibble::rowid_to_column("obs_id") %>% 
  ggplot(mapping = aes(x = x)) +
  geom_line(mapping = aes(y = mu),
            color = "black", size = 1.15) +
  geom_point(mapping = aes(y = y,
                           color = obs_id < (num_train_points + 1),
                           size = obs_id < (num_train_points + 1))) +
  scale_color_manual("data split",
                     values = c("TRUE" = "red",
                                "FALSE" = "grey30"),
                     labels = c("TRUE" = "train",
                                "FALSE" = "hold-out")) +
  scale_size_manual("data split",
                    values = c("TRUE" = 3,
                               "FALSE" = 1.15),
                    labels = c("TRUE" = "train",
                               "FALSE" = "hold-out")) +
  labs(y = "y") +
  theme_bw() +
  theme(legend.position = "top")

### separate out the training points
train_noisy <- noisy_df %>% 
  tibble::rowid_to_column("obs_id") %>% 
  filter(obs_id < (num_train_points + 1))

### create the appropriate design matrix for the 8th order model
### do not include the intercept because GLMNET does not want the 
### intercept column

Xmat <- model.matrix(y ~ poly(x, degree = 8, raw = TRUE) - 1, train_noisy)

Xmat %>% head()

### separate the response vector into its own variable

yobs <- train_noisy$y

### train a lasso model -- you will probably see some warning messages on this
### data set, that's ok.
lasso_mod <- glmnet(Xmat, yobs)

plot(lasso_mod)

plot(lasso_mod, xvar = "lambda")

plot(lasso_mod, xvar = "lambda", label = TRUE)

### interactive plot to explore the results
coefplot::coefpath(lasso_mod)

coef(lasso_mod, s = 0.001)
coef(lasso_mod, s = 0.1)
coef(lasso_mod, s = 2)

coefplot::coefplot(lasso_mod, lambda = 0.001) + theme_bw()
coefplot::coefplot(lasso_mod, lambda = 0.1) + theme_bw()
coefplot::coefplot(lasso_mod, lambda = 100) + theme_bw()

### now use cross validation to tune the penalty factor
lasso_mod_cv <- cv.glmnet(Xmat, yobs, nfolds = 5,
                          lambda = exp(seq(log(0.001), log(5), length.out = 101)))

plot(lasso_mod_cv)

lasso_mod_cv_2 <- cv.glmnet(Xmat, yobs, nfolds = 5,
                            lambda = exp(seq(log(0.05), log(5), length.out = 101)))

plot(lasso_mod_cv_2)

coef(lasso_mod_cv_2)

### ridge regression, specify the mixing weight alpha to be 0
ridge_mod <- glmnet(Xmat, yobs, alpha = 0)

plot(ridge_mod, xvar = "lambda")