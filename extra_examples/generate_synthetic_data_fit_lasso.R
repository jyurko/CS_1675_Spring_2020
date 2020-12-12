### practice generating data to fit with regularized non-Bayesian models

library(tidyverse)

### consider a categorical or discrete input with 4 levels

xcat_levels <- c("a", "b", "c", "d")

### randomly sample with replacement the 4 levels

num_samples <- 400

set.seed(98123)
xcat_samples <- sample(xcat_levels, size = num_samples, replace = TRUE)

xcat_samples %>% head()

### generate continuous inputs from MVN assume all inputs are
### independent with marginal standard normals
num_numeric_inputs <- 8

diag(num_numeric_inputs) ### shortcut for the identity matrix

set.seed(12314)

xnumbers <- MASS::mvrnorm(n = num_samples, 
                          mu = rep(0, num_numeric_inputs),
                          Sigma = diag(num_numeric_inputs)) %>% 
  as.data.frame() %>% tibble::as_tibble() %>% 
  purrr::set_names(sprintf("x%02d", 1:num_numeric_inputs))

xnumbers

### combine the numeric inputs with the categorical input

df_inputs <- xnumbers %>% mutate(xA = xcat_samples)

df_inputs %>% summary()

### a categorical variable in R is known as a FACTOR

df_inputs <- df_inputs %>% mutate(xA = factor(xA, levels = xcat_levels))

df_inputs %>% summary()

### let's create the design matrix associated with a model consisting
### only of the discrete or categorical input, `xA`

df_inputs %>% head()

model.matrix( ~ xA, df_inputs) %>% head()

### what does the design matrix look like wiht a single continuous input?

model.matrix( ~ x01, df_inputs) %>% head()

### perhaps we wanted to study a categorical AND a continuous input

model.matrix( ~ xA + x01, df_inputs) %>% head()

### if I wanted a polynomial for x01...

model.matrix( ~ xA + poly(x01, 3, raw=TRUE), df_inputs) %>% head()

### categorical and continuous interactions

model.matrix( ~ xA * x01, df_inputs) %>% head()

### more than 2 variable interactions, for example an interaction
### between the discrete and 2 continuous inputs

model.matrix( ~ xA * x01 * x02, df_inputs) %>% head()

model.matrix( ~ xA * x01 * x02, df_inputs) %>% colnames()

### how many features would we have if we allowed ALL pair-wise interactions?

model.matrix( ~ (.)^2, df_inputs) %>% colnames()

### `.` is a shortcut for "EVERYTHING ELSE" in the data set

model.matrix( ~ ., df_inputs) %>% colnames()

### how many features would we have if we allowed all TRIPLET or 3way interactions?

model.matrix( ~ (.)^3, df_inputs) %>% dim()

model.matrix( ~ (.)^3, df_inputs) %>% colnames()

### let's define a TRUE trend and then try and see if we can recover
### that true trend by fitting a model

model.matrix( ~ xA * x01 * x02 + x07 * x08, df_inputs) %>% colnames()

Xmat_true <- model.matrix( ~ xA * x01 * x02 + x07 * x08, df_inputs)

### randomly generate the "beta coefficients"

set.seed(89123)

beta_true <- rnorm(n = ncol(Xmat_true), mean = 0, sd = 1.75)

beta_true

### generate a continuous response where we first calculate the true mean trend
### and then generate noisy responses around that mean trend

sigma_noise <- 0.55

set.seed(332123)

train_df <- df_inputs %>% 
  mutate(mu = as.vector(Xmat_true %*% matrix(beta_true)),
         y = rnorm(n = n(), mean = mu, sd = sigma_noise))

train_df %>% summary()

### how does the response relate to the categorical input?

train_df %>% 
  ggplot(mapping = aes(x = xA, y = y)) +
  geom_boxplot() +
  stat_summary(fun.data = "mean_se",
               fun.args = list(mult = 2),
               color = "red") +
  theme_bw()

### fit a linear model just considering the categorical input

fit_cat <- lm( y ~ xA, train_df)

fit_cat %>% summary()

coef(fit_cat)

### look at the value of the average response for each discrete level
train_df %>% 
  group_by(xA) %>% 
  summarise(y_avg = mean(y)) %>% 
  ungroup()

levels(train_df$xA)

coef(fit_cat)[-1] + coef(fit_cat)[1]

### fit a model with all additive terms

### remove all variables except the inputs and the response

train_df_b <- train_df %>% select(starts_with("x"), y)

train_df_b

fit_additive <- lm( y ~ ., train_df_b)

fit_additive %>% summary()

coefplot::coefplot(fit_additive) + theme_bw()

### study a model with all pair wise interactions

fit_pairs <- lm( y ~ (.)^2, train_df_b)

coefplot::coefplot(fit_pairs)

fit_pairs %>% summary()

### what about a model with all TRIPLET interactions?

fit_trips <- lm( y ~ (.)^3, train_df_b)

fit_trips %>% summary()

coefplot::coefplot(fit_trips)

### how can we select the minimal number of features? if we were
### unsure how many potential TRIPLET interactions exist...

### USE GLMNET

library(glmnet)

### create the design matrix for ALL triplet interactions but DO NOT include
### the intercept

Xmat <- model.matrix( y ~ (.)^3 - 1, train_df_b)

Xmat %>% colnames()

yobs <- train_df_b$y

### fit the lasso regression model

lasso_mod <- glmnet(Xmat, yobs)

plot(lasso_mod)

plot(lasso_mod, xvar = "lambda")

coefplot::coefpath(lasso_mod)

coef(lasso_mod, s = 0.001)
coef(lasso_mod, s = 0.1)
coef(lasso_mod, s = 2)

coefplot::coefplot(lasso_mod, lambda = 0.1) + theme_bw()

### use cross-validation to tuen the penalty term

lasso_mod_cv <- cv.glmnet(Xmat, yobs, nfolds = 10)

plot(lasso_mod_cv)

coef(lasso_mod_cv)
