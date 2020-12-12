### generate our own neural network

library(tidyverse)

### use 7 hidden units in a single hidden layer

### first specify a single input, setup a grid for visualization
x_input <- seq(-3, 3, length.out = 51)

### we need to specify the hidden unit weights (the slopes) and 
### the biases (the intercepts) (the hidden unit parameters)
### lets randomly generate them rather than setting by hand

num_hidden_units <- 7

set.seed(12345)
Bmat <- MASS::mvrnorm(n = 2,
                      mu = rep(0, num_hidden_units),
                      Sigma = diag(num_hidden_units))

Bmat

### create the design matrix
Xmat <- model.matrix( ~ x, data = tibble::tibble(x = x_input))

Xmat %>% head()

Xmat %>% dim()

### calculate the linear predictor for each hidden unit (neuron)
Amat <- Xmat %*% Bmat

Amat %>% dim()

Amat %>% head()

### visualize the linear predictors for each hidden unit

Amat %>% as.data.frame() %>% tibble::as_tibble() %>% 
  tibble::rowid_to_column() %>% 
  tidyr::gather(key = "key", value = "value", -rowid) %>% 
  left_join(tibble::tibble(x = x_input) %>% tibble::rowid_to_column(),
            by = "rowid") %>% 
  ggplot(mapping = aes(x = x, y = value)) +
  geom_line(mapping = aes(color = key,
                          group = key),
            size = 1.15) +
  ggthemes::scale_color_colorblind("Hidden unit") +
  theme_bw()

### pass through a non-linear transformation

### use the tanh instead of logistic to be different from the lecture notes
Hmat <- tanh(Amat)

Hmat %>% head()

### visualize the trends of the non-linear hidden unit responses wrt the input
Hmat %>% as.data.frame() %>% tibble::as_tibble() %>% 
  tibble::rowid_to_column() %>% 
  tidyr::gather(key = "key", value = "value", -rowid) %>% 
  left_join(tibble::tibble(x = x_input) %>% tibble::rowid_to_column(),
            by = "rowid") %>% 
  ggplot(mapping = aes(x = x, y = value)) +
  geom_line(mapping = aes(color = key,
                          group = key),
            size = 1.15) +
  ggthemes::scale_color_colorblind("Hidden unit") +
  theme_bw()

### check the hidden unit coefficients
Bmat

### add in the output layer, we must specify the output layer
### weights (the slopes applied to each hidden unit) and the output
### layer bias (the output layer intecept)

### randomly generate the output lyater parameters

set.seed(987123)

a_params <- rnorm(n = num_hidden_units + 1, mean = 0 , sd = 1)

a_params

### separate the output layer parameters into weights and bias
a_bias <- a_params[1]
a_weight <- a_params[-1]

a_bias
a_weight

### the neural network response (for a continuous variable) is just a
### linear model of the hidden units

f_output <- as.vector(a_bias + Hmat %*% matrix(a_weight))

### visualize the trends of the nerual network response wrt the input
df <- tibble::tibble(x = x_input, f = f_output)

df %>% 
  ggplot(mapping = aes(x = x, y = f)) +
  geom_line(size = 1.15) +
  theme_bw()

### what if we instead used the ReLU as the non-linear transformation?
Hmat_relu <- Amat %>% as.data.frame() %>% tibble::as_tibble() %>% 
  mutate_all(function(x){ifelse(x > 0, x, 0)}) %>% 
  as.matrix()

Hmat_relu %>% head()

### let's visualize the trends of the hidden unit non-linear responses when
### we use the ReLU activation

Hmat_relu %>% as.data.frame() %>% tibble::as_tibble() %>% 
  tibble::rowid_to_column() %>% 
  tidyr::gather(key = "key", value = "value", -rowid) %>% 
  left_join(tibble::tibble(x = x_input) %>% tibble::rowid_to_column(),
            by = "rowid") %>% 
  ggplot(mapping = aes(x = x, y = value)) +
  geom_line(mapping = aes(color = key,
                          group = key),
            size = 1.15) +
  ggthemes::scale_color_colorblind("Hidden unit") +
  theme_bw()

### calculate the neural network response using the ReLU hidden units
f_output_relu <- as.vector(a_bias + Hmat_relu %*% matrix(a_weight))

df_relu <- tibble::tibble(x = x_input, f = f_output_relu)

df_relu %>% 
  ggplot(mapping = aes(x = x, y = f)) +
  geom_line(size = 1.15) +
  theme_bw()

### generate some noisy data around the "true" trend
sigma_true <- 0.2

### focus on the behavior associated with the tanh nonlinear transfomration function
set.seed(781231)
df_b <- df %>% 
  mutate(y = rnorm(n = n(), mean = f, sd = sigma_true))

df_b %>% 
  ggplot(mapping = aes(x = x)) +
  geom_line(mapping = aes(y = f), size = 1.15, color = 'red') +
  geom_point(mapping = aes(y = y), color = "grey30", size = 3.5) +
  theme_bw()

### fit a few neural networks

library(neuralnet)

### fit the coefficients of a neural network using the exact number
### if hidden units that were used to generate the data

set.seed(91231)
mod1_a <- neuralnet(y ~ x, 
                    data = df_b,
                    hidden = 7,
                    err.fct = "sse",
                    act.fct = "tanh",
                    linear.output = TRUE,
                    likelihood = TRUE)

### access the parameters
mod1_a$weights

### look at the neural network diagram
plot(mod1_a, rep = "best")

# plot(mod1_a) # to "pop out" a separate graphic window

### what if we used a different initial guess?
set.seed(1234123)
mod1_b <- neuralnet(y ~ x, 
                    data = df_b,
                    hidden = 7,
                    err.fct = "sse",
                    act.fct = "tanh",
                    linear.output = TRUE,
                    likelihood = TRUE)

mod1_b$weights

### visualize the trends for each neural network by predicting the traiing set
df_c <- df_b %>% 
  mutate(pred_nnet_a = as.vector(predict(mod1_a, df_b)),
         pred_nnet_b = as.vector(predict(mod1_b, df_b)))

df_c %>% 
  select(x, starts_with("pred_")) %>% 
  tibble::rowid_to_column() %>% 
  tidyr::gather(key = "key", value = "value", -x, -rowid) %>% 
  tidyr::separate(key,
                  c("pred_word", "nnet_word", "Attempt"),
                  sep = "_") %>% 
  ggplot(mapping = aes(x = x)) +
  geom_line(mapping = aes(color = Attempt,
                          group = Attempt,
                          y = value),
            size = 1.15) +
  geom_point(data = df_b,
             mapping = aes(y = y),
             color = "black", size = 3, alpha = 0.5) +
  geom_line(data = df_b,
            mapping = aes(x = x, y = f),
            color = "red", size = 1.15, linetype = "dashed") +
  ggthemes::scale_color_colorblind() +
  theme_bw()

### what if we used a simpler model?

set.seed(81231)
mod2_a <- neuralnet(y ~ x, 
                    data = df_b,
                    hidden = 3,
                    err.fct = "sse",
                    act.fct = "tanh",
                    linear.output = TRUE,
                    likelihood = TRUE)

plot(mod2_a, rep = "best")

### what if we used a more complex model?
set.seed(512312)
mod3_a <- neuralnet(y ~ x, 
                    data = df_b,
                    hidden = 12,
                    err.fct = "sse",
                    act.fct = "tanh",
                    linear.output = TRUE,
                    likelihood = TRUE)

plot(mod3_a, rep = "best")

### consider the AIC/BIC score to select the simplest model
mod1_a$result.matrix %>% as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  tibble::as_tibble() %>% 
  slice(1:5) %>% 
  tidyr::spread(rowname, V1)

extract_model_performance <- function(mod_object, mod_name)
{
  mod_object$result.matrix %>% as.data.frame() %>% 
    tibble::rownames_to_column() %>% 
    tibble::as_tibble() %>% 
    slice(1:5) %>% 
    tidyr::spread(rowname, V1) %>% 
    mutate(model_name = mod_name)
}

purrr::map2_dfr(list(mod1_a, mod2_a, mod3_a),
                c("7 hidden units", "3 hidden units", "12 hidden units"),
                extract_model_performance) %>% 
  arrange(aic, bic)

### visualize the trends of the neural network with just 3 hidden units and 12 hidden units
df_b %>% 
  mutate(pred_nnet_07_a = as.vector(predict(mod1_a, df_b)),
         pred_nnet_03_a = as.vector(predict(mod2_a, df_b)),
         pred_nnet_12_a = as.vector(predict(mod3_a, df_b))) %>% 
  select(x, starts_with("pred_")) %>% 
  tibble::rowid_to_column() %>% 
  tidyr::gather(key = "key", value = "value", -x, -rowid) %>% 
  tidyr::separate(key,
                  c("pred_word", "nnet_word", "H", "Attempt"),
                  sep = "_") %>% 
  ggplot(mapping = aes(x = x)) +
  geom_line(mapping = aes(color = as.factor(H),
                          group = H,
                          y = value),
            size = 1.15) +
  geom_point(data = df_b,
             mapping = aes(y = y),
             color = "black", size = 3, alpha = 0.5) +
  geom_line(data = df_b,
            mapping = aes(x = x, y = f),
            color = "red", size = 1.15, linetype = "dashed") +
  facet_wrap(~Attempt, labeller = "label_both") +
  ggthemes::scale_color_colorblind() +
  theme_bw()

### try using different random hidden unit and output layer parameters
### to generate the data! See which model is considered the best!
