### CS 1675 Spring 2020 - week 11
### Program fitting a neural network by minimizing the SSE
### usig `optim()`

library(dplyr)
library(ggplot2)

### set up the simple problem with 2 hidden units and the logistic
### non-linear transformation function

### specify the hidden unit parameters
b1 <- c(0.2, 1.2)
b2 <- c(-0.1, -0.8)

### set the input to be evenly spaced between -3 and +3
x <- seq(-3, 3, length.out = 25)

### create the design matrix
Xmat <- model.matrix( ~ x, data = data.frame(x = x))

### calculate the hidden unit linear predictors 
Bmat <- cbind(b1, b2)

Bmat

Amat <- Xmat %*% Bmat

### next calculate the non-linear transformed values per hidden unit
### assuming the logistic function

Hmat <- boot::inv.logit(Amat)

### now define the true output layer parameters
a0true <- 0.2
atrue <- c(-1.5, 1.25)

### calculate the true nnet output
ftrue <- a0true + Hmat %*% as.matrix(atrue)

### set the noise to a low value
sigma_true <- 0.15

### generate noisy observations and package 
### the input with the noisy response
set.seed(1131)
demo_df <- tibble::tibble(x = x) %>% 
  mutate(f = as.vector(ftrue),
         y = rnorm(n = n(), mean = f, sd = sigma_true))

### plot the training set
demo_df %>% 
  ggplot(mapping = aes(x = x)) +
  geom_line(mapping = aes(y = f),
            color = "grey50",
            size = 1.15) +
  geom_point(mapping = aes(y = y),
             size = 3, color = "red") +
  labs(y = "y") +
  theme_bw()

### setup the SSE function we wish to minimize 
### all parameters will be contained in the theta vector
### need to extract them appropriately

my_neuralnet_sse <- function(theta, my_info)
{
  # extract the hidden unit parameters
  X <- my_info$design_matrix
  length_beta <- ncol(X)
  total_num_betas <- my_info$num_hidden * length_beta
  beta_vec <- theta[1:total_num_betas]
  
  # extract the output layer parameters
  a_all <- theta[(total_num_betas + 1):length(theta)]
  
  # reorganize the beta parameters into a matrix
  Bmat <- matrix(beta_vec, nrow = length_beta, byrow = FALSE)
  
  # reorganize the output layer parameters by extracting
  # the output layer intercept (the bias)
  a0 <- a_all[1] # the first element in a_all is the bias
  av <- a_all[-1] # select all EXCEPT the first element
  
  # calculate the linear predictors associated with
  # each hidden unit
  A <- X %*% Bmat
  
  # pass through the non-linear transformation function
  H <- my_info$transform_hidden(A)
  
  # calculate the response
  f <- as.vector(a0 + H %*% as.matrix(av))
  
  # performance metric - calculate the SSE
  sum((my_info$yobs - f)^2)
}

### assemble the list of required information
two_hidden_sse_info <- list(
  yobs = demo_df$y,
  design_matrix = model.matrix( y ~ x, demo_df),
  num_hidden = 2,
  transform_hidden = boot::inv.logit
)

### test out the function
num_params <- two_hidden_sse_info$num_hidden * ncol(two_hidden_sse_info$design_matrix) +
  two_hidden_sse_info$num_hidden + 1

my_neuralnet_sse(rep(0, num_params), two_hidden_sse_info)

my_neuralnet_sse(rep(0.25, num_params), two_hidden_sse_info)

my_neuralnet_sse(rep(-1, num_params), two_hidden_sse_info)

### fit the model using optim(), notice that we do NOT set the
### `fnscale` argument since we want to MINIMIZE the SSE

two_hidden_fit_a <- optim(rep(0, num_params),
                          my_neuralnet_sse,
                          gr = NULL,
                          two_hidden_sse_info,
                          method = "BFGS",
                          hessian = TRUE,
                          control = list(maxit = 5001))

two_hidden_fit_a

### use a separate starting guess
two_hidden_fit_b <- optim(rep(0.25, num_params),
                          my_neuralnet_sse,
                          gr = NULL,
                          two_hidden_sse_info,
                          method = "BFGS",
                          hessian = TRUE,
                          control = list(maxit = 5001))

two_hidden_fit_b

### compare the identified parameter values
two_hidden_fit_a$par

two_hidden_fit_b$par

### try randomly generated starting guess
set.seed(101)
two_hidden_fit_c <- optim(rnorm(num_params),
                          my_neuralnet_sse,
                          gr = NULL,
                          two_hidden_sse_info,
                          method = "BFGS",
                          hessian = TRUE,
                          control = list(maxit = 5001))

two_hidden_fit_c$par

two_hidden_fit_a$par
