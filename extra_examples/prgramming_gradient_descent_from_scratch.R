### create our own 2 input neural network and then estimate the
### weights and biases using gradient descent

### start out by creating our own neural network

library(tidyverse)

### define a grid of input values
x_input <- expand.grid(x1 = seq(-3, 3, length.out = 21),
                       x2 = seq(-3, 3, length.out = 21),
                       KEEP.OUT.ATTRS = FALSE,
                       stringsAsFactors = FALSE) %>% 
  as.data.frame() %>% tibble::as_tibble()

x_input %>% glimpse()

### number of linear predictor coefficients for each hidden unit
P <- ncol(x_input) + 1

### use 5 hidden units for our neural network
num_hidden_units <- 5

### randomly generate all hidden unit parameters

set.seed(12412)

Bmat <- t( MASS::mvrnorm(n = num_hidden_units,
                         mu = rep(0, P),
                         Sigma = 1.75^2 * diag(P)) )

Bmat

### create and create the design matrix associaed with out input grid

Xgrid <- model.matrix( ~ x1 + x2, x_input)

Xgrid %>% head()

Xgrid %>% dim()

### calculate the linear predictors for every hidden unit
Agrid <- Xgrid %*% Bmat

Agrid %>% dim()

Agrid %>% head()

Agrid %>% class()

### visualize the linear predictors with respect to x1 for every hidden unit
Agrid %>% as.data.frame() %>% tibble::as_tibble() %>% 
  tibble::rowid_to_column() %>% 
  tidyr::gather(key = "key", value = "value", -rowid) %>% 
  left_join(x_input %>% tibble::rowid_to_column(),
            by = "rowid") %>% 
  ggplot(mapping = aes(x = x1, y = value)) +
  geom_line(mapping = aes(group = interaction(key, x2),
                          color = x2),
            size = 1.) +
  facet_wrap(~key) +
  scale_color_viridis_c(option = 'inferno') +
  theme_bw()

### transform each hidden unit by calculating the non-linear transformed response
Hgrid <- tanh(Agrid)

### visualize the trends of the non-linear hidden unit response with respect to x1
Hgrid %>% as.data.frame() %>% tibble::as_tibble() %>% 
  tibble::rowid_to_column() %>% 
  tidyr::gather(key = "key", value = "value", -rowid) %>% 
  left_join(x_input %>% tibble::rowid_to_column(),
            by = "rowid") %>% 
  ggplot(mapping = aes(x = x1, y = value)) +
  geom_line(mapping = aes(group = interaction(key, x2),
                          color = x2),
            size = 1.) +
  facet_wrap(~key) +
  scale_color_viridis_c(option = 'inferno') +
  theme_bw()

### let's visualize the surface trends of the hidden unit responses
### with respect to boht inputs
Hgrid %>% as.data.frame() %>% tibble::as_tibble() %>% 
  tibble::rowid_to_column() %>% 
  tidyr::gather(key = "key", value = "value", -rowid) %>% 
  left_join(x_input %>% tibble::rowid_to_column(),
            by = "rowid") %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = value)) +
  facet_wrap(~key) +
  scale_fill_viridis_c() +
  theme_bw()

### randomly generate the output layer coefficients

set.seed(91231)
a_params <- rnorm(n = num_hidden_units + 1, mean = 0, sd = 1.7)

a_params

### separate out the output layer bias from the weights
a_bias <- a_params[1]
a_weight <- a_params[-1]

a_bias

a_weight

### the neural network response or trend is a linear model
fgrid <- as.vector( a_bias + Hgrid %*% matrix(a_weight) )

### visualize the trends wrt the inputs
dfgrid <- x_input %>% 
  mutate(f = fgrid)

dfgrid %>% glimpse()

### trend with respect to x1
dfgrid %>% 
  ggplot(mapping = aes(x = x1, y = f)) +
  geom_line(mapping = aes(group = x2, color = x2),
            size = 1.) +
  scale_color_viridis_c(option = 'inferno') +
  theme_bw()

### trend with respect to x2
dfgrid %>% 
  ggplot(mapping = aes(x = x2, y = f)) +
  geom_line(mapping = aes(group = x1, color = x1),
            size = 1.) +
  scale_color_viridis_c(option = 'plasma') +
  theme_bw()

### visualize the trend as a surface
dfgrid %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = f)) +
  geom_contour(mapping = aes(z = f), color = "white") +
  scale_fill_viridis_c() +
  theme_bw()

dfgrid %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = f)) +
  geom_contour(mapping = aes(z = f), color = "white") +
  scale_fill_viridis_b() +
  theme_bw()

### generate noisy observations around the true trend

dfgrid %>% summary()

### specify the noise to be quite small, at roughly 5% of the range
### want to easily resolve the nonlinear output to input relationship

sigma_true <- 0.3

set.seed(71231)
grid_data <- dfgrid %>% 
  mutate(y = rnorm(n = n(), mean = f, sd = sigma_true))

### visualize the randomly generated outputs on the grid
grid_data %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = y)) +
  scale_fill_viridis_b() +
  theme_bw()

### fit the neural network by minimizing the SSE, use the SSE function
### from lecture

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
  
  # calculate the neural network in one line of code
  # your homework assignment breaks this operation 
  # into several lines, so do NOT copy and paste!!!
  # you may use this expression to help debug your code
  f <- as.vector( a0 + my_info$transform_hidden(X %*% Bmat) %*% matrix(av) )
  
  # performance metric - calculate the SSE
  sum((my_info$yobs - f)^2)
}

### setup the list of required information

my_info_5_units <- list(
  yobs = grid_data$y,
  design_matrix = Xgrid,
  num_hidden = 5,
  transform_hidden = tanh
)

### calculate the total number of unknown coefficients that must be learned
my_info_5_units$num_params <- my_info_5_units$num_hidden * (ncol(my_info_5_units$design_matrix)) +
  my_info_5_units$num_hidden + 1

my_info_5_units$num_params

### check our SSE function
my_neuralnet_sse(rep(0, my_info_5_units$num_params), my_info_5_units)

### find a local optimal set of parameters using a starting guess of zeros
### for all weights and biases, use optim() with finite differences to estimate
### the gradient and the BFGS quasi-newton algorithm

fit_0 <- optim(rep(0, my_info_5_units$num_params),
               my_neuralnet_sse,
               gr = NULL,
               my_info_5_units,
               method = "BFGS",
               hessian = TRUE,
               control = list(maxit = 5001))

fit_0$par

fit_0$convergence

fit_0$counts

### what if we would start from a random initial guess?
set.seed(12313)
start_guess_1 <- rnorm(n = my_info_5_units$num_params)

fit_1 <- optim(start_guess_1,
               my_neuralnet_sse,
               gr = NULL,
               my_info_5_units,
               method = "BFGS",
               hessian = TRUE,
               control = list(maxit = 5001))

fit_1$par

fit_1$convergence

fit_1$counts

### try with another random initial guess
set.seed(612312)
start_guess_2 <- rnorm(n = my_info_5_units$num_params)

fit_2 <- optim(start_guess_2,
               my_neuralnet_sse,
               gr = NULL,
               my_info_5_units,
               method = "BFGS",
               hessian = TRUE,
               control = list(maxit = 5001))

fit_2$par

fit_2$convergence

fit_2$counts

### Calculate the gradient numerically using hte numDeriv::grad() function

estimate_sse_grad <- function(unknowns, my_info)
{
  numDeriv::grad(my_neuralnet_sse, unknowns, method="Richardson", 
                 side=NULL, method.args=list(),
                 my_info)
}

### estimate the gradient at any set of parmaeter values
estimate_sse_grad(rep(0, my_info_5_units$num_params), my_info_5_units)

### check the optimization results when the "external" gradient function is provided
fit_2_with_grad <- optim(start_guess_2,
                         my_neuralnet_sse,
                         gr = estimate_sse_grad,
                         my_info_5_units,
                         method = "BFGS",
                         hessian = TRUE,
                         control = list(maxit = 5001))

fit_2_with_grad$par

fit_2_with_grad$convergence

fit_2_with_grad$counts

### check the locally optimal parameter estimates
fit_2_with_grad$par

fit_2$par

### summarize the absolute difference...the gradient calculated with the 
### `numDeriv::grad()` function is more precise than the gradient estimated
### by the default approach within `optim()`. With more than a few
### hidden units, those differences start to matter and create some slight differences
### between the identified optimal parameter values
###
### calculate min, average, and max absolute differences
min( abs(fit_2_with_grad$par - fit_2$par) )
mean( abs(fit_2_with_grad$par - fit_2$par) )
max( abs(fit_2_with_grad$par - fit_2$par) )

### check which parameters have the largest difference
which.max( abs(fit_2_with_grad$par - fit_2$par) )
fit_2_with_grad$par[ which.max( abs(fit_2_with_grad$par - fit_2$par) ) ]
fit_2$par[ which.max( abs(fit_2_with_grad$par - fit_2$par) ) ]

### program our own gradient descent algorithm, to help understand what's
### going on when we use a starting guess of 0 for this example

grad_descent_update <- function(unknowns, my_info, step_size)
{
  g <- estimate_sse_grad(unknowns, my_info)
  
  unknowns - step_size * g
}

### define a function which executes the gradient descent a desired 
### number of iterations, from a supplied starting guess, so this does not
### check for convergence, it's just a simple implementation, which runs
### a specified number of times

run_grad_descent <- function(start_guess, num_steps, step_size, my_info)
{
  # initialize
  res <- vector(mode = "list", length = num_steps + 1)
  res[[1]] <- start_guess
  
  # iterate the desired number of steps
  for(k in 2:(num_steps+1)){
    res[[k]] <- grad_descent_update(res[[k - 1]], my_info, step_size)
  }
  
  res
}

### test our function
run_grad_descent(rep(0, my_info_5_units$num_params), 3, step_size = 0.5e-3, my_info_5_units)

### define functions to help organize the iteration results

tidy_grad_descent_results <- function(res, iter_id)
{
  purrr::map2_dfr(res, iter_id,
                  function(ll, id){tibble::tibble(theta = ll) %>% 
                      mutate(iter_id = id) %>% 
                      tibble::rowid_to_column("theta_id")})
}

run_tidy_gradient_descent <- function(start_guess, num_steps, step_size, my_info)
{
  my_res <- run_grad_descent(start_guess, num_steps, step_size, my_info)
  
  tidy_grad_descent_results(my_res, seq_along(my_res) - 1)
}

### execute our gradient descent algorithm from a starting guess of 0

opt_iterations_0 <- run_tidy_gradient_descent(rep(0, my_info_5_units$num_params),
                                              50,
                                              step_size = 0.5e-3,
                                              my_info_5_units)

opt_iterations_0

opt_iterations_0 %>% 
  ggplot(mapping = aes(x = iter_id, y = theta)) +
  geom_line(mapping = aes(group = theta_id), size = 1.) +
  facet_wrap(~theta_id, labeller = "label_both") +
  theme_bw()

### if we would a smaller step size...
opt_iterations_0_b <- run_tidy_gradient_descent(rep(0, my_info_5_units$num_params),
                                                50,
                                                step_size = 0.5e-4,
                                                my_info_5_units)

opt_iterations_0_b %>% 
  ggplot(mapping = aes(x = iter_id, y = theta)) +
  geom_line(mapping = aes(group = theta_id), size = 1.) +
  facet_wrap(~theta_id, labeller = "label_both") +
  theme_bw()

### if we would one of the random initial guesses, lets use the second initial guess

opt_iterations_2 <- run_tidy_gradient_descent(start_guess_2,
                                              101,
                                              step_size = 0.5e-3,
                                              my_info_5_units)

opt_iterations_2 %>% 
  ggplot(mapping = aes(x = iter_id, y = theta)) +
  geom_line(mapping = aes(group = theta_id), size = 1.) +
  facet_wrap(~theta_id, labeller = "label_both") +
  theme_bw()
