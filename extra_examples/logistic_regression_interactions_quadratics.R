### visualize the trends of logistic regression with 2 inputs
### that include an interaction term

library(tidyverse)

### create my grid of the two inputs
input_grid <- expand.grid(x1 = seq(-3, 3, length.out = 51),
                          x2 = seq(-3, 3, length.out = 51),
                          KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>% 
  as.data.frame() %>% tibble::as_tibble()

input_grid %>% glimpse()

input_grid %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_tile(fill = 'grey', color = 'black') +
  theme_bw()

### linear predictor and the probability of the event as a surface

### define a function for the linear predictor

calc_eta <- function(b0, b1, b2, b3, x1, x2)
{
  b0 + b1 * x1 + b2 * x2 + b3 * x1 * x2
}

### test out our eta calculation
input_grid %>% 
  mutate(eta = calc_eta(b0 = 0, b1 = 1, b2 = -1, b3 = 0, x1 = x1, x2 = x2)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = eta)) +
  coord_equal() +
  scale_fill_viridis_c() +
  theme_bw()

input_grid %>% 
  mutate(eta = calc_eta(b0 = 0, b1 = 1, b2 = -1, b3 = 0, x1 = x1, x2 = x2)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = eta)) +
  geom_contour(mapping = aes(z = eta), color = "white") +
  coord_equal() +
  scale_fill_viridis_c() +
  theme_bw()

input_grid %>% 
  mutate(eta = calc_eta(b0 = 0, b1 = 1, b2 = -1, b3 = 0, x1 = x1, x2 = x2)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = eta)) +
  geom_contour(mapping = aes(z = eta), color = "white") +
  coord_equal() +
  scale_fill_viridis_b() +
  theme_bw()

### take a look at eta with an interaction term
input_grid %>% 
  mutate(eta = calc_eta(b0 = 0, b1 = 1, b2 = -1, b3 = 1, x1 = x1, x2 = x2)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = eta)) +
  geom_contour(mapping = aes(z = eta), color = "white") +
  coord_equal() +
  scale_fill_viridis_b() +
  theme_bw()

### what if the interaction slope was -1?
input_grid %>% 
  mutate(eta = calc_eta(b0 = 0, b1 = 1, b2 = -1, b3 = -1, x1 = x1, x2 = x2)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = eta)) +
  geom_contour(mapping = aes(z = eta), color = "white") +
  coord_equal() +
  scale_fill_viridis_b() +
  theme_bw()

### study the behavior of the linear predictor for various slopes

beta_grid <- expand.grid(b0 = 0,
                         b1 = c(-1, 0, 1),
                         b2 = c(-1, 0, 1),
                         b3 = c(-2, -1, 0, 1, 2),
                         KEEP.OUT.ATTRS = FALSE,
                         stringsAsFactors = FALSE) %>% 
  as.data.frame() %>% tibble::as_tibble()

beta_grid %>% glimpse()

### create a function to allow calculate the linear predictor at user specified
### coefficient values

study_eta <- function(b0, b1, b2, b3, input_df)
{
  input_df %>% 
    mutate(eta = calc_eta(b0 = b0, b1 = b1, b2 = b2, b3 = b3, x1 = x1, x2 = x2)) %>% 
    mutate(b0 = b0, b1 = b1, b2 = b2, b3 = b3)
}

study_trends_df <- purrr::pmap_dfr(list(beta_grid$b0,
                                        beta_grid$b1,
                                        beta_grid$b2,
                                        beta_grid$b3),
                                   study_eta,
                                   input_df = input_grid)

study_trends_df %>% glimpse()

study_trends_df %>% count(x1, x2) %>% glimpse()

study_trends_df %>% count(b1)

study_trends_df %>% count(b0, b1, b2, b3) %>% glimpse()

### visualize the trends in the linear predictor over the input surface
### for each combination of the coefficients
study_trends_df %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = eta,
                            group = interaction(b0, b1, b2, b3))) +
  geom_contour(mapping = aes(z = eta,
                             group = interaction(b0, b1, b2, b3)),
               color = 'white') +
  coord_equal() +
  facet_grid( b2 ~ b1 + b3, labeller = "label_both") +
  scale_fill_viridis_b() +
  theme_bw()

### we have to back-transform from eta to the probability of the event (mu)
### to study the trends of the event probability wrt the input surface

study_trends_df %>% 
  mutate(mu = boot::inv.logit(eta)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = mu,
                            group = interaction(b0, b1, b2, b3))) +
  geom_contour(mapping = aes(z = mu,
                             group = interaction(b0, b1, b2, b3)),
               color = 'white') +
  coord_equal() +
  facet_grid( b2 ~ b1 + b3, labeller = "label_both") +
  scale_fill_viridis_b(option = 'inferno') +
  theme_bw()

### consider quadratic effects

calc_eta_quad <- function(b0, b1, b2, b3, b4, b5, x1, x2)
{
  b0 + b1 * x1 + b2 * x2 + b3 * x1 * x2 + b4 * x1^2 + b5 * x2^2
}

input_grid %>% 
  mutate(eta = calc_eta_quad(b0=0, b1=1, b2=-1, b3=0, b4=1, b5=-1, x1=x1, x2=x2)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = eta)) +
  geom_contour(mapping = aes(z = eta), color = 'white') +
  coord_equal() +
  scale_fill_viridis_c() +
  theme_bw()

### include a positive interaction effect
input_grid %>% 
  mutate(eta = calc_eta_quad(b0=0, b1=1, b2=-1, b3=1, b4=1, b5=-1, x1=x1, x2=x2)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = eta)) +
  geom_contour(mapping = aes(z = eta), color = 'white') +
  coord_equal() +
  scale_fill_viridis_c() +
  theme_bw()

### include a negative interaction effect
input_grid %>% 
  mutate(eta = calc_eta_quad(b0=0, b1=1, b2=-1, b3=-1, b4=1, b5=-1, x1=x1, x2=x2)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = eta)) +
  geom_contour(mapping = aes(z = eta), color = 'white') +
  coord_equal() +
  scale_fill_viridis_c() +
  theme_bw()

### calculate the event probability from the linear predictor

### first consider NO interaction
input_grid %>% 
  mutate(eta = calc_eta_quad(b0=0, b1=1, b2=-1, b3=0, b4=1, b5=-1, x1=x1, x2=x2)) %>% 
  mutate(mu = boot::inv.logit(eta)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = mu)) +
  geom_contour(mapping = aes(z = mu), color = 'white') +
  coord_equal() +
  scale_fill_viridis_c(option = 'inferno') +
  theme_bw()

### use a binned color scale
input_grid %>% 
  mutate(eta = calc_eta_quad(b0=0, b1=1, b2=-1, b3=0, b4=1, b5=-1, x1=x1, x2=x2)) %>% 
  mutate(mu = boot::inv.logit(eta)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = mu)) +
  geom_contour(mapping = aes(z = mu), color = 'white') +
  coord_equal() +
  scale_fill_viridis_b(option = 'inferno') +
  theme_bw()

### try out different values for the coefficients
input_grid %>% 
  mutate(eta = calc_eta_quad(b0=0, b1=1, b2=-1, b3=0, b4=2, b5=-1, x1=x1, x2=x2)) %>% 
  mutate(mu = boot::inv.logit(eta)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = mu)) +
  geom_contour(mapping = aes(z = mu), color = 'white') +
  coord_equal() +
  scale_fill_viridis_b(option = 'inferno') +
  theme_bw()

input_grid %>% 
  mutate(eta = calc_eta_quad(b0=0, b1=1, b2=-1, b3=0, b4=1, b5=-2, x1=x1, x2=x2)) %>% 
  mutate(mu = boot::inv.logit(eta)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = mu)) +
  geom_contour(mapping = aes(z = mu), color = 'white') +
  coord_equal() +
  scale_fill_viridis_b(option = 'inferno') +
  theme_bw()

### use both quadratic terms as positive coefficients
input_grid %>% 
  mutate(eta = calc_eta_quad(b0=0, b1=1, b2=-1, b3=0, b4=1, b5=1, x1=x1, x2=x2)) %>% 
  mutate(mu = boot::inv.logit(eta)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = mu)) +
  # geom_contour(mapping = aes(z = mu), color = 'white') +
  coord_equal() +
  scale_fill_viridis_b(option = 'inferno') +
  theme_bw()

### discetize the event probability surface into GREATER THAN 0.5 and
### LESS THAN 0.5 --- THIS is a threshold value of 50%

threshold_value <- 0.5

### the following makes sure that the RED fill is the EVENT being that
### the event probabilty is GREATER than the threshold
input_grid %>% 
  mutate(eta = calc_eta_quad(b0=0, b1=1, b2=-1, b3=0, b4=1, b5=1, x1=x1, x2=x2)) %>% 
  mutate(mu = boot::inv.logit(eta)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = mu <= threshold_value)) +
  coord_equal() +
  scale_fill_brewer(palette = "Set1") +
  theme_bw()

### we can change the threshoold 

new_threshold_value <- 0.2

input_grid %>% 
  mutate(eta = calc_eta_quad(b0=0, b1=1, b2=-1, b3=0, b4=1, b5=1, x1=x1, x2=x2)) %>% 
  mutate(mu = boot::inv.logit(eta)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = mu <= new_threshold_value)) +
  coord_equal() +
  scale_fill_brewer(palette = "Set1") +
  theme_bw()

### check the minmum value of mu
input_grid %>% 
  mutate(eta = calc_eta_quad(b0=0, b1=1, b2=-1, b3=0, b4=1, b5=1, x1=x1, x2=x2)) %>% 
  mutate(mu = boot::inv.logit(eta)) %>% 
  summary()

### change the legend labels to be EVENT and NON-EVENT
input_grid %>% 
  mutate(eta = calc_eta_quad(b0=0, b1=1, b2=-1, b3=0, b4=1, b5=1, x1=x1, x2=x2)) %>% 
  mutate(mu = boot::inv.logit(eta)) %>% 
  mutate(y = ifelse(mu > threshold_value, "EVENT", "NON-EVENT")) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = y)) +
  coord_equal() +
  scale_fill_brewer(palette = "Set1") +
  theme_bw()

input_grid %>% 
  mutate(eta = calc_eta_quad(b0=0, b1=1, b2=-1, b3=0, b4=1, b5=1, x1=x1, x2=x2)) %>% 
  mutate(mu = boot::inv.logit(eta)) %>% 
  mutate(y = ifelse(mu > new_threshold_value, "EVENT", "NON-EVENT")) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = y)) +
  coord_equal() +
  scale_fill_brewer(palette = "Set1") +
  theme_bw()

### what would happen if we use negative coefficients on the quadratic terms
input_grid %>% 
  mutate(eta = calc_eta_quad(b0=0, b1=1, b2=-1, b3=0, b4=-1, b5=-1, x1=x1, x2=x2)) %>% 
  mutate(mu = boot::inv.logit(eta)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = mu)) +
  # geom_contour(mapping = aes(z = mu), color = 'white') +
  coord_equal() +
  scale_fill_viridis_b(option = 'inferno') +
  theme_bw()

### look at the decision boundary surface with a threshold of 0.5
input_grid %>% 
  mutate(eta = calc_eta_quad(b0=0, b1=1, b2=-1, b3=0, b4=-1, b5=-1, x1=x1, x2=x2)) %>% 
  mutate(mu = boot::inv.logit(eta)) %>% 
  mutate(y = ifelse(mu > threshold_value, "EVENT", "NON-EVENT")) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = y)) +
  coord_equal() +
  scale_fill_brewer(palette = "Set1") +
  theme_bw()

### what does the interaction term do, with quadratics?

### interactions between inputs turn the CIRCLE into an ELLIPSE
input_grid %>% 
  mutate(eta = calc_eta_quad(b0=0, b1=1, b2=-1, b3=1, b4=-1, b5=-1, x1=x1, x2=x2)) %>% 
  mutate(mu = boot::inv.logit(eta)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = mu)) +
  # geom_contour(mapping = aes(z = mu), color = 'white') +
  coord_equal() +
  scale_fill_viridis_b(option = 'inferno') +
  theme_bw()

input_grid %>% 
  mutate(eta = calc_eta_quad(b0=0, b1=1, b2=-1, b3=-1, b4=-1, b5=-1, x1=x1, x2=x2)) %>% 
  mutate(mu = boot::inv.logit(eta)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = mu)) +
  # geom_contour(mapping = aes(z = mu), color = 'white') +
  coord_equal() +
  scale_fill_viridis_b(option = 'inferno') +
  theme_bw()

### look at the elliptical decision surface
input_grid %>% 
  mutate(eta = calc_eta_quad(b0=0, b1=1, b2=-1, b3=1, b4=-1, b5=-1, x1=x1, x2=x2)) %>% 
  mutate(mu = boot::inv.logit(eta)) %>% 
  mutate(y = ifelse(mu > threshold_value, "EVENT", "NON-EVENT")) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = y)) +
  coord_equal() +
  scale_fill_brewer(palette = "Set1") +
  theme_bw()

input_grid %>% 
  mutate(eta = calc_eta_quad(b0=0, b1=1, b2=-1, b3=-1, b4=-1, b5=-1, x1=x1, x2=x2)) %>% 
  mutate(mu = boot::inv.logit(eta)) %>% 
  mutate(y = ifelse(mu > threshold_value, "EVENT", "NON-EVENT")) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = y)) +
  coord_equal() +
  scale_fill_brewer(palette = "Set1") +
  theme_bw()

### Generate our own binary outcome data for practice

### assume the inputs are from standard normals

### assume the linear predictor is associated with the true beta coefficients
### with the negative interaction elliptical decision surface

set.seed(812311)

Xinputs <- MASS::mvrnorm(n = 250, mu = rep(0, 2), diag(2)) %>% 
  as.data.frame() %>% tibble::as_tibble() %>% 
  purrr::set_names(sprintf("x%d", 1:2))

Xinputs

Xinputs %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_point(alpha = 0.5, size = 5) +
  coord_equal() +
  theme_bw()

### calculate the TRUE linear predictor and the TRUE probability of the event
### and then GENERATE random binary outcomes

set.seed(123213)
df <- Xinputs %>% 
  mutate(eta = calc_eta_quad(b0=0, b1=1, b2=-1, b3=-1, b4=-1, b5=-1, x1=x1, x2=x2),
         mu = boot::inv.logit(eta)) %>% 
  mutate(y = rbinom(n = n(), size = 1, prob = mu),
         y_class = ifelse(y == 1, "EVENT", "NON-EVENT"))

df

### visualize the randomly generated event/non-events on the TRUE event
### probabiliyt surface -- because this is a toy problem

input_grid %>% 
  mutate(eta = calc_eta_quad(b0=0, b1=1, b2=-1, b3=-1, b4=-1, b5=-1, x1=x1, x2=x2)) %>% 
  mutate(mu = boot::inv.logit(eta)) %>% 
  ggplot(mapping = aes(x = x1, y = x2)) +
  geom_raster(mapping = aes(fill = mu)) +
  geom_point(data = df,
             mapping = aes(x = x1, y = x2, shape = y_class,
                           color = mu > 0.5),
             size = 4) +
  # geom_contour(mapping = aes(z = mu), color = 'white') +
  coord_equal() +
  scale_fill_viridis_b(option = 'inferno') +
  scale_shape_discrete(solid = FALSE) +
  scale_color_manual(guide = FALSE,
                     values = c("TRUE" = "black",
                                "FALSE" = "white")) +
  theme_bw()
