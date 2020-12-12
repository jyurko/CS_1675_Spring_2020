#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    # assemble the TRUE coefficients
    beta_true <- reactive({
        
        c(input$beta_0, input$beta_1, input$beta_2,
          input$beta_3, input$beta_4, input$beta_5,
          input$beta_6, input$beta_7, input$beta_8)
    })
    
    # the TRUE relationship plot
    output$true_plot <- renderPlot({
        tibble::tibble(x = x_fine) %>% 
            mutate(f_true = my_true_func(x, beta_true())) %>% 
            ggplot(mapping = aes(x = x, y = f_true)) +
            geom_line(size = 1.25, color = "black") +
            coord_cartesian(xlim = c(-2.5, 2.5), 
                            ylim = c(input$user_true_lwr_bound, 
                                     input$user_true_upr_bound)) +
            labs(x = "x", y = "TRUTH",
                 title = "Your specified TRUE function") +
            theme_bw()
    })
    
    # set the noise level
    sigma_noise <- reactive({
        as.numeric(sigma_choices[input$user_noise_level])
    })
    
    # generate the complete training set
    my_data <- eventReactive(input$make_data_button, {
        set.seed(9291)
        #set.seed(8001)
        tibble::tibble(
            x = rnorm(n = input$user_num_points)
        ) %>% 
            mutate(mu = my_true_func(x, beta_true()),
                   y = rnorm(n = n(),
                             mean = mu,
                             sd = sigma_noise()))
    })
    
    # visualize the noise around the TRUTH with ribbons
    output$viz_noise_level_plot <- renderPlot({
        gg <- tibble::tibble(x = x_fine) %>% 
            mutate(f_true = my_true_func(x, beta_true())) %>% 
            ggplot(mapping = aes(x = x, y = f_true)) +
            geom_ribbon(mapping = aes(ymin = f_true - 2 * sigma_noise(),
                                      ymax = f_true + 2 * sigma_noise()),
                        fill = "grey50", alpha = 0.55) +
            geom_line(size = 1.25, color = "red") +
            coord_cartesian(xlim = c(-2.5, 2.5), 
                            ylim = c(input$user_true_lwr_bound, 
                                     input$user_true_upr_bound)) +
            labs(x = "x", y = "y",
                 title = "Noise level displayed as a ribbon around the TRUTH") +
            theme_bw()
        
        if (input$make_data_button) {
            gg <- gg + geom_point(data = my_data(),
                                  mapping = aes(x = x, y = y),
                                  color = "black", size = 1.75)
        }
        
        print(gg)
    })
    
    # make the resample object
    resample_info <- reactive({
        set.seed(23413)
        my_resample <- rsample::vfold_cv(my_data(), 
                                         v = as.integer(input$user_num_folds), 
                                         repeats = as.integer(input$user_num_reps))
        
        if (input$user_num_reps == 1){
            my_resample <- my_resample %>% rename(id2 = id) %>% mutate(id = "Repeat1")
        }
        
        my_resample
    })
    
    # assemble the separate folds to visualize the various train and test splits
    compiled_folds <- eventReactive(input$make_viz_resample_button, {
        my_res_object <- resample_info()
        
        purrr::pmap_dfr(list(my_res_object$splits, my_res_object$id2, my_res_object$id),
                        separate_fold_splits)
    })
    
    # visualize the training and test splits in each fold
    output$viz_resample_splits <- renderPlot({
        compiled_folds() %>% 
            ggplot(mapping = aes(x = x, y = y)) +
            geom_point(mapping = aes(color = type,
                                     shape = type),
                       size = 3) +
            facet_grid(rep_id~fold_id) +
            scale_color_manual("",
                               values = c("Training" = "red",
                                          "Hold-out" = "grey30")) +
            scale_shape_manual("",
                               values = c("Training" = 0,
                                          "Hold-out" = 16)) +
            theme_bw() +
            theme(legend.position = "top")
    })
    
    # train and assess all polynomial models using the specified resampling scheme
    all_model_cv_results <- reactive({
        purrr::map_dfr(0:8,
                       cv_perform_results,
                       resample_info = resample_info())
    })
    
    # visualize the resampling results on RMSE
    output$viz_cv_results_rmse <- renderPlot({
        all_model_cv_results() %>% 
            ggplot(mapping = aes(x = as.factor(poly_order),
                                 y = holdout_rmse)) +
            stat_summary(fun.data = "mean_se",
                         fun.args = list(mult = 2),
                         color = "black", alpha = 0.5,
                         mapping = aes(group = poly_order)) +
            stat_summary(fun.data = "mean_se",
                         fun.args = list(mult = 1),
                         color = "red", size = 1.75,
                         mapping = aes(group = poly_order)) +
            geom_point() +
            labs(x = "polynomial order", y = "Cross-validation RMSE") +
            theme_bw()
    })
    
    # summarize the CV results to get access to the best
    # performing model
    my_cv_results_summary <- reactive({
        all_model_cv_results() %>% 
            group_by(poly_order) %>% 
            summarise(num_rows = n(),
                      num_folds = n_distinct(id2),
                      num_repeats = n_distinct(id),
                      avg_rmse = mean(holdout_rmse),
                      sd_rmse = sd(holdout_rmse)) %>% 
            ungroup() %>% 
            mutate(se_rmse = sd_rmse / sqrt(num_rows))
    })
    
    my_best_model_summary <- reactive({
        my_cv_results_summary() %>% 
            arrange(avg_rmse) %>% 
            slice(1)
    })
    
    # zoom in on the RMSE summary figure around the best model
    output$viz_cv_results_rmse_zoom <- renderPlot({
        
        best_rmse_val <- my_best_model_summary() %>% 
            pull(avg_rmse)
        
        best_rmse_se <- my_best_model_summary() %>% 
            pull(se_rmse)
        
        all_model_cv_results() %>% 
            ggplot(mapping = aes(x = as.factor(poly_order),
                                 y = holdout_rmse)) +
            stat_summary(fun.data = "mean_se",
                         fun.args = list(mult = 2),
                         color = "black", alpha = 0.5,
                         mapping = aes(group = poly_order)) +
            stat_summary(fun.data = "mean_se",
                         fun.args = list(mult = 1),
                         color = "red", size = 1.75,
                         mapping = aes(group = poly_order)) +
            geom_point() +
            coord_cartesian(ylim = c(best_rmse_val - 2.5 * best_rmse_se,
                                     best_rmse_val + 3.5 * best_rmse_se)) +
            labs(x = "polynomial order", y = "Cross-validation RMSE",
                 title = "Zoom in on the Cross-validation RMSE summary results near the best model") +
            theme_bw()
    })

    # output$distPlot <- renderPlot({
    # 
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # 
    # })

})
