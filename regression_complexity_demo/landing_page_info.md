### Welcome to the regression complexity demo

This is an R Shiny app! You can interactively train polynomial regression models similar to the introduction to regression example from lecture. In this app you will:  

* Specify the TRUE functional relationship  
* Specify the desired noise level  
* Specify the total number of data points to use  
* **Generate the data**  
* Specify the resampling scheme to consider  
* Evaluate the resampling results at identifying the correct relationship  

#### Specify the TRUE relationship

In the "Specify truth" tab, you will specify the coefficients (parameters) of the **TRUE** functional relationship. By default, the **TRUE** relationship is quadratic with the coefficients equal to those from the lecture example. Thus, the higher order term coefficients are set to 0. You can change that however! If you want the truth to be a complex 7th order polynomial, you can try that out! However...will you be able to learn the truth given the noise?  

#### Noise and generating data

In the "Noise and generating data!" tab, you will set the desired noise level. For now, the noise is specified as "very low" to "very high". We will discuss quantifying noise later in the semester. For now, let's just think qualitatively. The visualization will help you understand the potential impact of your selected noise level.  

You will also set the total number of data points available to use. The minimum size is 30 and the maximum size is 150. You can modify the number of data points to see if that makes it easier or harder to learn the **TRUE** relationship you specified on the previous tab, given your specified noise level.  

#### Resampling

In the "Resampling" tab you specify the resampling scheme used to train and test (sometimes called validate) the models. You will specifically use K-fold cross-validation. You can specify the number of folds and the number of repeats. Thus, if you want you can try 5-fold cross-validation with 5 repeats.  

A specific number folds and repeats options are available to you. These are not the **only** choices you could make on your own for your own applications. The available choices are common values.  

The "Viz Resamples!" button will generate a visualization that marks the training and hold-out test points per fold. If you do not use repeated cross-validation, you will see one facet per fold. If you use repeated cross-validation, the folds are displayed by the horizontal subplots and the repeats are displayed by the vertical subplots. Thus, if you want to compare the first fold across 3 repeats in 5-fold cross-validation, you can look down the first column.  

#### Visualize resampling results

This app uses the same 9 models from the in-lecture example. Thus, you will compare an intercept-only model (a constant) up to and including an 8th order polynomial.  

All 9 models are trained and tested using your specified resampling scheme. When you select the "Visualize resampling results" tab you will see 2 figures. Both figures summarize the cross-validation RMSE results. The RMSE calculated on each resample fold is displayed with black markers. The average RMSE across all resample folds is shown by the large red dot. The one-standard error confidence interval is displayed by the vertical red lines, and the two-standard error confidence interval is displayed by the thin grey lines. 

The figure displayed at the top of the "Visualize results" tab shows the RMSE summary statistics for all 9 models. The bounds are set by `ggplot2`. This figure is sensitive to the worst performing model with the y-axis bounds set to show the highest RMSE on the worst performing fold for the worst performing model.  

The second figure zooms in around the best performing model. You should be able to see which models have one-standard error confidence intervals that overlap with the model with the lowest average RMSE.  

Can you tell if the resampling was able to identify the model that generated your data?  

### Good luck!  