#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(
    
    navbarPage("Regression complexity demo",
        
        tabPanel("Welcome!",
                 mainPanel(includeMarkdown("landing_page_info.md"))),
        tabPanel("Specify truth",
                 sidebarLayout(
                     sidebarPanel(
                         helpText("Specify the TRUE coefficients that control the TRUE relationship!"),
                         sliderInput("beta_0",
                                     "Intercept:",
                                     min = -1,
                                     max = 1,
                                     value = 0.33,
                                     step = 0.01),
                         sliderInput("beta_1",
                                     "linear coefficient:",
                                     min = -3,
                                     max = 3,
                                     value = 1.15,
                                     step = 0.01),
                         sliderInput("beta_2",
                                     "quadratic coefficient:",
                                     min = -3,
                                     max = 3,
                                     value = -2.25,
                                     step = 0.01),
                         sliderInput("beta_3",
                                     "cubic coefficient:",
                                     min = -3,
                                     max = 3,
                                     value = 0,
                                     step = 0.01),
                         sliderInput("beta_4",
                                     "4th order coefficient:",
                                     min = -3,
                                     max = 3,
                                     value = 0,
                                     step = 0.01),
                         sliderInput("beta_5",
                                     "5th order coefficient:",
                                     min = -3,
                                     max = 3,
                                     value = 0,
                                     step = 0.01),
                         sliderInput("beta_6",
                                     "6th order coefficient:",
                                     min = -3,
                                     max = 3,
                                     value = 0,
                                     step = 0.01),
                         sliderInput("beta_7",
                                     "7th order coefficient:",
                                     min = -3,
                                     max = 3,
                                     value = 0,
                                     step = 0.01),
                         sliderInput("beta_8",
                                     "8th order coefficient:",
                                     min = -3,
                                     max = 3,
                                     value = 0,
                                     step = 0.01),
                     ),
                     mainPanel(plotOutput("true_plot"),
                               numericInput("user_true_upr_bound",
                                            "Type in y-axis upper bound:",
                                            value = 7,
                                            min = 5,
                                            max = 15,
                                            step = 1),
                               numericInput("user_true_lwr_bound",
                                            "Type in the y-axis lower bound:",
                                            value = -17,
                                            min = -30,
                                            max = -5,
                                            step = 1))
                 )
                 ),
        tabPanel("Noise and generate data!",
                 sidebarLayout(
                     sidebarPanel(
                         helpText("Set the noise level of the observations around the TRUTH"),
                         selectInput("user_noise_level",
                                     "Noise level:",
                                     c("very low", "low", "moderate", "high", "very high"),
                                     selected = "high"),
                         helpText("How many data points do you want to work with in this example?"),
                         sliderInput("user_num_points",
                                     "Total number of data points:",
                                     min = 30,
                                     max = 150,
                                     value = 30,
                                     step = 1),
                         helpText("If you CHANGED the TRUTH or the NOISE, please generate the data again!"),
                         actionButton("make_data_button",
                                      "Generate data!")
                     ),
                     mainPanel(plotOutput("viz_noise_level_plot"))
                 )),
        tabPanel("Resampling",
                 sidebarLayout(
                     sidebarPanel(
                         helpText("How many folds and repeats will you use?"),
                         selectInput("user_num_folds",
                                     "Number of folds:",
                                     c(3, 5, 10, 15),
                                     selected = 5),
                         selectInput("user_num_reps",
                                     "Number of repeats:",
                                     c(1, 3, 5, 10),
                                     selected = 1),
                         helpText("Would you like to visualize the folds?"),
                         actionButton("make_viz_resample_button",
                                      "Viz Resamples!")
                     ),
                     mainPanel(plotOutput("viz_resample_splits", height = "900px"))
                 )),
        tabPanel("Visualize resampling results",
                 mainPanel(plotOutput("viz_cv_results_rmse"),
                           plotOutput("viz_cv_results_rmse_zoom")))
    )
)

### below is my practice using navbarPage
# shinyUI(
#     
#     navbarPage("Old Faithful Geyser Data",
#                tabPanel("Histogram",
#                         
#                         sidebarLayout(
#                             sidebarPanel(
#                                 sliderInput("bins",
#                                             "Number of bins:",
#                                             min = 1, 
#                                             max = 50,
#                                             value = 30)
#                             ),
#                             mainPanel(
#                                 plotOutput("distPlot")
#                             )
#                         )
#                         ),
#                tabPanel("Another tab"))
# )

### following is the default shinyUI from rstudio
# # Define UI for application that draws a histogram
# shinyUI(fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#             plotOutput("distPlot")
#         )
#     )
# )
# 
# )
