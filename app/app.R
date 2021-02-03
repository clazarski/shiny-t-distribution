## ###########################################################
##
## t-distribution/app.R
##
## Educational shiny app to teach students the ideas
## around the t-distribution
##
## @author: Craig Lazarski & Jeffery Painter
## @modified: 2021-Jan-24
##
## ###########################################################

library(shiny)
library(ggplot2)
library(dplyr)
#library(tidyverse)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://www.w3schools.com/w3css/4/w3.css")
  ),
  
  title = "t-distribution Explorer",
  h1("t-distribution Explorer"),
  
  tabsetPanel(
    type = "tabs",
    
    ## ###########################################################
    ## Panel 1
    ## ###########################################################
    tabPanel("t-dist vs Normal",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   "panel1_input_deg_free",
                   "Number of df:",
                   min = 1,
                   max = 50,
                   value = 1
                 )
               ),
               mainPanel(plotOutput("panel1_plot"),
                         includeHTML("www/task_01.html"))
             )),
    
    ## ###########################################################
    ## Panel 2
    ## ###########################################################
    tabPanel("Rate of rejection",
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   "panel2_input_sample_size",
                   "Sample size",
                   min = 1,
                   max = 50,
                   value = 10
                 ),
                 
                 sliderInput(
                   "panel2_input_alpha",
                   "Alpha",
                   min = 0,
                   max = .20,
                   value = .05,
                   step = .01
                 ),
                 sliderInput(
                   "panel2_input_obs",
                   "Observation",
                   min = 1,
                   max = 1000,
                   value = 500,
                   step = 1
                 ),
                 tableOutput(outputId = "panel2_tbl_obs"),
                 # sliderInput(
                 #   "panel2_input_sd",
                 #   "Population Standard Deviation",
                 #   min = 1,
                 #   max = 100,
                 #   value = 20
                 # ),
               ),
               mainPanel(
                 plotOutput("panel2_plot"),
                 tableOutput(outputId = "panel2_tbl_rates"),
                 includeHTML("www/task_02.html")
               )
             )),
    
    ## ###########################################################
    ## About Panel
    ## ###########################################################
    tabPanel("About",
             includeHTML("www/about.html"))
  )
)

## ###########################################################
##  Shiny server code
## ###########################################################
server <- function(input, output)
{
  #########################################
  # Panel 1
  #########################################
  
  ## #################################
  ## Panel 1 input/outputs
  ## #################################
  # INPUT
  #    panel1_input_deg_free
  ## #################################
  # OUTPUT
  #   panel1_plot
  ## #################################
  output$panel1_plot <- renderPlot({
    # any change to this slider will update the graph
    my_deg_free <- input$panel1_input_deg_free
    
    #Create set of points to create t and normal curves
    points = as.data.frame(seq(from = -5, to = 5, by = .05))
    colnames(points) = c("values")
    
    #Plot t and normal distributions
    ggplot(points, aes(x = values)) +
      theme_classic() +
      stat_function(
        fun = dt,
        n = 101,
        args = list(df = my_deg_free),
        color = 'red'
      ) +
      stat_function(
        fun = dnorm,
        n = 101,
        args = list(mean = 0, sd = 1),
        color = 'Blue'
      )
  })
  
  #########################################
  # Panel 2
  #########################################
  
  # Reactive values create shared data space in the app
  mydata <-
    reactiveValues(samples = NULL,
                   alpha = 0.05,
                   observation = NULL)
  
  ## #################################
  ## Panel 2 input/outputs
  ## #################################
  # INPUT
  #    panel2_input_sample_size
  #    panel2_input_sd
  #    panel2_input_alpha
  ## #################################
  # OUTPUT
  #    panel2_plot
  #    panel2_tbl_rates
  ## #################################
  
  ## Create a new sample data set that is
  ## shared by both the plot and table
  updateSampleData <- function()
  {
    # Inputs
    my_sample_size = input$panel2_input_sample_size
    my_sd          = 50 #input$panel2_input_sd
    my_alpha       = input$panel2_input_alpha
    
    # Create a blank data frame
    samples = data.frame()
    for (i in 1:1000)
    {
      random_sample <- rnorm(n = my_sample_size, mean = 10, sd = my_sd)
      mean_sample   <- mean(random_sample)
      sd_sample     <- sd(random_sample)
      std_z         <-
        (mean_sample - 10) / (sd_sample / sqrt(my_sample_size))
      std_t         <-
        (mean_sample - 10) / (sd_sample / sqrt(my_sample_size))
      
      # computed per sample run
      norm_cutoff = qnorm(1 - my_alpha,
                          mean = 10,
                          sd = (sd_sample / sqrt(my_sample_size)))
      t_cutoff = qt(1 - my_alpha, df = 9) * (sd_sample / sqrt(my_sample_size)) + 10
      
      # combine all values into a row and add to our sample data frame
      row = cbind(mean_sample,
                  sd_sample,
                  std_z,
                  std_t,
                  norm_cutoff,
                  t_cutoff)
      samples = rbind(samples, row)
    }
    
    # rename the columns
    colnames(samples) = c("mean", "sd", "z", "t", "z_cut", "t_cut")
    
    #sort the data
    samples = samples %>% arrange(z)
    
    # update the reactive variables
    mydata$samples = samples
    mydata$alpha   = my_alpha
    return()
  }
  
  # changes to sample size needs new data
  observeEvent(input$panel2_input_sample_size, {
    updateSampleData()
  })
  
  # changes to SD needs new data
  observeEvent(input$panel2_input_sd, {
    updateSampleData()
  })
  
  # changes to alpha only update the table
  observeEvent(input$panel2_input_alpha, {
    # update the reactive variable which will update the table output
    mydata$alpha = input$panel2_input_alpha
  })
  
  
  output$panel2_plot <- renderPlot({
    # Any change to these will invalidate the plot
    my_sample_size = isolate(input$panel2_input_sample_size)
    my_sd          = isolate(input$panel2_input_sd)
    my_alpha       = isolate(input$panel2_input_alpha)
    
    # This is a reactive variable and when changed, causes the plot
    # to be updated and redrawn. we have separated the logic from
    # updating alpha, so that alpha changes work on the same
    # data and only update the table and not the plot
    samples = mydata$samples
    
    #Create set of points to create t and normal curves
    points = as.data.frame(seq(from = -5, to = 5, by = .05))
    colnames(points) = c("values")
    
    index = input$panel2_input_obs
    plot_z = samples[index,]$z
    plot_t = samples[index,]$t
    mydata$observation = samples[index,]
    
    dotcolor = "maroon"
    if (mydata$observation$z < qnorm(1 - mydata$alpha))
    {
      dotcolor = "orange"
    }
    if (mydata$observation$z > qnorm(1 - mydata$alpha) &&
        mydata$observation$z > qt(1 - mydata$alpha, my_sample_size - 1))
    {
      dotcolor = "purple"
    }
    
    myplot = ggplot(points, aes(x = values)) +
      theme_classic() +
      stat_function(
        fun = dt,
        n = 101,
        args = list(df = my_sample_size - 1),
        color = 'red'
      ) +
      stat_function(
        fun = dnorm,
        n = 101,
        args = list(mean = 0, sd = 1),
        color = 'Blue'
      ) +
      geom_vline(
        xintercept = qnorm(1 - mydata$alpha),
        color = "blue",
        linetype = 'dashed'
      ) +
      geom_vline(
        xintercept = qt(1 - mydata$alpha, df = my_sample_size - 1),
        color = "red",
        linetype = "dashed"
      ) +
      annotate(
        "point",
        x = plot_z,
        y = .01,
        color = dotcolor,
        size = 6
      ) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_continuous(expand = c(0, 0)) +
      annotate("text", x = 3, y = .25, label = "Normal", color = "blue", size=6)+
      annotate("text", x = 3, y = .23, label = "t-dist", color = "red", size=6)
    
    # Return the ggplot to render
    return(myplot)
    
  })
  
  output$panel2_tbl_rates <- renderTable({
    # Listen to changes from our reactive variables
    my_alpha = mydata$alpha
    samples  = mydata$samples
    
    # Since we are listening to the reactive value above, we can isolate these
    # to avoid multiple triggers to invalidating our table data
    my_sample_size = isolate(input$panel2_input_sample_size)
    my_sd          = isolate(input$panel2_input_sd)
    
    norm_value = qnorm(1 - my_alpha)
    t_value = qt(1 - my_alpha, df = my_sample_size - 1)
    
    # extract these values from our sample data frame
    count_norm      = sum(samples$z >= norm_value)
    count_t         = sum(samples$t >= t_value)
    percent_rejectz = count_norm / 1000
    percent_rejectt = count_t / 1000
    
    # This is the table that will be displayed
    results = as.data.frame(cbind(count_norm, percent_rejectz, count_t, percent_rejectt))
    colnames(results) = c(
      "Number Rejected Using Z",
      "Percent Rejected Using Z",
      "Number Rejected Using  t",
      "Percent Rejected Using t"
    )
    return(results)
  })
  
  output$panel2_tbl_obs <- renderTable({
    df = mydata$observation
    df$t = NULL
    df$z_cut = NULL
    df$t_cut = NULL
    colnames(df) = c("Mean", "Sample SD", "Test Statistic")
    return(df)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)