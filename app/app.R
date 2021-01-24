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
               mainPanel(
                 plotOutput("panel1_plot"),
                 includeHTML("www/task_01.html")
               )
             )
    ),

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
                   "panel2_input_sd",
                   "Population Standard Deviation",
                   min = 1,
                   max = 100,
                   value = 20
                 ),
                 sliderInput(
                   "panel2_input_alpha",
                   "Alpha",
                   min = 0,
                   max = .20,
                   value = .05,
                   step = .01
                 )
               ),
               mainPanel(
                 plotOutput("panel2_plot"),
                 tableOutput(outputId = "panel2_tbl_rates"),
                 includeHTML("www/task_02.html")
               )
             )
    ),

    ## ###########################################################
    ## About Panel
    ## ###########################################################
    tabPanel("About",
            includeHTML("www/about.html")
    )
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
  mydata <- reactiveValues( samples = NULL, alpha = 0.05 )

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
    my_sd          = input$panel2_input_sd
    my_alpha       = input$panel2_input_alpha

    # Create a blank data frame
    samples = data.frame()
    for (i in 1:1000)
    {
      random_sample <- rnorm(n = my_sample_size, mean = 10, sd = my_sd)
      mean_sample   <- mean(random_sample)
      sd_sample     <- sd(random_sample)
      std_z         <- (mean_sample-10)/(sd_sample/sqrt(my_sample_size))
      std_t         <- (mean_sample-10)/(sd_sample/sqrt(my_sample_size))

      # computed per sample run
      norm_cutoff = qnorm(1-my_alpha, mean = 10, sd = (sd_sample/sqrt(my_sample_size)))
      t_cutoff = qt(1-my_alpha,df=9)*(sd_sample/sqrt(my_sample_size)) + 10

      # combine all values into a row and add to our sample data frame
      row = cbind(mean_sample, sd_sample,std_z, std_t, norm_cutoff, t_cutoff)
      samples = rbind( samples, row)
    }

    # rename the columns
    colnames(samples) = c("mean", "sd", "z", "t", "z_cut", "t_cut")

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

    #
    # I had an idea for annotation to use mean z/t cut, 
    # but this is not really relevant as each sample has it's
    # own unique cutoff points.
    #
    # norm_cutoff = mean(samples$z_cut)
    # t_cutoff    = mean(samples$t_cut)

    myplot = ggplot(samples,aes(x=mean)) +
      geom_histogram(color="Blue", fill="White")+
      geom_histogram(data=subset(samples,mean>z_cut),
                     colour="blue", fill="blue", alpha = .7)+
      # this needs to come after blue or it won't show up
      # can't get rid of pink line on bottom of x-axis :-(
      geom_histogram(data=subset(samples,mean>t_cut),
                     colour="red", fill="red", alpha = .5)+

      theme_classic()
    
    #
    # These lines no longer represent a single cutoff as
    #  each observation has it's own unique cut off.
    #
    # geom_vline(xintercept=norm_cutoff, color = "blue", linetype = 2, size = 2, alpha = .2) +
    # geom_vline(xintercept=t_cutoff, color = "blue", linetype = 2, size = 2, alpha = .4) +
    # annotate("text", label = "t-cutoff", fontface=2, x = t_cutoff , y = 80, size = 6, colour = "red", angle=90) +
    # annotate("text", label = "z-cutoff", fontface=2, x = norm_cutoff , y = 80, size = 6, colour = "red", angle=90)

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

    norm_value = qnorm(1-my_alpha)
    t_value = qt(1-my_alpha, df = my_sample_size - 1)

    # extract these values from our sample data frame
    count_norm      = sum(samples$z >= norm_value)
    count_t         = sum(samples$t >= t_value)
    percent_rejectz = count_norm/1000
    percent_rejectt = count_t/1000

    # This is the table that will be displayed
    results = as.data.frame(cbind(count_norm, percent_rejectz, count_t, percent_rejectt))
    colnames(results) = c("Number Rejected Using Z", "Percent Rejected Using Z", "Number Rejected Using  t", "Percent Rejected Using t")
    return(results)
  })


}

# Run the application
shinyApp(ui = ui, server = server)
