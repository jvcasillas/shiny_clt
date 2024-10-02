library(shiny)
library(tidyverse)
library(shinythemes)
library(patchwork)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("spacelab"),
  
  # Application title
  titlePanel("CLT"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 3, 
       numericInput("n_of_dist", 
                    "Number of rolls:", 
                    value = 10000, 
                    min = 1, 
                    max = 10000),
       sliderInput("n_samples", 
                   "Number of distribution samples", 
                   min = 2, 
                   max = 1000, 
                   value = 1000), 
       sliderInput("sample_size", 
                   "Size of sample draws", 
                   min = 2, 
                   max = 50, 
                   value = 2), 
       sliderInput("plot_bw", 
                   "Binwidth", 
                   min = 0, 
                   max = 1, 
                   value = 0.5, 
                   step = 0.05),
       br(),
       p(strong("Created by:"), 
         tags$a("Joseph V. Casillas", href="http://www.jvcasillas.com"),
       br(), 
         strong("Source code:"), 
         tags$a("Github", href="https://github.com/jvcasill/shiny_clt/"))),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("clt_plots")
    )
  )
))
