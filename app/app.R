library("shiny")
library("ggplot2")
library("tidyr")
library("dplyr")
library("shinythemes")
library("patchwork")

# shinylive::export("app", "docs")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("spacelab"),
  
  # Application title
  titlePanel("CLT", windowTitle = "CLT"),
  
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
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$clt_plots <- renderPlot({
    
    # Generate distribution
    my_distribution <- sample(1:6, size = input$n_of_dist, replace = TRUE)
    
    # Descriptives of the distribution
    dist_mean <- mean(my_distribution)
    dist_sd   <- sd(my_distribution)
    dist_se   <- dist_sd / sqrt(input$n_of_dist)
    
    # Generate vector to store samples
    my_samples <- vector(mode = "numeric", length = input$n_samples)
    
    # Get samples
    for (i in 1:input$n_samples) {
      my_samples[i] <- mean(sample(my_distribution, 
                                   size = input$sample_size, 
                                   replace = FALSE))
    }
    
    # Descriptives of sample
    sample_mean <- mean(my_samples)
    sample_sd   <- sd(my_samples)
    sample_se   <- sample_sd / sqrt(length(my_samples))
    
    my_theme <- function(...) {
      list(
        theme_minimal(base_family = 'Palatino', base_size = 20), 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank())
      )
    }
    
    # Plot results
    p5 <- tibble(my_distribution) |>
      mutate(fac = "fac") |>
      ggplot() + 
      aes(x = my_distribution, fill = fac) + 
      geom_histogram(binwidth = 1, color = "grey20")
    
    p5_ymax <- ggplot_build(p5)$data[[1]] |> 
      summarize(ymax = max(y) + max(y) * 0.15) |> 
      pull()
    
    p5 <- p5 + geom_vline(xintercept = dist_mean, lty = 3, 
                          color = "white", linewidth = 1) + 
      geom_vline(xintercept = dist_mean - dist_sd, lty = 3, 
                 color = "white", linewidth = 1) + 
      geom_vline(xintercept = dist_mean + dist_sd, lty = 3, 
                 color = "white", linewidth = 1) + 
      scale_fill_viridis_d(name = NULL, begin = 0.3, guide = "none") + 
      scale_x_continuous(limits = c(-2, 9), labels = 1:6, breaks = 1:6) + 
      coord_cartesian(ylim = c(0, p5_ymax * 1.05)) + 
      annotate("text", x = dist_mean, y = p5_ymax, parse = T, 
               size = 7, label = paste0("mu == ", round(dist_mean, 3))) + 
      annotate("text", x = dist_mean, y = p5_ymax * .93, parse = T, size = 7, 
               label = paste0("sigma == ", round(dist_sd, 3))) + 
      labs(x = paste0(input$n_of_dist, " rolls"), y = NULL) +
      my_theme()
    
    p6 <- tibble(my_samples) |>
      mutate(fac = "fac") |>
      ggplot() + 
      aes(x = my_samples, fill = fac) + 
      geom_histogram(
        aes(y = after_stat(density)), 
        binwidth = input$plot_bw, color = "grey20"
      )
    
    p6_x_ymax <- ggplot_build(p6)$data[[1]] |> 
      summarize(
        xmin = min(my_samples) - sd(my_samples) * 2, 
        xmax = max(my_samples) + sd(my_samples) * 2, 
        ymax = max(ymax) + max(ymax) * .15
      ) |> 
      pivot_longer(
        cols = everything(), 
        names_to = "my_axis", 
        values_to = "val"
      ) |> 
      as.data.frame()
    
    p6 <- p6 + stat_function(fun = dnorm, 
                             args = list(mean = mean(my_samples), 
                                         sd = sd(my_samples))) + 
      geom_vline(xintercept = sample_mean, lty = 3, color = "white", linewidth = 1) + 
      geom_vline(xintercept = sample_mean - sample_sd, lty = 3, 
                 color = "white", linewidth = 1) + 
      geom_vline(xintercept = sample_mean + sample_sd, lty = 3, 
                 color = "white", linewidth = 1) + 
      scale_fill_viridis_d(name = NULL, begin = 0.8, guide = "none") + 
      scale_x_continuous(limits = c(-2, 9), labels = 1:6, breaks = 1:6) + 
      scale_y_continuous(position = "right") + 
      annotate("text", x = sample_mean, y = p6_x_ymax[3, "val"], 
               parse = T, size = 7,
               label = paste0("bar(x)  == ", round(sample_mean, 3))) + 
      annotate("text", x = sample_mean, y = p6_x_ymax[3, "val"] * .93, 
               parse = T, size = 7,
               label = paste0("sd == ", round(sample_sd, 3))) + 
      labs(
        x = paste0(input$n_samples, " samples (n = ", input$sample_size, ")"), 
        y = NULL
      ) +
      my_theme()
    
    
    p5 + p6
    
  })
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)
