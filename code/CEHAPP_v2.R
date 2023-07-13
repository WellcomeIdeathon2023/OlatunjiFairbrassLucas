library(shiny)
library(leaflet)
library(rgdal)
library(sf)
library(here)
library(tmap)
library(DiagrammeR)
library(ggplot2)

# Set the seed
set.seed(123)

# Expand the limit of the memory
options(shiny.maxRequestSize = 1000 * 1024^2)

# Read shapefile data
data <- st_read("../data/IND_adm2.shp")

# UI
ui <- fluidPage(
  titlePanel("Causal Analysis Toolkit for Climate, Economy and Health"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("a", "Effect of Temperature on Rice Yield (a)", min = 0, max = 1, value = 0.5, step = 0.1),
      sliderInput("b", "Effect of Rice Yield on Suicide (b)", min = 0, max = 1, value = 0.5, step = 0.1),
      sliderInput("c", "Direct effect of Temperature on Suicide (c)", min = 0, max = 1, value = 0, step = 0.1),
      sliderInput("d", "Effect of Unobserved factors on Rice Yield (d)", min = 0, max = 1, value = 0.5, step = 0.1),
      sliderInput("e", "Effect of Unobserved factors on Suicide (e)", min = 0, max = 1, value = 0.5, step = 0.1),
      actionButton("calculate", "Calculate")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Plots",
                 grVizOutput("dag_plot"),  # DAG plot
                 plotOutput("plot1")  # Box plot
        ),
        tabPanel("Maps",
                 fluidRow(
                   column(width = 6, h4("Indirect effect"), leafletOutput("map1")),  # Map 1
                   column(width = 6, h4("Log of number of suicides"), leafletOutput("map2"))  # Map 2
                 ),
                 hr(),
                 fluidRow(
                   column(width = 12, h4("Probability of \n log number of suicides > 0"), leafletOutput("map3"))  # Map 3
                 )
        )
      )
    )
  )
)

# Server
server <- function(input, output) {

  # Create a node data frame (ndf) for DAG plot
  ndf <- create_node_df(
    n         = 4,
    label     = c( "Temperature", "Rice Yield", "Suicide", "Unobserved"),
    shape     = c(rep("rectangle", 3), "circle"),
    style     = "empty",
    fontsize  = 6,
    fixedsize = TRUE,
    height    = .5,
    width     = .75,
    color     = "gray80",
    x         = c(1, 2, 3, 3.5),
    y         = c(1, 2, 1, 2.5)
  )

  # Create an edge data frame (edf) for DAG plot
  edf <- create_edge_df(
    from     = c(1, 1, 2, 4, 4),
    to       = c(2, 3, 3, 2, 3),
    label    = c("a", "c", "b", "d", "e"),
    fontsize = 6,
    minlen   = 1,
    color    = c("blue", "red", "blue", "gray80", "gray80")
  )

  # Create a graph with the ndf and edf for DAG plot
  dag <- create_graph(
    nodes_df = ndf,
    edges_df = edf
  )

  # Render DAG plot
  output$dag_plot <- renderGrViz({
    render_graph(dag, as_svg = TRUE)
  })

  # Calculate and plot indirect effect
  observeEvent(input$calculate, {
    N <- nrow(data)
    temperature <- runif(N)
    unobserved <- runif(N)
    rice_yield <- input$a * temperature + input$d * unobserved + rnorm(N, sd = 0.01)
    suicide <- input$b * rice_yield + input$c * temperature + input$e * unobserved + rnorm(N, sd = 0.01)

    # Correct answer
    mtr <- lm(rice_yield ~ temperature + unobserved)
    mrs <- lm(suicide ~ rice_yield + unobserved)
    correct_indirect <- mrs$coefficients['rice_yield'] * mtr$coefficients['temperature']

    # Wrong answer
    mtr <- lm(rice_yield ~ temperature + unobserved)
    mrs <- lm(suicide ~ rice_yield + temperature + unobserved)
    wrong_indirect <- mrs$coefficients['rice_yield'] * mtr$coefficients['temperature']

    # Perform bootstrapping for uncertainties
    num_bootstraps <- 1000
    correct_indirect_boot <- replicate(num_bootstraps, {
      idx <- sample(N, replace = TRUE)
      mtr_boot <- lm(rice_yield[idx] ~ temperature[idx] + unobserved[idx])
      mrs_boot <- lm(suicide[idx] ~ rice_yield[idx] + unobserved[idx])
      coef(mrs_boot)["rice_yield[idx]"] * coef(mtr_boot)["temperature[idx]"]
    })

    wrong_indirect_boot <- replicate(num_bootstraps, {
      idx <- sample(N, replace = TRUE)
      mtr_boot <- lm(rice_yield[idx] ~ temperature[idx] + unobserved[idx])
      mrs_boot <- lm(suicide[idx] ~ rice_yield[idx] + temperature[idx] + unobserved[idx])
      coef(mrs_boot)["rice_yield[idx]"] * coef(mtr_boot)["temperature[idx]"]
    })

    # Create a data frame for box plot
    plot_data <- data.frame(
      Effect = c(correct_indirect_boot, wrong_indirect_boot),
      Method = rep(c("Correct Indirect Effect", "Wrong Indirect Effect"), each = num_bootstraps)
    )

    # Plotting
    output$plot1 <- renderPlot({
      # Plotting with ggplot
      ggplot(plot_data, aes(x = Method, y = Effect)) +
        geom_boxplot() +
        geom_hline(yintercept = input$a * input$b, linetype = "dashed", color = "green", size = 1) +
        labs(x = "Method", y = "Indirect Effect") +
        ggtitle("") +
        theme_bw()
    })
  })

  # Render an empty plot as a placeholder
  output$plot1 <- renderPlot({
    NULL
  })

  # Render Map 1: Indirect Effect
  output$map1 <- renderLeaflet({
    N <- nrow(data)
    data$indirect <- input$a * input$b * runif(N, 0, 1)
    tm <- tm_shape(data) + tm_polygons("indirect", legend.title = "Indirect Effect", title = "Indirect Effect")
    tmap_leaflet(tm)
  })

  # Render Map 2: Log of Number of Suicides
  output$map2 <- renderLeaflet({
    N <- nrow(data)
    temperature <- runif(N)
    unobserved <- runif(N)
    rice_yield <- input$a * temperature + input$d * unobserved + rnorm(N, sd = 0.01)
    suicide <- input$b * rice_yield + input$c * temperature + input$e * unobserved + rnorm(N, sd = 0.01)

    # Correct model
    model <- lm(suicide ~ rice_yield + unobserved)

    # Create new data for prediction
    new_data <- data.frame(
      rice_yield = input$a * temperature + input$d * unobserved + rnorm(N, sd = 0.01),
      temperature = temperature,
      unobserved = unobserved
    )

    predictions <- predict(model, newdata = new_data)

    data$log_suicide <- predictions
    tm <- tm_shape(data) + tm_polygons("log_suicide", title = "Log of Number of Suicides", legend.title = "Log(Number of Suicides)")
    tmap_leaflet(tm)
  })

  # Render Map 3: Probability of log Number of Suicides > 0
  output$map3 <- renderLeaflet({
    N <- nrow(data)
    temperature <- runif(N)
    unobserved <- runif(N)
    rice_yield <- input$a * temperature + input$d * unobserved + rnorm(N, sd = 0.01)
    suicide <- input$b * rice_yield + input$c * temperature + input$e * unobserved + rnorm(N, sd = 0.01)

    # Correct model
    model <- lm(suicide ~ rice_yield + unobserved)

    # Create new data for prediction
    new_data <- data.frame(
      rice_yield = input$a * temperature + input$d * unobserved + rnorm(N, sd = 0.01),
      temperature = temperature,
      unobserved = unobserved
    )

    # Number of samples
    num_samples <- 1000

    # Generate samples of predictions
    prediction_samples <- replicate(num_samples, {
      predictions <- predict(model, newdata = new_data) + rnorm(N, sd = summary(model)$sigma)
      predictions
    })

    # Calculate probability for each data point
    prob_suicide_gt_2 <- rowMeans(prediction_samples > 0.5)
    data$exceed <- prob_suicide_gt_2
    tm <- tm_shape(data) + tm_polygons("exceed", title = "Prob that \nlog-suicide > 0", legend.title = "Prob that suicide exceeds 0")
    tmap_leaflet(tm)
  })
}

# Run the Shiny app
shinyApp(ui, server)

