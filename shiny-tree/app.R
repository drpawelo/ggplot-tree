#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("decorating the tree!"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("ornaments_number",
                  "Number of ornaments:",
                  min = 1,
                  max = 30,
                  value = 10),
      sliderInput("branches_number",
                  "Number of branches:",
                  min = 2,
                  max = 10,
                  value = 4)
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    branches <-  input$branches_number
    
    
    # input$branches_number
    x_min <- -3
    x_max <-  3
    x_distance <- x_max - x_min
    x_one_way_length_min <- 1
    
    y_min <- 1
    y_max <- 4
    y_distance <- y_max - y_min
    y_one_height <- y_distance / (branches)
    
    
    # tree
    tree_data <- data.frame(
      x = c(rbind(rep(0, branches),
                  seq( -x_one_way_length_min, x_min, by = (x_min + x_one_way_length_min) / (branches-1)),
                  seq( x_one_way_length_min, x_max, by = (x_max - x_one_way_length_min) / (branches-1))
      )),
      y = c(rbind(seq(y_max,   y_min + y_one_height , by = - y_one_height),
                  seq(y_max - y_one_height, y_min,   by = - y_one_height),
                  seq(y_max - y_one_height, y_min,   by = - y_one_height)
      )),
      group = c(rep(seq(1,branches,1), each=3))
    )
    # tree_data <- data.frame(
    #   x = c(0, -2, 2, 0, -3, 3, 0, -4, 4,  0, -5, 5),
    #   y = c(4, 3, 3, 3, 2, 2, 2, 1, 1,1,0,0),
    #   group = c(1, 1, 1, 2, 2, 2, 3, 3, 3,4,4,4)
    # )
    
    
    print(tree_data)
    
    # trunk
    trunk_data <- data.frame(
      x = c(-0.5, 0.5, 0.5, -0.5),
      y = c(1, 1, 0, 0)
    )
    
    # bulbs
    ornament_data <- data.frame(
      x = round(runif(input$ornaments_number,-2.0, 3.0), 2),
      y = round(runif(input$ornaments_number, 1.0, 3.5), 2)
    )
  
    
    ggplot() +
      geom_polygon(data = tree_data, aes(x = x, y = y, group = group), fill = "forestgreen", color = "darkgreen") +
      geom_polygon(data = trunk_data, aes(x = x, y = y), fill = "brown", color = "black") +
      geom_point(data = ornament_data, aes(x = x, y = y), color = "red", size = 10, alpha=I(0.7) ) +
      theme_void() + # no axis
      coord_fixed() + # size
      ggtitle("Tree 🎄")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
