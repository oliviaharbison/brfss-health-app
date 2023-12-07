library(shiny)
library(tidyverse)

#data
cdc <- read_delim(file = "data/cdc.txt", delim = "|") %>%
  mutate(
    exerany = factor(
      exerany,
      levels = c(1, 0),
      labels = c("Yes", "No")
    ),
    smoke100 = factor(
      smoke100,
      levels = c(1, 0),
      labels = c("Yes", "No")
    ),
    gender = factor(
      gender,
      levels = c("f", "m"),
      labels = c("Female", "Male")
    ),
    genhlth = factor(
      genhlth,
      levels = c("excellent", "very good", "good", "fair", "poor"),
      labels = c("Excellent", "Very Good", "Good", "Fair", "Poor")
    ),
    hlthplan = factor(
      hlthplan,
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
  )

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("CDC BRFSS: Histogram of Weight Grouped by Gender"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    position = "right",
    # Sidebar panel for inputs ----
    sidebarPanel(
      #Input: selectInput
      selectInput(
        inputId = "var",
        label = "Select Variable:",
        choices = list("Actual Weight",
                       "Desired Weight",
                       "Height"),
        selected = "Actual Weight"
      ),
      
      # Input: Slider for the number of bins ----
      sliderInput(
        inputId = "bins",
        label = "Number of bins:",
        min = 5,
        max = 50,
        value = 30
      ),
      
      #Input: radio buttons
      radioButtons(
        inputId = "var_leg",
        label = "Select Fill/Legend Variable:",
        choices = list(
          "General Health",
          "Health Coverage",
          "Exercised in Past Month",
          "Smoked 100 Cigarettes",
          "Gender"
        )
      )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(# Output: Histogram ----
              plotOutput(outputId = "distPlot"))
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$distPlot <- renderPlot({
    var_hist <- switch(
      input$var,
      "Actual Weight" = cdc$weight,
      "Desired Weight" = cdc$wtdesire,
      "Height" = cdc$height
    )
    
    x_label <- switch(
      input$var,
      "Actual Weight" = "Actual Weight in Pounds",
      "Desired Weight" = "Desired Weight in Pounds",
      "Height" = "Height in Inches"
    )
    
    fill_hist <- switch(
      input$var_leg,
      "General Health" = cdc$genhlth,
      "Health Coverage" = cdc$hlthplan,
      "Exercised in Past Month" = cdc$exerany,
      "Smoked 100 Cigarettes" = cdc$smoke100,
      "Gender" = cdc$gender
    )
    

    
    ggplot(cdc, aes(x = var_hist, fill = fill_hist)) +
      geom_histogram(bins = input$bins, color = "black") +
      theme_minimal() +
      labs(x = x_label,
            y = "Count") +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title.align = 0.5
      ) +
      scale_fill_discrete(name = input$var_leg) +
      guides(fill = guide_legend(title.position = "top"))
    
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)